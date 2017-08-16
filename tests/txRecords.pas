unit txRecords;

interface

type
  TConflictAll = (
    caUnknown,
    caOnlyOne,
    caNoConflict,
    caConflictBenign,
    caOverride,
    caConflict,
    caConflictCritical
  );
  TConflictThis = (
    ctUnknown,
    ctIgnored,
    ctNotDefined,
    ctIdenticalToMaster,
    ctOnlyOne,
    ctHiddenByModGroup,
    ctMaster,
    ctConflictBenign,
    ctOverride,
    ctIdenticalToMasterWinsConflict,
    ctConflictWins,
    ctConflictLoses
  );

  // PUBLIC TESTING INTERFACE
  procedure BuildRecordHandlingTests;
  procedure TestIsMaster(rec: Cardinal; expectedValue: WordBool);

implementation

uses
  Mahogany,
  {$IFDEF USE_DLL}
  txImports,
  {$ENDIF}
  {$IFNDEF USE_DLL}
  xeMeta, xeElements, xeRecords, xeElementValues,
  {$ENDIF}
  txMeta;

procedure TestGetRecords(context: Cardinal; path, search: PWideChar; includeOverrides: WordBool; expectedCount: Integer);
var
  h: Cardinal;
  len: Integer;
begin
  if path <> '' then
    ExpectSuccess(GetElement(context, path, @h))
  else
    h := context;
  ExpectSuccess(GetRecords(h, search, includeOverrides, @len));
  ExpectEqual(len, expectedCount);
  gra(len);
end;

function TestFindNextRecord(context: Cardinal; search: PWideChar; byEdid, byName: WordBool; expectedEdid: String): Cardinal;
var
  len: Integer;
begin
  ExpectSuccess(FindNextRecord(context, search, byEdid, byName, @Result));
  Expect(Result > 0, 'Should return a handle');
  ExpectSuccess(GetValue(Result, 'EDID', @len));
  ExpectEqual(grs(len), expectedEDID);
end;

procedure TestIsMaster(rec: Cardinal; expectedValue: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(IsMaster(rec, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestIsInjected(rec: Cardinal; expectedValue: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(IsInjected(rec, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestIsOverride(rec: Cardinal; expectedValue: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(IsOverride(rec, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestIsWinningOverride(rec: Cardinal; expectedValue: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(IsWinningOverride(rec, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestGetConflictData(nodes, element: Cardinal; path: PWideChar; ca: TConflictAll; ct: TConflictTHis);
var
  caResult, ctResult: Byte;
  h: Cardinal;
begin
  if path <> '' then
    ExpectSuccess(GetElement(element, path, @element));
  ExpectSuccess(GetConflictData(nodes, element, @caResult, @ctResult));
  ExpectEqual(caResult, Ord(ca));
  ExpectEqual(ctResult, Ord(ct));
end;

procedure BuildRecordHandlingTests;
var
  b: WordBool;
  skyrim, armo, ar1, dnam, ar2, ar3, kw1, kw2, kw3, h, n1, n2, n3: Cardinal;
begin
  Describe('Record Handling', procedure
    begin
      BeforeAll(procedure
        begin
          ExpectSuccess(GetElement(0, 'Skyrim.esm', @skyrim));
          ExpectSuccess(GetElement(skyrim, 'ARMO', @armo));
          ExpectSuccess(GetElement(armo, '00012E46', @ar1));
          ExpectSuccess(GetElement(ar1, 'DNAM', @dnam));
          ExpectSuccess(GetElement(0, 'xtest-2.esp\00012E46', @ar2));
          ExpectSuccess(GetElement(0, 'xtest-3.esp\00012E46', @ar3));
          ExpectSuccess(GetElement(0, 'xtest-1.esp\00C23800', @kw1));
          ExpectSuccess(GetElement(0, 'xtest-1.esp\00C23801', @kw2));
          ExpectSuccess(GetElement(0, 'xtest-4.esp\00C23801', @kw3));
        end);

      Describe('GetRecords', procedure
        begin
          Describe('No search', procedure
            begin
              It('Should return all records in a file', procedure
                begin
                  TestGetRecords(0, 'xtest-2.esp', '', True, 6);
                end);

              It('Should be able to exclude overrides', procedure
                begin
                  TestGetRecords(0, 'xtest-2.esp', '', False, 1);
                end);

              It('Should return all records in a top level group', procedure
                begin
                  TestGetRecords(armo, '', '', True, 2762);
                  TestGetRecords(0, 'xtest-2.esp\CELL', '', True, 3);
                end);

              It('Should return all records in a subgroup', procedure
                begin
                  TestGetRecords(0, 'xtest-2.esp\00027D1C\Child Group\Persistent', '', True, 2);
                end);

              It('Should return all record children of a record', procedure
                begin
                  TestGetRecords(0, 'xtest-2.esp\00027D1C', '', True, 2);
                end);
            end);

          Describe('Search', procedure
            begin
              It('Should return all records of a given signature in all files', procedure
                begin
                  TestGetRecords(0, '', 'DOBJ', False, 1);
                  TestGetRecords(0, '', 'DOBJ', True, 2);
                  TestGetRecords(0, '', 'ARMO', False, 2763);
                  TestGetRecords(0, '', 'ARMO', True, 2808);
                end);

              It('Should be able to handle multiple signatures', procedure
                begin
                  TestGetRecords(0, '', 'ARMO,WEAP,MISC', False, 2763 + 2484 + 371);
                end);

              It('Should map names to signatures', procedure
                begin
                  TestGetRecords(0, '', 'Armor', False, 2763);
                  TestGetRecords(0, '', 'Constructible Object,Non-Player Character (Actor)', False, 606 + 5119);
                end);
            end);
        end);

      Describe('FindNextRecord', procedure
        begin
          BeforeAll(procedure
            begin
              ExpectSuccess(SetSortMode(1, false));
            end);

          AfterAll(procedure
            begin
              ExpectSuccess(SetSortMode(0, false));
            end);

          It('Should work with root handle', procedure
            begin
              h := TestFindNextRecord(0, 'Armor', True, False, 'TG08ANightingaleArmorActivator');
            end);

          It('Should work from record handle', procedure
            begin
              h := TestFindNextRecord(h, 'Armor', True, False, 'FortifySkillHeavyArmor02');
            end);
        end);

      Describe('IsMaster', procedure
        begin
          It('Should return true for master records', procedure
            begin
              TestIsMaster(ar1, True);
              TestIsMaster(kw1, True);
              TestIsMaster(kw2, True);
            end);

          It('Should return false for override records', procedure
            begin
              TestIsMaster(ar2, False);
              TestIsMaster(ar3, False);
              TestIsMaster(kw3, False);
            end);

          It('Should fail on elements that are not records', procedure
            begin
              ExpectFailure(IsMaster(skyrim, @b));
              ExpectFailure(IsMaster(armo, @b));
              ExpectFailure(IsMaster(dnam, @b));
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(IsMaster(0, @b));
            end);
        end);

      Describe('IsInjected', procedure
        begin
          It('Should return false for master records', procedure
            begin
              TestIsInjected(ar1, False);
            end);

          It('Should return false for override records', procedure
            begin
              TestIsInjected(ar2, False);
            end);

          It('Should return true for injected records', procedure
            begin
              TestIsInjected(kw1, True);
              TestIsInjected(kw2, True);
            end);

          It('Should fail on elements that are not records', procedure
            begin
              ExpectFailure(IsInjected(skyrim, @b));
              ExpectFailure(IsInjected(armo, @b));
              ExpectFailure(IsInjected(dnam, @b));
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(IsInjected(0, @b));
            end);
        end);

      Describe('IsOverride', procedure
        begin
          It('Should return false for master records', procedure
            begin
              TestIsOverride(ar1, False);
              TestIsOverride(kw1, False);
              TestIsOverride(kw2, False);
            end);

          It('Should return true for override records', procedure
            begin
              TestIsOverride(ar2, True);
              TestIsOverride(ar3, True);
              TestIsOverride(kw3, True);
            end);

          It('Should fail on elements that are not records', procedure
            begin
              ExpectFailure(IsOverride(skyrim, @b));
              ExpectFailure(IsOverride(armo, @b));
              ExpectFailure(IsOverride(dnam, @b));
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(IsOverride(0, @b));
            end);
        end);

      Describe('IsWinningOverride', procedure
        begin
          It('Should return true for records with no overrides', procedure
            begin
              TestIsWinningOverride(kw1, True);
            end);

          It('Should return false for losing master records', procedure
            begin
              TestIsWinningOverride(ar1, False);
            end);

          It('Should return false for losing override records', procedure
            begin
              TestIsWinningOverride(ar2, False);
              TestIsWinningOverride(kw2, False);
            end);

          It('Should return true for winning override records', procedure
            begin
              TestIsWinningOverride(ar3, True);
              TestIsWinningOverride(kw3, True);
            end);

          It('Should fail on elements that are not records', procedure
            begin
              ExpectFailure(IsWinningOverride(skyrim, @b));
              ExpectFailure(IsWinningOverride(armo, @b));
              ExpectFailure(IsWinningOverride(dnam, @b));
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(IsWinningOverride(0, @b));
            end);
        end);

      Describe('GetNodes', procedure
        begin
          It('Should return a handle if argument is record', procedure
            begin
              ExpectSuccess(GetNodes(kw1, @h));
              ExpectSuccess(ReleaseNodes(h));
            end);

          It('Should work with records with overrides', procedure
            begin
              ExpectSuccess(GetNodes(ar1, @h));
              ExpectSuccess(ReleaseNodes(h));
            end);

          It('Should fail on elements that are not records', procedure
            begin
              ExpectFailure(GetNodes(skyrim, @h));
              ExpectFailure(GetNodes(armo, @h));
              ExpectFailure(GetNodes(dnam, @h));
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(GetNodes(0, @h));
            end);
        end);

      Describe('GetConflictData', procedure
        begin
          BeforeAll(procedure
            begin
              ExpectSuccess(GetNodes(kw1, @n1));
              ExpectSuccess(GetNodes(kw2, @n2));
              ExpectSuccess(GetNodes(ar1, @n3));
            end);

          It('Should work on main records', procedure
            begin
              TestGetConflictData(n1, kw1, '', caOnlyOne, ctOnlyOne);
              TestGetConflictData(n2, kw2, '', caConflictCritical, ctMaster);
              TestGetConflictData(n3, ar1, '', caConflict, ctMaster);
            end);

          It('Should work on struct elements', procedure
            begin
              TestGetConflictData(n1, kw1, 'Record Header', caOnlyOne, ctOnlyOne);
              TestGetConflictData(n2, kw2, 'Record Header', caNoConflict, ctMaster);
              TestGetConflictData(n3, ar1, 'Record Header', caNoConflict, ctMaster);
              TestGetConflictData(n2, kw2, 'CNAM - Color', caConflictCritical, ctMaster);
            end);

          It('Should work on value elements', procedure
            begin
              TestGetConflictData(n1, kw1, 'Record Header\Signature', caOnlyOne, ctOnlyOne);
              TestGetConflictData(n2, kw2, 'Record Header\Signature', caNoConflict, ctMaster);
              TestGetConflictData(n3, ar1, 'Record Header\Signature', caNoConflict, ctMaster);
              TestGetConflictData(n2, kw2, 'CNAM - Color\Red', caConflictCritical, ctMaster);
            end);

          AfterAll(procedure
            begin
              ExpectSuccess(ReleaseNodes(n1));
              ExpectSuccess(ReleaseNodes(n2));
            end);
        end);
    end);
end;

end.
