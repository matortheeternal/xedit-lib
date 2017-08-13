unit txRecords;

interface

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
  xeElements, xeRecords, xeElementValues,
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

procedure BuildRecordHandlingTests;
var
  b: WordBool;
  skyrim, armo, ar1, dnam, ar2, ar3, kw1, kw2, kw3, h: Cardinal;
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
          It('Should work with root handle', procedure
            begin
              h := TestFindNextRecord(0, 'Armor', True, False, 'sHUDArmorRating');
            end);

          It('Should work from record handle', procedure
            begin
              h := TestFindNextRecord(h, 'Armor', True, False, 'sEnchantArmorIncompatible');
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
    end);
end;

end.
