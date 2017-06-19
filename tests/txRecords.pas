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
  xeElements, xeRecords,
  {$ENDIF}
  txMeta;

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
  skyrim, armo, ar1, dnam, ar2, ar3, kw1, kw2, kw3: Cardinal;
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
