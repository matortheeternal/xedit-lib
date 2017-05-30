unit txRecords;

interface

  // PUBLIC TESTING INTERFACE
  procedure BuildRecordHandlingTests;

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
          ExpectSuccess(GetElement(0, 'xtest-3.esp\00C23801', @kw3));
        end);

      Describe('IsMaster', procedure
        begin
          It('Should return true for master records', procedure
            begin
              ExpectSuccess(IsMaster(ar1, @b));
              Expect(b);
              ExpectSuccess(IsMaster(kw1, @b));
              Expect(b);
              ExpectSuccess(IsMaster(kw2, @b));
              Expect(b);
            end);

          It('Should return false for override records', procedure
            begin
              ExpectSuccess(IsMaster(ar2, @b));
              Expect(not b);
              ExpectSuccess(IsMaster(ar3, @b));
              Expect(not b);
              ExpectSuccess(IsMaster(kw3, @b));
              Expect(not b);
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
              ExpectSuccess(IsInjected(ar1, @b));
              Expect(not b);
            end);

          It('Should return false for override records', procedure
            begin
              ExpectSuccess(IsInjected(ar2, @b));
              Expect(not b);
            end);

          It('Should return true for injected records', procedure
            begin
              ExpectSuccess(IsInjected(kw1, @b));
              Expect(b);
              ExpectSuccess(IsInjected(kw2, @b));
              Expect(b);
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
              ExpectSuccess(IsOverride(ar1, @b));
              Expect(not b);
              ExpectSuccess(IsOverride(kw1, @b));
              Expect(not b);
              ExpectSuccess(IsOverride(kw2, @b));
              Expect(not b);
            end);

          It('Should return true for override records', procedure
            begin
              ExpectSuccess(IsOverride(ar2, @b));
              Expect(b);
              ExpectSuccess(IsOverride(ar3, @b));
              Expect(b);
              ExpectSuccess(IsOverride(kw3, @b));
              Expect(b);
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
              ExpectSuccess(IsWinningOverride(kw1, @b));
              Expect(b);
            end);

          It('Should return false for losing master records', procedure
            begin
              ExpectSuccess(IsWinningOverride(ar1, @b));
              Expect(not b);
            end);

          It('Should return false for losing override records', procedure
            begin
              ExpectSuccess(IsWinningOverride(ar2, @b));
              Expect(not b);
              ExpectSuccess(IsWinningOverride(kw2, @b));
              Expect(not b);
            end);

          It('Should return true for winning override records', procedure
            begin
              ExpectSuccess(IsWinningOverride(ar3, @b));
              Expect(b);
              ExpectSuccess(IsWinningOverride(kw3, @b));
              Expect(b);
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
