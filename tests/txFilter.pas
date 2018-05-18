unit txFilter;

interface

  // PUBLIC TESTING INTERFACE
  procedure BuildFilterTests;

implementation

uses
  SysUtils,
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFilter, xeFiles, xeElements, xeElementValues, xeRecords,
{$ENDIF}
  txMeta, txElements;

procedure BuildFilterTests;
var
  testFile, armo, rec, cell, refr, dnam, h: Cardinal;
  len: Integer;
begin
  Describe('Serialization', procedure
    begin
      BeforeAll(procedure
        begin
          ExpectSuccess(GetElement(0, 'xtest-2.esp', @testFile));
          ExpectSuccess(GetElement(testFile, 'ARMO', @armo));
          ExpectSuccess(GetElement(armo, '00012E46', @rec));
          ExpectSuccess(GetElement(testFile, 'CELL', @cell));
          ExpectSuccess(GetElement(testFile, '000170F0', @refr));
          ExpectSuccess(GetElement(rec, 'DNAM', @dnam));
        end);

      Describe('FilterRecord', procedure
        begin
          It('Should succeed on records', procedure
            begin
              ExpectSuccess(FilterRecord(rec));
              ExpectSuccess(FilterRecord(refr));
            end);

          It('Should fail on files', procedure
            begin
              ExpectFailure(FilterRecord(testFile));
            end);

          It('Should fail on groups', procedure
            begin
              ExpectFailure(FilterRecord(armo));
            end);

          It('Should fail on elements', procedure
            begin
              ExpectFailure(FilterRecord(dnam));
            end);

          Describe('GetElements results', procedure
            begin
              It('Should filter files', procedure
                begin
                  ExpectSuccess(GetElements(0, '', False, True, @len));
                  ExpectEqual(len, 1);
                end);

              It('Should filter groups', procedure
                begin
                  ExpectSuccess(GetElements(testFile, '', False, True, @len));
                  ExpectEqual(len, 2);
                end);

              It('Should filter records', procedure
                begin
                  ExpectSuccess(GetElements(armo, '', False, True, @len));
                  ExpectEqual(len, 1);
                end);
            end);
        end);

      Describe('ResetFilter', procedure
        begin
          It('Should suceed', procedure
            begin
              ExpectSuccess(ResetFilter);
            end);

          Describe('GetElements results', procedure
            begin
              It('Should reset files', procedure
                begin
                  ExpectSuccess(GetElements(0, '', False, True, @len));
                  ExpectEqual(len, 0);
                end);

              It('Should reset groups', procedure
                begin
                  ExpectSuccess(GetElements(testFile, '', False, True, @len));
                  ExpectEqual(len, 0);
                end);

              It('Should reset records', procedure
                begin
                  ExpectSuccess(GetElements(armo, '', False, True, @len));
                  ExpectEqual(len, 0);
                end);
            end);
        end);
    end);
end;

end.
