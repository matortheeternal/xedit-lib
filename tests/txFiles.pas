unit txFiles;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildFileHandlingTests;

implementation

uses
  Mahogany,
  txMeta,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles;
{$ENDIF}

procedure BuildFileHandlingTests;
var
  h: Cardinal;
  count: Integer;
begin
  Describe('File Handling Functions', procedure
    begin
      Describe('FileByName', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              ExpectSuccess(FileByName('Skyrim.esm', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return false if a matching file is not loaded', procedure
            begin
              ExpectFailure(FileByName('NonExistingFile.esp', @h));
            end);
        end);

      Describe('FileByIndex', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              ExpectSuccess(FileByIndex(1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return false if index is out of bounds', procedure
            begin
              ExpectFailure(FileByIndex(999, @h));
            end);
        end);

      Describe('FileByLoadOrder', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              ExpectSuccess(FileByLoadOrder(1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return false if index is out of bounds', procedure
            begin
              ExpectFailure(FileByLoadOrder(999, @h));
            end);
        end);

      Describe('FileByAuthor', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              ExpectSuccess(FileByAuthor('mcarofano', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return false if a matching file is not loaded', procedure
            begin
              ExpectFailure(FileByAuthor('U. N. Owen', @h));
            end);
        end);

      Describe('OverrideRecordCount', procedure
        begin
          It('Should return an integer > 0 for a plugin with overrides', procedure
            begin
              ExpectSuccess(FileByName('Update.esm', @h));
              ExpectSuccess(OverrideRecordCount(h, @count));
              Expect(count > 0, 'Should be greater than 0 for Update.esm');
            end);

          It('Should return 0 for a plugin with no records', procedure
            begin
              ExpectSuccess(FileByName('xtest-1.esp', @h));
              ExpectSuccess(OverrideRecordCount(h, @count));
              ExpectEqual(count, 0);
            end);
        end);
    end);
end;

end.
