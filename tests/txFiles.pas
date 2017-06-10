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
        
      Describe('SaveFile', procedure
        begin
          It('Should return true if it succeeded in saving the file', procedure
            begin
              ExpectSuccess(SaveFile(3));
            end);

          It('Should return false if the index is out of bounds', procedure
            begin
              ExpectFailure(SaveFile(999));
            end);
        end);

      {$IF false}
      Describe('AddFile', procedure
        var
          i: Integer;
        begin
          It('Should return true if it succeeds', procedure
            begin
              ExpectSuccess(AddFile('abc.esp', @h));
            end);

          It('Should return false if the file already exists', procedure
            begin
              ExpectFailure(AddFile('Dawnguard.esm', @h));
            end);

          {$IF false}
          It('Should return false if the load order is already full', procedure
            begin
              for i := GetGlobal('FileCount') to 254 do
                AddFile(Concat(IntToStr(i),'.esp', @h));
              ExpectFailure(AddFile('255.esp', @h));
            end);
          {$IFEND}
        end);
      {$IFEND}
    end);
end;

end.
