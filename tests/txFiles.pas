unit txFiles;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildFileHandlingTests;

implementation

uses
  Mahogany,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles;
{$ENDIF}

procedure BuildFileHandlingTests;
var
  success: WordBool;
  h: Cardinal;
begin
  Describe('File Handling Functions', procedure
    begin
      Describe('FileByName', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              success := FileByName('Skyrim.esm', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return return false if a matching file is not loaded', procedure
            begin
              success := FileByName('NonExistingFile.esp', @h);
              Expect(not success, 'Should return false');
            end);
        end);

      Describe('FileByIndex', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              success := FileByIndex(1, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return false if index is out of bounds', procedure
            begin
              success := FileByIndex(999, @h);
              Expect(not success, 'Should return false');
            end);
        end);

      Describe('FileByLoadOrder', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              success := FileByLoadOrder(1, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return return false if index is out of bounds', procedure
            begin
              success := FileByLoadOrder(999, @h);
              Expect(not success, 'Should return false');
            end);
        end);

      Describe('FileByAuthor', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              success := FileByAuthor('mcarofano', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return return false if a matching file is not loaded', procedure
            begin
              success := FileByAuthor('U. N. Owen', @h);
              Expect(not success, 'Should return false');
            end);
        end);
    end);
end;

end.
