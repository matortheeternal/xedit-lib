unit txFiles;

interface

uses
  SysUtils;

  // FILE HANDLING METHODS
  function NewFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildFileHandlingTests;

implementation

uses
  maMain;

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

          It('Should return return 0 if a matching file is not loaded', procedure
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

          It('Should return return 0 if index is out of bounds', procedure
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

          It('Should return return 0 if a matching file is not loaded', procedure
            begin
              success := FileByAuthor('U. N. Owen', @h);
              Expect(not success, 'Should return false');
            end);
        end);
    end);
end;

end.
