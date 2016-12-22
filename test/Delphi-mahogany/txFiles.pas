unit txFiles;

interface

uses
  SysUtils;

  // FILE HANDLING METHODS
  function NewFile(filename: PAnsiChar): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByIndex(index: Integer): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByName(name: PAnsiChar): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByAuthor(author: PAnsiChar): Cardinal; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal): Cardinal; cdecl; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileNames(fileNames: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure TestFileHandling;

implementation

uses
  maMain;

procedure TestFileHandling;
var
  h: Cardinal;
begin
  Describe('File Handling Functions', procedure
    begin
      Describe('FileByName', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              h := FileByName('Skyrim.esm');
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return 0 if a matching file is not loaded', procedure
            begin
              h := FileByName('NonExistingFile.esp');
              Expect(h = 0, 'Handle should be 0');
            end);
        end);

      Describe('FileByIndex', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              h := FileByIndex(1);
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return 0 if index is out of bounds', procedure
            begin
              h := FileByIndex(999);
              Expect(h = 0, 'Handle should be 0');
            end);
        end);

      Describe('FileByLoadOrder', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              h := FileByLoadOrder(1);
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return 0 if index is out of bounds', procedure
            begin
              h := FileByLoadOrder(999);
              Expect(h = 0, 'Handle should be 0');
            end);
        end);

      Describe('FileByAuthor', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              h := FileByAuthor('mcarofano');
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return 0 if a matching file is not loaded', procedure
            begin
              h := FileByAuthor('U. N. Owen');
              Expect(h = 0, 'Handle should be 0');
            end);
        end);
    end);
end;

end.
