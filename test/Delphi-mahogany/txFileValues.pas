unit txFileValues;

interface

uses
  SysUtils,
  txFileHandling;

  // FILE VALUE METHODS
  function GetFileHeader(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetNextObjectId(_id: Cardinal; nextObjectID: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileName(_id: Cardinal; fileName: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetAuthor(_id: Cardinal; author: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetDescription(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetIsESM(_id: Cardinal; isESM: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetIsESM(_id: Cardinal; isESM: WordBool): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure TestFileValues;

implementation

uses
  maMain;

procedure TestFileValues;
var
  h: Cardinal;
  str: PWideChar;
  bIsEsm: PWordBool;
  nextObjectID, fileHeader: PCardinal;
  count: PInteger;
begin
  Describe('File Value Functions', procedure
    begin
      BeforeAll(procedure
        begin
          GetMem(str, 4096);
        end);

      Describe('GetFileName', procedure
        begin
          It('Should match filename used with FileByName', procedure
            begin
              h := FileByName('Skyrim.esm');
              GetFileName(h, str, 4096);
              Expect(str = 'Skyrim.esm', 'Filename should be Skyrim.esm');
            end);
        end);

      Describe('GetAuthor', procedure
        begin
          It('Should match author used with FileByAuthor', procedure
            begin
              h := FileByAuthor('mcarofano');
              GetAuthor(h, str, 4096);
              Expect(str = 'mcarofano', 'Author should be mcarofano');
            end);
        end);

      Describe('GetDescription', procedure
        begin
          It('Should return an empty string if plugin has no description', procedure
            begin
              h := FileByName('Skyrim.esm');
              GetDescription(h, str, 4096);
              Expect(str = '', 'Skyrim.esm''s descriptin should be an empty string');
            end);

          It('Should return the description if defined', procedure
            begin
              h := FileByName('xtest-1.esp');
              GetDescription(h, str, 4096);
              Expect(str = 'Test plugin for xedit-lib', 'xtest-1.esp''s description should be "Test plugin for xedit-lib"');
            end);
        end);

      Describe('GetIsESM', procedure
        begin
          BeforeEach(procedure
            begin
              GetMem(bIsEsm, 1);
            end);

          AfterEach(procedure
            begin
              FreeMem(bIsEsm, 1);
            end);

          It('Should return true for ESM files', procedure
            begin
              h := FileByName('Skyrim.esm');
              GetIsESM(h, bIsEsm);
              Expect(Assigned(bIsEsm) and bIsEsm^, 'Should return true for Skyrim.esm');
            end);

          It('Should return true for ESP files with the IsESM flag', procedure
            begin
              h := FileByName('xtest-1.esp');
              GetIsESM(h, bIsEsm);
              Expect(bIsEsm^ = true, 'Should return true for xtest-1.esp');
            end);

          It('Should return false for ESP files without the IsESM flag', procedure
            begin
              h := FileByName('xtest-2.esp');
              GetIsESM(h, bIsEsm);
              Expect(bIsEsm^ = false, 'Should return false for xtest-2.esp');
            end);
        end);

      Describe('GetNextObjectID', procedure
        begin
          BeforeEach(procedure
            begin
              GetMem(nextObjectId, 4);
            end);

          AfterEach(procedure
            begin
              FreeMem(nextObjectId, 4);
            end);

          It('Should return an integer > 0 for Skyrim.esm', procedure
            begin
              h := FileByName('Skyrim.esm');
              GetNextObjectID(h, nextObjectID);
              Expect(nextObjectID^ > 0, 'Should be greater than 0 for Skyrim.esm');
            end);

          It('Should equal 2048 for xtest-1.esp', procedure
            begin
              h := FileByName('xtest-1.esp');
              GetNextObjectID(h, nextObjectID);
              Expect(nextObjectID^ = 2048, 'Should equal 2048 for xtest-1.esp');
            end);
        end);

      Describe('GetFileHeader', procedure
        begin
          BeforeEach(procedure
            begin
              GetMem(fileHeader, 4);
            end);

          AfterEach(procedure
            begin
              FreeMem(fileHeader, 4);
            end);

          It('Should return a handle if input resolves to a file', procedure
            begin
              h := FileByName('Skyrim.esm');
              GetFileHeader(h, fileHeader);
              Expect(fileHeader^ > 0, 'Handle should be greater than 0');
            end);
        end);

      Describe('OverrideRecordCount', procedure
        begin
          BeforeEach(procedure
            begin
              GetMem(count, 4);
            end);

          AfterEach(procedure
            begin
              FreeMem(count, 4);
            end);

          It('Should return an integer > 0 for a plugin with overrides', procedure
            begin
              h := FileByName('Update.esm');
              OverrideRecordCount(h, count);
              Expect(count^ > 0, 'Should be greater than 0 for Update.esm');
            end);

          It('Should return 0 for a plugin with no records', procedure
            begin
              h := FileByName('xtest-1.esp');
              OverrideRecordCount(h, count);
              Expect(count^ = 0, 'Should be equal to 0 for xtest-1.esp');
            end);
        end);

      // SetAuthor
      // SetDescription
      // SetIsESM
      // SetNextObjectID

    end);
end;

end.
