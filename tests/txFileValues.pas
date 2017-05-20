unit txFileValues;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildFileValueTests;

implementation

uses
  Mahogany,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles, xeFileValues;
{$ENDIF}
  

procedure BuildFileValueTests;
var
  h: Cardinal;
  str: PWideChar;
  bIsEsm: WordBool;
  nextObjectID, fileHeader: Cardinal;
  count: Integer;
begin
  Describe('File Value Functions', procedure
    begin
      BeforeAll(procedure
        begin
          GetMem(str, 4096);
        end);

      AfterAll(procedure
        begin
          FreeMem(str, 4096);
        end);

      Describe('GetFileName', procedure
        begin
          It('Should match filename used with FileByName', procedure
            begin
              FileByName('Skyrim.esm', @h);
              GetFileName(h, str, 4096);
              Expect(str = 'Skyrim.esm', 'Filename should be Skyrim.esm');
            end);
        end);

      Describe('GetAuthor', procedure
        begin
          It('Should match author used with FileByAuthor', procedure
            begin
              FileByAuthor('mcarofano', @h);
              GetAuthor(h, str, 4096);
              Expect(str = 'mcarofano', 'Author should be mcarofano');
            end);
        end);

      Describe('GetDescription', procedure
        begin
          It('Should return an empty string if plugin has no description', procedure
            begin
              FileByName('Skyrim.esm', @h);
              GetDescription(h, str, 4096);
              Expect(str = '', 'Skyrim.esm''s descriptin should be an empty string');
            end);

          It('Should return the description if defined', procedure
            begin
              FileByName('xtest-1.esp', @h);
              GetDescription(h, str, 4096);
              Expect(str = 'Test plugin for xedit-lib', 'xtest-1.esp''s description should be "Test plugin for xedit-lib"');
            end);
        end);

      Describe('GetIsESM', procedure
        begin
          It('Should return true for ESM files', procedure
            begin
              FileByName('Skyrim.esm', @h);
              GetIsESM(h, @bIsEsm);
              Expect(bIsEsm = true, 'Should return true for Skyrim.esm');
            end);

          It('Should return true for ESP files with the IsESM flag', procedure
            begin
              FileByName('xtest-1.esp', @h);
              GetIsESM(h, @bIsEsm);
              Expect(bIsEsm = true, 'Should return true for xtest-1.esp');
            end);

          It('Should return false for ESP files without the IsESM flag', procedure
            begin
              FileByName('xtest-2.esp', @h);
              GetIsESM(h, @bIsEsm);
              Expect(bIsEsm = false, 'Should return false for xtest-2.esp');
            end);
        end);

      Describe('GetNextObjectID', procedure
        begin
          It('Should return an integer > 0 for Skyrim.esm', procedure
            begin
              FileByName('Skyrim.esm', @h);
              GetNextObjectID(h, @nextObjectID);
              Expect(nextObjectID > 0, 'Should be greater than 0 for Skyrim.esm');
            end);

          It('Should equal 2048 for xtest-1.esp', procedure
            begin
              FileByName('xtest-1.esp', @h);
              GetNextObjectID(h, @nextObjectID);
              Expect(nextObjectID = 2048, 'Should equal 2048 for xtest-1.esp');
            end);
        end);

      Describe('GetFileHeader', procedure
        begin
          It('Should return a handle if input resolves to a file', procedure
            begin
              FileByName('Skyrim.esm', @h);
              GetFileHeader(h, @fileHeader);
              Expect(fileHeader > 0, 'Handle should be greater than 0');
            end);
        end);

      Describe('OverrideRecordCount', procedure
        begin
          It('Should return an integer > 0 for a plugin with overrides', procedure
            begin
              FileByName('Update.esm', @h);
              OverrideRecordCount(h, @count);
              Expect(count > 0, 'Should be greater than 0 for Update.esm');
            end);

          It('Should return 0 for a plugin with no records', procedure
            begin
              FileByName('xtest-1.esp', @h);
              OverrideRecordCount(h, @count);
              Expect(count = 0, 'Should be equal to 0 for xtest-1.esp');
            end);
        end);

      Describe('SetAuthor', procedure
        begin
          It('Should set the author', procedure
            begin
              FileByName('xtest-1.esp', @h);
              SetAuthor(h, PWideChar('Test'));
              GetAuthor(h, str, 4096);
              Expect(str = 'Test', 'Author should be "Test"');
            end);

          It('Should be able to unset the author', procedure
            begin
              FileByName('xtest-1.esp', @h);
              SetAuthor(h, PWideChar(''));
              GetAuthor(h, str, 4096);
              Expect(str = '', 'Author should be an empty string');
            end);
        end);

      Describe('SetDescription', procedure
        begin
          It('Should create element and set description if the plugin has no description element', procedure
            begin
              FileByName('xtest-2.esp', @h);
              SetDescription(h, PWideChar('Test'));
              GetDescription(h, str, 4096);
              Expect(str = 'Test', 'Description should be set to "Test"');
            end);

          It('Should be able to unset the description', procedure
            begin
              FileByName('xtest-2.esp', @h);
              SetDescription(h, PWideChar(''));
              GetDescription(h, str, 4096);
              Expect(str = '', 'Description should be an empty string');
            end);
        end);

      Describe('SetIsESM', procedure
        begin
          It('Should be able to set the ESM flag', procedure
            begin
              FileByName('xtest-2.esp', @h);
              SetIsEsm(h, true);
              GetIsESM(h, @bIsEsm);
              Expect(bIsEsm = true, 'ESM flag should be set');
            end);

          It('Should be able to unset the ESM flag', procedure
            begin
              FileByName('xtest-2.esp', @h);
              SetIsEsm(h, false);
              GetIsESM(h, @bIsEsm);
              Expect(bIsEsm = false, 'ESM flag should be unset');
            end);
        end);

      Describe('SetNextObjectID', procedure
        begin
          It('Should set the next object ID', procedure
            begin
              FileByName('xtest-1.esp', @h);
              SetNextObjectID(h, 4096);
              GetNextObjectID(h, @nextObjectID);
              Expect(nextObjectID = 4096, 'Next Object ID should equal 4096');
            end);
        end);
    end);
end;

end.
