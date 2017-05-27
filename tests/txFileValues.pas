unit txFileValues;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildFileValueTests;

implementation

uses
  Mahogany,
  txMeta,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles, xeFileValues;
{$ENDIF}
  

procedure BuildFileValueTests;
var
  h, skyrim, update, xt1, xt2, xt3, v: Cardinal;
  str: PWideChar;
  b: WordBool;
  count: Integer;
begin
  Describe('File Value Functions', procedure
    begin
      BeforeAll(procedure
        begin
          GetMem(str, 4096);
          FileByName('Skyrim.esm', @skyrim);
          FileByName('Update.esm', @update);
          FileByName('xtest-1.esp', @xt1);
          FileByName('xtest-2.esp', @xt2);
          FileByName('xtest-3.esp', @xt3);
        end);

      AfterAll(procedure
        begin
          FreeMem(str, 4096);
        end);

      Describe('GetFileName', procedure
        begin
          It('Should return filename', procedure
            begin
              ExpectSuccess(GetFileName(skyrim, str, 4096));
              ExpectEqual(string(str), 'Skyrim.esm');
            end);
        end);

      Describe('GetAuthor', procedure
        begin
          It('Should return file author', procedure
            begin
              ExpectSuccess(GetAuthor(skyrim, str, 4096));
              ExpectEqual(string(str), 'mcarofano');
            end);
        end);

      Describe('GetDescription', procedure
        begin
          It('Should return an empty string if plugin has no description', procedure
            begin
              ExpectSuccess(GetDescription(skyrim, str, 4096));
              ExpectEqual(string(str), '');
            end);

          It('Should return the description if defined', procedure
            begin
              ExpectSuccess(GetDescription(xt1, str, 4096));
              ExpectEqual(string(str), 'Test plugin for xedit-lib');
            end);
        end);

      Describe('GetIsESM', procedure
        begin
          It('Should return true for ESM files', procedure
            begin
              ExpectSuccess(GetIsESM(skyrim, @b));
              Expect(b, 'Should return true for Skyrim.esm');
            end);

          It('Should return true for ESP files with the IsESM flag', procedure
            begin
              ExpectSuccess(GetIsESM(xt1, @b));
              Expect(b , 'Should return true for xtest-1.esp');
            end);

          It('Should return false for ESP files without the IsESM flag', procedure
            begin
              ExpectSuccess(GetIsESM(xt2, @b));
              Expect(not b, 'Should return false for xtest-2.esp');
            end);
        end);

      Describe('GetNextObjectID', procedure
        begin
          It('Should return an integer > 0 for Skyrim.esm', procedure
            begin
              ExpectSuccess(GetNextObjectID(skyrim, @v));
              Expect(v > 0, 'Should be greater than 0 for Skyrim.esm');
            end);

          It('Should equal 2048 for xtest-1.esp', procedure
            begin
              ExpectSuccess(GetNextObjectID(xt1, @v));
              ExpectEqual(v, 2048);
            end);
        end);

      Describe('GetFileHeader', procedure
        begin
          It('Should return a handle if input resolves to a file', procedure
            begin
              ExpectSuccess(GetFileHeader(skyrim, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);
        end);

      Describe('OverrideRecordCount', procedure
        begin
          It('Should return an integer > 0 for a plugin with overrides', procedure
            begin
              ExpectSuccess(OverrideRecordCount(update, @count));
              Expect(count > 0, 'Should be greater than 0 for Update.esm');
            end);

          It('Should return 0 for a plugin with no records', procedure
            begin
              ExpectSuccess(OverrideRecordCount(xt1, @count));
              ExpectEqual(count, 0);
            end);
        end);

      Describe('SetAuthor', procedure
        begin
          It('Should set the author', procedure
            begin
              ExpectSuccess(SetAuthor(xt1, 'Test'));
              ExpectSuccess(GetAuthor(xt1, str, 4096));
              ExpectEqual(string(str), 'Test');
            end);

          It('Should be able to unset the author', procedure
            begin
              ExpectSuccess(SetAuthor(xt1, ''));
              ExpectSuccess(GetAuthor(xt1, str, 4096));
              ExpectEqual(string(str), '');
            end);
        end);

      Describe('SetDescription', procedure
        begin
          It('Should create element and set description if the plugin has no description element', procedure
            begin
              ExpectSuccess(SetDescription(xt2, 'Test'));
              ExpectSuccess(GetDescription(xt2, str, 4096));
              ExpectEqual(string(str), 'Test');
            end);

          It('Should be able to unset the description', procedure
            begin
              ExpectSuccess(SetDescription(xt2, ''));
              ExpectSuccess(GetDescription(xt2, str, 4096));
              ExpectEqual(string(str), '');
            end);
        end);

      Describe('SetIsESM', procedure
        begin
          It('Should be able to set the ESM flag', procedure
            begin
              ExpectSuccess(SetIsEsm(xt2, True));
              ExpectSuccess(GetIsESM(xt2, @b));
              Expect(b, 'ESM flag should be set');
            end);

          It('Should be able to unset the ESM flag', procedure
            begin
              ExpectSuccess(SetIsEsm(xt2, False));
              ExpectSuccess(GetIsESM(xt2, @b));
              Expect(not b, 'ESM flag should be unset');
            end);
        end);

      Describe('SetNextObjectID', procedure
        begin
          It('Should set the next object ID', procedure
            begin
              ExpectSuccess(SetNextObjectID(xt1, 4096));
              ExpectSuccess(GetNextObjectID(xt1, @v));
              ExpectEqual(v, 4096);
            end);
        end);
    end);
end;

end.
