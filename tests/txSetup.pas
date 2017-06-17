unit txSetup;

interface

  // PUBLIC TESTING API
  procedure BuildSetupTests;

implementation

uses
  Windows, SysUtils, Classes,
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeSetup, xeMeta, xeMessages, xeFiles,
{$ENDIF}
  txMeta;

const
  TestLoadOrder =
    'Skyrim.esm'#13 +
    'Update.esm'#13 +
    {$IFDEF LOAD_DLC}
    'Dawnguard.esm'#13 +
    'HearthFires.esm'#13 +
    'Dragonborn.esm'#13 +
    {$ENDIF}
    'xtest-1.esp'#13 +
    'xtest-2.esp'#13 +
    'xtest-3.esp'#13 +
    'xtest-4.esp'#13 +
    'xtest-5.esp';

procedure BackupFile(filePath: String);
begin
  if FileExists(filePath) then
    RenameFile(filePath, filePath + '.bak');
end;

procedure RestoreFile(filePath: String);
begin
  DeleteFile(filePath);
  if FileExists(filePath + '.bak') then
    RenameFile(filePath + '.bak', filePath);
end;

procedure TestLoader(expectedTime: Double);
var
  n: Integer;
begin
  Expect(not GetLoaderDone, 'Loader should take time');
  n := 0;
  while not GetLoaderDone do begin
    WriteMessages;
    Inc(n);
    Sleep(100);
  end;
  Expect(n < expectedTime * 10, 'Loader should complete in under ' + FloatToStr(expectedTime) + ' seconds');
  WriteMessages;
end;

procedure BuildSetupTests;
var
  appDataPath, loadOrder, plugins: String;
  len, p, n, m: Integer;
  h: Cardinal;
begin
  Describe('Setup', procedure
    begin
      Describe('SetGameMode', procedure
        begin
          AfterAll(procedure
            begin
              ExpectSuccess(GetGlobal('AppDataPath', @len));
              appDataPath := grs(len);
            end);

          It('Should succeed for the first time for Skyrim game mode', procedure
            begin
              ExpectSuccess(SetGameMode(3));
            end);

          It('Should fail the second time', procedure
            begin
              ExpectFailure(SetGameMode(4));
            end);
        end);

      Describe('GetLoadOrder', procedure
        begin
          BeforeAll(procedure
            begin
              BackupFile(appDataPath + 'loadorder.txt');
            end);

          AfterAll(procedure
            begin
              RestoreFile(appDataPath + 'loadorder.txt');
            end);

          It('Should use loadorder.txt from AppData', procedure
            begin
              WriteStringToFile('xtest-5.esp'#13#10' '#13#10'# comment'#13#10'NonExistingPlugin.esp', appDataPath + 'loadorder.txt');
              ExpectSuccess(GetLoadOrder(@len));
              loadOrder := grs(len);
              Expect(Pos('xtest-5.esp', loadOrder) > 0, 'Should contain xtest-5.esp');
            end);

          It('Should remove comments and empty lines', procedure
            begin
              Expect(Pos(' '#13#10, loadOrder) = 0, 'Empty line should not be present');
              Expect(Pos('# comment', loadOrder) = 0, 'Comment should not be present');
            end);

          It('Should remove files that do not exist', procedure
            begin
              Expect(Pos('NonExistingPlugin.esp', loadOrder) = 0, 'Should not contain NonExistingPlugin.esp');
            end);

          It('Should add missing plugins', procedure
            begin
              p := Pos('xtest-5.esp', loadOrder);
              Expect(Pos('xtest-1.esp', loadOrder) > p, 'Should contain xtest-1.esp');
              Expect(Pos('xtest-2.esp', loadOrder) > p, 'Should contain xtest-2.esp');
              Expect(Pos('xtest-3.esp', loadOrder) > p, 'Should contain xtest-3.esp');
              Expect(Pos('xtest-4.esp', loadOrder) > p, 'Should contain xtest-4.esp');
            end);

          It('Should add required plugins in correct positions', procedure
            begin
              p := Pos('xtest-5.esp', loadOrder);
              n := Pos('Skyrim.esm', loadOrder);
              Expect(n > 0, 'Skyrim.esm should be included');
              Expect(n < p, 'Skyrim.esm should be before xtest-5.esp');
              m := Pos('Update.esm', loadOrder);
              Expect(m > n, 'Skyrim.esm should be included and after Skyrim.esm');
              Expect(m < p, 'Update.esm should be before xtest-5.esp');
            end);
        end);

      Describe('GetActivePlugins', procedure
        begin
          BeforeAll(procedure
            begin
              BackupFile(appDataPath + 'plugins.txt');
            end);

          AfterAll(procedure
            begin
              RestoreFile(appDataPath + 'plugins.txt');
            end);

          It('Should get list of active plugins from plugins.txt', procedure
            begin
              WriteStringToFile('xtest-5.esp'#13#10' '#13#10'# comment'#13#10'NonExistingPlugin.esp', appDataPath + 'plugins.txt');
              ExpectSuccess(GetActivePlugins(@len));
              plugins := grs(len);
            end);

          It('Should remove comments and empty lines', procedure
            begin
              Expect(Pos(' '#13#10, plugins) = 0, 'Empty line should not be present');
              Expect(Pos('# comment', plugins) = 0, 'Comment should not be present');
            end);

          It('Should remove files that do not exist', procedure
            begin
              Expect(Pos('NonExistingPlugin.esp', plugins) = 0, 'Should not contain NonExistingPlugin.esp');
            end);

          It('Should add required plugins', procedure
            begin
              Expect(Pos('Skyrim.esm', loadOrder) > 0, 'Skyrim.esm should be present');
              Expect(Pos('Update.esm', loadOrder) > 0, 'Skyrim.esm should be present');
            end);
        end);

      Describe('LoadPlugins', procedure
        begin
          It('Should load plugins based on input load order', procedure
            begin
              WriteLn(' ');
              ExpectSuccess(LoadPlugins(TestLoadOrder));
              TestLoader(10);
              WriteLn(' ');
            end);

          It('Should set FileCount global', procedure
            begin
              ExpectSuccess(GetGlobal('FileCount', @len));
              ExpectEqual(grs(len), '8');
            end);
        end);

      {Describe('BuildReferences', procedure
        begin
          It('Should build references for the input plugin', procedure
            begin
              WriteLn(' ');
              ExpectSuccess(FileByName('xtest-2.esp', @h));
              ExpectSuccess(BuildReferences(h));
              while not GetLoaderDone do
                Sleep(100);
              WriteMessages;
              WriteLn(' ');
            end);
        end);}

      Describe('UnloadPlugin', procedure
        begin
          It('Should fail if plugin is required by other loaded plugins', procedure
            begin
              ExpectSuccess(FileByName('Update.esm', @h));
              ExpectFailure(UnloadPlugin(h));
            end);

          {It('Should fail if references have been built for the plugin', procedure
            begin
              ExpectSuccess(FileByName('xtest-2.esp', @h));
              ExpectFailure(UnloadPlugin(h));
            end);}

          It('Else it should successfully unload plugins', procedure
            begin
              ExpectSuccess(FileByName('xtest-5.esp', @h));
              ExpectSuccess(UnloadPlugin(h));
            end);

          It('Should update FileCount global', procedure
            begin
              ExpectSuccess(GetGlobal('FileCount', @len));
              ExpectEqual(grs(len), '7');
            end);
        end);

      Describe('LoadPlugin', procedure
        begin
          It('Should successfully load plugins', procedure
            begin
              WriteLn(' ');
              ExpectSuccess(LoadPlugin('xtest-5.esp'));
              TestLoader(0.5);
              WriteLn(' ');
            end);

          It('Should update FileCount global', procedure
            begin
              ExpectSuccess(GetGlobal('FileCount', @len));
              ExpectEqual(grs(len), '8');
            end);
        end);
    end);
end;

end.
