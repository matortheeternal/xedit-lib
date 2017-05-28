unit txSetup;

interface

uses
  SysUtils;

  // PUBLIC TESTING API
  procedure LoadXEdit;

implementation

uses
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeSetup, xeMeta,
{$ENDIF}
  txMeta;

const
  TestLoadOrder =
    'Skyrim.esm'#13 +
    'Update.esm'#13 +
    'xtest-1.esp'#13 +
    'xtest-2.esp'#13 +
    'xtest-3.esp'#13 +
    'xtest-4.esp'#13 +
    'xtest-5.esp';

procedure LoadXEdit;
begin
  SetGameMode(3);
  LoadPlugins(TestLoadOrder);
  while not GetLoaderDone do begin
    WriteMessages;
    Sleep(100);
  end;
  WriteMessages;
  WriteLn(' ');
end;

end.
