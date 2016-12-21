unit txSetup;

interface

uses
  SysUtils,
  txMeta;

  // LOADING AND SET UP METHODS
  procedure SetGameMode(mode: Integer); cdecl; external 'XEditLib.dll';
  function GetLoadOrder(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl; cdecl; external 'XEditLib.dll';
  function GetLoaderDone: WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING API
  procedure LoadXEdit;

implementation

const
  TestLoadOrder =
    'Skyrim.esm'#44 +
    'Update.esm'#44 +
    'xtest-1.esp'#44 +
    'xtest-2.esp'#44 +
    'xtest-3.esp'#44 +
    'xtest-4.esp'#44 +
    'xtest-5.esp';

procedure LoadXEdit;
var
  str: PWideChar;
begin
  SetGameMode(3);
  GetMem(str, 4096); // 16KB for load order, to be safe
  StrLCopy(str, PWideChar(TestLoadOrder), 4096);
  LoadPlugins(str);
  while not GetLoaderDone do begin
    WriteBuffer;
    Sleep(100);
  end;
  WriteBuffer;
  WriteLn(' ');
end;

end.
