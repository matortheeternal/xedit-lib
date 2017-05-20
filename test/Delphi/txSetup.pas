unit txSetup;

interface

uses
  SysUtils,
  txMeta;

  // LOADING AND SET UP METHODS
  function SetGameMode(mode: Integer): Boolean; cdecl; external 'XEditLib.dll';
  function GetLoadOrder(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl; cdecl; external 'XEditLib.dll';
  function GetLoaderDone: WordBool; cdecl; external 'XEditLib.dll';
  function GetGamePath(gameMode: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING API
  procedure LoadXEdit;

implementation

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
