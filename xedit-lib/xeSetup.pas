unit xeSetup;

interface

uses
  // xedit units
  wbInterface, wbImplementation;

  // LOADING AND SETUP METHODS
  procedure SetGameMode(mode: Integer); StdCall;
  function GetLoadOrder(str: PAnsiChar; len: Integer): WordBool; StdCall;
  function LoadPlugins(str: PAnsiChar; len: Integer): WordBool; StdCall;
  function GetGlobal(key, value: PAnsiChar; len: Integer): WordBool; StdCall;

var
  Files: array of IwbFile;

implementation

uses
  Classes, SysUtils,
  // lib units
  xeMeta, xeConfiguration, xeMessages;

{******************************************************************************}
{ LOADIND AND SETUP METHODS
  Methods for loading and setting up the API.
}
{******************************************************************************}

procedure SetGameMode(mode: Integer); StdCall;
begin
  try
    SetGame(mode);
    AddMessage('Game: ' + ProgramStatus.GameMode.longName);
    AddMessage('DataPath: ' + settings.gameDataPath);
    AddMessage(' ');
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;


function GetLoadOrder(str: PAnsiChar; len: Integer): WordBool; StdCall;
begin
  Result := false;
  // TODO
end;

function LoadPlugins(str: PAnsiChar; len: Integer): WordBool; StdCall;
begin
  Result := false;
  // TODO
end;

function GetGlobal(key, value: PAnsiChar; len: Integer): WordBool; StdCall;
begin
  Result := false;
  // TODO
end;

end.
