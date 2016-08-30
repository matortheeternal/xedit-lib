unit xeSetup;

interface

uses
  // xedit units
  wbInterface, wbImplementation;

  // LOADING AND SETUP METHODS
  procedure SetGameMode(mode: Integer); StdCall;
  function GetLoadOrder(str: PAnsiChar; len: Integer): WordBool; StdCall;
  function LoadPlugins(str: PAnsiChar; len: Integer): WordBool; StdCall;
  function GetGlobal(key, result: PAnsiChar; len: Integer): WordBool; StdCall;

var
  Files: array of IwbFile;

implementation

uses
  Classes,
  // lib units
  xeConfiguration, xeMessages;

{******************************************************************************}
{ LOADIND AND SETUP METHODS
  Methods for loading and setting up the API.
}
{******************************************************************************}

procedure SetGameMode(mode: Integer); StdCall;
begin
  SetGame(mode);
  AddMessage('Game: ' + ProgramStatus.GameMode.longName);
  AddMessage('DataPath: ' + settings.gameDataPath);
  AddMessage(' ');
end;


function GetLoadOrder(str: PAnsiChar; len: Integer): WordBool; StdCall;
begin
end;

function LoadPlugins(str: PAnsiChar; len: Integer): WordBool; StdCall;
begin
end;

function GetGlobal(key, result: PAnsiChar; len: Integer): WordBool; StdCall;
begin
end;

end.
