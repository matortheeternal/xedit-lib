unit xeSetup;

interface

uses
  Classes,
  // xedit units
  wbInterface, wbImplementation;

type
  TLoaderThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  procedure SetGameMode(mode: Integer); StdCall;
  function GetLoadOrder(str: PAnsiChar; len: Integer): WordBool; StdCall;
  function LoadPlugins(str: PAnsiChar; len: Integer): WordBool; StdCall;
  function GetGlobal(key, value: PAnsiChar; len: Integer): WordBool; StdCall;

var
  Files: array of IwbFile;
  slLoadOrder: TStringList;

implementation

uses
  SysUtils,
  // mte units
  mteHelpers,
  // xelib units
  xeMeta, xeConfiguration, xeMessages;

{******************************************************************************}
{ LOADIND AND SETUP METHODS
  Methods for setting the xEdit game mode and loading files.
}
{******************************************************************************}

procedure TLoaderThread.Execute;
var
  i: Integer;
  sFileName: String;
  _file: IwbFile;
begin
  try
    for i := 0 to Pred(slLoadOrder.Count) do begin
      sFileName := slLoadOrder[i];
      AddMessage(Format('Loading %s (%d/%d)', [sFileName, i + 1, slLoadOrder.Count]));

      // load plugin
      try
        _file := wbFile(wbDataPath + sFileName, i, '', false, false);
        _file._AddRef;
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := _file;
      except
        on x: Exception do begin
          AddMessage('Exception loading ' + sFileName);
          AddMessage(x.Message);
          raise x;
        end;
      end;

      // load hardcoded dat
      if i = 0 then try
        _file := wbFile(Globals.Values['ProgramPath'] + wbGameName + wbHardcodedDat, 0);
        _file._AddRef;
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := _file;
      except
        on x: Exception do begin
          AddMessage('Exception loading ' + wbGameName + wbHardcodedDat);
          raise x;
        end;
      end;
    end;

    // done loading
    ProgramStatus.bLoaderDone := true;
    AddMessage('Done loading files.');
  except
    on E: Exception do begin
      AddMessage('Fatal Error: <' + e.ClassName + ': ' + e.Message + '>');
      wbLoaderError := true;
    end;
  end;
end;

procedure SetGameMode(mode: Integer); StdCall;
begin
  try
    SetGame(mode);
    // set global values
    Globals.Values['GameName'] := ProgramStatus.GameMode.gameName;
    Globals.Values['AppName'] := ProgramStatus.GameMode.appName;
    Globals.Values['LongGameName'] := ProgramStatus.GameMode.longName;
    Globals.Values['DataPath'] := settings.gameDataPath;
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
