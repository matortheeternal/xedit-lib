unit xeMeta;

interface

uses
  Classes, SysUtils, Generics.Collections;

  procedure InitXEdit; cdecl;
  procedure CloseXEdit; cdecl;
  procedure ExceptionHandler(x: Exception);
  procedure GetMessagesLength(len: PInteger); cdecl;
  procedure GetMessages(str: PWideChar; len: Integer); cdecl;
  procedure ClearMessages; cdecl;
  procedure GetResultString(str: PWideChar; len: Integer); cdecl;
  procedure GetResultArray(_res: PCardinal; len: Integer); cdecl;
  procedure GetExceptionMessageLength(len: PInteger); cdecl;
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;
  function GetGlobal(key: PWideChar; len: PInteger): WordBool; cdecl;
  procedure StoreIfAssigned(var x: IInterface; var _res: PCardinal; var Success: WordBool);
  function Resolve(_id: Cardinal): IInterface;
  function Store(x: IInterface): Cardinal;
  function Release(_id: Cardinal): WordBool; cdecl;
  function ResetStore: WordBool; cdecl;

var
  _store: TInterfaceList;
  _releasedIDs: TList<Cardinal>;
  nextID: Cardinal;
  exceptionMessage: String;
  resultStr: String;
  resultArray: array of Cardinal;

implementation

uses
  wbImplementation, wbInterface,
  // mte modules
  mteHelpers,
  // xelib modules
  xeConfiguration, xeMessages, xeSetup;


{******************************************************************************}
{ META METHODS
  Methods which correspond to the overall functioning of the API.
}
{******************************************************************************}

procedure InitXEdit; cdecl;
begin
  // initialize variables
  MessageBuffer := TStringList.Create;
  _store := TInterfaceList.Create;
  _releasedIDs := TList<Cardinal>.Create;
  _store.Add(nil);
  exceptionMessage := '';
  resultStr := '';

  // add welcome message
  AddMessage('XEditLib v' + ProgramStatus.ProgramVersion);

  // store global values
  Globals.Values['ProgramPath'] := ExtractFilePath(ParamStr(0));
  Globals.Values['Version'] := ProgramStatus.ProgramVersion;
end;

procedure CloseXEdit; cdecl;
begin
  SaveBuffer;
  settings.Free;
  MessageBuffer.Free;
  _releasedIDs.Free;
  _store.Free;
  SetLength(xFiles, 0);
  xFiles := nil;
  wbFileForceClosed;
  if Assigned(wbContainerHandler) then
    wbContainerHandler._Release;
end;

procedure ExceptionHandler(x: Exception);
begin
  if x.Message <> '' then
    exceptionMessage := x.Message
  else
    exceptionMessage := 'Unknown exception.';
  AddMessage(exceptionMessage);
end;

procedure GetMessagesLength(len: PInteger); cdecl;
begin
  len^ := Length(MessageBuffer.Text) * SizeOf(WideChar);
end;

procedure GetMessages(str: PWideChar; len: Integer); cdecl;
begin
  StrLCopy(str, PWideChar(MessageBuffer.Text), len);
end;

procedure GetResultString(str: PWideChar; len: Integer); cdecl;
begin
  StrLCopy(str, PWideChar(resultStr), len);
  resultStr := '';
end;

{$POINTERMATH ON}
procedure GetResultArray(_res: PCardinal; len: Integer); cdecl;
var
  i: Integer;
begin
  for i := 0 to High(resultArray) do begin
    if i >= len then break;
    _res[i] := resultArray[i];
  end;
  SetLength(resultArray, 0);
end;
{$POINTERMATH OFF}

procedure ClearMessages; cdecl;
begin
  MessageBuffer.Clear;
end;

procedure GetExceptionMessageLength(len: PInteger); cdecl;
begin
  len^ := Length(exceptionMessage) * SizeOf(WideChar);
end;

function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    if Length(exceptionMessage) > 0 then begin
      StrLCopy(str, PWideChar(exceptionMessage), len);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetGlobal(key: PWideChar; len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    if Globals.IndexOfName(key) > -1 then begin
      resultStr := Globals.Values[key];
      len^ := Length(resultStr) * SizeOf(WideChar);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function Resolve(_id: Cardinal): IInterface;
begin
  if _id = 0 then raise Exception.Create('ERROR: Cannot resolve NULL reference.');
  Result := _store[_id];
end;

procedure StoreIfAssigned(var x: IInterface; var _res: PCardinal; var Success: WordBool);
begin
  if Assigned(x) then begin
    _res^ := Store(x);
    Success := True;
  end;
end;

function Store(x: IInterface): Cardinal;
var
  i: Integer;
begin
  if _releasedIDs.Count > 0 then begin
    i := _releasedIDs[0];
    _store[i] := x;
    _releasedIDs.Delete(0);
    Result := i;
  end
  else
    Result := _store.Add(x);
end;

function Release(_id: Cardinal): WordBool; cdecl;
begin
  Result := False;
  try
    if (_id = 0) or (_id >= Cardinal(_store.Count)) then exit;
    _store[_id]._Release;
    _store[_id] := nil;
    _releasedIDs.Add(_id);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ResetStore: WordBool; cdecl;
begin
  Result := False;
  try
    _store.Clear;
    _releasedIDs.Clear;
    _store.Add(nil);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
