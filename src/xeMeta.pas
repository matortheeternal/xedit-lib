unit xeMeta;

interface

uses
  Classes, SysUtils, Generics.Collections;

  procedure Initialize; cdecl;
  procedure Finalize; cdecl;
  procedure ExceptionHandler(x: Exception);
  procedure GetMessagesLength(len: PInteger); cdecl;
  procedure GetMessages(str: PWideChar; len: Integer); cdecl;
  procedure ClearMessages; cdecl;
  procedure GetResult(str: PWideChar; len: Integer); cdecl;
  procedure GetExceptionMessageLength(len: PInteger); cdecl;
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; cdecl;
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

implementation

uses
  // mte modules
  mteHelpers,
  // xelib modules
  xeConfiguration, xeMessages;


{******************************************************************************}
{ META METHODS
  Methods which correspond to the overall functioning of the API.
}
{******************************************************************************}

procedure Initialize; cdecl;
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

procedure Finalize; cdecl;
begin
  // TODO: Clear loaded files
  SaveBuffer;
  MessageBuffer.Free;
  _store.Free;
  _releasedIDs.Free;
end;

procedure ExceptionHandler(x: Exception);
begin
  if x.StackTrace <> '' then
    exceptionMessage := Format('%s: %s', [x.StackTrace, x.Message])
  else if x.Message <> '' then   
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
var
  text: String;
begin
  text := MessageBuffer.Text;
  Delete(text, 1, Length(text));
  StrLCopy(str, PWideChar(WideString(text)), len);
end;

procedure GetResult(str: PWideChar; len: Integer); cdecl;
begin
  StrLCopy(str, PWideChar(resultStr), len);
  resultStr := '';
end;

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

function GetGlobal(key, value: PWideChar; len: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    if Globals.IndexOfName(key) > -1 then begin
      StrLCopy(value, PWideChar(WideString(Globals.Values[key])), len);
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
    if (_id = 0) or (_id >= _store.Count) then exit;
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
