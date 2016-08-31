unit xeMeta;

interface

uses
  Classes, SysUtils, Generics.Collections;

  procedure Initialize; StdCall;
  procedure Finalize; StdCall;
  procedure ExceptionHandler(x: Exception);
  procedure GetBuffer(str: PWideChar; len: Integer); StdCall;
  procedure FlushBuffer; StdCall;
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; StdCall;
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; StdCall;
  function Resolve(_id: Cardinal): IInterface;
  function Store(x: IInterface): Cardinal;
  procedure Release(_id: Cardinal); StdCall;
  procedure ResetStore; StdCall;

var
  _store: TInterfaceList;
  _releasedIDs: TList<Cardinal>;
  nextID: Cardinal;
  exceptionMessage: String;

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

procedure Initialize; StdCall;
begin
  // initialize variables
  MessageBuffer := TStringList.Create;
  _store := TInterfaceList.Create;
  _releasedIDs := TList<Cardinal>.Create;
  exceptionMessage := '';

  // add welcome message
  AddMessage('XEditLib v' + ProgramStatus.ProgramVersion);

  // store global values
  Globals.Values['ProgramPath'] := ExtractFilePath(ParamStr(0));
  Globals.Values['Version'] := ProgramStatus.ProgramVersion;
end;

procedure Finalize; StdCall;
begin
  ResetStore;
  MessageBuffer.Free;
end;

procedure ExceptionHandler(x: Exception);
begin
  exceptionMessage := Format('%s: %s', [x.StackTrace, x.Message]);
  AddMessage(exceptionMessage);
end;

procedure GetBuffer(str: PWideChar; len: Integer); StdCall;
begin
  StrLCopy(str, PWideChar(WideString(MessageBuffer.Text)), len);
end;

procedure FlushBuffer; StdCall;
begin
  MessageBuffer.Clear;
end;

function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; StdCall;
begin
  Result := false;
  if Length(exceptionMessage) > 0 then begin
    StrLCopy(str, PWideChar(WideString(exceptionMessage)), len);
    Result := true;
  end;
end;

function GetGlobal(key, value: PWideChar; len: Integer): WordBool; StdCall;
begin
  Result := false;
  try
    if Globals.IndexOfName(key) > -1 then begin
      StrLCopy(value, PWideChar(WideString(Globals[key])), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function Resolve(_id: Cardinal): IInterface;
begin
  Result := _store[_id];
end;

function Store(x: IInterface): Cardinal;
var
  i: Integer;
begin
  if _releasedIDs.Count > 0 then begin
    i := _releasedIDs.First;
    _store[i] := x;
    _releasedIDs.Delete(0);
    Result := i;
  end
  else
    Result := _store.Add(x);
end;

procedure Release(_id: Cardinal); StdCall;
begin
  _store[_id]._Release;
  _store[_id] := nil;
  _releasedIDs.Add(_id);
end;

procedure ResetStore; StdCall;
var
  i: Integer;
begin
  for i := 0 to Pred(_store.Count) do
    if Assigned(_store[i]) then
      _store[i]._Release;
  _store.Clear;
end;

end.
