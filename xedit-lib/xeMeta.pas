unit xeMeta;

interface

uses
  Classes, SysUtils, Generics.Collections;

  procedure Initialize; StdCall;
  procedure Finalize; StdCall;
  procedure ExceptionHandler(x: Exception);
  procedure GetBuffer(str: PWideChar; len: Integer); StdCall;
  procedure FlushBuffer; StdCall;
  function Resolve(_id: Cardinal): IInterface;
  function Store(x: IInterface): Cardinal;
  procedure Release(_id: Cardinal); StdCall;
  procedure ResetStore; StdCall;

var
  _store: TInterfaceList;
  _releasedIDs: TList<Cardinal>;
  nextID: Cardinal;

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
  MessageBuffer := TStringList.Create;
  _store := TInterfaceList.Create;
  _releasedIDs := TList<Cardinal>.Create;
  AddMessage('XEditLib v' + ProgramStatus.ProgramVersion);

  // get program path
  PathList.Values['ProgramPath'] := ExtractFilePath(ParamStr(0));
end;

procedure Finalize; StdCall;
begin
  ResetStore;
  MessageBuffer.Free;
end;

procedure ExceptionHandler(x: Exception);
begin
  AddMessage(Format('%s: %s', [x.StackTrace, x.Message]));
end;

procedure GetBuffer(str: PWideChar; len: Integer); StdCall;
begin
  StrLCopy(str, PWideChar(WideString(MessageBuffer.Text)), len);
end;

procedure FlushBuffer; StdCall;
begin
  MessageBuffer.Clear;
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
