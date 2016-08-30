unit xeMeta;

interface

  procedure Initialize; StdCall;
  procedure Finalize; StdCall;
  procedure GetBuffer(str: PAnsiChar; len: Integer); StdCall;
  procedure FlushBuffer; StdCall;
  function Resolve(_id: Cardinal): IInterface;
  function Store(x: IInterface): Cardinal;
  procedure ResetStore; StdCall;

var
  _store: Array[0..$FFFFFFFF] of IInterface;
  last_id: Cardinal;

implementation

uses
  SysUtils, Classes,
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
  AddMessage('XEditLib v' + ProgramStatus.ProgramVersion);
  last_id := 0;

  // get program path
  PathList.Values['ProgramPath'] := ExtractFilePath(ParamStr(0));
end;

procedure Finalize; StdCall;
begin
  ResetStore;
  MessageBuffer.Free;
end;

procedure GetBuffer(str: PAnsiChar; len: Integer); StdCall;
begin
  StrLCopy(str, PAnsiChar(AnsiString(MessageBuffer.Text)), len);
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
begin
  _store[last_id] = x;
  Result := last_id;
  Inc(last_id);
  if last_id = $FFFFFFFF then begin
    AddMessage('WARNING: STORE OVERFLOW.');
    last_id := 0;
  end;
end;

procedure ResetStore; StdCall;
var
  i: Integer;
begin
  for i := 0 to Pred(last_id) do begin
    if Assigned(_store[i]) then
      _store[i]._Release;
  end;
  last_id = 0;
end;

end.
