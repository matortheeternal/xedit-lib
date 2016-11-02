unit xeMeta;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  CardinalArray = array of PCardinal;
  PCardinalArray = ^CardinalArray;

  procedure Initialize; StdCall;
  procedure Finalize; StdCall;
  procedure ExceptionHandler(x: Exception);
  procedure GetBuffer(str: PWideChar; len: Integer); StdCall;
  procedure FlushBuffer; StdCall;
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; StdCall;
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; StdCall;
  procedure StoreIfAssigned(var x: IInterface; var _res: PCardinal; var Success: WordBool);
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
  _store.Add(nil);
  exceptionMessage := '';

  // add welcome message
  AddMessage('XEditLib v' + ProgramStatus.ProgramVersion);

  // store global values
  Globals.Values['ProgramPath'] := ExtractFilePath(ParamStr(0));
  Globals.Values['Version'] := ProgramStatus.ProgramVersion;
end;

procedure Finalize; StdCall;
begin
  // TODO: Clear loaded files
  MessageBuffer.Free;
  _store.Free;
  _releasedIDs.Free;
end;

procedure ExceptionHandler(x: Exception);
begin
  exceptionMessage := Format('%s: %s', [x.StackTrace, x.Message]);
  AddMessage(exceptionMessage);
end;

procedure GetBuffer(str: PWideChar; len: Integer); StdCall;
var
  text: String;
begin
  text := MessageBuffer.Text;
  Delete(text, Length(text), 1);
  StrLCopy(str, PWideChar(WideString(text)), len);
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
      StrLCopy(value, PWideChar(WideString(Globals.Values[key])), len);
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

procedure StoreIfAssigned(var x: IInterface; var _res: PCardinal; var Success: WordBool);
begin
  if Assigned(x) then begin
    _res^ := Store(x);
    Success := true;
  end;
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
  if _id = 0 then exit;
  _store[_id]._Release;
  _store[_id] := nil;
  _releasedIDs.Add(_id);
end;

procedure ResetStore; StdCall;
begin
  _store.Clear;
  _store.Add(nil);
end;

end.
