unit xeMessages;

interface

uses
  SysUtils;

  {$region 'Native functions'}
  procedure ExceptionHandler(x: Exception);
  procedure AddMessage(msg: String);
  procedure SaveMessages;
  {$endregion}

  {$region 'API functions'}
  procedure GetMessagesLength(len: PInteger); cdecl;
  function GetMessages(str: PWideChar; maxLen: Integer): WordBool; cdecl;
  procedure ClearMessages; cdecl;
  procedure GetExceptionMessageLength(len: PInteger); cdecl;
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;
  {$endregion}

const
  LineBreak = #13#10;

var
  LogPosition: Integer;
  Messages: WideString;
  ExceptionMessage: WideString;

implementation

uses
  Classes,
  xeMeta;

{$region 'Native functions'}
procedure ExceptionHandler(x: Exception);
begin
  if x.Message <> '' then
    ExceptionMessage := Copy(x.Message, 1, Length(x.Message))
  else
    ExceptionMessage := 'Unknown exception.';
  AddMessage(ExceptionMessage);
end;

procedure AddMessage(msg: String);
begin
  Messages := Messages + msg + LineBreak;
end;

procedure SaveMessages;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Messages;
    sl.SaveToFile('xelib_log.txt');
  finally
    sl.Free;
  end;
end;
{$endregion}

{$region 'API functions'}
procedure GetExceptionMessageLength(len: PInteger); cdecl;
begin
  len^ := Length(ExceptionMessage);
end;

function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    if Length(ExceptionMessage) > 0 then begin
      Result := xStrCopy(ExceptionMessage, str, len);
      ExceptionMessage := '';
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{$POINTERMATH ON}
procedure GetMessagesLength(len: PInteger); cdecl;
begin
  len^ := Length(PWideChar(Messages) + LogPosition);
end;

function GetMessages(str: PWideChar; maxLen: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    StrLCopy(str, PWideChar(Messages) + LogPosition, maxLen);
    Inc(LogPosition, Length(str));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$POINTERMATH OFF}

procedure ClearMessages; cdecl;
begin
  LogPosition := 0;
  Messages := '';
end;
{$endregion}

initialization
  LogPosition := 0;
  Messages := '';
  ExceptionMessage := '';

end.
