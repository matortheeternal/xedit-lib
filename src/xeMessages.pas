unit xeMessages;

interface

uses
  SysUtils;

  // API METHODS
  procedure GetMessagesLength(len: PInteger); cdecl;
  function GetMessages(str: PWideChar; maxLen: Integer): WordBool; cdecl;
  procedure ClearMessages; cdecl;
  procedure GetExceptionMessageLength(len: PInteger); cdecl;
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;

  // NATIVE METHODS
  procedure ExceptionHandler(x: Exception);
  procedure AddMessage(msg: String);
  procedure SaveMessages;

const
  LineBreak = #13#10;

var
  Messages: WideString;
  ExceptionMessage: WideString;

implementation

uses
  Classes,
  xeMeta;

// NATIVE METHODS
procedure ExceptionHandler(x: Exception);
begin
  if x.Message <> '' then
    ExceptionMessage := x.Message
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

procedure GetExceptionMessageLength(len: PInteger); cdecl;
begin
  len^ := Length(exceptionMessage);
end;

function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    if Length(exceptionMessage) > 0 then begin
      Result := xStrCopy(exceptionMessage, str, len);
      exceptionMessage := '';
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// API METHODS
procedure GetMessagesLength(len: PInteger); cdecl;
begin
  len^ := Length(Messages);
end;

function GetMessages(str: PWideChar; maxLen: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    StrLCopy(str, PWideChar(Messages), maxLen);
    Delete(Messages, 1, maxLen);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

procedure ClearMessages; cdecl;
begin
  Messages := '';
end;

end.
