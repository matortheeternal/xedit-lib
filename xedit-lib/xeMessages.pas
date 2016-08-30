unit xeMessages;

interface

uses
  Classes;

  procedure AddMessage(msg: String);
  procedure SaveBuffer;

var
  MessageBuffer: TStringList;

implementation

procedure AddMessage(msg: String);
begin
  {$IFDEF CONSOLE}
  WriteLn(msg);
  {$ELSE}
  MessageBuffer.Add(msg);
  {$ENDIF}
end;

procedure SaveBuffer;
begin
  {$IFNDEF CONSOLE}
  MessageBuffer.SaveToFile('mod_dump_log.txt');
  {$ENDIF}
end;

initialization
begin
  MessageBuffer := TStringList.Create;
end;

finalization
begin
  MessageBuffer.Free;
end;

end.
