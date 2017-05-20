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
  MessageBuffer.Add(msg);
end;

procedure SaveBuffer;
begin
  MessageBuffer.SaveToFile('xelib_log.txt');
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
