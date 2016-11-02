program DelphiTest;

{$APPTYPE CONSOLE}

uses
  ShareMem,
  SysUtils;

  // META METHODS
  procedure Initialize; StdCall; external 'XEditLib.dll';
  procedure Finalize; StdCall; external 'XEditLib.dll';
  procedure GetBuffer(str: PWideChar; len: Integer); StdCall; external 'XEditLib.dll';
  procedure FlushBuffer; StdCall; external 'XEditLib.dll';
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; StdCall; external 'XEditLib.dll';
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; StdCall; external 'XEditLib.dll';
  procedure Release(_id: Cardinal); StdCall; external 'XEditLib.dll';
  procedure ResetStore; StdCall; external 'XEditLib.dll';

  // LOADING AND SET UP METHODS
  procedure SetGameMode(mode: Integer); StdCall; external 'XEditLib.dll';
  function GetLoadOrder(str: PWideChar; len: Integer): WordBool; StdCall; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar): WordBool; StdCall; StdCall; external 'XEditLib.dll';
  function GetLoaderDone: WordBool; StdCall; external 'XEditLib.dll';

  // FILE HANDLING METHODS
  function NewFile(filename: PAnsiChar): Cardinal; StdCall; external 'XEditLib.dll';
  function FileByIndex(index: Integer): Cardinal; StdCall; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer): Cardinal; StdCall; external 'XEditLib.dll';
  function FileByName(name: PAnsiChar): Cardinal; StdCall; external 'XEditLib.dll';
  function FileByAuthor(author: PAnsiChar): Cardinal; StdCall; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal): Cardinal; StdCall; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal): WordBool; StdCall; external 'XEditLib.dll';

  // FILE VALUE METHODS
  function GetFileHeader(_id, _res: Cardinal): WordBool; StdCall; external 'XEditLib.dll';
  function GetNextObjectId(_id, nextObjectID: Cardinal): WordBool; StdCall; external 'XEditLib.dll';
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; StdCall; external 'XEditLib.dll';
  function GetFileName(_id: Cardinal; fileName: PWideChar; len: Integer): WordBool; StdCall; external 'XEditLib.dll';
  function GetAuthor(_id: Cardinal; author: PWideChar; len: Integer): WordBool; StdCall; external 'XEditLib.dll';
  function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; StdCall; external 'XEditLib.dll';
  function GetDescription(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; StdCall; external 'XEditLib.dll';
  function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; StdCall; external 'XEditLib.dll';
  function OverrideRecordCount(_id: Cardinal; count: Integer): WordBool; StdCall; external 'XEditLib.dll';
  function GetIsESM(_id: Cardinal; isESM: PWordBool): WordBool; StdCall; external 'XEditLib.dll';
  function SetIsESM(_id: Cardinal; isESM: WordBool): WordBool; StdCall; external 'XEditLib.dll';


procedure WriteBuffer;
var
  str: PWideChar;
begin
  GetMem(str, 4096);
  GetBuffer(str, 4096);
  if Length(string(str)) > 0 then begin
    FlushBuffer();
    WriteLn(str);
  end;
end;

procedure LoadXEdit;
var
  str: PWideChar;
begin
  SetGameMode(3);
  GetMem(str, 16384); // 16KB for load order, to be safe
  GetLoadOrder(str, 16384);
  LoadPlugins(str);
  while not GetLoaderDone do begin
    WriteBuffer;
    Sleep(100);
  end;
end;

procedure TestFileHandling;
var
  h: Cardinal;
begin
  h := FileByName('Skyrim.esm');
  WriteLn('Skyrim.esm returned with handle: ' + IntToStr(h));
  h := FileByIndex(1);
  WriteLn('File at index 1 returned with handle: ' + IntToStr(h));
  h := FileByLoadOrder(1);
  WriteLn('File with load order 01 returned with handle: ' + IntToStr(h));
  h := FileByAuthor('mcarofano');
  WriteLn('File with author "mcarofano" returned with handle: ' + IntToStr(h));
end;

procedure TestFileValues;
var
  h: Cardinal;
  str: PWideChar;
  bIsEsm: PWordBool;
begin
  h := FileByName('Skyrim.esm');
  GetMem(str, 4096);
  GetFileName(h, str, 4096);
  WriteLn('Filename: ' + str);
  GetAuthor(h, str, 4096);
  WriteLn('Author: ' + str);
  GetDescription(h, str, 4096);
  WriteLn('Description: ' + str);
  GetMem(bIsEsm, 1);
  GetIsESM(h, bIsEsm);
  WriteLn('IsESM: ' + BoolToStr(bIsEsm^, true));
end;

begin
  Initialize;
  LoadXEdit;
  TestFileHandling;
  TestFileValues;
  Finalize;
  WriteBuffer;
  WriteLn('Done!');
  Readln;
end.
