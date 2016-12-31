program DelphiTest;

{$APPTYPE CONSOLE}

uses
  ShareMem,
  Classes,
  SysUtils;

type
  CardinalArray = array of Cardinal;
  PCardinalArray = ^CardinalArray;

  // META METHODS
  procedure Initialize; cdecl; external 'XEditLib.dll';
  procedure Finalize; cdecl; external 'XEditLib.dll';
  procedure GetBuffer(str: PWideChar; len: Integer); cdecl; external 'XEditLib.dll';
  procedure FlushBuffer; cdecl; external 'XEditLib.dll';
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  procedure Release(_id: Cardinal); cdecl; external 'XEditLib.dll';
  procedure ResetStore; cdecl; external 'XEditLib.dll';

  // LOADING AND SET UP METHODS
  procedure SetGameMode(mode: Integer); cdecl; external 'XEditLib.dll';
  function GetLoadOrder(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl; cdecl; external 'XEditLib.dll';
  function GetLoaderDone: WordBool; cdecl; external 'XEditLib.dll';

  // FILE HANDLING METHODS
  function NewFile(filename: PAnsiChar): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByIndex(index: Integer): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByName(name: PAnsiChar): Cardinal; cdecl; external 'XEditLib.dll';
  function FileByAuthor(author: PAnsiChar): Cardinal; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal): Cardinal; cdecl; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileNames(fileNames: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';

  // FILE VALUE METHODS
  function GetFileHeader(_id, _res: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetNextObjectId(_id, nextObjectID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileName(_id: Cardinal; fileName: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetAuthor(_id: Cardinal; author: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetDescription(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function OverrideRecordCount(_id: Cardinal; count: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetIsESM(_id: Cardinal; isESM: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetIsESM(_id: Cardinal; isESM: WordBool): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT HANDLING METHODS
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; _res: PCardinalArray): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function NewElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function ElementCount(_id: Cardinal): Integer; cdecl; external 'XEditLib.dll';
  function ElementAssigned(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function Equals(_id, _id2: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsMaster(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsInjected(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsOverride(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsWinningOverride(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // SERIALIZATION METHODS
  function ElementToJson(_id: Cardinal; json: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';

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
  WriteBuffer;
  WriteLn(' ');
end;

procedure WriteStrToFile(str, fileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := str;
    sl.SaveToFile(fileName);
  finally
    sl.Free;
  end;
end;

procedure TestFileHandling;
const
  MB = 1048576;
  pluginFileName = 'Dawnguard.esm';
var
  h: Cardinal;
  str: PWideChar;
begin
  WriteLn('== File Handling Tests ==');
  h := FileByName('Skyrim.esm');
  WriteLn('Skyrim.esm returned with handle: ' + IntToStr(h));
  h := FileByIndex(1);
  WriteLn('File at index 1 returned with handle: ' + IntToStr(h));
  h := FileByLoadOrder(1);
  WriteLn('File with load order 01 returned with handle: ' + IntToStr(h));
  h := FileByAuthor('mcarofano');
  WriteLn('File with author "mcarofano" returned with handle: ' + IntToStr(h));
  GetMem(str, 150 * MB);
  h := FileByName(pluginFileName);
  ElementToJson(h, str, 150 * MB);
  WriteLn('FileToJSON length: '+IntToStr(Length(str)));
  WriteStrToFile(str, pluginFileName + '.json');
  WriteBuffer;
  WriteLn(' ');
end;

procedure TestFileValues;
var
  h: Cardinal;
  str: PWideChar;
  bIsEsm: PWordBool;
begin
  WriteLn('== File Value Tests ==');
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
  WriteLn(' ');
end;

procedure TestGetElement;
var
  f, g, r, h: Cardinal;
begin
  WriteLn('=== GetElement Tests ===');
  // Test GetElement resolving a file by index
  GetElement(0, '[0]', @f);
  WriteLn('Resolved [0] with handle: ' + IntToStr(f));
  // Test GetElement resolving a file by name
  GetElement(0, 'Skyrim.esm', @f);
  WriteLn('Resolved Skyrim.esm with handle: ' + IntToStr(f));

  // Test GetElement resolving file header from file
  GetElement(f, '[0]', @h);
  WriteLn('Skyrim.esm - Resolved [0] with handle: ' + IntToStr(h));
  // Test GetElement resolving groups from file
  GetElement(f, 'WRLD', @g);
  WriteLn('Skyrim.esm - Resolved WRLD with handle: ' + IntToStr(g));
  GetElement(f, 'ARMO', @g);
  WriteLn('Skyrim.esm - Resolved ARMO with handle: ' + IntToStr(g));

  // Test GetElement resolving records from group
  GetElement(g, '[0]', @r);
  WriteLn('Skyrim.esm\ARMO - Resolved [0] with handle: ' + IntToStr(r));
  GetElement(g, '[1]', @r);
  WriteLn('Skyrim.esm\ARMO - Resolved [1] with handle: ' + IntToStr(r));

  // Test GetElement resolving top level fields from a record via signature
  GetElement(r, 'FULL', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved FULL with handle: ' + IntToStr(h));
  // Test GetElement resolving top level fields from a record via name
  GetElement(r, 'Male world model', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved Male world model with handle: ' + IntToStr(h));

  // Test GetElement resolving nested fields from a record with indexes
  GetElement(r, 'KWDA\[1]', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved KWDA\[1] with handle: ' + IntToStr(h));
  GetElement(r, 'BODT\[0]', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved BODT\[0] with handle: ' + IntToStr(h));

  // Test GetElement resolving a record from a file by FormID
  GetElement(f, 'ARMO\00012E46', @r);
  WriteLn('Skyrim.esm - Resolved ARMO\00012E46 with handle: ' + IntToStr(r));
  // Test GetElement resolving a record from a file by index
  GetElement(f, 'ARMO\[2]', @r);
  WriteLn('Skyrim.esm - Resolved ARMO\[2] with handle: ' + IntToStr(r));
  // Test GetElement resolving an element from a file
  GetElement(f, 'ARMO\00012E46\KWDA\[0]', @h);
  WriteLn('Skyrim.esm - Resolved ARMO\00012E46\KWDA\[0] with handle: ' + IntToStr(h));

  // Test GetElement resolving a full path
  GetElement(0, 'Skyrim.esm\ARMO\00013938\DATA\Value', @h);
  WriteLn('Resolved Skyrim.esm\ARMO\00013938\DATA\Value with handle: ' + IntToStr(h));
  // Test GetElement resolving a full path of indexes
  GetElement(0, '[0]\[1]\[2]\[1]', @h);
  WriteLn('Resolved [0]\[1]\[2]\[1] with handle: ' + IntToStr(h));
  WriteLn(' ');
end;

procedure WriteArray(a: CardinalArray);
var
  s: String;
  i: Integer;
begin
  s := '  [ ';
  for i := Low(a) to High(a) do begin
    s := s + IntToStr(a[i]);
    if i < High(a) then
      s := s + ', ';
  end;
  s := s + ' ]';
  WriteLn(s);
end;

procedure TestGetElements;
var
  f, g, r: Cardinal;
  a: CardinalArray;
begin
  WriteLn('=== GetElements Tests ===');
  f := FileByName('Skyrim.esm');
  GetElement(f, 'ARMO', @g);
  GetElement(g, '00012E46', @r);

  // test getting elements from a file
  GetElements(f, @a);
  WriteLn('Resolved ' + IntToStr(High(a) + 1) + ' element handles from Skyrim.esm:');
  WriteArray(a);

  // test getting elements from a group
  GetElements(g, @a);
  WriteLn('Resolved ' + IntToStr(High(a) + 1) + ' element handles from the Skyrim.esm\ARMO:');
  WriteArray(a);

  // test getting elements from a group
  GetElements(r, @a);
  WriteLn('Resolved ' + IntToStr(High(a) + 1) + ' element handles from the Skyrim.esm\ARMO\00012E46:');
  WriteArray(a);
  WriteLn(' ');
end;

procedure TestElementHandling;
begin
  WriteLn('== Element Handling Tests ==');
  TestGetElement;
  TestGetElements;
  WriteLn(' ');
end;

begin
  Initialize;
  LoadXEdit;
  TestFileHandling;
  TestFileValues;
  TestElementHandling;
  Finalize;
  WriteBuffer;
  WriteLn('Done!');
  Readln;
end.
