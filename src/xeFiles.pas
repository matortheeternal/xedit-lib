unit xeFiles;

interface

uses
  Classes, wbInterface;

  {$region 'Native functions'}
  function NativeAddFile(filename: string): IwbFile;
  function NativeFileByIndex(index: Integer): IwbFile;
  function NativeFileByLoadOrder(loadOrder: Integer): IwbFile;
  function NativeFileByName(name: String): IwbFile;
  function NativeFileByNameEx(name: String): IwbFile;
  function IndexOfFile(_file: IWbFile): Integer;
  function CompareLoadOrder(List: TStringList; Index1, Index2: Integer): Integer;
  {$endregion}

  {$region 'API functions'}
  function AddFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl;
  function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl;
  function FileByLoadOrder(loadOrder: Integer; _res: PCardinal): WordBool; cdecl;
  function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl;
  function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl;
  function RenameFile(_id: Cardinal; filename: PWideChar): WordBool; cdecl;
  function SaveFile(_id: Cardinal): WordBool; cdecl;
  function MD5Hash(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function CRCHash(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function SortEditorIDs(_id: Cardinal; sig: PWideChar): WordBool; cdecl;
  function SortNames(_id: Cardinal; sig: PWideChar): WordBool; cdecl;
  {$endregion}

implementation

uses
  SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation, wbHelpers,
  // xelib modules
  xeMessages, xeMeta, xeSetup;

{$region 'Native functions'}
function NextLoadOrder: Integer;
begin
  Result := 0;
  if Length(xFiles) > 0 then
    Result := Succ(xFiles[High(xFiles)].LoadOrder);
end;

function NativeAddFile(filename: string): IwbFile;
var
  LoadOrder : Integer;
  _file: IwbFile;
  filePath: String;
begin
  // fail if the file already exists
  filePath := wbDataPath + string(filename);
  if FileExists(filePath) then
    raise Exception.Create(Format('File with name %s already exists.', [filename]));

  // fail if maximum load order reached
  LoadOrder := NextLoadOrder;
  if LoadOrder > 254 then
    raise Exception.Create('Maximum plugin count of 254 reached.');

  // create new file
  _file := wbNewFile(filePath, LoadOrder);
  SetLength(xFiles, Succ(Length(xFiles)));
  xFiles[High(xFiles)] := _file;
  _file._AddRef;
  UpdateFileCount;
  Result := _file;
end;

function NativeFileByIndex(index: Integer): IwbFile;
begin
  Result := xFiles[index];
end;

function NativeFileByLoadOrder(loadOrder: Integer): IwbFile;
var
  i: Integer;
begin
  for i := Low(xFiles) to High(xFiles) do begin
    Result := xFiles[i];
    if Result.LoadOrder = loadOrder then
      exit;
  end;
  raise Exception.Create('Failed to find file with load order: ' + IntToHex(loadOrder, 2));
end;

function NativeFileByName(name: String): IwbFile;
var
  i: Integer;
begin
  for i := Low(xFiles) to High(xFiles) do begin
    Result := xFiles[i];
    if Result.FileName = name then
      exit;
  end;
  Result := nil;
end;

function NativeFileByNameEx(name: String): IwbFile;
begin
  Result := NativeFileByName(name);
  if not Assigned(Result) then
    raise Exception.Create('Failed to find file with name: ' + name);
end;

function NativeFileByAuthor(author: String): IwbFile;
var
  i: Integer;
begin
  for i := Low(xFiles) to High(xFiles) do begin
    Result := xFiles[i];
    if SameText(Result.Header.ElementEditValues['CNAM'], author) then
      exit;
  end;
  raise Exception.Create('Failed to find file with author: ' + author);
end;

function IndexOfFile(_file: IwbFile): Integer;
begin
  for Result := Low(xFiles) to High(xFiles) do
    if xFiles[Result].Equals(_file) then
      exit;
  Result := -1;
end;

function CompareLoadOrder(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if Index1 = Index2 then
    Result := 0
  else
    Result := CmpI32(
      IwbFile(Pointer(List.Objects[Index1])).LoadOrder,
      IwbFile(Pointer(List.Objects[Index2])).LoadOrder);
end;
{$endregion}

{$region 'API functions'}
function AddFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    _res^ := Store(NativeAddFile(string(filename)));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    _res^ := Store(NativeFileByIndex(index));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FileByLoadOrder(loadOrder: Integer; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    _res^ := Store(NativeFileByLoadOrder(loadOrder));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    _res^ := Store(NativeFileByNameEx(string(name)));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    _res^ := Store(NativeFileByAuthor(string(author)));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RenameFile(_id: Cardinal; fileName: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    if not FileNameValid(fileName) then
      raise Exception.Create('Filename has invalid characters.');
    _file.FileName := string(fileName);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SaveFile(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
  FileStream: TFileStream;
  path: String;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    path := wbDataPath + _file.FileName + '.save';
    FileStream := TFileStream.Create(path, fmCreate);
    try
      _file.WritetoStream(FileStream, False);
      slSavedFiles.Add(path);
      Result := True;
    finally
      FileStream.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function MD5Hash(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    resultStr := wbMD5File(wbDataPath + _file.FileName);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function CRCHash(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    resultStr := IntToHex(wbCRC32File(wbDataPath + _file.FileName), 8);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    count^ := 0;
    for i := 0 to Pred(_file.RecordCount) do
      if not _file.Records[i].IsMaster then
        Inc(count^);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortEditorIDs(_id: Cardinal; sig: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if _id = 0 then begin
      for i := Low(xFiles) to High(xFiles) do
        xFiles[i].SortEditorIDs(string(sig));
    end
    else if Supports(Resolve(_id), IwbFile, _file) then
      _file.SortEditorIDs(string(sig))
    else
      raise Exception.Create('Interface must be a file.');
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortNames(_id: Cardinal; sig: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if _id = 0 then begin
      for i := Low(xFiles) to High(xFiles) do
        xFiles[i].SortNames(string(sig));
    end
    else if Supports(Resolve(_id), IwbFile, _file) then
      _file.SortNames(string(sig))
    else
      raise Exception.Create('Interface must be a file.');
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

end.
