unit xeFiles;

interface

uses
  wbInterface;

  function AddFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl;
  function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl;
  function FileByLoadOrder(load_order: Integer; _res: PCardinal): WordBool; cdecl;
  function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl;
  function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl;
  function SaveFile(_id: Cardinal): WordBool; cdecl;

  // native functions
  function NativeAddFile(filename: string): IwbFile;
  function NativeFileByIndex(index: Integer): IwbFile;
  function NativeFileByLoadOrder(load_order: Integer): IwbFile;
  function NativeFileByName(name: String): IwbFile;
  function NativeFileByAuthor(author: String): IwbFile;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMessages, xeMeta, xeSetup;


{******************************************************************************}
{ FILE HANDLING
  Methods for handling loaded files.
}
{******************************************************************************}

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
  Result := _file;
end;

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

function NativeFileByIndex(index: Integer): IwbFile;
begin
  Result := xFiles[index];
end;

function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    _file := NativeFileByIndex(index);
    if Assigned(_file) then begin
      _res^ := Store(_file);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NativeFileByLoadOrder(load_order: Integer): IwbFile;
var
  i: Integer;
begin
  for i := Low(xFiles) to High(xFiles) do begin
    Result := xFiles[i];
    if Result.LoadOrder = load_order then
      exit;
  end;
  Result := nil;
end;

function FileByLoadOrder(load_order: Integer; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    _file := NativeFileByLoadOrder(load_order);
    if Assigned(_file) then begin
      _res^ := Store(_file);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
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

function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    _file := NativeFileByName(string(name));
    if Assigned(_file) then begin
      _res^ := Store(_file);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
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
  Result := nil;
end;

function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    _file := NativeFileByAuthor(string(author));
    if Assigned(_file) then begin
      _res^ := Store(_file);
      Result := True;
    end;
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
    if Supports(Resolve(_id), IwbFile, _file) then begin
      path := wbDataPath + _file.FileName + '.save';
      FileStream := TFileStream.Create(path, fmCreate);
      try
        _file.WritetoStream(FileStream, False);
        Result := True;
        // TODO: Need to handle renaming when library is finalized
      finally
        FileStream.Free;
      end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
