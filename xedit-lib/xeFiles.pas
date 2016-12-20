unit xeFiles;

interface

uses
  wbInterface;

  function NewFile(filename: PAnsiChar): Cardinal; cdecl;
  function FileByIndex(index: Integer): Cardinal; cdecl;
  function FileByLoadOrder(load_order: Integer): Cardinal; cdecl;
  function FileByName(name: PAnsiChar): Cardinal; cdecl;
  function FileByAuthor(author: PAnsiChar): Cardinal; cdecl;
  function GetElementFile(_id: Cardinal): Cardinal; cdecl;
  function SaveFile(_id: Cardinal): WordBool; cdecl;
  function GetFileNames(fileNames: PWideChar; len: Integer): WordBool; cdecl;

  // native functions
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

function NewFile(filename: PAnsiChar): Cardinal; cdecl;
var
  LoadOrder : Integer;
  _file: IwbFile;
  filePath: String;
begin
  Result := 0;
  try
    // fail if the file already exists
    filePath := wbDataPath + string(filename);
    if FileExists(filePath) then
      raise Exception.Create(Format('File with name %s already exists.', [filename]));

    // get load order for new file
    LoadOrder := 0;
    if Length(Files) > 0 then
      LoadOrder := Files[High(Files)].LoadOrder + 1;

    // fail if maximum load order reached
    if LoadOrder > 254 then
      raise Exception.Create('Maximum plugin count of 254 reached.');

    // create new file
    _file := wbNewFile(filePath, LoadOrder);
    SetLength(Files, Succ(Length(Files)));
    Files[High(Files)] := _file;
    _file._AddRef;

    // store the file and return the result
    Result := Store(_file);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NativeFileByIndex(index: Integer): IwbFile;
begin
  Result := Files[index];
end;

function FileByIndex(index: Integer): Cardinal; cdecl;
var
  _file: IwbFile;
begin
  Result := 0;
  try
    _file := NativeFileByIndex(index);
    if Assigned(_file) then
      Result := Store(_file);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NativeFileByLoadOrder(load_order: Integer): IwbFile;
var
  i: Integer;
begin
  for i := Low(Files) to High(Files) do
    if Files[i].LoadOrder = load_order then begin
      Result := Files[i];
      exit;
    end;
end;

function FileByLoadOrder(load_order: Integer): Cardinal; cdecl;
var
  _file: IwbFile;
begin
  Result := 0;
  try
    _file := NativeFileByLoadOrder(load_order);
    if Assigned(_file) then
      Result := Store(_file);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NativeFileByName(name: String): IwbFile;
var
  i: Integer;
begin
  for i := Low(Files) to High(Files) do
    if Files[i].FileName = string(name) then begin
      Result := Files[i];
      exit;
    end;
end;

function FileByName(name: PAnsiChar): Cardinal; cdecl;
var
  _file: IwbFile;
begin
  Result := 0;
  try
    _file := NativeFileByName(string(name));
    if Assigned(_file) then
      Result := Store(_file);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NativeFileByAuthor(author: String): IwbFile;
var
  i: Integer;
  s: String;
begin
  for i := Low(Files) to High(Files) do begin
    s := Files[i].Header.ElementEditValues['CNAM'];
    if SameText(s, author) then begin
      Result := Files[i];
      exit;
    end;
  end;
end;

function FileByAuthor(author: PAnsiChar): Cardinal; cdecl;
var
  _file: IwbFile;
begin
  Result := 0;
  try
    _file := NativeFileByAuthor(string(author));
    if Assigned(_file) then
      Result := Store(_file);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetElementFile(_id: Cardinal): Cardinal; cdecl;
var
  element: IwbElement;
begin
  Result := 0;
  try
    if Supports(Resolve(_id), IwbElement, element) then
      Result := Store(element._File);
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
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      path := wbDataPath + _file.FileName + '.save';
      FileStream := TFileStream.Create(path, fmCreate);
      try
        _file.WritetoStream(FileStream, false);
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

function GetFileNames(fileNames: PWideChar; len: Integer): WordBool; cdecl;
var
  s: String;
  i: Integer;
begin
  Result := false;
  try
    s := '';
    for i := Low(Files) to High(Files) do
      s := s + Files[i].FileName + #13;
    StrLCopy(fileNames, PWideChar(WideString(Trim(s))), len);
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
