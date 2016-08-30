unit xeFiles;

interface

  function NewFile(filename: PAnsiChar): Integer; StdCall;
  function FileByIndex(index: Integer): Integer; StdCall;
  function FileByLoadOrder(load_order: Integer): Integer; StdCall;
  function FileByName(name: PAnsiChar): Integer; StdCall;
  function FileByAuthor(author: PAnsiChar): Integer; StdCall;
  function GetElementFile(_element: Integer): Integer; StdCall;
  function SaveFile(_id: Integer): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta, xeSetup;


{******************************************************************************}
{ FILE HANDLING
  Methods for handling loaded files.
}
{******************************************************************************}

function NewFile(filename: PAnsiChar): Cardinal; StdCall;
var
  LoadOrder : Integer;
  _file: IwbFile;
begin
  Result := -1;

  // fail if the file already exists
  if FileExists(wbDataPath + filename) then begin
    AddMessage(Format('NewFile: File with name %s already exists.', filename));
    exit;
  end;

  // get load order for new file
  LoadOrder := 0;
  if Length(Files) > 0 then
    LoadOrder := Files[High(Files)].LoadOrder + 1;

  // fail if maximum load order reached
  if LoadOrder > 254 then begin
    AddMessage('NewFile: Maximum plugin count of 254 reached.');
    Exit;
  end;

  // create new file
  _file := wbNewFile(wbDataPath + filename, LoadOrder);
  SetLength(Files, Succ(Length(Files)));
  Files[High(Files)] := _file;
  _file._AddRef;

  // store the file and return the result
  Result := Store(_file);
end;

function FileByIndex(index: Integer): Cardinal; StdCall;
begin
  Result := -1;
  try
     Result := Store(Files[index]);
  except
    on x: Exception do
      AddMessage('FileByIndex: '+x.Message);
  end;
end;

function FileByLoadOrder(load_order: Integer): Cardinal; StdCall;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Files) to High(Files) do
    if Files[i].LoadOrder = load_order then begin
      Result := Store(Files[i]);
      exit;
    end;
end;

function FileByName(name: PAnsiChar): Cardinal; StdCall;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Files) to High(Files) do
    if Files[i].FileName = name then begin
      Result := Store(Files[i]);
      exit;
    end;
end;

function FileByAuthor(author: PAnsiChar): Cardinal; StdCall;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Files) to High(Files) do
    if Files[i].Header.ElementEditValues('Author') = author then begin
      Result := Store(Files[i]);
      exit;
    end;
end;

function GetElementFile(_element: Cardinal): Cardinal; StdCall;
var
  element: IwbElement;
begin
  Result := -1;
  if Supports(Resolve(_element), IwbElement, element) then
    Result := Store(element._File);
end;

function SaveFile(_id: Cardinal): WordBool; StdCall;
var
  _file: IwbFile;
  FileStream: TFileStream;
  path: String;
begin
  Result := false;

  if Supports(Resolve(_id), IwbFile, _file) then begin
    path := wbDataPath + _file.FileName + '.save';
    try
      FileStream := TFileStream.Create(path, fmCreate);
      _file.WritetoStream(FileStream, false);
      // TODO: Need to handle renaming when library is finalized
    except
      on x: Exception do
        AddMessage('Failed to save: '+x.Message);
    end;
    TryToFree(FileStream);
  end;
end;

end.
