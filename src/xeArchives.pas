unit xeArchives;

interface

uses
  Classes;

  {$region 'API functions'}
  function ExtractContainer(name, destination: PWideChar; replace: WordBool): WordBool; cdecl;
  function ExtractFile(name, source, destination: PWideChar): WordBool; cdecl;
  function GetContainerFiles(name, path: PWideChar; len: PInteger): WordBool; cdecl;
  function GetFileContainer(path: PWideChar; len: PInteger): WordBool; cdecl;
  function GetLoadedContainers(len: PInteger): WordBool; cdecl;
  function LoadContainer(filePath: PWideChar): WordBool; cdecl;
  {$endregion}

implementation

uses
  SysUtils,
  // xedit modules
  wbInterface, wbBSArchive,
  // xelib modules
  xeMessages, xeMeta;

function ExtractContainer(name, destination: PWideChar; replace: WordBool): WordBool; cdecl;
var
  ResourceList: TStringList;
  i: Integer;
begin
  Result := False;
  try
    if not wbContainerHandler.ContainerExists(name) then
      raise Exception.Create(name + ' not loaded.');
    ResourceList := TStringList.Create;
    try
      wbContainerHandler.ContainerResourceList(name, ResourceList, '');
      for i := 0 to Pred(ResourceList.Count) do
        wbContainerHandler.ResourceCopy(name, ResourceList[i], destination);
    finally
      ResourceList.Free;
    end;
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function ExtractFile(name, source, destination: PWideChar): WordBool; cdecl;
begin
  Result := False;
  try
    wbContainerHandler.ResourceCopy(name, source, destination);
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function GetContainerFiles(name, path: PWideChar; len: PInteger): WordBool; cdecl;
var
  ResourceList: TStringList;
  i: Integer;
begin
  Result := False;
  try
    if (name <> '') and not wbContainerHandler.ContainerExists(name) then
      raise Exception.Create(name + ' not loaded.');
    ResourceList := TStringList.Create;
    try
      wbContainerHandler.ContainerResourceList(name, ResourceList, path);
      SetResultFromList(ResourceList, len);
    finally
      ResourceList.Free;
    end;
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function GetFileContainer(path: PWideChar; len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    resultStr := wbContainerHandler.GetResourceContainer(path);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function GetLoadedContainers(len: PInteger): WordBool; cdecl;
var
  ContainerList: TStringList;
begin
  Result := False;
  try
    ContainerList := TStringList.Create;
    try
      wbContainerHandler.ContainerList(ContainerList);
      SetResultFromList(ContainerList, len);
    finally
      ContainerList.Free;
    end;
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function LoadContainer(filePath: PWideChar): WordBool; cdecl;
var
  sFilePath, containerName: String;
begin
  Result := False;
  try
    sFilePath := string(filePath);
    containerName := ExtractFileName(sFilePath);
    if wbContainerHandler.ContainerExists(containerName) then
      raise Exception.Create(containerName + ' already loaded.');
    if FileExists(sFilePath) then begin
      if ExtractFileExt(containerName) = '.bsa' then
        wbContainerHandler.AddBSA(sFilePath)
      else
        wbContainerHandler.AddBA2(sFilePath);
    end
    else if DirectoryExists(sFilePath) then
      wbContainerHandler.AddFolder(sFilePath)
    else
      raise Exception.Create('Failed to load ' + containerName + ', path does not exist');
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function BuildArchive(name, folder, filePaths: PWideChar; archiveType: Integer;
  bCompress, bShare: WordBool; af, ff: PWideChar): WordBool; cdecl;
var
  slFiles: TStringList;
  bsa: TwbBSArchive;
  i: Integer;
begin
  Result := False;
  try
    slFiles := TStringList.Create;
    bsa := TwbBSArchive.Create;
    try
      if Trim(filePaths) = '' then
        raise Exception.Create('No files to compress.');

      slFiles.Text := filePaths;
      bsa.Compress := bCompress;
      bsa.ShareData := bShare;

      // create archive
      try
        bsa.CreateArchive(name, TBSArchiveType(archiveType), slFiles);
      except
        on x: Exception do
          raise Exception.Create('Archive creation error: ' + x.Message);
      end;

      // set custom flags
      if af <> '' then bsa.ArchiveFlags := StrToInt('$' + af);
      if ff <> '' then bsa.FileFlags := StrToInt('$' + ff);

      // add files to archive
      for i := 0 to Pred(slFiles.Count) do try
        bsa.AddFile(folder, folder + slFiles[i]);
      except
        on x: Exception do
          raise Exception.Create(Format('File packing error "%s%s": %s',
            [folder, slFiles[i], x.Message]));
      end;

      // save archive
      try
        bsa.Save;
      except
        on x: Exception do
          raise Exception.Create('Archive saving error: ' + x.Message);
      end;
    finally
      slFiles.Free;
      bsa.Free;
    end;
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

end.
