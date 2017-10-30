unit xeSetup;

interface

uses
  Classes,
  // xedit units
  wbHelpers, wbInterface, wbImplementation;

type
  {$region 'Types'}
  TLoaderThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TRefThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TLoaderState = ( lsInactive, lsActive, lsDone, lsError );
  TPluginsFormat = ( pfPlain, pfAsterisks, pfEquals );
  {$endregion}

  {$region 'Native functions}
  procedure SetLoaderState(state: TLoaderState);
  procedure UpdateFileCount;
  procedure LoadPluginFiles;
  procedure LoadResources;
  procedure LoadPluginsList(const sLoadPath: String; var sl: TStringList; noDelete: Boolean = False);
  procedure LoadLoadOrder(const sLoadPath: String; var slLoadOrder, slPlugins: TStringList);
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  procedure AddBaseMasters(var sl: TStringList);
  procedure FixLoadOrder(var sl: TStringList; const filename: String; index: Integer);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
  procedure RenameSavedFiles;
  {$endregion}

  {$region 'API functions'}
  function SetGamePath(path: PWideChar): WordBool; cdecl;
  function SetLanguage(lang: PWideChar): WordBool; cdecl;
  function SetBackupPath(path: PWideChar): WordBool; cdecl;
  function SetGameMode(mode: Integer): WordBool; cdecl;
  function GetGamePath(mode: Integer; len: PInteger): WordBool; cdecl;
  function GetLoadOrder(len: PInteger): WordBool; cdecl;
  function GetActivePlugins(len: PInteger): WordBool; cdecl;
  function LoadPlugins(loadOrder: PWideChar; smartLoad: WordBool): WordBool; cdecl;
  function LoadPlugin(filename: PWideChar): WordBool; cdecl;
  function LoadPluginHeader(fileName: PWideChar; _res: PCardinal): WordBool; cdecl;
  function BuildReferences(_id: Cardinal; synchronous: WordBool): WordBool; cdecl;
  function GetLoaderStatus(status: PByte): WordBool; cdecl;
  function UnloadPlugin(_id: Cardinal): WordBool; cdecl;
  {$endregion}

var
  xFiles, rFiles: array of IwbFile;
  slLoadOrder, slSavedFiles: TStringList;
  LoaderThread: TLoaderThread;
  RefThread: TRefThread;
  LoaderState: TLoaderState;
  BaseFileIndex: Integer;

implementation

uses
  Windows, SysUtils, ShlObj,
  // xelib units
  xeHelpers, xeMeta, xeConfiguration, xeMessages, xeMasters;

{$region 'TLoaderThread'}
procedure TLoaderThread.Execute;
begin
  try     
    LoadPluginFiles;
    LoadResources;
    UpdateFileCount;

    // done loading
    slLoadOrder.Free;
    AddMessage('Done loading files.');
    SetLoaderState(lsDone);
  except
    on e: Exception do begin
      AddMessage('Fatal Error: ' + e.Message);
      SetLoaderState(lsError);
    end;
  end;
end;
{$endregion}

{$region 'TRefThread'}
procedure TRefThread.Execute;
var
  i: Integer;
  _file: IwbFile;
begin
  try
    for i := Low(rFiles) to High(rFiles) do begin
      _file := rFiles[i];
      AddMessage(Format('Building references for %s (%d/%d)', [_file.FileName, i + 1, Length(rFiles)]));
      rFiles[i].BuildRef;
    end;

    // done loading
    SetLength(rFiles, 0);
    AddMessage('Done building references.');
    SetLoaderState(lsDone);
  except
    on E: Exception do begin
      AddMessage('Fatal Error: <' + e.ClassName + ': ' + e.Message + '>');
      SetLoaderState(lsError);
    end;
  end;
end;
{$endregion}

{$region 'Native functions'}
procedure SetLoaderState(state: TLoaderState);
begin
  LoaderState := state;
  case state of
    lsActive: wbLoaderdone := False;
    lsDone: wbLoaderdone := True;
    lsError: wbLoaderError := True;
  end;
end;

{$region 'Reference building helpers}
procedure BuildReferencesAsync(_id: Cardinal);
var
  _file: IwbFile;
begin
  if _id = 0 then
    rFiles := Copy(xFiles, 0, MaxInt)
  else begin
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    SetLength(rFiles, 1);
    rFiles[0] := _file;
  end;

  // start reference building thread
  SetLoaderState(lsActive);
  RefThread := TRefThread.Create;
end;

procedure BuildReferencesSync(_id: Cardinal);
var
  i: Integer;
  _file: IwbFile;
begin
  if _id = 0 then begin
    for i := Low(xFiles) to High(xFiles) do
      xFiles[i].BuildRef;
  end
  else begin
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    _file.BuildRef;
  end;
end;
{$endregion}

{$region 'File loading'}
procedure UpdateFileCount;
begin
  Globals.Values['FileCount'] := IntToStr(Length(xFiles));
end;

procedure LoadFile(const filePath: String; loadOrder: Integer);
var
  _file: IwbFile;
begin
  _file := wbFile(filePath, loadOrder, '', False, False);
  SetLength(xFiles, Length(xFiles) + 1);
  xFiles[High(xFiles)] := _file;
end;

procedure LoadHardcodedDat;
var
  _file: IwbFile;
begin
  _file := wbFile(Globals.Values['ProgramPath'] + wbGameName + wbHardcodedDat, 0);
  SetLength(xFiles, Length(xFiles) + 1);
  xFiles[High(xFiles)] := _file;
end;

procedure ThreadException(const msg: String);
begin
  AddMessage(msg);
  raise Exception.Create(msg);
end;

procedure LoadPluginFiles;
var
  i: Integer;
  sFileName: String;
begin
  BaseFileIndex := Length(xFiles);
  for i := 0 to Pred(slLoadOrder.Count) do begin
    sFileName := slLoadOrder[i];
    AddMessage(Format('Loading %s (%d/%d)', [sFileName, i + 1, slLoadOrder.Count]));

    // load plugin
    try
      LoadFile(wbDataPath + sFileName, BaseFileIndex + i);
    except
      on x: Exception do
        ThreadException('Exception loading ' + sFileName + ': ' + x.Message);
    end;

    // load hardcoded dat
    if (i = 0) and (sFileName = wbGameName + '.esm') then try
      LoadHardCodedDat;
    except
      on x: Exception do
        ThreadException('Exception loading ' + wbGameName + wbHardcodedDat + ': ' + x.Message);
    end;
  end;
end;

procedure LoadBSAFile(const sFileName: String);
var
  sFileExt: String;
begin
  sFileExt := ExtractFileExt(sFileName);
  AddMessage('Loading resources from ' + sFileName);
  if sFileExt = '.bsa' then
    wbContainerHandler.AddBSA(wbDataPath + sFileName)
  else if sFileExt = '.ba2' then
    wbContainerHandler.AddBA2(wbDataPath + sFileName);
end;

procedure LoadBSAs(var slBSAFileNames, slErrors: TStringList);
var
  i: Integer;
begin
  for i := 0 to slBSAFileNames.Count - 1 do
    LoadBSAFile(slBSAFileNames[i]);
  for i := 0 to slErrors.Count - 1 do
    AddMessage(slErrors[i] + ' was not found');
end;

procedure LoadResources;
var
  slBSAFileNames: TStringList;
  slErrors: TStringList;
  i: Integer;
  modName: String;
  bIsTES5: Boolean;
begin
  wbContainerHandler.AddFolder(wbDataPath);
  bIsTES5 := wbGameMode in [gmTES5, gmSSE];
  slBSAFileNames := TStringList.Create;
  try
    slErrors := TStringList.Create;
    try
      if BaseFileIndex = 0 then begin
        FindBSAs(wbTheGameIniFileName, wbDataPath, slBSAFileNames, slErrors);
        LoadBSAs(slBSAFileNames, slErrors);
      end;

      for i := BaseFileIndex to High(xFiles) do begin
        slBSAFileNames.Clear;
        slErrors.Clear;
        modName := ChangeFileExt(xFiles[i].GetFileName, '');
        HasBSAs(modName, wbDataPath, bIsTES5, bIsTES5, slBSAFileNames, slErrors);
        LoadBSAs(slBSAFileNames, slErrors);
      end;
    finally
      slErrors.Free;
    end;
  finally
    slBSAFileNames.Free;
  end;
end;

function IndexOfFile(const _file: IwbFile): Integer;
begin
  for Result := Low(xFiles) to High(xFiles) do
    if xFiles[Result] = _file then exit;
  Result := -1;
end;

procedure ForceClose(const _file: IwbFile);
var
  i, index, len: Integer;
begin
  index := IndexOfFile(_file);
  if index > -1 then begin
    len := Length(xFiles);
    for i := index + 1 to Pred(len) do
      xFiles[i - 1] := xFiles[i];
    SetLength(xFiles, len - 1);
    UpdateFileCount;
  end;
  wbFileForceClosed(_file);
end;
{$endregion}

{$region 'Load order helpers'}
function LoadFileHeader(const filePath: String): IwbFile;
begin
  try
    Result := wbFile(filePath, -1, '', False, True);
  except
    on x: Exception do 
      raise Exception.Create(Format('Failed to load file header %s, %s', [filePath, x.Message]));
  end;
end;

procedure AddToLoadOrder(const filePath: String);
var
  _file: IwbFile;
  masterNames: TDynStrings;
  i: Integer;
begin
  _file := LoadFileHeader(filePath);
  masterNames := NativeGetMasterNames(_file);
  for i := Low(masterNames) to High(masterNames) do
    if slLoadOrder.IndexOf(masterNames[i]) = -1 then
      AddToLoadOrder(masterNames[i]);
  slLoadOrder.Add(filePath);
end;

procedure BuildLoadOrder(const loadOrder: String);
var
  slFiles: TStringList;
  i: Integer;
begin
  slFiles := TStringList.Create;
  try        
    slFiles.Text := loadOrder;
    for i := 0 to Pred(slFiles.Count) do
      AddToLoadOrder(slFiles[i]);
  finally
    wbFileForceClosed;
    slFiles.Free;
  end;
end;

function GetPluginsFormat(var sl: TStringList): TPluginsFormat;
var
  asterisksCount, equalsCount, i: Integer;
  s: String;
begin
  asterisksCount := 0;
  equalsCount := 0;
  for i := 0 to Pred(sl.Count) do begin
    s := sl[i];
    if Length(s) = 0 then continue;
    if s[1] = '*' then
      Inc(asterisksCount);
    if StrEndsWith(s, '=0') or StrEndsWith(s, '=1') then
      Inc(equalsCount);
  end;
  if equalsCount > 0 then
    Result := pfEquals
  else if asterisksCount > 0 then
    Result := pfAsterisks
  else
    Result := pfPlain;
end;

procedure ProcessAsterisks(var sl: TStringList; index: Integer; noDelete: Boolean);
var
  s: String;
begin
  s := sl[index];
  if s[1] <> '*' then begin
    if not noDelete then sl.Delete(index);
  end
  else
    sl[index] := Copy(s, 2, Length(s));
end;

procedure ProcessEquals(var sl: TStringList; index: Integer; noDelete: Boolean);
var
  s, endChars: String;
begin
  s := sl[index];
  endChars := Copy(s, Length(s) - 1, 2);
  if endChars[1] <> '=' then exit;
  if (endChars[2] <> '1') and (not noDelete) then
    sl.Delete(index)
  else
    sl[index] := Copy(s, 1, Length(s) - 2);
end;

procedure ProcessPluginsFormat(var sl: TStringList; noDelete: Boolean);
var
  pf: TPluginsFormat;
  i: Integer;
  s: String;
begin
  pf := GetPluginsFormat(sl);
  for i := Pred(sl.Count) downto 0 do begin
    s := sl[i];
    case pf of
      pfAsterisks: ProcessAsterisks(sl, i, noDelete);
      pfEquals: ProcessEquals(sl, i, noDelete);
    end;
  end;
end;

procedure LoadPluginsList(const sLoadPath: String; var sl: TStringList; noDelete: Boolean = False);
var
  sPath: String;
begin
  sPath := sLoadPath + 'plugins.txt';
  if FileExists(sPath) then begin
    sl.LoadFromFile(sPath);
    if (wbGameMode = gmSSE) or (wbGameMode = gmFO4) then
      ProcessPluginsFormat(sl, noDelete);
  end
  else
    AddMissingFiles(sl);

  // remove comments and missing files
  RemoveCommentsAndEmpty(sl);
  RemoveMissingFiles(sl);
end;

procedure LoadLoadOrder(const sLoadPath: String; var slLoadOrder, slPlugins: TStringList);
var
  sPath: String;
begin
  sPath := sLoadPath + 'loadorder.txt';
  if FileExists(sPath) then
    slLoadOrder.LoadFromFile(sPath)
  else
    slLoadOrder.AddStrings(slPlugins);

  // remove comments and add/remove files
  RemoveCommentsAndEmpty(slLoadOrder);
  RemoveMissingFiles(slLoadOrder);
  AddMissingFiles(slLoadOrder);
end;

{ Remove comments and empty lines from a stringlist }
procedure RemoveCommentsAndEmpty(var sl: TStringList);
var
  i, j: integer;
  s: string;
begin
  for i := Pred(sl.Count) downto 0 do begin
    s := Trim(sl.Strings[i]);
    j := Pos('#', s);
    if j > 0 then
      System.Delete(s, j, High(Integer));
    if s = '' then
      sl.Delete(i);
  end;
end;

{ Remove nonexistent files from stringlist }
procedure RemoveMissingFiles(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if not FileExists(wbDataPath + sl.Strings[i]) then
      sl.Delete(i);
end;

{ Add missing *.esp and *.esm files to list }
procedure AddMissingFiles(var sl: TStringList);
var
  F: TSearchRec;
  i, j: integer;
  slNew: TStringList;
begin
  slNew := TStringList.Create;
  try
    // search for missing plugins and masters
    if FindFirst(wbDataPath + '*.*', faAnyFile, F) = 0 then try
      repeat
        if not (IsFileESM(F.Name) or IsFileESP(F.Name)) then
          continue;
        if sl.IndexOf(F.Name) = -1 then
          slNew.AddObject(F.Name, TObject(FileAge(wbDataPath + F.Name)));
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;

    // sort the list
    slNew.CustomSort(PluginListCompare);

    // The for loop won't initialize j if sl.count = 0, we must force it
    // to -1 so inserting will happen at index 0
    if sl.Count = 0 then
      j := -1
    else
      // find position of last master
      for j := Pred(sl.Count) downto 0 do
        if IsFileESM(sl[j]) then
          Break;

    // add esm masters after the last master, add esp plugins at the end
    Inc(j);
    for i := 0 to Pred(slNew.Count) do begin
      if IsFileESM(slNew[i]) then begin
        sl.InsertObject(j, slNew[i], slNew.Objects[i]);
        Inc(j);
      end else
        sl.AddObject(slNew[i], slNew.Objects[i]);
    end;
  finally
    slNew.Free;
  end;
end;

{ Get date modified for plugins in load order and store in stringlist objects }
procedure GetPluginDates(var sl: TStringList);
var
  i: Integer;
begin
  for i := 0 to Pred(sl.Count) do
    sl.Objects[i] := TObject(FileAge(wbDataPath + sl[i]));
end;

procedure AddBaseMasters(var sl: TStringList);
begin
  if (wbGameMode = gmTES5) then begin
    FixLoadOrder(sl, 'Skyrim.esm', 0);
    FixLoadOrder(sl, 'Update.esm', 1);
  end
  else if (wbGameMode = gmSSE) then begin
    FixLoadOrder(sl, 'Skyrim.esm', 0);
    FixLoadOrder(sl, 'Update.esm', 1);
    FixLoadOrder(sl, 'Dawnguard.esm', 2);
    FixLoadOrder(sl, 'HearthFires.esm', 3);
    FixLoadOrder(sl, 'Dragonborn.esm', 4);
  end
  else if (wbGameMode = gmFO4) then begin
    FixLoadOrder(sl, 'Fallout4.esm', 0);
    FixLoadOrder(sl, 'DLCRobot.esm', 1);
    FixLoadOrder(sl, 'DLCworkshop01.esm', 2);
    FixLoadOrder(sl, 'DLCCoast.esm', 3);
    FixLoadOrder(sl, 'DLCworkshop02.esm', 4);
    FixLoadOrder(sl, 'DLCworkshop03.esm', 5);
    FixLoadOrder(sl, 'DLCNukaworld.esm', 6);
  end;
end;

{ Forces a plugin to load at a specific position }
procedure FixLoadOrder(var sl: TStringList; const filename: String; index: Integer);
var
  oldIndex: Integer;
begin
  oldIndex := sl.IndexOf(filename);
  if (oldIndex > -1) then begin
    if oldIndex = index then exit;
    sl.Delete(oldIndex);
    sl.Insert(index, filename);
  end
  else if FileExists(wbDataPath + filename) then
    sl.Insert(index, filename);
end;

{ Compare function for sorting load order by date modified/esms }
function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  IsESM1, IsESM2: Boolean;
  FileAge1,FileAge2: Integer;
  FileDateTime1, FileDateTime2: TDateTime;
begin
  IsESM1 := IsFileESM(List[Index1]);
  IsESM2 := IsFileESM(List[Index2]);

  if IsESM1 = IsESM2 then begin
    FileAge1 := Integer(List.Objects[Index1]);
    FileAge2 := Integer(List.Objects[Index2]);

    if FileAge1 < FileAge2 then
      Result := -1
    else if FileAge1 > FileAge2 then
      Result := 1
    else begin
      if not SameText(List[Index1], List[Index1])
      and FileAge(List[Index1], FileDateTime1) and FileAge(List[Index2], FileDateTime2) then begin
        if FileDateTime1 < FileDateTime2 then
          Result := -1
        else if FileDateTime1 > FileDateTime2 then
          Result := 1
        else
          Result := 0;
      end else
        Result := 0;
    end;

  end else if IsESM1 then
    Result := -1
  else
    Result := 1;
end;
{$endregion}

{$region 'Rename saved files'}
var
  FileTimeStr: String;

procedure BackupFile(const path: String);
var
  bakPath: String;
begin
  if DirectoryExists(BackupPath) then
    bakPath := BackupPath + ExtractFileName(path) + '.' + FileTimeStr + '.bak'
  else
    bakPath := path + '.bak';
  if not RenameFile(path, bakPath) then
    RaiseLastOSError;
end;

procedure RenameSavedFile(const path: String);
var
  newPath: String;
begin
  newPath := Copy(path, 1, Length(path) - 5);
  if FileExists(newPath) then
    BackupFile(newPath);
  if not RenameFile(path, newPath) then
    RaiseLastOSError;
end;

procedure CreateBackupFolder;
begin
  try
    ForceDirectories(BackupPath);
  except
    on x: Exception do
      AddMessage('Error creating backup folder: ' + x.Message);
  end;
end;

procedure RenameSavedFiles;
var
  i: Integer;
begin
  CreateBackupFolder;
  DateTimeToString(FileTimeStr, 'yymmdd_hhnnss', Now);
  for i := 0 to Pred(slSavedFiles.Count) do try
    RenameSavedFile(slSavedFiles[i]);
  except
    on x: Exception do
      AddMessage('Error renaming saved file, ' + x.Message);
  end;
end;
{$endregion}
{$endregion}

{$region 'API functions'}
function SetGamePath(path: PWideChar): WordBool; cdecl;
begin
  Result := False;
  try
    GamePath := string(path);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetLanguage(lang: PWideChar): WordBool; cdecl;
begin
  Result := False;
  try
    Language := string(lang);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetBackupPath(path: PWideChar): WordBool; cdecl;
begin
  Result := False;
  try
    BackupPath := string(path);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetGameMode(mode: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    if wbGameName <> '' then
      raise Exception.Create('Game mode already set to: ' + wbGameName);
    SetGame(mode);
    // log message
    AddMessage(Format('Game: %s, DataPath: %s', [wbGameName, wbDataPath]));
    // set global values
    Globals.Values['GameName'] := GameMode.gameName;
    Globals.Values['AppName'] := GameMode.appName;
    Globals.Values['LongGameName'] := GameMode.longName;
    Globals.Values['DataPath'] := wbDataPath;
    Globals.Values['AppDataPath'] := wbAppDataPath;
    Globals.Values['MyGamesPath'] := wbMyGamesPath;
    Globals.Values['GameIniPath'] := wbTheGameIniFileName;
    // success
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetGamePath(mode: Integer; len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    resultStr := NativeGetGamePath(GameArray[mode]);
    if resultStr <> '' then begin
      len^ := Length(resultStr);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLoadOrder(len: PInteger): WordBool; cdecl;
var
  slPlugins, slLoadOrder: TStringList;
  sLoadPath: String;
begin
  Result := False;
  try
    slPlugins := TStringList.Create;
    slLoadOrder := TStringList.Create;

    try
      sLoadPath := Globals.Values['AppDataPath'];
      LoadPluginsList(sLoadPath, slPlugins, True);
      LoadLoadOrder(sLoadPath, slLoadOrder, slPlugins);

      // if GameMode is not SkyrimSE or Fallout 4 and we don't
      // have a loadorder.txt, sort by date modified
      if (wbGameMode <> gmSSE) and (wbGameMode <> gmFO4)
      and not FileExists(sLoadPath + 'loadorder.txt') then begin
        GetPluginDates(slLoadOrder);
        slLoadOrder.CustomSort(PluginListCompare);
      end;

      // add base masters if missing
      AddBaseMasters(slLoadOrder);

      // SET RESULT STRING
      resultStr := slLoadOrder.Text;
      Delete(resultStr, Length(resultStr) - 1, 2);
      len^ := Length(resultStr);
      Result := True;
    finally
      slPlugins.Free;
      slLoadOrder.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetActivePlugins(len: PInteger): WordBool; cdecl;
var
  slPlugins: TStringList;
  sLoadPath: String;
begin
  Result := False;
  try
    slPlugins := TStringList.Create;

    try
      sLoadPath := Globals.Values['AppDataPath'];
      LoadPluginsList(sLoadPath, slPlugins);
      AddBaseMasters(slPlugins);

      // SET RESULT STRING
      resultStr := slPlugins.Text;
      Delete(resultStr, Length(resultStr) - 1, 2);
      len^ := Length(resultStr);
      Result := True;
    finally
      slPlugins.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function LoadPlugins(loadOrder: PWideChar; smartLoad: WordBool): WordBool; cdecl;
begin
  Result := False;
  try
    // exit if loader is already active
    if LoaderState <> lsInactive then
      raise Exception.Create('Error: You can only call LoadPlugins once per session. ' +
        'Use LoadPlugin to load additional plugins.');
    
    // prepare load order
    slLoadOrder := TStringList.Create;
    if smartLoad then
      BuildLoadOrder(loadOrder)
    else
      slLoadOrder.Text := loadOrder;

    // start loader thread
    SetLoaderState(lsActive);
    LoaderThread := TLoaderThread.Create;
    Result := True;
  except
    on x: Exception do begin  
      slLoadOrder.Free;
      ExceptionHandler(x);
    end;
  end;
end;

function LoadPlugin(fileName: PWideChar): WordBool; cdecl;
begin
  Result := False;
  try
    // prepare load order
    slLoadOrder := TStringList.Create;
    slLoadOrder.Add(fileName);

    // start loader thread
    SetLoaderState(lsActive);
    LoaderThread := TLoaderThread.Create;
    Result := True;
  except
    on x: Exception do begin 
      slLoadOrder.Free;
      ExceptionHandler(x);
    end;
  end;
end;

function LoadPluginHeader(fileName: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    _file := LoadFileHeader(fileName);
    _res^ := Store(_file);
    Result := True;
  except
    on x: Exception do
      ExceptionHandler(x);
  end;
end;

function BuildReferences(_id: Cardinal; synchronous: WordBool): WordBool; cdecl;
begin
  Result := False;
  try
    if not synchronous then
      BuildReferencesAsync(_id)
    else
      BuildReferencesSync(_id);

    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLoaderStatus(status: PByte): WordBool; cdecl;
begin
  Result := False;
  try
    status^ := Ord(LoaderState);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function UnloadPlugin(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
  container: IwbContainer;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file)
    or not Supports(_file, IwbContainer, container) then
      raise Exception.Create('Interface must be a file.');
    if csRefsBuild in container.GetContainerStates then
      raise Exception.Create('Cannot unload plugin which has had refs built.');
    for i := Low(xFiles) to High(xFiles) do
      if NativeFileHasMaster(xFiles[i], _file) then
        raise Exception.Create(Format('Cannot unload plugin %s, it is required by %s.', [_file.FileName, xFiles[i].FileName]));
    ForceClose(_file);
    Result := Release(_id);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

initialization
  slSavedFiles := TStringList.Create;
  LoaderState := lsInactive;

finalization
  slSavedFiles.Free;

end.
