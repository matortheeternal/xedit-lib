unit xeSetup;

interface

uses
  Classes,
  // xedit units
  wbHelpers, wbInterface, wbImplementation;

type
  TLoaderThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  function SetGameMode(mode: Integer): WordBool; cdecl;
  function GetLoadOrder(len: PInteger): WordBool; cdecl;
  function GetActivePlugins(len: PInteger): WordBool; cdecl;
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl;
  function GetLoaderDone: WordBool; cdecl;
  function GetGamePath(mode: Integer; len: PInteger): WordBool; cdecl;

  // LOAD ORDER HELPERS
  procedure BuildPluginsList(sLoadPath: String; var sl: TStringList);
  procedure BuildLoadOrder(sLoadPath: String; var slLoadOrder, slPlugins: TStringList);
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  procedure AddBaseMasters(var sl: TStringList);
  procedure FixLoadOrder(var sl: TStringList; filename: String; index: Integer);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;

var
  xFiles: array of IwbFile;
  slLoadOrder: TStringList;
  LoaderThread: TLoaderThread;

implementation

uses
  SysUtils, ShlObj,
  // mte units
  mteHelpers,
  // xelib units
  xeMeta, xeConfiguration, xeMessages;


{******************************************************************************}
{ LOADIND AND SETUP METHODS
  Methods for setting the xEdit game mode and loading files.
}
{******************************************************************************}

procedure LoadFile(filePath: String; loadOrder: Integer);
var
  _file: IwbFile;
begin
  _file := wbFile(filePath, loadOrder, '', False, False);
  _file._AddRef;
  SetLength(xFiles, Length(xFiles) + 1);
  xFiles[High(xFiles)] := _file;
end;

procedure LoadHardcodedDat;
var
  _file: IwbFile;
begin
  _file := wbFile(Globals.Values['ProgramPath'] + wbGameName + wbHardcodedDat, 0);
  _file._AddRef;
  SetLength(xFiles, Length(xFiles) + 1);
  xFiles[High(xFiles)] := _file;
end;

procedure LoadPluginFiles;
var
  i: Integer;
  sFileName: String;
begin
  for i := 0 to Pred(slLoadOrder.Count) do begin
    sFileName := slLoadOrder[i];
    AddMessage(Format('Loading %s (%d/%d)', [sFileName, i + 1, slLoadOrder.Count]));

    // load plugin
    try
      LoadFile(wbDataPath + sFileName, i);
    except
      on x: Exception do begin
        AddMessage('Exception loading ' + sFileName);
        AddMessage(x.Message);
        raise x;
      end;
    end;

    // load hardcoded dat
    if i = 0 then try
      LoadHardCodedDat;
    except
      on x: Exception do begin
        AddMessage('Exception loading ' + wbGameName + wbHardcodedDat);
        raise x;
      end;
    end;
  end;
end;

procedure LoadBSAFile(sFileName: String);
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
      FindBSAs(wbTheGameIniFileName, wbDataPath, slBSAFileNames, slErrors);
      LoadBSAs(slBSAFileNames, slErrors);

      for i := Low(xFiles) to High(xFiles) do begin
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

procedure TLoaderThread.Execute;
begin
  try
    LoadPluginFiles;
    LoadResources;

    // done loading
    ProgramStatus.bLoaderDone := True;
    AddMessage('Done loading files.');
  except
    on E: Exception do begin
      AddMessage('Fatal Error: <' + e.ClassName + ': ' + e.Message + '>');
      wbLoaderError := True;
    end;
  end;
end;

function SetGameMode(mode: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    SetGame(mode);
    // log message
    AddMessage(Format('Game: %s, DataPath: %s', [wbGameName, wbDataPath]));
    // set global values
    Globals.Values['GameName'] := ProgramStatus.GameMode.gameName;
    Globals.Values['AppName'] := ProgramStatus.GameMode.appName;
    Globals.Values['LongGameName'] := ProgramStatus.GameMode.longName;
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
      BuildPluginsList(sLoadPath, slPlugins);
      BuildLoadOrder(sLoadPath, slLoadOrder, slPlugins);

      // add base masters if missing
      AddBaseMasters(slPlugins);
      AddBaseMasters(slLoadOrder);

      // if GameMode is not Skyrim, SkyrimSE or Fallout 4 sort
      // by date modified
      if not (wbGameMode in [gmTES5, gmSSE, gmFO4]) then begin
        GetPluginDates(slLoadOrder);
        slLoadOrder.CustomSort(PluginListCompare);
      end;

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
      BuildPluginsList(sLoadPath, slPlugins);
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

function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl;
begin
  Result := False;
  try
    // exit if we have already started loading plugins
    if Assigned(slLoadOrder) then exit;
    
    // store load order we're going to use in slLoadOrder
    slLoadOrder := TStringList.Create;
    slLoadOrder.Text := loadOrder;

    // set filecount global
    Globals.Values['FileCount'] := IntToStr(slLoadOrder.Count);

    // start loader thread
    LoaderThread := TLoaderThread.Create;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLoaderDone: WordBool; cdecl;
begin
  Result := ProgramStatus.bLoaderDone;
  if Result then begin
    if Assigned(LoaderThread) then LoaderThread.Free;
    if Assigned(slLoadOrder) then slLoadOrder.Free;
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


{******************************************************************************}
{ LOAD ORDER HELPERS
  Set of helper functions for building a working load order.
{******************************************************************************}

procedure BuildPluginsList(sLoadPath: String; var sl: TStringList);
var
  sPath: String;
begin
  sPath := sLoadPath + 'plugins.txt';
  if FileExists(sPath) then
    sl.LoadFromFile(sPath)
  else
    AddMissingFiles(sl);

  // remove comments and missing files
  RemoveCommentsAndEmpty(sl);
  RemoveMissingFiles(sl);
end;

procedure BuildLoadOrder(sLoadPath: String; var slLoadOrder, slPlugins: TStringList);
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
  i, j, k: integer;
  s: string;
begin
  for i := Pred(sl.Count) downto 0 do begin
    s := Trim(sl.Strings[i]);
    j := Pos('#', s);
    k := Pos('*', s);
    if j > 0 then
      System.Delete(s, j, High(Integer));
    if s = '' then
      sl.Delete(i);
    if k = 1 then
      sl[i] := Copy(s, 2, Length(s));
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
    FixLoadOrder(sl, 'Hearthfires.esm', 3);
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
procedure FixLoadOrder(var sl: TStringList; filename: String; index: Integer);
var
  oldIndex: Integer;
begin
  oldIndex := sl.IndexOf(filename);
  if (oldIndex > -1) and (oldIndex <> index) then begin
    sl.Delete(oldIndex);
    sl.Insert(index, filename);
  end;
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

end.
