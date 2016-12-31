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

  procedure SetGameMode(mode: Integer); cdecl;
  function GetLoadOrder(str: PWideChar; len: Integer): WordBool; cdecl;
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl;
  function GetLoaderDone: WordBool; cdecl;

  // LOAD ORDER HELPERS
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;

var
  Files: array of IwbFile;
  slLoadOrder: TStringList;

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

procedure TLoaderThread.Execute;
var
  i: Integer;
  sFileName: String;
  _file: IwbFile;
begin
  try
    for i := 0 to Pred(slLoadOrder.Count) do begin
      sFileName := slLoadOrder[i];
      AddMessage(Format('Loading %s (%d/%d)', [sFileName, i + 1, slLoadOrder.Count]));

      // load plugin
      try
        _file := wbFile(wbDataPath + sFileName, i, '', false, false);
        _file._AddRef;
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := _file;
      except
        on x: Exception do begin
          AddMessage('Exception loading ' + sFileName);
          AddMessage(x.Message);
          raise x;
        end;
      end;

      // load hardcoded dat
      if i = 0 then try
        _file := wbFile(Globals.Values['ProgramPath'] + wbGameName + wbHardcodedDat, 0);
        _file._AddRef;
        SetLength(Files, Length(Files) + 1);
        Files[High(Files)] := _file;
      except
        on x: Exception do begin
          AddMessage('Exception loading ' + wbGameName + wbHardcodedDat);
          raise x;
        end;
      end;
    end;

    // done loading
    ProgramStatus.bLoaderDone := true;
    AddMessage('Done loading files.');
  except
    on E: Exception do begin
      AddMessage('Fatal Error: <' + e.ClassName + ': ' + e.Message + '>');
      wbLoaderError := true;
    end;
  end;
end;

procedure SetGameMode(mode: Integer); cdecl;
begin
  try
    SetGame(mode);
    // set global values
    Globals.Values['GameName'] := ProgramStatus.GameMode.gameName;
    Globals.Values['AppName'] := ProgramStatus.GameMode.appName;
    Globals.Values['LongGameName'] := ProgramStatus.GameMode.longName;
    Globals.Values['DataPath'] := settings.gameDataPath;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLoadOrder(str: PWideChar; len: Integer): WordBool; cdecl;
var
  slPlugins, slLoadOrder: TStringList;
  sLoadPath, sPath: String;
begin
  Result := false;
  try
    slPlugins := TStringList.Create;
    slLoadOrder := TStringList.Create;

    try                                           
      sLoadPath := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA) + wbGameName+'\';  
      // LOAD LIST OF ACTIVE PLUGINS (plugins.txt)      
      slPlugins := TStringList.Create;
      sPath := sLoadPath + 'plugins.txt';
      if FileExists(sPath) then
        slPlugins.LoadFromFile(sPath)
      else
        AddMissingFiles(slPlugins);

      // PREPARE PLUGINS LIST
      RemoveCommentsAndEmpty(slPlugins);
      RemoveMissingFiles(slPlugins);

      // LOAD ORDER OF ALL PLUGINS (loadorder.txt)    
      sPath := sLoadPath + 'loadorder.txt';
      if FileExists(sPath) then
        slLoadOrder.LoadFromFile(sPath)
      else
        slLoadOrder.AddStrings(slPlugins);

      // PREPARE LOAD ORDER
      RemoveCommentsAndEmpty(slLoadOrder);
      RemoveMissingFiles(slLoadOrder);
      AddMissingFiles(slLoadOrder);

      // if GameMode is not Skyrim sort by date modified else add
      // Update.esm and Skyrim.esm to load order if they're missing
      if wbGameMode <> gmTES5 then begin
        GetPluginDates(slPlugins);
        GetPluginDates(slLoadOrder);
        slPlugins.CustomSort(PluginListCompare);
        slLoadOrder.CustomSort(PluginListCompare);
      end
      else begin
        if slLoadOrder.IndexOf('Skyrim.esm') = -1 then
          slLoadOrder.Insert(0, 'Skyrim.esm');
        if slLoadOrder.IndexOf('Update.esm') = -1 then
          slLoadOrder.Insert(1, 'Update.esm');
      end;

      // RETURN RESULT
      StrLCopy(str, PWideChar(WideString(slLoadOrder.CommaText)), len);
      Result := true;
    finally
      slPlugins.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl;
begin
  Result := false;
  try
    // exit if we have already started loading plugins
    if Assigned(slLoadOrder) then exit;
    
    // store load order we're going to use in slLoadOrder
    slLoadOrder := TStringList.Create;
    slLoadOrder.StrictDelimiter := true;
    slLoadOrder.Delimiter := ',';
    slLoadOrder.DelimitedText := string(loadOrder);

    // set filecount global
    Globals.Values['FileCount'] := IntToStr(slLoadOrder.Count);

    // start loader thread
    TLoaderThread.Create;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLoaderDone: WordBool; cdecl;
begin
  Result := ProgramStatus.bLoaderDone;
end;


{******************************************************************************}
{ LOAD ORDER HELPERS
  Set of helper functions for building a working load order.
{******************************************************************************}

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
    if Trim(s) = '' then
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
