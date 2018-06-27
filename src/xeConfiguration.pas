unit xeConfiguration;

interface

uses
  SysUtils, Classes, ShlObj,
  // xedit units
  wbInterface, wbBSA, wbDefinitionsFO4, wbDefinitionsTES5, wbDefinitionsTES4,
  wbDefinitionsFNV, wbDefinitionsFO3;

type
  {$region 'Types'}
  TGameMode = Record
    longName: string;
    gameName: string;
    gameMode: TwbGameMode;
    regName: string;
    appName: string;
    exeName: string;
    appIDs: string;
    abbrName: string;
  end;
  {$endregion}

  {$region 'Functions'}
  procedure SetGame(id: integer);
  function NativeGetGamePath(mode: TGameMode): string;
  {$endregion}

var
  Globals, slLanguageMap: TStringList;
  wbAppDataPath, wbMyGamesPath, BackupPath: String;
  GameMode: TGameMode;
  ProgramVersion, GamePath, Language: String;
  HideChildGroups: Boolean;

const
  {$region 'Game modes'}
  GameArray: array[0..5] of TGameMode = (
    ( longName: 'Fallout New Vegas'; gameName: 'FalloutNV'; gameMode: gmFNV;
      regName: 'FalloutNV'; appName: 'FNV'; exeName: 'FalloutNV.exe';
      appIDs: '22380,2028016'; ),
    ( longName: 'Fallout 3'; gameName: 'Fallout3'; gameMode: gmFO3;
      regName: 'Fallout3'; appName: 'FO3'; exeName: 'Fallout3.exe';
      appIDs: '22300,22370'; ),
    ( longName: 'Oblivion'; gameName: 'Oblivion'; gameMode: gmTES4;
      regName: 'Oblivion'; appName: 'TES4'; exeName: 'Oblivion.exe';
      appIDs: '22330,900883'; ),
    ( longName: 'Skyrim'; gameName: 'Skyrim'; gameMode: gmTES5;
      regName: 'Skyrim'; appName: 'TES5'; exeName: 'TESV.exe';
      appIDs: '72850'; ),
    ( longName: 'Skyrim Special Edition'; gameName: 'Skyrim'; gameMode: gmSSE;
      regName: 'Skyrim Special Edition'; appName: 'SSE';
      exeName: 'SkyrimSE.exe'; appIDs: '489830'; ),
    ( longName: 'Fallout 4'; gameName: 'Fallout4'; gameMode: gmFO4;
      regName: 'Fallout4'; appName: 'FO4'; exeName: 'Fallout4.exe';
      appIDs: '377160'; )
  );
  {$endregion}

implementation

uses
  StrUtils, Rtti, TypInfo,
  xeHelpers;

{$region 'SetGame'}
function GetMyGamesPath: String;
var
  profilePath: String;
begin
  Result := '';
  profilePath := GetCSIDLShellFolder(CSIDL_PERSONAL);
  if profilePath <> '' then
    Result := profilePath + 'My Games\' + wbGameName2 + '\';
end;

function GetAppDataPath: String;
var
  appDataPath: String;
begin
  Result := '';
  appDataPath := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA);
  if appDataPath <> '' then
    Result := appDataPath + wbGameName2 + '\';
end;

function GetGameIniPath: String;
begin
  Result := '';
  if wbMyGamesPath <> '' then begin
    if wbGameMode in [gmFO3, gmFNV] then
      Result := wbMyGamesPath + 'Fallout.ini'
    else
      Result := wbMyGamesPath + wbGameName + '.ini';
  end;
end;

function GetLanguageFileSuffix: String;
begin
  Result := Language;
  if (wbGameMode = gmFO4) and (slLanguageMap.IndexOfName(Result) > -1) then
    Result := slLanguageMap.Values[Result];
end;

{ Sets the game mode in the TES5Edit API }
procedure SetGame(id: integer);
var
  dataPath: String;
begin
  // default game path
  if GamePath = '' then
    GamePath := NativeGetGamePath(GameArray[id]);

  // test data path
  dataPath := GamePath + 'data\';
  if not DirectoryExists(dataPath) then
    raise Exception.Create(Format('Game Data Path "%s" does not exist.', [dataPath]));

  // default backup path
  if BackupPath = '' then
    BackupPath := dataPath + 'zEdit Backups\';

  // update xEdit vars
  GameMode := GameArray[id];
  wbGameName := GameMode.gameName;
  wbGameName2 := GameMode.regName;
  wbGameMode := GameMode.gameMode;
  wbAppName := GameMode.appName;
  wbDataPath := dataPath;
  wbVWDInTemporary := wbGameMode in [gmSSE, gmTES5, gmFO3, gmFNV];
  wbVWDAsQuestChildren := wbGameMode = gmFO4;
  wbArchiveExtension := IfThen(wbGameMode = gmFO4, '.ba2', '.bsa');
  wbLoadBSAs := wbGameMode in [gmFO4, gmSSE, gmTES5, gmTES4];
  wbSimpleRecords := False;
  wbDisplayLoadOrderFormID := True;
  wbSortSubRecords := True;
  wbDisplayShorterNames := True;
  wbHideUnused := True;
  wbFlagsAsArray := True;
  wbRequireLoadOrder := True;
  wbLanguage := GetLanguageFileSuffix;
  wbStringEncoding := seUTF8;
  wbEditAllowed := True;
  wbLoaderDone := True;
  wbContainerHandler := wbCreateContainerHandler;
  wbContainerHandler._AddRef;
  wbAppDataPath := GetAppDataPath;
  wbMyGamesPath := GetMyGamesPath;
  wbTheGameIniFileName := GetGameIniPath;

  // load definitions
  case wbGameMode of
    gmSSE: DefineTES5;
    gmFO4: DefineFO4;
    gmTES5: DefineTES5;
    gmFNV: DefineFNV;
    gmTES4: DefineTES4;
    gmFO3: DefineFO3;
  end;
end;
{$endregion}

{ Gets the path of a game from registry key or app path }
function NativeGetGamePath(mode: TGameMode): string;
const
  sBethRegKey     = '\SOFTWARE\Bethesda Softworks\';
  sBethRegKey64   = '\SOFTWARE\Wow6432Node\Bethesda Softworks\';
  sSteamRegKey    = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+
    'Steam App ';
  sSteamRegKey64  = '\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\'+
    'Uninstall\Steam App ';
var
  i: Integer;
  keys, appIDs: TStringList;
begin
  Result := '';

  // initialize variables
  keys := TStringList.Create;
  appIDs := TStringList.Create;
  appIDs.CommaText := mode.appIDs;

  // add keys to check
  keys.Add(sBethRegKey + mode.regName + '\Installed Path');
  keys.Add(sBethRegKey64 + mode.regName + '\Installed Path');
  for i := 0 to Pred(appIDs.Count) do begin
    keys.Add(sSteamRegKey + appIDs[i] + '\InstallLocation');
    keys.Add(sSteamRegKey64 + appIDs[i] + '\InstallLocation');
  end;

  // try to find path from registry
  Result := TryRegistryKeys(keys);

  // free memory
  keys.Free;
  appIDs.Free;

  // set result
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

initialization
  ProgramVersion := GetVersionMem;
  Globals := TStringList.Create;
  GamePath := '';
  HideChildGroups := True;
  Language := 'English';
  slLanguageMap := TStringList.Create;
  slLanguageMap.Text :=
    'English=en'#13 +
    'French=fr'#13 +
    'German=de'#13 +
    'Italian=it'#13 +
    'Spanish=es'#13 +
    'Russian=ru'#13 +
    'Polish=pl'#13 +
    'Japanese=ja'#13 +
    'Portugese=pt'#13 +
    'Chinese=zh';

finalization
  Globals.Free;
  slLanguageMap.Free;

end.
