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
  function GetMyGamesPath: String;
  function GetGameIniPath(myGamesPath: String; GameMode: TGameMode): String;
  procedure SetGame(id: integer);
  function NativeGetGamePath(mode: TGameMode): string;
  {$endregion}

var
  Globals, slLanguageMap, slUTF8Languages: TStringList;
  wbAppDataPath, wbMyGamesPath, BackupPath: String;
  GameMode: TGameMode;
  ProgramVersion, GamePath, Language: String;

const
  {$region 'Game modes'}
  GameArray: array[0..7] of TGameMode = (
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
      appIDs: '377160'; ),
    ( longName: 'Skyrim VR'; gameName: 'Skyrim'; gameMode: gmTES5VR;
      regName: 'Skyrim VR'; appName: 'TES5VR';
      exeName: 'SkyrimVR.exe'; appIDs: '611670'; ),
    ( longName: 'Fallout 4 VR'; gameName: 'Fallout4'; gameMode: gmFO4VR;
      regName: 'Fallout 4 VR'; appName: 'FO4VR';
      exeName: 'Fallout4VR.exe'; appIDs: '611660'; )
  );
  {$endregion}
  {$region 'TES4Languages'}
  TES4Languages: array[1..4] of String = (
    'German', 'French', 'Spanish', 'Italian'
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

function GetGameIniPath(myGamesPath: String; GameMode: TGameMode): String;
begin
  Result := '';
  if myGamesPath <> '' then begin
    if GameMode.gameMode in [gmFO3, gmFNV] then
      Result := myGamesPath + 'Fallout.ini'
    else
      Result := myGamesPath + GameMode.gameName + '.ini';
  end;
end;

function GetLanguage: String;
begin
  Result := Language;
  if (wbGameMode in [gmFO4, gmFO4VR]) and (slLanguageMap.IndexOfName(Result) > -1) then
    Result := slLanguageMap.Values[Result];
end;

function GetLanguageEncoding: TwbStringEncoding;
begin
  Result := seCP1252;
  if (slUTF8Languages.IndexOf(Language) > -1)
  or ((wbGameMode in [gmSSE, gmFO4, gmTES5VR, gmFO4VR]) and (Language <> 'English')) then
    Result := seUTF8;
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
  wbVWDInTemporary := wbGameMode in [gmTES5VR, gmSSE, gmTES5, gmFO3, gmFNV];
  wbVWDAsQuestChildren := wbGameMode in [gmFO4VR, gmFO4];
  wbArchiveExtension := IfThen(wbGameMode in [gmFO4VR, gmFO4], '.ba2', '.bsa');
  wbLoadBSAs := wbGameMode in [gmFO4VR, gmFO4, gmTES5VR, gmSSE, gmTES5, gmTES4];
  wbSimpleRecords := False;
  wbDisplayLoadOrderFormID := True;
  wbSortSubRecords := True;
  wbDisplayShorterNames := True;
  wbHideUnused := True;
  wbFlagsAsArray := True;
  wbRequireLoadOrder := True;
  wbEditAllowed := True;
  wbLoaderDone := True;
  wbContainerHandler := wbCreateContainerHandler;
  wbContainerHandler._AddRef;
  wbAppDataPath := GetAppDataPath;
  wbMyGamesPath := GetMyGamesPath;
  wbTheGameIniFileName := GetGameIniPath(wbMyGamesPath, GameMode);
  wbLanguage := GetLanguage;
  wbStringEncoding := GetLanguageEncoding;

  // load definitions
  case wbGameMode of
    gmFO4VR: DefineFO4;
    gmFO4: DefineFO4;
    gmTES5VR: DefineTES5
    gmSSE: DefineTES5;
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
  slUTF8Languages := TStringList.Create;
  slUTF8Languages.Text :=
    'Japanese'#13 +
    'Chinese';

finalization
  Globals.Free;
  slLanguageMap.Free;

end.
