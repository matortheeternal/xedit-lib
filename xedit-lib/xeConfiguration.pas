unit xeConfiguration;

interface

uses
  SysUtils, Classes, ShlObj,
  // mte units
  mteHelpers, CRC32, RttiIni,
  // xedit units
  wbInterface, wbBSA, wbDefinitionsFO4, wbDefinitionsTES5, wbDefinitionsTES4,
  wbDefinitionsFNV, wbDefinitionsFO3;

type
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
  TSettings = class(TObject)
  public
    [IniSection('General')]
    language: string;
    [IniSection('Games')]
    skyrimSEPath: string;
    skyrimPath: string;
    oblivionPath: string;
    fallout4Path: string;
    fallout3Path: string;
    falloutNVPath: string;
    constructor Create; virtual;
    function GameDataPath: String;
    procedure UpdateGamePaths;
    procedure SetGamePath(key: String; gameMode: Integer);
  end;
  TProgramStatus = class(TObject)
  public
    bLoaderDone: boolean;
    ProgramVersion: string;
    GameMode: TGameMode;
    constructor Create; virtual;
  end;

  procedure SetGame(id: integer);
  function NativeGetGamePath(mode: TGameMode): string;
  function SetGameAbbr(abbrName: string): boolean;
  function SetGameParam(param: string): boolean;
  procedure LoadSettings;
  procedure SaveSettings;

var
  ProgramStatus: TProgramStatus;
  Globals: TStringList;
  settings: TSettings;
  dummyPluginHash: string;

const
  // GAME MODES
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

implementation

uses
  StrUtils, Rtti, TypInfo;


{ TSettings }
constructor TSettings.Create;
begin
  language := 'English';
  UpdateGamePaths;
end;

function TSettings.GameDataPath: string;
begin
  case ProgramStatus.GameMode.gameMode of
    gmTES5: Result := skyrimPath;
    gmTES4: Result := oblivionPath;
    gmFNV: Result := falloutNVPath;
    gmFO3: Result := fallout3Path;
    gmFO4: Result := fallout4Path;
    gmSSE: Result := skyrimSEPath;
  end;
end;

procedure TSettings.SetGamePath(key: String; gameMode: Integer);
var
  ctx: TRttiContext;
  objType: TRttiType;
  field: TRttiField;
  path: String;
begin
  ctx := TRttiContext.Create;
  try
    objType := ctx.GetType(self.ClassInfo);
    field := objType.GetField(key);
    path := field.GetValue(self).AsString;
    if (path = '') or (path = 'data\') then
      field.SetValue(self, NativeGetGamePath(GameArray[gameMode]) + 'data\');
  finally
    ctx.Free;
  end;
end;

procedure TSettings.UpdateGamePaths;
begin
  SetGamePath('falloutNVPath', 0);
  SetGamePath('fallout3Path', 1);
  SetGamePath('oblivionPath', 2);
  SetGamePath('skyrimPath', 3);
  SetGamePath('skyrimSEPath', 4);
  SetGamePath('fallout4Path', 5);
end;

{ TProgramStatus }
constructor TProgramStatus.Create;
begin
  bLoaderDone := false;
  ProgramVersion := GetVersionMem;
end;

{ Sets the game mode in the TES5Edit API }
procedure SetGame(id: integer);
var
  sMyDocumentsPath, sIniPath: String;
begin
  // update our vars
  ProgramStatus.GameMode := GameArray[id];
  LoadSettings;
  SaveSettings;

  // update xEdit vars
  wbGameName := ProgramStatus.GameMode.gameName;
  wbGameName2 := ProgramStatus.GameMode.regName;
  wbGameMode := ProgramStatus.GameMode.gameMode;
  wbAppName := ProgramStatus.GameMode.appName;
  wbDataPath := settings.GameDataPath;
  wbVWDInTemporary := wbGameMode in [gmSSE, gmTES5, gmFO3, gmFNV];
  wbVWDAsQuestChildren := wbGameMode = gmFO4;
  wbArchiveExtension := IfThen(wbGameMode = gmFO4, '.ba2', '.bsa');
  wbLoadBSAs := wbGameMode in [gmFO4, gmSSE, gmTES5, gmTES4];
  wbDisplayLoadOrderFormID := True;
  wbSortSubRecords := True;
  wbDisplayShorterNames := True;
  wbHideUnused := True;
  wbFlagsAsArray := True;
  wbRequireLoadOrder := True;
  wbLanguage := settings.language;
  wbEditAllowed := True;
  wbLoaderDone := True;
  wbContainerHandler := wbCreateContainerHandler;
  wbContainerHandler._AddRef;

  // find game ini inside the user's documents folder.
  sMyDocumentsPath := GetCSIDLShellFolder(CSIDL_PERSONAL);
  if sMyDocumentsPath <> '' then begin
    sIniPath := sMyDocumentsPath + 'My Games\' + wbGameName + '\';

    if wbGameMode in [gmFO3, gmFNV] then
      wbTheGameIniFileName := sIniPath + 'Fallout.ini'
    else
      wbTheGameIniFileName := sIniPath + wbGameName + '.ini';
  end;

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
  gameName: string;
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

function SetGameAbbr(abbrName: String): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := Low(GameArray) to High(GameArray) do
    if SameText(GameArray[i].abbrName, abbrName) then begin
      SetGame(i);
      Result := true;
      exit;
    end;
end;

function SetGameParam(param: string): boolean;
var
  abbrName: string;
begin
  abbrName := Copy(param, 2, Length(param));
  Result := SetGameAbbr(abbrName);
end;

procedure LoadSettings;
begin
  settings := TSettings.Create;
  TRttiIni.Load(Globals.Values['ProgramPath'] + 'settings.ini', settings);
  settings.UpdateGamePaths;
end;

procedure SaveSettings;
begin
  TRttiIni.Save(Globals.Values['ProgramPath'] + 'settings.ini', settings);
end;

initialization
begin
  ProgramStatus := TProgramStatus.Create;
  Globals := TStringList.Create;
end;

finalization
begin
  ProgramStatus.Free;
  Globals.Free;
end;

end.
