unit txImports;

interface

type
  CardinalArray = array of Cardinal;
  PCardinalArray = ^CardinalArray;
  TErrorTypeID = ( erITM, erITPO, erDR, erUES, erURR, erUER, erUnknown );

  // META METHODS
  procedure InitXEdit; cdecl; external 'XEditLib.dll';
  procedure CloseXEdit; cdecl; external 'XEditLib.dll';
  function GetResultString(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetResultArray(_res: PCardinal; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobal(key: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobals(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Release(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function Switch(_id, _id2: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetDuplicateHandles(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ResetStore: WordBool; cdecl; external 'XEditLib.dll';

  // MESSAGE METHODS
  procedure GetMessagesLength(len: PInteger); cdecl; external 'XEditLib.dll';
  function GetMessages(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  procedure ClearMessages; cdecl; external 'XEditLib.dll';
  procedure GetExceptionMessageLength(len: PInteger); cdecl; external 'XEditLib.dll';
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';

  // LOADING AND SET UP METHODS
  function SetGameMode(mode: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGamePath(gameMode: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetLoadOrder(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetActivePlugins(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugin(filename: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function BuildReferences(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetLoaderDone: WordBool; cdecl; external 'XEditLib.dll';
  function UnloadPlugin(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // FILE HANDLING METHODS
  function AddFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function MD5Hash(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function CRCHash(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SortEditorIDs(_id: Cardinal; sig: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function SortNames(_id: Cardinal; sig: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // MASTER HANDLING METHODS
  function CleanMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SortMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function AddMasters(_id: Cardinal; masters: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT HANDLING METHODS
  function HasElement(_id: Cardinal; key: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElementOrParent(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; key: PWideChar; sortBy: Byte; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementRecord(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementEquals(_id, _id2: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function ElementMatches(_id: Cardinal; path, value: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function HasArrayItem(_id: Cardinal; path, subpath, value: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetArrayItem(_id: Cardinal; path, subpath, value: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddArrayItem(_id: Cardinal; path, subpath, value: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveArrayItem(_id: Cardinal; path, subpath, value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function MoveArrayItem(_id: Cardinal; index: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function CopyElement(_id, _id2: Cardinal; aAsNew: WordBool; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetSignatureAllowed(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SortKey(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementType(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function DefType(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SmashType(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT VALUE METHODS
  function Name(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function LongName(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function DisplayName(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Path(_id: Integer; full: WordBool; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Signature(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetValue(_id: Integer; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetValue(_id: Integer; path: PWideChar; value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetIntValue(_id: Integer; path: PWideChar; value: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetIntValue(_id: Integer; path: PWideChar; value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetUIntValue(_id: Integer; path: PWideChar; value: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetUIntValue(_id: Integer; path: PWideChar; value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFloatValue(_id: Integer; path: PWideChar; value: PDouble): WordBool; cdecl; external 'XEditLib.dll';
  function SetFloatValue(_id: Integer; path: PWideChar; value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function GetFlag(_id: Integer; path: PWideChar; name: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetFlag(_id: Integer; path: PWideChar; name: PWideChar; enabled: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnabledFlags(_id: Integer; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetEnabledFlags(_id: Integer; path: PWideChar; flags: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetAllFlags(_id: Integer; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SignatureFromName(name: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function NameFromSignature(sig: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetSignatureNameMap(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // SERIALIZATION METHODS
  function ElementToJson(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // RECORD HANDLING METHODS
  function GetFormID(_id: Cardinal; formID: PCardinal; local: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetFormID(_id: Cardinal; formID: Cardinal; local, fixReferences: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetRecords(_id: Cardinal; search: PWideChar; allowOverrides: WordBool; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetOverrides(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ExchangeReferences(_id, oldFormID, newFormID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetReferencedBy(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function IsMaster(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsInjected(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsWinningOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function ConflictAll(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function ConflictThis(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';

  // PLUGIN ERROR METHODS
  function CheckForErrors(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetErrorThreadDone: WordBool; cdecl; external 'XEditLib.dll';
  function GetErrors(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetErrorString(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

implementation
end.
