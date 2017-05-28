unit txImports;

interface

type
  CardinalArray = array of Cardinal;
  PCardinalArray = ^CardinalArray;

  // META METHODS
  procedure InitXEdit; cdecl; external 'XEditLib.dll';
  procedure CloseXEdit; cdecl; external 'XEditLib.dll';
  procedure GetMessagesLength(len: PInteger); cdecl; external 'XEditLib.dll';
  procedure GetMessages(str: PWideChar; len: Integer); cdecl; external 'XEditLib.dll';
  procedure ClearMessages; cdecl; external 'XEditLib.dll';
  procedure GetResultString(str: PWideChar; len: Integer); cdecl; external 'XEditLib.dll';
  procedure GetResultArray(_res: PCardinal; len: Integer); cdecl; external 'XEditLib.dll';
  procedure GetExceptionMessageLength(len: PInteger); cdecl; external 'XEditLib.dll';
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Release(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ResetStore: WordBool; cdecl; external 'XEditLib.dll';

  // LOADING AND SET UP METHODS
  function SetGameMode(mode: Integer): Boolean; cdecl; external 'XEditLib.dll';
  function GetLoadOrder(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar): WordBool; cdecl; cdecl; external 'XEditLib.dll';
  function GetLoaderDone: WordBool; cdecl; external 'XEditLib.dll';
  function GetGamePath(gameMode: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // MASTER HANDLING METHODS
  function CleanMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SortMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetMaster(_id: Cardinal; index: Integer): Cardinal; cdecl; external 'XEditLib.dll';

  // FILE HANDLING METHODS
  function NewFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // FILE VALUE METHODS
  function GetFileHeader(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetNextObjectId(_id: Cardinal; nextObjectID: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileName(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetAuthor(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetDescription(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetIsESM(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetIsESM(_id: Cardinal; bool: WordBool): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT HANDLING METHODS
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ElementExists(_id: Cardinal; key: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementEquals(_id, _id2: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsMaster(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsInjected(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsWinningOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT VALUE METHODS
  function Name(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Path(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function EditorID(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Signature(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function FullName(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SortKey(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementType(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function DefType(_id: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetValue(_id: Integer; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetValue(_id: Integer; path: PWideChar; value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetIntValue(_id: Integer; path: PWideChar; value: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetIntValue(_id: Integer; path: PWideChar; value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetUIntValue(_id: Integer; path: PWideChar; value: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetUIntValue(_id: Integer; path: PWideChar; value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFloatValue(_id: Integer; path: PWideChar; value: PDouble): WordBool; cdecl; external 'XEditLib.dll';
  function SetFloatValue(_id: Integer; path: PWideChar; value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function SetFlag(_id: Integer; path: PWideChar; name: PWideChar; enabled: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetFlag(_id: Integer; path: PWideChar; name: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function ToggleFlag(_id: Integer; path: PWideChar; name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnabledFlags(_id: Integer; path: PWideChar; flags: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // SERIALIZATION METHODS
  function ElementToJson(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  //function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';

  // GROUP HANDLING METHODS
  function HasGroup(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function AddGroup(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetGroupSignatures(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GroupSignatureFromName(name: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GroupNameFromSignature(sig: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetGroupSignatureNameMap(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // RECORD HANDLING METHODS
  function AddRecord(_id: Cardinal; sig: string; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetRecords(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function RecordsBySignature(_id: Cardinal; sig: string; _res: PCardinalArray): WordBool; cdecl; external 'XEditLib.dll';
  function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RecordByFormID(_id, formID: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RecordByEditorID(_id: Cardinal; edid: string; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RecordByName(_id: Cardinal; full: string; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function OverrideCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function OverrideByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

implementation
end.
