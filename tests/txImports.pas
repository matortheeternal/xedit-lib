unit txImports;

interface

type
  CardinalArray = array of Cardinal;
  PCardinalArray = ^CardinalArray;
  TLoaderState = ( lsInactive, lsActive, lsDone, lsError );
  TErrorTypeID = ( erITM, erITPO, erDR, erUES, erURR, erUER, erUnknown );
  TValueType = ( vtUnknown, vtBytes, vtNumber, vtString, vtText, vtReference, vtFlags,
    vtEnum, vtColor, vtArray, vtStruct );

  // META METHODS
  procedure InitXEdit; cdecl; external 'XEditLib.dll';
  procedure CloseXEdit; cdecl; external 'XEditLib.dll';
  function GetResultString(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetResultArray(_res: PCardinal; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetResultBytes(_res: PByte; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobal(key: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobals(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetSortMode(_sortBy: Byte; _reverse: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function Release(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ReleaseNodes(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
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
  function GetGamePath(mode: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetGamePath(path: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetGameLanguage(mode: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetLanguage(lang: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function SetBackupPath(path: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function SetGameMode(mode: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetLoadOrder(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetActivePlugins(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugins(loadOrder: PWideChar; smartLoad: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPlugin(filename: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function LoadPluginHeader(fileName: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function BuildReferences(_id: Cardinal; synchronous: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetLoaderStatus(status: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function UnloadPlugin(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // FILE HANDLING METHODS
  function AddFile(filename: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByIndex(index: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByLoadOrder(load_order: Integer; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByName(name: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FileByAuthor(author: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SaveFile(_id: Cardinal; filePath: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetOverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function MD5Hash(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function CRCHash(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SortEditorIDs(_id: Cardinal; sig: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function SortNames(_id: Cardinal; sig: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileLoadOrder(_id: Cardinal; loadOrder: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // RESOURCE HANDLING METHODS
  function ExtractContainer(name, destination: PWideChar; replace: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function ExtractFile(name, source, destination: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainerFiles(name, path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetFileContainer(path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetLoadedContainers(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function LoadContainer(filePath: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function BuildArchive(name, folder, filePaths: PWideChar; archiveType: Integer;
    bCompress, bShare: WordBool; af, ff: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetTextureData(resourceName: PWideChar; width, height: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // MASTER HANDLING METHODS
  function CleanMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SortMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function AddMasters(_id: Cardinal; masters: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function AddRequiredMasters(_id, _id2: Cardinal; asNew: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetMasterNames(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT HANDLING METHODS
  function HasElement(_id: Cardinal; key: PWideChar; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddElementValue(_id: Cardinal; key, value: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElementOrParent(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetElement(_id, _id2: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; key: PWideChar; sort, filter: WordBool; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetDefNames(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetAddList(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetLinksTo(_id: Cardinal; key: PWideChar; _id2: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementIndex(_id: Cardinal; index: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementRecord(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
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
  function GetAllowedSignatures(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetIsModified(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetIsEditable(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetIsRemoveable(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetCanAdd(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SortKey(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementType(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function DefType(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function SmashType(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function ValueType(_id: Cardinal; enum: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function IsSorted(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';

  // ELEMENT VALUE METHODS
  function Name(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function LongName(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function DisplayName(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Path(_id: Cardinal; short, local: WordBool; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function Signature(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetValue(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetValue(_id: Cardinal; path, value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetIntValue(_id: Cardinal; path: PWideChar; value: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetIntValue(_id: Cardinal; path: PWideChar; value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetUIntValue(_id: Cardinal; path: PWideChar; value: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetUIntValue(_id: Cardinal; path: PWideChar; value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFloatValue(_id: Cardinal; path: PWideChar; value: PDouble): WordBool; cdecl; external 'XEditLib.dll';
  function SetFloatValue(_id: Cardinal; path: PWideChar; value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetAllFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnabledFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetEnabledFlags(_id: Cardinal; path, flags: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnumOptions(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SignatureFromName(name: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function NameFromSignature(sig: PWideChar; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetSignatureNameMap(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // SERIALIZATION METHODS
  function ElementToJson(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // RECORD HANDLING METHODS
  function GetFormID(_id: Cardinal; formID: PCardinal; local: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function SetFormID(_id: Cardinal; formID: Cardinal; local, fixReferences: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetRecord(_id: Cardinal; formID: Cardinal; searchMasters: WordBool; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetRecords(_id: Cardinal; search: PWideChar; includeOverrides: WordBool; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetREFRs(_id: Cardinal; search: PWideChar; flags: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetOverrides(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetMasterRecord(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetWinningOverride(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetInjectionTarget(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FindNextRecord(_id: Cardinal; search: PWideChar; byEdid, byName: WordBool; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FindPreviousRecord(_id: Cardinal; search: PWideChar; byEdid, byName: Wordbool; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function FindValidReferences(_id: Cardinal; signature, search: PWideChar; limitTo: Integer; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetReferencedBy(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ExchangeReferences(_id, oldFormID, newFormID: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsMaster(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsInjected(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function IsWinningOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetNodes(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetConflictData(_id: Cardinal; _id2: Cardinal; conflictAll, conflictThis: PByte): WordBool; cdecl; external 'XEditLib.dll';
  function GetNodeElements(_id: Cardinal; _id2: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';

  // PLUGIN ERROR METHODS
  function CheckForErrors(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetErrorThreadDone: WordBool; cdecl; external 'XEditLib.dll';
  function GetErrors(len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function GetErrorString(_id: Cardinal; len: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveIdenticalRecords(_id: Cardinal; removeITMs, removeITPOs: WordBool): WordBool; cdecl; external 'XEditLib.dll';

  // FILTER FUNCTIONS
  function FilterRecord(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ResetFilter: WordBool; cdecl; external 'XEditLib.dll';

implementation
end.
