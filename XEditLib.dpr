library XEditLib;

uses
  SysUtils,
  xeHelpers in 'src\xeHelpers.pas',
  xeTypes in 'src\xeTypes.pas',
  xeMessages in 'src\xeMessages.pas',
  xeConfiguration in 'src\xeConfiguration.pas',
  xeMeta in 'src\xeMeta.pas',
  xeSetup in 'src\xeSetup.pas',
  xeArchives in 'src\xeArchives.pas',
  xeFiles in 'src\xeFiles.pas',
  xeMasters in 'src\xeMasters.pas',
  xeElements in 'src\xeElements.pas',
  xeElementValues in 'src\xeElementValues.pas',
  xeErrors in 'src\xeErrors.pas',
  xeRecords in 'src\xeRecords.pas',
  xeSerialization in 'src\xeSerialization.pas',
  xeConflict in 'src\xeConflict.pas',
  xeFilter in 'src\xeFilter.pas',
  wbImplementation in 'lib\xedit\wbImplementation.pas',
  wbInterface in 'lib\xedit\wbInterface.pas',
  wbBSA in 'lib\xedit\wbBSA.pas',
  wbSort in 'lib\xedit\wbSort.pas',
  wbDefinitionsFNV in 'lib\xedit\wbDefinitionsFNV.pas',
  wbDefinitionsFO3 in 'lib\xedit\wbDefinitionsFO3.pas',
  wbDefinitionsFO4 in 'lib\xedit\wbDefinitionsFO4.pas',
  wbDefinitionsTES3 in 'lib\xedit\wbDefinitionsTES3.pas',
  wbDefinitionsTES4 in 'lib\xedit\wbDefinitionsTES4.pas',
  wbDefinitionsTES5 in 'lib\xedit\wbDefinitionsTES5.pas',
  wbHelpers in 'lib\xedit\wbHelpers.pas',
  wbLocalization in 'lib\xedit\wbLocalization.pas',
  wbStreams in 'lib\xedit\wbStreams.pas',
  Argo in 'lib\Argo\Argo.pas',
  ArgoTypes in 'lib\Argo\ArgoTypes.pas',
  xeStackTrace in 'src\xeStackTrace.pas';

{$R *.RES}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

exports
  // META METHODS
  InitXEdit, CloseXEdit, GetResultString, GetResultArray, GetGlobal, GetGlobals,
  SetSortMode, Release, ReleaseNodes, Switch, GetDuplicateHandles, CleanStore,
  ResetStore,
  // MESSAGE METHODS
  GetMessagesLength, GetMessages, ClearMessages, GetExceptionMessageLength,
  GetExceptionMessage, GetExceptionStackLength, GetExceptionStack,
  // LOADING AND SET UP METHODS
  GetGamePath, SetGamePath, SetLanguage, SetBackupPath, SetGameMode, GetLoadOrder,
  GetActivePlugins, LoadPlugins, LoadPlugin, LoadPluginHeader, BuildReferences,
  GetLoaderStatus, UnloadPlugin,
  // ARCHIVE HANDLING METHODS
  ExtractContainer, ExtractFile, GetContainerFiles, GetLoadedContainers, LoadContainer,
  // FILE HANDLING METHODS
  AddFile, FileByIndex, FileByLoadOrder, FileByName, FileByAuthor, NukeFile,
  RenameFile, SaveFile, GetRecordCount, GetOverrideRecordCount, MD5Hash, CRCHash,
  SortEditorIDs, SortNames, GetFileLoadOrder,
  // MASTER HANDLING METHODS
  CleanMasters, SortMasters, AddMaster, AddMasters, AddRequiredMasters, GetMasters,
  GetRequiredBy, GetMasterNames,
  // ELEMENT HANDLING METHODS
  HasElement, GetElement, AddElement, AddElementValue, RemoveElement,
  RemoveElementOrParent, SetElement, GetElements, GetDefNames, GetAddList,
  GetContainer, GetElementFile, GetElementGroup, GetElementRecord, GetLinksTo,
  SetLinksTo, ElementCount, ElementEquals, ElementMatches, HasArrayItem,
  GetArrayItem, AddArrayItem, RemoveArrayItem, MoveArrayItem, CopyElement,
  FindNextElement, FindPreviousElement, GetSignatureAllowed, GetAllowedSignatures,
  GetIsModified, GetIsEditable, SetIsEditable, GetIsRemoveable, GetCanAdd, SortKey,
  ElementType, DefType, SmashType, ValueType, IsSorted, IsFixed,
  // PLUGIN ERROR METHODS
  CheckForErrors, GetErrorThreadDone, GetErrors, RemoveIdenticalRecords,
  // SERIALIZATION METHODS
  ElementToJson, ElementFromJson,
  // ELEMENT VALUE METHODS
  Name, LongName, DisplayName, Path, Signature, GetValue, SetValue, GetIntValue,
  SetIntValue, GetUIntValue, SetUIntValue, GetFloatValue, SetFloatValue,
  GetFlag, SetFlag, GetEnabledFlags, SetEnabledFlags, GetAllFlags, GetEnumOptions,
  SignatureFromName, NameFromSignature, GetSignatureNameMap,
  // RECORD HANDLING METHODS
  GetFormID, SetFormID, GetRecord, GetRecords, GetOverrides, GetReferencedBy,
  GetMasterRecord, GetPreviousOverride, GetWinningOverride, FindNextRecord,
  FindPreviousRecord, FindValidReferences, ExchangeReferences, IsMaster, IsInjected,
  IsOverride, IsWinningOverride, GetNodes, GetConflictData, GetNodeElements,
  // FILTERING METHODS
  FilterRecord, ResetFilter;

begin
  SysUtils.FormatSettings.DecimalSeparator := '.';
  IsMultiThread := True;
end.


