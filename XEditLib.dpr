library XEditLib;

uses
  ShareMem,
  xeMessages in 'src\xeMessages.pas',
  xeConfiguration in 'src\xeConfiguration.pas',
  xeMeta in 'src\xeMeta.pas',
  xeSetup in 'src\xeSetup.pas',
  xeFiles in 'src\xeFiles.pas',
  xeMasters in 'src\xeMasters.pas',
  xeFileValues in 'src\xeFileValues.pas',
  xeElements in 'src\xeElements.pas',
  xeElementValues in 'src\xeElementValues.pas',
  xeErrors in 'src\xeErrors.pas',
  xeRecords in 'src\xeRecords.pas',
  xeRecordValues in 'src\xeRecordValues.pas',
  xeGroups in 'src\xeGroups.pas',
  xeSerialization in 'src\xeSerialization.pas',
  mteHelpers in 'lib\mte\mteHelpers.pas',
  CRC32 in 'lib\mte\CRC32.pas',
  RttiIni in 'lib\mte\RttiIni.pas',
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
  Argo in 'lib\Argo\Argo.pas';

{$R *.RES}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

exports
  // META METHODS
  InitXEdit, CloseXEdit, GetResultString, GetResultArray, GetGlobal, GetGlobals,
  Release, ResetStore,
  // MESSAGE METHODS
  GetMessagesLength, GetMessages, ClearMessages, GetExceptionMessageLength,
  GetExceptionMessage,
  // LOADING AND SET UP METHODS
  SetGameMode, GetLoadOrder, LoadPlugins, GetLoaderDone, GetGamePath,
  // FILE HANDLING METHODS
  AddFile, FileByIndex, FileByLoadOrder, FileByName, FileByAuthor, SaveFile,
  // MASTER HANDLING METHODS
  CleanMasters, SortMasters, AddMaster, GetMaster, GetMasters, GetRequiredBy,
  // FILE VALUE METHODS
  GetFileHeader, GetNextObjectId, SetNextObjectID, GetFileName, GetAuthor,
  SetAuthor, GetDescription, SetDescription, OverrideRecordCount, GetIsESM,
  SetIsESM,
  // ELEMENT HANDLING METHODS
  GetElement, GetElements, GetElementFile, GetContainer, AddElement,
  RemoveElement, RemoveElementOrParent, ElementExists, ElementCount, GetLinksTo,
  ElementEquals, CopyElement, MoveElement, GetExpectedSignatures, SortKey,
  ElementType, DefType,
  // ERROR CHECKING METHODS
  CheckForErrors, GetErrorThreadDone, GetErrors, GetErrorString,
  // SERIALIZATION METHODS
  ElementToJson, {ElementFromJson,}
  // ELEMENT VALUE METHODS
  Name, LongName, DisplayName, Path, EditorID, Signature, FullName, GetValue,
  SetValue, GetIntValue, SetIntValue, GetUIntValue, SetUIntValue, GetFloatValue,
  SetFloatValue, SetFlag, GetFlag, ToggleFlag, GetEnabledFlags,
  // GROUP HANDLING METHODS
  HasGroup, AddGroup, GetGroupSignatures, GetChildGroup, GroupSignatureFromName,
  GroupNameFromSignature, GetGroupSignatureNameMap,
  // RECORD HANDLING METHODS
  AddRecord, GetRecords, RecordsBySignature, RecordByFormID, RecordByEditorID,
  RecordByName, GetOverrides, GetFormID, SetFormID, ExchangeReferences,
  GetReferences, IsMaster, IsInjected, IsOverride, IsWinningOverride;
  // RECORD VALUE METHODS

begin
  IsMultiThread := True;
end.


