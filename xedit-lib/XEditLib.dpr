library XEditLib;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  xeMessages in 'xeMessages.pas',
  xeConfiguration in 'xeConfiguration.pas',
  xeMeta in 'xeMeta.pas',
  xeSetup in 'xeSetup.pas',
  xeFiles in 'xeFiles.pas',
  xeMasters in 'xeMasters.pas',
  xeFileValues in 'xeFileValues.pas',
  xeElements in 'xeElements.pas',
  xeElementValues in 'xeElementValues.pas',
  mteHelpers in '..\lib\mte\mteHelpers.pas',
  CRC32 in '..\lib\mte\CRC32.pas',
  RttiIni in '..\lib\mte\RttiIni.pas',
  wbImplementation in '..\lib\xedit\wbImplementation.pas',
  wbInterface in '..\lib\xedit\wbInterface.pas' {$R *.res},
  wbBSA in '..\lib\xedit\wbBSA.pas',
  wbSort in '..\lib\xedit\wbSort.pas',
  wbDefinitionsFNV in '..\lib\xedit\wbDefinitionsFNV.pas',
  wbDefinitionsFO3 in '..\lib\xedit\wbDefinitionsFO3.pas',
  wbDefinitionsFO4 in '..\lib\xedit\wbDefinitionsFO4.pas',
  wbDefinitionsTES3 in '..\lib\xedit\wbDefinitionsTES3.pas',
  wbDefinitionsTES4 in '..\lib\xedit\wbDefinitionsTES4.pas',
  wbDefinitionsTES5 in '..\lib\xedit\wbDefinitionsTES5.pas',
  wbHelpers in '..\lib\xedit\wbHelpers.pas',
  wbLocalization in '..\lib\xedit\wbLocalization.pas',
  wbStreams in '..\lib\xedit\wbStreams.pas',
  Zlibex in '..\lib\xedit\Zlibex.pas';

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

exports
  // META METHODS
  Initialize, Finalize, GetBuffer, FlushBuffer, Release, ResetStore,
  // LOADING AND SET UP METHODS
  SetGameMode, GetLoadOrder, LoadPlugins, GetGlobal,
  // FILE HANDLING METHODS
  NewFile, FileByIndex, FileByLoadOrder, FileByName, FileByAuthor,
  GetElementFile, SaveFile,
  // MASTER HANDLING METHODS
  CleanMasters, SortMasters, AddMaster, RemoveMaster,
  // ELEMENT HANDLING METHODS
  GetElement, NewElement, RemoveElement, ElementExists, ElementCount,
  ElementAssigned, Equals, IsMaster, IsInjected, IsOverride, IsWinningOverride,
  // ELEMENT VALUE METHODS
  Name, EditorID, Signature, ShortName, SortKey, ElementType, DefType, GetValue,
  SetValue, GetIntValue, SetIntValue, GetUIntValue, SetUIntValue, GetFloatValue,
  SetFloatValue;

begin
  IsMultiThread := True;
end.


