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
  xeMeta in 'xeMeta.pas',
  xeSetup in 'xeSetup.pas',
  xeFiles in 'xeFiles.pas',
  xeMasters in 'xeMasters.pas',
  xeFileValues in 'xeFileValues.pas',
  xeElements in 'xeElements.pas',
  xeElementValues in 'xeElementValues.pas';

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

exports
  // META METHODS
  Initialize, Finalize, GetBuffer, FlushBuffer, ResetStore,
  // LOADING AND SET UP METHODS
  SetGameMode, GetLoadOrder, LoadPlugins, GetGlobal,
  // FILE HANDLING METHODS
  NewFile, FileByIndex, FileByLoadOrder, FileByName, FileByAuthor,
  GetElementFile, SaveFile,
  // MASTER HANDLING METHODS
  CleanMasters, SortMasters, AddMaster, RemoveMaster,
  // ELEMENT HANDLING METHODS
  GetElement, NewElement, RemoveElement, ElementExists, ElementCount, Assigned,
  Equals, IsMaster, IsInjected, IsOverride, IsWinningOverride;

begin
  IsMultiThread := True;
end.


