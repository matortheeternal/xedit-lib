program XEditLibTests;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  {$IFDEF USE_DLL}
  txImports in 'tests\txImports.pas',
  {$ENDIF}
  {$IFNDEF USE_DLL}
  xeTypes in 'src\xeTypes.pas',
  xeMessages in 'src\xeMessages.pas',
  xeConfiguration in 'src\xeConfiguration.pas',
  xeMeta in 'src\xeMeta.pas',
  xeSetup in 'src\xeSetup.pas',
  xeFiles in 'src\xeFiles.pas',
  xeMasters in 'src\xeMasters.pas',
  xeElements in 'src\xeElements.pas',
  xeElementValues in 'src\xeElementValues.pas',
  xeErrors in 'src\xeErrors.pas',
  xeRecords in 'src\xeRecords.pas',
  xeSerialization in 'src\xeSerialization.pas',
  mteHelpers in 'lib\mte\mteHelpers.pas',
  mteConflict in 'lib\mte\mteConflict.pas',
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
  {$ENDIF}
  txMeta in 'tests\txMeta.pas',
  txMessages in 'tests\txMessages.pas',
  txSetup in 'tests\txSetup.pas',
  txFiles in 'tests\txFiles.pas',
  txMasters in 'tests\txMasters.pas',
  txElements in 'tests\txElements.pas',
  txElementValues in 'tests\txElementValues.pas',
  txSerialization in 'tests\txSerialization.pas',
  txRecords in 'tests\txRecords.pas',
  txErrors in 'tests\txErrors.pas',
  Argo in 'lib\Argo\Argo.pas',
  ArgoTypes in 'lib\Argo\ArgoTypes.pas',
  Mahogany in 'lib\mahogany\Mahogany.pas';

{$R XEditLib.RES}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

procedure BuildXETests;
begin
  BuildSetupTests;
  BuildMetaTests;
  BuildMessageTests;
  BuildFileHandlingTests;
  BuildMasterHandlingTests;
  BuildElementHandlingTests;
  BuildElementValueTests;
  BuildRecordHandlingTests;
  BuildSerializationTests;
  BuildPluginErrorTests;
  BuildFinalTests;
end;

procedure RunXETests;
var
  LogToConsole: TMessageProc;
begin
  // log messages to the console
  LogToConsole := procedure(msg: String)
  begin
    WriteLn(msg);
  end;

  // init xedit
  InitXEdit;
  WriteMessages;
  WriteLn(' ');

  // run the tests
  RunTests(LogToConsole);

  // report testing results
  WriteLn(' ');
  ReportResults(LogToConsole);
end;

begin
  try
    BuildXETests;
    RunXETests;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
