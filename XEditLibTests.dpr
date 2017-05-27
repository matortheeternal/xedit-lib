program XEditLibTests;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  {$IFDEF USE_DLL}
  ShareMem,
  txImports in 'tests\txImports.pas',
  {$ENDIF}
  {$IFNDEF USE_DLL}
  xeElements in 'src\xeElements.pas',
  xeElementValues in 'src\xeElementValues.pas',
  xeErrors in 'src\xeErrors.pas',
  xeFiles in 'src\xeFiles.pas',
  xeFileValues in 'src\xeFileValues.pas',
  xeGroups in 'src\xeGroups.pas',
  xeMasters in 'src\xeMasters.pas',
  xeMeta in 'src\xeMeta.pas',
  xeRecords in 'src\xeRecords.pas',
  xeRecordValues in 'src\xeRecordValues.pas',
  xeSerialization in 'src\xeSerialization.pas',
  xeSetup in 'src\xeSetup.pas',
  {$ENDIF}
  txMeta in 'tests\txMeta.pas',
  txSetup in 'tests\txSetup.pas',
  txFiles in 'tests\txFiles.pas',
  txFileValues in 'tests\txFileValues.pas',
  txMasters in 'tests\txMasters.pas',
  txElements in 'tests\txElements.pas',
  txElementValues in 'tests\txElementValues.pas',
  txSerialization in 'tests\txSerialization.pas',
  txGroups in 'tests\txGroups.pas',
  txRecords in 'tests\txRecords.pas',
  txRecordValues in 'tests\txRecordValues.pas',
  Argo in 'lib\Argo\Argo.pas',
  Mahogany in 'lib\mahogany\Mahogany.pas';

procedure BuildXETests;
begin
  BuildMetaTests;
  BuildFileHandlingTests;
  BuildFileValueTests;
  BuildMasterHandlingTests;
  BuildElementHandlingTests;
  BuildElementValueTests;
  BuildSerializationTests;
  BuildGroupHandlingTests;
  BuildRecordHandlingTests;
  BuildRecordValueTests;
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

  // run the tests
  Initialize;
  LoadXEdit;
  RunTests(LogToConsole);
  Finalize;

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
