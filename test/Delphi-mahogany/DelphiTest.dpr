program DelphiTest;

{$APPTYPE CONSOLE}

uses
  ShareMem,
  SysUtils,
  txMeta in 'txMeta.pas',
  txSetup in 'txSetup.pas',
  txFiles in 'txFiles.pas',
  txFileValues in 'txFileValues.pas',
  txMasters in 'txMasters.pas',
  txElements in 'txElements.pas',
  txElementValues in 'txElementValues.pas',
  txSerialization in 'txSerialization.pas',
  txGroups in 'txGroups.pas',
  txRecords in 'txRecords.pas',
  txRecordValues in 'txRecordValues.pas',
  superobject in '..\..\lib\superobject\superobject.pas',
  maMain in '..\..\lib\mahogany\maMain.pas';

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
