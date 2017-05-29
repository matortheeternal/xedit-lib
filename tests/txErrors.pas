unit txErrors;

interface

  // public testing interface
  procedure BuildPluginErrorTests;

implementation

uses
  SysUtils,
  Mahogany,
  txMeta,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles, xeErrors;
{$ENDIF}

procedure CheckPluginForErrors(filename: String);
var
  h: Cardinal;
  len: Integer;
  errors: String;
begin
  ExpectSuccess(FileByName(PWideChar(filename), @h));
  ExpectSuccess(CheckForErrors(h));
  while not GetErrorThreadDone do
    Sleep(100);
  ExpectSuccess(GetErrors(@len));
  errors := grs(len);
  Expect(Length(errors) > 0, 'Should return errors');
end;

{$DEFINE FULL_ERROR_CHECK}
procedure BuildPluginErrorTests;
begin
  Describe('Plugin Error Functions', procedure
    begin
      {$IFDEF FULL_ERROR_CHECK}
      Describe('Full Error Check', procedure
        begin
          It('Should work on Update.esm', procedure
            begin
              CheckPluginForErrors('Update.esm');
            end);
          It('Should work on Dawnguard.esm', procedure
            begin
              CheckPluginForErrors('Dawnguard.esm');
            end);
          It('Should work on HearthFires.esm', procedure
            begin
              CheckPluginForErrors('HearthFires.esm');
            end);
          It('Should work on Dragonborn.esm', procedure
            begin
              CheckPluginForErrors('Dragonborn.esm');
            end);
        end);
      {$ENDIF}
    end);
end;

end.
