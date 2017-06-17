unit txMasters;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildMasterHandlingTests;

implementation

uses
  Mahogany,
  txMeta,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeMasters;
{$ENDIF}

procedure BuildMasterHandlingTests;
var
  h: Cardinal;
begin
  {$IF false}
  Describe('Master Handling Functions', procedure
    begin
      Describe('AddMaster', procedure
        begin
          It('Should return true if it succeeds', procedure
            begin
              ExpectSuccess(AddMaster(3, 'xtest-1.esp');
            end);
        end);
    end);
  {$IFEND}
end;

end.
