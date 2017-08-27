unit txMessages;

interface

  procedure BuildMessageTests;

implementation

uses
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeMeta, xeFiles, xeMessages,
{$ENDIF}
  txMeta;

procedure TestGetExceptionMessage(expectedMessage: String; callback: TProc);
var
  len: Integer;
begin
  callback;
  GetExceptionMessageLength(@len);
  ExpectEqual(gem(len), expectedMessage);
end;

procedure BuildMessageTests;
var
  h: Cardinal;
  len: Integer;
begin
  Describe('Message Methods', procedure
    begin
      Describe('GetMessages', procedure
        begin
          // TODO
        end);

      Describe('ClearMessages', procedure
        begin
          // TODO
        end);

      Describe('GetExceptionMessage', procedure
        begin
          It('Should get the last exception message', procedure
            begin
              TestGetExceptionMessage('Failed to find file with name: a.esp', procedure
                begin
                  FileByName('b.esp', @h);
                  FileByName('a.esp', @h);
                end);
            end);

          It('Should clear exception message after retrieval', procedure
            begin
              GetExceptionMessageLength(@len);
              ExpectEqual(len, 0);
            end);
        end);
    end);
end;

end.
