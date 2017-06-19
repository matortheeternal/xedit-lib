unit txErrors;

interface

  // public testing interface
  procedure BuildPluginErrorTests;

implementation

uses
  SysUtils,
  Mahogany, Argo,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles, xeErrors,
{$ENDIF}
  txMeta;

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

function FindError(obj: TJSONObject; errorType: TErrorTypeID; n: String): TJSONObject;
var
  ary: TJSONArray;
  i: Integer;
begin
  ary := obj.A['errors'];
  for i := 0 to Pred(ary.Count) do begin
    Result := ary.O[i];
    if (Result.I['group'] = Ord(errorType)) and (Pos(n, Result.S['name']) = 1) then
      exit;
  end;
  Result := nil;
end;

procedure BuildPluginErrorTests;
var
  h: Cardinal;
  errors: String;
  len: Integer;
  obj, errorObj: TJSONObject;
begin
  Describe('Plugin Error Functions', procedure
    begin
      Describe('GetErrors', procedure
        begin
          It('Should fail if no error check has been performed', procedure
            begin
              ExpectFailure(GetErrors(@len));
            end);

          It('Should return errors', procedure
            begin
              ExpectSuccess(FileByName('xtest-4.esp', @h));
              ExpectSuccess(CheckForErrors(h));
              while not GetErrorThreadDone do
                Sleep(100);
              ExpectSuccess(GetErrors(@len));
              errors := grs(len);
              Expect(Length(errors) > 0, 'Should return errors');
            end);

          It('Should be in valid JSON format', procedure
            begin
              obj := TJSONObject.Create(errors);
              Expect(obj.HasKey('errors'), 'Should have errors key');
            end);

          Describe('Errors', procedure
            begin
              Describe('Identical to Master records (ITMs)', procedure
                begin
                  It('Should be found', procedure
                    begin
                      errorObj := FindError(obj, erITM, 'ITMTest');
                      Expect(Assigned(errorObj), 'Matching error not found');
                    end);

                  It('Should include records with children', procedure
                    begin
                      errorObj := FindError(obj, erITM, 'KilkreathRuins03');
                      Expect(Assigned(errorObj), 'Matching error not found');
                    end);

                  It('Should not include injected records', procedure
                    begin
                      errorObj := FindError(obj, erITM, 'InjectedTest3');
                      Expect(not Assigned(errorObj), 'Matching error found');
                    end);
                end);

              Describe('Identical to Previous Override records (ITPOs)', procedure
                begin
                  It('Should be found', procedure
                    begin
                      errorObj := FindError(obj, erITPO, 'ITPOTest');
                      Expect(Assigned(errorObj), 'Matching error not found');
                    end);

                  It('Should not include injected records', procedure
                    begin
                      errorObj := FindError(obj, erITPO, 'InjectedTest2');
                      Expect(not Assigned(errorObj), 'Matching error found');
                    end);
                end);

              It('Should find Deleted References (UDRs)', procedure
                begin
                  errorObj := FindError(obj, erUDR, '[REFR:00027DE7]');
                  Expect(Assigned(errorObj), 'Matching error not found');
                end);

              It('Should find Unexpected References (UERs)', procedure
                begin
                  errorObj := FindError(obj, erUER, 'UERTest');
                  Expect(Assigned(errorObj), 'Matching error not found');
                end);

              It('Should find Unresolved References (URRs)', procedure
                begin
                  errorObj := FindError(obj, erURR, 'URRTest');
                  Expect(Assigned(errorObj), 'Matching error not found');
                end);

              It('Should find Unexpected Subrecord Errors (UESs)', procedure
                begin
                  errorObj := FindError(obj, erUES, 'UESTest');
                  Expect(Assigned(errorObj), 'Matching error not found');
                end);

              {It('Should find Other Errors (OEs)', procedure
                begin
                  errorObj := FindError(obj, erUnknown, 'OETest');
                  Expect(Assigned(errorObj), 'Matching error not found');
                end);}
            end);
        end);

      Describe('GetErrorString', procedure
        begin
          // TODO
        end);

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
