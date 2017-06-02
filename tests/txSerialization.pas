unit txSerialization;

interface

  // PUBLIC TESTING INTERFACE
  procedure BuildSerializationTests;

implementation

uses
  SysUtils,
  Argo, Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeSerialization, xeElements,
{$ENDIF}
  txMeta, txElements;

procedure ExpectExists(obj: TJSONObject; key: string);
begin
  Expect(obj.HasKey(key), key + ' should exist');
end;

procedure ExportFileToJSON(filename: PWideChar; mb: Integer);
var
  h: Cardinal;
  len: Integer;
begin
  Expect(GetElement(0, filename, @h), 'GetElement');
  Expect(ElementToJson(h, @len, False), 'ElementToJSON');
  WriteStringToFile(grs(len), filename + '.json');
end;

procedure BuildSerializationTests;
var
  testFile, armo, rec, cell, keywords, keyword, dnam, h: Cardinal;
  obj, obj2, obj3: TJSONObject;
  len: Integer;
  ary: TJSONArray;
begin
  Describe('Serialization', procedure
    begin
      BeforeAll(procedure
        begin
          GetElement(0, 'xtest-2.esp', @testFile);
          GetElement(testFile, 'ARMO', @armo);
          GetElement(armo, '00012E46', @rec);
          GetElement(testFile, 'CELL', @cell);
          GetElement(rec, 'KWDA', @keywords);
          GetElement(keywords, '[0]', @keyword);
          GetElement(rec, 'DNAM', @dnam);
        end);

      Describe('ElementToJSON', procedure
        begin
          Describe('Root serialization', procedure
            begin
              It('Should fail', procedure
                begin
                  ExpectFailure(ElementToJson(0, @len, False));
                end);
            end);

          Describe('File serialization', procedure
            begin
              AfterAll(procedure
                begin
                  obj.Free;
                end);

              It('Should succeed', procedure
                begin
                  ExpectSuccess(ElementToJson(testFile, @len, False));
                  obj := TJSONObject.Create(grs(len));
                end, True);

              It('Should have correct filename', procedure
                begin
                  ExpectEqual(obj.S['Filename'], 'xtest-2.esp', '');
                end);

              Describe('File Header', procedure
                begin
                  It('Should be present', procedure
                    begin
                      ExpectExists(obj, 'File Header');
                      obj2 := obj.O['File Header'];
                    end, true);

                  It('Should have correct author', procedure
                    begin
                      ExpectEqual(obj2.S['CNAM - Author'], 'Mator', '');
                    end);

                  It('Should have Record Header', procedure
                    begin
                      ExpectExists(obj2, 'Record Header');
                      obj3 := obj2.O['Record Header'];
                      ExpectEqual(obj3.I['Record Flags'], 0, '');
                      ExpectEqual(obj3.I['FormID'], 0, '');
                      ExpectEqual(obj3.S['Version Control Info 1'], '00 00 00 00', '');
                      ExpectEqual(obj3.I['Form Version'], 43, '');
                      ExpectEqual(obj3.S['Version Control Info 2'], '00 00', '');
                    end);

                  It('Should have HEDR - Header', procedure
                    begin
                      ExpectExists(obj2, 'HEDR - Header');
                      obj3 := obj2.O['HEDR - Header'];
                      ExpectEqual(obj3.D['Version'], 1.7, '');
                      ExpectEqual(obj3.I['Number of Records'], 11, '');
                      ExpectEqual(obj3.I['Next Object ID'], 2048, '');
                    end);

                  It('Should have Master Files', procedure
                    begin
                      ExpectExists(obj2, 'Master Files');
                      ary := obj2.A['Master Files'];
                      ExpectEqual(ary.Count, 2, 'There should be 2 master files');
                      ExpectEqual(ary.O[0].S['MAST - Filename'], 'Skyrim.esm', '');
                      ExpectEqual(ary.O[1].S['MAST - Filename'], 'Update.esm', '');
                    end);
                end);

              Describe('Groups', procedure
                begin
                  It('Should be present', procedure
                    begin
                      ExpectExists(obj, 'Groups');
                      obj2 := obj.O['Groups'];
                    end, True);

                  It('Should have the correct groups', procedure
                    begin
                      ExpectEqual(obj2.Count, 3, 'There should be 3 groups');
                      ExpectExists(obj2, 'ACTI');
                      ExpectExists(obj2, 'ARMO');
                      ExpectExists(obj2, 'CELL');
                    end);
                end);
            end);

          Describe('Group serialization', procedure
            begin
              AfterAll(procedure
                begin
                  obj3 := nil;
                  obj.Free;
                  obj2.Free;
                end);

              It('Should succeed', procedure
                begin
                  ExpectSuccess(ElementToJson(cell, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  ExpectSuccess(ElementToJson(armo, @len, False));
                  obj2 := TJSONObject.Create(grs(len));
                end, True);

              It('Should put the group under a key', procedure
                begin
                  ExpectExists(obj, 'CELL');
                  ExpectExists(obj2, 'ARMO');
                  obj3 := obj.O['CELL'];
                  ary := obj2.A['ARMO'];
                end, True);

              It('Should include the records in the group', procedure
                begin
                  ExpectEqual(ary.Count, 1, 'There should be 1 record');
                end);

              It('Should serialize blocks', procedure
                begin
                  ExpectExists(obj3, 'Block 0');
                  obj3 := obj3.O['Block 0'];
                end, True);

              It('Should serialize sub-blocks', procedure
                begin
                  ExpectExists(obj3, 'Sub-Block 0');
                  ary := obj3.A['Sub-Block 0'];
                end, True);

              It('Should serialize sub-block records', procedure
                begin
                  ExpectEqual(ary.Count, 1, 'There should be 1 record');
                  obj3 := ary.O[0];
                end, True);

              It('Should serialize child groups', procedure
                begin
                  ExpectExists(obj3, 'Child Group');
                end);
            end);

          Describe('Record serialization', procedure
            const
              ExpectedFields: array[0..13] of string = (
                'Record Header',
                'EDID - Editor ID',
                'FULL - Name',
                'Male world model',
                'Female world model',
                'BODT - Body Template',
                'ZNAM - Sound - Put Down',
                'RNAM - Race',
                'KSIZ - Keyword Count',
                'KWDA - Keywords',
                'DESC - Description',
                'Armature',
                'DATA - Data',
                'DNAM - Armor Rating'
              );
            begin
              AfterAll(procedure
                begin
                  obj.Free;
                end);

              It('Should succeed', procedure
                begin
                  ExpectSuccess(ElementToJson(rec, @len, False));
                  obj := TJSONObject.Create(grs(len));
                end);

              It('Should have expected fields', procedure
                var
                  i: Integer;
                begin
                  for i := Low(ExpectedFields) to High(ExpectedFields) do
                    ExpectExists(obj, ExpectedFields[i]);
                end);
            end);

          Describe('Element serialization', procedure
            begin
              It('Should serialize strings', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'EDID', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    ExpectEqual(obj.S['EDID - Editor ID'], 'ArmorIronGauntlets', '');
                  finally
                    obj.Free;
                  end;
                end);

              It('Should serialize integer numbers', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'DATA\Value', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    ExpectEqual(obj.I['Value'], 25, '');
                  finally
                    obj.Free;
                  end;
                end);

              It('Should serialize real numbers', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'DATA\Weight', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    ExpectEqual(obj.D['Weight'], 7.3, '');
                  finally
                    obj.Free;
                  end;
                end);

              It('Should serialize FormIDs as integers', procedure
                begin
                  ExpectSuccess(ElementToJson(keyword, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    ExpectEqual(obj.I['Keyword'], 180599, '');
                  finally
                    obj.Free;
                  end;
                end);

              It('Should serialize byte arrays as strings', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'BODT\Unused', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    ExpectEqual(obj.S['Unused'], '64 73 00', '');
                  finally
                    obj.Free;
                  end;
                end);

              It('Should serialize flags as booleans', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'BODT\First Person Flags', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    Expect(obj.O['First Person Flags'].B['33 - Hands'], '');
                  finally
                    obj.Free;
                  end;
                end);

              It('Should serialize empty flags as an empty object', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'BODT\General Flags', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  ExpectEqual(grs(len), '{"General Flags":{}}', '');
                end);

              It('Should serialize arrays properly', procedure
                begin
                  ExpectSuccess(ElementToJson(keywords, @len, False));
                  obj := TJSONObject.Create(grs(len));
                  try
                    ExpectEqual(obj.A['KWDA - Keywords'].I[0], 180599, '');
                  finally
                    obj.Free;
                  end;
                end);
            end);
        end);

      Describe('ElementFromJSON', procedure
        begin
          // TODO
        end);
    end);
end;

end.
