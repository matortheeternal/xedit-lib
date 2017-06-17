unit txSerialization;

interface

  // PUBLIC TESTING INTERFACE
  procedure BuildSerializationTests;

implementation

uses
  SysUtils, Math,
  Argo, Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeSerialization, xeFiles, xeElements, xeElementValues, xeRecords,
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
  testFile, armo, rec, cell, refr, keywords, keyword, dnam, h: Cardinal;
  obj, obj2, obj3: TJSONObject;
  len, i, count: Integer;
  d: Double;
  ary: TJSONArray;
  b: WordBool;
begin
  Describe('Serialization', procedure
    begin
      BeforeAll(procedure
        begin
          ExpectSuccess(GetElement(0, 'xtest-2.esp', @testFile));
          ExpectSuccess(GetElement(testFile, 'ARMO', @armo));
          ExpectSuccess(GetElement(armo, '00012E46', @rec));
          ExpectSuccess(GetElement(testFile, 'CELL', @cell));
          ExpectSuccess(GetElement(testFile, '000170F0', @refr));
          ExpectSuccess(GetElement(rec, 'KWDA', @keywords));
          ExpectSuccess(GetElement(keywords, '[0]', @keyword));
          ExpectSuccess(GetElement(rec, 'DNAM', @dnam));
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
                      ExpectEqual(obj3.I['Number of Records'], 13, '');
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
                      ExpectEqual(obj2.Count, 4, 'There should be 4 groups');
                      ExpectExists(obj2, 'ACTI');
                      ExpectExists(obj2, 'ARMO');
                      ExpectExists(obj2, 'CELL');
                      ExpectExists(obj2, 'LVLI');
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
                  ExpectSuccess(GetElement(0, 'Update.esm\000223E7\FNAM', @h));
                  ExpectSuccess(ElementToJson(h, @len, False));
                  ExpectEqual(grs(len), '{"FNAM - Flags":{}}', '');
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
          BeforeAll(procedure
            begin
              ExpectSuccess(ElementCount(0, @count));
            end);

          Describe('Root deserialization', procedure
            begin
              It('Should create missing files', procedure
                begin
                  ExpectSuccess(ElementFromJson(0, '', '{"xtest-6.esp":{"File Header":{"CNAM":"Bob Ross"}}}'));
                  ExpectSuccess(ElementCount(0, @i));
                  ExpectEqual(i, count + 1);
                  ExpectSuccess(GetValue(0, 'xtest-6.esp\File Header\CNAM', @len));
                  ExpectEqual(grs(len), 'Bob Ross');
                end);

              It('Should use existing file if present', procedure
                begin
                  ExpectSuccess(ElementFromJson(0, '', '{"xtest-2.esp":{"File Header":{}}}'));
                  ExpectSuccess(ElementCount(0, @i));
                  ExpectEqual(i, count + 1);
                end);
            end);

          Describe('File deserialization', procedure
            begin
              It('Should deserialize file header values', procedure
                begin
                  ExpectSuccess(ElementFromJson(testFile, '', '{"File Header":{"CNAM":"George","SNAM":"La dee da"}}'));
                  ExpectSuccess(GetValue(testFile, 'File Header\CNAM', @len));
                  ExpectEqual(grs(len), 'George');
                  ExpectSuccess(GetValue(testFile, 'File Header\SNAM', @len));
                  ExpectEqual(grs(len), 'La dee da');
                end);
            end);

          Describe('Group deserialization', procedure
            begin
              It('Should create top level group if missing', procedure
                begin
                  ExpectSuccess(ElementFromJson(testFile, '', '{"Groups":{"ARMA":[]}}'));
                  ExpectSuccess(HasElement(testFile, 'ARMA', @b));
                  ExpectEqual(b, True);
                end);

              It('Should use existing top level group if present', procedure
                begin
                  ExpectSuccess(ElementFromJson(testFile, '', '{"Groups":{"ARMA":[{"EDID":"TestARMA"}]}}'));
                  ExpectSuccess(HasElement(testFile, 'ARMA\[0]', @b));
                  ExpectEqual(b, True);
                end);

              It('Should create inner groups if missing', procedure
                begin
                  ExpectSuccess(ElementFromJson(testFile, '',
                    '{"Groups":{"CELL":{"Block 1":{"Sub-Block 1":['+
                      '{"EDID":"NewCell01","XLCN":"000E9DA7","Child Group":{'+
                        '"Temporary":['+
                          '{"Record Header":{"Signature":"REFR"},"EDID":"NullRef001","DATA":{"Position":{"X":1234.56}}}'+
                        ']'+
                      '}}'+
                    ']}}}}'));
                  ExpectSuccess(GetFloatValue(testFile, '03000801\Child Group\Temporary\NullRef001\DATA\Position\X', @d));
                  ExpectEqual(RoundTo(d, -2), 1234.56);
                end);

              It('Should use existing inner groups if present', procedure
                begin
                  ExpectSuccess(ElementFromJson(testFile, '',
                    '{"Groups":{"CELL":{"Block 0":{"Sub-Block 0":['+
                      '{"EDID":"KilkreathRuins03","Child Group":{'+
                        '"Persistent":['+
                          '{"EDID":"DA09PedestalEmptyRef","DATA":{"Position":{"X":1234.56}}}'+
                        ']'+
                      '}}'+
                    ']}}}}'));
                  ExpectSuccess(GetFloatValue(refr, 'DATA\Position\X', @d));
                  ExpectEqual(RoundTo(d, -2), 1234.56);
                end);
            end);

          Describe('Record deserialization', procedure
            begin
              It('Should create new record when necessary', procedure
                begin
                  ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"EDID":"NewArmor","FULL":"New Armor"}]}'));
                  ExpectSuccess(ElementCount(armo, @i));
                  ExpectEqual(i, 2);
                  ExpectSuccess(GetElement(armo, '[1]', @h));
                  ExpectSuccess(GetValue(h, 'EDID', @len));
                  ExpectEqual(grs(len), 'NewArmor');
                  ExpectSuccess(GetValue(h, 'FULL', @len));
                  ExpectEqual(grs(len), 'New Armor');
                end);

              Describe('Existing records', procedure
                begin
                  It('Should recognize existing records by FormID', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"Record Header":{"FormID":"03000803"},"FULL":"New Armor2"}]}'));
                      ExpectSuccess(ElementCount(armo, @i));
                      ExpectEqual(i, 2);
                      ExpectSuccess(GetElement(armo, '[1]', @h));
                      ExpectSuccess(GetValue(h, 'FULL', @len));
                      ExpectEqual(grs(len), 'New Armor2');
                    end);

                  It('Should recognize existing records by Editor ID', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"EDID":"NewArmor","FULL":"New Armor3"}]}'));
                      ExpectSuccess(ElementCount(armo, @i));
                      ExpectEqual(i, 2);
                      ExpectSuccess(GetElement(armo, '[1]', @h));
                      ExpectSuccess(GetValue(h, 'FULL', @len));
                      ExpectEqual(grs(len), 'New Armor3');
                    end);

                  It('Should recognize existing records by FULL Name', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"FULL":"New Armor3","DATA":{"Value":"999"}}]}'));
                      ExpectSuccess(ElementCount(armo, @i));
                      ExpectEqual(i, 2);
                      ExpectSuccess(GetElement(armo, '[1]', @h));
                      ExpectSuccess(GetValue(h, 'DATA\Value', @len));
                      ExpectEqual(grs(len), '999');
                    end);

                  It('Should fail if signatures don''t match', procedure
                    begin
                      ExpectFailure(ElementFromJson(armo, '', '{"Records":[{"Record Header":{"FormID":"03000803","Signature":"ALCH"}}]}'));
                    end);
                end);

              Describe('Overriding records', procedure
                begin
                  BeforeAll(procedure
                    begin
                      ExpectSuccess(FileByName('Skyrim.esm', @h));
                      ExpectSuccess(SortNames(h, 'ARMO'));
                      ExpectSuccess(SortEditorIDs(h, 'ARMO'));
                    end);

                  It('Should override existing records by FormID', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"Record Header":{"FormID":"00012E49"},"FULL":"Iron Armor2"}]}'));
                      ExpectSuccess(ElementCount(armo, @i));
                      ExpectEqual(i, 3);
                      ExpectSuccess(GetElement(armo, '[1]', @h));
                      ExpectSuccess(GetValue(h, 'FULL', @len));
                      ExpectEqual(grs(len), 'Iron Armor2');
                    end);

                  It('Should not override existing records by Editor ID', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"EDID":"ArmorIronBoots"}]}'));
                      ExpectSuccess(ElementCount(armo, @i));
                      ExpectEqual(i, 4);
                      ExpectSuccess(GetElement(armo, '[3]', @h));
                      ExpectSuccess(IsMaster(h, @b));
                      ExpectEqual(b, true);
                    end);

                  It('Should not override existing records by FULL Name', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '', '{"Records":[{"FULL":"Iron Shield"}]}'));
                      ExpectSuccess(ElementCount(armo, @i));
                      ExpectEqual(i, 5);
                      ExpectSuccess(GetElement(armo, '[4]', @h));
                      ExpectSuccess(IsMaster(h, @b));
                      ExpectEqual(b, true);
                    end);
                end);

              Describe('Record header', procedure
                begin
                  It('Should fail if signature does not match', procedure
                    begin
                      ExpectFailure(ElementFromJson(rec, '', '{"Record Header":{"Signature":"ALCH"}}'));
                    end);

                  It('Should ignore data size', procedure
                    begin
                      ExpectSuccess(ElementFromJson(rec, '', '{"Record Header":{"Data Size":320}}'));
                      ExpectSuccess(GetValue(rec, 'Record Header\Data Size', @len));
                      ExpectEqual(grs(len), '271');
                    end);

                  It('Should ignore form version', procedure
                    begin
                      ExpectSuccess(ElementFromJson(rec, '', '{"Record Header":{"Form Version":43}}'));
                      ExpectSuccess(GetValue(rec, 'Record Header\Form Version', @len));
                      ExpectEqual(grs(len), '40');
                    end);

                  It('Should ignore version control info', procedure
                    begin
                      ExpectSuccess(ElementFromJson(armo, '[1]', '{"Record Header":{"Version Control Info 1":"12 34 56 78","Version Control Info 2":"01 23"}}'));
                      ExpectSuccess(GetValue(armo, '[1]\Record Header\Version Control Info 1', @len));
                      ExpectEqual(grs(len), '00 00 00 00');
                      ExpectSuccess(GetValue(armo, '[1]\Record Header\Version Control Info 2', @len));
                      ExpectEqual(grs(len), '00 00');
                    end);

                  It('Should set record flags properly', procedure
                    begin
                      ExpectSuccess(ElementFromJson(rec, '', '{"Record Header":{"Record Flags":{"Ignored":true,"Unknown 15":true}}}'));
                      ExpectSuccess(GetFlag(rec, 'Record Header\Record Flags', 'Ignored', @b));
                      ExpectEqual(b, true);
                      ExpectSuccess(GetFlag(rec, 'Record Header\Record Flags', 'Unknown 15', @b));
                      ExpectEqual(b, true);
                    end);

                  It('Should set FormID properly', procedure
                    begin
                      ExpectSuccess(GetElement(armo, '[2]', @h));
                      ExpectSuccess(ElementFromJson(h, '', '{"Record Header":{"FormID":"03123456"}}'));
                      ExpectSuccess(GetValue(armo, '[2]\Record Header\FormID', @len));
                      ExpectEqual(grs(len), 'NewArmor "New Armor3" [ARMO:03123456]');
                    end);
                end);
            end);

          Describe('Element deserialization', procedure
            begin
              BeforeAll(procedure
                begin
                  ExpectSuccess(FileByName('Skyrim.esm', @h));
                  ExpectSuccess(SortEditorIDs(h, 'SNDR'));
                end);

              It('Should deserialize strings', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, '', '{"EDID":"Deserialization01"}'));
                  ExpectSuccess(GetValue(rec, 'EDID', @len));
                  ExpectEqual(grs(len), 'Deserialization01');
                  ExpectSuccess(ElementFromJson(rec, 'BODT', '{"Unused":"12 34 56"}'));
                  ExpectSuccess(GetValue(rec, 'BODT\Unused', @len));
                  ExpectEqual(grs(len), '12 34 56');
                end);

              It('Should deserialize integer numbers', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, '', '{"DNAM":9900}'));
                  ExpectSuccess(GetIntValue(rec, 'DNAM', @i));
                  ExpectEqual(i, 9900);
                end);

              It('Should deserialize real numbers', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, 'DATA', '{"Weight":5.432}'));
                  ExpectSuccess(GetFloatValue(rec, 'DATA\Weight', @d));
                  ExpectEqual(d, 5.432);
                end);

              It('Should deserialize references from integers', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, '', '{"ZNAM":282309}'));
                  ExpectSuccess(GetIntValue(rec, 'ZNAM', @i));
                  ExpectEqual(i, 282309);
                end);

              It('Should deserialize references from strings', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, '', '{"ZNAM":"DRSIronAlleyClose"}'));
                  ExpectSuccess(GetValue(rec, 'ZNAM', @len));
                  ExpectEqual(grs(len), 'DRSIronAlleyClose [SNDR:000C0303]');
                  ExpectSuccess(ElementFromJson(rec, '', '{"ZNAM":"AMBCobwebSD [SNDR:0003E5DD]"}'));
                  ExpectSuccess(GetValue(rec, 'ZNAM', @len));
                  ExpectEqual(grs(len), 'AMBCobwebSD [SNDR:0003E5DD]');
                end);

              It('Should deserialize nested elements properly', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, '', '{"DATA":{"Weight":8.12,"Value":6565}}'));
                  ExpectSuccess(GetFloatValue(rec, 'DATA\Weight', @d));
                  ExpectEqual(d, 8.12);
                  ExpectSuccess(GetIntValue(rec, 'DATA\Value', @i));
                  ExpectEqual(i, 6565);
                end);

              It('Should deserialize flags properly', procedure
                begin
                  ExpectSuccess(ElementFromJson(rec, 'Record Header', '{"Record Flags":{"Non-Playable":true}}'));
                  ExpectSuccess(GetFlag(rec, 'Record Header\Record Flags', 'Non-Playable', @b));
                  ExpectEqual(b, True);
                  ExpectSuccess(ElementFromJson(rec, 'Record Header', '{"Record Flags":{}}'));
                  ExpectSuccess(GetFlag(rec, 'Record Header\Record Flags', 'Non-Playable', @b));
                  ExpectEqual(b, False);
                end);
            end);
        end);
    end);
end;

end.
