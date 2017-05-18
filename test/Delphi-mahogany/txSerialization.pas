unit txSerialization;

interface

  // SERIALIZATION METHODS
  function ElementToJson(_id: Cardinal; json: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildSerializationTests;

implementation

uses
  superobject,
  maMain,
  txMeta, txElements;

procedure ExpectExists(obj: ISuperObject; key: string);
begin
  Expect(obj.AsObject.Exists(key), key + ' should exist');
end;

procedure BuildSerializationTests;
var
  testFile, armo, rec, cell, keywords, keyword, dnam: Cardinal;
  json: PWideChar;
  obj, obj2, obj3: ISuperObject;
  ary: TSuperArray;
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
          BeforeEach(procedure
            begin
              GetMem(json, 16384);
            end);

          AfterEach(procedure
            begin
              FreeMem(json, 16384);
            end);

          Describe('Root serialization', procedure
            begin
              It('Should fail', procedure
                begin
                  ExpectFailure(ElementToJson(0, json, 16384));
                end);
            end);

          Describe('File serialization', procedure
            begin
              It('Should succeed', procedure
                begin
                  ExpectSuccess(ElementToJson(testFile, json, 16384));
                  obj := SO(json);
                end, true);

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
                      ExpectEqual(ary.Length, 2, 'There should be 2 master files');
                      ExpectEqual(ary.O[0].O['Master File'].S['MAST - Filename'], 'Skyrim.esm', '');
                      ExpectEqual(ary.O[1].O['Master File'].S['MAST - Filename'], 'Update.esm', '');
                    end);
                end);

              Describe('Groups', procedure
                begin
                  It('Should be present', procedure
                    begin
                      ExpectExists(obj, 'Groups');
                      obj2 := obj.O['Groups'];
                    end, true);

                  It('Should have the correct groups', procedure
                    begin
                      ExpectEqual(obj2.AsObject.GetNames.AsArray.Length, 3, 'There should be 3 groups');
                      ExpectExists(obj2, 'ACTI');
                      ExpectExists(obj2, 'ARMO');
                      ExpectExists(obj2, 'CELL');
                    end);
                end);
            end);

          Describe('Group serialization', procedure
            begin
              It('Should succeed', procedure
                begin
                  ExpectSuccess(ElementToJson(cell, json, 16384));
                  obj := SO(json);
                  ExpectSuccess(ElementToJson(armo, json, 16384));
                  obj2 := SO(json);
                end, true);

              It('Should put the group under a key', procedure
                begin
                  ExpectExists(obj, 'CELL');
                  ExpectExists(obj2, 'ARMO');
                  obj := obj.O['CELL'];
                  ary := obj2.A['ARMO'];
                end, true);

              It('Should include the records in the group', procedure
                begin
                  ExpectEqual(ary.Length, 1, 'There should be 1 record');
                end);

              It('Should serialize blocks', procedure
                begin
                  ExpectExists(obj, 'Block 0');
                  obj := obj.O['Block 0'];
                end, true);

              It('Should serialize sub-blocks', procedure
                begin
                  ExpectExists(obj, 'Sub-Block 0');
                  ary := obj.A['Sub-Block 0'];
                end, true);

              It('Should serialize sub-block records', procedure
                begin
                  ExpectEqual(ary.Length, 1, 'There should be 1 record');
                  obj := ary.O[0];
                end, true);

              It('Should serialize child groups', procedure
                begin
                  ExpectExists(obj, 'Child Group');
                  obj := ary.O[0];
                end, true);
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
              It('Should succeed', procedure
                begin
                  ExpectSuccess(ElementToJson(rec, json, 16384));
                  obj := SO(json);
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
              // TODO
            end);
        end);

      Describe('ElementFromJSON', procedure
        begin
          // TODO
        end);
    end);
end;

end.
