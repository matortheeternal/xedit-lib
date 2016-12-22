unit txElementHandling;

interface

uses
  SysUtils,
  txMeta, txFileHandling;

  // ELEMENT HANDLING METHODS
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; _res: PCardinalArray): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function NewElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function ElementCount(_id: Cardinal): Integer; cdecl; external 'XEditLib.dll';
  function ElementAssigned(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function Equals(_id, _id2: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsMaster(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsInjected(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsOverride(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsWinningOverride(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure TestElementHandling;

implementation

uses
  maMain;

procedure TestElementHandling;
var
  success: WordBool;
  h, skyrim, armo, rec: Cardinal;
  a: CardinalArray;
begin
  Describe('Element Handling', procedure
    begin
      BeforeAll(procedure
        begin
          GetElement(0, 'Skyrim.esm', @skyrim);
          GetElement(skyrim, 'ARMO', @armo);
          GetElement(armo, '00012E46', @rec);
        end);

      Describe('GetElement', procedure
        begin
          BeforeEach(procedure
            begin
              h := 0;
            end);

          Describe('File resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  GetElement(0, '[0]', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  success := GetElement(0, '[-1]', @h);
                  Expect(not success, 'Result should be false');
                end);
            end);

          Describe('File resolution by name', procedure
            begin
              It('Should return a handle if a matching file is loaded', procedure
                begin
                  GetElement(0, 'Skyrim.esm', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if a matching file is not loaded', procedure
                begin
                  success := GetElement(0, 'NonExistingPlugin.esp', @h);
                  Expect(not success, 'Result should be false');
                end);
            end);

          Describe('File element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  GetElement(skyrim, '[0]', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
               begin
                 success := GetElement(skyrim, '[-1]', @h);
                 Expect(not success, 'Result should be false');
               end);
            end);

          Describe('File group resolution by signature', procedure
            begin
              It('Should return a handle if the group exists', procedure
                begin
                  GetElement(skyrim, 'ARMO', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the group does not exist', procedure
                begin
                  success := GetElement(skyrim, 'ABCD', @h);
                  Expect(not success, 'Result should be false');
                end);
            end);

          Describe('Group element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  GetElement(armo, '[0]', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
               begin
                 success := GetElement(armo, '[-1]', @h);
                 Expect(not success, 'Result should be false');
               end);
            end);

          Describe('Group record resolution by FormID', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  GetElement(armo, '00012E46', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the record does not exist', procedure
               begin
                 success := GetElement(armo, '00000000', @h);
                 Expect(not success, 'Result should be false');
               end);
            end);

          Describe('Record element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  GetElement(rec, '[0]', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
               begin
                 success := GetElement(rec, '[-1]', @h);
                 Expect(not success, 'Result should be false');
               end);
            end);

          Describe('Record element resolution by signature', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  GetElement(rec, 'FULL', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the element does not exist', procedure
               begin
                 success := GetElement(rec, 'ABCD', @h);
                 Expect(not success, 'Result should be false');
               end);
            end);

          Describe('Record element resolution by name', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  GetElement(rec, 'Male world model', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the element does not exist', procedure
               begin
                 success := GetElement(rec, 'Does not exist', @h);
                 Expect(not success, 'Result should be false');
               end);
            end);

          Describe('Record element resolution by path', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  GetElement(rec, 'BODT - Body Template', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);
            end);

          Describe('Nested resolution', procedure
            begin
              It('Should resolve nested indexes correctly if the indexes are all in bounds', procedure
                begin
                  GetElement(0, '[0]\[1]\[2]\[1]', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if any index is out of bounds', procedure
                begin
                  success := GetElement(0, '[0]\[1]\[9999999]\[1]', @h);
                  Expect(not success, 'Result should be false');
                end);

              It('Should resolve paths correctly if valid', procedure
                begin
                  GetElement(0, 'Skyrim.esm\ARMO\00012E46\KWDA\[0]', @h);
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if any subpath is invalid', procedure
                begin
                  success := GetElement(0, 'Skyrim.esm\ARMO\00012E46\ABCD', @h);
                  Expect(not success, 'Result should be false');
                end);
            end);

        end);

       Describe('GetElements', procedure
          begin
            Describe('Root children (files)', procedure
              begin
                It('Should resolve all files loaded', procedure
                  begin
                    GetElements(0, @a);
                    Expect(Length(a) = 8, 'There should be 8 handles');
                  end);
              end);

            Describe('File children (file header and groups)', procedure
              begin
                It('Should resolve the file header and all groups', procedure
                  begin
                    GetElements(skyrim, @a);
                    Expect(Length(a) = 118, 'There should be 118 handles');
                  end);
              end);

            Describe('Group children (records)', procedure
              begin
                It('Should resolve all records', procedure
                  begin
                    GetElements(armo, @a);
                    Expect(Length(a) = 2762, 'There should be 2762 handles');
                  end);
              end);

            Describe('Record children (subrecords/elements)', procedure
              begin
                It('Should resolve all children elements', procedure
                  begin
                    GetElements(rec, @a);
                    Expect(Length(a) = 13, 'There should be 13 handles');
                  end);
              end);

            Describe('Element children', procedure
              begin
                It('Should resolve all array elements', procedure
                  begin
                    GetElement(rec, 'KWDA', @h);
                    GetElements(h, @a);
                    Expect(Length(a) = 5, 'There should be 5 handles');
                  end);
              end);
          end);
    end);
end;

end.
