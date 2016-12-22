unit txElementHandling;

interface

uses
  SysUtils,
  txMeta, txFileHandling;

  // ELEMENT HANDLING METHODS
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; _res: PCardinalArray): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
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
  h, skyrim, testFile, armo, rec, element: Cardinal;
  a: CardinalArray;
  str: PWideChar;
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
          Describe('File resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  success := GetElement(0, '[0]', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(0, 'Skyrim.esm', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(skyrim, '[0]', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(skyrim, 'ARMO', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(armo, '[0]', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(armo, '00012E46', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                   success := GetElement(rec, '[0]', @h);
                   Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(rec, 'FULL', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(rec, 'Male world model', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElement(rec, 'BODT - Body Template', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
                end);
            end);

          Describe('Nested resolution', procedure
            begin
              It('Should resolve nested indexes correctly if the indexes are all in bounds', procedure
                begin
                  success := GetElement(0, '[0]\[1]\[2]\[1]', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
                end);

              It('Should fail if any index is out of bounds', procedure
                begin
                  success := GetElement(0, '[0]\[1]\[9999999]\[1]', @h);
                  Expect(not success, 'Result should be false');
                end);

              It('Should resolve paths correctly if valid', procedure
                begin
                  success := GetElement(0, 'Skyrim.esm\ARMO\00012E46\KWDA\[0]', @h);
                  Expect(success and (h > 0), 'Handle should be greater than 0');
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
                  success := GetElements(0, @a);
                  Expect(success and (Length(a) = 8), 'There should be 8 handles');
                end);
            end);

          Describe('File children (file header and groups)', procedure
            begin
              It('Should resolve the file header and all groups', procedure
                begin
                  success := GetElements(skyrim, @a);
                  Expect(success and (Length(a) = 118), 'There should be 118 handles');
                end);
            end);

          Describe('Group children (records)', procedure
            begin
              It('Should resolve all records', procedure
                begin
                  success := GetElements(armo, @a);
                  Expect(success and (Length(a) = 2762), 'There should be 2762 handles');
                end);
            end);

          Describe('Record children (subrecords/elements)', procedure
            begin
              It('Should resolve all children elements', procedure
                begin
                  success := GetElements(rec, @a);
                  Expect(success and (Length(a) = 13), 'There should be 13 handles');
                end);
            end);

          Describe('Element children', procedure
            begin
              It('Should resolve all array elements', procedure
                begin
                  GetElement(rec, 'KWDA', @h);
                  success := GetElements(h, @a);
                  Expect(success and (Length(a) = 5), 'There should be 5 handles');
                end);
            end);
        end);

      Describe('GetElementFile', procedure
        begin
          It('Should return the input if the input is a file', procedure
            begin
              success := GetElementFile(skyrim, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0')
            end);

          It('Should return the file containing a group', procedure
            begin
              success := GetElementFile(armo, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return the file containing a record', procedure
            begin
              success := GetElementFile(rec, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return the file containing an element', procedure
            begin
              GetElement(rec, 'DATA\Value', @element);
              success := GetElementFile(element, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);
        end);

      Describe('GetContainer', procedure
        begin
          It('Should return the file containing a group', procedure
            begin
              success := GetContainer(armo, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return the group containing a record', procedure
            begin
              success := GetContainer(rec, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return the record containing an element', procedure
            begin
              GetElement(rec, 'EDID', @element);
              success := GetContainer(element, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should return the parent element containing a child element', procedure
            begin
              GetElement(rec, 'BODT\Armor Type', @element);
              success := GetContainer(element, @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should fail if called on a file', procedure
            begin
              success := GetContainer(skyrim, @h);
              Expect(not success, 'Result should be false')
            end);
        end);

      Describe('NewElement', procedure
        begin
          BeforeAll(procedure
            begin
              GetElement(0, 'xtest-2.esp', @testFile);
              GetElement(testFile, 'ARMO', @armo);
              GetElement(armo, '00012E46', @rec);
            end);

          It('Should create a new file if no handle given', procedure
            begin
              success := NewElement(0, 'NewFile-1.esp', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should be able to add groups to files', procedure
            begin
              success := NewElement(testFile, 'ARMO', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should be able to add records to groups', procedure
            begin
              success := NewElement(armo, 'ARMO', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);


          It('Should be able to create a new element on a record', procedure
            begin
              success := NewElement(rec, 'Destructable', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should be able to push a new element onto an array', procedure
            begin
              GetElement(rec, 'KWDA', @element);
              success := NewElement(element, '', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should be able to assign an element at an index in an array', procedure
            begin
              GetElement(rec, 'KWDA', @element);
              success := NewElement(element, '[1]', @h);
              Expect(success and (h > 0), 'Handle should be greater than 0');
            end);

          It('Should fail if parent element is not a container', procedure
            begin
              GetElement(rec, 'FULL', @element);
              success := NewElement(element, '', @h);
              Expect(not success, 'Result should be false');
            end);
        end);
    end);
end;

end.
