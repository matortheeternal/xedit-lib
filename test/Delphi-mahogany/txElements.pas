unit txElements;

interface

uses
  SysUtils,
  txMeta, txFiles;

  // ELEMENT HANDLING METHODS
  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetElements(_id: Cardinal; _res: PCardinal; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function NewElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function ElementEquals(_id, _id2: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsMaster(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsInjected(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsOverride(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function IsWinningOverride(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildElementHandlingTests;

implementation

uses
  classes,
  maMain;

procedure BuildElementHandlingTests;
var
  b: WordBool;
  h, skyrim, testFile, armo, rec, keywords, keyword, dnam, element, testArmo,
  testRec, testRec2: Cardinal;
  a: PCardinal;
  i: Integer;
  lst: TList;
begin
  Describe('Element Handling', procedure
    begin
      BeforeAll(procedure
        begin
          GetElement(0, 'Skyrim.esm', @skyrim);
          GetElement(skyrim, 'ARMO', @armo);
          GetElement(armo, '00012E46', @rec);
          GetElement(rec, 'KWDA', @keywords);
          GetElement(keywords, '[0]', @keyword);
          GetElement(rec, 'DNAM', @dnam);
          GetElement(0, 'xtest-3.esp', @testFile);
          GetElement(testFile, 'ARMO', @testArmo);
          GetElement(testArmo, '00012E46', @testRec);
          GetElement(0, 'xtest-2.esp\00012E46', @testRec2);
        end);

      Describe('GetElement', procedure
        begin
          Describe('File resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  ExpectSuccess(GetElement(0, '[0]', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(0, '[-1]', @h));
                end);
            end);

          Describe('File resolution by name', procedure
            begin
              It('Should return a handle if a matching file is loaded', procedure
                begin
                  ExpectSuccess(GetElement(0, 'Skyrim.esm', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if a matching file is not loaded', procedure
                begin
                  ExpectFailure(GetElement(0, 'NonExistingPlugin.esp', @h));
                end);
            end);

          Describe('File element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  ExpectSuccess(GetElement(skyrim, '[0]', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(skyrim, '[-1]', @h));
                end);
            end);

          Describe('File group resolution by signature', procedure
            begin
              It('Should return a handle if the group exists', procedure
                begin
                  ExpectSuccess(GetElement(skyrim, 'ARMO', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the group does not exist', procedure
                begin
                  ExpectFailure(GetElement(skyrim, 'ABCD', @h));
                end);
            end);

          Describe('Group element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  ExpectSuccess(GetElement(armo, '[0]', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(armo, '[-1]', @h));
                end);
            end);

          Describe('Group record resolution by FormID', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  ExpectSuccess(GetElement(armo, '00012E46', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the record does not exist', procedure
                begin
                  ExpectFailure(GetElement(armo, '00000000', @h));
                end);
            end);

          Describe('Record element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                 begin
                   ExpectSuccess(GetElement(rec, '[0]', @h));
                   Expect(h > 0, 'Handle should be greater than 0');
                 end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(rec, '[-1]', @h));
                end);
            end);

          Describe('Record element resolution by signature', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'FULL', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the element does not exist', procedure
                begin
                  ExpectFailure(GetElement(rec, 'ABCD', @h));
                end);
            end);

          Describe('Record element resolution by name', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'Male world model', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the element does not exist', procedure
                begin
                  ExpectFailure(GetElement(rec, 'Does not exist', @h));
                end);
            end);

          Describe('Record element resolution by path', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  ExpectSuccess(GetElement(rec, 'BODT - Body Template', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);
            end);

          Describe('Nested resolution', procedure
            begin
              It('Should resolve nested indexes correctly if the indexes are all in bounds', procedure
                begin
                  ExpectSuccess(GetElement(0, '[0]\[1]\[2]\[1]', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if any index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(0, '[0]\[1]\[9999999]\[1]', @h));
                end);

              It('Should resolve paths correctly if valid', procedure
                begin
                  ExpectSuccess(GetElement(0, 'Skyrim.esm\ARMO\00012E46\KWDA\[0]', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if any subpath is invalid', procedure
                begin
                  ExpectFailure(GetElement(0, 'Skyrim.esm\ARMO\00012E46\ABCD', @h));
                end);
            end);
        end);

      Describe('ElementExists', procedure
        begin
          It('Should return true for files that exist', procedure
            begin
              Expect(ElementExists(0, 'Skyrim.esm'), 'Result should be true');
            end);

          It('Should return true for elements that exist', procedure
            begin
              Expect(ElementExists(rec, 'Male world model'), 'Result should be true');
            end);

          It('Should return true for handles that are assigned', procedure
            begin
              Expect(ElementExists(rec, ''), 'Result should be true');
            end);

          It('Should return false for files that do not exist', procedure
            begin
              Expect(not ElementExists(0, 'NonExistingFile.esp'), 'Result should be false');
            end);

          It('Should return false for elements that do not exist', procedure
            begin
              Expect(not ElementExists(rec, 'KWDA\[5]'), 'Result should be false');
            end);

          It('Should return false for handles that are not assigned', procedure
            begin
              Expect(not ElementExists($FFFFFF, ''), 'Result should be false');
            end);
        end);

      Describe('ElementCount', procedure
        begin
          It('Should return number of files if null handle is passed', procedure
            begin
              ExpectSuccess(ElementCount(0, @i));
              ExpectEqual(i, 8, '');
            end);

          It('Should return number of elements in a file', procedure
            begin
              ExpectSuccess(ElementCount(skyrim, @i));
              ExpectEqual(i, 118, '');
            end);

          It('Should return the number of elements in a group', procedure
            begin
              ExpectSuccess(ElementCount(armo, @i));
              ExpectEqual(i, 2762, '');
            end);

          It('Should return the number of elements in a record', procedure
            begin
              ExpectSuccess(ElementCount(rec, @i));
              ExpectEqual(i, 13, '');
            end);

          It('Should return the number of elements in a subrecord', procedure
            begin
              ExpectSuccess(ElementCount(keywords, @i));
              ExpectEqual(i, 5, '');
            end);

          It('Should return 0 if there are no children', procedure
            begin
              ExpectSuccess(ElementCount(dnam, @i));
              ExpectEqual(i, 0, '');
            end);
        end);

      Describe('ElementEquals', procedure
        begin
          It('Should return true for same element', procedure
            begin
              ExpectSuccess(ElementEquals(skyrim, skyrim));
              ExpectSuccess(ElementEquals(armo, armo));
              ExpectSuccess(ElementEquals(rec, rec));
              ExpectSuccess(ElementEquals(keywords, keywords));
              ExpectSuccess(ElementEquals(dnam, dnam));
            end);

          It('Should return false for identical but different elements', procedure
            begin
              ExpectSuccess(GetElement(testRec, 'DNAM', @h));
              ExpectFailure(ElementEquals(dnam, h));
            end);

          It('Should return false for different elements', procedure
            begin
              ExpectFailure(ElementEquals(skyrim, armo));
              ExpectFailure(ElementEquals(armo, rec));
              ExpectFailure(ElementEquals(rec, keywords));
              ExpectFailure(ElementEquals(keywords, dnam));
            end);

          It('Should return false if null handle passed', procedure
            begin
              ExpectFailure(ElementEquals(0, 0));
            end);
        end);

      Describe('GetElements', procedure
        begin
          BeforeEach(procedure
            begin
              GetMem(a, 4096 * 4);
              FillChar(a^, 4096 * 4, 0);
              lst := TList.Create;
            end);

          AfterEach(procedure
            begin
              FreeMem(a, 4096 * 4);
              lst.Free;
            end);

          It('Should resolve root children (files)', procedure
            begin
              ExpectSuccess(GetElements(0, a, 4096));
              GetCardinalArray(a, 4096, lst);
              //WriteCardinalArray(lst);
              Expect(lst.Count = 8, 'There should be 8 handles');
            end);

          It('Should resolve file children (file header and groups)', procedure
            begin
              ExpectSuccess(GetElements(skyrim, a, 4096));
              GetCardinalArray(a, 4096, lst);
              //WriteCardinalArray(lst);
              Expect(lst.Count = 118, 'There should be 118 handles');
            end);

          It('Should resolve group children (records)', procedure
            begin
              ExpectSuccess(GetElements(armo, a, 4096));
              GetCardinalArray(a, 4096, lst);
              //WriteCardinalArray(lst);
              Expect(lst.Count = 2762, 'There should be 2762 handles');
            end);

          It('Should resolve record children (subrecords/elements)', procedure
            begin
              ExpectSuccess(GetElements(rec, a, 4096));
              GetCardinalArray(a, 4096, lst);
              //WriteCardinalArray(lst);
              Expect(lst.Count = 13, 'There should be 13 handles');
            end);

          It('Should resolve element children', procedure
            begin
              ExpectSuccess(GetElements(keywords, a, 4096));
              GetCardinalArray(a, 4096, lst);
              //WriteCardinalArray(lst);
              Expect(lst.Count = 5, 'There should be 5 handles');
            end);
        end);

      Describe('GetElementFile', procedure
        begin
          It('Should return the input if the input is a file', procedure
            begin
              ExpectSuccess(GetElementFile(skyrim, @h));
              Expect(h > 0, 'Handle should be greater than 0')
            end);

          It('Should return the file containing a group', procedure
            begin
              ExpectSuccess(GetElementFile(armo, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the file containing a record', procedure
            begin
              ExpectSuccess(GetElementFile(rec, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the file containing an element', procedure
            begin
              GetElement(rec, 'DATA\Value', @element);
              ExpectSuccess(GetElementFile(element, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);
        end);

      Describe('GetContainer', procedure
        begin
          It('Should return the file containing a group', procedure
            begin
              ExpectSuccess(GetContainer(armo, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the group containing a record', procedure
            begin
              ExpectSuccess(GetContainer(rec, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the record containing an element', procedure
            begin
              GetElement(rec, 'EDID', @element);
              ExpectSuccess(GetContainer(element, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the parent element containing a child element', procedure
            begin
              GetElement(rec, 'BODT\Armor Type', @element);
              ExpectSuccess(GetContainer(element, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should fail if called on a file', procedure
            begin
              ExpectFailure(GetContainer(skyrim, @h));
            end);
        end);

      Describe('GetLinksTo', procedure
        begin
          It('Should return the referenced record', procedure
            begin
              ExpectSuccess(GetLinksTo(keyword, '', @h));
              Expect(h > 0, 'Handle should be greater than 0');
              ExpectSuccess(GetLinksTo(rec, 'RNAM', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should fail if called on a NULL reference', procedure
            begin
              ExpectFailure(GetLinksTo(testArmo, 'ZNAM', @h));
            end);

          It('Should fail if path is invalid', procedure
            begin
              ExpectFailure(GetLinksTo(keywords, '[7]', @h));
            end);

          It('Should fail if called on an element that does not store a reference', procedure
            begin
              ExpectFailure(GetLinksTo(0, '', @h));
              ExpectFailure(GetLinksTo(skyrim, '', @h));
              ExpectFailure(GetLinksTo(rec, '', @h));
              ExpectFailure(GetLinksTo(dnam, '', @h));
            end);
        end);

      Describe('NewElement', procedure
        begin
          It('Should create a new file if no handle given', procedure
            begin
              ExpectSuccess(NewElement(0, 'NewFile-1.esp', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to add groups to files', procedure
            begin
              ExpectSuccess(NewElement(testFile, 'ARMO', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to add records to groups', procedure
            begin
              ExpectSuccess(NewElement(testArmo, 'ARMO', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to create a new element on a record', procedure
            begin
              ExpectSuccess(NewElement(testRec, 'Destructable', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to push a new element onto an array', procedure
            begin
              ExpectSuccess(NewElement(keywords, '', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to assign an element at an index in an array', procedure
            begin
              ExpectSuccess(NewElement(keywords, '[1]', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should fail if parent element is not a container', procedure
            begin
              GetElement(testRec, 'FULL', @element);
              ExpectFailure(NewElement(element, '', @h));
            end);
        end);

      Describe('RemoveElement', procedure
        begin
          It('Should remove the element at the given path', procedure
            begin
              ExpectSuccess(RemoveElement(testRec, 'Female world model'));
              b := ElementExists(testRec, 'Female world model');
              Expect(not b, 'The element should no longer be present');
            end);

          It('Should remove the element at the given indexed path', procedure
            begin
              ExpectSuccess(RemoveElement(testRec2, 'KWDA\[4]'));
              b := ElementExists(testRec2, 'KWDA\[4]');
              Expect(not b, 'The element should no longer be present');
            end);

          It('Should remove the element passed if no path is given', procedure
            begin
              ExpectSuccess(GetElement(testRec, 'ZNAM', @element));
              ExpectSuccess(RemoveElement(element, ''));
              b := ElementExists(testRec, 'ZNAM');
              Expect(not b, 'The element should no longer be present');
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(RemoveElement(0, ''));
            end);

          It('Should fail if no element exists at the given path', procedure
            begin
              ExpectFailure(RemoveElement(testRec, 'YNAM'));
            end);
        end);
    end);
end;

end.
