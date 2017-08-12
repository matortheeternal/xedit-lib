unit txElements;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildElementHandlingTests;

implementation

uses
  classes,
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles, xeMasters, xeElements, xeElementValues, xeRecords,
{$ENDIF}
  txMeta, txRecords;

procedure TestHasElement(h: Cardinal; path: PWideChar; expectedValue: WordBool = True);
var
  b: WordBool;
begin
  ExpectSuccess(HasElement(h, path, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestGetElement(h: Cardinal; path: PWideChar);
var
  element: Cardinal;
begin
  ExpectSuccess(GetElement(h, path, @element));
  Expect(element > 0, 'Handle should be greater than 0');
end;
  
procedure TestAddElement(h: Cardinal; path: PWideChar; testExistence: Boolean = True);
var
  element: Cardinal;
  b: WordBool;
begin
  ExpectSuccess(AddElement(h, path, @element));
  if testExistence then begin
    ExpectSuccess(HasElement(h, path, @b));
    Expect(b, 'The element should be present');
  end;
  Expect(element > 0, 'Handle should be greater than 0');
end;

procedure TestElementCount(h: Cardinal; expectedCount: Integer);
var
  count: Integer;
begin
  ExpectSuccess(ElementCount(h, @count));
  ExpectEqual(count, expectedCount);
end;

procedure TestElementEquals(e1, e2: Cardinal; expectedValue: WordBool = True); overload;
var
  b: WordBool;
begin
  ExpectSuccess(ElementEquals(e1, e2, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestElementEquals(e1, container: Cardinal; path: PWideChar; expectedValue: WordBool = True); overload;
var
  e2: Cardinal;
begin
  ExpectSuccess(GetElement(container, path, @e2));
  TestElementEquals(e1, e2, expectedValue);
end;

procedure TestRemoveElement(h: Cardinal; path: PWideChar; testPresence: Boolean = True);
var
  b: WordBool;
begin
  ExpectSuccess(RemoveElement(h, path));
  if testPresence then begin
    ExpectSuccess(HasElement(h, path, @b));
    Expect(not b, 'The element should not longer be present');
  end;
end;

procedure TestElementMatches(h: Cardinal; path, value: PWideChar; expectedValue: WordBool = True);
var
  b: WordBool;
begin
  ExpectSuccess(ElementMatches(h, path, value, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestHasArrayItem(h: Cardinal; path, subpath, value: PWideChar; expectedValue: WordBool = True);
var
  b: WordBool;
begin
  ExpectSuccess(HasArrayItem(h, path, subpath, value, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestGetArrayItem(h: Cardinal; path, subpath, value: PWideChar);
var
  item: Cardinal;
begin
  ExpectSuccess(GetArrayItem(h, path, subpath, value, @item));
  Expect(item > 0, 'Handle should be greater than 0');
end;

procedure TestAddArrayItem(h: Cardinal; path, subpath, value: PWideChar; expectedValue: String = '');
var
  item: Cardinal;
  len: Integer;
begin
  ExpectSuccess(AddArrayItem(h, path, subpath, value, @item));
  Expect(item > 0, 'Handle should be greater than 0');
  if expectedValue <> '' then begin
    ExpectSuccess(GetValue(item, subpath, @len));
    ExpectEqual(grs(len), expectedValue);
  end;
end;

function TestCopyElement(path: PWideChar; destination: Cardinal; asNew: WordBool): Cardinal;
var
  source: Cardinal;
begin
  ExpectSuccess(GetElement(0, path, @source));
  ExpectSuccess(CopyElement(source, destination, asNew, @Result));
end;

procedure TestElementFile(element: Cardinal; expectedFileName: String);
var
  h: Cardinal;
  len: Integer;
begin
  ExpectSuccess(GetElementFile(element, @h));
  ExpectSuccess(Name(h, @len));
  ExpectEqual(grs(len), expectedFileName);
end;

procedure RemoveElements(_file: Cardinal);
var
  len, i: Integer;
  elements: CardinalArray;
begin
  ExpectSuccess(GetElements(_file, '', 0, @len));
  elements := gra(len);
  for i := 1 to High(elements) do
    ExpectSuccess(RemoveElement(elements[i], ''));
end;

procedure TestNames(a: CardinalArray; firstName, lastName: String);
var
  len: Integer;
begin
  ExpectSuccess(Name(a[Low(a)], @len));
  ExpectEqual(grs(len), firstName);
  ExpectSuccess(Name(a[High(a)], @len));
  ExpectEqual(grs(len), lastName);
end;

procedure TestEdids(a: CardinalArray; firstEdid, lastEdid: String);
var
  len: Integer;
begin
  ExpectSuccess(GetValue(a[Low(a)], 'EDID', @len));
  ExpectEqual(grs(len), firstEdid);
  ExpectSuccess(GetValue(a[High(a)], 'EDID', @len));
  ExpectEqual(grs(len), lastEdid);
end;

procedure TestGetContainer(h: Cardinal; path: PWideChar);
var
  element, container: Cardinal;
begin
  if path <> '' then
    ExpectSuccess(GetElement(h, path, @element))
  else
    element := h;
  ExpectSuccess(GetContainer(element, @container));
  Expect(container > 0, 'Handle should be greater than 0');
end;

procedure TestGetElementFile(h: Cardinal; expectedFileName: String);
var
  _file: Cardinal;
  len: Integer;
begin
  ExpectSuccess(GetElementFile(h, @_file));
  Expect(_file > 0, 'Handle should be greater than 0');
  ExpectSuccess(Name(_file, @len));
  ExpectEqual(grs(len), expectedFileName);
end;

procedure TestGetLinksTo(h: Cardinal; path: PWideChar; expectedRecordName: String);
var
  rec: Cardinal;
  len: Integer;
begin
  ExpectSuccess(GetLinksTo(h, path, @rec));
  Expect(rec > 0, 'Handle should be greater than 0');
  ExpectSuccess(Name(rec, @len));
  ExpectEqual(grs(len), expectedRecordName);
end;

procedure TestGetSignatureAllowed(h: Cardinal; sig: PWideChar; expectedResult: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(GetSignatureAllowed(h, sig, @b));
  ExpectEqual(b, expectedResult);
end;

procedure BuildElementHandlingTests;
var
  b: WordBool;
  h, rec, skyrim, xt3, xt5, armo1, ar1, keywords, keyword, dnam, element, armo2,
  ar2, ar3, refr, lvli, entries, armature: Cardinal;
  len: Integer;
begin
  Describe('Element Handling', procedure
    begin
      BeforeAll(procedure
        begin
          ExpectSuccess(GetElement(0, 'Skyrim.esm', @skyrim));
          ExpectSuccess(GetElement(skyrim, 'ARMO', @armo1));
          ExpectSuccess(GetElement(armo1, '00012E46', @ar1));
          ExpectSuccess(GetElement(ar1, 'KWDA', @keywords));
          ExpectSuccess(GetElement(keywords, '[0]', @keyword));
          ExpectSuccess(GetElement(ar1, 'DNAM', @dnam));
          ExpectSuccess(GetElement(0, 'xtest-2.esp\00012E46', @ar2));
          ExpectSuccess(GetElement(ar2, 'Armature', @armature));
          ExpectSuccess(GetElement(0, 'xtest-3.esp', @xt3));
          ExpectSuccess(GetElement(0, 'xtest-5.esp', @xt5));
          ExpectSuccess(GetElement(xt3, 'ARMO', @armo2));
          ExpectSuccess(GetElement(armo2, '00012E46', @ar3));
          ExpectSuccess(GetElement(0, 'xtest-2.esp\000170F0', @refr));
          ExpectSuccess(GetElement(0, 'xtest-2.esp\00013739', @lvli));
          ExpectSuccess(GetElement(lvli, 'Leveled List Entries', @entries));
        end);

      Describe('HasElement', procedure
        begin
          It('Should return true for files that exist', procedure
            begin
              TestHasElement(0, 'Skyrim.esm');
            end);

          It('Should return true for elements that exist', procedure
            begin
              TestHasElement(ar1, 'Male world model');
            end);

          It('Should return true for handles that are assigned', procedure
            begin
              TestHasElement(ar1, '');
            end);

          It('Should return false for files that do not exist', procedure
            begin
              TestHasElement(0, 'NonExistingFile.esp', False);
            end);

          It('Should return false for elements that do not exist', procedure
            begin
              TestHasElement(ar1, 'KWDA\[5]', False);
            end);

          It('Should fail if the handle is not assigned', procedure
            begin
              ExpectFailure(HasElement($FFFFFF, '', @b));
            end);
        end);

      Describe('GetElement', procedure
        begin
          Describe('File resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  TestGetElement(0, '[0]');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(0, '[-9]', @h));
                end);
            end);

          Describe('File resolution by name', procedure
            begin
              It('Should return a handle if a matching file is loaded', procedure
                begin
                  TestGetElement(0, 'Skyrim.esm');
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
                  TestGetElement(skyrim, '[0]');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(skyrim, '[-9]', @h));
                end);
            end);

          Describe('File group resolution by signature', procedure
            begin
              It('Should return a handle if the group exists', procedure
                begin
                  TestGetElement(skyrim, 'ARMO');
                end);

              It('Should fail if the group does not exist', procedure
                begin
                  ExpectFailure(GetElement(skyrim, 'ABCD', @h));
                end);
            end);

          Describe('File record resolution by FormID', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  TestGetElement(skyrim, '00012E46');
                end);

              It('Should fail if the record does not exist', procedure
                begin
                  ExpectFailure(GetElement(skyrim, '00FFFFFF', @h));
                end);
            end);

          Describe('File record resolution by EditorID', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  TestGetElement(xt3, 'ArmorIronGauntlets');
                end);

              It('Should fail if the record does not exist', procedure
                begin
                  ExpectFailure(GetElement(xt3, 'NonExistentEditorID', @h));
                end);
            end);

          Describe('File record resolution by Name', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  TestGetElement(xt3, '"Iron Gauntlets"');
                end);

              It('Should fail if the record does not exist', procedure
                begin
                  ExpectFailure(GetElement(xt3, '"U. N. Owen"', @h));
                end);
            end);

          Describe('Group element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  TestGetElement(armo1, '[0]');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(armo1, '[-9]', @h));
                  ExpectFailure(GetElement(armo1, '[99999]', @h));
                end);
            end);

          Describe('Group record resolution by FormID', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  TestGetElement(armo1, '00012E46');
                end);

              It('Should fail if the record does not exist', procedure
                begin
                  ExpectFailure(GetElement(armo1, '00000000', @h));
                end);
            end);

          Describe('Record element resolution by index', procedure
            begin
              It('Should return a handle if the index is in bounds', procedure
                begin
                  TestGetElement(ar1, '[0]');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(ar1, '[-9]', @h));
                  ExpectFailure(GetElement(ar1, '[99999]', @h));
                end);
            end);

          Describe('Record element resolution by signature', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  TestGetElement(ar1, 'FULL');
                end);

              It('Should fail if the element does not exist', procedure
                begin
                  ExpectFailure(GetElement(ar1, 'ABCD', @h));
                end);
            end);

          Describe('Record element resolution by name', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  TestGetElement(ar1, 'Male world model');
                  TestGetElement(ar1, 'BODT - Body Template');
                end);

              It('Should fail if the element does not exist', procedure
                begin
                  ExpectFailure(GetElement(ar1, 'Does not exist', @h));
                end);
            end);

          Describe('Nested resolution', procedure
            begin
              It('Should resolve nested indexes correctly if the indexes are all in bounds', procedure
                begin
                  TestGetElement(0, '[0]\[1]\[2]\[1]');
                end);

              It('Should fail if any index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(0, '[0]\[1]\[9999999]\[1]', @h));
                end);

              It('Should resolve paths correctly if valid', procedure
                begin
                  TestGetElement(0, 'Skyrim.esm\ARMO\00012E46\KWDA\[0]');
                end);

              It('Should fail if any subpath is invalid', procedure
                begin
                  ExpectFailure(GetElement(0, 'Skyrim.esm\ARMO\00012E46\ABCD', @h));
                end);
            end);
        end);

      Describe('AddElement', procedure
        begin
          It('Should create a new file if no handle given', procedure
            begin
              TestAddElement(0, 'NewFile-1.esp');
            end);

          It('Should be able to add groups to files', procedure
            begin
              TestAddElement(xt3, 'ARMO');
              TestAddElement(xt3, 'CELL');
            end);

          It('Should be able to add records to groups', procedure
            begin
              TestAddElement(armo2, 'ARMO', False);
              TestElementCount(armo2, 2);
            end);

          It('Should be able to create a new element on a record', procedure
            begin
              TestAddElement(ar2, 'Destructable');
            end);

          It('Should be able to push a new element onto an array', procedure
            begin
              TestAddElement(keywords, '.', False);
              TestElementCount(keywords, 6);
              TestAddElement(armature, '.', False);
              TestElementCount(armature, 2);
            end);

          It('Should be able to insert an element at an index in an array', procedure
            begin
              TestAddElement(armature, '^0', False);
              TestElementCount(armature, 3);
            end);

          It('Should fail if interface is not a container', procedure
            begin
              ExpectSuccess(GetElement(ar2, 'FULL', @element));
              ExpectFailure(AddElement(element, '.', @h));
            end);
        end);

      Describe('RemoveElement', procedure
        begin
          It('Should not be able to remove files', procedure
            begin
              ExpectFailure(RemoveElement(0, 'NewFile-1.esp'));
            end);

          It('Should be able to remove groups from files', procedure
            begin
              TestRemoveElement(xt3, 'CELL');
            end);

          It('Should be able to remove records from groups', procedure
            begin
              TestRemoveElement(armo2, '[1]');
            end);

          It('Should be able to remove elements from records', procedure
            begin
              TestRemoveElement(ar2, 'Destructable');
            end);

          It('Should be able to remove an element from an array', procedure
            begin
              TestRemoveElement(keywords, '[0]', False);
              TestElementCount(keywords, 5);
              TestRemoveElement(armature, '[0]', False);
              TestElementCount(armature, 2);
            end);

          It('Should remove the element passed if no path is given', procedure
            begin
              // TODO
            end);

          It('Should fail if a null handle is passed', procedure
            begin
              ExpectFailure(RemoveElement(0, ''));
            end);

          It('Should fail if no element exists at the given path', procedure
            begin
              ExpectFailure(RemoveElement(ar3, 'YNAM'));
            end);
        end);
      
      Describe('GetElements', procedure
        begin
          It('Should resolve root children (files)', procedure
            begin
              ExpectSuccess(GetElements(0, '', 0, @len));
              ExpectEqual(len, 9);
              TestNames(gra(len), 'Skyrim.esm', 'NewFile-1.esp');
            end);

          It('Should resolve file children (file header and groups)', procedure
            begin
              ExpectSuccess(GetElements(skyrim, '', 0, @len));
              ExpectEqual(len, 118);
              TestNames(gra(len), 'File Header', 'Reverb Parameters');
            end);

          It('Should resolve group children (records)', procedure
            begin
              ExpectSuccess(GetElements(armo1, '', 0, @len));
              ExpectEqual(len, 2762);
              TestEdids(gra(len), 'DremoraBoots', 'SkinNaked');
            end);

          It('Should resolve record children (subrecords/elements)', procedure
            begin
              ExpectSuccess(GetElements(ar1, '', 0, @len));
              ExpectEqual(len, 13);
              TestNames(gra(len), 'Record Header', 'DNAM - Armor Rating');
            end);

          It('Should resolve element children', procedure
            begin
              ExpectSuccess(GetElements(keywords, '', 0, @len));
              ExpectEqual(len, 5);
              TestNames(gra(len), 'Keyword', 'Keyword');
            end);

          It('Should resolve paths', procedure
            begin
              ExpectSuccess(GetElements(ar2, 'KWDA', 0, @len));
              ExpectEqual(len, 5);
            end);

          It('Should sort elements based on SortBy param', procedure
            begin
              ExpectSuccess(GetElements(0, 'Skyrim.esm', 1, @len));
              TestNames(gra(len), 'File Header', 'Weather');
            end);

          It('Should not include child groups', procedure
            begin
              ExpectSuccess(GetElements(0, 'Skyrim.esm\DIAL', 1, @len));
              ExpectEqual(len, 15037);
            end);
        end);
      
      Describe('GetContainer', procedure
        begin
          It('Should return the file containing a group', procedure
            begin
              TestGetContainer(armo1, '');
            end);

          It('Should return the group containing a record', procedure
            begin
              TestGetContainer(ar1, '');
            end);

          It('Should return the record containing an element', procedure
            begin
              TestGetContainer(ar1, 'EDID');
              TestGetContainer(refr, 'Record Header');;
            end);

          It('Should return the parent element containing a child element', procedure
            begin
              TestGetContainer(ar1, 'BODT\Armor Type');
            end);

          It('Should fail if called on a file', procedure
            begin
              ExpectFailure(GetContainer(skyrim, @h));
            end);
        end);

      Describe('GetElementFile', procedure
        begin
          It('Should return the input if the input is a file', procedure
            begin
              TestGetElementFile(skyrim, 'Skyrim.esm');
            end);

          It('Should return the file containing a group', procedure
            begin
              TestGetElementFile(armo1, 'Skyrim.esm');
            end);

          It('Should return the file containing a record', procedure
            begin
              TestGetElementFile(ar1, 'Skyrim.esm');
              TestGetElementFile(ar2, 'xtest-2.esp');
              TestGetElementFile(ar3, 'xtest-3.esp');
            end);

          It('Should return the file containing an element', procedure
            begin
              TestGetElementFile(keywords, 'Skyrim.esm');
              TestGetElementFile(entries, 'xtest-2.esp');
            end);
        end);

      Describe('GetLinksTo', procedure
        begin
          It('Should return the referenced record', procedure
            begin
              TestGetLinksTo(keyword, '', 'PerkFistsIron');
              TestGetLinksTo(ar1, 'RNAM', 'Default Race');
            end);

          It('Should work with navmesh edge links', procedure
            begin
              ExpectSuccess(GetElement(skyrim, '000FF1DE', @h));
              TestGetLinksTo(h, 'NVNM\Edge Links\[0]\Mesh', '[NAVM:000FF1CB]');
            end);

          It('Should fail if called on a NULL reference', procedure
            begin
              ExpectFailure(GetLinksTo(armo2, 'ZNAM', @h));
            end);

          It('Should fail if path is invalid', procedure
            begin
              ExpectFailure(GetLinksTo(keywords, '[99]', @h));
            end);

          It('Should fail on elements that cannot store a reference', procedure
            begin
              ExpectFailure(GetLinksTo(0, '', @h));
              ExpectFailure(GetLinksTo(skyrim, '', @h));
              ExpectFailure(GetLinksTo(ar1, '', @h));
              ExpectFailure(GetLinksTo(dnam, '', @h));
            end);
        end);

      Describe('ElementCount', procedure
        begin
          It('Should return number of files if null handle is passed', procedure
            begin
              TestElementCount(0, 9);
            end);

          It('Should return number of elements in a file', procedure
            begin
              TestElementCount(skyrim, 118);
            end);

          It('Should return the number of elements in a group', procedure
            begin
              TestElementCount(armo1, 2762);
            end);

          It('Should return the number of elements in a record', procedure
            begin
              TestElementCount(ar1, 13);
            end);

          It('Should return the number of elements in a subrecord', procedure
            begin
              TestElementCount(keywords, 5);
            end);

          It('Should return 0 if there are no children', procedure
            begin
              TestElementCount(dnam, 0);
            end);
        end);

      Describe('ElementEquals', procedure
        begin
          It('Should return true for same element', procedure
            begin
              TestElementEquals(skyrim, 0, 'Skyrim.esm');
              TestElementEquals(armo1, skyrim, 'ARMO');
              TestElementEquals(ar1, armo1, '00012E46');
              TestElementEquals(keywords, ar1, 'KWDA');
              TestElementEquals(dnam, ar1, 'DNAM');
            end);

          It('Should return false for different elements holding the same value', procedure
            begin
              TestElementEquals(dnam, ar2, 'DNAM', False);
              TestElementEquals(keywords, ar2, 'KWDA', False);
            end);

          It('Should return false for different elements', procedure
            begin
              TestElementEquals(skyrim, armo1, False);
              TestElementEquals(armo1, ar1, False);
              TestElementEquals(ar1, keywords, False);
              TestElementEquals(keywords, dnam, False);
            end);

          It('Should return false if null handle passed', procedure
            begin
              ExpectFailure(ElementEquals(0, 0, @b));
            end);
        end);

      Describe('ElementMatches', procedure
        begin
          Describe('Edit Value Matching', procedure
            begin
              It('Should work on NULL references', procedure
                begin
                  TestElementMatches(ar2, 'ZNAM', 'NULL - Null Reference [00000000]', true);
                  TestElementMatches(ar2, 'ZNAM', '', false);
                end);

              It('Should work on string fields', procedure
                begin
                  TestElementMatches(ar2, 'EDID', 'ArmorIronGauntlets', true);
                  TestElementMatches(ar2, 'EDID', 'Blarg', false);
                end);

              It('Should work on integer fields', procedure
                begin
                  TestElementMatches(ar2, 'OBND\Z1', '-1', true);
                  TestElementMatches(ar2, 'OBND\Z1', '-69', false);
                end);

              It('Should work on float fields', procedure
                begin
                  TestElementMatches(ar2, 'DATA\Weight', '5.0', true);
                  TestElementMatches(ar2, 'DATA\Weight', '5.01', false);
                  TestElementMatches(ar2, 'DATA\Weight', '5', true);
                  TestElementMatches(ar2, 'DNAM', '10.0', true);
                  TestElementMatches(ar2, 'DNAM', '10', true);
                end);
            end);

          Describe('FormID matching', procedure
            begin
              It('Should return true if FormID matches', procedure
                begin
                  TestElementMatches(keywords, '[0]', '000424EF', true);
                  TestElementMatches(ar2, 'ZNAM', '00000000', true);
                  TestElementMatches(ar2, 'RNAM', '00000019', true);
                end);

              It('Should return false if formID does not match', procedure
                begin
                  TestElementMatches(keywords, '[0]', '000A82BB', false);
                  TestElementMatches(ar2, 'RNAM', '00000029', false);
                end);
            end);

          Describe('Editor ID matching', procedure
            begin
              It('Should return true if Editor ID matches', procedure
                begin
                  TestElementMatches(keywords, '[0]', 'PerkFistsIron');
                  TestElementMatches(keywords, '[3]', 'ArmorGauntlets');
                  TestElementMatches(ar2, 'RNAM', 'DefaultRace');
                end);

              It('Should return false if Editor ID does not match', procedure
                begin
                  TestElementMatches(keywords, '[0]', '"Vampire"', false);
                  TestElementMatches(keywords, '[1]', '"ArMorHeAvY"', false);
                end);
            end);

          Describe('FULL name matching', procedure
            begin
              It('Should return true if FULL name matches', procedure
                begin
                  TestElementMatches(ar2, 'RNAM', '"Default Race"');
                  TestElementMatches(keywords, '[0]', '""');
                end);

              It('Should return false if FULL name does not match', procedure
                begin
                  TestElementMatches(ar2, 'RNAM', '"Default RacE"', false);
                  TestElementMatches(ar2, 'ZNAM', '"Null Reference"', false);
                end);
            end);
        end);

      Describe('HasArrayItem', procedure
        begin
          Describe('Value arrays', procedure
            begin
              It('Should return true if array item is present', procedure
                begin
                  TestHasArrayItem(ar2, 'KWDA', '', 'PerkFistsIron');
                  TestHasArrayItem(keywords, '', '', 'ArmorGauntlets');
                  TestHasArrayItem(keywords, '', '', '0006BBE3');
                  TestHasArrayItem(ar2, 'Armature', '', 'IronGlovesAA');
                end);

              It('Should return false if array item is not present', procedure
                begin
                  TestHasArrayItem(keywords, '', '', 'PerkFistsSteel', false);
                  TestHasArrayItem(keywords, '', '', 'ArmorHelmet', false);
                  TestHasArrayItem(keywords, '', '', '0006BBD4', false);
                  TestHasArrayItem(ar2, 'Armature', '', 'IronHelmetAA', false);
                end);
            end);

          Describe('Struct arrays', procedure
            begin
              It('Should return true if array item is present', procedure
                begin
                  TestHasArrayItem(entries, '', 'LVLO\Reference', 'ArmorIronGauntlets');
                  TestHasArrayItem(entries, '', 'LVLO\Reference', '"Iron Armor"');
                  TestHasArrayItem(entries, '', 'LVLO\Reference', '00012E4B');
                  TestHasArrayItem(entries, '', 'LVLO\Reference', '"Iron Helmet"');
                end);

              It('Should return false if array item is not present', procedure
                begin
                  TestHasArrayItem(entries, '', 'LVLO\Reference', 'ArmorSteelHelmetA', false);
                  TestHasArrayItem(entries, '', 'LVLO\Reference', '"Steel Helmet"', false);
                  TestHasArrayItem(entries, '', 'LVLO\Reference', '00131954', false);
                end);
            end);
        end);

      Describe('GetArrayItem', procedure
        begin
          Describe('Value arrays', procedure
            begin
              It('Should succeed if array item is present', procedure
                begin
                  TestGetArrayItem(ar2, 'KWDA', '', 'PerkFistsIron');
                  TestGetArrayItem(keywords, '', '', 'ArmorGauntlets');
                  TestGetArrayItem(keywords, '', '', '0006BBE3');
                  TestGetArrayItem(ar2, 'Armature', '', 'IronGlovesAA');
                end);

              It('Should fail if array item is not present', procedure
                begin
                  ExpectFailure(GetArrayItem(keywords, '', '', 'PerkFistsSteel', @h));
                  ExpectFailure(GetArrayItem(keywords, '', '', 'ArmorHelmet', @h));
                  ExpectFailure(GetArrayItem(keywords, '', '', '0006BBD4', @h));
                  ExpectFailure(GetArrayItem(ar2, 'Armature', '', 'IronHelmetAA', @h));
                end);
            end);

          Describe('Struct arrays', procedure
            begin
              It('Should succeed if array item is present', procedure
                begin
                  TestGetArrayItem(entries, '', 'LVLO\Reference', 'ArmorIronGauntlets');
                  TestGetArrayItem(entries, '', 'LVLO\Reference', '"Iron Armor"');
                  TestGetArrayItem(entries, '', 'LVLO\Reference', '00012E4B');
                  TestGetArrayItem(entries, '', 'LVLO\Reference', '"Iron Helmet"');
                end);

              It('Should fail if array item is not present', procedure
                begin
                  ExpectFailure(GetArrayItem(entries, '', 'LVLO\Reference', 'ArmorSteelHelmetA', @h));
                  ExpectFailure(GetArrayItem(entries, '', 'LVLO\Reference', '"Steel Helmet"', @h));
                  ExpectFailure(GetArrayItem(entries, '', 'LVLO\Reference', '00131954', @h));
                end);
            end);
        end);

      Describe('AddArrayItem', procedure
        begin
          Describe('Adding references', procedure
            begin
              It('Should add an array item', procedure
                begin
                  TestAddArrayItem(keywords, '', '', '');
                  TestElementCount(keywords, 6);
                end);

              It('Should be able to set reference with FormID', procedure
                begin
                  TestAddArrayItem(keywords, '', '', '0006BBD4', 'ArmorMaterialDaedric [KYWD:0006BBD4]');
                  TestElementCount(keywords, 7);
                end);

              It('Should be able to add reference with edit value', procedure
                begin
                  TestAddArrayItem(keywords, '', '', 'ArmorLight [KYWD:0006BBD3]', 'ArmorLight [KYWD:0006BBD3]');
                  TestElementCount(keywords, 8);
                end);
            end);

          Describe('Struct arrays', procedure
            begin
              It('Should add an array item', procedure
                begin
                  TestAddArrayItem(entries, '', '', '');
                  TestElementCount(entries, 5);
                end);

              It('Should be able to set value at subpath', procedure
                begin
                  TestAddArrayItem(entries, '', 'LVLO\Reference', 'ArmorLeatherBoots "Leather Boots" [ARMO:00013920]');
                  TestElementCount(entries, 6);
                end);

              It('Should fail if subpath is invalid', procedure
                begin
                  ExpectFailure(AddArrayItem(entries, '', 'Fake\Path', 'ArmorLeatherCuirass "Leather Armor" [ARMO:0003619E]', @h));
                end);
            end);

          It('Should fail if element is not an array', procedure
            begin
              ExpectFailure(AddArrayItem(ar1, '', '', '', @h));
            end);
        end);

      Describe('MoveArrayItem', procedure
        begin
          // TODO
        end);

      Describe('RemoveArrayItem', procedure
        begin
          // TODO
        end);

      Describe('CopyElement', procedure
        begin
          BeforeAll(procedure
            begin
              ExpectSuccess(AddMasters(xt5, 'xtest-2.esp'#13#10'xtest-4.esp'));
            end);

          AfterAll(procedure
            begin
              RemoveElements(xt5);
            end);

          It('Should be able to deep copy groups', procedure
            begin
              h := TestCopyElement('xtest-2.esp\ARMO', xt5, True);
              TestElementFile(h, 'xtest-5.esp');
              ExpectSuccess(GetElement(h, '[0]', @rec));
              TestIsMaster(rec, True);
            end);

          It('Should be able to deep copy records', procedure
            begin
              h := TestCopyElement('xtest-2.esp\00013739', xt5, True);
              TestElementFile(h, 'xtest-5.esp');
              TestIsMaster(h, True);
            end);

          It('Should be able to deep copy elements', procedure
            begin
              try
                ExpectSuccess(AddArrayItem(0, 'xtest-3.esp\00012E46\KWDA', '', '0006BBD4', @h));
                ExpectSuccess(GetElement(0, 'xtest-5.esp\ARMO\[0]', @rec));
                h := TestCopyElement('xtest-3.esp\00012E46\KWDA', rec, True);
                TestElementFile(h, 'xtest-5.esp');
                TestElementCount(h, 6);
              finally
                ExpectSuccess(RemoveArrayItem(0, 'xtest-3.esp\00012E46\KWDA', '', '0006BBD4'));
              end;
            end);

          It('Should be able to override records', procedure
            begin
              h := TestCopyElement('xtest-2.esp\00012E46', xt5, False);
              TestElementFile(h, 'xtest-5.esp');
              TestIsMaster(h, False);
            end);

          Describe('Copying records with errors', procedure
            begin
              It('Should copy records with Deleted References (UDRs)', procedure
                begin
                  h := TestCopyElement('xtest-4.esp\00027DE7', xt5, False);
                  TestElementFile(h, 'xtest-5.esp');
                end);

              It('Should copy records with Unexpected References (UERs)', procedure
                begin
                  h := TestCopyElement('xtest-4.esp\05000800', xt5, False);
                  TestElementFile(h, 'xtest-5.esp');
                end);

              It('Should copy records with Unresolved References (URRs)', procedure
                begin
                  h := TestCopyElement('xtest-4.esp\05000801', xt5, False);
                  TestElementFile(h, 'xtest-5.esp');
                end);

              It('Should copy records with Unexpected Subrecords (UESs)', procedure
                begin
                  h := TestCopyElement('xtest-4.esp\05000802', xt5, False);
                  TestElementFile(h, 'xtest-5.esp');
                end);
            end);
        end);
        
      Describe('GetSignatureAllowed', procedure
        begin
          It('Should return true if signature is allowed', procedure
            begin
              TestGetSignatureAllowed(keyword, 'KYWD', True);
              TestGetSignatureAllowed(keyword, 'NULL', True);
              ExpectSuccess(GetElement(ar2, 'ZNAM', @h));
              TestGetSignatureAllowed(h, 'SNDR', True);
              ExpectSuccess(GetElement(0, 'Update.esm\000E49CD\VMAD\Scripts\[0]\Properties\[0]\Value\Object Union\Object v2\FormID', @h));
              TestGetSignatureAllowed(h, 'NULL', True);
              TestGetSignatureAllowed(h, 'ARMO', True);
              TestGetSignatureAllowed(h, 'WEAP', True);
              TestGetSignatureAllowed(h, 'COBJ', True);
            end);
          It('Should return false if signature is not allowed', procedure
            begin
              TestGetSignatureAllowed(keyword, 'ARMO', False);
              TestGetSignatureAllowed(keyword, 'NPC_', False);
              ExpectSuccess(GetElement(ar2, 'ZNAM', @h));
              TestGetSignatureAllowed(h, 'NULL', False);
            end);

          It('Should raise an exception if a null handle is passed', procedure
            begin
              ExpectFailure(GetSignatureAllowed(0, 'TES4', @b));
            end);

          It('Should raise an exception if element isn''t an integer', procedure
            begin
              ExpectFailure(GetSignatureAllowed(skyrim, 'TES4', @b));
              ExpectFailure(GetSignatureAllowed(armo1, 'ARMO', @b));
              ExpectFailure(GetSignatureAllowed(ar1, 'BODT', @b));
              ExpectFailure(GetSignatureAllowed(keywords, 'KYWD', @b));
            end);
          It('Should raise an exception if element can''t hold formIDs', procedure
            begin
              ExpectFailure(GetSignatureAllowed(dnam, 'ARMO', @b));
            end);
        end);
    end);
end;

end.
