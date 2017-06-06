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
  xeFiles, xeElements, xeElementValues, xeRecordValues,
{$ENDIF}
  txMeta;

procedure TestElementMatches(h: Cardinal; path, value: PWideChar; expectedValue: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(ElementMatches(h, path, value, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestNames(a: CardinalArray; firstName, secondName: String);
var
  len: Integer;
begin
  ExpectSuccess(Name(a[Low(a)], @len));
  ExpectEqual(grs(len), firstName);
  ExpectSuccess(Name(a[High(a)], @len));
  ExpectEqual(grs(len), secondName);
end;

procedure TestEdids(a: CardinalArray; firstEdid, secondEdid: String);
var
  len: Integer;
begin
  ExpectSuccess(EditorID(a[Low(a)], @len));
  ExpectEqual(grs(len), firstEdid);
  ExpectSuccess(EditorID(a[High(a)], @len));
  ExpectEqual(grs(len), secondEdid);
end;

procedure BuildElementHandlingTests;
var
  b: WordBool;
  h, skyrim, xt3, armo1, ar1, keywords, keyword, dnam, element, armo2,
  ar2, ar3, refr: Cardinal;
  len, i: Integer;
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
          ExpectSuccess(GetElement(0, 'xtest-3.esp', @xt3));
          ExpectSuccess(GetElement(xt3, 'ARMO', @armo2));
          ExpectSuccess(GetElement(armo2, '00012E46', @ar3));
          ExpectSuccess(GetElement(0, 'xtest-2.esp\000170F0', @refr));
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
                  ExpectSuccess(GetElement(armo1, '[0]', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(armo1, '[-1]', @h));
                end);
            end);

          Describe('Group record resolution by FormID', procedure
            begin
              It('Should return a handle if the record exists', procedure
                begin
                  ExpectSuccess(GetElement(armo1, '00012E46', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
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
                   ExpectSuccess(GetElement(ar1, '[0]', @h));
                   Expect(h > 0, 'Handle should be greater than 0');
                 end);

              It('Should fail if index is out of bounds', procedure
                begin
                  ExpectFailure(GetElement(ar1, '[-1]', @h));
                end);
            end);

          Describe('Record element resolution by signature', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  ExpectSuccess(GetElement(ar1, 'FULL', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
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
                  ExpectSuccess(GetElement(ar1, 'Male world model', @h));
                  Expect(h > 0, 'Handle should be greater than 0');
                end);

              It('Should fail if the element does not exist', procedure
                begin
                  ExpectFailure(GetElement(ar1, 'Does not exist', @h));
                end);
            end);

          Describe('Record element resolution by path', procedure
            begin
              It('Should return a handle if the element exists', procedure
                begin
                  ExpectSuccess(GetElement(ar1, 'BODT - Body Template', @h));
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
              ExpectSuccess(ElementExists(0, 'Skyrim.esm', @b));
              Expect(b, 'Result should be true');
            end);

          It('Should return true for elements that exist', procedure
            begin
              ExpectSuccess(ElementExists(ar1, 'Male world model', @b));
              Expect(b, 'Result should be true');
            end);

          It('Should return true for handles that are assigned', procedure
            begin
              ExpectSuccess(ElementExists(ar1, '', @b));
              Expect(b, 'Result should be true');
            end);

          It('Should return false for files that do not exist', procedure
            begin
              ExpectSuccess(ElementExists(0, 'NonExistingFile.esp', @b));
              Expect(not b, 'Result should be false');
            end);

          It('Should return false for elements that do not exist', procedure
            begin
              ExpectSuccess(ElementExists(ar1, 'KWDA\[5]', @b));
              Expect(not b, 'Result should be false');
            end);

          It('Should fail if the handle is not assigned', procedure
            begin
              ExpectFailure(ElementExists($FFFFFF, '', @b));
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
              ExpectSuccess(ElementCount(armo1, @i));
              ExpectEqual(i, 2762, '');
            end);

          It('Should return the number of elements in a record', procedure
            begin
              ExpectSuccess(ElementCount(ar1, @i));
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
              ExpectSuccess(ElementEquals(skyrim, skyrim, @b));
              ExpectSuccess(ElementEquals(armo1, armo1, @b));
              ExpectSuccess(ElementEquals(ar1, ar1, @b));
              ExpectSuccess(ElementEquals(keywords, keywords, @b));
              ExpectSuccess(ElementEquals(dnam, dnam, @b));
            end);

          It('Should return false for identical but different elements', procedure
            begin
              ExpectSuccess(GetElement(ar2, 'DNAM', @h));
              ExpectSuccess(ElementEquals(dnam, h, @b));
              Expect(not b, 'Result should be false');
            end);

          It('Should return false for different elements', procedure
            begin
              ExpectSuccess(ElementEquals(skyrim, armo1, @b));
              Expect(not b, 'Result should be false');
              ExpectSuccess(ElementEquals(armo1, ar1, @b));
              Expect(not b, 'Result should be false');
              ExpectSuccess(ElementEquals(ar1, keywords, @b));
              Expect(not b, 'Result should be false');
              ExpectSuccess(ElementEquals(keywords, dnam, @b));
              Expect(not b, 'Result should be false');
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
                  TestElementMatches(ar2, 'DATA\Weight', '5.000000', true);
                  TestElementMatches(ar2, 'DATA\Weight', '5.00000', false);
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
                  TestElementMatches(keywords, '[0]', 'PerkFistsIron', true);
                  TestElementMatches(keywords, '[3]', 'ArmorGauntlets', true);
                  TestElementMatches(ar2, 'RNAM', 'DefaultRace', true);
                end);

              It('Should return false if Editor ID does not match', procedure
                begin
                  TestElementMatches(keywords, '[0]', 'Vampire', false);
                  TestElementMatches(keywords, '[1]', 'ArMorHeAvY', false);
                end);
            end);

          Describe('FULL name matching', procedure
            begin
              It('Should return true if FULL name matches', procedure
                begin
                  TestElementMatches(ar2, 'RNAM', '"Default Race"', true);
                  TestElementMatches(keywords, '[0]', '""', true);
                end);

              It('Should return false if FULL name does not match', procedure
                begin
                  TestElementMatches(ar2, 'RNAM', '"Default RacE"', false);
                  TestElementMatches(ar2, 'ZNAM', '"Null Reference"', false);
                end);
            end);
        end);

      Describe('GetElements', procedure
        begin
          It('Should resolve root children (files)', procedure
            begin
              ExpectSuccess(GetElements(0, '', @len));
              ExpectEqual(len, 8);
              TestNames(gra(len), 'Skyrim.esm', 'xtest-5.esp');
            end);

          It('Should resolve file children (file header and groups)', procedure
            begin
              ExpectSuccess(GetElements(skyrim, '', @len));
              ExpectEqual(len, 118);
              TestNames(gra(len), 'File Header', 'Reverb Parameters');
            end);

          It('Should resolve group children (records)', procedure
            begin
              ExpectSuccess(GetElements(armo1, '', @len));
              ExpectEqual(len, 2762);
              TestEdids(gra(len), 'DremoraBoots', 'SkinNaked');
            end);

          It('Should resolve record children (subrecords/elements)', procedure
            begin
              ExpectSuccess(GetElements(ar1, '', @len));
              ExpectEqual(len, 13);
              TestNames(gra(len), 'Record Header', 'DNAM - Armor Rating');
            end);

          It('Should resolve element children', procedure
            begin
              ExpectSuccess(GetElements(keywords, '', @len));
              ExpectEqual(len, 5);
              TestNames(gra(len), 'Keyword', 'Keyword');
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
              ExpectSuccess(GetElementFile(armo1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the file containing a record', procedure
            begin
              ExpectSuccess(GetElementFile(ar1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the file containing an element', procedure
            begin
              GetElement(ar1, 'DATA\Value', @element);
              ExpectSuccess(GetElementFile(element, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);
        end);

      Describe('GetContainer', procedure
        begin
          It('Should return the file containing a group', procedure
            begin
              ExpectSuccess(GetContainer(armo1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the group containing a record', procedure
            begin
              ExpectSuccess(GetContainer(ar1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the record containing an element', procedure
            begin
              GetElement(ar1, 'EDID', @element);
              ExpectSuccess(GetContainer(element, @h));
              Expect(h > 0, 'Handle should be greater than 0');
              GetElement(refr, 'Record Header', @element);
              ExpectSuccess(GetContainer(element, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return the parent element containing a child element', procedure
            begin
              GetElement(ar1, 'BODT\Armor Type', @element);
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
              ExpectSuccess(GetLinksTo(ar1, 'RNAM', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should fail if called on a NULL reference', procedure
            begin
              ExpectFailure(GetLinksTo(armo2, 'ZNAM', @h));
            end);

          It('Should fail if path is invalid', procedure
            begin
              ExpectFailure(GetLinksTo(keywords, '[7]', @h));
            end);

          It('Should fail if called on an element that does not store a reference', procedure
            begin
              ExpectFailure(GetLinksTo(0, '', @h));
              ExpectFailure(GetLinksTo(skyrim, '', @h));
              ExpectFailure(GetLinksTo(ar1, '', @h));
              ExpectFailure(GetLinksTo(dnam, '', @h));
            end);
        end);

      Describe('AddElement', procedure
        begin
          It('Should create a new file if no handle given', procedure
            begin
              ExpectSuccess(AddElement(0, 'NewFile-1.esp', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to add groups to files', procedure
            begin
              ExpectSuccess(AddElement(xt3, 'ARMO', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to add records to groups', procedure
            begin
              ExpectSuccess(AddElement(armo2, 'ARMO', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to create a new element on a record', procedure
            begin
              ExpectSuccess(AddElement(ar2, 'Destructable', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to push a new element onto an array', procedure
            begin
              ExpectSuccess(AddElement(keywords, '.', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should be able to assign an element at an index in an array', procedure
            begin
              ExpectSuccess(AddElement(keywords, '[1]', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should fail if parent element is not a container', procedure
            begin
              GetElement(ar2, 'FULL', @element);
              ExpectFailure(AddElement(element, '', @h));
            end);
        end);

      Describe('RemoveElement', procedure
        begin
          It('Should remove the element at the given path', procedure
            begin
              ExpectSuccess(RemoveElement(ar3, 'Female world model'));
              ExpectSuccess(ElementExists(ar3, 'Female world model', @b));
              Expect(not b, 'The element should no longer be present');
            end);

          It('Should remove the element at the given indexed path', procedure
            begin
              ExpectSuccess(RemoveElement(ar3, 'KWDA\[4]'));
              ExpectSuccess(ElementExists(ar3, 'KWDA\[4]', @b));
              Expect(not b, 'The element should no longer be present');
            end);

          It('Should remove the element passed if no path is given', procedure
            begin
              ExpectSuccess(GetElement(ar3, 'ZNAM', @element));
              ExpectSuccess(RemoveElement(element, ''));
              ExpectSuccess(ElementExists(ar3, 'ZNAM', @b));
              Expect(not b, 'The element should no longer be present');
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

      Describe('GetExpectedSignatures', procedure
        begin
          It('Should list allowed signatures', procedure
            begin
              ExpectSuccess(GetExpectedSignatures(keyword, @len));
              ExpectEqual(grs(len), 'KYWD,NULL');
              ExpectSuccess(GetElement(ar2, 'ZNAM', @h));
              ExpectSuccess(GetExpectedSignatures(h, @len));
              ExpectEqual(grs(len), 'SNDR');
            end);
          It('Should return * if any signature is allowed', procedure
            begin
              ExpectSuccess(GetElement(0, 'Update.esm\000E49CD\VMAD\Scripts\[0]\Properties\[0]\Value\Object Union\Object v2\FormID', @h));
              ExpectSuccess(GetExpectedSignatures(h, @len));
              ExpectEqual(grs(len), '*');
            end);
          It('Should raise an exception if a null handle is passed', procedure
            begin
              ExpectFailure(GetExpectedSignatures(0, @len));
            end);
          It('Should raise an exception if element isn''t an integer', procedure
            begin
              ExpectFailure(GetExpectedSignatures(skyrim, @len));
              ExpectFailure(GetExpectedSignatures(armo1, @len));
              ExpectFailure(GetExpectedSignatures(ar1, @len));
              ExpectFailure(GetExpectedSignatures(keywords, @len));
            end);
          It('Should raise an exception if element can''t hold formIDs', procedure
            begin
              ExpectFailure(GetExpectedSignatures(dnam, @len));
            end);
        end);
    end);
end;

end.
