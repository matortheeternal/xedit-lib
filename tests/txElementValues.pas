unit txElementValues;

interface

  // PUBLIC TESTING INTERFACE
  procedure BuildElementValueTests;

implementation

uses
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeElements, xeElementValues, xeRecords,
{$ENDIF}
  txMeta, txElements;

function GetFullName(h: Cardinal): String;
var
  len: Integer;
begin
  GetValue(h, 'FULL', @len);
  Result := grs(len);
end;

procedure TestSetValue(h: Cardinal; path, value: PWideChar);
var
  len: Integer;
begin
  ExpectSuccess(SetValue(h, path, value));
  ExpectSuccess(GetValue(h, path, @len));
  ExpectEqual(grs(len), string(value));
end;

procedure TestSetUIntValue(h: Cardinal; path: PWideChar; value: Cardinal);
var
  c: Cardinal;
begin
  ExpectSuccess(SetUIntValue(h, path, value));
  ExpectSuccess(GetUIntValue(h, path, @c));
  ExpectEqual(c, value);
end;

procedure TestGetFlag(h: Cardinal; path, flag: PWideChar; expectedValue: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(GetFlag(h, path, flag, @b));
  ExpectEqual(b, expectedValue);
end;

procedure TestSetEnabledFlags(h: Cardinal; path, flags: PWideChar);
var
  len: Integer;
begin
  ExpectSuccess(SetEnabledFlags(h, path, flags));
  ExpectSuccess(GetEnabledFlags(h, path, @len));
  ExpectEqual(grs(len), string(flags));
end;

procedure TestSignatureFromName(name: PWideChar; sig: String);
var
  len: Integer;
begin
  ExpectSuccess(SignatureFromName(name, @len));
  ExpectEqual(grs(len), sig);
end;

procedure TestNameFromSignature(sig: PWideChar; name: String);
var
  len: Integer;
begin
  ExpectSuccess(NameFromSignature(sig, @len));
  ExpectEqual(grs(len), name);
end;

procedure TestSetFlag(h: Cardinal; path, flag: PWideChar; enable: WordBool);
var
  b: WordBool;
begin
  ExpectSuccess(SetFlag(h, path, flag, enable));
  ExpectSuccess(GetFlag(h, path, flag, @b));
  ExpectEqual(b, enable);
end;

procedure BuildElementValueTests;
var
  xt2, fileHeader, fileFlags, block, subBlock, childGroup, persistentGroup, refr, armo, rec,
  refrFlags, element, keyword, h, c: Cardinal;
  expectedName, str: String;
  b: WordBool;
  f: Double;
  len, i: Integer;
begin
  Describe('Element Values', procedure
    begin
      BeforeAll(procedure
        begin
          ExpectSuccess(GetElement(0, 'xtest-2.esp', @xt2));
          ExpectSuccess(GetElement(xt2, 'File Header', @fileHeader));
          ExpectSuccess(GetElement(fileHeader, 'Record Header\Record Flags', @fileFlags));
          ExpectSuccess(GetElement(xt2, 'ARMO', @armo));
          ExpectSuccess(GetElement(armo, '00012E46', @rec));
          ExpectSuccess(GetElement(rec, 'DNAM', @element));
          ExpectSuccess(GetElement(rec, 'KWDA\[1]', @keyword));
          ExpectSuccess(GetElement(xt2, '00027D1C\Child Group', @childGroup));
          ExpectSuccess(GetElement(xt2, 'CELL\[0]', @block));
          ExpectSuccess(GetElement(block, '[0]', @subBlock));
          ExpectSuccess(GetElement(childGroup, '[0]', @persistentGroup));
          ExpectSuccess(GetElement(xt2, '000170F0', @refr));
          ExpectSuccess(GetElement(refr, 'Record Header\Record Flags', @refrFlags));
        end);
        
      Describe('Name', procedure
        begin
          It('Should resolve file names', procedure
            begin
              ExpectSuccess(Name(xt2, @len));
              ExpectEqual(grs(len), 'xtest-2.esp');
            end);

          Describe('Group names', procedure
            begin
              It('Should resolve top level group names', procedure
                begin
                  ExpectSuccess(Name(armo, @len));
                  ExpectEqual(grs(len), 'Armor');
                end);

              It('Should resolve block names', procedure
                begin
                  ExpectSuccess(Name(block, @len));
                  ExpectEqual(grs(len), 'Block 0');
                end);

              It('Should resolve sub-block names', procedure
                begin
                  ExpectSuccess(Name(subBlock, @len));
                  ExpectEqual(grs(len), 'Sub-Block 0');
                end);

              It('Should resolve child group names', procedure
                begin
                  ExpectSuccess(Name(childGroup, @len));
                  expectedName := 'Children of 00027D1C';
                  ExpectEqual(grs(len), expectedName);
                end);

              It('Should resolve persistent/temporary group names', procedure
                begin
                  ExpectSuccess(Name(persistentGroup, @len));
                  expectedName := 'Persistent';
                  ExpectEqual(grs(len), expectedName);
                end);
            end);

          Describe('Record names', procedure
            begin
              It('Should resolve FULL name, if present', procedure
                begin
                  ExpectSuccess(Name(rec, @len));
                  ExpectEqual(grs(len), 'Iron Gauntlets');
                end);

              It('Should resolve Editor ID, if present', procedure
                begin
                  ExpectSuccess(Name(refr, @len));
                  ExpectEqual(grs(len), 'ITPOTest');
                end);

              It('Should resolve context for cells with no EDID or FULL', procedure
                begin
                  ExpectSuccess(GetElement(0, 'Update.esm\00038381', @h));
                  ExpectSuccess(Name(h, @len));
                  ExpectEqual(grs(len), '"Windhelm" <32,9>');
                end);

              It('Should resolve context for placements with no EDID', procedure
                begin
                  ExpectSuccess(GetElement(0, 'Update.esm\0003F70F', @h));
                  ExpectSuccess(Name(h, @len));
                  ExpectEqual(grs(len), 'Places "Chest" in "Skyrim" <0,0>');
                end);
            end);

          It('Should resolve element names', procedure
            begin
              ExpectSuccess(Name(element, @len));
              ExpectEqual(grs(len), 'DNAM - Armor Rating');
            end);
        end);

      Describe('DisplayName', procedure
        begin
          Describe('File names', procedure
            begin
              It('Should include filename', procedure
                begin
                  ExpectSuccess(DisplayName(xt2, @len));
                  Expect(Pos('xtest-2.esp', string(grs(len))) > 0);
                end);

              It('Should include load order', procedure
                begin
                  ExpectSuccess(DisplayName(xt2, @len));
                  Expect(Pos('[03]', string(grs(len))) = 1);
                end);

              It('Should format hardcoded dat names properly', procedure
                begin
                  ExpectSuccess(GetElement(0, 'Skyrim.Hardcoded.dat', @h));
                  ExpectSuccess(DisplayName(h, @len));
                  ExpectEqual(grs(len), '[00] Skyrim.exe');
                end);
            end);
        end);

      Describe('Path', procedure
        begin
          It('Should return file names', procedure
            begin
              ExpectSuccess(Path(xt2, false, false, false, @len));
              ExpectEqual(grs(len), 'xtest-2.esp');
            end);

          It('Should return group signatures', procedure
            begin
              ExpectSuccess(Path(armo, false, false, false,  @len));
              ExpectEqual(grs(len), 'xtest-2.esp\ARMO');
            end);

          It('Should return block names', procedure
            begin
              ExpectSuccess(Path(block, false, false, false,  @len));
              ExpectEqual(grs(len), 'xtest-2.esp\CELL\Block 0');
            end);

          It('Should return sub-block names', procedure
            begin
              ExpectSuccess(Path(subBlock, false, false, false,  @len));
              ExpectEqual(grs(len), 'xtest-2.esp\CELL\Block 0\Sub-Block 0');
            end);

          It('Should return child groups', procedure
            begin
              ExpectSuccess(Path(childGroup, true, false, false, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00027D1C\Child Group');
            end);

          It('Should return temporary/persistent groups', procedure
            begin
              ExpectSuccess(Path(persistentGroup, true, false, false,  @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00027D1C\Child Group\Persistent');
            end);

          It('Should return record FormIDs', procedure
            begin
              ExpectSuccess(Path(refr, true, false, false, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\000170F0');
            end);

          It('Should return file headers', procedure
            begin
              ExpectSuccess(Path(fileHeader, false, false, false,  @len));
              ExpectEqual(grs(len), 'xtest-2.esp\File Header');
            end);

          It('Should return element names', procedure
            begin
              ExpectSuccess(Path(element, true, false, false, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00012E46\DNAM');
            end);

          It('Should return array element indexes', procedure
            begin
              ExpectSuccess(Path(keyword, true, false, false, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00012E46\KWDA\[1]');
            end);

          It('Should return local paths when local is true', procedure
            begin
              ExpectSuccess(Path(keyword, false, true, false, @len));
              ExpectEqual(grs(len), 'KWDA\[1]');
            end);

          It('Should return sortkeys when sort is true', procedure
            begin
              ExpectSuccess(Path(keyword, false, true, true, @len));
              ExpectEqual(grs(len), 'KWDA\<0006BBD2>');
            end);
        end);

      Describe('PathName', procedure
        begin
          It('Should return file names', procedure
            begin
              ExpectSuccess(PathName(xt2, false, @len));
              ExpectEqual(grs(len), 'xtest-2.esp');
            end);

          It('Should return group signatures', procedure
            begin
              ExpectSuccess(PathName(armo, false,  @len));
              ExpectEqual(grs(len), 'ARMO');
            end);

          It('Should return block names', procedure
            begin
              ExpectSuccess(PathName(block, false,  @len));
              ExpectEqual(grs(len), 'Block 0');
            end);

          It('Should return sub-block names', procedure
            begin
              ExpectSuccess(PathName(subBlock, false,  @len));
              ExpectEqual(grs(len), 'Sub-Block 0');
            end);

          It('Should return child groups', procedure
            begin
              ExpectSuccess(PathName(childGroup, false, @len));
              ExpectEqual(grs(len), 'Child Group');
            end);

          It('Should return temporary/persistent groups', procedure
            begin
              ExpectSuccess(PathName(persistentGroup, false,  @len));
              ExpectEqual(grs(len), 'Persistent');
            end);

          It('Should return record FormIDs', procedure
            begin
              ExpectSuccess(PathName(refr, false, @len));
              ExpectEqual(grs(len), '000170F0');
            end);

          It('Should return file headers', procedure
            begin
              ExpectSuccess(PathName(fileHeader, false,  @len));
              ExpectEqual(grs(len), 'File Header');
            end);

          It('Should return element names', procedure
            begin
              ExpectSuccess(PathName(element, false, @len));
              ExpectEqual(grs(len), 'DNAM');
            end);

          It('Should return array element indexes', procedure
            begin
              ExpectSuccess(PathName(keyword, false, @len));
              ExpectEqual(grs(len), '[1]');
            end);

          It('Should return sortkeys when sort is true', procedure
            begin
              ExpectSuccess(PathName(keyword, true, @len));
              ExpectEqual(grs(len), '<0006BBD2>');
            end);
        end);
        
      Describe('Signature', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(Signature(xt2, @len));
            end);

          It('Should fail if an element with no signature is passed', procedure
            begin
              ExpectFailure(Signature(keyword, @len));
            end);

          It('Should resolve group signatures', procedure
            begin
              ExpectSuccess(Signature(block, @len));
              ExpectEqual(grs(len), 'GRUP');
              ExpectSuccess(Signature(subBlock, @len));
              ExpectEqual(grs(len), 'GRUP');
              ExpectSuccess(Signature(childGroup, @len));
              ExpectEqual(grs(len), 'GRUP');
              ExpectSuccess(Signature(persistentGroup, @len));
              ExpectEqual(grs(len), 'GRUP');
              ExpectSuccess(Signature(armo, @len));
              ExpectEqual(grs(len), 'ARMO');
            end);

          It('Should resolve record signatures', procedure
            begin
              ExpectSuccess(Signature(rec, @len));
              ExpectEqual(grs(len), 'ARMO');
              ExpectSuccess(Signature(refr, @len));
              ExpectEqual(grs(len), 'REFR');
            end);

          It('Should resolve element signatures', procedure
            begin
              ExpectSuccess(Signature(element, @len));
              ExpectEqual(grs(len), 'DNAM');
            end);
        end);

      Describe('GetValue', procedure
        begin
          It('Should resolve element values', procedure
            begin
              ExpectSuccess(GetValue(element, '', @len));
              ExpectEqual(grs(len), '10.000000');
              ExpectSuccess(GetValue(keyword, '', @len));
              ExpectEqual(grs(len), 'ArmorHeavy [KYWD:0006BBD2]');
            end);

          It('Should resolve element value at path', procedure
            begin
              ExpectSuccess(GetValue(rec, 'OBND\X1', @len));
              ExpectEqual(grs(len), '-11');
              ExpectSuccess(GetValue(rec, 'KWDA\[1]', @len));
              ExpectEqual(grs(len), 'ArmorHeavy [KYWD:0006BBD2]');
              ExpectSuccess(GetValue(rec, 'Female world model\MOD4', @len));
              ExpectEqual(grs(len), 'Test');
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetValue(rec, 'Non\Existent\Path', @len));
            end);

          {It('Should be fast', procedure
            begin
              ExpectSuccess(GetElement(0, 'Skyrim.esm\ARMO\000135BA', @h));
              Benchmark(100000, procedure
                begin
                  GetFullName(h);
                end);
            end);}
        end);

      Describe('GetRefValue', procedure
        begin
          It('Should return ref values for records', procedure
            begin
              ExpectSuccess(GetRefValue(rec, '', @len));
              ExpectEqual(grs(len), '{Skyrim.esm:012E46}');
            end);

          It('Should return ref values for FormID elements', procedure
            begin
              ExpectSuccess(GetRefValue(rec, 'KWDA\[1]', @len));
              ExpectEqual(grs(len), '{Skyrim.esm:06BBD2}');
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetRefValue(rec, 'Non\Existent\Path', @len));
            end);

          It('Should fail if element is not supported', procedure
            begin
              ExpectFailure(GetRefValue(rec, 'OBND\Y1', @len));
            end);
        end);

      Describe('GetIntValue', procedure
        begin
          It('Should resolve element integer values', procedure
            begin
              ExpectSuccess(GetElement(rec, 'OBND\Y1', @h));
              ExpectSuccess(GetIntValue(h, '', @i));
              ExpectEqual(i, -15);
            end);

          It('Should resolve element integer values at paths', procedure
            begin
              ExpectSuccess(GetIntValue(rec, 'OBND\Z1', @i));
              ExpectEqual(i, -1);
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetIntValue(rec, 'Non\Existent\Path', @i));
            end);
        end);

      Describe('GetUIntValue', procedure
        begin
          It('Should resolve element unsigned integer values', procedure
            begin
              ExpectSuccess(GetUIntValue(keyword, '', @c));
              ExpectEqual(c, $6BBD2);
            end);

          It('Should resolve element unsigned integer values at paths', procedure
            begin
              ExpectSuccess(GetUIntValue(rec, 'KWDA\[0]', @c));
              ExpectEqual(c, $424EF);
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetUIntValue(rec, 'Non\Existent\Path', @c));
            end);
        end);

      Describe('GetFloatValue', procedure
        begin
          It('Should resolve element float values', procedure
            begin
              ExpectSuccess(GetFloatValue(element, '', @f));
              // armor rating is stored at *100 internally, for some reason
              ExpectEqual(f, 1000.0);
            end);

          It('Should resolve element float values at paths', procedure
            begin
              ExpectSuccess(GetFloatValue(rec, 'DATA\Weight', @f));
              ExpectEqual(f, 5.0);
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetFloatValue(rec, 'Non\Existent\Path', @f));
            end);
        end);

      Describe('SetValue', procedure
        begin
          It('Should set element values', procedure
            begin
              TestSetValue(element, '', '14.100000');
              TestSetValue(keyword, '', 'ArmorLight [KYWD:0006BBD3]');
            end);

          It('Should set element value at path', procedure
            begin
              TestSetValue(rec, 'OBND\X1', '-8');
              TestSetValue(rec, 'KWDA\[0]', 'PerkFistsEbony [KYWD:0002C178]');
              TestSetValue(rec, 'Female world model\MOD4', 'Armor\Iron\F\GauntletsGND.nif');
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(SetValue(rec, 'Non\Existent\Path', 'Test'));
            end);
        end);

      Describe('SetIntValue', procedure
        begin
          It('Should set element integer values', procedure
            begin
              GetElement(rec, 'OBND\Y1', @h);
              ExpectSuccess(SetIntValue(h, '', -13));
              ExpectSuccess(GetIntValue(h, '', @i));
              ExpectEqual(i, -13);
            end);

          It('Should set element integer values at paths', procedure
            begin
              ExpectSuccess(SetIntValue(rec, 'OBND\Z1', -4));
              ExpectSuccess(GetIntValue(rec, 'OBND\Z1', @i));
              ExpectEqual(i, -4);
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(SetIntValue(rec, 'Non\Existent\Path', 1));
            end);
        end);

      Describe('SetUIntValue', procedure
        begin
          It('Should set element unsigned integer values', procedure
            begin
              TestSetUIntValue(keyword, '', $6BBE2);
            end);

          It('Should set element unsigned integer values at paths', procedure
            begin
              TestSetUIntValue(rec, 'KWDA\[0]', $2C177)
            end);

          It('Should work with version control info', procedure
            begin
              TestSetUIntValue(rec, 'Record Header\Version Control Info 1', $1234);
              TestSetUIntValue(rec, 'Record Header\Version Control Info 2', $1234);
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(SetUIntValue(rec, 'Non\Existent\Path', $10));
            end);
        end);

      Describe('SetFloatValue', procedure
        begin
          It('Should resolve element float values', procedure
            begin
              ExpectSuccess(SetFloatValue(element, '', 1920.0));
              ExpectSuccess(GetFloatValue(element, '', @f));
              // armor rating is stored at *100 internally, for some reason
              ExpectEqual(f, 1920.0);
            end);

          It('Should resolve element float values at paths', procedure
            begin
              ExpectSuccess(SetFloatValue(rec, 'DATA\Weight', 7.3));
              ExpectSuccess(GetFloatValue(rec, 'DATA\Weight', @f));
              ExpectEqual(f, 7.3);
            end);

          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(SetFloatValue(rec, 'Non\Existent\Path', 1.23));
            end);
        end);

      Describe('GetFlag', procedure
        begin
          It('Should return false for disabled flags', procedure
            begin
              TestGetFlag(fileFlags, '', 'ESM', false);
              TestGetFlag(fileFlags, '', 'Localized', false);
              TestGetFlag(fileFlags, '', '', false);
              TestGetFlag(refrFlags, '', 'Deleted', false);
              TestGetFlag(refrFlags, '', 'Ignored', false);
              TestGetFlag(refrFlags, '', 'Unknown 0', false);
            end);

          It('Should return true for enabled flags', procedure
            begin
              TestGetFlag(0, 'xtest-1.esp\File Header\Record Header\Record Flags', 'ESM', true);
              TestGetFlag(rec, 'BODT\First Person Flags', '33 - Hands', true);
              TestGetFlag(refrFlags, '', 'Persistent', true);
              TestGetFlag(refrFlags, '', 'Initially Disabled', true);
            end);

          It('Should fail if flag is not found', procedure
            begin
              ExpectFailure(GetFlag(refrFlags, '', 'Temporary', @b));
              ExpectFailure(GetFlag(refrFlags, '', 'Unknown 5', @b));
            end);

          It('Should fail on elements that do not have flags', procedure
            begin
              ExpectFailure(GetFlag(xt2, '', 'ESM', @b));
              ExpectFailure(GetFlag(xt2, 'File Header', 'ESM', @b));
              ExpectFailure(GetFlag(refr, '', 'Deleted', @b));
              ExpectFailure(GetFlag(refr, 'Record Header', 'Deleted', @b));
            end);
        end);

      Describe('SetFlag', procedure
        begin
          It('Should enable disabled flags', procedure
            begin
              TestSetFlag(fileFlags, '', 'Localized', true);
              // This test causes issues later on for some reason.
              //TestSetFlag(refrFlags, '', 'Deleted', true);
              TestSetFlag(refrFlags, '', 'Ignored', true);
              TestSetFlag(rec, 'BODT\First Person Flags', '32 - Body', true);
            end);

          It('Should disable enabled flags', procedure
            begin
              TestSetFlag(fileFlags, '', 'Localized', false);
              //TestSetFlag(refrFlags, '', 'Deleted', false);
              TestSetFlag(refrFlags, '', 'Ignored', false);
              TestSetFlag(rec, 'BODT\First Person Flags', '32 - Body', false);
            end);

          It('Should fail if flag is not found', procedure
            begin
              ExpectFailure(SetFlag(refrFlags, '', 'Temporary', true));
              ExpectFailure(SetFlag(refrFlags, '', 'Unknown 5', false));
            end);

          It('Should fail on elements that do not have flags', procedure
            begin
              ExpectFailure(SetFlag(xt2, '', 'ESM', true));
              ExpectFailure(SetFlag(xt2, 'File Header', 'ESM', true));
              ExpectFailure(SetFlag(refr, '', 'Deleted', true));
              ExpectFailure(SetFlag(refr, 'Record Header', 'Deleted', true));
            end);
        end);

      Describe('GetEnabledFlags', procedure
        begin
          It('Should return an empty string if no flags are enabled', procedure
            begin
              ExpectSuccess(GetEnabledFlags(fileFlags, '', @len));
              ExpectEqual(len, 0);
            end);

          It('Should return a comma separated string of flag names', procedure
            begin
              ExpectSuccess(GetEnabledFlags(0, 'xtest-1.esp\File Header\Record Header\Record Flags', @len));
              ExpectEqual(grs(len), 'ESM');
              ExpectSuccess(GetEnabledFlags(rec, 'BODT\First Person Flags', @len));
              ExpectEqual(grs(len), '33 - Hands');
              ExpectSuccess(GetEnabledFlags(refrFlags, '', @len));
              ExpectEqual(grs(len), 'Persistent,Initially Disabled');
            end);

          It('Should fail on elements that do not have flags', procedure
            begin
              ExpectFailure(GetEnabledFlags(xt2, '', @len));
              ExpectFailure(GetEnabledFlags(xt2, 'File Header', @len));
              ExpectFailure(GetEnabledFlags(refr, '', @len));
              ExpectFailure(GetEnabledFlags(refr, 'Record Header', @len));
            end);
        end);

      Describe('SetEnabledFlags', procedure
        begin
          It('Should enable flags that are present', procedure
            begin
              TestSetEnabledFlags(fileFlags, '', 'ESM,Localized,Ignored');
              // NOTE: the 0th Record Flag is disallowed except on the File Header
              TestSetEnabledFlags(refrFlags, '', 'Unknown 1,Persistent,Initially Disabled,Ignored,Multibound');
              TestSetEnabledFlags(rec, 'BODT\First Person Flags', '30 - Head,33 - Hands,40 - Tail,52 - Unnamed,61 - FX01');
            end);

          It('Should disable flags that are not present', procedure
            begin
              TestSetEnabledFlags(fileFlags, '', '');
              TestSetEnabledFlags(refrFlags, '', 'Persistent,Initially Disabled');
              TestSetEnabledFlags(rec, 'BODT\First Person Flags', '33 - Hands');
            end);

          It('Should fail on elements that do not have flags', procedure
            begin
              ExpectFailure(SetEnabledFlags(xt2, '', ''));
              ExpectFailure(SetEnabledFlags(xt2, 'File Header', ''));
              ExpectFailure(SetEnabledFlags(refr, '', ''));
              ExpectFailure(SetEnabledFlags(refr, 'Record Header', ''));
            end);
        end);

      Describe('GetAllFlags', procedure
        begin
          It('Should return a comma separated list of all flag names', procedure
            begin
              // FILE FLAGS
              ExpectSuccess(GetAllFlags(fileFlags, '', @len));
              str := grs(len);
              Expect(Pos('ESM', str) = 1, 'ESM should be the first flag');
              Expect(Pos(',,,,,,,', str) > 0, 'Should include empty flags');
              Expect(Pos('Localized', str) > 0, 'Localized should be included');
              Expect(Pos('Ignored', str) > 0, 'Ignored should be included');
              // REFR FLAGS
              ExpectSuccess(GetAllFlags(refrFlags, '', @len));
              str := grs(len);
              Expect(Pos('Unknown 0', str) = 1, 'Unknown 0 should be the first flag');
              Expect(Pos('Hidden From Local Map', str) > 0, 'Hidden From Local Map should be included');
              Expect(Pos('Visible when distant', str) > 0, 'Visible when  distant should be included');
              Expect(Pos('Multibound', str) > 0, 'Multibound should be included');
              // ARMO FIRST PERSON FLAGS
              ExpectSuccess(GetAllFlags(rec, 'BODT\First Person Flags', @len));
              str := grs(len);
              Expect(Pos('30 - Head', str) = 1, '30 - Head should be the first flag');
              Expect(Pos('36 - Ring', str) > 0, '36 - Ring should be included');
              Expect(Pos('44 - Unnamed', str) > 0, '44 - Unnamed should be included');
              Expect(Pos('61 - FX01', str) > 0, '61 - FX01 should be included');
            end);

          It('Should fail on elements that do not have flags', procedure
            begin
              ExpectFailure(GetAllFlags(xt2, '', @len));
              ExpectFailure(GetAllFlags(xt2, 'File Header', @len));
              ExpectFailure(GetAllFlags(refr, '', @len));
              ExpectFailure(GetAllFlags(refr, 'Record Header', @len));
            end);
        end);

      Describe('SignatureFromName', procedure
        begin
          It('Should succeed for top level record names', procedure
            begin
              TestSignatureFromName('Armor', 'ARMO');
              TestSignatureFromName('Weapon', 'WEAP');
              TestSignatureFromName('Cell', 'CELL');
              TestSignatureFromName('Non-Player Character (Actor)', 'NPC_');
              TestSignatureFromName('Constructible Object', 'COBJ');
              TestSignatureFromName('Explosion', 'EXPL');
              TestSignatureFromName('Word of Power', 'WOOP');
            end);

          It('Should succeed for other names', procedure
            begin
              TestSignatureFromName('Placed Object', 'REFR');
              TestSignatureFromName('Navigation Mesh', 'NAVM');
              TestSignatureFromName('Placed NPC', 'ACHR');
              TestSignatureFromName('Placed Projectile', 'PGRE');
              TestSignatureFromName('Placed Missile', 'PMIS');
              TestSignatureFromName('Placed Arrow', 'PARW');
              TestSignatureFromName('Placed Beam', 'PBEA');
              TestSignatureFromName('Placed Flame', 'PFLA');
              TestSignatureFromName('Placed Cone/Voice', 'PCON');
              TestSignatureFromName('Placed Barrier', 'PBAR');
              TestSignatureFromName('Placed Hazard', 'PHZD');
              TestSignatureFromName('HAIR', 'HAIR');
            end);

          It('Should fail if name does not correspond to a signature', procedure
            begin
              ExpectFailure(SignatureFromName('', @len));
              ExpectFailure(SignatureFromName('This is fake', @len));
              ExpectFailure(SignatureFromName('ArMoR', @len));
              ExpectFailure(SignatureFromName('Constructible', @len));
              ExpectFailure(SignatureFromName('Explosion=', @len));
            end);
        end);

      Describe('NameFromSignature', procedure
        begin
          It('Should succeed for top level record signatures', procedure
            begin
              TestNameFromSignature('ARMO', 'Armor');
              TestNameFromSignature('WEAP', 'Weapon');
              TestNameFromSignature('CELL', 'Cell');
              TestNameFromSignature('NPC_', 'Non-Player Character (Actor)');
              TestNameFromSignature('COBJ', 'Constructible Object');
              TestNameFromSignature('EXPL', 'Explosion');
              TestNameFromSignature('WOOP', 'Word of Power');
            end);

          It('Should succeed for other signatures', procedure
            begin
              TestNameFromSignature('REFR', 'Placed Object');
              TestNameFromSignature('NAVM', 'Navigation Mesh');
              TestNameFromSignature('ACHR', 'Placed NPC');
              TestNameFromSignature('PGRE', 'Placed Projectile');
              TestNameFromSignature('PMIS', 'Placed Missile');
              TestNameFromSignature('PARW', 'Placed Arrow');
              TestNameFromSignature('PBEA', 'Placed Beam');
              TestNameFromSignature('PFLA', 'Placed Flame');
              TestNameFromSignature('PCON', 'Placed Cone/Voice');
              TestNameFromSignature('PBAR', 'Placed Barrier');
              TestNameFromSignature('PHZD', 'Placed Hazard');
              TestNameFromSignature('HAIR', 'HAIR');
            end);

          It('Should fail if signature does not correspond to a name', procedure
            begin
              ExpectFailure(NameFromSignature('', @len));
              ExpectFailure(NameFromSignature('FAKE', @len));
              ExpectFailure(NameFromSignature('armo', @len));
              ExpectFailure(NameFromSignature('COBJ_', @len));
              ExpectFailure(NameFromSignature('NPC', @len));
            end);
        end);

      Describe('GetSignatureNameMap', procedure
        begin
          It('Should succeed', procedure
            begin
              ExpectSuccess(GetSignatureNameMap(@len));
            end, true);

          It('Should yield a string', procedure
            begin
              str := string(grs(len));
              Expect(Length(str) > 0, 'String should have length > 0');
            end, true);

          It('Should include signature name pairs', procedure
            begin
              Expect(Pos('ARMO=Armor', str) > 0, 'Should contain ARMO=Armor');
              Expect(Pos('REFR=Placed Object', str) > 0, 'Should contain REFR=Placed Object');
              Expect(Pos('PHZD=Placed Hazard', str) > 0, 'Should contain PHZD=Placed Hazard');
              Expect(Pos('PWAT=PWAT', str) > 0, 'Should contain PWAT=PWAT');
              Expect(Pos('NPC_=Non-Player Character (Actor)', str) > 0, 'Should contain NPC_=Non-Player Character (Actor)');
            end);
        end);
    end);
end;

end.
