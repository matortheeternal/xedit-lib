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
  xeElements, xeElementValues,
{$ENDIF}
  txMeta, txElements;

procedure BuildElementValueTests;
var
  xt2, block, subBlock, childGroup, persistentGroup, refr, armo, rec,
  element, keyword, h, c: Cardinal;
  expectedName: String;
  f: Double;
  len, i: Integer;
begin
  Describe('Element Values', procedure
    begin
      BeforeAll(procedure
        begin
          GetElement(0, 'xtest-2.esp', @xt2);
          GetElement(xt2, 'ARMO', @armo);
          GetElement(armo, '00012E46', @rec);
          GetElement(rec, 'DNAM', @element);
          GetElement(rec, 'KWDA\[1]', @keyword);
          GetElement(xt2, '00027D1C\Child Group', @childGroup);
          GetElement(xt2, 'CELL\[0]', @block);
          GetElement(block, '[0]', @subBlock);
          GetElement(childGroup, '[0]', @persistentGroup);
          GetElement(xt2, '000170F0', @refr);
        end);
        
      Describe('Name', procedure
        begin
          It('Should resolve file names', procedure
            begin
              ExpectSuccess(Name(xt2, @len));
              ExpectEqual(grs(len), 'xtest-2.esp', '');
            end);
          Describe('Group names', procedure
            begin
              It('Should resolve top level group names', procedure
                begin
                  ExpectSuccess(Name(armo, @len));
                  ExpectEqual(grs(len), 'Armor', '');
                end);
              It('Should resolve block names', procedure
                begin
                  ExpectSuccess(Name(block, @len));
                  ExpectEqual(grs(len), 'Block 0', '');
                end);
              It('Should resolve sub-block names', procedure
                begin
                  ExpectSuccess(Name(subBlock, @len));
                  ExpectEqual(grs(len), 'Sub-Block 0', '');
                end);
              It('Should resolve child group names', procedure
                begin
                  ExpectSuccess(Name(childGroup, @len));
                  expectedName := 'Children of 00027D1C';
                  ExpectEqual(grs(len), expectedName, '');
                end);
              It('Should resolve persistent/temporary group names', procedure
                begin
                  ExpectSuccess(Name(persistentGroup, @len));
                  expectedName := 'Persistent';
                  ExpectEqual(grs(len), expectedName, '');
                end);
            end);
          Describe('Record names', procedure
            begin
              It('Should resolve FULL name, if present', procedure
                begin
                  ExpectSuccess(Name(rec, @len));
                  ExpectEqual(grs(len), 'Iron Gauntlets', '');
                end);
              It('Should resolve BASE name, if present', procedure
                begin
                  ExpectSuccess(Name(refr, @len));
                  expectedName := 'DA09PedestalEmpty "Pedestal" [ACTI:0007F82A]';
                  ExpectEqual(grs(len), expectedName, '');
                end);
            end);
          It('Should resolve element names', procedure
            begin
              ExpectSuccess(Name(element, @len));
              ExpectEqual(grs(len), 'DNAM - Armor Rating', '');
            end);
        end);

      Describe('Path', procedure
        begin
          It('Should resolve file names', procedure
            begin
              ExpectSuccess(Path(xt2, @len));
              ExpectEqual(grs(len), 'xtest-2.esp', '');
            end);
          It('Should resolve group signatures', procedure
            begin
              ExpectSuccess(Path(armo, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\ARMO', '');
            end);
          It('Should resolve block names', procedure
            begin
              ExpectSuccess(Path(block, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\CELL\Block 0', '');
            end);
          It('Should resolve sub-block names', procedure
            begin
              ExpectSuccess(Path(subBlock, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\CELL\Block 0\Sub-Block 0', '');
            end);
          It('Should resolve child groups', procedure
            begin
              ExpectSuccess(Path(childGroup, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00027D1C\Child Group', '');
            end);
          It('Should resolve temporary/persistent groups', procedure
            begin
              ExpectSuccess(Path(persistentGroup, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00027D1C\Child Group\Persistent', '');
            end);
          It('Should resolve record FormIDs', procedure
            begin
              ExpectSuccess(Path(refr, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\000170F0', '');
            end);
          It('Should resolve element names', procedure
            begin
              ExpectSuccess(Path(element, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00012E46\DNAM - Armor Rating', '');
            end);
          It('Should resolve array element indexes', procedure
            begin
              ExpectSuccess(Path(keyword, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00012E46\KWDA - Keywords\[1]', '');
            end);
        end);
        
      Describe('EditorID', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(EditorID(xt2, @len));
            end);
          It('Should fail if a group is passed', procedure
            begin
              ExpectFailure(EditorID(block, @len));
              ExpectFailure(EditorID(subBlock, @len));
              ExpectFailure(EditorID(childGroup, @len));
              ExpectFailure(EditorID(persistentGroup, @len));
              ExpectFailure(EditorID(armo, @len));
            end);
          It('Should fail if an element is passed', procedure
            begin
              ExpectFailure(EditorID(element, @len));
              ExpectFailure(EditorID(keyword, @len));
            end);
          It('Should return EditorID if a record is passed', procedure
            begin
              ExpectSuccess(EditorID(rec, @len));
              ExpectEqual(grs(len), 'ArmorIronGauntlets', '');
              ExpectSuccess(EditorID(refr, @len));
              ExpectEqual(grs(len), 'DA09PedestalEmptyRef', '');
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
              ExpectEqual(grs(len), 'GRUP', '');
              ExpectSuccess(Signature(subBlock, @len));
              ExpectEqual(grs(len), 'GRUP', '');
              ExpectSuccess(Signature(childGroup, @len));
              ExpectEqual(grs(len), 'GRUP', '');
              ExpectSuccess(Signature(persistentGroup, @len));
              ExpectEqual(grs(len), 'GRUP', '');
              ExpectSuccess(Signature(armo, @len));
              ExpectEqual(grs(len), 'ARMO', '');
            end);
          It('Should resolve record signatures', procedure
            begin
              ExpectSuccess(Signature(rec, @len));
              ExpectEqual(grs(len), 'ARMO', '');
              ExpectSuccess(Signature(refr, @len));
              ExpectEqual(grs(len), 'REFR', '');
            end);
          It('Should resolve element signatures', procedure
            begin
              ExpectSuccess(Signature(element, @len));
              ExpectEqual(grs(len), 'DNAM', '');
            end);
        end);
        
      Describe('FullName', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(FullName(xt2, @len));
            end);
          It('Should fail if a group is passed', procedure
            begin
              ExpectFailure(FullName(block, @len));
              ExpectFailure(FullName(subBlock, @len));
              ExpectFailure(FullName(childGroup, @len));
              ExpectFailure(FullName(persistentGroup, @len));
              ExpectFailure(FullName(armo, @len));
            end);
          It('Should fail if an element is passed', procedure
            begin
              ExpectFailure(FullName(element, @len));
              ExpectFailure(FullName(keyword, @len));
            end);
          It('Should fail if a record with no full name is passed', procedure
            begin
              ExpectFailure(FullName(refr, @len));
            end);
          It('Should return Full Name if a record is passed', procedure
            begin
              ExpectSuccess(FullName(rec, @len));
              ExpectEqual(grs(len), 'Iron Gauntlets', '');
            end);
        end);

      Describe('GetValue', procedure
        begin
          It('Should resolve element values', procedure
            begin
              ExpectSuccess(GetValue(element, '', @len));
              ExpectEqual(grs(len), '10.000000', '');
              ExpectSuccess(GetValue(keyword, '', @len));
              ExpectEqual(grs(len), 'ArmorHeavy [KYWD:0006BBD2]', '');
            end);
          It('Should resolve element value at path', procedure
            begin
              ExpectSuccess(GetValue(rec, 'OBND\X1', @len));
              ExpectEqual(grs(len), '-11', '');
              ExpectSuccess(GetValue(rec, 'KWDA\[1]', @len));
              ExpectEqual(grs(len), 'ArmorHeavy [KYWD:0006BBD2]', '');
              ExpectSuccess(GetValue(rec, 'Female world model\MOD4', @len));
              ExpectEqual(grs(len), 'Test', '');
            end);
          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetValue(rec, 'Non\Existent\Path', @len));
            end);
        end);

      Describe('GetIntValue', procedure
        begin
          It('Should resolve element integer values', procedure
            begin
              GetElement(rec, 'OBND\Y1', @h);
              ExpectSuccess(GetIntValue(h, '', @i));
              ExpectEqual(i, -15, '');
            end);
          It('Should resolve element integer values at paths', procedure
            begin
              ExpectSuccess(GetIntValue(rec, 'OBND\Z1', @i));
              ExpectEqual(i, -1, '');
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
              ExpectEqual(c, $6BBD2, '');
            end);
          It('Should resolve element unsigned integer values at paths', procedure
            begin
              ExpectSuccess(GetUIntValue(rec, 'KWDA\[0]', @c));
              ExpectEqual(c, $424EF, '');
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
              ExpectEqual(f, 1000.0, '');
            end);
          It('Should resolve element float values at paths', procedure
            begin
              ExpectSuccess(GetFloatValue(rec, 'DATA\Weight', @f));
              ExpectEqual(f, 5.0, '');
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
              ExpectSuccess(SetValue(element, '', '14.100000'));
              ExpectSuccess(GetValue(element, '', @len));
              ExpectEqual(grs(len), '14.100000', '');
              ExpectSuccess(SetValue(keyword, '', 'ArmorLight [KYWD:0006BBD3]'));
              ExpectSuccess(GetValue(keyword, '', @len));
              ExpectEqual(grs(len), 'ArmorLight [KYWD:0006BBD3]', '');
            end);
          It('Should set element value at path', procedure
            begin
              ExpectSuccess(SetValue(rec, 'OBND\X1', '-8'));
              ExpectSuccess(GetValue(rec, 'OBND\X1', @len));
              ExpectEqual(grs(len), '-8', '');
              ExpectSuccess(SetValue(rec, 'KWDA\[0]', 'PerkFistsEbony [KYWD:0002C178]'));
              ExpectSuccess(GetValue(rec, 'KWDA\[0]', @len));
              ExpectEqual(grs(len), 'PerkFistsEbony [KYWD:0002C178]', '');
              ExpectSuccess(SetValue(rec, 'Female world model\MOD4', 'Armor\Iron\F\GauntletsGND.nif'));
              ExpectSuccess(GetValue(rec, 'Female world model\MOD4', @len));
              ExpectEqual(grs(len), 'Armor\Iron\F\GauntletsGND.nif', '');
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
              ExpectEqual(i, -13, '');
            end);
          It('Should set element integer values at paths', procedure
            begin
              ExpectSuccess(SetIntValue(rec, 'OBND\Z1', -4));
              ExpectSuccess(GetIntValue(rec, 'OBND\Z1', @i));
              ExpectEqual(i, -4, '');
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
              ExpectSuccess(SetUIntValue(keyword, '', $6BBE2));
              ExpectSuccess(GetUIntValue(keyword, '', @c));
              ExpectEqual(c, $6BBE2, '');
            end);
          It('Should set element unsigned integer values at paths', procedure
            begin
              ExpectSuccess(SetUIntValue(rec, 'KWDA\[0]', $2C177));
              ExpectSuccess(GetUIntValue(rec, 'KWDA\[0]', @c));
              ExpectEqual(c, $2C177, '');
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
              ExpectEqual(f, 1920.0, '');
            end);
          It('Should resolve element float values at paths', procedure
            begin
              ExpectSuccess(SetFloatValue(rec, 'DATA\Weight', 7.3));
              ExpectSuccess(GetFloatValue(rec, 'DATA\Weight', @f));
              ExpectEqual(f, 7.3, '');
            end);
          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(SetFloatValue(rec, 'Non\Existent\Path', 1.23));
            end);
        end);
    end);
end;

end.
