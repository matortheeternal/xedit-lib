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

procedure TestSignatureFromName(name, sig: String);
var
  len: Integer;
begin
  ExpectSuccess(SignatureFromName(PWideChar(name), @len));
  ExpectEqual(grs(len), sig);
end;

procedure TestNameFromSignature(sig, name: String);
var
  len: Integer;
begin
  ExpectSuccess(NameFromSignature(PWideChar(sig), @len));
  ExpectEqual(grs(len), name);
end;

procedure BuildElementValueTests;
var
  xt2, block, subBlock, childGroup, persistentGroup, refr, armo, rec,
  element, keyword, h, c: Cardinal;
  expectedName, str: String;
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
                  ExpectEqual(grs(len), 'DA09PedestalEmptyRef');
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

      Describe('Path', procedure
        begin
          It('Should resolve file names', procedure
            begin
              ExpectSuccess(Path(xt2, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp');
            end);
          It('Should resolve group signatures', procedure
            begin
              ExpectSuccess(Path(armo, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\ARMO');
            end);
          It('Should resolve block names', procedure
            begin
              ExpectSuccess(Path(block, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\CELL\Block 0');
            end);
          It('Should resolve sub-block names', procedure
            begin
              ExpectSuccess(Path(subBlock, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\CELL\Block 0\Sub-Block 0');
            end);
          It('Should resolve child groups', procedure
            begin
              ExpectSuccess(Path(childGroup, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00027D1C\Child Group');
            end);
          It('Should resolve temporary/persistent groups', procedure
            begin
              ExpectSuccess(Path(persistentGroup, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00027D1C\Child Group\Persistent');
            end);
          It('Should resolve record FormIDs', procedure
            begin
              ExpectSuccess(Path(refr, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\000170F0');
            end);
          It('Should resolve element names', procedure
            begin
              ExpectSuccess(Path(element, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00012E46\DNAM - Armor Rating');
            end);
          It('Should resolve array element indexes', procedure
            begin
              ExpectSuccess(Path(keyword, true, @len));
              ExpectEqual(grs(len), 'xtest-2.esp\00012E46\KWDA - Keywords\[1]');
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
              ExpectSuccess(SetValue(element, '', '14.100000'));
              ExpectSuccess(GetValue(element, '', @len));
              ExpectEqual(grs(len), '14.100000');
              ExpectSuccess(SetValue(keyword, '', 'ArmorLight [KYWD:0006BBD3]'));
              ExpectSuccess(GetValue(keyword, '', @len));
              ExpectEqual(grs(len), 'ArmorLight [KYWD:0006BBD3]');
            end);
          It('Should set element value at path', procedure
            begin
              ExpectSuccess(SetValue(rec, 'OBND\X1', '-8'));
              ExpectSuccess(GetValue(rec, 'OBND\X1', @len));
              ExpectEqual(grs(len), '-8');
              ExpectSuccess(SetValue(rec, 'KWDA\[0]', 'PerkFistsEbony [KYWD:0002C178]'));
              ExpectSuccess(GetValue(rec, 'KWDA\[0]', @len));
              ExpectEqual(grs(len), 'PerkFistsEbony [KYWD:0002C178]');
              ExpectSuccess(SetValue(rec, 'Female world model\MOD4', 'Armor\Iron\F\GauntletsGND.nif'));
              ExpectSuccess(GetValue(rec, 'Female world model\MOD4', @len));
              ExpectEqual(grs(len), 'Armor\Iron\F\GauntletsGND.nif');
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
              ExpectSuccess(SetUIntValue(keyword, '', $6BBE2));
              ExpectSuccess(GetUIntValue(keyword, '', @c));
              ExpectEqual(c, $6BBE2);
            end);
          It('Should set element unsigned integer values at paths', procedure
            begin
              ExpectSuccess(SetUIntValue(rec, 'KWDA\[0]', $2C177));
              ExpectSuccess(GetUIntValue(rec, 'KWDA\[0]', @c));
              ExpectEqual(c, $2C177);
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
              Expect(Pos('NPC_=Non-Player Character (Actor)', str) > 0,
                'Should contain NPC_=Non-Player Character (Actor)');
            end);
        end);
    end);
end;

end.
