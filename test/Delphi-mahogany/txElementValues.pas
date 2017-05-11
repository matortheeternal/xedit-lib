unit txElementValues;

interface

  // ELEMENT VALUE METHODS
  function Name(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Path(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function EditorID(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Signature(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function FullName(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SortKey(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function ElementType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function DefType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetValue(_id: Integer; path: PWideChar; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetValue(_id: Integer; path: PWideChar; value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetIntValue(_id: Integer; path: PWideChar; value: PInteger): WordBool; cdecl; external 'XEditLib.dll';
  function SetIntValue(_id: Integer; path: PWideChar; value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetUIntValue(_id: Integer; path: PWideChar; value: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetUIntValue(_id: Integer; path: PWideChar; value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFloatValue(_id: Integer; path: PWideChar; value: PDouble): WordBool; cdecl; external 'XEditLib.dll';
  function SetFloatValue(_id: Integer; path: PWideChar; value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Integer; path: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetFlag(_id: Integer; path: PWideChar; name: PWideChar; enabled: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetFlag(_id: Integer; path: PWideChar; name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function ToggleFlag(_id: Integer; path: PWideChar; name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnabledFlags(_id: Integer; path: PWideChar; flags: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildElementValueTests;

implementation

uses
  maMain,
  txMeta, txElements;

procedure BuildElementValueTests;
var
  testFile, block, subBlock, childGroup, persistentGroup, refr, armo, rec, 
  element, keyword, h, c: Cardinal;
  success: WordBool;
  expectedName: String;
  str: PWideChar;
  i: Integer;
begin
  Describe('Element Values', procedure
    begin
      BeforeAll(procedure
        begin
          GetElement(0, 'xtest-2.esp', @testFile);
          GetElement(testFile, 'ARMO', @armo);
          GetElement(armo, '00012E46', @rec);
          GetElement(rec, 'DNAM', @element);
          GetElement(rec, 'KWDA\[1]', @keyword);
          GetElement(testFile, '00027D1C\Child Group', @childGroup);
          GetElement(testFile, 'CELL\[0]', @block);
          GetElement(block, '[0]', @subBlock);
          GetElement(childGroup, '[0]', @persistentGroup);
          GetElement(testFile, '000170F0', @refr);
          GetMem(str, 4096);
        end);
        
      AfterAll(procedure
        begin
          FreeMem(str, 4096);
        end);
        
      Describe('Name', procedure
        begin
          It('Should resolve file names', procedure
            begin
              ExpectSuccess(Name(testFile, str, 256));
              ExpectEqual(String(str), 'xtest-2.esp', '');
            end);
          Describe('Group names', procedure
            begin
              It('Should resolve top level group names', procedure
                begin
                  ExpectSuccess(Name(armo, str, 256));
                  ExpectEqual(String(str), 'Armor', '');
                end);
              It('Should resolve block names', procedure
                begin
                  ExpectSuccess(Name(block, str, 256));
                  ExpectEqual(String(str), 'Block 0', '');
                end);
              It('Should resolve sub-block names', procedure
                begin
                  ExpectSuccess(Name(subBlock, str, 256));
                  ExpectEqual(String(str), 'Sub-Block 0', '');
                end);
              It('Should resolve child group names', procedure
                begin
                  ExpectSuccess(Name(childGroup, str, 256));
                  expectedName := 'Children of 00027D1C';
                  ExpectEqual(String(str), expectedName, '');
                end);
              It('Should resolve persistent/temporary group names', procedure
                begin
                  ExpectSuccess(Name(persistentGroup, str, 256));
                  expectedName := 'Persistent';
                  ExpectEqual(String(str), expectedName, '');
                end);
            end);
          Describe('Record names', procedure
            begin
              It('Should resolve FULL name, if present', procedure
                begin
                  ExpectSuccess(Name(rec, str, 256));
                  ExpectEqual(String(str), 'Iron Gauntlets', '');
                end);
              It('Should resolve BASE name, if present', procedure
                begin
                  ExpectSuccess(Name(refr, str, 256));
                  expectedName := 'DA09PedestalEmpty "Pedestal" [ACTI:0007F82A]';
                  ExpectEqual(String(str), expectedName, '');
                end);
            end);
          It('Should resolve element names', procedure
            begin
              ExpectSuccess(Name(element, str, 256));
              ExpectEqual(String(str), 'DNAM - Armor Rating', '');
            end);
        end);

      Describe('Path', procedure
        begin
          It('Should resolve file names', procedure
            begin
              ExpectSuccess(Path(testFile, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp', '');
            end);
          It('Should resolve group signatures', procedure
            begin
              ExpectSuccess(Path(armo, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\ARMO', '');
            end);
          It('Should resolve block names', procedure
            begin
              ExpectSuccess(Path(block, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\CELL\Block 0', '');
            end);
          It('Should resolve sub-block names', procedure
            begin
              ExpectSuccess(Path(subBlock, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\CELL\Block 0\Sub-Block 0', '');
            end);
          It('Should resolve child groups', procedure
            begin
              ExpectSuccess(Path(childGroup, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\00027D1C\Child Group', '');
            end);
          It('Should resolve temporary/persistent groups', procedure
            begin
              ExpectSuccess(Path(persistentGroup, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\00027D1C\Child Group\Persistent', '');
            end);
          It('Should resolve record FormIDs', procedure
            begin
              ExpectSuccess(Path(refr, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\000170F0', '');
            end);
          It('Should resolve element names', procedure
            begin
              ExpectSuccess(Path(element, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\00012E46\DNAM - Armor Rating', '');
            end);
          It('Should resolve array element indexes', procedure
            begin
              ExpectSuccess(Path(keyword, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\00012E46\KWDA - Keywords\[1]', '');
            end);
        end);
        
      Describe('EditorID', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(EditorID(testFile, str, 256));
            end);
          It('Should fail if a group is passed', procedure
            begin
              ExpectFailure(EditorID(block, str, 256));
              ExpectFailure(EditorID(subBlock, str, 256));
              ExpectFailure(EditorID(childGroup, str, 256));
              ExpectFailure(EditorID(persistentGroup, str, 256));
              ExpectFailure(EditorID(armo, str, 256));
            end);
          It('Should fail if an element is passed', procedure
            begin
              ExpectFailure(EditorID(element, str, 256));
              ExpectFailure(EditorID(keyword, str, 256));
            end);
          It('Should return EditorID if a record is passed', procedure
            begin
              ExpectSuccess(EditorID(rec, str, 256));
              ExpectEqual(String(str), 'ArmorIronGauntlets', '');
              ExpectSuccess(EditorID(refr, str, 256));
              ExpectEqual(String(str), 'DA09PedestalEmptyRef', '');
            end);
        end);
        
      Describe('Signature', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(Signature(testFile, str, 256));
            end);
          It('Should fail if an element with no signature is passed', procedure
            begin
              ExpectFailure(Signature(keyword, str, 256));
            end);
          It('Should resolve group signatures', procedure
            begin
              ExpectSuccess(Signature(block, str, 256));
              ExpectEqual(String(str), 'GRUP', '');
              ExpectSuccess(Signature(subBlock, str, 256));
              ExpectEqual(String(str), 'GRUP', '');
              ExpectSuccess(Signature(childGroup, str, 256));
              ExpectEqual(String(str), 'GRUP', '');
              ExpectSuccess(Signature(persistentGroup, str, 256));
              ExpectEqual(String(str), 'GRUP', '');
              ExpectSuccess(Signature(armo, str, 256));
              ExpectEqual(String(str), 'ARMO', '');
            end);
          It('Should resolve record signatures', procedure
            begin
              ExpectSuccess(Signature(rec, str, 256));
              ExpectEqual(String(str), 'ARMO', '');
              ExpectSuccess(Signature(refr, str, 256));
              ExpectEqual(String(str), 'REFR', '');
            end);
          It('Should resolve element signatures', procedure
            begin
              ExpectSuccess(Signature(element, str, 256));
              ExpectEqual(String(str), 'DNAM', '');
            end);
        end);
        
      Describe('FullName', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(FullName(testFile, str, 256));
            end);
          It('Should fail if a group is passed', procedure
            begin
              ExpectFailure(FullName(block, str, 256));
              ExpectFailure(FullName(subBlock, str, 256));
              ExpectFailure(FullName(childGroup, str, 256));
              ExpectFailure(FullName(persistentGroup, str, 256));
              ExpectFailure(FullName(armo, str, 256));
            end);
          It('Should fail if an element is passed', procedure
            begin
              ExpectFailure(FullName(element, str, 256));
              ExpectFailure(FullName(keyword, str, 256));
            end);
          It('Should fail if a record with no full name is passed', procedure
            begin
              ExpectFailure(FullName(refr, str, 256));
            end);
          It('Should return Full Name if a record is passed', procedure
            begin
              ExpectSuccess(FullName(rec, str, 256));
              ExpectEqual(String(str), 'Iron Gauntlets', '');
            end);
        end);

      Describe('GetValue', procedure
        begin
          It('Should resolve element values', procedure
            begin
              ExpectSuccess(GetValue(element, '', str, 256));
              ExpectEqual(String(str), '10.000000', '');
              ExpectSuccess(GetValue(keyword, '', str, 256));
              ExpectEqual(String(str), 'ArmorHeavy [KYWD:0006BBD2]', '');
            end);
          It('Should resolve element value at path', procedure
            begin
              ExpectSuccess(GetValue(rec, 'OBND\X1', str, 256));
              ExpectEqual(String(str), '-11', '');
              ExpectSuccess(GetValue(rec, 'KWDA\[1]', str, 256));
              ExpectEqual(String(str), 'ArmorHeavy [KYWD:0006BBD2]', '');
              ExpectSuccess(GetValue(rec, 'Female world model\MOD4', str, 256));
              ExpectEqual(String(str), 'Test', '');
            end);
          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetValue(rec, 'Non\Existent\Path', str, 256));
            end);
        end);

      Describe('GetIntValue', procedure
        begin
          It('Should resolve element integer values', procedure
            begin
              GetElement(rec, 'OBND\Y1', @h);
              ExpectSuccess(GetIntValue(h, '', @i));
              ExpectEqual(i, -6, '');
            end);
          It('Should resolve element integer values at paths', procedure
            begin
              ExpectSuccess(GetIntValue(rec, 'OBND\Z1', @i));
              ExpectEqual(i, -9, '');
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
          It('Should resolve element integer values at paths', procedure
            begin
              ExpectSuccess(GetUIntValue(rec, 'KWDA\[0]', @c));
              ExpectEqual(c, $0, '');
            end);
          It('Should fail if path does not exist', procedure
            begin
              ExpectFailure(GetUIntValue(rec, 'Non\Existent\Path', @c));
            end);
        end);
    end);
end;

end.
