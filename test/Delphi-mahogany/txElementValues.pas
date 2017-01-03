unit txElementValues;

interface

  // ELEMENT VALUE METHODS
  function Name(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Path(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function EditorID(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Signature(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function ShortName(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SortKey(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function ElementType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function DefType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetValue(_id: Integer; path, str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetValue(_id: Integer; path, value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetIntValue(_id: Integer; path: PWideChar; out value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetIntValue(_id: Integer; path: PWideChar; value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetUIntValue(_id: Integer; path: PWideChar; out value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetUIntValue(_id: Integer; path: PWideChar; value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFloatValue(_id: Integer; path: PWideChar; out value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function SetFloatValue(_id: Integer; path: PWideChar; value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Integer; path: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetFlag(_id: Integer; path, name: PWideChar; enabled: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetFlag(_id: Integer; path, name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function ToggleFlag(_id: Integer; path, name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnabledFlags(_id: Integer; path: PWideChar; out flags: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildElementValueTests;

implementation

uses
  maMain,
  txMeta, txElements;

procedure BuildElementValueTests;
var
  testFile, block, subBlock, childGroup, persistentGroup, refr, armo, rec, 
  element, keyword: Cardinal;
  success: WordBool;
  expectedName: String;
  str: PWideChar;
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
              ExpectEqual(String(str), 'xtest-2.esp\00027D1C\Children', '');
            end);
          It('Should resolve temporary/persistent groups', procedure
            begin
              ExpectSuccess(Path(persistentGroup, str, 1024));
              ExpectEqual(String(str), 'xtest-2.esp\00027D1C\Children\Persistent', '');
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
    end);
end;

end.
