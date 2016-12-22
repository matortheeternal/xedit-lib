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

procedure TestGetElement;
var
  f, g, r, h: Cardinal;
begin
  WriteLn('=== GetElement Tests ===');
  // Test GetElement resolving a file by index
  GetElement(0, '[0]', @f);
  WriteLn('Resolved [0] with handle: ' + IntToStr(f));
  // Test GetElement resolving a file by name
  GetElement(0, 'Skyrim.esm', @f);
  WriteLn('Resolved Skyrim.esm with handle: ' + IntToStr(f));

  // Test GetElement resolving file header from file
  GetElement(f, '[0]', @h);
  WriteLn('Skyrim.esm - Resolved [0] with handle: ' + IntToStr(h));
  // Test GetElement resolving groups from file
  GetElement(f, 'WRLD', @g);
  WriteLn('Skyrim.esm - Resolved WRLD with handle: ' + IntToStr(g));
  GetElement(f, 'ARMO', @g);
  WriteLn('Skyrim.esm - Resolved ARMO with handle: ' + IntToStr(g));

  // Test GetElement resolving records from group
  GetElement(g, '[0]', @r);
  WriteLn('Skyrim.esm\ARMO - Resolved [0] with handle: ' + IntToStr(r));
  GetElement(g, '[1]', @r);
  WriteLn('Skyrim.esm\ARMO - Resolved [1] with handle: ' + IntToStr(r));

  // Test GetElement resolving top level fields from a record via signature
  GetElement(r, 'FULL', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved FULL with handle: ' + IntToStr(h));
  // Test GetElement resolving top level fields from a record via name
  GetElement(r, 'Male world model', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved Male world model with handle: ' + IntToStr(h));

  // Test GetElement resolving nested fields from a record with indexes
  GetElement(r, 'KWDA\[1]', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved KWDA\[1] with handle: ' + IntToStr(h));
  GetElement(r, 'BODT\[0]', @h);
  WriteLn('Skyrim.esm\ARMO\[1] - Resolved BODT\[0] with handle: ' + IntToStr(h));

  // Test GetElement resolving a record from a file by FormID
  GetElement(f, 'ARMO\00012E46', @r);
  WriteLn('Skyrim.esm - Resolved ARMO\00012E46 with handle: ' + IntToStr(r));
  // Test GetElement resolving a record from a file by index
  GetElement(f, 'ARMO\[2]', @r);
  WriteLn('Skyrim.esm - Resolved ARMO\[2] with handle: ' + IntToStr(r));
  // Test GetElement resolving an element from a file
  GetElement(f, 'ARMO\00012E46\KWDA\[0]', @h);
  WriteLn('Skyrim.esm - Resolved ARMO\00012E46\KWDA\[0] with handle: ' + IntToStr(h));

  // Test GetElement resolving a full path
  GetElement(0, 'Skyrim.esm\ARMO\00013938\DATA\Value', @h);
  WriteLn('Resolved Skyrim.esm\ARMO\00013938\DATA\Value with handle: ' + IntToStr(h));
  // Test GetElement resolving a full path of indexes
  GetElement(0, '[0]\[1]\[2]\[1]', @h);
  WriteLn('Resolved [0]\[1]\[2]\[1] with handle: ' + IntToStr(h));
  WriteLn(' ');
end;

procedure TestGetElements;
var
  f, g, r: Cardinal;
  a: CardinalArray;
begin
  WriteLn('=== GetElements Tests ===');
  f := FileByName('Skyrim.esm');
  GetElement(f, 'ARMO', @g);
  GetElement(g, '00012E46', @r);

  // test getting elements from a file
  GetElements(f, @a);
  WriteLn('Resolved ' + IntToStr(High(a) + 1) + ' element handles from Skyrim.esm:');
  WriteArray(a);

  // test getting elements from a group
  GetElements(g, @a);
  WriteLn('Resolved ' + IntToStr(High(a) + 1) + ' element handles from the Skyrim.esm\ARMO:');
  WriteArray(a);

  // test getting elements from a group
  GetElements(r, @a);
  WriteLn('Resolved ' + IntToStr(High(a) + 1) + ' element handles from the Skyrim.esm\ARMO\00012E46:');
  WriteArray(a);
  WriteLn(' ');
end;

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
          end);
    end);
end;

end.
