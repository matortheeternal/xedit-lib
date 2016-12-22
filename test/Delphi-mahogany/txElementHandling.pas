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
begin
  Describe('Element Handling', procedure
    begin
      Describe('GetElement', procedure
        begin
          BeforeAll(procedure
            begin
              GetElement(0, 'Skyrim.esm', @skyrim);
              GetElement(skyrim, 'ARMO', @armo);
              GetElement(armo, '00012E46', @rec);
            end);

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
        end);
    end);
end;

end.
