unit txMeta;

interface

uses
  SysUtils;

type
  CardinalArray = array of Cardinal;
  PCardinalArray = ^CardinalArray;

  // META METHODS
  procedure Initialize; cdecl; external 'XEditLib.dll';
  procedure Finalize; cdecl; external 'XEditLib.dll';
  procedure GetBuffer(str: PWideChar; len: Integer); cdecl; external 'XEditLib.dll';
  procedure FlushBuffer; cdecl; external 'XEditLib.dll';
  function GetExceptionMessage(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGlobal(key, value: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Release(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function ResetStore: WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure WriteBuffer;
  procedure WriteArray(a: CardinalArray);
  procedure TestMeta;

implementation

uses
  txFileHandling,
  maMain;

procedure WriteBuffer;
var
  str: PWideChar;
begin
  GetMem(str, 4096);
  GetBuffer(str, 4096);
  if Length(string(str)) > 0 then begin
    FlushBuffer();
    WriteLn(str);
  end;
end;

procedure WriteArray(a: CardinalArray);
var
  s: String;
  i: Integer;
begin
  s := '  [ ';
  for i := Low(a) to High(a) do begin
    s := s + IntToStr(a[i]);
    if i < High(a) then
      s := s + ', ';
  end;
  s := s + ' ]';
  WriteLn(s);
end;

procedure TestMeta;
var
  str: PWideChar;
  success: WordBool;
  h1, h2: Cardinal;
begin
  Describe('Meta Methods', procedure
    begin
      Describe('GetGlobal', procedure
        begin
          BeforeEach(procedure
            begin
              GetMem(str, 512);
            end);

          AfterEach(procedure
            begin
              FreeMem(str, 512);
            end);

          It('Should have the ProgramPath global', procedure
            begin
              GetGlobal(PWideChar('ProgramPath'), str, 512);
              Expect(Length(str) > 0, 'Should return a string');
            end);

          It('Should have the Version global', procedure
            begin
              GetGlobal(PWideChar('Version'), str, 512);
              Expect(Length(str) > 0, 'Should return a string');
            end);

          It('Should have the GameName global', procedure
            begin
              GetGlobal(PWideChar('GameName'), str, 512);
              Expect(Length(str) > 0, 'Should return a string');
            end);

          It('Should have the AppName global', procedure
            begin
              GetGlobal(PWideChar('AppName'), str, 512);
              Expect(Length(str) > 0, 'Should return a string');
            end);

          It('Should have the LongGameName global', procedure
            begin
              GetGlobal(PWideChar('LongGameName'), str, 512);
              Expect(Length(str) > 0, 'Should return a string');
            end);

          It('Should have the DataPath global', procedure
            begin
              GetGlobal(PWideChar('DataPath'), str, 512);
              Expect(Length(str) > 0, 'Should return a string');
            end);

          It('Should fail if global does not exist', procedure
            begin
              success := GetGlobal(PWideChar('DoesNotExist'), str, 512);
              Expect(not success, 'Result should be false');
            end);
        end);

      Describe('Release', procedure
        begin
          It('Should fail if handle is not allocated', procedure
            begin
              success := Release(100);
              Expect(not success, 'Result should be false');
            end);

          It('Should fail if null handle is passed', procedure
            begin
              success := Release(0);
              Expect(not success, 'Result should be false');
            end);

          It('Should free an allocated handle', procedure
            begin
              h1 := FileByName('Skyrim.esm');
              success := Release(h1);
              Expect(success, 'Result should be true');
              h2 := FileByName('Skyrim.esm');
              Expect(h1 = h2, 'Next allocation should use the freed handle');
            end);
        end);
end;

end.
