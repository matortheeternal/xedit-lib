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
  procedure WriteExceptions;
  procedure ExpectSuccess(b: WordBool);
  procedure ExpectFailure(b: WordBool);
  procedure BuildMetaTests;

implementation

uses
  txFiles,
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

{$POINTERMATH ON}
function ArrayLength(a: PCardinal): Integer;
begin
  Result := 0;
  while a^ <> 0 do begin
    Inc(Result, 1);
    Inc(a, 4);
  end;
end;
{$POINTERMATH OFF}

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

procedure WriteExceptions;
var
  str: PWideChar;
begin
  GetMem(str, 4096);
  GetExceptionMessage(str, 4096);
  if Length(string(str)) > 0 then
    WriteLn(str);
end;

procedure ExpectSuccess(b: WordBool);
begin
  if not b then WriteExceptions;
  Expect(b, 'Function should return true');
end;

procedure ExpectFailure(b: WordBool);
begin
  Expect(not b, 'Function should return false');
end;

procedure BuildMetaTests;
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
              FileByName('Skyrim.esm', @h1);
              success := Release(h1);
              Expect(success, 'Result should be true');
              FileByName('Skyrim.esm', @h2);
              Expect(h1 = h2, 'Next allocation should use the freed handle');
            end);
        end);

      Describe('ResetStore', procedure
        begin
          It('Should clear all handles', procedure
            begin
              success := ResetStore;
              Expect(success, 'Result should be true');
              FileByName('Skyrim.esm', @h1);
              Expect(h1 = 1, 'First handle allocated after resetting store should be 1');
            end);
        end);
    end);
end;

end.
