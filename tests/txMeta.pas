unit txMeta;

interface

uses
  classes,
  Windows, SysUtils;

type
  CardinalArray = array of Cardinal;
  PCardinalArray = ^CardinalArray;
  TStringArray = array of String;

  // PUBLIC TESTING INTERFACE
  procedure WriteMessages;
  procedure WriteArray(a: CardinalArray);
  procedure WriteExceptions;
  procedure WriteStringToFile(str, filename: string);
  procedure ExpectSuccess(b: WordBool);
  procedure ExpectFailure(b: WordBool);
  function GetDataPath: String;
  procedure BuildMetaTests;
  procedure BuildFinalTests;
  function grs(len: Integer): WideString;
  function gem(len: Integer): WideString;
  function gra(len: Integer): CardinalArray;

implementation

uses
  Mahogany,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeMeta, xeFiles, xeMessages;
{$ENDIF}

procedure WriteMessages;
var
  len: Integer;
  str: WideString;
  wcBuffer: PWideChar;
begin
  GetMessagesLength(@len);
  if len > 0 then begin
    SetLength(str, len + 1);
    wcBuffer := PWideChar(str);
    GetMessages(wcBuffer, len);
    Delete(str, Length(str) - 1, 2);
    WriteLn('> ' + StringReplace(str, #13#10, #13#10'> ', [rfReplaceAll]));
  end;
end;

// gem = Get Exception Message
function gem(len: Integer): WideString;
var
  wcBuffer: PWideChar;
begin
  if len = 0 then begin
    Result := '';
    exit;
  end;
  SetLength(Result, len);
  wcBuffer := PWideChar(Result);
  ExpectSuccess(GetExceptionMessage(wcBuffer, len));
end;

// grs = Get Result String
function grs(len: Integer): WideString;
var
  wcBuffer: PWideChar;
begin
  if len = 0 then begin
    Result := '';
    exit;
  end;
  SetLength(Result, len);
  wcBuffer := PWideChar(Result);
  ExpectSuccess(GetResultString(wcBuffer, len));
end;

function gra(len: Integer): CardinalArray;
var
  cBuffer: PCardinal;
begin
  if len = 0 then begin
    SetLength(Result, 0);
    exit;
  end;
  SetLength(Result, len);
  cBuffer := PCardinal(Result);
  ExpectSuccess(GetResultArray(cBuffer, len));
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
  WriteLn('> ' + s);
end;

procedure WriteExceptions;
var
  len: Integer;
  str: WideString;
  wcBuffer: PWideChar;
begin
  GetExceptionMessageLength(@len);
  if len > 0 then begin
    SetLength(str, len);
    wcBuffer := PWideChar(str);
    GetExceptionMessage(wcBuffer, len);
    WriteLn('> ' + str);
  end;
end;

procedure WriteStringToFile(str, filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := str;
    sl.SaveToFile(filename);
  finally
    sl.Free;
  end;
end;

procedure ExpectSuccess(b: WordBool);
begin
  if not b then WriteExceptions;
  Expect(b, 'Function should return true');
end;

procedure ExpectFailure(b: WordBool);
begin
  Expect(not b, 'Function should return false');
  ClearMessages;
end;

procedure TestGetGlobal(global: PWideChar);
var
  len: Integer;
begin
  ExpectSuccess(GetGlobal(global, @len));
  Expect(Length(grs(len)) > 0, 'Should return a string');
end;

function GetDataPath: String;
var
  len: Integer;
begin
  ExpectSuccess(GetGlobal('DataPath', @len));
  Result := grs(len);
end;

function GetBackupPath(fileName: String): String;
var
  BackupFolder: String;
  sr: TSearchRec;
  aTime: TDateTime;
begin
  Result := '';
  aTime := 0;
  BackupFolder := GetDataPath + 'zEdit Backups\';
  if FindFirst(BackupFolder + '*.bak', faAnyFile, sr) = 0 then begin
    repeat
      if (Pos(fileName, sr.Name) = 1) and (sr.TimeStamp > aTime) then begin
        aTime := sr.TimeStamp;
        Result := BackupFolder + sr.Name;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TestRename(const fileName: String);
var
  filePath: String;
begin
  filePath := GetDataPath + fileName;
  Expect(not FileExists(filePath + '.save'), fileName + '.save should no longer be present');
  Expect(FileExists(filePath), fileName + ' should exist');
end;

procedure BuildMetaTests;
var
  h1, h2: Cardinal;
  len: Integer;
begin
  Describe('Meta Methods', procedure
    begin
      Describe('GetGlobal', procedure
        begin
          It('Should have the ProgramPath global', procedure
            begin
              TestGetGlobal('ProgramPath');
            end);

          It('Should have the Version global', procedure
            begin
              TestGetGlobal('Version');
            end);

          It('Should have the GameName global', procedure
            begin
              TestGetGlobal('GameName');
            end);

          It('Should have the AppName global', procedure
            begin
              TestGetGlobal('AppName');
            end);

          It('Should have the LongGameName global', procedure
            begin
              TestGetGlobal('LongGameName');
            end);

          It('Should have the DataPath global', procedure
            begin
              TestGetGlobal('DataPath');
            end);

          It('Should have the AppDataPath global', procedure
            begin
              TestGetGlobal('AppDataPath');
            end);

          It('Should have the MyGamesPath global', procedure
            begin
              TestGetGlobal('MyGamesPath');
            end);

          It('Should have the GameIniPath global', procedure
            begin
              TestGetGlobal('GameIniPath');
            end);

          It('Should have the FileCount global', procedure
            begin
              TestGetGlobal('FileCount');
            end);

          It('Should fail if global does not exist', procedure
            begin
              ExpectFailure(GetGlobal('DoesNotExist', @len));
            end);

          It('Should be fast', procedure
            begin
              Benchmark(100000, procedure
                begin
                  GetGlobal('ProgramPath', @len);
                end);
            end);
        end);

      Describe('SetSortMode', procedure
        begin
          It('Should succeed if sort mode is valid', procedure
            begin
              ExpectSuccess(SetSortMode(3, true));
              ExpectSuccess(SetSortMode(2, false));
              ExpectSuccess(SetSortMode(1, true));
              ExpectSuccess(SetSortMode(0, false));
            end);

          It('Should fail if sort mode is invalid', procedure
            begin
              ExpectFailure(SetSortMode(4, false));
            end);
        end);

      Describe('Release', procedure
        begin
          It('Should fail if handle is not allocated', procedure
            begin
              ExpectFailure(Release(100));
            end);

          It('Should fail if null handle is passed', procedure
            begin
              ExpectFailure(Release(0));
            end);

          It('Should free an allocated handle', procedure
            begin
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(Release(h1));
              ExpectSuccess(FileByName('Skyrim.esm', @h2));
              ExpectSuccess(Release(h2));
              Expect(h1 = h2, 'Next allocation should use the freed handle');
            end);
        end);

      Describe('GetDuplicateHandles', procedure
        begin
          BeforeAll(procedure
            begin
              ExpectSuccess(ResetStore);
            end);

          It('Should fail if handle is not allocated', procedure
            begin
              ExpectFailure(GetDuplicateHandles(100, @len));
            end);

          It('Should return an empty array if there are no duplicates', procedure
            begin
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(GetDuplicateHandles(h1, @len));
              ExpectSuccess(Release(h1));
              ExpectEqual(len, 0);
            end);

          It('Should return duplicates', procedure
            begin
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              ExpectSuccess(FileByName('Update.esm', @h2));
              ExpectSuccess(GetDuplicateHandles(h1, @len));
              ExpectEqual(len, 4);
            end);
        end);

      Describe('ResetStore', procedure
        begin
          It('Should clear all handles', procedure
            begin
              ExpectSuccess(ResetStore);
              ExpectSuccess(FileByName('Skyrim.esm', @h1));
              Expect(h1 = 1, 'First handle allocated after resetting store should be 1');
            end);
        end);
    end);
end;

procedure BuildFinalTests;
begin
  Describe('CloseXEdit', procedure
    begin
      BeforeAll(procedure
        begin
          CloseXEdit;
        end);

      {$IFDEF SKYRIM}
      AfterAll(procedure
        begin
          DeleteFile(GetDataPath + 'xtest-6.esp');
          //DeleteFile(GetDataPath + 'xtest-5.esp');
          //SysUtils.RenameFile(GetBackupPath('xtest-5.esp'), GetDataPath + 'xtext-5.esp');
        end);

      It('Should rename .save files', procedure
        begin
          TestRename('xtest-6.esp');
          TestRename('xtest-5.esp');
        end);

      It('Should create backups', procedure
        begin
          Expect(GetBackupPath('xtest-5.esp') <> '');
        end);
      {$ENDIF}
    end);
end;

end.
