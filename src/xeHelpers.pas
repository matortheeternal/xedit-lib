unit xeHelpers;

interface

uses
  Windows, SysUtils, Classes,
  // xedit units
  wbInterface;

  function StrEndsWith(str, substr: String): Boolean;
  function GetCSIDLShellFolder(CSIDLFolder: integer): string;
  function TryRegistryKeys(var keys: TStringList): string;
  function GetVersionMem: string;
  function FileNameValid(filename: string): boolean;
  function StrArrayJoin(ary: TDynStrings; const separator: String): String;

implementation

uses
  StrUtils, IOUtils, ShellApi, shlObj, Registry;

function StrEndsWith(str, substr: String): Boolean;
var
  len, subLen: Integer;
begin
  Result := False;
  len := Length(str);
  subLen := Length(substr);
  if (len < subLen) then exit;
  Result :=  Copy(str, len - subLen + 1, subLen) = substr;
end;

function StrArrayJoin(ary: TDynStrings; const separator: String): String;
var
  len, i: Integer;
begin
  Result := '';
  len := Length(ary);
  if len > 0 then
    Result := ary[0];
  for i := 1 to Pred(len) do
    Result := Result + #13#10 + ary[i];
end;

{
  GetCSIDLShellFolder:
  Gets a folder by its integer CSID.
}
function GetCSIDLShellFolder(CSIDLFolder: integer): string;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(0, PChar(Result), CSIDLFolder, True);
  SetLength(Result, StrLen(PChar(Result)));
  if (Result <> '') then
    Result := IncludeTrailingBackslash(Result);
end;

{
  TryRegistryKeys:
  Tries to load various registry keys.
}
function TryRegistryKeys(var keys: TStringList): string;
var
  i: Integer;
  path, name: string;
begin
  Result := '';
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;

    // try all keys
    for i := 0 to Pred(keys.Count) do begin
      path := ExtractFilePath(keys[i]);
      name := ExtractFileName(keys[i]);
      if OpenKeyReadOnly(path) then begin
        Result := ReadString(name);
        break;
      end;
    end;
  finally
    Free;
  end;
end;

{ Get program version from memory }
function GetVersionMem: string;
var
  verblock: PVSFIXEDFILEINFO;
  versionMS, versionLS, verlen: cardinal;
  rs: TResourceStream;
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    try
      rs := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
      try
        m.CopyFrom(rs, rs.Size);
      finally
        rs.Free;
      end;
      m.Position := 0;
      if VerQueryValue(m.Memory, '\', Pointer(verblock), verlen) then begin
        VersionMS := verblock.dwFileVersionMS;
        VersionLS := verblock.dwFileVersionLS;
        Result := Format('%s.%s.%s.%s', [IntToStr(versionMS shr 16),
          IntToStr(versionMS and $FFFF), IntToStr(VersionLS shr 16),
          IntToStr(VersionLS and $FFFF)]);
      end;
    except
      on x: Exception do
        Result := '0.0.0.0';
    end;
  finally
    m.Free;
  end;
end;

{
  FileNameValid:
  Returns true if the input filename is valid.
}
function FileNameValid(filename: string): boolean;
begin
  Result := (Length(Trim(filename)) > 0) and
    TPath.HasValidFileNameChars(filename, false);
end;

end.
