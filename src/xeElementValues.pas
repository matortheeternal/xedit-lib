unit xeElementValues;

interface

uses
  wbInterface,
  xeTypes;

  {$region 'Native functions'}
  function GetPath(const element: IwbElement; short: WordBool = False; local: WordBool = False; curPath: String = ''): String;
  function GetPathName(const element: IwbElement): String;
  function NativeName(const e: IwbElement; quoteFull: Boolean = False): String;
  function ParseFormIDValue(const value: String; var formID: Int64): Boolean;
  procedure SetElementValue(const element: IwbElement; const value: String);
  function IndexOfFlag(const flagsDef: IwbFlagsDef; const name: String): Integer;
  procedure NativeSetFlag(const element: IwbElement; index: Integer; enabled: WordBool);
  function NativeSignatureFromName(const name: String): String;
  function NativeNameFromSignature(const sig: String): String;
  procedure BuildSignatureNameMap;
  {$endregion}

  {$region 'API functions'}
  function Name(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function LongName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function DisplayName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function Path(_id: Cardinal; short, local: WordBool; len: PInteger): WordBool; cdecl;
  function Signature(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetValue(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function SetValue(_id: Cardinal; path, value: PWideChar): WordBool; cdecl;
  function GetIntValue(_id: Cardinal; path: PWideChar; value: PInteger): WordBool; cdecl;
  function SetIntValue(_id: Cardinal; path: PWideChar; value: Integer): WordBool; cdecl;
  function GetUIntValue(_id: Cardinal; path: PWideChar; value: PCardinal): WordBool; cdecl;
  function SetUIntValue(_id: Cardinal; path: PWideChar; value: Cardinal): WordBool; cdecl;
  function GetFloatValue(_id: Cardinal; path: PWideChar; value: PDouble): WordBool; cdecl;
  function SetFloatValue(_id: Cardinal; path: PWideChar; value: Double): WordBool; cdecl;
  function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl;
  function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
  function GetAllFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function GetEnabledFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function SetEnabledFlags(_id: Cardinal; path, flags: PWideChar): WordBool; cdecl;
  function GetEnumOptions(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function SignatureFromName(name: PWideChar; len: PInteger): WordBool; cdecl;
  function NameFromSignature(sig: PWideChar; len: PInteger): WordBool; cdecl;
  function GetSignatureNameMap(len: PInteger): WordBool; cdecl;
  {$endregion}

var
  slSignatureNameMap: TFastStringList;
  bSignatureNameMapBuilt: Boolean;

implementation

uses
  Classes, SysUtils, Variants,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMeta, xeMessages, xeElements, xeRecords;

{$region 'Native functions'}
{$region 'Name helpers'}
function FormString(const rec: IwbMainRecord): String;
begin
  Result := Format('[%s:%s]', [
    AnsiString(rec.Signature),
    IntToHex(rec.LoadOrderFormID, 8)
  ]);
end;

function CellName(const rec: IwbMainRecord): String;
begin
  Result := Format('%s <%d,%d>', [
    NativeName(rec.ElementByPath['Worldspace'].LinksTo, true),
    Integer(rec.ElementNativeValues['XCLC\X']),
    Integer(rec.ElementNativeValues['XCLC\Y'])
  ]);
end;

function PlacementName(const rec: IwbMainRecord): String;
begin
  Result := Format('Places %s in %s', [
    NativeName(rec.ElementByPath['NAME'].LinksTo, true),
    NativeName(rec.ElementByPath['Cell'].LinksTo, true)
  ]);
end;

function NativeName(const e: IwbElement; quoteFull: Boolean = False): String;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
begin
  if Supports(e, IwbFile, _file) then
    Result := _file.FileName
  else if Supports(e, IwbGroupRecord, group) then
    Result := group.ShortName
  else if Supports(e, IwbMainRecord, rec) then begin
    if rec.ElementExists['FULL'] then begin
      if quoteFull then
        Result := '"' + rec.FullName + '"'
      else
        Result := rec.FullName;
    end
    else if rec.ElementExists['EDID'] then
      Result := rec.EditorID
    else if String(rec.Signature) = 'CELL' then
      Result := CellName(rec)
    else if rec.ElementExists['NAME'] then
      Result := PlacementName(rec)
    else if rec.Signature = 'TES4' then
      Result := 'File Header'
    else
      Result := FormString(rec);
  end
  else
    Result := e.Name;
end;
{$endregion}

{$region 'Path helpers'}
function HexFormID(const rec: IwbMainRecord): String;
begin
  Result := IntToHex(rec.LoadOrderFormID, 8);
end;

function GetPathName(const element: IwbElement): String;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  parent: IwbElement;
begin
  if Supports(element, IwbFile, _file) then
    Result := _file.FileName
  else begin
    parent := element.Container as IwbElement;
    if Supports(element, IwbGroupRecord, group) then begin
      if group.GroupType = 0 then
        Result := String(TwbSignature(group.GroupLabel))
      else if IsChildGroup(group) then
        Result := 'Child Group'
      else
        Result := group.ShortName;
    end
    else if Supports(element, IwbMainRecord, rec) then begin
      if rec.Signature = 'TES4' then
        Result := 'File Header'
      else
        Result := HexFormID(rec);
    end
    else if IsArray(parent) then
      Result := Format('[%d]', [element.Container.IndexOf(element)])
    else
      Result := element.Name;
  end;
end;

function GetPath(const element: IwbElement; short: WordBool = False; local: WordBool = False; curPath: String = ''): String;
var
  container: IwbContainer;
begin
  Result := GetPathName(element);
  if curPath <> '' then
    Result := Format('%s\%s', [Result, curPath]);
  if Supports(element, IwbMainRecord) and short then
    Result := GetPath(element._File as IwbElement, short, local, Result)
  else if not Supports(element, IwbFile) then begin
    container := NativeContainer(element);
    if Supports(container, IwbMainRecord) and local then exit;
    Result := GetPath(container as IwbElement, short, local, Result);
  end;
end;
{$endregion}

function NativeSignature(const element: IwbElement): String;
var
  group: IwbGroupRecord;
  e: IwbHasSignature;
begin
  if Supports(element, IwbGroupRecord, group) then begin
    if group.GroupType = 0 then
      Result := String(TwbSignature(group.GroupLabel))
    else
      Result := String(group.Signature);
  end
  else if Supports(element, IwbHasSignature, e) then
    Result := String(e.Signature)
  else
    raise Exception.Create('Error: Element does not have a signature.');
end;

{$region 'SetValue helpers'}
function ParseFormIDValue(const value: String; var formID: Int64): Boolean;
var
  n, len, open: Integer;
begin
  Result := True;
  open := 0;
  n := 1;
  len := Length(value);
  // attempt to parse formID via a string in format [SIGN:12345678] or [12345678]
  // stops parsing early if valid string cannot fit in remaining buffer
  while n <= len do begin
    if (open = 0) and (len - n < 9) then
      break;
    case value[n] of
      '[': open := n;
      ':':
        if n - open = 5 then
          open := n
        else
          open := 0;
      ']':
        if (open <> 0) and (n - open = 9)
        and TryStrToInt64('$' + Copy(value, open + 1, 8), formID) then
          exit;
    end;
    Inc(n);
  end;
  // attempts to convert entire key to integer
  Result := TryStrToInt64('$' + value, formID);
end;

procedure SetElementValue(const element: IwbElement; const value: String);
var
  formID: Int64;
begin
  if IsFormID(element) then begin
    if value = '' then
      element.NativeValue := 0
    else if ParseFormIDValue(value, formID) then
      element.NativeValue := element._File.LoadOrderFormIDtoFileFormID(formID)
    else
      element.NativeValue := EditorIDToFormID(element._File, value);
  end
  else
    element.EditValue := value;
end;
{$endregion}

{$region 'Native value helpers'}
function GetNativeValue(_id: Cardinal; path: PWideChar): Variant;
var
  element: IwbElement;
begin
  element := NativeGetElement(_id, path) as IwbElement;
  if ElementNotFound(element, path) then exit;
  Result := element.NativeValue;
end;

procedure SetNativeValue(_id: Cardinal; path: PWideChar; value: Variant);
var
  element: IwbElement;
begin
  element := NativeGetElement(_id, path) as IwbElement;
  if ElementNotFound(element, path) then exit;
  element.NativeValue := value;
end;
{$endregion}

function IndexOfFlag(const flagsDef: IwbFlagsDef; const name: String): Integer;
begin
  for Result := 0 to Pred(flagsDef.FlagCount) do
    if flagsDef.Flags[Result] = name then exit;
  Result := -1;
end;

procedure NativeSetFlag(const element: IwbElement; index: Integer; enabled: WordBool);
var
  flagVal: UInt64;
begin
  flagVal := 1 shl index;
  if enabled then
    element.NativeValue := element.NativeValue or flagVal
  else
    element.NativeValue := element.NativeValue and not flagVal;
end;

{$region 'SignatureNameMap helpers'}
function NativeSignatureFromName(const name: String): String;
var
  i: Integer;
begin
  BuildSignatureNameMap;
  i := slSignatureNameMap.IndexOfValue(name);
  if i = -1 then
    raise Exception.Create('Could not find signature for name: ' + name);
  Result := slSignatureNameMap.Names[i];
end;

function NativeNameFromSignature(const sig: String): String;
var
  i: Integer;
begin
  BuildSignatureNameMap;
  i := slSignatureNameMap.IndexOfName(sig);
  if i = -1 then
    raise Exception.Create('Could not find name for signature: ' + sig);
  Result := slSignatureNameMap.ValueFromIndex[i];
end;

procedure BuildSignatureNameMap;
var
  i: Integer;
  sig: String;
  recordDef: TwbRecordDefEntry;
begin
  if bSignatureNameMapBuilt then exit;
  for i := Low(wbRecordDefs) to High(wbRecordDefs) do begin
    recordDef := wbRecordDefs[i];
    sig := String(recordDef.rdeSignature);
    slSignatureNameMap.Values[sig] := recordDef.rdeDef.Name;
  end;
  bSignatureNameMapBuilt := True;
end;
{$endregion}
{$endregion}

{$region 'API functions'}
{$region 'Name functions'}
function Name(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := NativeName(element);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function LongName(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := element.Name;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function DisplayName(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := element.DisplayName;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

function Path(_id: Cardinal; short, local: WordBool; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := GetPath(element, short, local);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function Signature(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := NativeSignature(element);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{$region 'Edit value functions'}
function GetValue(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    resultStr := element.EditValue;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetValue(_id: Cardinal; path, value: PWideChar): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    SetElementValue(element, string(value));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

{$region 'Native value functions'}
function GetIntValue(_id: Cardinal; path: PWideChar; value: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    value^ := Integer(GetNativeValue(_id, path));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetIntValue(_id: Cardinal; path: PWideChar; value: Integer): WordBool; cdecl;
begin
  Result := False;
  try
    SetNativeValue(_id, path, value);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetUIntValue(_id: Cardinal; path: PWideChar; value: PCardinal): WordBool; cdecl;
const
  vtBytes = 8209;
var
  v: Variant;
begin
  Result := False;
  try
    v := GetNativeValue(_id, path);
    if VarType(v) = vtBytes then
      value^ := v[3] + (v[2] shl 8) + (v[1] shl 16) + (v[0] shl 24)
    else
      value^ := Cardinal(v);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetUIntValue(_id: Cardinal; path: PWideChar; value: Cardinal): WordBool; cdecl;
begin
  Result := False;
  try
    SetNativeValue(_id, path, value);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFloatValue(_id: Cardinal; path: PWideChar; value: PDouble): WordBool; cdecl;
begin
  Result := False;
  try
    value^ := Double(GetNativeValue(_id, path));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFloatValue(_id: Cardinal; path: PWideChar; value: Double): WordBool; cdecl;
begin
  Result := False;
  try
    SetNativeValue(_id, path, value);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

{$region 'Flag functions'}
function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  index: Integer;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not GetFlagsDef(element, flagsDef) then
      raise Exception.Create('Element does not have flags');
    index := IndexOfFlag(flagsDef, string(name));
    if index > -1 then begin
      enabled^ := element.NativeValue and (1 shl index);
      Result := True;
    end
    else
      raise Exception.Create('Flag "' + name + '" not found.');
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
var
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  index: Integer;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not GetFlagsDef(element, flagsDef) then
      raise Exception.Create('Element does not have flags');
    index := IndexOfFlag(flagsDef, name);
    if index > -1 then begin
      NativeSetFlag(element, index, enabled);
      Result := True;
    end
    else
      raise Exception.Create('Flag "' + name + '" not found.');
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetAllFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
var
  slFlags: TStringList;
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  i: Integer;
begin
  Result := False;
  try
    slFlags := TStringList.Create;
    slFlags.StrictDelimiter := True;
    slFlags.Delimiter := ',';

    try
      element := NativeGetElement(_id, path) as IwbElement;
      if ElementNotFound(element, path) then exit;
      if not GetFlagsDef(element, flagsDef) then
        raise Exception.Create('Element does not have flags');
      for i := 0 to Pred(flagsDef.FlagCount) do
        slFlags.Add(flagsDef.Flags[i]);

      // set output
      resultStr := slFlags.DelimitedText;
      len^ := Length(resultStr);
      Result := True;
    finally
      slFlags.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetEnabledFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
var
  slFlags: TStringList;
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  i: Integer;
  flagVal: UInt64;
begin
  Result := False;
  try
    slFlags := TStringList.Create;
    slFlags.StrictDelimiter := True;
    slFlags.Delimiter := ',';

    try
      element := NativeGetElement(_id, path) as IwbElement;
      if ElementNotFound(element, path) then exit;
      if not GetFlagsDef(element, flagsDef) then
        raise Exception.Create('Element does not have flags');
      for i := 0 to Pred(flagsDef.FlagCount) do begin
        flagVal := 1 shl i;
        if element.NativeValue and flagVal then
          slFlags.Add(flagsDef.Flags[i]);
      end;

      // set output
      resultStr := slFlags.DelimitedText;
      len^ := Length(resultStr);
      Result := True;
    finally
      slFlags.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetEnabledFlags(_id: Cardinal; path, flags: PWideChar): WordBool; cdecl;
var
  slFlags: TStringList;
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  i: Integer;
  enabled: Boolean;
  flagVal: UInt64;
begin
  Result := False;
  try
    slFlags := TStringList.Create;
    slFlags.StrictDelimiter := True;
    slFlags.Delimiter := ',';
    slFlags.DelimitedText := flags;

    try
      element := NativeGetElement(_id, path) as IwbElement;
      if ElementNotFound(element, path) then exit;
      if not GetFlagsDef(element, flagsDef) then
        raise Exception.Create('Element does not have flags');
      flagVal := 0;
      for i := Pred(flagsDef.FlagCount) downto 0 do begin
        enabled := (flagsDef.Flags[i] <> '') and
          (slFlags.IndexOf(flagsDef.Flags[i]) > -1);
        flagVal := flagVal shl 1 + UInt64(Ord(enabled));
      end;
      element.NativeValue := flagVal;
      Result := True;
    finally
      slFlags.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

{$region 'Enum functions'}
function GetEnumOptions(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
var
  slOptions: TStringList;
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
begin
  Result := False;
  try
    slOptions := TStringList.Create;
    slOptions.StrictDelimiter := True;
    slOptions.Delimiter := ',';

    try
      element := NativeGetElement(_id, path) as IwbElement;
      if ElementNotFound(element, path) then exit;
      if not GetEnumDef(element, enumDef) then
        raise Exception.Create('Element does not have enumeration');
      for i := 0 to Pred(enumDef.NameCount) do
        slOptions.Add(enumDef.Names[i]);

      // set output
      resultStr := slOptions.DelimitedText;
      len^ := Length(resultStr);
      Result := True;
    finally
      slOptions.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

{$region 'SignatureNameMap functions'}
function SignatureFromName(name: PWideChar; len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    resultStr := NativeSignatureFromName(string(name));
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NameFromSignature(sig: PWideChar; len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    resultStr := NativeNameFromSignature(sig);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetSignatureNameMap(len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    BuildSignatureNameMap;
    SetResultFromList(TStringList(slSignatureNameMap), len);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}
{$endregion}

initialization
begin
  slSignatureNameMap := TFastStringList.Create;
  bSignatureNameMapBuilt := False;
end;

finalization
begin
  slSignatureNameMap.Free;
end;

end.
