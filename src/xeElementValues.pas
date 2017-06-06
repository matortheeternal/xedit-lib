unit xeElementValues;

interface

uses
  ArgoTypes, wbInterface;

  function Name(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function LongName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function DisplayName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function Path(_id: Cardinal; full: WordBool; len: PInteger): WordBool; cdecl;
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
  function ToggleFlag(_id: Cardinal; path, name: PWideChar): WordBool; cdecl;
  function GetAllFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function GetEnabledFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function SignatureFromName(name: PWideChar; len: PInteger): WordBool; cdecl;
  function NameFromSignature(sig: PWideChar; len: PInteger): WordBool; cdecl;
  function GetSignatureNameMap(len: PInteger): WordBool; cdecl;

  // native functions
  function GetPath(element: IwbElement; full: WordBool = True; curPath: String = ''): String;
  function GetPathName(element: IwbElement): String;
  function NativeName(e: IwbElement; quoteFull: Boolean = False): String;
  function NativeSignatureFromName(name: String): String;
  function NativeNameFromSignature(sig: String): String;
  procedure BuildSignatureNameMap;

var
  slSignatureNameMap: TFastStringList;
  bSignatureNameMapBuilt: Boolean;

implementation

uses
  Classes, SysUtils, Variants,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeElements, xeMessages, xeGroups, xeMeta;

function FormString(rec: IwbMainRecord): String;
begin
  Result := Format('[%s:%s]', [
    AnsiString(rec.Signature),
    IntToHex(rec.LoadOrderFormID, 8)
  ]);
end;

function CellName(rec: IwbMainRecord): String;
begin
  Result := Format('%s <%d,%d>', [
    NativeName(rec.ElementByPath['Worldspace'].LinksTo, true),
    Integer(rec.ElementNativeValues['XCLC\X']),
    Integer(rec.ElementNativeValues['XCLC\Y'])
  ]);
end;

function PlacementName(rec: IwbMainRecord): String;
begin
  Result := Format('Places %s in %s', [
    NativeName(rec.ElementByPath['NAME'].LinksTo, true),
    NativeName(rec.ElementByPath['Cell'].LinksTo, true)
  ]);
end;

function NativeName(e: IwbElement; quoteFull: Boolean = False): String;
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
      Result := 'File Header';
  end
  else
    Result := e.Name;
end;

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

function HexFormID(rec: IwbMainRecord): String;
begin
  Result := IntToHex(rec.LoadOrderFormID, 8);
end;

function GetPathName(element: IwbElement): String;
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
    else if Supports(element, IwbMainRecord, rec) then
      Result := HexFormID(rec)
    else if IsArray(parent) then
      Result := Format('[%d]', [element.Container.IndexOf(element)])
    else
      Result := element.Name;
  end;
end;

function GetPath(element: IwbElement; full: WordBool = True; curPath: String = ''): String;
var
  container: IwbContainer;
begin
  Result := GetPathName(element);
  if curPath <> '' then
    Result := Format('%s\%s', [Result, curPath]);
  if Supports(element, IwbMainRecord) then
    Result := GetPath(element._File as IwbElement, full, Result)
  else if not Supports(element, IwbFile) then begin
    container := NativeContainer(element);
    if Supports(container, IwbMainRecord) and not full then exit;
    Result := GetPath(container as IwbElement, full, Result);
  end;
end;

function Path(_id: Cardinal; full: WordBool; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := GetPath(element, full);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NativeSignature(element: IwbElement): String;
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

function GetValue(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
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
    element := NativeGetElementEx(_id, path);
    element.EditValue := string(value);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetNativeValue(_id: Cardinal; path: PWideChar): Variant;
var
  element: IwbElement;
begin
  Result := Variants.Null;
  element := NativeGetElementEx(_id, path);
  Result := element.NativeValue;
end;

procedure SetNativeValue(_id: Cardinal; path: PWideChar; value: Variant);
var
  element: IwbElement;
begin
  element := NativeGetElementEx(_id, path);
  element.NativeValue := value;
end;

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
begin
  Result := False;
  try
    value^ := Cardinal(GetNativeValue(_id, path));
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

function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  i: Integer;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
    if not GetFlagsDef(element, flagsDef) then
      raise Exception.Create('Element does not have flags');
    for i := 0 to Pred(flagsDef.FlagCount) do
      if flagsDef.Flags[i] = name then begin
        enabled^ := element.NativeValue and (1 shl i);
        break;
      end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
var
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
    if not GetFlagsDef(element, flagsDef) then
      raise Exception.Create('Element does not have flags');
    for i := 0 to Pred(flagsDef.FlagCount) do
      if flagsDef.Flags[i] = name then begin
        flagVal := 1 shl i;
        if enabled then
          element.NativeValue := element.NativeValue or flagVal
        else
          element.NativeValue := element.NativeValue and not flagVal;
      end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ToggleFlag(_id: Cardinal; path, name: PWideChar): WordBool; cdecl;
var
  element: IwbElement;
  flagsDef: IwbFlagsDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
    if not GetFlagsDef(element, flagsDef) then
      raise Exception.Create('Element does not have flags');
    for i := 0 to Pred(flagsDef.FlagCount) do
      if flagsDef.Flags[i] = name then begin
        flagVal := 1 shl i;
        if element.NativeValue and flagVal then
          element.NativeValue := element.NativeValue and not flagVal
        else
          element.NativeValue := element.NativeValue or flagVal;
      end;
    Result := True;
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
      element := NativeGetElementEx(_id, path);
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
  flagVal: Cardinal;
begin
  Result := False;
  try
    slFlags := TStringList.Create;
    slFlags.StrictDelimiter := True;
    slFlags.Delimiter := ',';

    try
      element := NativeGetElementEx(_id, path);
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

function NativeElementMatches(element: IwbElement; value: PWideChar): WordBool;
var
  rec: IwbMainRecord;
begin
  Result := string(value) = element.EditValue;
  if not Result and Supports(element.LinksTo, IwbMainRecord, rec) then
    Result := string(value) = NativeName(rec);
end;

function NativeGetArrayValue(container: IwbContainerElementRef; value: PWideChar): IwbElement;
var
  i: Integer;
begin
  for i := 0 to Pred(container.ElementCount) do begin
    Result := container.Elements[i];
    if NativeElementMatches(Result, value) then
      exit;
  end;
  Result := nil;
end;

function HasArrayValue(_id: Cardinal; value: PWideChar; bool: PWordBool): WordBool; cdecl;
var
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbContainerElementRef, container)
    or not IsArray(container) then
      raise Exception.Create('Interface must be an array.');
    bool^ := Assigned(NativeGetArrayValue(container, value));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetArrayValue(_id: Cardinal; value: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IInterface;
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbContainerElementRef, container)
    or not IsArray(container) then
      raise Exception.Create('Interface must be an array.');
    element := NativeGetArrayValue(container, value);
    StoreIfAssigned(element, _res, Result);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{function DeleteArrayValue(_id: Cardinal; value: PWideChar): WordBool; cdecl;
begin

end;}

function NativeSignatureFromName(name: String): String;
var
  i: Integer;
begin
  BuildSignatureNameMap;
  i := slSignatureNameMap.IndexOfValue(name);
  if i = -1 then
    raise Exception.Create('Could not find signature for name: ' + name);
  Result := slSignatureNameMap.Names[i];
end;

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

function NativeNameFromSignature(sig: String): String;
var
  i: Integer;
begin
  BuildSignatureNameMap;
  i := slSignatureNameMap.IndexOfName(sig);
  if i = -1 then
    raise Exception.Create('Could not find name for signature: ' + sig);
  Result := slSignatureNameMap.ValueFromIndex[i];
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
    resultStr := slSignatureNameMap.Text;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
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
