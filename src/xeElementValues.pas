unit xeElementValues;

interface

uses
  wbInterface;

  function Name(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function LongName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function DisplayName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function Path(_id: Cardinal; full: WordBool; len: PInteger): WordBool; cdecl;
  function EditorID(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function Signature(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function FullName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function SortKey(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function ElementType(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function DefType(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetValue(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
  function SetValue(_id: Cardinal; path, value: PWideChar): WordBool; cdecl;
  function GetIntValue(_id: Cardinal; path: PWideChar; value: PInteger): WordBool; cdecl;
  function SetIntValue(_id: Cardinal; path: PWideChar; value: Integer): WordBool; cdecl;
  function GetUIntValue(_id: Cardinal; path: PWideChar; value: PCardinal): WordBool; cdecl;
  function SetUIntValue(_id: Cardinal; path: PWideChar; value: Cardinal): WordBool; cdecl;
  function GetFloatValue(_id: Cardinal; path: PWideChar; value: PDouble): WordBool; cdecl;
  function SetFloatValue(_id: Cardinal; path: PWideChar; value: Double): WordBool; cdecl;
  function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
  function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl;
  function ToggleFlag(_id: Cardinal; path, name: PWideChar): WordBool; cdecl;
  function GetEnabledFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;

  // native functions
  function GetPath(element: IwbElement; full: WordBool = True; curPath: String = ''): String;
  function GetPathName(element: IwbElement): String;
  function NativeName(e: IwbElement; quoteFull: Boolean = False): String;

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
begin
  Result := GetPathName(element);
  if curPath <> '' then
    Result := Format('%s\%s', [Result, curPath]);
  if Supports(element, IwbMainRecord) then begin
    if not full then exit;
    Result := GetPath(element._File as IwbElement, full, Result)
  end
  else if not Supports(element, IwbFile) then
    Result := GetPath(NativeContainer(element) as IwbElement, full, Result);
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

function EditorID(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      resultStr := rec.EditorID;
      len^ := Length(resultStr);
      Result := True;
    end;
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

function FullName(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    if not rec.ElementExists['FULL'] then
      raise Exception.Create('Record does not have a FULL name.');
    resultStr := rec.FullName;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortKey(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := element.SortKey[False];
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function etToString(et: TwbElementType): String;
begin
  case Ord(et) of
    Ord(etFile): Result := 'etFile';
    Ord(etMainRecord): Result := 'etMainRecord';
    Ord(etGroupRecord): Result := 'etGroupRecord';
    Ord(etSubRecord): Result := 'etSubRecord';
    Ord(etSubRecordStruct): Result := 'etSubRecordStruct';
    Ord(etSubRecordArray): Result := 'etSubRecordArray';
    Ord(etSubRecordUnion): Result := 'etSubRecordUnion';
    Ord(etArray): Result := 'etArray';
    Ord(etStruct): Result := 'etStruct';
    Ord(etValue): Result := 'etValue';
    Ord(etFlag): Result := 'etFlag';
    Ord(etStringListTerminator): Result := 'etStringListTerminator';
    Ord(etUnion): Result := 'etUnion';
    Ord(etStructChapter): Result := 'etStructChapter';
  end;
end;

function ElementType(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := etToString(element.ElementType);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function dtToString(dt: TwbDefType): String;
begin
  case Ord(dt) of
    Ord(dtRecord): Result := 'dtRecord';
    Ord(dtSubRecord): Result := 'dtSubRecord';
    Ord(dtSubRecordArray): Result := 'dtSubRecordArray';
    Ord(dtSubRecordStruct): Result := 'dtSubRecordStruct';
    Ord(dtSubRecordUnion): Result := 'dtSubRecordUnion';
    Ord(dtString): Result := 'dtString';
    Ord(dtLString): Result := 'dtLString';
    Ord(dtLenString): Result := 'dtLenString';
    Ord(dtByteArray): Result := 'dtByteArray';
    Ord(dtInteger): Result := 'dtInteger';
    Ord(dtIntegerFormater): Result := 'dtIntegerFormater';
    Ord(dtIntegerFormaterUnion): Result := 'dtIntegerFormaterUnion';
    Ord(dtFlag): Result := 'dtFlag';
    Ord(dtFloat): Result := 'dtFloat';
    Ord(dtArray): Result := 'dtArray';
    Ord(dtStruct): Result := 'dtStruct';
    Ord(dtUnion): Result := 'dtUnion';
    Ord(dtEmpty): Result := 'dtEmpty';
    Ord(dtStructChapter): Result := 'dtStructChapter';
  end;
end;

function DefType(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    resultStr := dtToString(GetDefType(element));
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

function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
var
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
    if not Supports(element.Def, IwbEnumDef, enumDef) then
      raise Exception.Create('Element does not have flags');
    for i := 0 to Pred(enumDef.NameCount) do
      if SameText(enumDef.Names[i], String(name)) then begin
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

function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
    if not Supports(element.Def, IwbEnumDef, enumDef) then
      raise Exception.Create('Element does not have flags');
    for i := 0 to Pred(enumDef.NameCount) do
      if SameText(enumDef.Names[i], name) then begin
        enabled^ := element.NativeValue and (1 shl i);
        exit;
      end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ToggleFlag(_id: Cardinal; path, name: PWideChar): WordBool; cdecl;
var
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, path);
    if not Supports(element.Def, IwbEnumDef, enumDef) then
      raise Exception.Create('Element does not have flags');
    for i := 0 to Pred(enumDef.NameCount) do
      if SameText(enumDef.Names[i], name) then begin
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

function GetEnabledFlags(_id: Cardinal; path: PWideChar; len: PInteger): WordBool; cdecl;
var
  slFlags: TStringList;
  element: IwbElement;
  enumDef: IwbEnumDef;
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
      if not Supports(element.Def, IwbEnumDef, enumDef) then
        raise Exception.Create('Element does not have flags');
      for i := 0 to Pred(enumDef.NameCount) do begin
        flagVal := 1 shl i;
        if element.NativeValue and flagVal then
          slFlags.Add(enumDef.Names[i]);
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

end.
