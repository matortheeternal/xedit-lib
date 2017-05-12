unit xeElementValues;

interface

  function Name(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function Path(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function EditorID(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function Signature(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function FullName(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function SortKey(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function ElementType(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function DefType(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  function GetValue(_id: Cardinal; path, str: PWideChar; len: Integer): WordBool; cdecl;
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
  function GetEnabledFlags(_id: Cardinal; path, flags: PWideChar; len: Integer): WordBool; cdecl;

  // native functions
  function NativeName(e: IInterface): String;

implementation

uses
  Classes, SysUtils, Variants,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeElements, xeMessages, xeGroups, xeMeta;

function NativeName(e: IInterface): String;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  element: IwbElement;
begin
  if Supports(e, IwbFile, _file) then
    Result := _file.FileName
  else if Supports(e, IwbGroupRecord, group) then
    Result := group.ShortName
  else if Supports(e, IwbMainRecord, rec) then begin
    if rec.ElementExists['FULL'] then
      Result := rec.FullName
    else if rec.ElementExists['NAME'] then
      Result := rec.ElementEditValues['NAME'];
  end
  else if Supports(e, IwbElement, element) then
    Result := element.Name;
end;

function Name(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  sName: String;
begin
  Result := false;
  try
    sName := NativeName(Resolve(_id));
    if sName <> '' then begin
      StrLCopy(str, PWideChar(WideString(sName)), len);
      Result := True;
    end;
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

function GetPath(curPath: String; element: IwbElement): String;
begin
  Result := GetPathName(element);
  if curPath <> '' then
    Result := Format('%s\%s', [Result, curPath]);
  if Supports(element, IwbMainRecord) then
    Result := GetPath(Result, element._File as IwbElement)
  else if not Supports(element, IwbFile) then
    Result := GetPath(Result, NativeContainer(element) as IwbElement);
end;

function Path(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  sPath: String;
  element: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      sPath := GetPath('', element);
      StrLCopy(str, PWideChar(WideString(sPath)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function EditorID(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      StrLCopy(str, PWideChar(WideString(rec.EditorID)), len);
      Result := true;
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

function Signature(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  element: IwbElement;
  sig: String;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      sig := NativeSignature(element);
      StrLCopy(str, PWideChar(WideString(sig)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FullName(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) and rec.ElementExists['FULL'] then begin
      StrLCopy(str, PWideChar(WideString(rec.FullName)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortKey(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      StrLCopy(str, PWideChar(WideString(element.SortKey[false])), len);
      Result := true;
    end;
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

function ElementType(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  element: IwbElement;
  s: String;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      s := etToString(element.ElementType);
      StrLCopy(str, PWideChar(WideString(s)), len);
      Result := true;
    end;
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

function DefType(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
var
  element: IwbElement;
  s: String;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      s := dtToString(GetDefType(element));
      StrLCopy(str, PWideChar(WideString(s)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetValue(_id: Cardinal; path, str: PWideChar; len: Integer): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  sValue: string;
begin
  Result := false;
  try
    e := NativeGetElement(_id, path);
    if Supports(e, IwbElement, element) then begin
      sValue := element.EditValue;
      StrLCopy(str, PWideChar(WideString(sValue)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetValue(_id: Cardinal; path, value: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
begin
  Result := false;
  try
    e := NativeGetElement(_id, path);
    if Supports(e, IwbElement, element) then begin
      element.EditValue := string(value);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetNativeValue(_id: Cardinal; path: PWideChar): Variant;
var
  e: IInterface;
  element: IwbElement;
begin
  Result := Variants.Null;
  e := NativeGetElement(_id, path);
  if Supports(e, IwbElement, element) then
    Result := element.NativeValue;
end;

procedure SetNativeValue(_id: Cardinal; path: PWideChar; value: Variant);
var
  e: IInterface;
  element: IwbElement;
begin
  e := NativeGetElement(_id, path);
  if Supports(e, IwbElement, element) then
    element.NativeValue := value;
end;

function GetIntValue(_id: Cardinal; path: PWideChar; value: PInteger): WordBool; cdecl;
begin
  Result := false;
  try
    value^ := Integer(GetNativeValue(_id, path));
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetIntValue(_id: Cardinal; path: PWideChar; value: Integer): WordBool; cdecl;
begin
  Result := false;
  try
    SetNativeValue(_id, path, value);
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetUIntValue(_id: Cardinal; path: PWideChar; value: PCardinal): WordBool; cdecl;
begin
  Result := false;
  try
    value^ := Cardinal(GetNativeValue(_id, path));
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetUIntValue(_id: Cardinal; path: PWideChar; value: Cardinal): WordBool; cdecl;
begin
  Result := false;
  try
    SetNativeValue(_id, path, value);
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFloatValue(_id: Cardinal; path: PWideChar; value: PDouble): WordBool; cdecl;
begin
  Result := false;
  try
    value^ := Double(GetNativeValue(_id, path));
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFloatValue(_id: Cardinal; path: PWideChar; value: Double): WordBool; cdecl;
begin
  Result := false;
  try
    SetNativeValue(_id, path, value);
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFlag(_id: Cardinal; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := false;
  try
    e := NativeGetElement(_id, path);
    if Supports(e, IwbElement, element)
    and Supports(element.Def, IwbEnumDef, enumDef) then begin
      for i := 0 to Pred(enumDef.NameCount) do
        if SameText(enumDef.Names[i], String(name)) then begin
          flagVal := 1 shl i;
          if enabled then
            element.NativeValue := element.NativeValue or flagVal
          else
            element.NativeValue := element.NativeValue and not flagVal;
        end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFlag(_id: Cardinal; path, name: PWideChar; enabled: PWordBool): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
begin
  Result := False;
  try
    e := NativeGetElement(_id, path);
    if Supports(e, IwbElement, element)
    and Supports(element.Def, IwbEnumDef, enumDef) then begin
      for i := 0 to Pred(enumDef.NameCount) do
        if SameText(enumDef.Names[i], name) then begin
          enabled^ := element.NativeValue and (1 shl i);
          exit;
        end;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ToggleFlag(_id: Cardinal; path, name: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := false;
  try
    e := NativeGetElement(_id, path);
    if Supports(e, IwbElement, element)
    and Supports(element.Def, IwbEnumDef, enumDef) then begin
      for i := 0 to Pred(enumDef.NameCount) do
        if SameText(enumDef.Names[i], name) then begin
          flagVal := 1 shl i;
          if element.NativeValue and flagVal then
            element.NativeValue := element.NativeValue and not flagVal
          else
            element.NativeValue := element.NativeValue or flagVal;
        end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetEnabledFlags(_id: Cardinal; path, flags: PWideChar; len: Integer): WordBool; cdecl;
var
  slFlags: TStringList;
  e: IInterface;
  element: IwbElement;
  enumDef: IwbEnumDef;
  i: Integer;
  flagVal: Cardinal;
begin
  Result := false;
  try
    slFlags := TStringList.Create;
    slFlags.StrictDelimiter := true;
    slFlags.Delimiter := ',';

    try
      e := NativeGetElement(_id, path);
      if Supports(e, IwbElement, element)
      and Supports(element.Def, IwbEnumDef, enumDef) then begin
        for i := 0 to Pred(enumDef.NameCount) do begin
          flagVal := 1 shl i;
          if element.NativeValue and flagVal then
            slFlags.Add(enumDef.Names[i]);
        end;
      end;

      // set output
      StrLCopy(flags, PWideChar(WideString(slFlags.DelimitedText)), len);
      Result := true;
    finally
      slFlags.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
