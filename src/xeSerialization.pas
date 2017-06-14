unit xeSerialization;

interface

uses
  Argo, ArgoTypes,
  wbInterface;

  {$region 'Native functions'}
  procedure JsonToElement(element: IwbElement; obj: TJSONObject; path: String);
  procedure JsonToElements(container: IwbContainerElementRef; obj: TJSONObject; const excludedPaths: array of string);
  function NativeElementToJson(element: IwbElement): TJSONValue;
  function GroupToJson(group: IwbGroupRecord; obj: TJSONObject): TJSONObject;
  {$endregion}

  {$region 'API functions'}
  function ElementToJson(_id: Cardinal; len: PInteger; editValues: WordBool): WordBool; cdecl;
  function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar): WordBool; cdecl;
  {$endregion}

implementation

uses
  Variants, SysUtils, StrUtils,
  wbImplementation,
  xeMeta, xeFiles, xeElements, xeElementValues, xeMessages;

var
  SerializeEditValues: Boolean;

{$region 'Native functions'}
function IsFlags(element: IwbElement): Boolean;
var
  def: IwbNamedDef;
  subDef: IwbSubrecordDef;
  intDef: IwbIntegerDef;
begin
  def := element.Def;
  if Supports(def, IwbSubrecordDef, subDef) then
    def := subDef.Value;
  Result := Supports(def, IwbIntegerDef, intDef)
    and Supports(intDef.Formater[element], IwbFlagsDef);
end;

{$region 'ElementToJSON helpers'}
function ValueToJson(element: IwbElement): TJSONValue;
var
  v: Variant;
begin
  Result := TJSONValue.Create;
  if SerializeEditValues then
    Result.Put(element.EditValue)
  else begin
    v := element.NativeValue;
    case VarType(v) of
      varSmallInt, varInteger, varInt64, varByte, varWord, varLongWord:
        Result.Put(LongWord(v));
      varSingle, varDouble:
        Result.Put(Double(v));
      varBoolean:
        Result.Put(Boolean(v));
    else
      Result.Put(element.EditValue);
    end;
  end;
end;

function StructToJson(container: IwbContainerElementRef): TJSONValue;
var
  obj: TJSONObject;
  i: Integer;
  childElement: IwbElement;
begin
  Result := TJSONValue.Create;
  obj := TJSONObject.Create;
  for i := 0 to Pred(container.ElementCount) do begin
    childElement := container.Elements[i];
    obj[childElement.Name] := NativeElementToJson(childElement);
  end;
  Result.Put(obj);
end;

function ArrayToJson(container: IwbContainerElementRef): TJSONValue;
var
  ary: TJSONArray;
  i: Integer;
begin
  Result := TJSONValue.Create;
  ary := TJSONArray.Create;
  for i := 0 to Pred(container.ElementCount) do
    ary.AddValue(NativeElementToJson(container.Elements[i]));
  Result.Put(ary);
end;

function NativeElementToJson(element: IwbElement): TJSONValue;
var
  container: IwbContainerElementRef;
begin
  if Supports(element, IwbContainerElementRef, container)
  and ((container.ElementCount > 0) or IsFlags(element)) then begin
    if IsArray(element) then
      Result := ArrayToJson(container)
    else
      Result := StructToJson(container);
  end
  else
    Result := ValueToJSON(element);
end;

function RecordToJson(rec: IwbMainRecord): TJSONObject;
var
  i: Integer;
  element: IwbElement;
  path: String;
begin
  Result := TJSONObject.Create;
  // serialize elements
  for i := 0 to Pred(rec.ElementCount) do begin
    element := rec.Elements[i];
    path := element.Name;
    Result[path] := NativeElementToJson(element);
  end;
  // serialize child group
  if Assigned(rec.ChildGroup) then
    GroupToJson(rec.ChildGroup, Result);
end;

function GroupToJson(group: IwbGroupRecord; obj: TJSONObject): TJSONObject;
var
  name: String;
  i: Integer;
  rec: IwbMainRecord;
  innerGroup: IwbGroupRecord;
  records: TJSONArray;
  groups: TJSONObject;
begin
  Result := obj;
  records := TJSONArray.Create;
  groups := TJSONObject.Create;
  // iterate through children
  for i := 0 to Pred(group.ElementCount) do begin
    if Supports(group.Elements[i], IwbMainRecord, rec) then
      records.Add(RecordToJson(rec))
    else if Supports(group.Elements[i], IwbGroupRecord, innerGroup)
    and not (innerGroup.GroupType in [1, 6..7]) then
      GroupToJson(innerGroup, groups);
  end;
  // assign objects
  name := GetPathName(group as IwbElement);
  if groups.Count = 0 then begin
    groups.Free;
    obj.A[name] := records;
  end
  else begin
    obj.O[name] := groups;
    if records.Count > 1 then
      obj.O[name].A['Records'] := records
    else
      records.Free;
  end;
end;

function FileToJson(_file: IwbFile): TJSONObject;
var
  group: IwbGroupRecord;
  i: Integer;
begin
  Result := TJSONObject.Create;
  // serialize filename and header
  Result.S['Filename'] := _file.FileName;
  Result.O['File Header'] := RecordToJson(_file.Header);
  // serialize groups
  Result.O['Groups'] := TJSONObject.Create;
  for i := 1 to Pred(_file.ElementCount) do
    if Supports(_file.Elements[i], IwbGroupRecord, group) then
      GroupToJson(group, Result.O['Groups']);
end;
{$endregion}

{$region 'ElementFromJSON helpers'}
function AddElementIfMissing(container: IwbContainerElementRef; path: String): IwbElement;
begin
  Result := container.ElementByPath[path];
  if not Assigned(Result) then
    Result := container.Add(path);
end;

function AssignElementIfMissing(container: IwbContainerElementRef; index: Integer): IwbElement;
begin
  if container.ElementCount > index then
    Result := container.Elements[index]
  else
    Result := container.Assign(High(integer), nil, False);
end;

procedure JsonToArrayElement(element: IwbElement; ary: TJSONArray; index: Integer);
var
  v: TJSONValue;
begin
  v := ary[index];
  case v.JSONValueType of
    jtInt, jtBoolean, jtDouble:
      element.NativeValue := v.AsVariant;
    jtString:
      element.EditValue := v.AsString;
    jtObject:
      JsonToElement(element, v.AsObject, '');
  end;
end;

procedure JsonToFlags(element: IwbElement; flagsDef: IwbFlagsDef; obj: TJSONObject);
var
  flagVal: UInt64;
  i, index: Integer;
  flagName: String;
begin
  flagVal := 0;
  for i := 0 to Pred(obj.Count) do begin
    flagName := obj.Keys[i];
    index := IndexOfFlag(flagsDef, flagName);
    flagVal := flagVal or (1 shl index);
  end;
  element.NativeValue := flagVal;
end;

procedure JsonToElement(element: IwbElement; obj: TJSONObject; path: String);
var
  container: IwbContainerElementRef;
  childElement: IwbElement;
  ary: TJSONArray;
  i: Integer;
  v: TJSONValue;
  flagsDef: IwbFlagsDef;
begin
  if not Assigned(element) or not Assigned(obj) then
    exit;
  if GetFlagsDef(element, flagsDef) then
    JsonToFlags(element, flagsDef, obj.O[path])
  else if Supports(element, IwbContainerElementRef, container)
  and (container.ElementCount > 0) then begin
    if IsArray(element) then begin
      ary := obj.A[path];
      for i := 0 to Pred(ary.Count) do begin
        childElement := AssignElementIfMissing(container, i);
        JsonToArrayElement(childElement, ary, i);
      end;
    end
    else
      JsonToElements(container, obj.O[path], []);
  end
  else begin
    v := obj[path];
    case v.JSONValueType of
      jtInt, jtBoolean, jtDouble:
        element.NativeValue := v.AsVariant;
      jtString:
        SetElementValue(element, v.AsString);
    end;
  end;
end;

procedure JsonToElements(container: IwbContainerElementRef; obj: TJSONObject; const excludedPaths: array of string);
var
  element: IwbElement;
  path: string;
  i: Integer;
begin
  for i := 0 to Pred(obj.Count) do begin
    path := obj.Keys[i];
    if MatchStr(path, excludedPaths) then continue;
    element := CreateFromContainer(container, path) as IwbElement;
    JsonToElement(element, obj, path);
  end;
end;

function GetObjSignature(obj: TJSONObject; var signature: String): Boolean;
begin
  Result := obj.HasKey('Signature');
  if Result then
    signature := obj.S['Signature'];
end;

function GetObjFormID(obj: TJSONObject; var formID: Cardinal): Boolean;
var
  v: TJSONValue;
begin
  Result := obj.HasKey('FormID');
  if Result then begin
    v := obj['FormID'];
    case v.JSONValueType of
      jtInt: formID := v.AsVariant;
      jtString: formID := StrToInt('$' + v.AsString);
      else
        Result := false;
    end;
  end;
end;

procedure JsonToRecordHeader(header: IwbElement; obj: TJSONObject);
var
  container: IwbContainerElementRef;
  recordSig, objSig: String;
  recordFormID, objFormID: Cardinal;
begin
  if not Supports(header, IwbContainerElementRef, container)
  or not Assigned(obj) then
    exit;
  // raise exception if signature does not match
  recordSig := container.ElementEditValues['Signature'];
  if GetObjSignature(obj, objSig) and (recordSig <> objSig) then
    raise Exception.Create(Format('Error deserializing record header: record ' +
      'signatures do not match, %s != %s', [recordSig, objSig]));
  // set load order formID if different
  recordFormID := container.ElementNativeValues['FormID'];
  if GetObjFormID(obj, objFormID) and (recordFormID <> objFormID) then
    header.ContainingMainRecord.SetLoadOrderFormID(objFormID);
  // assign to whitelisted paths
  JsonToElements(container, obj, ['Signature', 'Data Size', 'FormID', 'Form Version']);
end;

procedure JsonToRecord(rec: IwbMainRecord; obj: TJSONObject);
const
  ExcludedPaths: array[0..0] of string = (
    'Record Header'
  );
var
  container: IwbContainerElementRef;
begin
  // deserialize header
  JsonToRecordHeader(rec.ElementByPath['Record Header'], obj.O['Record Header']);
  // deserialize elements
  if Supports(rec, IwbContainerElementRef, container) then
    JsonToElements(container, obj, ExcludedPaths);
end;

function GetObjString(obj: TJSONObject; key: String; var value: String): Boolean;
begin
  Result := obj.HasKey(key);
  if Result then
    value := obj.S[key];
end;

function GetRecordKey(recObj: TJSONObject): String;
var
  recHeader: TJSONObject;
  v: TJSONValue;
  str: String;
begin
  Result := '';
  recHeader := recObj.O['Record Header'];
  if recHeader.HasKey('FormID') then begin
    v := recHeader.Values['FormID'];
    if v.JSONValueType = jtInt then
      Result := IntToHex(recHeader.I['FormID'], 8)
    else if v.JSONValueType = jtString then
      Result := recHeader.S['FormID'];
  end
  else if GetObjString(recHeader, 'EDID - Editor ID', str)
  or GetObjString(recHeader, 'EDID', str) then
    Result := str
  else if GetObjString(recHeader, 'FULL - Name', str)
  or GetObjString(recHeader, 'FULL', str) then
    Result := '"' + str + '"';
end;

procedure JsonToGroup(group: IwbGroupRecord; ary: TJSONArray);
var
  recObj: TJSONObject;
  key: String;
  e: IInterface;
  rec: IwbMainRecord;
  i: Integer;
begin
  // loop through array of records
  for i := 0 to Pred(ary.Count) do begin
    recObj := ary.O[i];
    key := GetRecordKey(recObj);
    // attempt to resolve existing record if resolution key found
    if key <> '' then
      e := ResolveRecord(group, key, '')
    else
      e := nil;
    // create record if not found
    if e = nil then
      e := group.Add(recObj.O['Record Header'].S['Signature']);
    // deserialize record JSON
    if Supports(e, IwbMainRecord, rec) then
      JsonToRecord(rec, recObj);
  end;
end;

procedure JsonToFileHeader(header: IwbMainRecord; obj: TJSONObject);
const
  ExcludedPaths: array[0..3] of string = (
    'Record Header',
    'HEDR - Header',
    'Master Files',
    'ONAM - Overridden Forms' // may be able to include?
  );
var
  container: IwbContainerElementRef;
  _file: IwbFile;
  ary: TJSONArray;
  i: Integer;
begin
  if not Supports(header, IwbContainerElementRef, container)
  or not Assigned(obj) then
    exit;
  // add masters
  _file := header._File;
  ary := obj.A['Master Files'];
  for i := 0 to Pred(ary.Count) do
    _file.AddMasterIfMissing(ary.O[i].S['MAST - Filename']);
  // set record header and element values
  JsonToRecordHeader(header.ElementByPath['Record Header'], obj.O['Record Header']);
  JsonToElements(container, obj, ExcludedPaths);
end;

procedure JsonToFile(_file: IwbFile; obj: TJSONObject);
var
  groups: TJSONObject;
  group: IwbGroupRecord;
  signature: string;
  i: Integer;
begin
  // deserialize header
  JsonToFileHeader(_file.Header, obj.O['File Header']);
  // deserialize groups
  groups := obj.O['Groups'];
  for i := 0 to Pred(groups.Count) do begin
    signature := groups.Keys[i];
    group := AddGroupIfMissing(_file, signature);
    JsonToGroup(group, groups.A[signature]);
  end;
end;

procedure JsonToFiles(obj: TJSONObject);
var
  i: Integer;
  _file: IwbFile;
  fileName: String;
begin
  for i := 0 to Pred(obj.Count) do begin
    fileName := obj.Keys[i];
    _file := CreateFile(fileName, '') as IwbFile;
    JsonToFile(_file, obj.O[fileName]);
  end;
end;
{$endregion}
{$endregion}

{$region 'API functions'}
function ElementToJson(_id: Cardinal; len: PInteger; editValues: WordBool): WordBool; cdecl;
var
  e: IInterface;
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  element: IwbElement;
  obj: TJSONObject;
begin
  Result := False;
  try
    SerializeEditValues := editValues;
    e := Resolve(_id);
    obj := nil;
    // convert input element to JSONObject
    if Supports(e, IwbFile, _file) then
      obj := FileToJson(_file)
    else if Supports(e, IwbGroupRecord, group) then
      obj := GroupToJson(group, TJSONObject.Create)
    else if Supports(e, IwbMainRecord, rec) then
      obj := RecordToJson(rec)
    else if Supports(e, IwbElement, element) then begin
      obj := TJSONObject.Create;
      obj[element.Name] := NativeElementToJson(element);
    end;
    // serialize JSON to string
    if Assigned(obj) then try
      resultStr := obj.ToString;
      len^ := Length(resultStr);
      Result := True;
    finally
      obj.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  obj: TJSONObject;
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    obj := TJSONObject.Create(json);
    try
      if (_id = 0) and (path = '') then
        JsonToFiles(obj)
      else begin
        e := NativeAddElement(_id, path);
        if Supports(e, IwbFile, _file) then
          JsonToFile(_file, obj)
        else if Supports(e, IwbGroupRecord, group) then
          JsonToGroup(group, obj.A['Records'])
        else if Supports(e, IwbMainRecord, rec) then
          JsonToRecord(rec, obj)
        else if Supports(e, IwbContainerElementRef, container) then
          JsonToElements(container, obj, []);
      end;
    finally
      obj.Free;
    end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

end.
