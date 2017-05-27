unit xeSerialization;

interface

uses
  Argo, ArgoTypes,
  wbInterface;

  function ElementToJson(_id: Cardinal; json: PWideChar; len: Integer): WordBool; cdecl;
  //function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar; _res: PCardinal): WordBool; cdecl;

  // native functions
  function NativeElementToJson(element: IwbElement; obj: TJSONObject): TJSONObject;
  function GroupToJson(group: IwbGroupRecord; obj: TJSONObject): TJSONObject;

implementation

uses
  Variants, SysUtils, StrUtils,
  xeMeta, xeFiles, xeGroups, xeElements, xeElementValues, xeMessages;

procedure JsonArrayAdd(ary: TJSONArray; obj: TJSONObject);
begin
  if obj.Count = 1 then
    ary.AddValue(obj.ValueFromIndex[0])
  else
    ary.Add(obj);
end;

function IsFlags(element: IwbElement): Boolean;
var
  intDef: IwbIntegerDef;
begin
  Result := Supports(element.Def, IwbIntegerDef, intDef)
    and Supports(intDef.Formater[element], IwbFlagsDef);
end;

function NativeElementToJson(element: IwbElement; obj: TJSONObject): TJSONObject;
const
  ArrayTypes: TSmashTypes = [stUnsortedArray, stUnsortedStructArray, stSortedArray,
    stSortedStructArray];
var
  path: String;
  container: IwbContainerElementRef;
  childElement: IwbElement;
  v: Variant;
  i: Integer;
  childObject: TJSONObject;
begin
  path := Element.Name;
  if Supports(element, IwbContainerElementRef, container)
  and ((container.ElementCount > 0) or IsFlags(element)) then begin
    if GetSmashType(element) in ArrayTypes then begin
      obj.A[path] := TJSONArray.Create;
      for i := 0 to Pred(container.ElementCount) do begin
        childElement := container.Elements[i];
        JsonArrayAdd(obj.A[path], NativeElementToJson(childElement, TJSONObject.Create));
      end;
    end
    else begin
      childObject := TJSONObject.Create;
      for i := 0 to Pred(container.ElementCount) do begin
        childElement := container.Elements[i];
        NativeElementToJson(childElement, childObject);
      end;
      obj.O[path] := childObject;
    end;
  end
  else begin
    v := element.NativeValue;
    case VarType(v) of
      varSmallInt, varInteger, varInt64, varByte, varWord, varLongWord:
        obj.I[path] := v;
      varSingle, varDouble:
        obj.D[path] := Double(v);
      varBoolean:
        obj.B[path] := Boolean(v);
    else
      obj.S[path] := element.EditValue;
    end;
  end;

  // return the JSON object
  Result := obj;
end;

function RecordToJson(rec: IwbMainRecord; obj: TJSONObject): TJSONObject;
var
  i: Integer;
begin
  for i := Pred(rec.ElementCount) downto 0 do
    NativeElementToJson(rec.Elements[i], obj);
  if Assigned(rec.ChildGroup) then
    GroupToJson(rec.ChildGroup, obj);
  Result := obj;
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
  records := TJSONArray.Create;
  groups := TJSONObject.Create;
  // iterate through children
  for i := 0 to Pred(group.ElementCount) do begin
    if Supports(group.Elements[i], IwbMainRecord, rec) then
      records.Add(RecordToJson(rec, TJSONObject.Create))
    else if Supports(group.Elements[i], IwbGroupRecord, innerGroup)
    and not (innerGroup.GroupType in [1, 6..7]) then
      GroupToJson(innerGroup, groups);
  end;
  // assign objects
  name := GetPathName(group as IwbElement);
  if groups.Count = 0 then
    obj.A[name] := records
  else begin
    obj.O[name] := groups;
    if records.Count > 1 then
      obj.O[name].A['Records'] := records;
  end;
  // return result
  Result := obj;
end;

function FileToJson(_file: IwbFile): TJSONObject;
var
  obj: TJSONObject;
  group: IwbGroupRecord;
  i: Integer;
begin
  obj := TJSONObject.Create;
  obj.S['Filename'] := _file.FileName;
  obj.O['File Header'] := RecordToJson(_file.Header, TJSONObject.Create);
  obj.O['Groups'] := TJSONObject.Create;
  for i := 1 to Pred(_file.ElementCount)  do begin
    if Supports(_file.Elements[i], IwbGroupRecord, group) then
      GroupToJson(group, obj.O['Groups']);
  end;
  Result := obj;
end;

function ElementToJson(_id: Cardinal; json: PWideChar; len: Integer): WordBool; cdecl;
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
    e := Resolve(_id);
    obj := nil;
    if Supports(e, IwbFile, _file) then
      obj := FileToJson(_file)
    else if Supports(e, IwbGroupRecord, group) then
      obj := GroupToJson(group, TJSONObject.Create)
    else if Supports(e, IwbMainRecord, rec) then
      obj := RecordToJson(rec, TJSONObject.Create)
    else if Supports(e, IwbElement, element) then
      obj := NativeElementToJson(element, TJSONObject.Create);
    if Assigned(obj) then begin
      StrLCopy(json, PWideChar(obj.ToString), len);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

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

{function JsonToElement(element: IwbElement; obj: TJSONObject): IInterface;
const
  ArrayTypes: TSmashTypes = [stUnsortedArray, stUnsortedStructArray, stSortedArray,
    stSortedStructArray];
var
  container: IwbContainerElementRef;
  childElement: IwbElement;
  path: String;
  ary: TJSONArray;
  i: Integer;
  v: Variant;
begin
  if not Assigned(element) or not Assigned(obj) then
    exit;
  path := GetPathName(element);
  if Supports(element, IwbContainerElementRef, container) then begin
    if GetSmashType(element) in ArrayTypes then begin
      ary := obj.A[path];
      for i := 0 to Pred(ary.Count) do begin
        childElement := AssignElementIfMissing(container, i);
        SOToElement(childElement, ary.O[i]);
      end;
    end
    else begin
      ary := obj.A[path];
      for i := 0 to Pred(obj.Count) do begin
        path := obj.Keys[i];
        childElement := AddElementIfMissing(container, path);
        SOToElement(childElement, obj.O[path]);
      end;
    end;
  end
  else begin
    v := element.NativeValue;
    case VarType(v) of
      varSmallInt, varInteger, varInt64, varByte, varWord, varLongWord:
        element.NativeValue := obj.I[path];
      varSingle, varDouble:
        element.NativeValue := obj.D[path];
      varBoolean:
        element.NativeValue := obj.B[path];
    else
      element.EditValue := obj.S[path];
    end;
  end;
  Result := element;
end;

procedure ApplyJson(container: IwbContainerElementRef; obj: TJSONObject; path: string);
begin
  SOToElement(container.ElementByPath[path], obj.O[path]);
end;

procedure JsonToElements(container: IwbContainerElementRef; var obj: TJSONObject;
  const excludedPaths: array of string);
var
  element: IwbElement;
  path: string;
  i: Integer;
begin
  for i := 0 to Pred(obj.Count) do begin
    path := obj.Keys[i];
    if MatchStr(path, excludedPaths) then continue;
    element := AddElementIfMissing(container, path);
    SOToElement(element, obj);
  end;
end;

procedure JsonToRecordHeader(header: IwbElement; obj: TJSONObject);
const
  ExcludedPaths: array[0..1] of string = (
    'Signature',
    'Data Size'
  );
  SignatureExceptionFormat = 'Error deserializing record header: record ' +
    'signatures do not match, %s != %s';
var
  container: IwbContainerElementRef;
  recordSig, objSig: String;
begin
  if not Supports(header, IwbContainerElementRef, container)
  or not Assigned(obj) then
    exit;
  // raise exception if signature does not match
  recordSig := container.ElementEditValues['Signature'];
  objSig := obj.S['Signature'];
  if recordSig <> objSig then
    raise Exception.Create(Format(SignatureExceptionFormat, [recordSig, objSig]));
  // assign to whitelisted paths
  SOToElements(container, obj, ExcludedPaths);
end;

function JsonToRecord(rec: IwbMainRecord; obj: TJSONObject): IInterface;
const
  ExcludedPaths: array[0..0] of string = (
    'Record Header'
  );
var
  container: IwbContainerElementRef;
begin
  Result := rec;
  // deserialize header
  SOToRecordHeader(rec.ElementByPath['Record Header'], obj.O['Record Header']);
  // deserialize elements
  if Supports(rec, IwbContainerElementRef, container) then
    SOToElements(container, obj, ExcludedPaths);
end;

function JsonToGroup(group: IwbGroupRecord; ary: TJSONArray): IInterface;
var
  recObj, recHeader: TJSONObject;
  e: IInterface;
  rec: IwbMainRecord;
  i: Integer;
begin
  Result := group;
  // loop through array of records
  for i := 0 to Pred(ary.Count) do begin
    recObj := ary.O[i];
    recHeader := recObj.O['Record Header'];
    // attempt to resolve existing record
    e := ResolveFromGroup(group, recHeader.S['FormID']);
    // create record if not found
    if not Assigned(e) then
      e := group.Add(recHeader.S['Signature']);
    // deserialize record JSON
    if Supports(e, IwbMainRecord, rec) then
      SOToRecord(rec, recObj);
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
  SOToRecordHeader(header.ElementByPath['Record Header'], obj.O['Record Header']);
  SOToElements(container, obj, ExcludedPaths);
end;

function JsonToFile(_file: IwbFile; obj: TJSONObject): IInterface;
var
  groups: TJSONObject;
  group: IwbGroupRecord;
  signature: string;
  i: Integer;
begin
  Result := _file;
  // deserialize header
  SOToFileHeader(_file.Header, obj.O['File Header']);
  // deserialize groups
  groups := obj.O['Groups'];
  for i := 0 to Pred(groups.Count) do begin
    signature := groups.Keys[i];
    group := AddGroupIfMissing(_file, signature);
    SOToGroup(group, groups.A[signature]);
  end;
end;

function WriteElementFromJson(e: IInterface; obj: TJSONObject): IInterface;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  element: IwbElement;
begin
  if Supports(e, IwbFile, _file) then
    Result := SOToFile(_file, obj)
  //else if Supports(e, IwbGroupRecord, group) then
    //Result := SOToGroup(group, obj)
  else if Supports(e, IwbMainRecord, rec) then
    Result := SOToRecord(rec, obj)
  else if Supports(e, IwbElement, element) then
    Result := SOToElement(element, obj);
end;

function ResolveOrAddElement(_id: Cardinal; path: PWideChar): IInterface;
begin
  if string(path) <> '' then
    Result := NativeAddElement(_id, string(path))
  else
    Result := Resolve(_id);
end;

function ElementFromJson(_id: Cardinal; path: PWideChar; json: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  e: IInterface;
begin
  Result := False;
  try
    e := ResolveOrAddElement(_id, path);
    WriteElementFromSO(e, TJSONObject.Create(json));
    _res^ := Store(e);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;}

end.
