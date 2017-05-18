unit xeSerialization;

interface

uses
  superobject,
  wbInterface;

  function ElementToJson(_id: Cardinal; json: PWideChar; len: Integer): WordBool; cdecl;
  function ElementFromJson(_id: Cardinal; json: PWideChar; _res: PCardinal): WordBool; cdecl;

  // native functions
  function ChildGroupToSO(group: IwbGroupRecord; obj: ISuperObject): ISuperObject;

implementation

uses
  Variants, SysUtils, StrUtils,
  xeMeta, xeFiles, xeGroups, xeElements, xeMessages;

function ElementToSO(element: IwbElement; obj: ISuperObject): ISuperObject;
const
  ArrayTypes: TSmashTypes = [stUnsortedArray, stUnsortedStructArray, stSortedArray,
    stSortedStructArray];
var
  path: String;
  container: IwbContainerElementRef;
  childElement: IwbElement;
  v: Variant;
  i: Integer;
  childObject: ISuperObject;
begin
  path := Element.Name;
  if Supports(element, IwbContainerElementRef, container) and (container.ElementCount > 0) then begin
    if GetSmashType(element) in ArrayTypes then begin
      obj.O[path] := SA([]);
      for i := 0 to Pred(container.ElementCount) do begin
        childElement := container.Elements[i];
        obj.A[path].Add(ElementToSO(childElement, SO));
      end;
    end
    else begin
      childObject := SO;
      for i := 0 to Pred(container.ElementCount) do begin
        childElement := container.Elements[i];
        ElementToSO(childElement, childObject);
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

function RecordToSO(rec: IwbMainRecord; obj: ISuperObject): ISuperObject;
var
  i: Integer;
begin
  for i := Pred(rec.ElementCount) downto 0 do
    ElementToSO(rec.Elements[i], obj);
  if Assigned(rec.ChildGroup) then
    ChildGroupToSO(rec.ChildGroup, obj);
  Result := obj;
end;

function ChildGroupToSO(group: IwbGroupRecord; obj: ISuperObject): ISuperObject;
var
  i: Integer;
  mainRecord: IwbMainRecord;
begin
  obj.O['Child Group'] := SA([]);
  for i := 0 to Pred(group.ElementCount) do begin
    if Supports(group.Elements[i], IwbMainRecord, mainRecord) then
      obj.A['Child Group'].Add(RecordToSO(mainRecord, SO));
  end;
  Result := obj;
end;

function GroupToSO(group: IwbGroupRecord; obj: ISuperObject): ISuperObject;
var
  sig: String;
  i: Integer;
  mainRecord: IwbMainRecord;
begin
  sig := String(TwbSignature(group.GroupLabel));
  obj.O[sig] := SA([]);
  for i := 0 to Pred(group.ElementCount) do begin
    if Supports(group.Elements[i], IwbMainRecord, mainRecord) then
      obj.A[sig].Add(RecordToSO(mainRecord, SO));
  end;
  Result := obj;
end;

function FileToSO(_file: IwbFile): ISuperObject;
var
  obj: ISuperObject;
  group: IwbGroupRecord;
  i: Integer;
begin
  obj := SO;
  obj.S['Filename'] := _file.FileName;
  obj.O['File Header'] := SO;
  RecordToSO(_file.Header, obj.O['File Header']);
  obj.O['Groups'] := SO;
  for i := 1 to Pred(_file.ElementCount)  do begin
    if Supports(_file.Elements[i], IwbGroupRecord, group) then
      GroupToSO(group, obj.O['Groups']);
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
  obj: ISuperObject;
begin
  Result := false;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then
      obj := FileToSO(_file)
    else if Supports(e, IwbGroupRecord, group) then
      obj := GroupToSO(group, SO)
    else if Supports(e, IwbMainRecord, rec) then
      obj := RecordToSO(rec, SO)
    else if Supports(e, IwbElement, element) then
      obj := ElementToSO(element, SO);
    if Assigned(obj) then begin
      StrLCopy(json, PWideChar(obj.AsJSon), len);
      Result := true;
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
    Result := container.Assign(High(integer), nil, false);
end;

function SOToElement(element: IwbElement; obj: ISuperObject): IInterface;
const
  ArrayTypes: TSmashTypes = [stUnsortedArray, stUnsortedStructArray, stSortedArray,
    stSortedStructArray];
var
  container: IwbContainerElementRef;
  childElement: IwbElement;
  ary, paths: TSuperArray;
  path: string;
  i: Integer;
  v: Variant;
begin
  if not Assigned(element) or not Assigned(obj) then
    exit;
  if Supports(element, IwbContainerElementRef, container) then begin
    if GetSmashType(element) in ArrayTypes then begin
      ary := obj.AsArray;
      for i := 0 to Pred(ary.Length) do begin
        childElement := AssignElementIfMissing(container, i);
        SOToElement(childElement, ary.O[i]);
      end;
    end
    else begin
      paths := obj.AsObject.GetNames.AsArray;
      for i := 0 to Pred(paths.Length) do begin
        path := paths[i].AsString;
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

procedure ApplySO(container: IwbContainerElementRef; obj: ISuperObject; path: string);
begin
  SOToElement(container.ElementByPath[path], obj.O[path]);
end;

procedure SOToElements(container: IwbContainerElementRef; obj: ISuperObject;
  const excludedPaths: array of string);
var
  element: IwbElement;
  paths: TSuperArray;
  path: string;
  i: Integer;
begin
  paths := obj.AsObject.GetNames.AsArray;
  for i := 0 to Pred(paths.Length) do begin
    path := paths[i].AsString;
    if MatchStr(path, excludedPaths) then continue;
    element := AddElementIfMissing(container, path);
    SOToElement(element, obj.O[path]);
  end;
end;

procedure SOToRecordHeader(header: IwbElement; obj: ISuperObject);
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

function SOToRecord(rec: IwbMainRecord; obj: ISuperObject): IInterface;
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

function SOToGroup(group: IwbGroupRecord; obj: ISuperObject): IInterface;
var
  records: TSuperArray;
  recObj, recHeader: ISuperObject;
  e: IInterface;
  rec: IwbMainRecord;
  i: Integer;
begin
  Result := group;
  records := obj.AsArray;
  // loop through array of records
  for i := 0 to Pred(records.Length) do begin
    recObj := records.O[i];
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

procedure SOToFileHeader(header: IwbMainRecord; obj: ISuperObject);
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
  ary: TSuperArray;
  i: Integer;
begin
  if not Supports(header, IwbContainerElementRef, container)
  or not Assigned(obj) then
    exit;
  // add masters
  _file := header._File;
  ary := obj.A['Master Files'];
  for i := 0 to Pred(ary.Length) do
    _file.AddMasterIfMissing(ary.O[i].S['MAST - Filename']);
  // set record header and element values
  SOToRecordHeader(header.ElementByPath['Record Header'], obj.O['Record Header']);
  SOToElements(container, obj, ExcludedPaths);
end;

function SOToFile(_file: IwbFile; obj: ISuperObject): IInterface;
var
  groups: ISuperObject;
  group: IwbGroupRecord;
  signatures: TSuperArray;
  signature: string;
  i: Integer;
begin
  // create file if no file passed
  if not Assigned(_file) then
    _file := NativeAddFile(obj.S['Filename']);
  Result := _file;
  // deserialize header
  SOToFileHeader(_file.Header, obj.O['File Header']);
  // deserialize groups
  groups := obj.O['Groups'];
  signatures := groups.AsObject.GetNames.AsArray;
  for i := 0 to Pred(signatures.Length) do begin
    signature := signatures[i].AsString;
    group := AddGroupIfMissing(_file, signature);
    SOToGroup(group, groups.O[signature]);
  end;
end;

function ElementFromJson(_id: Cardinal; json: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  e, output: IInterface;
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  element: IwbElement;
  obj: ISuperObject;
begin
  Result := false;
  try
    obj := SO(json);
    if _id = 0 then
      output := SOToFile(nil, obj)
    else begin
      e := Resolve(_id);
      if Supports(e, IwbFile, _file) then
        output := SOToFile(_file, obj)
      else if Supports(e, IwbGroupRecord, group) then
        output := SOToGroup(group, obj)
      else if Supports(e, IwbMainRecord, rec) then
        output := SOToRecord(rec, obj)
      else if Supports(e, IwbElement, element) then
        output := SOToElement(element, obj);
    end;
    if Assigned(output) then begin
      _res^ := Store(output);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
