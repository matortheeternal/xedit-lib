unit xeSerialization;

interface

  function ElementToJson(_id: Cardinal; json: PWideChar; len: Integer): WordBool; cdecl;

implementation

uses
  Variants, SysUtils,
  superobject,
  wbInterface,
  xeMeta, xeElements, xeMessages;

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
  obj, groupsObj: ISuperObject;
  group: IwbGroupRecord;
  i: Integer;
begin
  obj := SO;
  obj.S['Filename'] := _file.FileName;
  RecordToSO(_file.Header, obj);
  groupsObj := SO;
  for i := 1 to Pred(_file.ElementCount)  do begin
    if Supports(_file.Elements[i], IwbGroupRecord, group) then
      GroupToSO(group, groupsObj);
  end;
  obj['Groups'] := groupsObj;
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

end.
