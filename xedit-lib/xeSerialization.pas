unit xeSerialization;

interface

  function FileToJson(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; cdecl;

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

procedure RecordToSO(rec: IwbMainRecord; obj: ISuperObject);
var
  i: Integer;
begin
  for i := Pred(rec.ElementCount) downto 0 do
    ElementToSO(rec.Elements[i], obj);
end;

function FileToSO(_file: IwbFile): ISuperObject;
var
  obj, groupsObj: ISuperObject;
  group: IwbGroupRecord;
  i: Integer;
  sig: String;
begin
  obj := SO;
  obj.S['Filename'] := _file.FileName;
  RecordToSO(_file.Header, obj);
  groupsObj := SO;
  for i := 1 to Pred(_file.ElementCount)  do begin
    if Supports(_file.Elements[i], IwbGroupRecord, group) then begin
      sig := String(TwbSignature(group.GroupLabel));
      groupsObj.O[sig] := SA([]);
    end;
  end;
  obj['Groups'] := groupsObj;
  Result := obj;
end;

function FileToJson(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; cdecl;
var
  _file: IwbFile;
  obj: ISuperObject;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      obj := FileToSO(_file);
      StrLCopy(desc, PWideChar(obj.AsJSon), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
