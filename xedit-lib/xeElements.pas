unit xeElements;

interface

uses
  //xedit units
  wbInterface,
  // xelib units
  xeMeta;

type
  TSmashType = ( stUnknown, stRecord, stString, stInteger, stFlag, stFloat,
    stStruct, stUnsortedArray, stUnsortedStructArray, stSortedArray,
    stSortedStructArray, stByteArray, stUnion );
  TSmashTypes = set of TSmashType;

  function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
  function GetElements(_id: Cardinal; _res: PCardinalArray): WordBool; cdecl;
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function NewElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl;
  function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl;
  function ElementCount(_id: Cardinal): Integer; cdecl;
  function ElementAssigned(_id: Cardinal): WordBool; cdecl;
  function Equals(_id, _id2: Cardinal): WordBool; cdecl;
  function IsMaster(_id: Cardinal): WordBool; cdecl;
  function IsInjected(_id: Cardinal): WordBool; cdecl;
  function IsOverride(_id: Cardinal): WordBool; cdecl;
  function IsWinningOverride(_id: Cardinal): WordBool; cdecl;

  // serialization and deserialization
  //function ElementToJson(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  //function ElementToXML(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  //function JsonToElement(_id: Cardinal; json: PWideChar; _res: Cardinal): WordBool; cdecl;
  //function XMLToElement(_id: Cardinal; xml: PWideChar; _res: Cardinal): WordBool; cdecl;

  // local functions
  function ResolveElement(e: IInterface; path: String; _res: PCardinal): WordBool;
  function GetSmashType(element: IwbElement): TSmashType;

implementation

uses
  Variants, Classes, SysUtils,
  // mte units
  mteHelpers,
  // xedit units
  wbImplementation,
  // xelib units
  xeMessages, xeFiles, xeSetup;


{******************************************************************************}
{ ELEMENT HANDLING
  Methods for handling elements: groups, records, and subrecords.
}
{******************************************************************************}

function ParseIndex(key: string; var index: Integer): Boolean;
begin
  Result := (key[1] = '[') and (key[Length(key)] = ']');
  if Result then
    index := StrToInt(Copy(key, 2, Length(key) - 2));
end;

procedure SplitPath(path: String; var key, nextPath: String);
var
  i: Integer;
begin
  i := Pos('\', path);
  if i > 0 then begin
    key := Copy(path, 1, i - 1);
    nextPath := Copy(path, i + 1, Length(path));
  end
  else
    key := path;
end;

function ResolveByIndex(container: IwbContainerElementRef; index: Integer; nextPath: String; _res: PCardinal): WordBool;
var
  element: IwbElement;
begin
  Result := False;
  // resolve element from container if container present
  if Assigned(container) then begin
    if index < container.ElementCount then
      element := container.Elements[index];
  end
  // else resolve file at index
  else
    element := NativeFileByIndex(index);

  if not Assigned(element) then exit;
  // resolve next element if nextPath is present
  if Length(nextPath) > 0 then
    Result := ResolveElement(element, nextPath, _res)
  // else store the element and return it
  else begin
    _res^ := Store(element);
    Result := True;
  end;
end;

function ResolveFileElement(path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  index: Integer;
  _file: IwbFile;
begin
  Result := False;
  SplitPath(path, key, nextPath);
  // resolve file by index if key is an index
  if ParseIndex(string(key), index) then
    Result := ResolveByIndex(nil, index, nextPath, _res)
  // else resolve by file name
  else begin
    _file := NativeFileByName(string(key));
    if not Assigned(_file) then exit;
    if Length(nextPath) > 0 then
      Result := ResolveElement(_file, nextPath, _res)
    else begin
      _res^ := Store(_file);
      Result := True;
    end;
  end;
end;

function ResolveGroupElement(_file: IwbFile; path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  index: Integer;
  group: IwbGroupRecord;
begin
  Result := False;
  SplitPath(path, key, nextPath);
  // resolve group by index if key is an index
  if ParseIndex(string(key), index) then
    Result := ResolveByIndex(_file as IwbContainerElementRef, index, nextPath, _res)
  // else resolve by group signature
  else begin
    // TODO: perhaps also by group name?
    group := _file.GroupBySignature[StrToSignature(key)];
    if not Assigned(group) then exit;
    if Length(nextPath) > 0 then
      Result := ResolveElement(group, nextPath, _res)
    else begin
      _res^ := Store(group);
      Result := True;
    end;
  end;
end;

function ResolveRecordElement(group: IwbGroupRecord; path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  index: Integer;
  rec: IwbMainRecord;
begin
  Result := False;
  SplitPath(path, key, nextPath);
  // resolve record by index if key is an index
  if ParseIndex(string(key), index) then
    Result := ResolveByIndex(group as IwbContainerElementRef, index, nextPath, _res)
  // else resolve record by formID
  else begin
    rec := group.MainRecordByFormID[StrToInt('$' + key)];
    if not Assigned(rec) then exit;
    if Length(nextPath) > 0 then
      Result := ResolveElement(rec, nextPath, _res)
    else begin
      _res^ := Store(rec);
      Result := True;
    end;
  end;
end;

function ResolveElement(e: IInterface; path: String; _res: PCardinal): WordBool;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  container: IwbContainerElementRef;
  element: IwbElement;
begin
  Result := False;
  if Supports(e, IwbFile, _file) then
    Result := ResolveGroupElement(_file, path, _res)
  else if Supports(e, IwbGroupRecord, group) then
    Result := ResolveRecordElement(group, path, _res)
  else if Supports(e, IwbContainerElementRef, container) then begin
    element := container.ElementByPath[path];
    if not Assigned(element) then exit;
    _res^ := Store(element);
    Result := True;
  end;
end;

// Replaces ElementByName, ElementByPath, ElementByIndex, GroupBySignature, and
// ElementBySignature.  Supports indexed paths.
function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    if _id = 0 then
      Result := ResolveFileElement(string(key), _res)
    else
      Result := ResolveElement(Resolve(_id), string(key), _res);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFiles(_res: PCardinalArray): WordBool;
var
  i: Integer;
begin
  SetLength(_res^, Length(Files));
  for i := Low(Files) to High(Files) do
    _res^[i] := Store(Files[i]);
  Result := True;
end;

function GetChildrenElements(_id: Cardinal; _res: PCardinalArray): WordBool;
var
  i: Integer;
  container: IwbContainerElementRef;
begin
  Result := False;
  if Supports(Resolve(_id), IwbContainerElementRef, container) then begin
    SetLength(_res^, container.ElementCount);
    for i := 0 to Pred(container.ElementCount) do
      _res^[i] := Store(container.Elements[i]);
    Result := True;
  end;
end;

// returns an array of handles for the elements in a container
function GetElements(_id: Cardinal; _res: PCardinalArray): WordBool; cdecl;
begin
  Result := False;
  try
    if _id = 0 then
      Result := GetFiles(_res)
    else
      Result := GetChildrenElements(_id, _res);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      _res^ := Store(element._File);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile) then exit;
    if Supports(e, IwbElement, element) and Assigned(element.Container) then begin
      _res^ := Store(element.Container);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function NewContainerElement(_id: Cardinal; key: string): IwbElement;
var
  e: IInterface;
  container: IwbContainerElementRef;
  keyIndex: Integer;
begin
  e := Resolve(_id);
  if not Supports(e, IwbContainerElementRef, container) then exit;
  // Use Add for files and groups
  if Supports(e, IwbFile) or Supports(e, IwbGroupRecord) then
    Result := container.Add(key, true)
  else begin
    // no key means we're assigning an element at the end of the array
    if Length(key) = 0 then
      Result := container.Assign(High(integer), nil, false)
    else begin
      // assign element at given index if index given, else add
      if ParseIndex(key, keyIndex) then begin
        Result := container.Assign(High(integer), nil, false);
        Result.Remove;
        container.InsertElement(keyIndex, Result);
      end
      else
        Result := container.Add(key, true);
    end;
  end;
end;

// replaces ElementAssign, Add, AddElement, and InsertElement
function NewElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if _id = 0 then
      element := NewFileElement(string(key))
    else
      element := NewContainerElement(_id, string(key));

    // store and return element if assigned
    if Assigned(element) then begin
      _res^ := Store(element);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  container: IwbContainerElementRef;
begin
  Result := false;
  try
    e := Resolve(_id);
    if not Supports(e, IwbElement, element) then
      exit;
    if (not Assigned(key)) or (Length(key) = 0) then
      element.Remove
    else begin
      if not Supports(e, IwbContainerElementRef, container) then
        exit;
      container.ElementByPath[key].Remove;
    end;
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  element, linkedElement: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      linkedElement := element.LinksTo;
      if Assigned(linkedElement) then begin
        _res^ := Store(linkedElement);
        Result := true;
      end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// Replaces HasGroup and ElementExists
function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl;
var
  element: IInterface;
  _file: IwbFile;
  container: IwbContainerElementRef;
begin
  Result := false;
  try
    element := Resolve(_id);
    if Supports(element, IwbFile, _file) then
      // TODO: perhaps also by group name?
      Result := _file.HasGroup(StrToSignature(key))
    else if Supports(element, IwbContainerElementRef, container) then
      // TODO: adjust logic here so we can check paths
      Result := container.ElementExists[string(key)];
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementCount(_id: Cardinal): Integer; cdecl;
var
  container: IwbContainerElementRef;
begin
  Result := -1;
  try
    if Supports(Resolve(_id), IwbContainerElementRef, container) then
      Result := container.ElementCount;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementAssigned(_id: Cardinal): WordBool; cdecl;
var
  e: IInterface;
begin
  Result := false;
  try
    e := Resolve(_id);
    Result := Assigned(e);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function Equals(_id, _id2: Cardinal): WordBool; cdecl;
var
  element, element2: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then
      if Supports(Resolve(_id2), IwbElement, element2) then
        Result := element.Equals(element2);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsMaster(_id: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := rec.IsMaster;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsInjected(_id: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := rec.IsInjected;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsOverride(_id: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := not rec.IsMaster;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// TODO: Determine if subrecord is winner
function IsWinningOverride(_id: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := not rec.IsWinningOverride;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{ Returns true if @e is a sorted container }
function IsSorted(e: IwbElement): boolean;
var
  Container: IwbSortableContainer;
begin
  Result := false;
  if Supports(e, IwbSortableContainer, Container) then
    Result := Container.Sorted;
end;

{ Returns true if @e is a container with struct children }
function HasStructChildren(e: IwbElement): boolean;
var
  Container: IwbContainerElementRef;
begin
  Result := false;
  if Supports(e, IwbContainerElementRef, Container)
  and (Container.ElementCount > 0) then
    Result := GetSmashType(Container.Elements[0]) = stStruct;
end;

function GetSmashType(element: IwbElement): TSmashType;
var
  subDef: IwbSubRecordDef;
  dt: TwbDefType;
  bIsSorted, bHasStructChildren: boolean;
begin
  dt := element.Def.DefType;
  if Supports(element.Def, IwbSubRecordDef, subDef) then
    dt := subDef.Value.DefType;

  case Ord(dt) of
    Ord(dtRecord): Result := stRecord;
    Ord(dtSubRecord): Result := stUnknown;
    Ord(dtSubRecordStruct): Result := stStruct;
    Ord(dtSubRecordUnion): Result := stUnion;
    Ord(dtString): Result := stString;
    Ord(dtLString): Result := stString;
    Ord(dtLenString): Result := stString;
    Ord(dtByteArray): Result := stByteArray;
    Ord(dtInteger): Result := stInteger;
    Ord(dtIntegerFormater): Result := stInteger;
    Ord(dtIntegerFormaterUnion): Result := stInteger;
    Ord(dtFlag): Result := stFlag;
    Ord(dtFloat): Result := stFloat;
    Ord(dtSubRecordArray), Ord(dtArray): begin
      bIsSorted := IsSorted(element);
      bHasStructChildren := HasStructChildren(element);
      if bIsSorted then begin
        if bHasStructChildren then
          Result := stSortedStructArray
        else
          Result := stSortedArray;
      end
      else begin
        if bHasStructChildren then
          Result := stUnsortedStructArray
        else
          Result := stUnsortedArray;
      end;
    end;
    Ord(dtStruct): Result := stStruct;
    Ord(dtUnion): Result := stUnion;
    Ord(dtEmpty): Result := stUnknown;
    Ord(dtStructChapter): Result := stStruct;
    else Result := stUnknown;
  end;
end;

end.
