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
  function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function ElementAssigned(_id: Cardinal): WordBool; cdecl;
  function Equals(_id, _id2: Cardinal): WordBool; cdecl;
  function CopyElement(_id, _id2: Cardinal; aAsNew, aDeepCopy: WordBool; _res: PCardinal): WordBool; cdecl;
  function IsMaster(_id: Cardinal): WordBool; cdecl;
  function IsInjected(_id: Cardinal): WordBool; cdecl;
  function IsOverride(_id: Cardinal): WordBool; cdecl;
  function IsWinningOverride(_id: Cardinal): WordBool; cdecl;

  // native functions
  function ResolveFromGroup(group: IwbGroupRecord; path: String; _res: PCardinal): WordBool;
  function ResolveElement(e: IInterface; path: String; _res: PCardinal): WordBool;
  function NativeContainer(element: IwbElement): IwbContainer;
  function IsArray(element: IwbElement): Boolean;
  function GetDefType(element: IwbElement): TwbDefType;
  function GetSmashType(element: IwbElement): TSmashType;

implementation

uses
  Variants, Classes, SysUtils,
  // mte units
  mteHelpers,
  // xedit units
  wbImplementation,
  // xelib units
  xeMessages, xeFiles, xeGroups, xeSetup;


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

function IsHexStr(key: String): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 1 to Length(key) do
    if not CharInSet(key[i], ['A'..'F','0'..'9']) then exit;
  Result := true;
end;

function ParseFormID(key: String; var formID: Cardinal): Boolean;
begin
  Result := (Length(key) = 8) and IsHexStr(key);
  if Result then
    formID := StrToInt('$' + key);
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
  // else resolve file at index
  if Assigned(container) then begin
    if index < container.ElementCount then
      element := container.Elements[index];
  end
  else
    element := NativeFileByIndex(index);

  if not Assigned(element) then exit;
  // resolve next element if nextPath is present
  // else store the element and return it
  if Length(nextPath) > 0 then
    Result := ResolveElement(element, nextPath, _res)
  else begin
    _res^ := Store(element);
    Result := True;
  end;
end;

function ResolveRecord(_file: IwbFile; formID: Cardinal; path: String; _res: PCardinal): WordBool;
var
  rec: IwbMainRecord;
begin
  Result := False;
  rec := _file.RecordByFormID[formID, true];
  if not Assigned(rec) then exit;
  if Length(path) > 0 then
    Result := ResolveElement(rec, path, _res)
  else begin
    _res^ := Store(rec);
    Result := True;
  end;
end;

function ResolveChildGroup(rec: IwbMainRecord; nextPath: String; _res: PCardinal): WordBool;
var
  group: IwbGroupRecord;
begin
  group := rec.ChildGroup;
  if Length(nextPath) > 0 then
    Result := ResolveFromGroup(group, nextPath, _res)
  else begin
    _res^ := Store(group);
    Result := True;
  end;
end;

function ResolveGroup(_file: IwbFile; sig: TwbSignature; nextPath: String; _res: PCardinal): WordBool;
var
  group: IwbGroupRecord;
begin
  // TODO: perhaps also by group name?
  Result := False;
  group := _file.GroupBySignature[sig];
  if not Assigned(group) then exit;
  if Length(nextPath) > 0 then
    Result := ResolveElement(group, nextPath, _res)
  else begin
    _res^ := Store(group);
    Result := True;
  end;
end;

function ResolveFile(fileName, nextPath: String; _res: PCardinal): WordBool;
var
  _file: IwbFile;
begin
  Result := False;
  _file := NativeFileByName(fileName);
  if not Assigned(_file) then exit;
  if Length(nextPath) > 0 then
    Result := ResolveElement(_file, nextPath, _res)
  else begin
    _res^ := Store(_file);
    Result := True;
  end;
end;

function ResolveFromContainer(container: IwbContainerElementRef; path: String; _res: PCardinal): WordBool;
var
  element: IwbElement;
begin
  Result := False;
  element := container.ElementByPath[path];
  if Assigned(element) then begin
    _res^ := Store(element);
    Result := True;
  end;
end;

function ResolveFromRecord(rec: IwbMainRecord; path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  container: IwbContainerElementRef;
begin
  Result := False;
  SplitPath(path, key, nextPath);
  if SameText(key, 'Child Group') then
    Result := ResolveChildGroup(rec, nextPath, _res)
  else if Supports(rec, IwbContainerElementRef, container) then
    Result := ResolveFromContainer(container, path, _res);
end;

function ResolveFromGroup(group: IwbGroupRecord; path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  index: Integer;
  formID: Cardinal;
begin
  Result := False;
  SplitPath(path, key, nextPath);
  // resolve record by index if key is an index
  // else resolve record by formID
  if ParseIndex(key, index) then
    Result := ResolveByIndex(group as IwbContainerElementRef, index, nextPath, _res)
  else if ParseFormID(key, formID) then
    Result := ResolveRecord(group._File, formID, nextPath, _res);
end;

function ResolveFromFile(_file: IwbFile; path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  index: Integer;
  formID: Cardinal;
begin
  SplitPath(path, key, nextPath);
  // resolve group by index if key is an index
  // else resolve record by formID if key is a formID
  // else resolve by group signature
  if ParseIndex(key, index) then
    Result := ResolveByIndex(_file as IwbContainerElementRef, index, nextPath, _res)
  else if ParseFormID(key, formID) then
    Result := ResolveRecord(_file, formID, nextPath, _res)
  else 
    Result := ResolveGroup(_file, StrToSignature(key), nextPath, _res);
end;

function ResolveFromRoot(path: String; _res: PCardinal): WordBool;
var
  key, nextPath: String;
  index: Integer;
begin
  SplitPath(path, key, nextPath);
  // resolve file by index if key is an index
  // else resolve by file name
  if ParseIndex(key, index) then
    Result := ResolveByIndex(nil, index, nextPath, _res)
  else 
    Result := ResolveFile(key, nextPath, _res);
end;

function ResolveElement(e: IInterface; path: String; _res: PCardinal): WordBool;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  container: IwbContainerElementRef;
begin
  Result := False;
  if Supports(e, IwbFile, _file) then
    Result := ResolveFromFile(_file, path, _res)
  else if Supports(e, IwbGroupRecord, group) then
    Result := ResolveFromGroup(group, path, _res)
  else if Supports(e, IwbMainRecord, rec) then
    Result := ResolveFromRecord(rec, path, _res) 
  else if Supports(e, IwbContainerElementRef, container) then
    Result := ResolveFromContainer(container, path, _res);
end;

// Replaces ElementByName, ElementByPath, ElementByIndex, GroupBySignature, and
// ElementBySignature.  Supports indexed paths.
function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
begin
  Result := False;
  try
    if _id = 0 then
      Result := ResolveFromRoot(string(key), _res)
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

function NativeContainer(element: IwbElement): IwbContainer;
var
  group: IwbGroupRecord;
begin
  if Supports(element, IwbGroupRecord, group) and IsChildGroup(group) then
    Result := group.ChildrenOf as IwbContainer
  else
    Result := element.Container;
  if not Assigned(Result) then
    raise Exception.Create('Could not find container for ' + element.Name);
end;

function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
begin
  Result := False;
  try
    e := Resolve(_id);
    if not Supports(e, IwbFile) and Supports(e, IwbElement, element) then begin
      _res^ := Store(NativeContainer(element));
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
    if (not Assigned(key)) or (Length(string(key)) = 0) then
      element.Remove
    else begin
      if not Supports(e, IwbContainerElementRef, container) then
        exit;
      container.ElementByPath[string(key)].Remove;
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
      Result := _file.HasGroup(StrToSignature(string(key)))
    else if Supports(element, IwbContainerElementRef, container) then
      // TODO: adjust logic here so we can check paths
      Result := container.ElementExists[string(key)];
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
var
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbContainerElementRef, container) then begin
      count^ := container.ElementCount;
      Result := True;
    end;
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

function CopyElement(_id, _id2: Cardinal; aAsNew, aDeepCopy: WordBool; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
  rec: IwbMainRecord;
  element: IwbElement;
begin
  Result := false;
  try
    if not Supports(Resolve(_id), IwbElement, element) then exit;
    if Supports(Resolve(_id2), IwbFile, _file) then begin
      _res^ := Store(wbCopyElementToFile(element, _file, aAsNew, aDeepCopy, '', '', ''));
      Result := true;
    end
    else if Supports(Resolve(_id2), IwbMainRecord, rec) then begin
      _res^ := Store(wbCopyElementToRecord(element, rec, aAsNew, aDeepCopy));
      Result := true;
    end;
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

function IsArray(element: IwbElement): Boolean;
begin
  Result := GetDefType(element) in [dtSubRecordArray, dtArray];
end;

function GetDefType(element: IwbElement): TwbDefType;
var
  subDef: IwbSubRecordDef;
begin
  if Supports(element.Def, IwbSubRecordDef, subDef) then
    Result := subDef.Value.DefType
  else
    Result := element.Def.DefType;
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
