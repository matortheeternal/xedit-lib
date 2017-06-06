unit xeElements;

interface

uses
  Classes,
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
  function GetElements(_id: Cardinal; key: PWideChar; len: PInteger): WordBool; cdecl;
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function AddElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl;
  function RemoveElementOrParent(_id: Cardinal): WordBool; cdecl;
  function GetLinksTo(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
  function ElementExists(_id: Cardinal; key: PWideChar; bool: PWordBool): WordBool; cdecl;
  function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function ElementEquals(_id, _id2: Cardinal; bool: PWordBool): WordBool; cdecl;
  function CopyElement(_id, _id2: Cardinal; aAsNew, aDeepCopy: WordBool; _res: PCardinal): WordBool; cdecl;
  function MoveElementToIndex(_id: Cardinal; index: Integer): WordBool; cdecl;
  function GetExpectedSignatures(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function SortKey(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function ElementType(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function DefType(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function SmashType(_id: Cardinal; enum: PByte): WordBool; cdecl;

  // native functions
  function ResolveFromGroup(group: IwbGroupRecord; path: String): IInterface;
  function ResolveElement(e: IInterface; path: String): IInterface;
  function NativeGetElement(_id: Cardinal; key: PWideChar): IInterface;
  function NativeGetElementEx(_id: Cardinal; key: PWideChar): IwbElement;
  procedure NativeMoveElementToIndex(element: IwbElement; index: Integer);
  function NativeContainer(element: IwbElement): IwbContainer;
  function CreateFromGroup(group: IwbGroupRecord; path: String): IInterface;
  function CreateElement(e: IInterface; path: String): IInterface;
  function NativeAddElement(_id: Cardinal; key: string): IInterface;
  function IsSorted(e: IwbElement): Boolean;
  function IsArray(element: IwbElement): Boolean;
  function GetFlagsDef(element: IwbElement; var flagsDef: IwbFlagsDef): Boolean;
  function GetDefType(element: IwbElement): TwbDefType;
  function GetSmashType(element: IwbElement): TSmashType;

implementation

uses
  Variants, SysUtils,
  // mte units
  mteHelpers,
  // xedit units
  wbImplementation,
  // xelib units
  xeMessages, xeFiles, xeMasters, xeGroups, xeRecords, xeElementValues, xeSetup;


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
  Result := False;
  for i := 1 to Length(key) do
    if not CharInSet(key[i], ['A'..'F','0'..'9']) then exit;
  Result := True;
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

function ResolveByIndex(container: IwbContainerElementRef; index: Integer; nextPath: String): IInterface;
begin
  // resolve element from container if container present
  // else resolve file at index
  if Assigned(container) then begin
    if index < container.ElementCount then
      Result := container.Elements[index];
  end
  else begin
    if index <= High(xFiles) then
      Result := NativeFileByIndex(index);
  end;

  // resolve next element if nextPath is present
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveElement(Result, nextPath);
end;

function ResolveFromContainer(container: IwbContainerElementRef; path: String): IInterface;
begin
  Result := container.ElementByPath[path];
end;

function ResolveChildGroup(rec: IwbMainRecord; nextPath: String): IInterface;
begin
  Result := rec.ChildGroup;
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveFromGroup(Result as IwbGroupRecord, nextPath);
end;

function ResolveFromRecord(rec: IwbMainRecord; path: String): IInterface;
var
  key, nextPath: String;
  container: IwbContainerElementRef;
begin
  Result := nil;
  SplitPath(path, key, nextPath);
  if SameText(key, 'Child Group') then
    Result := ResolveChildGroup(rec, nextPath)
  else if Supports(rec, IwbContainerElementRef, container) then
    Result := ResolveFromContainer(container, path);
end;

function ResolveRecord(_file: IwbFile; formID: Cardinal; nextPath: String): IInterface; overload;
begin
  Result := _file.RecordByFormID[formID, True];
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveElement(Result, nextPath);
end;

function ResolveRecord(group: IwbGroupRecord; key, nextPath: String): IInterface; overload;
var
  name: String;
begin
  if (key[1] = '"') and (key[Length(key)] = '"') then begin
    name := Copy(key, 2, Length(key) - 2);
    Result := FindRecordByName(group, name);
  end
  else
    Result := group._File.RecordByEditorID[key];
  if Assigned(Result) and (nextPath <> '') then
    ResolveFromRecord(Result as IwbMainRecord, nextPath);
end;

function ResolveFromGroup(group: IwbGroupRecord; path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
  formID: Cardinal;
begin
  SplitPath(path, key, nextPath);
  // resolve record by index if key is an index
  // else resolve record by formID
  // TODO: resolve record by name/editorID
  if ParseIndex(key, index) then
    Result := ResolveByIndex(group as IwbContainerElementRef, index, nextPath)
  else if ParseFormID(key, formID) then
    Result := ResolveRecord(group._File, formID, nextPath)
  else
    Result := ResolveRecord(group, key, nextPath);
end;

function ResolveGroup(_file: IwbFile; key: String; nextPath: String): IInterface;
begin
  if Length(key) > 4 then
    key := NativeSignatureFromName(key);
  Result := _file.GroupBySignature[StrToSignature(key)];
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveFromGroup(Result as IwbGroupRecord, nextPath);
end;

function ResolveFromFile(_file: IwbFile; path: String): IInterface;
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
    Result := ResolveByIndex(_file as IwbContainerElementRef, index, nextPath)
  else if ParseFormID(key, formID) then
    Result := ResolveRecord(_file, formID, nextPath)
  else if key = 'File Header' then
    Result := ResolveFromRecord(_file.Header, nextPath)
  else 
    Result := ResolveGroup(_file, key, nextPath);
end;

function ResolveFile(fileName, nextPath: String): IInterface;
begin
  Result := NativeFileByName(fileName);
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveFromFile(Result as IwbFile, nextPath);
end;

function ResolveFromRoot(path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
begin
  SplitPath(path, key, nextPath);
  // resolve file by index if key is an index
  // else resolve by file name
  if ParseIndex(key, index) then
    Result := ResolveByIndex(nil, index, nextPath)
  else 
    Result := ResolveFile(key, nextPath);
end;

function ResolveElement(e: IInterface; path: String): IInterface;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  container: IwbContainerElementRef;
begin
  Result := nil;
  if Supports(e, IwbFile, _file) then
    Result := ResolveFromFile(_file, path)
  else if Supports(e, IwbGroupRecord, group) then
    Result := ResolveFromGroup(group, path)
  else if Supports(e, IwbMainRecord, rec) then
    Result := ResolveFromRecord(rec, path)
  else if Supports(e, IwbContainerElementRef, container) then
    Result := ResolveFromContainer(container, path);
end;

function NativeGetElement(_id: Cardinal; key: PWideChar): IInterface;
begin
  if string(key) = '' then
    Result := Resolve(_id)
  else if _id = 0 then
    Result := ResolveFromRoot(string(key))
  else
    Result := ResolveElement(Resolve(_id), string(key));
end;

function NativeGetElementEx(_id: Cardinal; key: PWideChar): IwbElement;
var
  e: IInterface;
begin
  e := NativeGetElement(_id, key);
  if not Supports(e, IwbElement, Result) then
    raise Exception.Create('Failed to resolve element at path: ' + string(key));
end;

// Has the functionality of ElementByName, ElementByPath, ElementByIndex,
// ElementBySignature, GroupBySignature, FileByName, FileByIndex, RecordByFormID,
// RecordByIndex, and supports indexed paths.
function GetElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, key);
    _res^ := Store(element as IInterface);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

procedure GetFiles(len: PInteger);
var
  i: Integer;
begin
  len^ := High(xFiles) + 1;
  SetLength(resultArray, len^);
  for i := 0 to High(xFiles) do
    resultArray[i] := Store(xFiles[i]);
end;

procedure GetContainerElements(container: IwbContainerElementRef; len: PInteger);
var
  i: Integer;
begin
  len^ := container.ElementCount;
  SetLength(resultArray, len^);
  for i := 0 to Pred(container.ElementCount) do
    resultArray[i] := Store(container.Elements[i]);
end;

procedure GetChildrenElements(element: IInterface; len: PInteger);
var
  container: IwbContainerElementRef;
begin
  if not Supports(element, IwbContainerElementRef, container) then
    raise Exception.Create('Interface must be a container.');
  GetContainerElements(container, len);
end;

// returns an array of handles for the elements in a container
function GetElements(_id: Cardinal; key: PWideChar; len: PInteger): WordBool; cdecl;
begin
  Result := False;
  try
    if (_id = 0) and (key = '') then
      GetFiles(len)
    else
      GetChildrenElements(NativeGetElementEx(_id, key), len);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    _res^ := Store(element._File);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

procedure NativeMoveElementToIndex(element: IwbElement; index: Integer);
var
  container: IwbContainerElementRef;
begin
  container := element.Container as IwbContainerElementRef;
  if not IsArray(container) then
    raise Exception.Create('Cannot move elements in non-array containers.');
  if IsSorted(container) then
    raise Exception.Create('Cannot move elements in sorted arrays.');
  if index > container.IndexOf(element) then
    Dec(index);
  element.Remove;
  container.InsertElement(index, element);
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
    if Supports(e, IwbFile) then
      raise Exception.Create('Cannot call GetContainer on files.');
    if not Supports(e, IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    _res^ := Store(NativeContainer(element));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function CreateByIndex(container: IwbContainerElementRef; index: Integer; nextPath: String): IInterface;
begin
  // resolve element from container if container present
  // else resolve file at index
  if Assigned(container) then begin
    if index < container.ElementCount then
      Result := container.Elements[index];
  end
  else begin
    if index <= High(xFiles) then
      Result := NativeFileByIndex(index);
  end;

  // create next element if nextPath is present
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateElement(Result, nextPath);
end;

function CreateFromContainer(container: IwbContainerElementRef; path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
  nextContainer: IwbContainerElementRef;
begin
  SplitPath(path, key, nextPath);
  // . corresponds to appending a new element
  // ^ corresponds to inserting an element at an index, e.g. ^3 inserts at index 3
  // else we get element at key/create it if missing
  if (key = '') or (key = '.') then
    Result := container.Assign(High(Integer), nil, false)
  else if key[1] = '^' then begin
    Result := container.Assign(High(Integer), nil, false);
    index := StrToInt(Copy(key, 2, Length(key) - 1));
    NativeMoveElementToIndex(Result as IwbElement, index);
  end
  else begin
    Result := container.ElementByPath[key];
    if not Assigned(Result) then
      Result := container.Add(key);
  end;
  // recurse for next path if present
  if Assigned(Result) and (nextPath <> '') then begin
    if not Supports(Result, IwbContainerElementRef, nextContainer) then
      raise Exception.Create('Failed to traverse into ' + nextPath);
    Result := CreateFromContainer(nextContainer, nextPath);
  end;
end;

function CreateChildGroup(rec: IwbMainRecord; nextPath: String): IInterface;
begin
  Result := rec.ChildGroup;
  if not Assigned(Result) then
    raise Exception.Create('Child group not found for ' + rec.Name);
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromGroup(Result as IwbGroupRecord, nextPath);
end;

function CreateFromRecord(rec: IwbMainRecord; path: String): IInterface;
var
  key, nextPath: String;
begin
  SplitPath(path, key, nextPath);
  if SameText(key, 'Child Group') then
    Result := CreateChildGroup(rec, nextPath)
  else
    Result := CreateFromContainer(rec as IwbContainerElementRef, path);
end;

function OverrideRecord(targetFile: IwbFile; formID: Cardinal; sig: TwbSignature): IwbMainRecord;
var
  f: IwbFile;
  rec: IwbMainRecord;
begin
  f := NativeFileByLoadOrder(formID shr 24);
  if not Assigned(f) then
    raise Exception.Create(Format('Failed to find file at load order %s to ' +
      'copy record %s from', [IntToHex(formID shr 24, 2), IntToHex(formID, 8)]));
  rec := f.RecordByFormID[formID, false];
  if not Assigned(rec) then
    raise Exception.Create(Format('Failed to find record %s in file %s',
      [IntToHex(formID, 8), f.FileName]));
  NativeAddRequiredMasters(rec as IwbElement, f, false);
  Result := wbCopyElementToFile(rec, targetFile, false, true, '', '', '') as IwbMainRecord;
end;

function CreateRecord(group: IwbGroupRecord; formID: Cardinal; nextPath: String): IInterface; overload;
var
  sig: TwbSignature;
begin
  sig := TwbSignature(group.GroupLabel);
  Result := group._File.RecordByFormID[formID, true];
  if not Assigned(Result) then
    Result := OverrideRecord(group._File, formID, sig);
  if Assigned(Result) and ((Result as IwbMainRecord).Signature <> sig) then
    raise Exception.Create(Format('Found record %s does not match expected ' +
      'signature %s.', [(Result as IwbMainRecord).Name, string(sig)]));
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromRecord(Result as IwbMainRecord, nextPath);
end;

function CreateRecord(group: IwbGroupRecord; key, nextPath: String): IwbMainRecord; overload;
begin
  Result := group.Add(key) as IwbMainRecord;
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromRecord(Result, nextPath) as IwbMainRecord;
end;

function CreateFromGroup(group: IwbGroupRecord; path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
  formID: Cardinal;
begin
  SplitPath(path, key, nextPath);
  if key = '.' then
    key := String(AnsiString(group.GroupLabel));
  // resolve record by index if key is an index
  // else resolve record by formID
  // else create new record by signature
  if ParseIndex(key, index) then
    Result := CreateByIndex(group as IwbContainerElementRef, index, nextPath)
  else if ParseFormID(key, formID) then
    Result := CreateRecord(group, formID, nextPath)
  else
    Result := CreateRecord(group, key, nextPath);
end;

function CreateGroup(_file: IwbFile; key: String; nextPath: String): IInterface;
begin
  if Length(key) > 4 then
    key := NativeSignatureFromName(key);
  Result := AddGroupIfMissing(_file, key);
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromGroup(Result as IwbGroupRecord, nextPath);
end;

function CreateFromFile(_file: IwbFile; path: String): IInterface;
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
    Result := CreateByIndex(_file as IwbContainerElementRef, index, nextPath)
  else if ParseFormID(key, formID) then
    Result := CreateFromRecord(_file.RecordByFormID[formID, false], nextPath)
  else if key = 'File Header' then
    Result := CreateFromRecord(_file.Header, nextPath)
  else
    Result := CreateGroup(_file, key, nextPath);
end;

function CreateFile(fileName, nextPath: String): IInterface;
begin
  Result := NativeFileByName(fileName);
  if not Assigned(Result) then
    Result := NativeAddFile(fileName);
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromFile(Result as IwbFile, nextPath);
end;

function CreateFromRoot(path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
begin
  SplitPath(path, key, nextPath);
  // resolve file by index if key is an index
  // else resolve by file name
  if ParseIndex(key, index) then
    Result := CreateByIndex(nil, index, nextPath)
  else
    Result := CreateFile(key, nextPath);
end;

function CreateElement(e: IInterface; path: String): IInterface;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
  container: IwbContainerElementRef;
begin
  Result := nil;
  if Supports(e, IwbFile, _file) then
    Result := CreateFromFile(_file, path)
  else if Supports(e, IwbGroupRecord, group) then
    Result := CreateFromGroup(group, path)
  else if Supports(e, IwbMainRecord, rec) then
    Result := CreateFromRecord(rec, path)
  else if Supports(e, IwbContainerElementRef, container) then
    Result := CreateFromContainer(container, path);
end;

function NativeAddElement(_id: Cardinal; key: String): IInterface;
begin
  if _id = 0 then
    Result := CreateFromRoot(key)
  else
    Result := CreateElement(Resolve(_id), key);
end;

// replaces ElementAssign, Add, AddElement, and InsertElement
function AddElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IInterface;
begin
  Result := False;
  try
    element := NativeAddElement(_id, string(key));
    if not Assigned(element) then
      raise Exception.Create('Failed to add element at path: ' + string(key));
    _res^ := Store(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
begin
  Result := False;
  try
    e := NativeGetElement(_id, key);
    if not Supports(e, IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    element.Remove;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveElementOrParent(_id: Cardinal): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  container: IwbContainer;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile) or Supports(e, IwbGroupRecord)
    or Supports(e, IwbMainRecord) then
      raise Exception.Create('Interface cannot be a file, group, or main record.');
    if not Supports(e, IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    container := element.Container;
    while not container.IsElementRemoveable(element) do begin
      if Supports(container, IwbMainRecord) then
        raise Exception.Create('Reached main record - could not remove.');
      if container.IsElementRemoveable(element) then
        break;
      element := container as IwbElement;
      container := element.Container;
    end;
    container.RemoveElement(container.IndexOf(element));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLinksTo(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element, linkedElement: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElementEx(_id, key);
    linkedElement := element.LinksTo;
    if not Assigned(linkedElement) then
      raise Exception.Create('Failed to resolve linked element.');
    _res^ := Store(linkedElement);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementExists(_id: Cardinal; key: PWideChar; bool: PWordBool): WordBool; cdecl;
var
  e: IInterface;
begin
  Result := False;
  try
    e := NativeGetElement(_id, key);
    bool^ := Assigned(e);
    Result := True;
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
    if _id = 0 then
      count^ := High(xFiles) + 1
    else if Supports(Resolve(_id), IwbContainerElementRef, container) then
      count^ := container.ElementCount
    else
      count^ := 0;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementEquals(_id, _id2: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element, element2: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('First interface is not an element.');
    if not Supports(Resolve(_id2), IwbElement, element2) then
      raise Exception.Create('Second interface is not an element.');
    bool^ := element.Equals(element2);
    Result := True;
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
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if Supports(Resolve(_id2), IwbFile, _file) then
      _res^ := Store(wbCopyElementToFile(element, _file, aAsNew, aDeepCopy, '', '', ''))
    else if Supports(Resolve(_id2), IwbMainRecord, rec) then
      _res^ := Store(wbCopyElementToRecord(element, rec, aAsNew, aDeepCopy))
    else
      raise Exception.Create('Second interface must be a file or a main record.');
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function MoveElementToIndex(_id: Cardinal; index: Integer): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    NativeMoveElementToIndex(element, index);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetExpectedSignatures(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
  integerDef: IwbIntegerDef;
  formDef: IwbFormIDChecked;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if not Supports(element.ValueDef, IwbIntegerDef, integerDef)
    or not Supports(integerDef.Formater[element], IwbFormID) then
      raise Exception.Create('Interface must be able to hold a FormID value.');
    if Supports(integerDef.Formater[element], IwbFormIDChecked, formDef) then
      resultStr := formDef.SignaturesText
    else
      resultStr := '*';
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

function ElementType(_id: Cardinal; enum: PByte): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    enum^ := Ord(element.ElementType);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function DefType(_id: Cardinal; enum: PByte): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    enum^ := Ord(GetDefType(element));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SmashType(_id: Cardinal; enum: PByte): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    enum^ := Ord(GetSmashType(element));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{ Returns true if @e is a sorted container }
function IsSorted(e: IwbElement): Boolean;
var
  Container: IwbSortableContainer;
begin
  Result := False;
  if Supports(e, IwbSortableContainer, Container) then
    Result := Container.Sorted;
end;

{ Returns true if @e is a container with struct children }
function HasStructChildren(e: IwbElement): boolean;
var
  Container: IwbContainerElementRef;
begin
  Result := False;
  if Supports(e, IwbContainerElementRef, Container)
  and (Container.ElementCount > 0) then
    Result := GetSmashType(Container.Elements[0]) = stStruct;
end;

function IsArray(element: IwbElement): Boolean;
begin
  Result := GetDefType(element) in [dtSubRecordArray, dtArray];
end;

function GetFlagsDef(element: IwbElement; var flagsDef: IwbFlagsDef): Boolean;
var
  intDef: IwbIntegerDef;
begin
  Result := Supports(element.ValueDef, IwbIntegerDef, intDef)
    and Supports(intDef.Formater[element], IwbFlagsDef, flagsDef);
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
