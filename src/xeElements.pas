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
  TValueType = ( vtUnknown, vtBytes, vtNumber, vtString, vtText, vtReference, vtFlags,
    vtEnum, vtColor, vtArray, vtStruct );

  {$region 'Native functions'}
  function ResolveGroupOrRecord(const group: IwbGroupRecord; const key, nextPath: String): IInterface; overload;
  function ResolveFromGroup(const group: IwbGroupRecord; const path: String): IInterface;
  function ResolveElement(const e: IInterface; const path: String): IInterface;
  function NativeGetElement(_id: Cardinal; path: PWideChar): IInterface;
  function ElementNotFound(const element: IwbElement; path: PWideChar): Boolean;
  procedure NativeMoveArrayItem(const element: IwbElement; index: Integer);
  function NativeContainer(const element: IwbElement): IwbContainer;
  function AddGroupIfMissing(const _file: IwbFile; const sig: String): IwbGroupRecord;
  function CreateFromContainer(const container: IwbContainerElementRef; const path: String): IInterface;
  function CreateFromRecord(const rec: IwbMainRecord; const path: String): IInterface;
  function CreateRecord(const group: IwbGroupRecord; formID: Cardinal; const nextPath: String): IInterface; overload;
  function CreateGroupOrRecord(const group: IwbGroupRecord; key, nextPath: String): IInterface; overload;
  function CreateFromGroup(const group: IwbGroupRecord; const path: String): IInterface;
  function CreateFile(const fileName, nextPath: String): IInterface;
  function CreateElement(const e: IInterface; const path: String): IInterface;
  function NativeAddElement(_id: Cardinal; const path: string): IInterface;
  function CopyElementToFile(const aSource: IwbElement; const aFile: IwbFile; aAsNew, aDeepCopy: Boolean): IwbElement;
  function CopyElementToRecord(const aSource: IwbElement; const aMainRecord: IwbMainRecord; aAsNew, aDeepCopy: Boolean): IwbElement;
  function CopyElementToArray(const aSource: IwbElement; const aArray: IwbElement): IwbElement;
  function ResolveDef(const element: IwbElement; decideUnions: Boolean): IwbNamedDef;
  function IsChildGroup(const group: IwbGroupRecord): Boolean;
  function NativeIsSorted(const e: IwbElement): Boolean;
  function NativeIsFlags(const e: IwbElement): Boolean;
  function IsArray(const element: IwbElement): Boolean;
  function IsFormID(const element: IwbElement): Boolean;
  function GetFlagsDef(const element: IwbElement; var flagsDef: IwbFlagsDef): Boolean;
  function GetEnumDef(const element: IwbElement; var enumDef: IwbEnumDef): Boolean;
  function GetDefType(const element: IwbElement): TwbDefType;
  function GetSmashType(const element: IwbElement): TSmashType;
  {$endregion}

  {$region 'API functions'}
  function HasElement(_id: Cardinal; path: PWideChar; bool: PWordBool): WordBool; cdecl;
  function GetElement(_id: Cardinal; path: PWideChar; _res: PCardinal): WordBool; cdecl;
  function AddElement(_id: Cardinal; path: PWideChar; _res: PCardinal): WordBool; cdecl;
  function AddElementValue(_id: Cardinal; path, value: PWideChar; _res: PCardinal): WordBool; cdecl;
  function RemoveElement(_id: Cardinal; path: PWideChar): WordBool; cdecl;
  function RemoveElementOrParent(_id: Cardinal): WordBool; cdecl;
  function SetElement(_id, _id2: Cardinal): WordBool; cdecl;
  function GetElements(_id: Cardinal; path: PWideChar; sort, filter: WordBool; len: PInteger): WordBool; cdecl;
  function GetDefNames(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetAddList(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetLinksTo(_id: Cardinal; path: PWideChar; _res: PCardinal): WordBool; cdecl;
  function SetLinksTo(_id: Cardinal; path: PWideChar; _id2: Cardinal): WordBool; cdecl;
  function GetElementIndex(_id: Cardinal; index: PInteger): WordBool; cdecl;
  function GetContainer(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GetElementFile(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GetElementGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GetElementRecord(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function ElementCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function ElementEquals(_id, _id2: Cardinal; bool: PWordBool): WordBool; cdecl;
  function ElementMatches(_id: Cardinal; path, value: PWideChar; bool: PWordBool): WordBool; cdecl;
  function HasArrayItem(_id: Cardinal; path, subpath, value: PWideChar; bool: PWordBool): WordBool; cdecl;
  function GetArrayItem(_id: Cardinal; path, subpath, value: PWideChar; _res: PCardinal): WordBool; cdecl;
  function AddArrayItem(_id: Cardinal; path, subpath, value: PWideChar; _res: PCardinal): WordBool; cdecl;
  function RemoveArrayItem(_id: Cardinal; path, subpath, value: PWideChar): WordBool; cdecl;
  function MoveArrayItem(_id: Cardinal; index: Integer): WordBool; cdecl;
  function CopyElement(_id, _id2: Cardinal; aAsNew: WordBool; _res: PCardinal): WordBool; cdecl;
  function FindNextElement(_id: Cardinal; search: PWideChar; byPath, byValue: WordBool; _res: PCardinal): WordBool; cdecl;
  function FindPreviousElement(_id: Cardinal; search: PWideChar; byPath, byValue: Wordbool; _res: PCardinal): WordBool; cdecl;
  function GetSignatureAllowed(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool; cdecl;
  function GetAllowedSignatures(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetIsModified(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function GetIsEditable(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function SetIsEditable(_id: Cardinal; bool: WordBool): WordBool; cdecl;
  function GetIsRemoveable(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function GetCanAdd(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function SortKey(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function ElementType(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function DefType(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function SmashType(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function ValueType(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function IsSorted(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function IsFixed(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  {$endregion}

implementation

uses
  Variants, SysUtils, StrUtils,
  // xedit units
  wbImplementation,
  // xelib units
  xeConfiguration, xeMessages, xeFiles, xeMasters, xeRecords, xeElementValues,
  xeSetup;

const
  textElements: array[0..4] of string = (
    'Response Text', 'Description', 'Book Text', 'Magic Item Description', 'Log Entry'
  );

{$region 'Native functions'}
{$region 'Path parsing'}
function ParseIndex(const key: string; var index: Integer): Boolean;
var
  len: Integer;
begin
  len := Length(key);
  Result := (len > 2) and (key[1] = '[') and (key[len] = ']')
    and TryStrToInt(Copy(key, 2, len - 2), index);
end;

function CheckIndex(maxIndex: Integer; var index: Integer): Boolean;
begin
  if index = -1 then
    index := maxIndex;
  Result := (index > -1) and (index <= maxIndex);
end;

function IsHexStr(const key: String; const start: Integer = 1): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := start to Length(key) do
    if not CharInSet(key[i], ['A'..'F','0'..'9']) then exit;
  Result := True;
end;

function ParseFormID(const key: String; var formID: Cardinal): Boolean;
begin
  Result := (Length(key) = 8) and IsHexStr(key);
  if Result then
    formID := StrToInt('$' + key);
end;

function ParseFileFormID(const key: String; var formID: Cardinal): Boolean;
begin
  Result := (key[1] = '&') and (Length(key) = 9) and IsHexStr(key, 2);
  if Result then
    formID := StrToInt('$' + Copy(key, 2, 8));
end;

function ParseFullName(const value: String; var fullName: String): Boolean;
begin
  Result := (value[1] = '"') and (value[Length(value)] = '"');
  if Result then
    fullName := Copy(value, 2, Length(value) - 2);
end;

function GetSignatureFromName(const name: String; var signature: TwbSignature): Boolean;
var
  index: Integer;
begin
  BuildSignatureNameMap;
  index := slSignatureNameMap.IndexOfValue(name);
  Result := index > -1;
  if Result then
    signature := StrToSignature(slSignatureNameMap.Names[index])
end;

procedure SplitPath(const path: String; var key, nextPath: String);
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
{$endregion}

{$region 'Element resolution'}
function ResolveByIndex(const container: IwbContainerElementRef; index: Integer; const nextPath: String): IInterface;
begin
  Result := nil;

  // resolve element from container if container present
  // else resolve file at index
  if Assigned(container) then begin
    if CheckIndex(container.ElementCount - 1, index) then
      Result := container.Elements[index];
  end
  else begin
    if CheckIndex(High(xFiles), index) then
      Result := NativeFileByIndex(index);
  end;

  // resolve next element if nextPath is present
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveElement(Result, nextPath);
end;

function ResolveFromContainer(const container: IwbContainerElementRef; const path: String): IInterface;
begin
  Result := container.ElementByPath[path];
end;

function ResolveChildGroup(const rec: IwbMainRecord; const nextPath: String): IInterface;
begin
  Result := rec.ChildGroup;
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveFromGroup(Result as IwbGroupRecord, nextPath);
end;

function ResolveFromRecord(const rec: IwbMainRecord; const path: String): IInterface;
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

function FindRecordOrGroup(const group: IwbGroupRecord; const searchKey: String): IInterface;
var
  i: Integer;
  element: IwbElement;
  rec: IwbMainRecord;
  grp: IwbGroupRecord;
begin
  for i := 0 to Pred(group.ElementCount) do begin
    element := group.Elements[i];
    if Supports(element, IwbMainRecord, rec) then begin
      if SameText(rec.EditorID, searchKey) then begin
        Result := rec;
        exit;
      end;
    end
    else if Supports(element, IwbGroupRecord, grp) then
      if SameText(grp.ShortName, searchKey) then begin
        Result := grp;
        exit;
      end;
  end;
end;

function ResolveGroupOrRecord(const group: IwbGroupRecord; const key, nextPath: String): IInterface; overload;
var
  name, sig: String;
  formID, fixedFormID: Cardinal;
  grp: IwbGroupRecord;
  rec: IwbMainRecord;
begin
  Result := nil;
  if ParseFormID(key, formID) then begin
    fixedFormID := group._File.LoadOrderFormIDtoFileFormID(formID);
    Result := group._File.RecordByFormID[fixedFormID, True];
  end
  else if ParseFullName(key, name) then
    Result := group.MainRecordByName[name]
  else begin
    sig := String(TwbSignature(group.GroupLabel));
    if group._File.EditorIDSorted(sig) then
      Result := group._File.RecordByEditorID[key];
    if not Assigned(Result) then
      Result := FindRecordOrGroup(group, key);
  end;
  if nextPath <> '' then begin
    if Supports(Result, IwbGroupRecord, grp) then
      Result := ResolveFromGroup(grp, nextPath)
    else if Supports(Result, IwbMainRecord, rec) then
      Result := ResolveFromRecord(rec, nextPath);
  end;
end;

function ResolveFromGroup(const group: IwbGroupRecord; const path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
begin
  SplitPath(path, key, nextPath);
  // resolve element by index if key is an index
  // else resolve main record by FormID/EditorID/Name
  if ParseIndex(key, index) then
    Result := ResolveByIndex(group as IwbContainerElementRef, index, nextPath)
  else
    Result := ResolveGroupOrRecord(group, key, nextPath);
end;

function ResolveGroupOrRecord(const _file: IwbFile; const key, nextPath: String): IInterface; overload;
var
  formID: Cardinal;
  name: String;
  sig: TwbSignature;
  rec: IwbMainRecord;
  group: IwbGroupRecord;
begin
  if ParseFormID(key, formID) then begin
    formID := _file.LoadOrderFormIDtoFileFormID(formID);
    Result := _file.RecordByFormID[formID, True];
  end
  else if ParseFullName(key, name) then begin
    _file.FindName(name, rec);
    Result := rec;
  end
  else if Length(key) > 4 then begin
    if GetSignatureFromName(key, sig) then
      Result := _file.GroupBySignature[sig]
    else begin
      _file.FindEditorID(key, rec);
      Result := rec;
    end;
  end
  else
    Result := _file.GroupBySignature[StrToSignature(key)];
  if nextPath <> '' then begin
    if Supports(Result, IwbMainRecord, rec) then
      Result := ResolveFromRecord(rec, nextPath)
    else if Supports(Result, IwbGroupRecord, group) then
      Result := ResolveFromGroup(group, nextPath);
  end;
end;

function ResolveFromFile(const _file: IwbFile; const path: String): IInterface;
var
  key, nextPath: String;
  index: Integer;
begin
  SplitPath(path, key, nextPath);
  // resolve group by index if key is an index
  // else resolve record by formID if key is a formID
  // else resolve by group signature
  if ParseIndex(key, index) then
    Result := ResolveByIndex(_file as IwbContainerElementRef, index, nextPath)
  else if key = 'File Header' then begin
    Result := _file.Header;
    if nextPath <> '' then
      Result := ResolveFromRecord(_file.Header, nextPath);
  end
  else 
    Result := ResolveGroupOrRecord(_file, key, nextPath);
end;

function ResolveFile(const fileName, nextPath: String): IInterface;
begin
  Result := NativeFileByName(fileName);
  if Assigned(Result) and (nextPath <> '') then
    Result := ResolveFromFile(Result as IwbFile, nextPath);
end;

function ResolveFromRoot(const path: String): IInterface;
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

function ResolveElement(const e: IInterface; const path: String): IInterface;
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

function ResolveElementEx(const e: IInterface; const path: String): IInterface;
begin
  Result := ResolveElement(e, path);
  if not Assigned(Result) then
    raise Exception.Create('Failed to resolve element at path: ' + path);
end;

function NativeGetElement(_id: Cardinal; path: PWideChar): IInterface;
begin
  if string(path) = '' then
    Result := Resolve(_id)
  else if _id = 0 then
    Result := ResolveFromRoot(string(path))
  else
    Result := ResolveElement(Resolve(_id), string(path));
end;

function ElementNotFound(const element: IwbElement; path: PWideChar): Boolean;
begin
  Result := not Assigned(element);
  if Result then
    SoftException('Failed to resolve element at path: ' + string(path));
end;
{$endregion}

{$region 'Element creation'}
function AddGroupIfMissing(const _file: IwbFile; const sig: String): IwbGroupRecord;
var
  _sig: TwbSignature;
begin
  _sig := StrToSignature(sig);
  if _file.HasGroup(_sig) then
    Result := _file.GroupBySignature[_sig]
  else
    Supports(_file.Add(sig), IwbGroupRecord, Result);
end;

function CreateFromContainer(const container: IwbContainerElementRef; const path: String): IInterface;
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
    NativeMoveArrayItem(Result as IwbElement, index);
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

function CreateChildGroup(const rec: IwbMainRecord; const nextPath: String): IInterface;
begin
  Result := rec.EnsureChildGroup;
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromGroup(Result as IwbGroupRecord, nextPath);
end;

function CreateFromRecord(const rec: IwbMainRecord; const path: String): IInterface;
var
  key, nextPath: String;
begin
  SplitPath(path, key, nextPath);
  if SameText(key, 'Child Group') then
    Result := CreateChildGroup(rec, nextPath)
  else
    Result := CreateFromContainer(rec as IwbContainerElementRef, path);
end;

procedure OverrideRecordIfNecessary(const rec: IwbMainRecord; const targetFile: IwbFile; var output: IInterface);
var
  ovr: IwbMainRecord;
begin
  if Assigned(rec) and not rec._File.Equals(targetFile) then begin
    ovr := NativeGetPreviousOverride(rec, targetFile);
    output := CopyElementToFile(ovr, targetFile, false, true);
  end;
end;

function CreateRecord(const group: IwbGroupRecord; formID: Cardinal; const nextPath: String): IInterface; overload;
var
  sig: TwbSignature;
  rec: IwbMainRecord;
begin
  sig := TwbSignature(group.GroupLabel);
  Result := group._File.RecordByFormID[formID, true];
  if not Supports(Result, IwbMainRecord, rec) then exit;
  if rec.Signature <> sig then
    raise Exception.Create(Format('Found record %s does not match expected ' +
      'signature %s.', [(Result as IwbMainRecord).Name, string(sig)]));
  OverrideRecordIfNecessary(rec, group._File, Result);
  if nextPath <> '' then
    Result := CreateFromRecord(Result as IwbMainRecord, nextPath);
end;

function CreateGroupOrRecord(const group: IwbGroupRecord; key, nextPath: String): IInterface; overload;
var
  innerGroup: IwbGroupRecord;
  rec: IwbMainRecord;
begin
  if key = '.' then
    key := String(AnsiString(TwbSignature(group.GroupLabel)));
  if Length(key) > 4 then begin
    Result := FindRecordOrGroup(group, key);
    if not Assigned(Result) then
      Result := group.AddGroup(key);
  end
  else
    Result := group.Add(key);
  if nextPath <> '' then begin
    if Supports(Result, IwbGroupRecord, innerGroup) then
      Result := CreateFromGroup(innerGroup, nextPath)
    else if Supports(Result, IwbMainRecord, rec) then
      Result := CreateFromRecord(Result as IwbMainRecord, nextPath);
  end;
end;

function CreateFromGroup(const group: IwbGroupRecord; const path: String): IInterface;
var
  key, nextPath: String;
  formID: Cardinal;
begin
  SplitPath(path, key, nextPath);
  // resolve/override record by formID
  // else create new group/main record
  if ParseFormID(key, formID) then
    Result := CreateRecord(group, formID, nextPath)
  else
    Result := CreateGroupOrRecord(group, key, nextPath);
end;

function CreateGroup(const _file: IwbFile; key, nextPath: String): IInterface; overload;
begin
  if Length(key) > 4 then
    key := NativeSignatureFromName(key);
  Result := AddGroupIfMissing(_file, key);
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromGroup(Result as IwbGroupRecord, nextPath);
end;

function CreateRecord(const _file: IwbFile; formID: Cardinal; const nextPath: String): IInterface; overload;
var
  rec: IwbMainRecord;
begin
  formID := _file.LoadOrderFormIDToFileFormID(formID);
  Result := _file.RecordByFormID[formID, true];
  if not Supports(Result, IwbMainRecord, rec) then exit;
  OverrideRecordIfNecessary(rec, _file, Result);
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromRecord(Result as IwbMainRecord, nextPath);
end;

function CreateFromFile(const _file: IwbFile; const path: String): IInterface;
var
  key, nextPath: String;
  formID: Cardinal;
begin
  SplitPath(path, key, nextPath);
  // resolve record by formID if key is a formID
  // else resolve file header
  // else resolve by group signature
  if ParseFormID(key, formID) then
    Result := CreateRecord(_file, formID, nextPath)
  else if key = 'File Header' then
    Result := CreateFromRecord(_file.Header, nextPath)
  else
    Result := CreateGroup(_file, key, nextPath);
end;

function CreateFile(const fileName, nextPath: String): IInterface;
begin
  Result := NativeFileByName(fileName);
  if not Assigned(Result) then
    Result := NativeAddFile(fileName);
  if Assigned(Result) and (nextPath <> '') then
    Result := CreateFromFile(Result as IwbFile, nextPath);
end;

function CreateFromRoot(const path: String): IInterface;
var
  key, nextPath: String;
begin
  SplitPath(path, key, nextPath);
  Result := CreateFile(key, nextPath);
end;

function CreateElement(const e: IInterface; const path: String): IInterface;
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

function NativeAddElement(_id: Cardinal; const path: String): IInterface;
begin
  if _id = 0 then
    Result := CreateFromRoot(path)
  else if path = '' then
    Result := Resolve(_id)   
  else
    Result := CreateElement(Resolve(_id), path);
end;
{$endregion}

{$region 'Multi-element resolution'}
procedure GetFiles();
var
  i: Integer;
begin
  SetLength(resultArray, Length(xFiles));
  for i := 0 to High(xFiles) do
    resultArray[i] := Store(xFiles[i]);
end;

procedure GetContainerElements(const container: IwbContainerElementRef);
var
  i, n: Integer;
  e: IwbElement;
  g: IwbGroupRecord;
begin
  SetLength(resultArray, container.ElementCount);
  if HideChildGroups then begin
    n := 0;
    for i := 0 to Pred(container.ElementCount) do begin
      e := container.Elements[i];
      if Supports(e, IwbGroupRecord, g) and IsChildGroup(g) then continue;
      resultArray[n] := Store(container.Elements[i]);
      Inc(n);
    end;
    SetLength(resultArray, n);
  end
  else
    for i := 0 to Pred(container.ElementCount) do
      resultArray[i] := Store(container.Elements[i]);
end;

procedure GetChildrenElements(const element: IInterface);
var
  container: IwbContainerElementRef;
begin
  if not Supports(element, IwbContainerElementRef, container) then
    raise Exception.Create('Interface must be a container.');
  GetContainerElements(container);
end;
{$endregion}

{$REGION 'Def names'}
function GetDefName(const def: IwbNamedDef): String;
var
  sigDef: IwbSignatureDef;
begin
  if Supports(def, IwbSignatureDef, sigDef) then
    Result := sigDef.Signatures[0] + ' - ' + def.Name
  else
    Result := def.Name;
end;

function DecideUnion(const e: IwbElement; const unionDef: IwbUnionDef): IwbValueDef;
var
  d: IwbDataContainer;
begin
  d := e as IwbDataContainer;
  Result := unionDef.Decide(d.DataBasePtr, d.DataEndPtr, e);
end;

procedure NativeGetDefNames(const element: IwbElement; var sl: TStringList);
var
  i: Integer;
  def: IwbNamedDef;
  unionDef: IwbUnionDef; 
  recDef: IwbRecordDef;
  container: IwbContainer;
  structDef: IwbStructDef;
  elementMap: TDynCardinalArray;
  hasElementMap: Boolean;
  sraDef: IwbSubRecordArrayDef;
  aDef: IwbArrayDef;
begin
  def := ResolveDef(element, False);
  // try IwbUnionDef
  if Supports(def, IwbUnionDef, unionDef) then begin
    def := DecideUnion(element, unionDef);
    sl.Add(def.Name);
  end
  // try IwbRecordDef
  else if Supports(def, IwbRecordDef, recDef) then begin
    if Supports(element, IwbContainer, container) then
      for i := 0 to Pred(container.AdditionalElementCount) do
        sl.Add(container.ElementBySortOrder[i].Name);
    for i := 0 to Pred(recDef.MemberCount) do
      sl.Add(GetDefName(recDef.Members[i]));
  end
  // try IwbStructDef
  else if Supports(def, IwbStructDef, structDef) then begin
    elementMap := structDef.GetElementMap;
    hasElementMap := Length(elementMap) > 0;
    for i := 0 to Pred(structDef.MemberCount) do
      if hasElementMap then
        sl.Add(GetDefName(structDef.Members[elementMap[i]]))
      else
        sl.Add(GetDefName(structDef.Members[i]));
  end
  // try IwbSubRecordArrayDef
  else if Supports(def, IwbSubRecordArrayDef, sraDef) then
    sl.Add(GetDefName(sraDef.Element))
  // try IwbArrayDef
  else if Supports(def, IwbArrayDef, aDef) then
    sl.Add(GetDefName(aDef.Element))
  else
    sl.Add(GetDefName(def));
end;
{$ENDREGION}

function NativeContainer(const element: IwbElement): IwbContainer;
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

{$region 'Element matching'}
function ElementValueMatches(element: IwbElement; value: string): WordBool;
var
  formID: Int64;
  rec: IwbMainRecord;
  fullName, v: String;
  e1, e2: Extended;
begin
  Result := False;
  if IsFormID(element) then begin
    if ParseFormIDValue(value, formID) then
      Result := element.NativeValue = formID
    else if Supports(element.LinksTo, IwbMainRecord, rec) then begin
      if ParseFullName(value, fullName) then
        Result := rec.FullName = fullName
      else
        Result := rec.EditorID = value;
    end
  end
  else begin
    v := element.EditValue;
    Result := (TryStrToFloat(value, e1) and TryStrToFloat(v, e2) and (e1 = e2)) or (v = value);
  end;
end;

function NativeElementMatches(element: IwbElement; path, value: string): WordBool;
var
  container: IwbContainerElementRef;
begin
  if path = '' then
    Result := ElementValueMatches(element, value)
  else begin
    if not Supports(element, IwbContainerElementRef, container) then
      raise Exception.Create('Interface must be a container to resolve subpaths.');
    Result := ElementValueMatches(container.ElementByPath[path], value);
  end;
end;
{$endregion}

{$region 'Array item handling'}
function NativeGetArrayItem(const container: IwbContainerElementRef; const path, value: string): IwbElement;
var
  i: Integer;
begin
  for i := 0 to Pred(container.ElementCount) do begin
    Result := container.Elements[i];
    if NativeElementMatches(Result, path, value) then
      exit;
  end;
  Result := nil;
end;

function NativeGetArrayItemEx(const container: IwbContainerElementRef; const path, value: string): IwbElement;
begin
  Result := NativeGetArrayItem(container, path, value);
  if not Assigned(Result) then
    raise Exception.Create('Could not find matching array element.');
end;

function NativeAddArrayItem(const container: IwbContainerElementRef; const path, value: String; first: Boolean): IwbElement;
var
  e: IInterface;
begin
  if first then
    Result := container.Elements[0]
  else
    Result := container.Assign(High(Integer), nil, False);
  if value = '' then exit;
  if path = '' then
    SetElementValue(Result, value)
  else begin
    e := ResolveElementEx(Result as IInterface, path);
    SetElementValue(e as IwbElement, value);
  end;
end;

procedure NativeRemoveArrayItem(const container: IwbContainerElementRef; const path, value: string);
var
  i: Integer;
begin
  for i := 0 to Pred(container.ElementCount) do
    if NativeElementMatches(container.Elements[i], path, value) then begin
      container.RemoveElement(i);
      break;
    end;
end;

procedure NativeMoveArrayItem(const element: IwbElement; index: Integer);
var
  container: IwbContainerElementRef;
begin
  container := element.Container as IwbContainerElementRef;
  if not IsArray(container) then
    raise Exception.Create('Cannot move elements in non-array containers.');
  if NativeIsSorted(container) then
    raise Exception.Create('Cannot move elements in sorted arrays.');
  if index > container.IndexOf(element) then
    Dec(index);
  element.Remove;
  container.InsertElement(index, element);
end;
{$endregion}

{$region 'Element copying'}
function CopyElementToFile(const aSource: IwbElement; const aFile: IwbFile; aAsNew, aDeepCopy: Boolean): IwbElement;
var
  MainRecord: IwbMainRecord;
  Container: IwbContainer;
  Target: IwbElement;
begin
  Result := nil;
  Container := aSource.Container;
  if Assigned(Container) then begin
    if Supports(Container, IwbMainRecord, MainRecord) then
      Container := MainRecord.HighestOverrideOrSelf[aFile.LoadOrder];
    Target := CopyElementToFile(Container, aFile, False, False)
  end
  else
    Result := aFile;

  if Assigned(Target) then
    Result := Target.AddIfMissing(aSource, aAsNew, aDeepCopy, '', '', '');
end;

function CopyElementToRecord(const aSource: IwbElement; const aMainRecord: IwbMainRecord; aAsNew, aDeepCopy: Boolean): IwbElement;
var
  Container: IwbContainer;
  Target: IwbElement;
begin
  Result := nil;

  if Assigned(aSource) and (aSource.ElementType = etMainRecord) then begin
    Result := aMainRecord;
    Exit;
  end;

  Container := aSource.Container;
  Assert(Assigned(Container));
  Target := CopyElementToRecord(Container, aMainRecord, False, False);

  if Assigned(Target) then
    Result := Target.AddIfMissing(aSource, aAsNew, aDeepCopy, '', '', '');
end;

function CopyElementToArray(const aSource: IwbElement; const aArray: IwbElement): IwbElement;
var
  container: IwbContainer;
begin
  Result := nil;
  if not Supports(aArray, IwbContainer, container) then
    exit;
  Result := container.Assign(High(Integer), aSource, False);
end;
{$endregion}

{$REGION 'Element searching'}
function NativeFindNextElement(const container: IwbContainer; const element: IwbElement; const search: String;
  byPath, byValue, recurse: WordBool): IwbElement;
var
  i: Integer;
  c: IwbContainer;
  e: IwbElement;
begin
  // iterate through children
  i := container.IndexOf(element) + 1;
  while i <= Pred(container.ElementCount) do begin
    Result := container.Elements[i];
    if byPath and ContainsText(GetPath(Result, False, True), search) then exit;
    if byValue and ContainsText(Result.EditValue, search) then exit;
    // recurse through child containers
    if Supports(Result, IwbContainer, c) then begin
      Result := NativeFindNextElement(c, nil, search, byPath, byValue, false);
      if Assigned(Result) then exit;
    end;
    Inc(i);
  end;
  Result := nil;
  // recurse through parent containers, but don't recurse above the main record
  if recurse and not Supports(container, IwbMainRecord) then begin
    e := container as IwbElement;
    c := e.Container;
    if Assigned(c) then
      Result := NativeFindNextElement(c, e, search, byPath, byValue, true);
  end;
end;

function NativeFindPreviousElement(const container: IwbContainer; const element: IwbElement; const search: String;
  byPath, byValue, recurse: WordBool): IwbElement;
var
  i: Integer;
  c: IwbContainer;
  e: IwbElement;
begin
  // iterate through children
  i := container.IndexOf(element) - 1;
  if i = -2 then i := Pred(container.ElementCount);
  while i > -1 do begin
    Result := container.Elements[i];
    if byPath and ContainsText(GetPath(Result, False, True), search) then exit;
    if byValue and ContainsText(Result.EditValue, search) then exit;
    // recurse through child containers
    if Supports(Result, IwbContainer, c) then begin
      Result := NativeFindPreviousElement(c, nil, search, byPath, byValue, false);
      if Assigned(Result) then exit;
    end;
    Dec(i);
  end;
  Result := nil;
  // recurse through parent containers, but don't recurse above the main record
  if recurse and not Supports(container, IwbMainRecord) then begin
    e := container as IwbElement;
    c := e.Container;
    if Assigned(c) then
      Result := NativeFindPreviousElement(c, e, search, byPath, byValue, true);
  end;
end;
{$ENDREGION}

{$REGION 'Signature checking'}
function NativeGetSignatureAllowed(const formDef: IwbFormIDChecked; sig: TwbSignature): WordBool;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Pred(formDef.SignatureCount) do
    if formDef.Signatures[i] = sig then begin
      Result := True;
      exit;
    end;
end;

function GetAllSignatures: String;
var
  i, len: Integer;
  recordDef: TwbRecordDefEntry;
begin
  Result := '';
  for i := Low(wbRecordDefs) to High(wbRecordDefs) do begin
    recordDef := wbRecordDefs[i];
    Result := Result + String(recordDef.rdeSignature) + #13#10;
  end;
  len := Length(Result);
  if len > 0 then
    Delete(Result, len - 1, 2);
end;
{$ENDREGION}

function NativeGetCanAdd(const element: IwbElement): Boolean;
var
  eContainer: IwbContainer;
begin
  Result := False;
  eContainer := element.Container;
  if Assigned(eContainer) then begin
    if not eContainer.IsElementEditable(element) then
      exit;
  end
  else if not element.IsEditable then
    exit;
  Result := not (esNotSuitableToAddTo in element.ElementStates);
end;

{$region 'Def/type helpers'}
function ResolveDef(const element: IwbElement; decideUnions: Boolean): IwbNamedDef;
var
  subDef: IwbSubrecordDef;
  unionDef: IwbUnionDef;
  et: TwbElementType;
begin
  Result := element.Def;
  // traverse into subrecord defs
  if Supports(Result, IwbSubRecordDef, subDef) then
    Result := subDef.Value;
  // handle union defs
  if Supports(Result, IwbUnionDef, unionDef) then begin
    et := element.Container.ElementType;
    if decideUnions or (et = etMainRecord) or (et = etSubRecordStruct) then
      Result := DecideUnion(element, unionDef);
  end;
end;

function IsChildGroup(const group: IwbGroupRecord): Boolean;
begin
  Result := group.GroupType in [1,6,7];
end;

{ Returns true if @e is a sorted container }
function NativeIsSorted(const e: IwbElement): Boolean;
var
  Container: IwbSortableContainer;
begin
  Result := False;
  if Supports(e, IwbSortableContainer, Container) then
    Result := Container.Sorted;
end;

{ Returns true if @e is a fixed length array }
function NativeIsFixed(const e: IwbElement): Boolean;
var
  arrayDef: IwbArrayDef;
begin
  Result := False;
  if Supports(ResolveDef(e, true), IwbArrayDef, arrayDef) then
    Result := arrayDef.ElementCount > 0;
end;

{ Returns true if @e is a flags element }
function NativeIsFlags(const e: IwbElement): Boolean;
var
  def: IwbNamedDef;
  subDef: IwbSubrecordDef;
  intDef: IwbIntegerDef;
begin
  def := e.Def;
  if Supports(def, IwbSubrecordDef, subDef) then
    def := subDef.Value;
  Result := Supports(def, IwbIntegerDef, intDef)
    and Supports(intDef.Formater[e], IwbFlagsDef);
end;

{ Returns true if @e is a container with struct children }
function HasStructChildren(const e: IwbElement): boolean;
var
  Container: IwbContainerElementRef;
begin
  Result := False;
  if Supports(e, IwbContainerElementRef, Container)
  and (Container.ElementCount > 0) then
    Result := GetSmashType(Container.Elements[0]) = stStruct;
end;

function IsArray(const element: IwbElement): Boolean;
begin
  Result := GetDefType(element) in [dtSubRecordArray, dtArray];
end;

function IsFormID(const element: IwbElement): Boolean;
var
  intDef: IwbIntegerDef;
begin
  Result := Supports(element.ValueDef, IwbIntegerDef, intDef)
    and Supports(intDef.Formater[element], IwbFormID);
end;

function IsColorDef(const def: IwbNamedDef): Boolean;
var
  structDef: IwbStructDef;
begin
  Result := Supports(def, IwbStructDef, structDef) and
    (structDef.Members[0].Name = 'Red') and
    (structDef.Members[1].Name = 'Green') and
    (structDef.Members[2].Name = 'Blue');
end;

function GetInnerDef(const element: IwbElement): IwbNamedDef;
var
  subDef: IwbSubRecordDef;
  unionDef: IwbUnionDef;
begin
  Result := element.Def;
  if Supports(Result, IwbSubRecordDef, subDef) then
    Result := subDef.Value;
  if Supports(Result, IwbUnionDef, unionDef) then
    Result := DecideUnion(element, unionDef);
end;

function GetFlagsDef(const element: IwbElement; var flagsDef: IwbFlagsDef): Boolean;
var
  intDef: IwbIntegerDef;
begin
  Result := Supports(GetInnerDef(element), IwbIntegerDef, intDef)
    and Supports(intDef.Formater[element], IwbFlagsDef, flagsDef);
end;

function GetEnumDef(const element: IwbElement; var enumDef: IwbEnumDef): Boolean;
var
  intDef: IwbIntegerDef;
begin
  Result := Supports(GetInnerDef(element), IwbIntegerDef, intDef)
    and Supports(intDef.Formater[element], IwbEnumDef, enumDef);
end;

function GetDefType(const element: IwbElement): TwbDefType;
var
  subDef: IwbSubRecordDef;
begin
  if Supports(element.Def, IwbSubRecordDef, subDef) then
    Result := subDef.Value.DefType
  else
    Result := element.Def.DefType;
end;

function GetSmashType(const element: IwbElement): TSmashType;
var
  subDef: IwbSubRecordDef;
  dt: TwbDefType;
  bIsSorted, bHasStructChildren: boolean;
begin
  dt := element.Def.DefType;
  if Supports(element.Def, IwbSubRecordDef, subDef) then
    dt := subDef.Value.DefType;

  case dt of
    dtRecord:
      Result := stRecord;
    dtString, dtLString, dtLenString:
      Result := stString;
    dtByteArray:
      Result := stByteArray;
    dtInteger, dtIntegerFormater, dtIntegerFormaterUnion:
      Result := stInteger;
    dtFlag:
      Result := stFlag;
    dtFloat:
      Result := stFloat;
    dtSubRecordArray, dtArray: begin
      bIsSorted := NativeIsSorted(element);
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
    dtSubRecordStruct, dtStruct, dtStructChapter:
      Result := stStruct;
    dtSubRecordUnion, dtUnion:
      Result := stUnion;
    else
      Result := stUnknown;
  end;
end;

function GetValueType(const element: IwbElement): TValueType;
var
  def: IwbNamedDef;
  intDef: IwbIntegerDef;
begin
  def := GetInnerDef(element);

  case def.DefType of
    dtSubRecordArray, dtArray:
      Result := vtArray;
    dtSubRecordStruct, dtStruct:
      if IsColorDef(def) then
        Result := vtColor
      else
        Result := vtStruct;
    dtString, dtLenString, dtLString:
      if MatchText(element.Def.Name, textElements) then
        Result := vtText
      else
        Result := vtString;
    dtByteArray:
      Result := vtBytes;
    dtInteger, dtIntegerFormater, dtIntegerFormaterUnion, dtFloat: begin
      Result := vtNumber;
      if Supports(def, IwbIntegerDef, intDef) then begin
        if Supports(intDef.Formater[element], IwbFormID) then
          Result := vtReference
        else if Supports(intDef.Formater[element], IwbFlagsDef) then
          Result := vtFlags
        else if Supports(intDef.Formater[element], IwbEnumDef) then
          Result := vtEnum;
      end;
    end;
    else
      Result := vtUnknown;
  end;
end;
{$endregion}
{$endregion}

{$region 'API functions'}
function HasElement(_id: Cardinal; path: PWideChar; bool: PWordBool): WordBool; cdecl;
var
  e: IInterface;
begin
  Result := False;
  try
    e := NativeGetElement(_id, path);
    bool^ := Assigned(e);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// Has the functionality of FileByName, FileByIndex, GetFileHeader, GroupBySignature,
// GroupByName, RecordByFormID, RecordByEditorID, RecordByName, RecordByIndex,
// GetChildGroup, ElementByName, ElementByPath, ElementByIndex, and ElementBySignature.
function GetElement(_id: Cardinal; path: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    _res^ := Store(element as IInterface);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddElement(_id: Cardinal; path: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IInterface;
begin
  Result := False;
  try
    element := NativeAddElement(_id, string(path));
    if not Assigned(element) and
      SoftException('Failed to add element at path: ' + string(path)) then exit;
    _res^ := Store(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddElementValue(_id: Cardinal; path, value: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IInterface;
begin
  Result := False;
  try
    element := NativeAddElement(_id, string(path));
    if not Assigned(element) then
      raise Exception.Create('Failed to add element at path: ' + string(path));
    SetElementValue(element as IwbElement, string(value));
    _res^ := Store(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveElement(_id: Cardinal; path: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
begin
  Result := False;
  try
    e := NativeGetElement(_id, path);
    if Supports(e, IwbFile) then
      raise Exception.Create('Cannot remove files.');
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
    Result := Release(_id);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetElement(_id, _id2: Cardinal): WordBool; cdecl;
var
  e1, e2: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, e1) then
      raise Exception.Create('First interface is not an element.');
    if not Supports(Resolve(_id2), IwbElement, e2) then
      raise Exception.Create('Second interface is not an element.');
    if Supports(e1, IwbFile) or Supports(e1, IwbGroupRecord) or Supports(e1, IwbMainRecord)
    or Supports(e2, IwbFile) or Supports(e2, IwbGroupRecord) or Supports(e2, IwbMainRecord) then
      raise Exception.Create('Interfaces cannot be a file, group, or record');
    if not e1.CanAssign(Low(Integer), e2, True) then
      raise Exception.Create('Second element cannot be assigned to the first.');
    e1.Assign(Low(Integer), e2, False);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// returns an array of handles for the elements in a container
function GetElements(_id: Cardinal; path: PWideChar; sort, filter: WordBool; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if (_id = 0) and (path = '') then
      GetFiles()
    else begin
      element := NativeGetElement(_id, path) as IwbElement;
      if ElementNotFound(element, path) then exit;
      GetChildrenElements(element);
    end;
    if filter then FilterResultArray;
    if sort then SortResultArray;
    len^ := Length(resultArray);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetDefNames(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  sl: TStringList;
begin
  Result := False;
  try
    e := Resolve(_id);
    if not Supports(e, IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if Supports(e, IwbFile) or Supports(e, IwbGroupRecord) then
      raise Exception.Create('Interface cannot be a file or group.');
    sl := TStringList.Create;
    try
      NativeGetDefNames(element, sl);
      resultStr := sl.Text;
      Delete(resultStr, Length(resultStr) - 1, 2);
      len^ := Length(resultStr);
      Result := True;
    finally
      sl.Free;
    end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetAddList(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  container: IwbContainer;
  strings: TDynStrings;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbContainer, container) then
      raise Exception.Create('Interface is not an container.');
    strings := container.GetAddList;
    resultStr := strings[0];
    for i := Low(strings) + 1 to High(strings) do
      resultStr := resultStr + #13#10 + strings[i];
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetLinksTo(_id: Cardinal; path: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element, linkedElement: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not IsFormID(element) then
      raise Exception.Create('Element cannot hold references.');
    linkedElement := element.LinksTo;
    if not Assigned(linkedElement) then
      _res^ := 0
    else
      _res^ := Store(linkedElement);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetLinksTo(_id: Cardinal; path: PWideChar; _id2: Cardinal): WordBool; cdecl;
var
  element: IwbElement;
  rec: IwbMainRecord;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not Supports(Resolve(_id2), IwbMainRecord, rec) then
      raise Exception.Create('Second interface is not a record.');
    if not IsFormID(element) then
      raise Exception.Create('Element cannot hold references.');
    element.NativeValue := element._File.LoadOrderFormIDtoFileFormID(rec.LoadOrderFormID);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetElementIndex(_id: Cardinal; index: PInteger): WordBool; cdecl;
var
  element: IwbElement;
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if Supports(element, IwbFile, _file) then
      index^ := IndexOfFile(_file)
    else
      index^ := NativeContainer(element).IndexOf(element);
    Result := True;
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

function GetElementGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    while not Supports(element, IwbGroupRecord) do
      Supports(NativeContainer(element), IwbElement, element);
    _res^ := Store(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetElementRecord(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    rec := element.ContainingMainRecord;
    if not Assigned(rec) then
      raise Exception.Create('Element is not contained in a record');
    _res^ := Store(rec);
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
      count^ := Length(xFiles)
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

function ElementMatches(_id: Cardinal; path, value: PWideChar; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    bool^ := NativeElementMatches(element, '', value);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{$region 'Array item handling'}
function HasArrayItem(_id: Cardinal; path, subpath, value: PWideChar; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not Supports(element, IwbContainerElementRef, container)
    or not IsArray(container) then
      raise Exception.Create('Interface must be an array.');
    bool^ := Assigned(NativeGetArrayItem(container, subpath, value));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetArrayItem(_id: Cardinal; path, subpath, value: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not Supports(element, IwbContainerElementRef, container)
    or not IsArray(container) then
      raise Exception.Create('Interface must be an array.');
    _res^ := Store(NativeGetArrayItemEx(container, subpath, value));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddArrayItem(_id: Cardinal; path, subpath, value: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  element: IInterface;
  container: IwbContainerElementRef;
  createdArray: Boolean;
begin
  Result := False;
  try
    createdArray := False;
    element := NativeGetElement(_id, path);
    if not Assigned(element) then begin
      element := CreateElement(Resolve(_id), path);
      createdArray := true;
    end;
    if not Supports(element, IwbContainerElementRef, container)
    or not IsArray(container) then
      raise Exception.Create('Interface must be an array.');
    _res^ := Store(NativeAddArrayItem(container, subpath, value, createdArray));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveArrayItem(_id: Cardinal; path, subpath, value: PWideChar): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainerElementRef;
begin
  Result := False;
  try
    element := NativeGetElement(_id, path) as IwbElement;
    if ElementNotFound(element, path) then exit;
    if not Supports(element, IwbContainerElementRef, container)
    or not IsArray(container) then
      raise Exception.Create('Interface must be an array.');
    NativeRemoveArrayItem(container, subpath, value);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function MoveArrayItem(_id: Cardinal; index: Integer): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    NativeMoveArrayItem(element, index);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

function CopyElement(_id, _id2: Cardinal; aAsNew: WordBool; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
  rec: IwbMainRecord;
  e: IInterface;
  element, container, copy: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    e := Resolve(_id2);
    if Supports(e, IwbFile, _file) then
      copy := CopyElementToFile(element, _file, aAsNew, True)
    else if Supports(e, IwbMainRecord, rec) then
      copy := CopyElementToRecord(element, rec, aAsNew, True)
    else if Supports(e, IwbElement, container) and IsArray(container) then
      copy := CopyElementToArray(element, container)
    else
      raise Exception.Create('Second interface must be a file or a main record.');
    if not Assigned(copy) then
      raise Exception.Create('Failed to copy element.');
    _res^ := Store(copy);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FindNextElement(_id: Cardinal; search: PWideChar; byPath, byValue: WordBool; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Input interface is not an element.');
    if Supports(element, IwbFile) or Supports(element, IwbGroupRecord) then
      raise Exception.Create('Input interface cannot be a file or group.');
    if not Supports(element, IwbContainer, container) then
      container := element.Container;
    element := NativeFindNextElement(container, element, string(search), byPath, byValue, true);
    if Assigned(element) then begin
      _res^ := Store(element);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FindPreviousElement(_id: Cardinal; search: PWideChar; byPath, byValue: Wordbool; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Input interface is not an element.');
    if Supports(element, IwbFile) or Supports(element, IwbGroupRecord) then
      raise Exception.Create('Input interface cannot be a file or group.');
    if not Supports(element, IwbContainer, container) then
      container := element.Container;
    element := NativeFindPreviousElement(container, element, string(search), byPath, byValue, true);
    if Assigned(element) then begin
      _res^ := Store(element);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetSignatureAllowed(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool;
var
  element: IwbElement;
  integerDef: IwbIntegerDef;
  formDef: IwbFormIDChecked;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if not Supports(ResolveDef(element, True), IwbIntegerDef, integerDef)
    or not Supports(integerDef.Formater[element], IwbFormID) then
      raise Exception.Create('Interface must be able to hold a FormID value.');
    if Supports(integerDef.Formater[element], IwbFormIDChecked, formDef) then
      bool^ := NativeGetSignatureAllowed(formDef, StrToSignature(string(sig)))
    else
      bool^ := true;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetAllowedSignatures(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  element: IwbElement;
  integerDef: IwbIntegerDef;
  formDef: IwbFormIDChecked;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if not Supports(ResolveDef(element, True), IwbIntegerDef, integerDef)
    or not Supports(integerDef.Formater[element], IwbFormID) then
      raise Exception.Create('Interface must be able to hold a FormID value.');
    if Supports(integerDef.Formater[element], IwbFormIDChecked, formDef) then begin
      resultStr := String(formDef.Signatures[0]);
      for i := 1 to Pred(formDef.SignatureCount) do
        resultStr := resultStr + #13#10 + String(formDef.Signatures[i]);
    end
    else
      resultStr := GetAllSignatures;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetIsModified(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := element.Modified;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetIsEditable(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := element.IsEditable;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetIsEditable(_id: Cardinal; bool: WordBool): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface is not a file.');
    _file.SetIsEditable(bool);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetIsRemoveable(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := element.IsRemoveable;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetCanAdd(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := NativeGetCanAdd(element);
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

function ValueType(_id: Cardinal; enum: PByte): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    if Supports(element, IwbFile) or Supports(element, IwbGroupRecord) or Supports(element, IwbMainRecord) then
      raise Exception.Create('Interface cannot be a file, group, or main record.');
    enum^ := Ord(GetValueType(element));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsFlags(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := NativeIsFlags(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsSorted(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := NativeIsSorted(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;


function IsFixed(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Interface is not an element.');
    bool^ := NativeIsFixed(element);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

end.
