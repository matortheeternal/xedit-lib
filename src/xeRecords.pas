unit xeRecords;

interface

uses
  wbInterface,
  xeMeta;

  {$region 'Native functions'}
  function EditorIDToFormID(_file: IwbFile; editorID: String): Cardinal;
  {$endregion}

  {$region 'API functions'}
  function GetFormID(_id: Cardinal; formID: PCardinal; local: WordBool): WordBool; cdecl;
  function SetFormID(_id: Cardinal; formID: Cardinal; local, fixReferences: WordBool): WordBool; cdecl;
  function GetRecords(_id: Cardinal; search: PWideChar; includeOverrides: WordBool; len: PInteger): WordBool; cdecl;
  function GetOverrides(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function FindPreviousRecord(_id: Cardinal; search: PWideChar; byEdid, byName: Wordbool; _res: PCardinal): WordBool; cdecl;
  function FindNextRecord(_id: Cardinal; search: PWideChar; byEdid, byName: WordBool; _res: PCardinal): WordBool; cdecl;
  function ExchangeReferences(_id, oldFormID, newFormID: Cardinal): WordBool; cdecl;
  function GetReferencedBy(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function IsMaster(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function IsInjected(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function IsOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function IsWinningOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function ConflictAll(_id: Cardinal; enum: PByte): WordBool; cdecl;
  function ConflictThis(_id: Cardinal; enum: PByte): WordBool; cdecl;
  {$endregion}

implementation

uses
  Classes, SysUtils,
  mteConflict,
  wbImplementation,
  xeTypes, xeMessages, xeSetup, xeElements, xeElementValues;

{$region 'Native functions'}
function EditorIDToFormID(_file: IwbFile; editorID: String): Cardinal;
var
  rec: IwbMainRecord;
begin
  rec := _file.RecordByEditorID[editorID];
  if not Assigned(rec) then
    raise Exception.Create('Failed to find record with Editor ID: ' + editorID + ' in file ' + _file.FileName);
  Result := _file.LoadOrderFormIDtoFileFormID(rec.LoadOrderFormID);
end;

procedure GetSignatures(search: String; signatures: TStringList);
var
  i: Integer;
  str: String;
begin
  signatures.StrictDelimiter := true;
  signatures.CommaText := search;
  for i := 0 to Pred(signatures.Count) do begin
    str := signatures[i];
    if Length(str) > 4 then
      signatures[i] := NativeSignatureFromName(str);
  end;
end;

function AllSignaturesTopLevel(signatures: TFastStringList): Boolean;
var
  i: Integer;
  sig: String;
begin
  Result := False;
  for i := 0 to Pred(signatures.Count) do begin
    sig := signatures[i];
    if (sig = 'CELL') or (wbGroupOrder.IndexOf(sig) = -1) then
      exit;
  end;
  Result := True;
end;

procedure FindRecords(_file: IwbFile; signatures: TFastStringList; includeOverrides: Boolean; lst: TList); overload;
var
  allRecords: Boolean;
  i, j: Integer;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
begin
  allRecords := signatures.Count = 0;
  if not allRecords and AllSignaturesTopLevel(signatures) then begin
    for i := 0 to Pred(signatures.Count) do begin
      group := _file.GroupBySignature[StrToSignature(signatures[i])];
      if not Assigned(group) then continue;
      for j := 0 to Pred(group.ElementCount) do
        if Supports(group.Elements[j], IwbMainRecord, rec)
        and (includeOverrides or rec.IsMaster) then
          lst.Add(Pointer(rec));
    end;
  end
  else begin
    for i := 0 to Pred(_file.RecordCount) do begin
      rec := _file.Records[i];
      if (includeOverrides or rec.IsMaster) and (allRecords
      or (signatures.IndexOf(string(rec.Signature)) > -1)) then
        lst.Add(Pointer(rec));
    end;
  end;
end;

procedure FindRecords(group: IwbGroupRecord; signatures: TFastStringList; includeOverrides: Boolean; lst: TList); overload;
var
  allRecords: Boolean;
  i: Integer;
  element: IwbElement;
  rec: IwbMainRecord;
  subgroup: IwbGroupRecord;
begin
  allRecords := signatures.Count = 0;
  for i := 0 to Pred(group.ElementCount) do begin
    element := group.Elements[i];
    if Supports(element, IwbMainRecord, rec) and (includeOverrides or rec.IsMaster)
    and (allRecords or (signatures.IndexOf(string(rec.Signature)) > -1)) then
      lst.Add(Pointer(rec))
    else if Supports(element, IwbGroupRecord, subgroup) then
      FindRecords(subgroup, signatures, includeOverrides, lst);
  end;
end;

procedure NativeGetRecords(_id: Cardinal; signatures: TFastStringList; includeOverrides: Boolean; lst: TList);
var
  i: Integer;
  e: IInterface;
  _file: IwbFile;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
begin
  if _id = 0 then begin
    for i := Low(xFiles) to High(xFiles) do
      FindRecords(xFiles[i], signatures, includeOverrides, lst);
  end
  else begin
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then
      FindRecords(_file, signatures, includeOverrides, lst)
    else if Supports(e, IwbGroupRecord, group) then
      FindRecords(group, signatures, includeOverrides, lst)
    else if Supports(e, IwbMainRecord, rec) then begin
      if Assigned(rec.ChildGroup) then
        FindRecords(rec.ChildGroup, signatures, includeOverrides, lst);
    end
    else
      raise Exception.Create('Interface must be a file, group, or main record.');
  end;
end;

function NativeFindNextRecord(container: IwbContainer; i: Integer; search: String; byEdid, byName, recurse: WordBool): IwbMainRecord;
var
  count: Integer;
  e: IwbElement;
  rec: IwbMainRecord;
  innerContainer: IwbContainer;
  elements: TDynElements;
begin
  // iterate through children
  GetSortedElements(container, elements);
  while i <= High(elements) do begin
    e := elements[i];
    if Supports(e, IwbMainRecord, Result) then begin
      if byEdid and (Pos(search, Result.EditorID) > 0) then exit;
      if byName and (Pos(search, Result.FullName) > 0) then exit;
    end
    // recurse through child containers
    else if Supports(e, IwbContainer, innerContainer) then begin
      Result := NativeFindNextRecord(innerContainer, 0, search, byEdid, byName, false);
      if Assigned(Result) then exit;
    end;
    Inc(i);
  end;
  Result := nil;
  // recurse to sibling container
  if recurse then begin
    e := container as IwbElement;
    container := e.Container;
    if Assigned(container) then
      Result := NativeFindNextRecord(container, container.IndexOf(e) + 1, search, byEdid, byName, true);
  end;
end;

function NativeFindPreviousRecord(container: IwbContainer; i: Integer; search: String; byEdid, byName, recurse: WordBool): IwbMainRecord;
var
  e: IwbElement;
  rec: IwbMainRecord;
  innerContainer: IwbContainer;
  elements: TDynElements;
begin
  // iterate through children
  GetSortedElements(container, elements);
  while i > -1 do begin
    e := elements[i];
    if Supports(e, IwbMainRecord, Result) then begin
      if byEdid and (Pos(search, Result.EditorID) > 0) then exit;
      if byName and (Pos(search, Result.FullName) > 0) then exit;
    end
    // recurse through child containers
    else if Supports(e, IwbContainer, innerContainer) then begin
      Result := NativeFindPreviousRecord(innerContainer, innerContainer.ElementCount - 1, search, byEdid, byName, false);
      if Assigned(Result) then exit;
    end;
    Dec(i);
  end;
  // recurse to sibling container
  if recurse then begin
    e := container as IwbElement;
    container := e.Container;
    if Assigned(container) then
      Result := NativeFindPreviousRecord(container, container.IndexOf(e) - 1, search, byEdid, byName, true);
  end;
end;
{$endregion}

{$region 'API functions'}
function GetFormID(_id: Cardinal; formID: PCardinal; local: WordBool): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    if local then
      formID^ := rec._File.LoadOrderFormIDtoFileFormID(rec.LoadOrderFormID)
    else
      formID^ := rec.LoadOrderFormID;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFormID(_id: Cardinal; formID: Cardinal; local, fixReferences: WordBool): WordBool; cdecl;
var
  rec: IwbMainRecord;
  oldFormID, newFormID: Cardinal;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    oldFormID := rec.FormID;
    if local then
      rec.LoadOrderFormID := rec._File.FileFormIDtoLoadOrderFormID(formID)
    else
      rec.LoadOrderFormID := formID;
    if fixReferences then begin
      rec._File.BuildRef;
      newFormID := rec.FormID;
      for i := Pred(rec.ReferencedByCount) downto 0 do
        rec.ReferencedBy[i].CompareExchangeFormID(oldFormID, newFormID);
    end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetRecords(_id: Cardinal; search: PWideChar; includeOverrides: WordBool; len: PInteger): WordBool; cdecl;
var
  lst: TList;
  signatures: TFastStringList;
begin
  Result := False;
  try
    lst := TList.Create;
    signatures := TFastStringList.Create;
    try
      GetSignatures(string(search), signatures);
      NativeGetRecords(_id, signatures, includeOverrides, lst);
      StoreList(lst, len);
      Result := True;
    finally
      lst.Free;
      signatures.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetOverrides(_id: Cardinal; count: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    rec := rec.MasterOrSelf;
    count^ := rec.OverrideCount;
    SetLength(resultArray, count^);
    for i := 0 to Pred(count^) do
      resultArray[i] := Store(IInterface(rec.Overrides[i]));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FindPreviousRecord(_id: Cardinal; search: PWideChar; byEdid, byName: Wordbool; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainer;
  rec: IwbMainRecord;
begin
  Result := False;
  try
    // treat root as last file
    // if element is a main record, iterate through its parent container
    // else if element is a group record or a file, iterate through it
    if _id = 0 then
      element := xFiles[High(xFiles)]
    else if not Supports(Resolve(_id), IwbContainer, element) then
      raise Exception.Create('Input interface is not an element.');
    if not Supports(element, IwbContainer, container) then
      raise Exception.Create('Input element is not a container.');
    if Supports(element, IwbMainRecord) then
      rec := NativeFindPreviousRecord(container, container.IndexOf(element) - 1, string(search), byEdid, byName, true)
    else if Supports(element, IwbGroupRecord) or Supports(element, IwbFile) then
      rec := NativeFindPreviousRecord(container, container.ElementCount - 1, string(search), byEdid, byName, true)
    else
      raise Exception.Create('Input element must be a file, group, or record.');
    if Assigned(rec) then begin
      _res^ := Store(rec);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FindNextRecord(_id: Cardinal; search: PWideChar; byEdid, byName: WordBool; _res: PCardinal): WordBool; cdecl;
var
  element: IwbElement;
  container: IwbContainer;
  rec: IwbMainRecord;
begin
  Result := False;
  try
    // treat root as first file
    // if element is a main record, iterate through its parent container
    // else if element is a group record or a file, iterate through it
    if _id = 0 then
      element := xFiles[Low(xFiles)]
    else if not Supports(Resolve(_id), IwbContainer, element) then
      raise Exception.Create('Input interface is not an element.');
    if not Supports(element, IwbContainer, container) then
      raise Exception.Create('Input element is not a container.');
    if Supports(element, IwbMainRecord) then
      rec := NativeFindNextRecord(container, container.IndexOf(element) + 1, string(search), byEdid, byName, True)
    else if Supports(element, IwbGroupRecord) or Supports(element, IwbFile) then
      rec := NativeFindNextRecord(container, 0, string(search), byEdid, byName, True)
    else
      raise Exception.Create('Input element must be a file, group, or record.');
    if Assigned(rec) then begin
      _res^ := Store(rec);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ExchangeReferences(_id, oldFormID, newFormID: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    rec.CompareExchangeFormID(oldFormID, newFormID);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetReferencedBy(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  rec, ref: IwbMainRecord;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    len^ := rec.ReferencedByCount;
    SetLength(resultArray, len^);
    for i := 0 to Pred(rec.ReferencedByCount) do
      if Supports(rec.ReferencedBy[i], IwbMainRecord, ref) then
        resultArray[i] := Store(ref);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsMaster(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    bool^ := rec.IsMaster;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsInjected(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    bool^ := rec.IsInjected;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    bool^ := not rec.IsMaster;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsWinningOverride(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    bool^ := rec.IsWinningOverride;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ConflictAll(_id: Cardinal; enum: PByte): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    enum^ := Ord(ConflictAllForMainRecord(rec));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ConflictThis(_id: Cardinal; enum: PByte): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    enum^ := Ord(ConflictThisForMainRecord(rec));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

end.
