unit xeRecords;

interface

uses
  wbInterface,
  xeMeta;

  {$region 'Native functions'}
  function EditorIDToFormID(_file: IwbFile; editorID: String): Cardinal;
  procedure StoreRecords(_file: IwbFile; len: PInteger); overload;
  procedure StoreRecords(group: IwbGroupRecord; len: PInteger); overload;
  procedure FindRecordsBySignature(group: IwbGroupRecord; sig: TwbSignature; len: PInteger); overload;
  procedure FindRecordsBySignature(_file: IwbFile; sig: TwbSignature; len: PInteger); overload;
  {$endregion}

  {$region 'API functions'}
  function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
  function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;
  function GetRecords(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function RecordsBySignature(_id: Cardinal; sig: PWideChar; len: PInteger): WordBool; cdecl;
  function GetOverrides(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function ExchangeReferences(_id, oldFormID, newFormID: Cardinal): WordBool; cdecl;
  function GetReferences(_id: Cardinal; len: PInteger): WordBool; cdecl;
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
  xeMessages, xeElements;

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

procedure StoreRecords(_file: IwbFile; len: PInteger);
var
  i: Integer;
begin
  len^ := _file.RecordCount;
  SetLength(resultArray, len^);
  for i := 0 to Pred(_file.RecordCount) do
    resultArray[i] := Store(_file.Records[i]);
end;

procedure StoreRecords(group: IwbGroupRecord; len: PInteger);
var
  i: Integer;
  rec: IwbMainRecord;
begin
  len^ := 0;
  SetLength(resultArray, group.ElementCount);
  for i := 0 to Pred(group.ElementCount) do
    if Supports(group.Elements[i], IwbMainRecord, rec) then begin
      resultArray[len^] := Store(rec);
      Inc(len^);
    end;
end;

procedure StoreRecord(rec: IwbMainRecord; len: PInteger);
var
  capacity: Integer;
begin
  // grow capacity by 1KB when reached
  capacity := High(resultArray);
  if len^ > capacity then
    SetLength(resultArray, capacity + 256);
  resultArray[len^] := Store(IInterface(rec));
  Inc(len^);
end;

procedure FindRecordsBySignature(group: IwbGroupRecord; sig: TwbSignature; len: PInteger);
var
  i: Integer;
  element: IwbElement;
  rec: IwbMainRecord;
  subGroup: IwbGroupRecord;
begin
  if (group.GroupType = 0) and (TwbSignature(group.GroupLabel) = sig) then
    StoreRecords(group, len)
  else
    for i := 0 to Pred(group.ElementCount) do begin
      element := group.Elements[i];
      if Supports(element, IwbMainRecord, rec) then begin
        if rec.Signature = sig then
          StoreRecord(rec, len)
        else if Assigned(rec.ChildGroup) then
          FindRecordsBySignature(rec.ChildGroup, sig, len);
      end
      else if Supports(element, IwbGroupRecord, subGroup) then
        FindRecordsBySignature(subGroup, sig, len);
    end;
end;

procedure FindRecordsBySignature(_file: IwbFile; sig: TwbSignature; len: PInteger);
var
  i: Integer;
  group: IwbGroupRecord;
begin
  if _file.HasGroup(sig) then
    StoreRecords(_file.GroupBySignature[sig], len)
  else
    for i := 0 to _file.ElementCount do
      if Supports(_file.Elements[i], IwbGroupRecord, group) then
        FindRecordsBySignature(group, sig, len);
end;
{$endregion}

{$region 'API functions'}
function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    formID^ := rec.LoadOrderFormID;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    // TODO: Fix references
    rec.LoadOrderFormID := formID;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetRecords(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  e: IInterface;
  _file: IwbFile;
  group: IwbGroupRecord;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then
      StoreRecords(_file, len)
    else if Supports(e, IwbGroupRecord, group) then
      StoreRecords(group, len)
    else
      raise Exception.Create('Interface must be a file or group.');
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordsBySignature(_id: Cardinal; sig: PWideChar; len: PInteger): WordBool; cdecl;
var
  _sig: TwbSignature;
  _file: IwbFile;
  group: IwbGroupRecord;
begin
  Result := False;
  try
    len^ := 0;
    _sig := TwbSignature(AnsiString(sig));
    if Supports(Resolve(_id), IwbFile, _file) then
      FindRecordsBySignature(_file, _sig, len)
    else if Supports(Resolve(_id), IwbGroupRecord, group) then
      FindRecordsBySignature(group, _sig, len)
    else
      raise Exception.Create('Interface must be a file or group.');
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetOverrides(_id: Cardinal; count: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Error Message');
    count^ := rec.OverrideCount + 1;
    Result := True;
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

function GetReferences(_id: Cardinal; len: PInteger): WordBool; cdecl;
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
