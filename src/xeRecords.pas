unit xeRecords;

interface

uses
  xeMeta;

  function AddRecord(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl;
  function GetRecords(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function RecordsBySignature(_id: Cardinal; sig: PWideChar; len: PInteger): WordBool; cdecl;
  function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
  function RecordByFormID(_id, formID: Cardinal; _res: PCardinal): WordBool; cdecl;
  function RecordByEditorID(_id: Cardinal; edid: PWideChar; _res: PCardinal): WordBool; cdecl;
  function RecordByName(_id: Cardinal; full: PWideChar; _res: PCardinal): WordBool; cdecl;
  function OverrideCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function OverrideByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
  function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
  function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;
  function ExchangeReferences(_id, oldFormID, newFormID: Cardinal): WordBool; cdecl;
  function GetReferences(_id: Cardinal; len: PInteger): WordBool; cdecl;

implementation

uses
  Classes, SysUtils,
  // xedit units
  wbInterface, wbImplementation,
  // xelib units
  xeGroups, xeMessages;

function AddRecord(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  element: IwbElement;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      group := AddGroupIfMissing(_file, string(sig));
      element := group.Add(string(sig));
      StoreIfAssigned(IInterface(element), _res, Result);
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

procedure StoreRecords(_file: IwbFile; len: PInteger); overload;
var
  i: Integer;
begin
  len^ := _file.RecordCount;
  SetLength(resultArray, len^);
  for i := 0 to Pred(_file.RecordCount) do
    resultArray[i] := Store(_file.Records[i]);
end;

procedure StoreRecords(group: IwbGroupRecord; len: PInteger); overload;
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

function GetRecords(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  e: IInterface;
  _file: IwbFile;
  group: IwbGroupRecord;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then begin
      StoreRecords(_file, len);
      Result := True;
    end
    else if Supports(e, IwbGroupRecord, group) then begin
      StoreRecords(group, len);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordsBySignature(_id: Cardinal; sig: PWideChar; len: PInteger): WordBool; cdecl;
var
  _sig: TwbSignature;
  _file: IwbFile;
  i: Integer;
  group: IwbGroupRecord;
  rec: IwbMainRecord;
begin
  Result := False;
  try
    _sig := TwbSignature(AnsiString(sig));
    if Supports(Resolve(_id), IwbFile, _file) and _file.HasGroup(_sig) then begin
      group := _file.GroupBySignature[_sig];
      len^ := 0;
      SetLength(resultArray, group.ElementCount);
      for i := 0 to Pred(group.ElementCount) do
        if Supports(group.Elements[i], IwbMainRecord, rec) then begin
          resultArray[len^] := Store(group.Elements[i]);
          Inc(len^);
        end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
var
  e: IInterface;
  _file: IwbFile;
  _group: IwbGroupRecord;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then begin
      _res^ := Store(_file.Records[index]);
      Result := True;
    end
    else if Supports(e, IwbGroupRecord, _group) then begin
      _res^ := Store(_group.Elements[index]);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordByFormID(_id, formID: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      rec := _file.RecordByFormID[formID, True];
      StoreIfAssigned(IInterface(rec), _res, Result);
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FindRecordByEditorID(group: IwbGroupRecord; edid: string): IwbMainRecord;
var
  i: Integer;
  rec: IwbMainRecord;
begin
  for i := 0 to Pred(group.ElementCount) do
    if Supports(group.Elements[i], IwbMainRecord, rec) then
      if SameText(rec.EditorID, edid) then begin
        Result := rec;
        break;
      end;
end;

function RecordByEditorID(_id: Cardinal; edid: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  e: IInterface;
  _file: IwbFile;
  rec: IwbMainRecord;
  _group: IwbGroupRecord;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then
      rec := _file.RecordByEditorID[string(edid)]
    else if Supports(e, IwbGroupRecord, _group) then
      rec := FindRecordByEditorID(_group, string(edid));
    StoreIfAssigned(IInterface(rec), _res, Result);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FindRecordByName(_file: IwbFile; full: string): IwbMainRecord; overload;
var
  i: Integer;
  rec: IwbMainRecord;
  s: String;
begin
  for i := 0 to Pred(_file.RecordCount) do begin
    rec := _file.Records[i];
    s := rec.ElementEditValues['FULL'];
    if SameText(s, full) then begin
      Result := rec;
      break;
    end;
  end;
end;

function FindRecordByName(group: IwbGroupRecord; full: string): IwbMainRecord; overload;
var
  i: Integer;
  rec: IwbMainRecord;
  s: String;
begin
  for i := 0 to Pred(group.ElementCount) do
    if Supports(group.Elements[i], IwbMainRecord, rec) then begin
      s := rec.ElementEditValues['FULL'];
      if SameText(s, full) then begin
        Result := rec;
        break;
      end;
    end;
end;

function FindRecordByName(e: IInterface; full: string): IwbMainRecord; overload;
var
  _file: IwbFile;
  group: IwbGroupRecord;
begin
  if Supports(e, IwbFile, _file) then
    FindRecordByName(_file, full)
  else if Supports(e, IwbGroupRecord, group) then
    FindRecordByName(group, full);
end;

function RecordByName(_id: Cardinal; full: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    rec := FindRecordByName(Resolve(_id), string(full));
    StoreIfAssigned(IInterface(rec), _res, Result);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function OverrideCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      count^ := rec.OverrideCount;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function OverrideByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      _res^ := Store(rec.Overrides[index]);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      formID^ := rec.LoadOrderFormID;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// TODO: Fix references
function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      rec.LoadOrderFormID := formID;
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
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      rec.CompareExchangeFormID(oldFormID, newFormID);
      Result := True;
    end;
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
    if Supports(Resolve(_id), IwbMainRecord, rec) then begin
      len^ := rec.ReferencedByCount;
      SetLength(resultArray, len^);
      for i := 0 to Pred(rec.ReferencedByCount) do
        if Supports(rec.ReferencedBy[i], IwbMainRecord, ref) then
          resultArray[i] := Store(ref);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
