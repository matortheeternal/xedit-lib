unit xeRecords;

interface

  function AddRecord(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
  //function GetRecords(_id: Cardinal; _res: PCardinalArray): WordBool; StdCall;
  function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; StdCall;
  //function RecordsBySignature(_id: Cardinal; sig: string; _res: PCardinalArray): WordBool; StdCall;
  function RecordByFormID(_id, formID: Cardinal; _res: PCardinal): WordBool; StdCall;
  function RecordByEditorID(_id: Cardinal; edid: string; _res: PCardinal): WordBool; StdCall;
  function RecordByName(_id: Cardinal; full: string; _res: PCardinal): WordBool; StdCall;
  //function RecordSignatureFromName(name, str: PWideChar): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // xedit units
  wbInterface, wbImplementation,
  // xelib units
  xeGroups, xeMeta;

function AddRecord(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  element: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      group := AddGroupIfMissing(_file, sig);
      element := group.Add(sig);
      StoreIfAssigned(IInterface(element), _res, Result);
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; StdCall;
var
  e: IInterface;
  _file: IwbFile;
  _group: IwbGroupRecord;
begin
  Result := false;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then begin
      _res^ := Store(_file.Records[index]);
      Result := true;
    end
    else if Supports(e, IwbGroupRecord, _group) then begin
      _res^ := Store(_group.Elements[index]);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordByFormID(_id, formID: Cardinal; _res: PCardinal): WordBool; StdCall;
var
  _file: IwbFile;
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      rec := _file.RecordByFormID[formID, true];
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

function RecordByEditorID(_id: Cardinal; edid: string; _res: PCardinal): WordBool; StdCall;
var
  e: IInterface;
  _file: IwbFile;
  rec: IwbMainRecord;
  _group: IwbGroupRecord;
begin
  Result := false;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then
      rec := _file.RecordByEditorID[edid]
    else if Supports(e, IwbGroupRecord, _group) then
      rec := FindRecordByEditorID(_group, edid);
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

function RecordByName(_id: Cardinal; full: string; _res: PCardinal): WordBool; StdCall;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    rec := FindRecordByName(Resolve(_id), full);
    StoreIfAssigned(IInterface(rec), _res, Result);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
