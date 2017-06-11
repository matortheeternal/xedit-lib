unit xeRecordValues;

interface

uses
  wbInterface;

  {$region 'Native functions'}
  function EditorIDToFormID(_file: IwbFile; editorID: String): Cardinal;
  {$endregion}

  {$region 'API functions'}
  function EditorID(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function FullName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
  function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;
  {$endregion}

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMessages, xeMeta;

{$region 'Native functions'}
function EditorIDToFormID(_file: IwbFile; editorID: String): Cardinal;
var
  rec: IwbMainRecord;
begin
  rec := _file.RecordByEditorID[editorID];
  if not Assigned(rec) then
    raise Exception.Create('Failed to find record with Editor ID: ' + editorID + ' in file ' + _file.FileName);
  Result := rec.LoadOrderFormID;
end;
{$endregion}

{$region 'API functions'}
function EditorID(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    resultStr := rec.EditorID;
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function FullName(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    if not rec.ElementExists['FULL'] then
      raise Exception.Create('Record does not have a FULL name.');
    resultStr := rec.FullName;
    len^ := Length(resultStr);
    Result := True;
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
{$endregion}

end.
