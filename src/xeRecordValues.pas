unit xeRecordValues;

interface

  function EditorID(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function FullName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetFormID(_id: Cardinal; formID: PCardinal): WordBool; cdecl;
  function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta;

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

// TODO: Fix references
function SetFormID(_id: Cardinal; formID: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    rec.LoadOrderFormID := formID;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;



end.
