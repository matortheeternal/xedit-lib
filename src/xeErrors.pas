unit xeErrors;

interface

uses
  Classes,
  //xedit units
  wbInterface,
  // xelib units
  xeMeta;

type
  {$region 'Types'}
  TErrorTypeID = ( erITM, erITPO, erDR, erUES, erURR, erUER, erUnknown );
  TErrorType = record
    id: TErrorTypeID;
    shortName: string[4];
    longName: string[32];
    expr: string[128];
  end;
  TRecordError = class
  public
    &type: TErrorType;
    handle: Cardinal;
    signature: TwbSignature;
    formID: integer;
    name: string;
    path: string;
    data: string;
    constructor Create(rec: IwbMainRecord; id: TErrorTypeID); overload;
    constructor Create(rec: IwbMainRecord; id: TErrorTypeID;
      error: string); overload;
    constructor Create(rec: IwbMainRecord; element: IwbElement;
      error: string); overload;
    procedure Init(rec: IwbMainRecord);
  end;
  TErrorCheckThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  {$endregion}

  {$region 'API functions'}
  function CheckForErrors(_id: Cardinal): WordBool; cdecl;
  function GetErrorThreadDone: WordBool; cdecl;
  function GetErrors(len: PInteger): WordBool; cdecl;
  {$endregion}

const
  ErrorTypes: array[0..6] of TErrorType = (
    (id: erITM; shortName: 'ITM'; longName: 'Identical to Master'; expr: ''),
    (id: erITPO; shortName: 'ITPO'; longName: 'Identical to Previous Override';
      expr: ''),
    (id: erDR; shortName: 'DR'; longName: 'Deleted Record';
      expr: 'Record marked as deleted but contains: (.+)'),
    (id: erUES; shortName: 'UES'; longName: 'Unexpected Subrecord';
      expr: 'Error: Record contains unexpected \(or out of order\) subrecord (.+)'),
    (id: erURR; shortName: 'URR'; longName: 'Unresolved Reference';
      expr: '\[([0-9A-F]+)\] \< Error: Could not be resolved \>'),
    (id: erUER; shortName: 'UER'; longName: 'Unexpected Reference';
      expr: 'Found a ([a-zA-Z_]+) reference, expected: (.+)'),
    (id: erUnknown; shortName: 'UNK'; longName: 'Unknown'; expr: '')
  );
  ReferenceSignatures: array[0..10] of String = (
    'REFR', 'PGRE', 'PMIS', 'ACHR', 'ACRE', 'PARW',
    'PBAR', 'PBEA', 'PCON', 'PFLA', 'PHZD'
  );

implementation

uses
  SysUtils, StrUtils, Masks, RegularExpressions,
  // mte units
  mteConflict,
  // xelib units
  xeMessages, xeElementValues,
  // library units
  Argo;

var
  errors: TList;
  bErrorCheckThreadDone: Boolean;
  elementToCheck: IwbElement;

{$region 'Native functions'}
{$region 'CheckForErrors helpers'}
procedure CheckForSubrecordErrors(rec: IwbMainRecord);
var
  error: String;
begin
  error := rec.GetSubRecordErrors;
  if error <> '' then
    errors.Add(TRecordError.Create(rec, erUES, Error));
end;  

procedure CheckForIdenticalErrors(rec: IwbMainRecord);
begin
  if rec.IsMaster or rec.Master.IsInjected then exit;
  if IsITM(rec) then
    errors.Add(TRecordError.Create(rec, erITM))
  else if IsITPO(rec) then
    errors.Add(TRecordError.Create(rec, erITPO));
end;

procedure CheckForDeletedErrors(rec: IwbMainRecord);
var
  sig: String;
begin
  sig := string(rec.Signature);
  if sig = 'NAVM' then
    errors.Add(TRecordError.Create(rec, erUnknown, 'Navmesh marked as deleted'))
  else if MatchStr(sig, ReferenceSignatures) then
    errors.Add(TRecordError.Create(rec, erDR, 'Reference marked as deleted'));
end;

function NativeCheckForErrors(element: IwbElement; lastRecord: IwbMainRecord): IwbMainRecord;
var
  rec: IwbMainRecord;
  error: String;            
  container: IwbContainerElementRef;
  i: Integer;
begin
  error := element.Check;

  // special main record error checks (ITM, ITPO, DR)
  if Supports(element, IwbMainRecord, rec) then begin
    CheckForSubrecordErrors(rec);
    CheckForIdenticalErrors(rec);
    if (error = '') and rec.isDeleted then
      CheckForDeletedErrors(rec);
  end;

  // general error checking     
  if error <> '' then begin
    Result := element.ContainingMainRecord;
    if Assigned(Result) then begin
      if (Result <> LastRecord) then
        AddMessage(Result.Name);
      errors.Add(TRecordError.Create(Result, element, error));
    end;
    // print error message to log
    if not Supports(element, IwbMainRecord) then
      AddMessage(Format('  %s -> %s', [element.Path, error]))
    else
      AddMessage('  ' + error);
  end;
  
  // recursion
  if Supports(element, IwbContainerElementRef, container) then
    for i := Pred(container.ElementCount) downto 0 do
      Result := NativeCheckForErrors(container.Elements[i], Result);
end;

procedure TErrorCheckThread.Execute;
begin
  try
    AddMessage('Checking for errors in ' + NativeName(elementToCheck));
    NativeCheckForErrors(elementToCheck, nil);
  except
    on x: Exception do
      ExceptionHandler(x);
  end;      
  bErrorCheckThreadDone := True;
end;
{$endregion}

{$region 'GetErrors helpers'}
function ErrorToJson(error: TRecordError): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.I['group'] := Ord(error.&type.id);
    Result.I['handle'] := error.handle;
    Result.S['signature'] := string(error.signature);
    Result.I['form_id'] := error.formID;
    Result.S['name'] := error.name;
    if error.path <> '' then
      Result.S['path'] := error.path;
    if error.data <> '' then
      Result.S['data'] := error.data;
  except
    on x: Exception do begin
      Result.Free;
      raise x;
    end;
  end;
end;
{$endregion}

{$region 'Error parsing'}
function MatchesError(error: string; errorID: TErrorTypeID;
  i1, i2: Integer; var &type: TErrorType; var data: string): boolean;
var
  errorType: TErrorType;
  regex: TRegex;
  match: TMatch;
begin
  errorType := ErrorTypes[Ord(errorID)];
  regex := TRegex.Create(string(errorType.expr));
  match := regex.Match(error);
  Result := match.success;

  // if the expression matches the error, use its type and
  // parse data from regex groups
  if match.success then begin
    &type := errorType;
    if i1 > 0 then begin
      data := match.Groups.Item[i1].Value;
      if i2 > 0 then
        data := data + ',' + match.Groups.Item[i2].Value;
    end;
  end;
end;

procedure ParseError(error: string; var &type: TErrorType;
  var data: string);
begin
  // test errors with regex expressions, and if they match use
  // their type and parse data from the correct regex groups
  if MatchesError(error, erDR, 1, 0, &type, data)
  or MatchesError(error, erUES, 2, 0, &type, data)
  or MatchesError(error, erURR, 1, 0, &type, data)
  or MatchesError(error, erUER, 1, 2, &type, data) then
    exit;

  // error unknown
  &type := ErrorTypes[Ord(erUnknown)];
  data := error;
end;
{$endregion}

{$region 'TRecordError'}
constructor TRecordError.Create(rec: IwbMainRecord; id: TErrorTypeID);
begin
  Init(rec);
  &type := ErrorTypes[Ord(id)];
end;

constructor TRecordError.Create(rec: IwbMainRecord; id: TErrorTypeID;
  error: string);
begin
  Init(rec);
  &type := ErrorTypes[Ord(id)];
  data := error;
end;

constructor TRecordError.Create(rec: IwbMainRecord; element: IwbElement;
  error: string);
begin
  Init(rec);
  if not Supports(element, IwbMainRecord) then
    path := GetPath(element, false);
  ParseError(error, &type, data);
end;

procedure TRecordError.Init(rec: IwbMainRecord);
begin
  handle := Store(rec);
  signature := rec.signature;
  formID := rec.FixedFormID;
  name := rec.Name;
end;
{$endregion}
{$endregion}

{$region 'API functions'}
function CheckForErrors(_id: Cardinal): WordBool; cdecl;
var
  element: IwbElement;
begin
  Result := False;
  try
    if not bErrorCheckThreadDone then
      raise Exception.Create('You''re currently checking a plugin for errors.');
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('Input interface must be an element.');
    errors := TList.Create;
    bErrorCheckThreadDone := False;
    elementToCheck := element;
    TErrorCheckThread.Create;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetErrorThreadDone: WordBool; cdecl;
begin
  Result := bErrorCheckThreadDone;
end;

function GetErrors(len: PInteger): WordBool; cdecl;
var
  i: Integer;      
  obj: TJSONObject;
  error: TRecordError;
begin
  Result := False;
  try
    obj := TJSONObject.Create;
    try
      obj.A['errors'] := TJSONArray.Create;
      for i := 0 to Pred(errors.Count) do begin
        error := TRecordError(errors[i]);
        obj.A['errors'].Add(ErrorToJson(error));
      end;
      resultStr := obj.ToString;
      len^ := Length(resultStr);
      Result := True;
    finally
      obj.Free;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

initialization
  bErrorCheckThreadDone := True;

end.
