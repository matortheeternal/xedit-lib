unit xeCore;

interface

uses
  Classes,
  // mte units
  mteBase, mteConflict,
  // xedit units
  wbInterface, wbImplementation;

type
  TErrorTypeID = ( erUnknown, erITM, erITPO, erUDR, erUES, erURR, erUER );
  TErrorType = record
    id: TErrorTypeID;
    shortName: string[4];
    longName: string[32];
    expr: string[128];
  end;
  TRecordError = class
  public
    &type: TErrorType;
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
  end;
  TRecordGroup = class
  public
    signature: TwbSignature;
    numRecords: Integer;
    numOverrides: Integer;
    constructor Create(signature: TwbSignature);
  end;
  TPlugin = class(TBasePlugin)
  public
    overrides: TStringList;
    errors: TList;
    groups: TList;
    constructor Create; override;
    destructor Destroy; override;
    procedure GetMdData(bExtData: boolean);
    procedure GetErrors;
    function GetGroup(sig: TwbSignature): TRecordGroup;
    procedure GetRecords;
  end;

  function PluginByFilename(sFilename: string): TPlugin;

const
  ErrorTypes: array[0..6] of TErrorType = (
    (id: erUnknown; shortName: 'UNK'; longName: 'Unknown'; expr: ''),
    (id: erITM; shortName: 'ITM'; longName: 'Identical to Master'; expr: ''),
    (id: erITPO; shortName: 'ITPO'; longName: 'Identical to Previous Override';
      expr: ''),
    (id: erUDR; shortName: 'UDR'; longName: 'Undelete and Disable Reference';
      expr: 'Record marked as deleted but contains: (\w+)'),
    (id: erUES; shortName: 'UES'; longName: 'Unexpected Subrecord';
      expr: 'Error: Record ([a-zA-Z_]+) contains unexpected \(or out of order\) subrecord (\w+)'),
    (id: erURR; shortName: 'URR'; longName: 'Unresolved Reference';
      expr: '\[([0-9A-F]+)\] \< Error: Could not be resolved \>'),
    (id: erUER; shortName: 'UER'; longName: 'Unexpected Reference';
      expr: 'Found a ([a-zA-Z_]+) reference, expected: (\w+)')
  );

implementation

uses
  SysUtils, Masks, RegularExpressions,
  // md units
  mdConfiguration;

function PluginByFilename(sFilename: string): TPlugin;
begin
  Result := TPlugin(TPluginHelpers.BasePluginByFilename(PluginsList, sFilename));
end;

function DumpErrors(aElement: IwbElement; var errors: TList): IwbMainRecord;
var
  error: string;
  errorObj: TRecordError;
  container: IwbContainerElementRef;
  i: Integer;
begin
  // check current element for errors
  Error := aElement.Check;

  // dump errors
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    if Assigned(Result) then begin
      errorObj := TRecordError.Create(Result, aElement, error);
      errors.Add(errorObj);
    end;
  end;

  // recursion
  if Supports(aElement, IwbContainerElementRef, container) then
    for i := Pred(container.ElementCount) downto 0 do
      Result := DumpErrors(Container.Elements[i], errors);
end;

procedure DumpSubrecordErrors(_File: IwbFile; var errors: TList);
var
  error: string;
  i: Integer;
  rec: IwbMainRecord;
  errorObj: TRecordError;
begin
  // loop through records in file
  for i := 0 to Pred(_File.RecordCount) do begin
    rec := _File.Records[i];

    // check record for out of order subrecords
    Error := rec.GetSubrecordErrors;

    if Error <> '' then begin
      errorObj := TRecordError.Create(rec, erUES, Error);
      errorObj.Data := Error;
      errors.Add(errorObj);
    end;
  end;
end;

procedure DumpIdenticalErrors(_File: IwbFile; var errors: TList);
var
  i: Integer;
  rec: IwbMainRecord;
  errorObj: TRecordError;
begin
  // loop through records in file
  for i := 0 to Pred(_File.RecordCount) do begin
    rec := _File.Records[i];

    // skip master records
    if rec.IsMaster then
      continue;

    // skip records that have elements in child group (WRLD, CELL, DIAL)
    if Assigned(rec.ChildGroup) and (rec.ChildGroup.ElementCount <> 0) then
      continue;

    // is the record an ITM or an ITPO?
    if IsITM(rec) then begin
      errorObj := TRecordError.Create(rec, erITM);
      errors.Add(errorObj);
    end
    else if IsITPO(rec) then begin
      errorObj := TRecordError.Create(rec, erITPO);
      errors.Add(errorObj);
    end;
  end;
end;

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
  if MatchesError(error, erUDR, 1, 0, &type, data)
  or MatchesError(error, erUES, 2, 0, &type, data)
  or MatchesError(error, erURR, 1, 0, &type, data)
  or MatchesError(error, erUER, 1, 2, &type, data) then
    exit;

  // error unknown
  &type := ErrorTypes[Ord(erUnknown)];
  data := error;
end;

{ TRecordError }
constructor TRecordError.Create(rec: IwbMainRecord; id: TErrorTypeID);
begin
  signature := rec.signature;
  formID := rec.FixedFormID;
  name := rec.Name;
  &type := ErrorTypes[Ord(id)];
end;

constructor TRecordError.Create(rec: IwbMainRecord; id: TErrorTypeID;
  error: string);
begin
  signature := rec.signature;
  formID := rec.FixedFormID;
  name := rec.Name;
  &type := ErrorTypes[Ord(id)];
  data := error;
end;

constructor TRecordError.Create(rec: IwbMainRecord; element: IwbElement;
  error: string);
begin
  signature := rec.signature;
  formID := rec.FixedFormID;
  name := rec.Name;
  path := element.Path;
  ParseError(error, &type, data);
end;

{ TRecordGroup }
constructor TRecordGroup.Create(signature: TwbSignature);
begin
  self.signature := signature;
  numRecords := 0;
  numOverrides := 0;
end;

{ TPlugin }
constructor TPlugin.Create;
begin
  overrides := TStringList.Create;
  errors := TList.Create;
  groups := TList.Create;
  inherited;
end;

destructor TPlugin.Destroy;
begin
  overrides.Free;
  errors.Free;
  groups.Free;
  inherited;
end;

procedure TPlugin.GetMdData(bExtData: boolean);
begin
  hasData := true;

  // get general data
  GetHash;
  GetData(PluginsList);

  // get extended data
  if bExtData {and (hash <> emptyPluginHash)} then begin
    GetErrors;
    GetRecords;
  end;
end;

procedure TPlugin.GetErrors;
begin
  // don't get errors if we've loaded an empty plugin
  if ProgramStatus.bUsedDummyPlugins then
    exit;

  // dump errors
  DumpErrors(_File as IwbElement, errors);
  DumpSubrecordErrors(_File, errors);
  DumpIdenticalErrors(_File, errors);
end;

function TPlugin.GetGroup(sig: TwbSignature): TRecordGroup;
var
  i: Integer;
  group: TRecordGroup;
begin
  Result := nil;

  // loop through groups that have been created
  for i := 0 to Pred(groups.Count) do begin
    group := TRecordGroup(groups[i]);
    if group.signature = sig then begin
      Result := group;
      break;
    end;
  end;

  // create group if it doesn't exist
  if not Assigned(Result) then begin
    group := TRecordGroup.Create(sig);
    groups.Add(group);
    Result := group;
  end;
end;

function CompareRecordGroups(Item1: Pointer; Item2: Pointer): Integer;
var
  group1, group2: TRecordGroup;
begin
  group1 := TRecordGroup(Item1);
  group2 := TRecordGroup(Item2);

  Result := AnsiCompareText(string(group1.signature), string(group2.signature));
end;

procedure TPlugin.GetRecords;
var
  i: Integer;
  rec: IwbMainRecord;
  group: TRecordGroup;
begin
  // loop through plugin's records
  for i := 0 to Pred(_File.RecordCount) do begin
    rec := _File.Records[i];
    group := GetGroup(rec.Signature);
    Inc(group.numRecords);
    // the record is an override, or injected
    if (not rec.IsMaster) or rec.IsInjected then begin
      Inc(numOverrides);
      Inc(group.numOverrides);
      overrides.AddObject(String(rec.Signature), TObject(rec.FixedFormID));
    end;
  end;

  // sort the groups list
  groups.Sort(CompareRecordGroups);
end;

end.
