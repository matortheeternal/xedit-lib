unit xeElements;

interface

  function GetElement(_id: Cardinal; key: PWideChar): Cardinal; StdCall;
  function NewElement(_id: Cardinal; key: PWideChar): Cardinal; StdCall;
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; StdCall;
  function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; StdCall;
  function ElementExists(_id: Cardinal; key: PWideChar): WordBool; StdCall;
  function ElementCount(_id: Cardinal): Integer; StdCall;
  function ElementAssigned(_id: Cardinal): WordBool; StdCall;
  function Equals(_id, _id2: Cardinal): WordBool; StdCall;
  function IsMaster(_id: Cardinal): WordBool; StdCall;
  function IsInjected(_id: Cardinal): WordBool; StdCall;
  function IsOverride(_id: Cardinal): WordBool; StdCall;
  function IsWinningOverride(_id: Cardinal): WordBool; StdCall;

  // serialization and deserialization
  //function ElementToJson(_id: Cardinal; str: PWideChar; len: Integer): WordBool; StdCall;
  //function ElementToXML(_id: Cardinal; str: PWideChar; len: Integer): WordBool; StdCall;
  //function JsonToElement(_id: Cardinal; json: PWideChar; _res: Cardinal): WordBool; StdCall;
  //function XMLToElement(_id: Cardinal; xml: PWideChar; _res: Cardinal): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // third party units
  SuperObject,
  // xedit units
  wbInterface, wbImplementation,
  // xelib units
  xeMeta;


{******************************************************************************}
{ ELEMENT HANDLING
  Methods for handling elements: groups, records, and subrecords.
}
{******************************************************************************}

function ParseIndex(key: string): Integer;
begin
  Result := -1;
  if (key[1] = '[') and (key[Length(key)] = ']') then
    Result := StrToInt(Copy(key, 2, Length(key) - 2));
end;

// Replaces ElementByName, ElementByPath, ElementByIndex, GroupBySignature, and
// ElementBySignature.  Supports indexed paths.
function GetElement(_id: Cardinal; key: PWideChar): Cardinal; StdCall;
var
  e: IInterface;
  _file: IwbFile;
  container: IwbContainerElementRef;
begin
  Result := 0;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then
      // TODO: perhaps also by group name?
      Result := Store(_file.GroupBySignature[StrToSignature(key)])
    else if Supports(e, IwbContainerElementRef, container) then
      Result := Store(container.ElementByPath[string(key)]);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// replaces ElementAssign, Add, AddElement, and InsertElement
function NewElement(_id: Cardinal; key: PWideChar): Cardinal; StdCall;
var
  e: IInterface;
  container: IwbContainerElementRef;
  keyIndex: Integer;
begin
  Result := 0;
  try
    e := Resolve(_id);
    if Supports(e, IwbContainerElementRef, container) then begin
      // Use Add for files and groups
      if Supports(e, IwbFile) or Supports(e, IwbGroupRecord) then
        Result := Store(container.Add(string(key), true))
      else begin
        // no key means we're doing ElementAssign
        if (not Assigned(key)) or (Length(key) = 0) then
          Result := Store(container.Assign(High(integer), nil, false))
        else begin
          keyIndex := ParseIndex(key);
          // use InsertElement if index was given, else use Add
          if keyIndex > -1 then
            Result := Store(container.Assign(keyIndex, nil, false))
          else
            Result := Store(container.Add(string(key), true));
        end;
      end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; StdCall;
var
  e: IInterface;
begin
  Result := false;
  try
    e := Resolve(_id);
    // TODO
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; StdCall;
var
  element, linkedElement: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then begin
      linkedElement := element.LinksTo;
      if Assigned(linkedElement) then begin
        _res^ := Store(linkedElement);
        Result := true;
      end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// Replaces HasGroup and ElementExists
function ElementExists(_id: Cardinal; key: PWideChar): WordBool; StdCall;
var
  element: IInterface;
  _file: IwbFile;
  container: IwbContainerElementRef;
begin
  Result := false;
  try
    element := Resolve(_id);
    if Supports(element, IwbFile, _file) then
      // TODO: perhaps also by group name?
      Result := _file.HasGroup(StrToSignature(key))
    else if Supports(element, IwbContainerElementRef, container) then
      // TODO: adjust logic here so we can check paths
      Result := container.ElementExists[string(key)];
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementCount(_id: Cardinal): Integer; StdCall;
var
  container: IwbContainerElementRef;
begin
  Result := -1;
  try
    if Supports(Resolve(_id), IwbContainerElementRef, container) then
      Result := container.ElementCount;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ElementAssigned(_id: Cardinal): WordBool; StdCall;
var
  e: IInterface;
begin
  Result := false;
  try
    e := Resolve(_id);
    Result := Assigned(e);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function Equals(_id, _id2: Cardinal): WordBool; StdCall;
var
  element, element2: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbElement, element) then
      if Supports(Resolve(_id2), IwbElement, element2) then
        Result := element.Equals(element2);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsMaster(_id: Cardinal): WordBool; StdCall;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := rec.IsMaster;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsInjected(_id: Cardinal): WordBool; StdCall;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := rec.IsInjected;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function IsOverride(_id: Cardinal): WordBool; StdCall;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := not rec.IsMaster;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

// TODO: Determine if subrecord is winner
function IsWinningOverride(_id: Cardinal): WordBool; StdCall;
var
  rec: IwbMainRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, rec) then
      Result := not rec.IsWinningOverride;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
