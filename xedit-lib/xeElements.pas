unit xeElements;

interface

  function GetElement(_id: Cardinal; key: PWideChar): Cardinal; cdecl;
  function NewElement(_id: Cardinal; key: PWideChar): Cardinal; cdecl;
  function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl;
  function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl;
  function ElementCount(_id: Cardinal): Integer; cdecl;
  function ElementAssigned(_id: Cardinal): WordBool; cdecl;
  function Equals(_id, _id2: Cardinal): WordBool; cdecl;
  function IsMaster(_id: Cardinal): WordBool; cdecl;
  function IsInjected(_id: Cardinal): WordBool; cdecl;
  function IsOverride(_id: Cardinal): WordBool; cdecl;
  function IsWinningOverride(_id: Cardinal): WordBool; cdecl;

  // serialization and deserialization
  //function ElementToJson(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  //function ElementToXML(_id: Cardinal; str: PWideChar; len: Integer): WordBool; cdecl;
  //function JsonToElement(_id: Cardinal; json: PWideChar; _res: Cardinal): WordBool; cdecl;
  //function XMLToElement(_id: Cardinal; xml: PWideChar; _res: Cardinal): WordBool; cdecl;

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
function GetElement(_id: Cardinal; key: PWideChar): Cardinal; cdecl;
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
function NewElement(_id: Cardinal; key: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  e: IInterface;
  container: IwbContainerElementRef;
  keyIndex: Integer;
begin
  Result := False;
  try
    e := Resolve(_id);
    if Supports(e, IwbContainerElementRef, container) then begin
      // Use Add for files and groups
      if Supports(e, IwbFile) or Supports(e, IwbGroupRecord) then
        _res^ := Store(container.Add(string(key), true))
      else begin
        // no key means we're assigning an element at the end of the array
        if (not Assigned(key)) or (Length(key) = 0) then
          _res^ := Store(container.Assign(High(integer), nil, false))
        else begin
          keyIndex := ParseIndex(key);
          // assign element at given index if index given, else add
          if keyIndex > -1 then
            _res^ := Store(container.Assign(keyIndex, nil, false))
          else
            _res^ := Store(container.Add(string(key), true));
        end;
      end;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RemoveElement(_id: Cardinal; key: PWideChar): WordBool; cdecl;
var
  e: IInterface;
  element: IwbElement;
  container: IwbContainerElementRef;
  keyIndex: Integer;
begin
  Result := false;
  try
    e := Resolve(_id);
    if not Supports(e, IwbElement, element) then
      exit;
    if (not Assigned(key)) or (Length(key) = 0) then
      element.Remove
    else begin
      if not Supports(e, IwbContainerElementRef, container) then
        exit;
      container.ElementByPath[key].Remove;
    end;
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function LinksTo(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
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
function ElementExists(_id: Cardinal; key: PWideChar): WordBool; cdecl;
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

function ElementCount(_id: Cardinal): Integer; cdecl;
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

function ElementAssigned(_id: Cardinal): WordBool; cdecl;
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

function Equals(_id, _id2: Cardinal): WordBool; cdecl;
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

function IsMaster(_id: Cardinal): WordBool; cdecl;
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

function IsInjected(_id: Cardinal): WordBool; cdecl;
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

function IsOverride(_id: Cardinal): WordBool; cdecl;
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
function IsWinningOverride(_id: Cardinal): WordBool; cdecl;
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
