unit xeGroups;

interface

uses
  wbInterface;

  {$region 'Native functions'}
  function IsChildGroup(group: IwbGroupRecord): Boolean;
  function AddGroupIfMissing(_file: IwbFile; sig: string): IwbGroupRecord;
  {$endregion}

  {$region 'API functions'}
  function HasGroup(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool; cdecl;
  function AddGroup(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl;
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  {$endregion}

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMessages, xeMeta, xeSetup;

{$region 'Native functions'}
function IsChildGroup(group: IwbGroupRecord): Boolean;
begin
  Result := group.GroupType in [1,6,7];
end;

function AddGroupIfMissing(_file: IwbFile; sig: String): IwbGroupRecord;
var
  _sig: TwbSignature;
begin
  _sig := TwbSignature(sig);
  if _file.HasGroup(_sig) then
    Result := _file.GroupBySignature[_sig]
  else
    Supports(_file.Add(sig), IwbGroupRecord, Result);
end;
{$endregion}

{$region 'API functions'}
function HasGroup(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      bool^ := _file.HasGroup(TwbSignature(AnsiString(sig)));
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddGroup(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res^ := Store(AddGroupIfMissing(_file, string(sig)));
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  _rec: IwbMainRecord;
  _group: IwbGroupRecord;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbMainRecord, _rec) then begin
      _group := _rec.ChildGroup;
      if Assigned(_group) then begin
        _res^ := Store(_group);
        Result := True;
      end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}


end.
