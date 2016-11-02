unit xeGroups;

interface

uses
  wbInterface;

  function HasGroup(_id: Cardinal; sig: string; _res: PWordBool): WordBool; StdCall;
  function AddGroupIfMissing(_file: IwbFile; sig: string): IwbGroupRecord;
  function AddGroup(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
  function GetGroups(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; StdCall;
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMessages, xeMeta, xeSetup;


{******************************************************************************}
{ GROUP HANDLING
  Methods for handling groups.
}
{******************************************************************************}

function HasGroup(_id: Cardinal; sig: string; _res: PWordBool): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res^ := _file.HasGroup(TwbSignature(sig));
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddGroupIfMissing(_file: IwbFile; sig: string): IwbGroupRecord;
var
  _sig: TwbSignature;
begin
  _sig := TwbSignature(sig);
  if _file.HasGroup(_sig) then
    Result := _file.GroupBySignature[_sig]
  else
    Supports(_file.Add(sig), IwbGroupRecord, Result);
end;

function AddGroup(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res^ := Store(AddGroupIfMissing(_file, sig));
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetGroups(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; StdCall;
var
  _file: IwbFile;
  s: String;
  i: Integer;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      s := '';
      for i := 1 to _file.ElementCount do
        s := s + string(IwbGroupRecord(_file.Elements[i]).Signature) + #13;
      StrLCopy(groups, PWideChar(s), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; StdCall;
var
  _rec: IwbMainRecord;
  _group: IwbGroupRecord;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbMainRecord, _rec) then begin
      _group := _rec.ChildGroup;
      if Assigned(_group) then begin
        _res^ := Store(_group);
        Result := true;
      end;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;


end.
