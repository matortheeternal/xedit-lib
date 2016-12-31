unit xeGroups;

interface

uses
  wbInterface;

  function HasGroup(_id: Cardinal; sig: string; _res: PWordBool): WordBool; cdecl;
  function AddGroup(_id: Cardinal; sig: string; _res: PCardinal): WordBool; cdecl;
  function GetGroupSignatures(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; cdecl;
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GroupSignatureFromName(name, str: PWideChar): WordBool; cdecl;
  function GroupNameFromSignature(sig, str: PWideChar; len: Integer): WordBool; cdecl;
  function GetGroupSignatureNameMap(str: PWideChar; len: Integer): WordBool; cdecl;

  // native functions
  function AddGroupIfMissing(_file: IwbFile; sig: string): IwbGroupRecord;
  procedure BuildGroupNameMap;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMessages, xeMeta, xeSetup;

var
  slGroupNameMap: TStringList;
  bGroupNameMapBuilt: Boolean;


{******************************************************************************}
{ GROUP HANDLING
  Methods for handling groups.
}
{******************************************************************************}

function HasGroup(_id: Cardinal; sig: string; _res: PWordBool): WordBool; cdecl;
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

function AddGroup(_id: Cardinal; sig: string; _res: PCardinal): WordBool; cdecl;
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

function GetGroupSignatures(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; cdecl;
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

function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
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

function GroupSignatureFromName(name, str: PWideChar): WordBool; cdecl;
var
  sig: String;
begin
  Result := false;
  try
    BuildGroupNameMap;
    if slGroupNameMap.IndexOfName(name) > -1 then begin
      sig := slGroupNameMap.Values[name];
      StrLCopy(str, PWideChar(sig), 4);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GroupNameFromSignature(sig, str: PWideChar; len: Integer): WordBool; cdecl;
var
  name: String;
  RecordDef: PwbRecordDef;
begin
  Result := false;
  try
    if wbFindRecordDef(AnsiString(sig), RecordDef) then begin
      name := RecordDef.Name;
      StrLCopy(str, PWideChar(name), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetGroupSignatureNameMap(str: PWideChar; len: Integer): WordBool; cdecl;
var
  text: String;
begin
  Result := false;
  try
    BuildGroupNameMap;
    text := slGroupNameMap.Text;
    Delete(text, Length(text), 1);
    StrLCopy(str, PWideChar(text), len);
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

procedure BuildGroupNameMap;
var
  i: Integer;
  sig: string;
  RecordDef: PwbRecordDef;
begin
  if bGroupNameMapBuilt then exit;
  for i := 0 to Pred(wbGroupOrder.Count) do begin
    sig := wbGroupOrder[i];
    if wbFindRecordDef(AnsiString(sig), RecordDef) then
      slGroupNameMap.Values[sig] := RecordDef.Name;
  end;
  bGroupNameMapBuilt := True;
end;

initialization
begin
  slGroupNameMap := TStringList.Create;
  slGroupNameMap.Sorted := True;
  slGroupNameMap.Duplicates := dupIgnore;
  bGroupNameMapBuilt := False;
end;


finalization
begin
  slGroupNameMap.Free;
end;


end.
