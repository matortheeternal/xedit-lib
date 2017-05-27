unit xeGroups;

interface

uses
  wbInterface;

  function HasGroup(_id: Cardinal; sig: PWideChar; bool: PWordBool): WordBool; cdecl;
  function AddGroup(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl;
  function GetGroupSignatures(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; cdecl;
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GroupSignatureFromName(name, sig: PWideChar): WordBool; cdecl;
  function GroupNameFromSignature(sig, name: PWideChar; len: Integer): WordBool; cdecl;
  function GetGroupSignatureNameMap(str: PWideChar; len: Integer): WordBool; cdecl;

  // native functions
  function IsChildGroup(group: IwbGroupRecord): Boolean;
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

function IsChildGroup(group: IwbGroupRecord): Boolean;
begin
  Result := group.GroupType in [1,6,7];
end;

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

function GetGroupSignatures(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; cdecl;
var
  _file: IwbFile;
  s: String;
  i: Integer;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      s := '';
      for i := 1 to _file.ElementCount do
        s := s + string(IwbGroupRecord(_file.Elements[i]).Signature) + #13;
      StrLCopy(groups, PWideChar(s), len);
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

function GroupSignatureFromName(name, sig: PWideChar): WordBool; cdecl;
var
  str: String;
begin
  Result := False;
  try
    BuildGroupNameMap;
    if slGroupNameMap.IndexOfName(string(name)) > -1 then begin
      str := slGroupNameMap.Values[string(name)];
      StrLCopy(sig, PWideChar(str), 4);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GroupNameFromSignature(sig, name: PWideChar; len: Integer): WordBool; cdecl;
var
  str: String;
  RecordDef: PwbRecordDef;
begin
  Result := False;
  try
    if wbFindRecordDef(AnsiString(sig), RecordDef) then begin
      str := RecordDef.Name;
      StrLCopy(name, PWideChar(WideString(str)), len);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetGroupSignatureNameMap(str: PWideChar; len: Integer): WordBool; cdecl;
var
  text: String;
begin
  Result := False;
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
