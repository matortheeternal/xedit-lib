unit xeMasters;

interface

  function CleanMasters(_id: Cardinal): WordBool; cdecl;
  function SortMasters(_id: Cardinal): WordBool; cdecl;
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
  function GetMaster(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;

implementation

uses
  SysUtils,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMeta;


{******************************************************************************}
{ MASTER HANDLING
  Methods for handling masters on loaded files.
}
{******************************************************************************}

function CleanMasters(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.CleanMasters;
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortMasters(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.SortMasters;
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.AddMasterIfMissing(string(masterName));
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetMaster(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res^ := Store(_file.Masters[index]);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetMasters(_id: Cardinal; a: Variant; len: Integer): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      // TODO
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetMasterFileNames(_id: Cardinal; masterNames: PWideChar; len: Integer): WordBool; cdecl;
var
  _file: IwbFile;
  s: String;
  i: Integer;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      s := '';
      for i := 0 to Pred(_file.MasterCount) do
        s := s + _file.Masters[i].FileName + #13;
      StrLCopy(masterNames, PWideChar(WideString(Trim(s))), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
