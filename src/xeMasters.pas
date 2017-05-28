unit xeMasters;

interface

uses
  wbInterface;

  function CleanMasters(_id: Cardinal): WordBool; cdecl;
  function SortMasters(_id: Cardinal): WordBool; cdecl;
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
  function GetMaster(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; cdecl;
  function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl;

  // native functions
  function NativeFileHasMaster(_file, _master: IwbFile): Boolean;

implementation

uses
  SysUtils,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMeta, xeSetup;


{******************************************************************************}
{ MASTER HANDLING
  Methods for handling masters on loaded files.
}
{******************************************************************************}

function CleanMasters(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.CleanMasters;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortMasters(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.SortMasters;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.AddMasterIfMissing(string(masterName));
      Result := True;
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

function NativeFileHasMaster(_file, _master: IwbFile): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Pred(_file.MasterCount) do
    if _file.Masters[i].FileName = _master.FileName then begin
      Result := True;
      break;
    end;
end;

{$POINTERMATH ON}
function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      len^ := _file.MasterCount;
      SetLength(resultArray, len^);
      for i := 0 to Pred(_file.MasterCount) do
        resultArray[i] := Store(_file.Masters[i]);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file, f: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      len^ := 0;
      SetLength(resultArray, High(xFiles) + 1);
      for i := Low(xFiles) to High(xFiles) do begin
        f := xFiles[i];
        if NativeFileHasMaster(f, _file) then begin
          resultArray[len^] := Store(f);
          Inc(len^);
        end;
      end;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$POINTERMATH OFF}

end.
