unit xeMasters;

interface

  function AddMaster(_file, _master: Cardinal): WordBool; StdCall;
  function RemoveMaster(_file, _master: Cardinal): WordBool; StdCall;
  function CleanMasters(_id: Cardinal): WordBool; StdCall;
  function SortMasters(_id: Cardinal): WordBool; StdCall;

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

function CleanMasters(_id: Cardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then
      _file.CleanMasters;
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortMasters(_id: Cardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then
      _file.SortMasters;
    Result := true;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddMaster(_file, _master: Cardinal): WordBool; StdCall;
begin
  Result := false;
  // TODO
end;

function RemoveMaster(_file, _master: Cardinal): WordBool; StdCall;
begin
  Result := false;
  // TODO
end;

end.
