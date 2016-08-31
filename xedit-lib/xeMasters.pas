unit xeMasters;

interface

  function CleanMasters(_file: Cardinal): WordBool; StdCall;
  function SortMasters(_file: Cardinal): WordBool; StdCall;
  function AddMaster(_file, _master: Cardinal): WordBool; StdCall;
  function RemoveMaster(_file, _master: Cardinal): WordBool; StdCall;

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

function CleanMasters(_file: Cardinal): WordBool; StdCall;
begin
  Result := false;
  // TODO
end;

function SortMasters(_file: Cardinal): WordBool; StdCall;
begin
  Result := false;
  // TODO
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
