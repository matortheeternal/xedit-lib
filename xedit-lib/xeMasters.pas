unit xeMasters;

interface

uses
  Classes,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMeta;

  // MASTER HANDLING
  function CleanMasters(_file: Integer): WordBool; StdCall;
  function SortMasters(_file: Integer): WordBool; StdCall;
  function AddMaster(_file, _master: Integer): WordBool; StdCall;
  function RemoveMaster(_file, _master: Integer): WordBool; StdCall;

implementation


{******************************************************************************}
{ MASTER HANDLING
  Methods for handling masters on loaded files.
}
{******************************************************************************}

function CleanMasters(_file: Cardinal): WordBool; StdCall;
begin
end;

function SortMasters(_file: Cardinal): WordBool; StdCall;
begin
end;

function AddMaster(_file, _master: Cardinal): WordBool; StdCall;
begin
end;

function RemoveMaster(_file, _master: Cardinal): WordBool; StdCall;
begin
end;

end.
