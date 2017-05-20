unit txMasters;

interface

  // MASTER HANDLING METHODS
  function CleanMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SortMasters(_id: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetMaster(_id: Cardinal; index: Integer): Cardinal; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildMasterHandlingTests;

implementation

procedure BuildMasterHandlingTests;
begin

end;

end.
