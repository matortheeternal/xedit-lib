unit txMasters;

interface

  // MASTER HANDLING METHODS
  function CleanMasters(_id: Cardinal): WordBool; cdecl;
  function SortMasters(_id: Cardinal): WordBool; cdecl;
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
  function GetMaster(_id: Cardinal; index: Integer): Cardinal; cdecl;

  // PUBLIC TESTING INTERFACE
  procedure BuildMasterHandlingTests;

implementation

procedure BuildMasterHandlingTests;
begin

end;

end.
