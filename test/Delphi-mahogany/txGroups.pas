unit txGroups;

interface

  // GROUP HANDLING METHODS
  function HasGroup(_id: Cardinal; sig: string; _res: PWordBool): WordBool; cdecl;
  function AddGroup(_id: Cardinal; sig: string; _res: PCardinal): WordBool; cdecl;
  function GetGroupSignatures(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; cdecl;
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GroupSignatureFromName(name, str: PWideChar): WordBool; cdecl;
  function GroupNameFromSignature(sig, str: PWideChar; len: Integer): WordBool; cdecl;
  function GetGroupSignatureNameMap(str: PWideChar; len: Integer): WordBool; cdecl;

  // PUBLIC TESTING INTERFACE
  procedure BuildGroupHandlingTests;

implementation

procedure BuildGroupHandlingTests;
begin

end;

end.
