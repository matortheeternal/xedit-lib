unit txGroups;

interface

  // GROUP HANDLING METHODS
  function HasGroup(_id: Cardinal; sig: PWideChar; _res: PWordBool): WordBool; cdecl; external 'XEditLib.dll';
  function AddGroup(_id: Cardinal; sig: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetGroupSignatures(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GroupSignatureFromName(name, str: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GroupNameFromSignature(sig, str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetGroupSignatureNameMap(str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildGroupHandlingTests;

implementation

procedure BuildGroupHandlingTests;
begin

end;

end.
