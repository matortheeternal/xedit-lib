unit txElementValues;

interface

  // ELEMENT VALUE METHODS
  function Name(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function Path(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function EditorID(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function Signature(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function ShortName(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function SortKey(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function ElementType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function DefType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl;
  function GetValue(_id: Integer; path, str: PWideChar; len: Integer): WordBool; cdecl;
  function SetValue(_id: Integer; path, value: PWideChar): WordBool; cdecl;
  function GetIntValue(_id: Integer; path: PWideChar; out value: Integer): WordBool; cdecl;
  function SetIntValue(_id: Integer; path: PWideChar; value: Integer): WordBool; cdecl;
  function GetUIntValue(_id: Integer; path: PWideChar; out value: Cardinal): WordBool; cdecl;
  function SetUIntValue(_id: Integer; path: PWideChar; value: Cardinal): WordBool; cdecl;
  function GetFloatValue(_id: Integer; path: PWideChar; out value: Double): WordBool; cdecl;
  function SetFloatValue(_id: Integer; path: PWideChar; value: Double): WordBool; cdecl;
  function GetLinksTo(_id: Integer; path: PWideChar; _res: PCardinal): WordBool; cdecl;
  function SetFlag(_id: Integer; path, name: PWideChar; enabled: WordBool): WordBool; cdecl;
  function GetFlag(_id: Integer; path, name: PWideChar): WordBool; cdecl;
  function ToggleFlag(_id: Integer; path, name: PWideChar): WordBool; cdecl;
  function GetEnabledFlags(_id: Integer; path: PWideChar; out flags: PWideChar): WordBool; cdecl;

  // PUBLIC TESTING INTERFACE
  procedure BuildElementValueTests;

implementation

procedure BuildElementValueTests;
begin

end;

end.
