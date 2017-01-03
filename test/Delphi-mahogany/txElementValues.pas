unit txElementValues;

interface

  // ELEMENT VALUE METHODS
  function Name(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Path(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function EditorID(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function Signature(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function ShortName(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SortKey(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function ElementType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function DefType(_id: Integer; str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetValue(_id: Integer; path, str: PWideChar; len: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetValue(_id: Integer; path, value: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetIntValue(_id: Integer; path: PWideChar; out value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function SetIntValue(_id: Integer; path: PWideChar; value: Integer): WordBool; cdecl; external 'XEditLib.dll';
  function GetUIntValue(_id: Integer; path: PWideChar; out value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetUIntValue(_id: Integer; path: PWideChar; value: Cardinal): WordBool; cdecl; external 'XEditLib.dll';
  function GetFloatValue(_id: Integer; path: PWideChar; out value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function SetFloatValue(_id: Integer; path: PWideChar; value: Double): WordBool; cdecl; external 'XEditLib.dll';
  function GetLinksTo(_id: Integer; path: PWideChar; _res: PCardinal): WordBool; cdecl; external 'XEditLib.dll';
  function SetFlag(_id: Integer; path, name: PWideChar; enabled: WordBool): WordBool; cdecl; external 'XEditLib.dll';
  function GetFlag(_id: Integer; path, name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function ToggleFlag(_id: Integer; path, name: PWideChar): WordBool; cdecl; external 'XEditLib.dll';
  function GetEnabledFlags(_id: Integer; path: PWideChar; out flags: PWideChar): WordBool; cdecl; external 'XEditLib.dll';

  // PUBLIC TESTING INTERFACE
  procedure BuildElementValueTests;

implementation

procedure BuildElementValueTests;
begin

end;

end.
