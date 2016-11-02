unit xeGroups;

interface

  function HasGroup(_id: Cardinal; sig: string; _res: PWordBool): WordBool; StdCall;
  function AddGroup(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
  function GetGroups(_id: Cardinal; groups: PWideChar; len: Integer): WordBool; StdCall;
  function GetChildGroup(_id: Cardinal; _res: PCardinal): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta, xeSetup;


end.
