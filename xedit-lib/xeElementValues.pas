unit xeElementValues;

interface

  function Name(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
  function EditorID(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
  function Signature(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
  function ShortName(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
  function SortKey(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta;


function Name(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
var
  element: IwbElement;
begin
  Result := false;
  if Supports(Resolve(_id), IwbElement, element) then begin
    StrLCopy(str, PAnsiChar(AnsiString(element.Name)), len);
    Result := true;
  end;
end;

function EditorID(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
var
  rec: IwbMainRecord;
begin
  Result := false;
  if Supports(Resolve(_id), IwbMainRecord, rec) then begin
    StrLCopy(str, PAnsiChar(AnsiString(rec.EditorID)), len);
    Result := true;
  end;
end;

function Signature(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
var
  rec: IwbRecord;
begin
  Result := false;
  if Supports(Resolve(_id), IwbRecord, rec) then begin
    StrLCopy(str, PAnsiChar(AnsiString(rec.Signature)), len);
    Result := true;
  end;
end;

function ShortName(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
var
  element: IwbElement;
begin
  Result := false;
  if Supports(Resolve(_id), IwbElement, element) then begin
    StrLCopy(str, PAnsiChar(AnsiString(element.ShortName)), len);
    Result := true;
  end;
end;

function SortKey(_id: Integer; str: PAnsiChar; len: Integer): WordBool; StdCall;
var
  element: IwbElement;
begin
  Result := false;
  if Supports(Resolve(_id), IwbElement, element) then begin
    StrLCopy(str, PAnsiChar(AnsiString(element.SortKey[false])), len);
    Result := true;
  end;
end;


end.
