unit xeRecords;

interface

  function AddRecord(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
  //function GetRecords(_id: Cardinal; _res: PCardinalArray): WordBool; StdCall;
  function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; StdCall;
  //function RecordSignatureFromName(name, str: PWideChar): WordBool; StdCall;

implementation

uses
  Classes, SysUtils,
  // xedit units
  wbInterface, wbImplementation,
  // xelib units
  xeGroups, xeMeta;

function AddRecord(_id: Cardinal; sig: string; _res: PCardinal): WordBool; StdCall;
var
  _file: IwbFile;
  group: IwbGroupRecord;
  element: IwbElement;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      group := AddGroupIfMissing(_file, sig);
      element := group.Add(sig);
      StoreIfAssigned(IInterface(element), _res, Result);
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function RecordByIndex(_id: Cardinal; index: Integer; _res: PCardinal): WordBool; StdCall;
var
  e: IInterface;
  _file: IwbFile;
  _group: IwbGroupRecord;
begin
  Result := false;
  try
    e := Resolve(_id);
    if Supports(e, IwbFile, _file) then begin
      _res^ := Store(_file.Records[index]);
      Result := true;
    end
    else if Supports(e, IwbGroupRecord, _group) then begin
      _res^ := Store(_group.Elements[index]);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

end.
