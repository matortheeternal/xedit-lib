unit xeFileValues;

interface

  {$region 'API functions'}
  function GetFileHeader(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
  function GetNextObjectId(_id: Cardinal; nextObjectID: PCardinal): WordBool; cdecl;
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; cdecl;
  function GetFileName(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetAuthor(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; cdecl;
  function GetDescription(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; cdecl;
  function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
  function GetIsESM(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
  function SetIsESM(_id: Cardinal; bool: WordBool): WordBool; cdecl;
  {$endregion}

implementation

uses
  Classes, SysUtils, Variants,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta;

{$region 'API functions'}
function GetFileHeader(_id: Cardinal; _res: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res^ := Store(_file.Header);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetNextObjectId(_id: Cardinal; nextObjectID: PCardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      nextObjectID^ := _file.Header.ElementNativeValues['HEDR\Next Object ID'];
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetNextObjectId(_id, nextObjectID: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.Header.ElementNativeValues['HEDR\Next Object ID'] := nextObjectID;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFileName(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      resultStr := _file.FileName;
      len^ := Length(resultStr);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetAuthor(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      resultStr := _file.Header.ElementEditValues['CNAM'];
      len^ := Length(resultStr);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.Header.ElementEditValues['CNAM'] := string(author);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetDescription(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      resultStr := _file.Header.ElementEditValues['SNAM'];
      len^ := Length(resultStr);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.Header.ElementEditValues['SNAM'] := string(desc);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
  i, n: Integer;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      n := 0;
      for i := 0 to Pred(_file.RecordCount) do
        if not _file.Records[i].IsMaster then n := n + 1;
      count^ := n;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetIsESM(_id: Cardinal; bool: PWordBool): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      bool^ := _file.IsESM;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetIsESM(_id: Cardinal; bool: WordBool): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.IsESM := bool;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}


end.
