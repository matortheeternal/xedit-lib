unit xeFileValues;

interface

  function GetFileHeader(_id: Cardinal; _res: PCardinal): WordBool; StdCall;
  function GetNextObjectId(_id: Cardinal; nextObjectID: PCardinal): WordBool; StdCall;
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; StdCall;
  function GetFileName(_id: Cardinal; fileName: PWideChar; len: Integer): WordBool; StdCall;
  function GetAuthor(_id: Cardinal; author: PWideChar; len: Integer): WordBool; StdCall;
  function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; StdCall;
  function GetDescription(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; StdCall;
  function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; StdCall;
  function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; StdCall;
  function GetIsESM(_id: Cardinal; isESM: PWordBool): WordBool; StdCall;
  function SetIsESM(_id: Cardinal; isESM: WordBool): WordBool; StdCall;

implementation

uses
  Classes, SysUtils, Variants,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta;


function GetFileHeader(_id: Cardinal; _res: PCardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res^ := Store(_file.Header);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetNextObjectId(_id: Cardinal; nextObjectID: PCardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      nextObjectID^ := _file.Header.ElementNativeValues['HEDR\Next Object ID'];
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetNextObjectId(_id, nextObjectID: Cardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.Header.ElementNativeValues['HEDR\Next Object ID'] := nextObjectID;
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetFileName(_id: Cardinal; fileName: PWideChar; len: Integer): WordBool; StdCall;
var
  _file: IwbFile;
  s: String;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      s := _file.FileName;
      StrLCopy(fileName, PWideChar(WideString(s)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetAuthor(_id: Cardinal; author: PWideChar; len: Integer): WordBool; StdCall;
var
  _file: IwbFile;
  s: String;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      s := _file.Header.ElementEditValues['CNAM'];
      StrLCopy(author, PWideChar(WideString(s)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetAuthor(_id: Cardinal; author: PWideChar): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.Header.ElementEditValues['CNAM'] := string(author);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetDescription(_id: Cardinal; desc: PWideChar; len: Integer): WordBool; StdCall;
var
  _file: IwbFile;
  s: String;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      s := _file.Header.ElementEditValues['SNAM'];
      StrLCopy(desc, PWideChar(WideString(s)), len);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetDescription(_id: Cardinal; desc: PWideChar): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.Header.ElementEditValues['SNAM'] := string(desc);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function OverrideRecordCount(_id: Cardinal; count: PInteger): WordBool; StdCall;
var
  _file: IwbFile;
  i, n: Integer;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      n := 0;
      for i := 0 to _file.RecordCount do
        if not _file.Records[i].IsMaster then n := n + 1;
      count^ := n;
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetIsESM(_id: Cardinal; isESM: PWordBool): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      isESM^ := _file.IsESM;
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SetIsESM(_id: Cardinal; isESM: WordBool): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.IsESM := isESM;
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;


end.
