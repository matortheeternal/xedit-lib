unit xeFileValues;

interface

  function GetFileHeader(_id, _res: Cardinal): WordBool; StdCall;
  function GetNextObjectId(_id, nextObjectID: Cardinal): WordBool; StdCall;
  function SetNextObjectID(_id, nextObjectID: Cardinal): WordBool; StdCall;
  function GetAuthor(_id: Cardinal; author: PWideChar; len: Integer): WordBool; StdCall;
implementation

uses
  Classes, SysUtils, Variants,
  // mte modules
  mteHelpers,
  // xedit modules
  wbInterface, wbImplementation,
  // xelib modules
  xeMessages, xeMeta;


function GetFileHeader(_id, _res: Cardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _res := Store(_file.Header);
      Result := true;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetNextObjectId(_id, nextObjectID: Cardinal): WordBool; StdCall;
var
  _file: IwbFile;
begin
  Result := false;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      nextObjectID := _file.Header.ElementNativeValues['HEDR\Next Object ID'];
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

end.
