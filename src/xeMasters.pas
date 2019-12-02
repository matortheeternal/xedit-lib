unit xeMasters;

interface

uses
  wbInterface;

  {$region 'Native functions'}
  function NativeGetMasterNames(const _file: IwbFile): TDynStrings;
  procedure NativeAddRequiredMasters(const element: IwbElement; const targetFile: IwbFile; asNew: Boolean);
  function NativeFileHasMaster(const _file, _master: IwbFile): Boolean;
  {$endregion}

  {$region 'API functions'}
  function CleanMasters(_id: Cardinal): WordBool; cdecl;
  function SortMasters(_id: Cardinal): WordBool; cdecl;
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
  function AddMasters(_id: Cardinal; masters: PWideChar): WordBool; cdecl;
  function AddRequiredMasters(_id, _id2: Cardinal; asNew: WordBool): WordBool; cdecl;
  function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetMasterNames(_id: Cardinal; len: PInteger): WordBool; cdecl;
  {$endregion}

implementation

uses
  SysUtils, Classes,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeHelpers, xeMeta, xeFiles, xeMessages, xeSetup;

{$region 'Native functions'}
function NativeGetMasterNames(const _file: IwbFile): TDynStrings;
var
  masters, master: IwbContainer;
  i: Integer;
begin
  masters := _file.Header.ElementByPath['Master Files'] as IwbContainer;
  if not Assigned(masters) then exit;
  SetLength(Result, masters.ElementCount);
  for i := 0 to Pred(masters.ElementCount) do begin
    master := masters.Elements[i] as IwbContainer;
    Result[i] := master.ElementEditValues['MAST'];
  end;
end;

procedure NativeAddMaster(const targetFile: IwbFile; const masterName: String);
begin
  NativeFileByNameEx(string(masterName));
  targetFile.AddMasterIfMissing(string(masterName));
end;

procedure NativeAddRequiredMasters(const element: IwbElement; const targetFile: IwbFile; asNew: Boolean);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  try
    element.ReportRequiredMasters(sl, asNew);
    if sl.Find(targetFile.FileName, i) then
      sl.Delete(i);
    for i := 0 to Pred(sl.Count) do
      NativeAddMaster(targetFile, sl[i]);
  finally
    sl.Free;
  end;
end;

function NativeFileHasMaster(const _file, _master: IwbFile): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Pred(_file.MasterCount[False]) do
    if _file.Masters[i, False].FileName = _master.FileName then begin
      Result := True;
      break;
    end;
end;
{$endregion}

{$region 'API functions'}
function CleanMasters(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.CleanMasters;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function SortMasters(_id: Cardinal): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      _file.SortMasters;
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    NativeAddMaster(_file, string(masterName));
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddMasters(_id: Cardinal; masters: PWideChar): WordBool; cdecl;
var
  _file: IwbFile;
  sl: TStringList;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    sl := TStringList.Create;
    try
      sl.Text := string(masters);
      for i := 0 to Pred(sl.Count) do
        NativeAddMaster(_file, sl[i]);
    finally
      sl.Free;
    end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function AddRequiredMasters(_id, _id2: Cardinal; asNew: WordBool): WordBool; cdecl;
var
  element: IwbElement;
  _file: IwbFile;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbElement, element) then
      raise Exception.Create('First interface must be an element.');
    if not Supports(Resolve(_id2), IwbFile, _file) then
      raise Exception.Create('Second interface must be a file.');
    NativeAddRequiredMasters(element, _file, asNew);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

{$POINTERMATH ON}
function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if Supports(Resolve(_id), IwbFile, _file) then begin
      len^ := _file.MasterCount[False];
      SetLength(resultArray, len^);
      for i := 0 to Pred(_file.MasterCount[False]) do
        resultArray[i] := Store(_file.Masters[i, False]);
      Result := True;
    end;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file, f: IwbFile;
  i: Integer;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    len^ := 0;
    SetLength(resultArray, High(xFiles) + 1);
    for i := Low(xFiles) to High(xFiles) do begin
      f := xFiles[i];
      if NativeFileHasMaster(f, _file) then begin
        resultArray[len^] := Store(f);
        Inc(len^);
      end;
    end;
    SetLength(resultArray, len^);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$POINTERMATH OFF}

function GetMasterNames(_id: Cardinal; len: PInteger): WordBool; cdecl;
var
  _file: IwbFile;
  masterNames: TDynStrings;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbFile, _file) then
      raise Exception.Create('Interface must be a file.');
    masterNames := NativeGetMasterNames(_file);
    resultStr := StrArrayJoin(masterNames, #13#10);
    len^ := Length(resultStr);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

end.
