unit xeMasters;

interface

uses
  wbInterface;

  {$region 'Native functions'}
  procedure NativeAddRequiredMasters(element: IwbElement; targetFile: IwbFile; asNew: Boolean);
  function NativeFileHasMaster(_file, _master: IwbFile): Boolean;
  {$endregion}

  {$region 'API functions'}
  function CleanMasters(_id: Cardinal): WordBool; cdecl;
  function SortMasters(_id: Cardinal): WordBool; cdecl;
  function AddMaster(_id: Cardinal; masterName: PWideChar): WordBool; cdecl;
  function AddMasters(_id: Cardinal; masters: PWideChar): WordBool; cdecl;
  function GetMasters(_id: Cardinal; len: PInteger): WordBool; cdecl;
  function GetRequiredBy(_id: Cardinal; len: PInteger): WordBool; cdecl;
  {$endregion}

implementation

uses
  SysUtils, Classes,
  // xedit modules
  wbImplementation,
  // xelib modules
  xeMeta, xeFiles, xeMessages, xeSetup;

{$region 'Native functions'}
procedure NativeAddMasters(targetFile: IwbFile; var masters: TStringList);
var
  i: Integer;
begin
  for i := 0 to Pred(masters.Count) do
    if IwbFile(Pointer(masters.Objects[i])).LoadOrder >= targetFile.LoadOrder then
      raise Exception.Create(Format('The required master "%s" cannot be ' +
        'added to "%s" because it has a higher load order.', [masters[i],
        targetFile.FileName]));
  masters.Sorted := False;
  masters.CustomSort(CompareLoadOrder);
  targetFile.AddMasters(masters);
end;

procedure GetMissingMasters(targetFile: IwbFile; var masters: TStringList);
var
  i, j: Integer;
begin
  for i := 0 to Pred(targetFile.MasterCount) do
    if masters.Find(targetFile.Masters[i].FileName, j) then
      masters.Delete(j);
  if masters.Find(targetFile.FileName, j) then
    masters.Delete(j);
end;

procedure NativeAddRequiredMasters(element: IwbElement; targetFile: IwbFile; asNew: Boolean);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Sorted := True;
  sl.Duplicates := dupIgnore;
  try
    element.ReportRequiredMasters(sl, asNew);
    GetMissingMasters(targetFile, sl);
    if sl.Count > 0 then
      NativeAddMasters(targetFile, sl);
  finally
    sl.Free;
  end;
end;

function NativeFileHasMaster(_file, _master: IwbFile): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Pred(_file.MasterCount) do
    if _file.Masters[i].FileName = _master.FileName then begin
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
    _file.AddMasterIfMissing(string(masterName));
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
        sl.Objects[i] := Pointer(NativeFileByNameEx(sl[i]));
      NativeAddMasters(_file, sl);
    finally
      sl.Free;
    end;
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
      len^ := _file.MasterCount;
      SetLength(resultArray, len^);
      for i := 0 to Pred(_file.MasterCount) do
        resultArray[i] := Store(_file.Masters[i]);
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
{$endregion}

end.
