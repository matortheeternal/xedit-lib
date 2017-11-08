unit xeFilter;

interface

uses
  wbInterface,
  xeMeta;

  {$region 'API functions'}
  function FilterRecord(_id: Cardinal): WordBool; cdecl;
  function ResetFilter: WordBool; cdecl;
  {$endregion}


implementation

uses
  xeElements, xeSetup, xeMessages, SysUtils;

{$region 'Native functions'}
procedure SetChildrenHidden(const container: IwbContainer);
var
  i: Integer;
  innerContainer: IwbContainer;
  rec: IwbMainrecord;
begin
  for i := 0 to Pred(container.ElementCount) do
    if Supports(container.Elements[i], IwbContainer, innerContainer)
    and (esFilterShow in innerContainer.ElementStates) then begin
      innerContainer.Filter(false);
      if Supports(innerContainer, IwbMainRecord, rec) then begin
        if Supports(rec.ChildGroup, IwbContainer, InnerContainer) then
          SetChildrenHidden(innerContainer);
      end
      else
        SetChildrenHidden(innerContainer);
    end;
end;

procedure SetParentsVisible(const container: IwbContainer);
var
  group: IwbGroupRecord;
  parentContainer: IwbContainer;
begin
  if Supports(container, IwbGroupRecord, group) and IsChildGroup(group) then
    parentContainer := group.ChildrenOf as IwbContainer
  else
    parentContainer := container.Container;
  if Assigned(parentContainer)
  and not (esFilterShow in parentContainer.elementStates) then begin
    parentContainer.Filter(true);
    SetParentsVisible(parentContainer);
  end;
end;
{$endregion}

{$region 'API functions'}
function FilterRecord(_id: Cardinal): WordBool; cdecl;
var
  rec: IwbMainRecord;
begin
  Result := False;
  try
    if not Supports(Resolve(_id), IwbMainRecord, rec) then
      raise Exception.Create('Interface must be a main record.');
    rec.Filter(true);
    SetParentsVisible(rec as IwbContainer);
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;

function ResetFilter: WordBool; cdecl;
var
  i: Integer;
  container: IwbContainer;
begin
  Result := False;
  try
    for i := Low(xFiles) to High(xFiles) do
      if Supports(xFiles[i], IwbContainer, container)
      and (esFilterShow in container.ElementStates) then begin
        container.Filter(false);
        SetChildrenHidden(container);
      end;
    Result := True;
  except
    on x: Exception do ExceptionHandler(x);
  end;
end;
{$endregion}

end.
