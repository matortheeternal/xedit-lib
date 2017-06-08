unit xeTypes;

interface

uses
  Classes;

type
  TFastStringList = class(TStringList)
  protected
    function CompareStrings(const S1, S2: String): Integer; override;
  public
    function IndexOfValue(const Value: String): Integer;
  end;


implementation

uses
  SysUtils;

{ TFastStringList }
function TFastStringList.CompareStrings(const S1, S2: string): Integer;
begin
  Result := CompareStr(S1, S2);
end;

function TFastStringList.IndexOfValue(const Value: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, P + 1, Length(S)), Value) = 0) then Exit;
  end;
  Result := -1;
end;

end.
