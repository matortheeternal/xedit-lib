unit Argo;

interface

uses
  SysUtils, Classes, Variants,
  ArgoTypes;

type
  TJSONValueType = (jtString, jtBoolean, jtInt, jtDouble, jtArray, jtObject);
  JSONExceptionType = (jxTerminated, jxUnexpectedChar, jxStartBracket,
    jxColonExpected, jxCommaExpected);

  JSONException = class(Exception)
  public
    constructor Create(exceptionType: JSONExceptionType; pos: PWideChar);
  end;

  TJSONObject = class;
  TJSONArray = class;

  TJSONValue = class(TObject)
  private
    _t: TJSONValueType;
    _v: record
      case _valueType: TJSONValueType of
        jtString:  (s: PWideChar);
        jtBoolean: (b: Boolean);
        jtInt:     (i: Int64);
        jtDouble:  (d: Double);
        jtObject:  (o: TJSONObject);
        jtArray:   (a: TJSONArray);
    end;
    constructor Create(var P: PWideChar); overload;
    procedure ParseObject(var P: PWideChar);
    procedure ParseArray(var P: PWideChar);
    procedure ParseString(var P: PWideChar);
    procedure ParseBoolean(var P: PWideChar);
    procedure ParseNumeric(var P: PWideChar);
    function GetJSONValueType: TJSONValueType;
  public
    constructor Create(json: String); overload;
    destructor Destroy; override;
    procedure Put(value: String); overload;
    procedure Put(value: Boolean); overload;
    procedure Put(value: Int64); overload;
    procedure Put(value: Double); overload;
    procedure Put(value: TJSONArray); overload;
    procedure Put(value: TJSONObject); overload;
    function ToString: string; override;
    property JSONValueType: TJSONValueType read GetJSONValueType;
  end;

  TJSONArray = class(TObject)
  private
    _Values: TList;
    constructor Create(var P: PWideChar); overload;
    function GetCount: Integer;
    function GetValue(index: Integer): TJSONValue;
    function MakeValue(index: Integer): TJSONValue;
    function GetS(index: Integer): String;
    function GetB(index: Integer): Boolean;
    function GetI(index: Integer): Int64;
    function GetD(index: Integer): Double;
    function GetA(index: Integer): TJSONArray;
    function GetO(index: Integer): TJSONObject;
    procedure SetS(index: Integer; value: String);
    procedure SetB(index: Integer; value: Boolean);
    procedure SetI(index: Integer; value: Int64);
    procedure SetD(index: Integer; value: Double);
    procedure SetA(index: Integer; value: TJSONArray);
    procedure SetO(index: Integer; value: TJSONObject);
  public
    constructor Create; overload;
    constructor Create(json: string); overload;
    destructor Destroy; override;
    procedure Delete(index: Integer);
    function ToString: string; override;
    property Count: Integer read GetCount;
    property Values[index: Integer]: TJSONValue read GetValue; default;
    property S[index: Integer]: String read GetS write SetS;
    property B[index: Integer]: Boolean read GetB write SetB;
    property I[index: Integer]: Int64 read GetI write SetI;
    property D[index: Integer]: Double read GetD write SetD;
    property O[index: Integer]: TJSONObject read GetO write SetO;
    property A[index: Integer]: TJSONArray read GetA write SetA;
    function Add(value: String): Integer; overload;
    function Add(value: Boolean): Integer; overload;
    function Add(value: Int64): Integer; overload;
    function Add(value: Double): Integer; overload;
    function Add(value: TJSONArray): Integer; overload;
    function Add(value: TJSONObject): Integer; overload;
    function Add(value: TJSONValue): Integer; overload;
  end;

  TJSONObject = class(TObject)
  private
    _Values: TList;
    _Keys: TArgoTree;
    constructor Create(var P: PWideChar) overload;
    procedure ParsePair(var P: PWideChar);
    procedure AddPair(key: string; value: TJSONValue);
    function GetKey(index: Integer): String;
    function GetValue(key: string): TJSONValue;
    function GetValueFromIndex(index: Integer): TJSONValue;
    procedure SetValue(key: String; value: TJSONValue);
    procedure SetValueFromIndex(index: Integer; value: TJSONValue);
    function MakeValue(key: string): TJSONValue;
    function GetCount: Integer;
    function GetS(key: string): String;
    function GetB(key: string): Boolean;
    function GetI(key: string): Int64;
    function GetD(key: string): Double;
    function GetA(key: string): TJSONArray;
    function GetO(key: string): TJSONObject;
    procedure SetS(key: string; value: String);
    procedure SetB(key: string; value: Boolean);
    procedure SetO(key: string; value: TJSONObject);
    procedure SetA(key: string; value: TJSONArray);
    procedure SetI(key: string; value: Int64);
    procedure SetD(key: string; value: Double);
  public
    constructor Create; overload;
    constructor Create(json: string); overload;
    destructor Destroy; override;
    function HasKey(key: string): Boolean;
    procedure Delete(key: string);
    function ToString: string; override;
    property Keys[index: Integer]: String read GetKey;
    property Values[index: string]: TJSONValue read GetValue write SetValue; default;
    property ValueFromIndex[index: Integer]: TJSONValue read GetValueFromIndex write SetValueFromIndex;
    property Count: Integer read GetCount;
    property S[index: string]: String read GetS write SetS;
    property B[index: string]: Boolean read GetB write SetB;
    property I[index: string]: Int64 read GetI write SetI;
    property D[index: string]: Double read GetD write SetD;
    property O[index: string]: TJSONObject read GetO write SetO;
    property A[index: string]: TJSONArray read GetA write SetA;
  end;


implementation

{ === HELPERS === }

function StringSize(str: string): Integer;
begin
  Result := ByteLength(str);
  if Result > 0 then
    Inc(Result, 2 * (SizeOf(word) + SizeOf(Longint)) + SizeOf(Char));
end;

function AllocString(str: string): PWideChar;
var
  size: Integer;
begin
  Result := PWideChar('');
  size := StringSize(str);
  if size = 0 then exit;
  GetMem(Result, size);
  StrLCopy(Result, PWideChar(str), size);
end;

{ === DESERIALIZATION === }

var
  LastToken: Boolean;

// function should be entered on the opening double quote for a JSON string.
function ParseJSONString(var P: PWideChar): String;
var
  escaped: Boolean;
  c: WideChar;
begin
  Result := '';
  escaped := false;
  while true do begin
    Inc(P);
    c := P^;
    case c of
      '\': // backslash
        escaped := not escaped;
      '"': // quote
        if not escaped then
          break
        else
          escaped := false;
      #0:
        raise JSONException.Create(jxTerminated, P);
      else begin
        if escaped then escaped := false;
        if ord(c) < 32 then
          raise JSONException.Create(jxUnexpectedChar, P)
        else
          Result := Result + c;
      end;
    end;
  end;
  Inc(P); // move past trailing quote
end;

// function should be entered between object or array members
function ParseSeparation(var P: PWideChar; separator: AnsiChar): Boolean;
begin
  Result := False;
  // iterate over whitespace and separators
  while CharInSet(P^, [#10, #13, ' ', separator]) do begin
    if P^ = WideChar(separator) then begin
      // duplicate separators raise an exception
      if Result then
        raise JSONException.Create(jxUnexpectedChar, P);
      Result := True;
    end;
    Inc(P);
  end;
end;

{ JSONException }
constructor JSONException.Create(exceptionType: JSONExceptionType; pos: PWideChar);
const
  JSONExceptionMessages: array[0..4] of string = (
    'Unexpected end of JSON near <%s>.',
    'Unexpected character in JSON near <%s>.',
    'Expected left brace to start object near <%s>.',
    'Expected colon separating key value near <%s>.',
    'Expected comma separating object/array members near <%s>.'
  );
var
  context: PWideChar;
begin
  GetMem(context, 34);
  StrLCopy(context, pos - 8, 17);
  self.Message := Format(JSONExceptionMessages[Ord(exceptionType)], [context]);
end;

{ TJSONObject Deserialization }
constructor TJSONObject.Create(json: string);
var
  P: PWideChar;
begin
  P := PWideChar(json);
  Create(P);
end;

constructor TJSONObject.Create(var P: PWideChar);
var
  c: WideChar;
begin
  _Values := TList.Create;
  _Keys := TArgoTree.Create;
  LastToken := False;
  if P^ <> '{' then
    raise JSONException.Create(jxStartBracket, P + 8);
  while true do begin
    Inc(P);
    c := P^;
    case c of
      #13, #10, ' ': continue; // whitespace characters
      '"':
        if LastToken then
          raise JSONException.Create(jxCommaExpected, P)
        else begin
          ParsePair(P);
          Dec(P); // reposition for next iteration
        end;
      '}': break;
      #0: raise JSONException.Create(jxTerminated, P);
      else raise JSONException.Create(jxUnexpectedChar, P);
    end;
  end;
  // reset token separation tracking
  LastToken := False;
  Inc(P);
end;

procedure TJSONObject.ParsePair(var P: PWideChar);
var
  key: string;
  value: TJSONValue;
begin
  key := ParseJSONString(P);
  if not ParseSeparation(P, ':') then
    raise JSONException.Create(jxColonExpected, P);
  value := TJSONValue.Create(P);
  AddPair(key, value);
  LastToken := not ParseSeparation(P, ',');
end;

{ TJSONArray Deserialization }
constructor TJSONArray.Create(json: String);
var
  P: PWideChar;
begin
  P := PWideChar(json);
  Create(P);
end;

constructor TJSONArray.Create(var P: PWideChar);
var
  c: WideChar;
begin
  _Values := TList.Create;
  LastToken := False;
  while true do begin
    Inc(P);
    c := P^;
    case c of
      #13, #10, ' ': continue; // whitespace characters
      ']': break;
      #0: raise JSONException.Create(jxTerminated, P);
      else begin
        if LastToken then
          raise JSONException.Create(jxCommaExpected, P)
        else
          _Values.Add(TJSONValue.Create(P));
        LastToken := not ParseSeparation(P, ',');
        Dec(P); // reposition for next iteration
      end;
    end;
  end;
  // reset token separation tracking
  LastToken := False;
  Inc(P);
end;

{ TJSONValue Deserialization }
constructor TJSONValue.Create(json: String);
var
  P: PWideChar;
begin
  P := PWideChar(json);
  Create(P);
end;

constructor TJSONValue.Create(var P: PWideChar);
begin
  case P^ of
    '{': ParseObject(P);
    '[': ParseArray(P);
    '"': ParseString(P);
    'f','t','F','T': ParseBoolean(P);
    else ParseNumeric(P);
  end;
end;

procedure TJSONValue.ParseString(var P: PWidechar);
begin
  _t := jtString;
  _v.s := AllocString(ParseJSONString(P));
end;

procedure TJSONValue.ParseBoolean(var P: PWidechar);
begin
  _t := jtBoolean;
  if StrLIComp(P, 'true', 4) = 0 then begin
    _v.b := true;
    Inc(P, 4);
  end
  else if StrLIComp(P, 'false', 5) = 0 then begin
    _v.b := false;
    Inc(P, 5);
  end
  else
    raise JSONException.Create(jxUnexpectedChar, P);
end;

procedure TJSONValue.ParseNumeric(var P: PWidechar);
var
  str: String;
  c: WideChar;
begin
  _t := jtInt;
  str := '';
  while true do begin
    c := P^;
    case c of
      #10, #13, ' ', ',','}',']': break;
      '.','e','E': begin
        _t := jtDouble;
        str := str + c;
      end;
      else begin
        if CharInSet(c, ['0'..'9','+','-']) then
          str := str + c
        else
          raise JSONException.Create(jxUnexpectedChar, P);
      end;
    end;
    Inc(P);
  end;
  if _t = jtDouble then
    _v.d := StrToFloat(str)
  else
    _v.i := StrToInt64(str);
end;

procedure TJSONValue.ParseArray(var P: PWidechar);
begin
  _t := jtArray;
  _v.a := TJSONArray.Create(P);
end;

procedure TJSONValue.ParseObject(var P: PWidechar);
begin
  _t := jtObject;
  _v.o := TJSONObject.Create(P);
end;

procedure TJSONValue.Put(value: String);
begin
  _t := jtString;
  _v.s := AllocString(value);
end;

procedure TJSONValue.Put(value: Boolean);
begin
  _t := jtBoolean;
  _v.b := value;
end;

procedure TJSONValue.Put(value: Int64);
begin
  _t := jtInt;
  _v.i := value;
end;

procedure TJSONValue.Put(value: Double);
begin
  _t := jtDouble;
  _v.d := value;
end;

procedure TJSONValue.Put(value: TJSONArray);
begin
  _t := jtArray;
  _v.a := value;
end;

procedure TJSONValue.Put(value: TJSONObject);
begin
  _t := jtObject;
  _v.o := value;
end;

function TJSONValue.ToString: String;
begin
  case _t of
    jtString: Result := '"' + _v.s + '"';
    jtBoolean: Result := BoolToStr(_v.b, true);
    jtInt: Result := IntToStr(_v.i);
    jtDouble: Result := FloatToStr(_v.d);
    jtArray: Result := _v.a.ToString;
    jtObject: Result := _v.o.ToString;
  end;
end;

function TJSONValue.GetJSONValueType: TJSONValueType;
begin
  Result := _t;
end;

{ === GENERAL === }

{ TJSONValue }
destructor TJSONValue.Destroy;
begin
  if _t = jtArray then _v.a.Free;
  if _t = jtObject then _v.o.Free;
  inherited;
end;

{ TJSONArray }
constructor TJSONArray.Create;
begin
  _Values := TList.Create;
end;

destructor TJSONArray.Destroy;
var
  i: Integer;
begin
  for i := 0 to Pred(_Values.Count) do
    TJSONValue(_Values[i]).Free;
  _Values.Free;
  inherited;
end;

function TJSONArray.GetCount: Integer;
begin
   Result := _Values.Count;
end;

function TJSONArray.GetValue(index: Integer): TJSONValue;
begin
  Result := nil;
  if index < _Values.Count then
    Result := TJSONValue(_Values[index]);
end;

function TJSONArray.MakeValue(index: Integer): TJSONValue;
begin
  Result := GetValue(index);
  if not Assigned(Result) then begin
    Result := TJSONValue.Create;
    _Values.Add(Result);
  end;
end;

function TJSONArray.GetS(index: Integer): String;
var
  value: TJSONValue;
begin
  Result := '';
  value := GetValue(index);
  if Assigned(value) and (value._t = jtString) then
    Result := value._v.s;
end;

function TJSONArray.GetB(index: Integer): Boolean;
var
  value: TJSONValue;
begin
  Result := false;
  value := GetValue(index);
  if Assigned(value) and (value._t = jtBoolean) then
    Result := value._v.b;
end;

function TJSONArray.GetI(index: Integer): Int64;
var
  value: TJSONValue;
begin
  Result := 0;
  value := GetValue(index);
  if Assigned(value) and (value._t = jtInt) then
    Result := value._v.i;
end;

function TJSONArray.GetD(index: Integer): Double;
var
  value: TJSONValue;
begin
  Result := 0.0;
  value := GetValue(index);
  if Assigned(value) and (value._t = jtDouble) then
    Result := value._v.d;
end;

function TJSONArray.GetA(index: Integer): TJSONArray;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(index);
  if Assigned(value) and (value._t = jtArray) then
    Result := value._v.a;
end;

function TJSONArray.GetO(index: Integer): TJSONObject;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(index);
  if Assigned(value) and (value._t = jtObject) then
    Result := value._v.o;
end;

procedure TJSONArray.SetS(index: Integer; value: String);
begin
  MakeValue(index).Put(value);
end;

procedure TJSONArray.SetB(index: Integer; value: Boolean);
begin
  MakeValue(index).Put(value);
end;

procedure TJSONArray.SetI(index: Integer; value: Int64);
begin
  MakeValue(index).Put(value);
end;

procedure TJSONArray.SetD(index: Integer; value: Double);
begin
  MakeValue(index).Put(value);
end;

procedure TJSONArray.SetA(index: Integer; value: TJSONArray);
begin
  MakeValue(index).Put(value);
end;

procedure TJSONArray.SetO(index: Integer; value: TJSONObject);
begin
  MakeValue(index).Put(value);
end;

function TJSONArray.Add(value: String): Integer;
begin
  Result := _Values.Add(TJSONValue.Create);
  TJSONValue(_Values[Result]).Put(value);
end;

function TJSONArray.Add(value: Boolean): Integer;
begin
  Result := _Values.Add(TJSONValue.Create);
  TJSONValue(_Values[Result]).Put(value);
end;

function TJSONArray.Add(value: Int64): Integer;
begin
  Result := _Values.Add(TJSONValue.Create);
  TJSONValue(_Values[Result]).Put(value);
end;

function TJSONArray.Add(value: Double): Integer;
begin
  Result := _Values.Add(TJSONValue.Create);
  TJSONValue(_Values[Result]).Put(value);
end;

function TJSONArray.Add(value: TJSONArray): Integer;
begin
  Result := _Values.Add(TJSONValue.Create);
  TJSONValue(_Values[Result]).Put(value);
end;

function TJSONArray.Add(value: TJSONObject): Integer;
begin
  Result := _Values.Add(TJSONValue.Create);
  TJSONValue(_Values[Result]).Put(value);
end;

function TJSONArray.Add(value: TJSONValue): Integer;
begin
  Result := _Values.Add(value);
end;

procedure TJSONArray.Delete(index: Integer);
begin
  if index < _Values.Count then
    _Values.Delete(index);
end;

function TJSONArray.ToString: string;
var
  i: Integer;
begin
  Result := '[';
  for i := 0 to Pred(_Values.Count) do
    Result := Result + TJSONValue(_Values[i]).ToString + ',';
  if _Values.Count > 0 then
    SetLength(Result, Length(Result) - 1);
  Result := Result + ']';
end;

{ TJSONObject }
constructor TJSONObject.Create;
begin
  _Values := TList.Create;
  _Keys := TArgoTree.Create;
end;

destructor TJSONObject.Destroy;
var
  i: Integer;
begin
  for i := 0 to Pred(_Values.Count) do
    TJSONValue(_Values[i]).Free;
  _Values.Free;
  _Keys.Free;
  inherited;
end;

function TJSONObject.GetKey(index: Integer): String;
begin
  Result := _Keys.Names[index];
end;

function TJSONObject.GetValue(key: String): TJSONValue;
var
  i: Integer;
begin
  Result := nil;
  i := _Keys[key];
  if i > -1 then
    Result := TJSONValue(_Values[i]);
end;

function TJSONObject.GetValueFromIndex(index: Integer): TJSONValue;
begin
  Result := TJSONValue(_Values[index]);
end;

procedure TJSONObject.SetValue(key: String; value: TJSONValue);
var
  i: Integer;
begin
  i := _Keys[key];
  if i > -1 then
    _Values[i] := value
  else
    AddPair(key, value);
end;

procedure TJSONObject.SetValueFromIndex(index: Integer; value: TJSONValue);
begin
  _Values[index] := value;
end;

function TJSONObject.GetS(key: string): string;
var
  value: TJSONValue;
begin
  Result := '';
  value := GetValue(key);
  if Assigned(value) and (value._t = jtString) then
    Result := WideString(value._v.s);
end;

function TJSONObject.GetB(key: string): boolean;
var
  value: TJSONValue;
begin
  Result := false;
  value := GetValue(key);
  if Assigned(value) and (value._t = jtBoolean) then
    Result := value._v.b;
end;

function TJSONObject.GetI(key: string): Int64;
var
  value: TJSONValue;
begin
  Result := 0;
  value := GetValue(key);
  if Assigned(value) and (value._t = jtInt) then
    Result := value._v.i;
end;

function TJSONObject.GetD(key: string): Double;
var
  value: TJSONValue;
begin
  Result := 0.0;
  value := GetValue(key);
  if Assigned(value) and (value._t = jtDouble) then
    Result := value._v.d;
end;

function TJSONObject.GetA(key: string): TJSONArray;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(key);
  if Assigned(value) and (value._t = jtArray) then
    Result := value._v.a;
end;

function TJSONObject.GetO(key: string): TJSONObject;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(key);
  if Assigned(value) and (value._t = jtObject) then
    Result := value._v.o;
end;

procedure TJSONObject.AddPair(key: string; value: TJSONValue);
begin
  _Keys.Add(key);
  _Values.Add(value);
end;

function TJSONObject.MakeValue(key: string): TJSONValue;
begin
  Result := GetValue(key);
  if not Assigned(Result) then begin
    Result := TJSONValue.Create;
    AddPair(key, Result);
  end;
end;

function TJSONObject.GetCount: Integer;
begin
  Result := _Keys.Size;
end;

procedure TJSONObject.SetS(key: string; value: string);
begin
  MakeValue(key).Put(value);
end;

procedure TJSONObject.SetB(key: string; value: boolean);
begin
  MakeValue(key).Put(value);
end;

procedure TJSONObject.SetI(key: string; value: Int64);
begin
  MakeValue(key).Put(value);
end;

procedure TJSONObject.SetD(key: string; value: Double);
begin
  MakeValue(key).Put(value);
end;

procedure TJSONObject.SetA(key: string; value: TJSONArray);
begin
  MakeValue(key).Put(value);
end;

procedure TJSONObject.SetO(key: string; value: TJSONObject);
begin
  MakeValue(key).Put(value);
end;

function TJSONObject.HasKey(key: string): Boolean;
begin
  Result := _Keys[key] > -1;
end;

procedure TJSONObject.Delete(key: string);
var
  i: Integer;
begin
  i := _Keys.Delete(key);
  if i > -1 then
    _Values.Delete(i);
end;

function TJSONObject.ToString: String;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to Pred(_Keys.Size) do
    Result := Result + '"' + _Keys.Names[i] + '":' + TJSONValue(_Values[i]).ToString + ',';
  if _Keys.Size > 0 then
    SetLength(Result, Length(Result) - 1);
  Result := Result + '}';
end;

end.