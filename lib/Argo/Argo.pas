unit Argo;

interface

uses
  SysUtils, Classes, Variants,
  ArgoTypes;

type
  TJSONValueType = (jtNull, jtString, jtBoolean, jtInt, jtDouble, jtArray, jtObject);
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
    _v: record
      case t: TJSONValueType of
        jtBoolean: (b: Boolean);
        jtInt:     (i: Int64);
        jtDouble:  (d: Double);
        jtObject:  (o: TJSONObject);
        jtArray:   (a: TJSONArray);
    end;
    _s: string;
    constructor Create(var P: PWideChar); overload;
    procedure FreeValue;
    procedure ParseNull(var P: PWideChar);
    procedure ParseString(var P: PWideChar);
    procedure ParseBoolean(var P: PWideChar);
    procedure ParseNumeric(var P: PWideChar);
    procedure ParseArray(var P: PWideChar);
    procedure ParseObject(var P: PWideChar);
    function GetJSONValueType: TJSONValueType;
  public
    constructor Create; overload;
    constructor Create(json: String); overload;
    destructor Destroy; override;
    procedure Nullify;
    procedure Put(value: String); overload;
    procedure Put(value: Boolean); overload;
    procedure Put(value: Int64); overload;
    procedure Put(value: Double); overload;
    procedure Put(value: TJSONArray); overload;
    procedure Put(value: TJSONObject); overload;
    function IsNull: Boolean;
    function AsArray: TJSONArray;
    function AsObject: TJSONObject;
    function AsString: String;
    function AsVariant: Variant;
    function ToString: string; override;
    property JSONValueType: TJSONValueType read GetJSONValueType;
  end;

  TJSONArray = class(TObject)
  private
    _Values: TList;
    constructor Create(var P: PWideChar); overload;
    function GetCount: Integer;
    function GetValue(index: Integer): TJSONValue;
    procedure SetValue(index: Integer; value: TJSONValue);
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
    property Values[index: Integer]: TJSONValue read GetValue write SetValue; default;
    property S[index: Integer]: String read GetS write SetS;
    property B[index: Integer]: Boolean read GetB write SetB;
    property I[index: Integer]: Int64 read GetI write SetI;
    property D[index: Integer]: Double read GetD write SetD;
    property O[index: Integer]: TJSONObject read GetO write SetO;
    property A[index: Integer]: TJSONArray read GetA write SetA;
    function AddValue(value: TJSONValue): Integer; overload;
    function Add(value: String): Integer; overload;
    function Add(value: Boolean): Integer; overload;
    function Add(value: Int64): Integer; overload;
    function Add(value: Double): Integer; overload;
    function Add(value: TJSONArray): Integer; overload;
    function Add(value: TJSONObject): Integer; overload;
  end;

  TJSONObject = class(TObject)
  private
    _Values: TList;
    {$IFDEF FAST_KEY_ACCESS}
    _Keys: TArgoTree;
    {$ENDIF}
    {$IFNDEF FAST_KEY_ACCESS}
    _Keys: TFastStringList;
    {$ENDIF}
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

uses
  Math;

{ === DESERIALIZATION === }

var
  LastToken: Boolean;

function HexDigit(c: WideChar): Byte;
begin
  if c <= '9' then  // parse 0-9
    Result := Byte(c) - Byte('0')
  else              // parse A-F, a-f
    Result := (Byte(c) and 7) + 9;
end;

function ParseUnicode(var P: PWideChar): WideChar;
var
  n: Integer;
  i: Integer;
begin
  n := 0;
  for i := 0 to 3 do begin
    Inc(P);
    Inc(n, Word(HexDigit(P^)) shl (4 * (3 - i)));
  end;
  Result := WideChar(n);
end;

function ParseHex(var P: PWideChar): WideChar;
var
  n: Integer;
  i: Integer;
begin
  n := 0;
  for i := 0 to 1 do begin
    Inc(P);
    Inc(n, Word(HexDigit(P^)) shl (4 * (1 - i)));
  end;
  Result := WideChar(n);
end;

function Unescape(c: WideChar; var P: PWideChar): string;
begin
  case c of
    #39,                            // single quote
    '/': Result := c;               // forward slash
    'b': Result := #8;              // backspace
    't': Result := #9;              // tab
    'n': Result := #10;             // line feed
    'f': Result := #12;             // form feed
    'r': Result := #13;             // carriage return
    'u': Result := ParseUnicode(P); // unicode escape sequence
    'x': Result := ParseHex(P);     // hexadecimal escape sequence
    else
      raise JSONException.Create(jxUnexpectedChar, P);
  end;
end;

function UnicodeEscape(var c: WideChar): string;
begin
  Result := '\u' + IntToHex(Ord(c), 4);
end;

function AsciiEscape(var c: WideChar): string;
begin
  case c of
    '"': Result := '\"';
    '\': Result := '\\';
    #8:  Result := '\b';
    #9:  Result := '\t';
    #10: Result := '\n';
    #12: Result := '\f';
    #13: Result := '\r';
    else
      Result := UnicodeEscape(c);
  end;
end;

function Escape(s: String): String;
var
  P: PWideChar;
  c: WideChar;
begin
  P := PWideChar(s);
  Result := '';
  while true do begin
    c := P^;
    Inc(P);
    if c = #0 then
      break
    else if CharInSet(c, [#1..#31, '"', '\', #127]) then
      Result := Result + AsciiEscape(c)
    else if c > #255 then
      Result := Result + UnicodeEscape(c)
    else
      Result := Result + c;
  end;
end;

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
      '\': begin // backslash
        escaped := not escaped;
        if not escaped then
          Result := Result + '\';
      end;
      '"': // quote
        if not escaped then
          break
        else begin
          escaped := false;
          Result := Result + '"';
        end;
      #0:
        raise JSONException.Create(jxTerminated, P);
      else begin
        if (c < #32) or (c = #127) then
          raise JSONException.Create(jxUnexpectedChar, P)
        else if escaped then begin
          escaped := false;
          Result := Result + Unescape(c, P);
        end
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
  len: Integer;
  context: WideString;
  pwBuffer: PWideChar;
begin
  len := Min(Length(pos) + 8, 17);
  SetLength(context, len);
  pwBuffer := PWideChar(context);
  StrLCopy(pwBuffer, pos - 8, len);
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
  {$IFDEF FAST_KEY_ACCESS}
  _Keys := TArgoTree.Create;
  {$ENDIF}
  {$IFNDEF FAST_KEY_ACCESS}
  _Keys := TFastStringList.Create;
  {$ENDIF}
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
    'n', 'N': ParseNull(P);
    else ParseNumeric(P);
  end;
end;

procedure TJSONValue.ParseNull(var P: PWideChar);
begin
  _v.t := jtNull;
  if StrLIComp(P, 'null', 4) = 0 then
    Inc(P, 4)
  else
    raise JSONException.Create(jxUnexpectedChar, P);
end;

procedure TJSONValue.ParseString(var P: PWidechar);
begin
  _v.t := jtString;
  _s := ParseJSONString(P);
end;

procedure TJSONValue.ParseBoolean(var P: PWidechar);
begin
  _v.t := jtBoolean;
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
  _v.t := jtInt;
  str := '';
  while true do begin
    c := P^;
    case c of
      #10, #13, ' ', ',','}',']': break;
      '.','e','E': begin
        _v.t := jtDouble;
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
  if _v.t = jtDouble then
    _v.d := StrToFloat(str)
  else
    _v.i := StrToInt64(str);
end;

procedure TJSONValue.ParseArray(var P: PWidechar);
begin
  _v.t := jtArray;
  _v.a := TJSONArray.Create(P);
end;

procedure TJSONValue.ParseObject(var P: PWidechar);
begin
  _v.t := jtObject;
  _v.o := TJSONObject.Create(P);
end;

procedure TJSONValue.Nullify;
begin
  FreeValue;
  _v.t := jtNull;
end;

procedure TJSONValue.Put(value: String);
begin
  FreeValue;
  _v.t := jtString;
  _s := value;
end;

procedure TJSONValue.Put(value: Boolean);
begin
  FreeValue;
  _v.t := jtBoolean;
  _v.b := value;
end;

procedure TJSONValue.Put(value: Int64);
begin
  FreeValue;
  _v.t := jtInt;
  _v.i := value;
end;

procedure TJSONValue.Put(value: Double);
begin
  FreeValue;
  _v.t := jtDouble;
  _v.d := value;
end;

procedure TJSONValue.Put(value: TJSONArray);
begin
  FreeValue;
  _v.t := jtArray;
  _v.a := value;
end;

procedure TJSONValue.Put(value: TJSONObject);
begin
  FreeValue;
  _v.t := jtObject;
  _v.o := value;
end;

function TJSONValue.IsNull: Boolean;
begin
  Result := _v.t = jtNull;
end;

function BoolToStr(b: Boolean): String;
begin
  if b then
    Result := 'true'
  else
    Result := 'false';
end;

function TJSONValue.AsArray: TJSONArray;
begin
  Result := _v.a;
end;

function TJSONValue.AsObject: TJSONObject;
begin
  Result := _v.o;
end;

function TJSONValue.AsString: String;
begin
  Result := _s;
end;

function TJSONValue.AsVariant: Variant;
begin
  case _v.t of
    jtNull: Result := VarEmpty;
    jtString: Result := _s;
    jtBoolean: Result := _v.b;
    jtInt: Result := _v.i;
    jtDouble: Result := _v.d;
    else
      raise Exception.Create('Arrays and objects cannot be treated as variants.');
  end;
end;

function TJSONValue.ToString: String;
begin
  case _v.t of
    jtNull: Result := 'null';
    jtString: Result := '"' + Escape(_s) + '"';
    jtBoolean: Result := BoolToStr(_v.b);
    jtInt: Result := IntToStr(_v.i);
    jtDouble: Result := FloatToStr(_v.d);
    jtArray: Result := _v.a.ToString;
    jtObject: Result := _v.o.ToString;
  end;
end;

function TJSONValue.GetJSONValueType: TJSONValueType;
begin
  Result := _v.t;
end;

{ === GENERAL === }

{ TJSONValue }
constructor TJSONValue.Create;
begin
  _v.t := jtNull;
end;

destructor TJSONValue.Destroy;
begin
  FreeValue;
  inherited;
end;

procedure TJSONValue.FreeValue;
begin
  case _v.t of
    jtArray: _v.a.Free;
    jtObject: _v.o.Free;
  end;
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
  Result := TJSONValue(_Values[index])
end;

procedure TJSONArray.SetValue(index: Integer; value: TJSONValue);
begin
  if not Assigned(value) then
    value := TJSONValue.Create;
  _Values[index] := value;
end;

function TJSONArray.GetS(index: Integer): String;
var
  value: TJSONValue;
begin
  Result := '';
  value := GetValue(index);
  if Assigned(value) and (value._v.t = jtString) then
    Result := value._s;
end;

function TJSONArray.GetB(index: Integer): Boolean;
var
  value: TJSONValue;
begin
  Result := false;
  value := GetValue(index);
  if Assigned(value) and (value._v.t = jtBoolean) then
    Result := value._v.b;
end;

function TJSONArray.GetI(index: Integer): Int64;
var
  value: TJSONValue;
begin
  Result := 0;
  value := GetValue(index);
  if Assigned(value) and (value._v.t = jtInt) then
    Result := value._v.i;
end;

function TJSONArray.GetD(index: Integer): Double;
var
  value: TJSONValue;
begin
  Result := 0.0;
  value := GetValue(index);
  if Assigned(value) and (value._v.t = jtDouble) then
    Result := value._v.d;
end;

function TJSONArray.GetA(index: Integer): TJSONArray;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(index);
  if Assigned(value) and (value._v.t = jtArray) then
    Result := value._v.a;
end;

function TJSONArray.GetO(index: Integer): TJSONObject;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(index);
  if Assigned(value) and (value._v.t = jtObject) then
    Result := value._v.o;
end;

procedure TJSONArray.SetS(index: Integer; value: String);
begin
  GetValue(index).Put(value);
end;

procedure TJSONArray.SetB(index: Integer; value: Boolean);
begin
  GetValue(index).Put(value);
end;

procedure TJSONArray.SetI(index: Integer; value: Int64);
begin
  GetValue(index).Put(value);
end;

procedure TJSONArray.SetD(index: Integer; value: Double);
begin
  GetValue(index).Put(value);
end;

procedure TJSONArray.SetA(index: Integer; value: TJSONArray);
begin
  GetValue(index).Put(value);
end;

procedure TJSONArray.SetO(index: Integer; value: TJSONObject);
begin
  GetValue(index).Put(value);
end;

function TJSONArray.AddValue(value: TJSONValue): Integer;
begin
  if not Assigned(value) then
    value := TJSONValue.Create;
  Result := _Values.Add(value)
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
  {$IFDEF FAST_KEY_ACCESS}
  _Keys := TArgoTree.Create;
  {$ENDIF}
  {$IFNDEF FAST_KEY_ACCESS}
  _Keys := TFastStringList.Create;
  {$ENDIF}
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
  Result := _Keys.Strings[index];
end;

function TJSONObject.GetValue(key: String): TJSONValue;
var
  i: Integer;
begin
  Result := nil;
  i := _Keys.IndexOf(key);
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
  i := _Keys.IndexOf(key);
  if not Assigned(value) then
    value := TJSONValue.Create;
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
  if Assigned(value) and (value._v.t = jtString) then
    Result := WideString(value._s);
end;

function TJSONObject.GetB(key: string): boolean;
var
  value: TJSONValue;
begin
  Result := false;
  value := GetValue(key);
  if Assigned(value) and (value._v.t = jtBoolean) then
    Result := value._v.b;
end;

function TJSONObject.GetI(key: string): Int64;
var
  value: TJSONValue;
begin
  Result := 0;
  value := GetValue(key);
  if Assigned(value) and (value._v.t = jtInt) then
    Result := value._v.i;
end;

function TJSONObject.GetD(key: string): Double;
var
  value: TJSONValue;
begin
  Result := 0.0;
  value := GetValue(key);
  if Assigned(value) and (value._v.t = jtDouble) then
    Result := value._v.d;
end;

function TJSONObject.GetA(key: string): TJSONArray;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(key);
  if Assigned(value) and (value._v.t = jtArray) then
    Result := value._v.a;
end;

function TJSONObject.GetO(key: string): TJSONObject;
var
  value: TJSONValue;
begin
  Result := nil;
  value := GetValue(key);
  if Assigned(value) and (value._v.t = jtObject) then
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
  Result := _Keys.Count;
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
  Result := _Keys.IndexOf(key) > -1;
end;

procedure TJSONObject.Delete(key: string);
var
  i: Integer;
begin
  {$IFDEF FAST_KEY_ACCESS}
  i := _Keys.Delete(key);
  {$ENDIF}
  {$IFNDEF FAST_KEY_ACCESS}
  i := _Keys.IndexOf(key);
  _Keys.Delete(i);
  {$ENDIF}
  if i > -1 then begin
    TJSONValue(_Values[i]).Free;
    _Values.Delete(i);
  end;
end;

function TJSONObject.ToString: String;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to Pred(_Keys.Count) do
    Result := Result + '"' + Escape(_Keys.Strings[i]) + '":' +
      TJSONValue(_Values[i]).ToString + ',';
  if _Keys.Count > 0 then
    SetLength(Result, Length(Result) - 1);
  Result := Result + '}';
end;

end.