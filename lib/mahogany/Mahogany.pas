unit Mahogany;

interface

uses
  SysUtils, Classes, Variants;

type
  TProc = reference to procedure;
  TMessageProc = reference to procedure(msg: String);

  TTest = class(TObject)
  public
    context: TTest;
    description: String;
    depth: Integer;
    passed: Boolean;
    failAll: Boolean;
    procedure Execute;
    procedure Fail(x: Exception); overload;
    procedure Fail(msg: String); overload;
  end;

  TSuite = class(TTest)
    children: TList;
    beforeEach: TProc;
    afterEach: TProc;
    beforeAll: TProc;
    afterAll: TProc;
    constructor Create(description: String; callback: TProc);
    procedure Execute;
    function GetSpecsCount: Integer;
    function GetTestsCount: Integer;
    function GetFailsCount: Integer;
    procedure AddChild(child: TTest);
    destructor Destroy; override;
  end;

  TSpec = class(TTest)
  public
    callback: TProc;
    constructor Create(description: String; callback: TProc; failAll: Boolean);
    procedure Execute;
  end;

  TFailure = class(TObject)
  public
    context: TTest;
    exception: Exception;

    constructor Create(context: TTest; exception: Exception);
    procedure MarkContextFailed;
  end;

  // PUBLIC API
  procedure TestComment(comment: String);
  procedure Describe(description: String; callback: TProc);
  procedure BeforeAll(callback: TProc);
  procedure AfterAll(callback: TProc);
  procedure BeforeEach(callback: TProc);
  procedure AfterEach(callback: TProc);
  procedure It(description: String; callback: TProc; failAll: boolean = false);
  procedure Expect(expectation: Boolean; description: String = '');
  procedure ExpectEqual(v1, v2: Variant; description: String = '');
  procedure ExpectException(proc: TProc; msg: String = '');
  procedure RunTests(messageProc: TMessageProc);
  procedure ReportResults(messageProc: TMessageProc);

  // PRIVATE
  procedure CannotCallException(target, context: String);
  procedure BuildFailedException(target, description: String; x: Exception);
  function GetSpecsCount: Integer;

var
  MainSuites: TList;
  Failures: TList;
  ActiveSuite: TSuite;
  LogMessage: TMessageProc;

implementation


{******************************************************************************}
{ Public API
  These functions provide the public API for Mahogany.
}
{******************************************************************************}

{ Utility Functions }
procedure TestComment(comment: String);
var
  spacing: string;
begin
  spacing := StringOfChar(' ', (ActiveSuite.depth + 2) * 2);
  LogMessage(spacing + comment);
end;

{ Suite Functions }
procedure Describe(description: String; callback: TProc);
begin
  try
    TSuite.Create(description, callback);
  except
    on x: Exception do
      BuildFailedException('test suite', description, x);
  end;
end;

{ Setup Functions }
procedure BeforeAll(callback: TProc);
begin
  if (ActiveSuite = nil) then
    CannotCallException('BeforeAll', 'Describe');
  ActiveSuite.beforeAll := callback;
end;

procedure AfterAll(callback: TProc);
begin
  if (ActiveSuite = nil) then
    CannotCallException('AfterAll', 'Describe');
  ActiveSuite.afterAll := callback;
end;

procedure BeforeEach(callback: TProc);
begin
  if (ActiveSuite = nil) then
    CannotCallException('BeforeEach', 'Describe');
  ActiveSuite.beforeEach := callback;
end;

procedure AfterEach(callback: TProc);
begin
  if (ActiveSuite = nil) then
    CannotCallException('AfterEach', 'Describe');
  ActiveSuite.afterEach := callback;
end;

{ Spec Functions }
procedure It(description: String; callback: TProc; failAll: boolean = false);
begin
  if (ActiveSuite = nil) then
    CannotCallException('It', 'Describe');
  try
    TSpec.Create(description, callback, failAll);
  except
    on x: Exception do
      BuildFailedException('spec', description, x);
  end;
end;

{ Expectations }
procedure Expect(expectation: Boolean; description: String = '');
begin
  if not expectation then
    raise Exception.Create(description);
end;

procedure ExpectEqual(v1, v2: Variant; description: String = '');
const
  ErrorFormat = 'Expected "%s", found "%s"';
begin
  if v1 <> v2 then
    raise Exception.Create(Format(ErrorFormat, [VarToStr(v2), VarToStr(v1)]));
end;

procedure ExpectException(proc: TProc; msg: String = '');
const
  NoExceptionError = 'Expected exception "%s", but no exception was rasied';
  AnyExceptionError = 'Expected an exception, but no exception was raised';
  WrongExceptionError = 'Expected exception "%s", but "%s" was raised';
var
  exceptionRaised: Boolean;
begin
  exceptionRaised := false;
  try
    proc();
  except
    on x: Exception do begin
      exceptionRaised := true;
      // fail if the proc raised the wrong exception
      if (msg <> '') and (msg <> x.Message) then
        raise Exception.Create(Format(WrongExceptionError, [msg, x.Message]));
    end;
  end;
  // fail if the proc didn't raise an exception
  if not exceptionRaised then begin
    if msg <> '' then
      raise Exception.Create(Format(NoExceptionError, [msg]))
    else
      raise Exception.Create(AnyExceptionError);
  end;
end;

{ Test Runner }
procedure RunTests(messageProc: TMessageProc);
var
  i: Integer;
  suite: TSuite;
begin
  LogMessage := messageProc;
  for i := 0 to Pred(MainSuites.Count) do begin
    suite := TSuite(MainSuites[i]);
    suite.Execute;
  end;
end;

procedure ReportResults(messageProc: TMessageProc);
const
  ReportMessage = '%d specs, %d failures';
begin
  messageProc(Format(ReportMessage, [GetSpecsCount, Failures.Count]));
end;


{******************************************************************************}
{ Private
  These functions are private to Mahogany.  You should not call them from
  your code.
}
{******************************************************************************}

{ Error Handling }
procedure CannotCallException(target, context: String);
const
  CannotCallError = 'You cannot call "%s" outside of a "%s".';
begin
  raise Exception.Create(Format(CannotCallError, [target, context]));
end;

procedure BuildFailedException(target, description: String; x: Exception);
const
  BuildFailedError = 'Failed to build %s "%s": %s';
begin
  x.Message := Format(BuildFailedError, [target, description, x.Message]);
  raise x;
end;

{ Helper Functions }
function GetSpecsCount: Integer;
var
  i: Integer;
  suite: TSuite;
begin
  Result := 0;
  for i := 0 to Pred(MainSuites.Count) do begin
    suite := TSuite(MainSuites[i]);
    Inc(Result, suite.GetSpecsCount);
  end;
end;

{ TTest }
procedure TTest.Execute;
begin
  if Assigned(LogMessage) then
    LogMessage(StringOfChar(' ', depth * 2) + description);
end;

procedure TTest.Fail(x: Exception);
begin
  TFailure.Create(self, x);
end;

procedure TTest.Fail(msg: String);
begin
  TFailure.Create(self, Exception.Create(msg));
end;

{ TSuite }
constructor TSuite.Create(description: String; callback: TProc);
begin
  // default to a to-level suite
  context := nil;
  depth := 0;  

  // suites can be nested inside of each other
  if Assigned(ActiveSuite) then begin
    context := TTest(ActiveSuite);
    depth := ActiveSuite.depth + 1;
    ActiveSuite.AddChild(TTest(self));
  end; 
  
  // set input properties
  self.description := description;
  children := TList.Create;
  // the suite passes if it has no specs
  passed := true;

  // build nested specs/suites
  try
    // the suite is active in the context of its callback
    ActiveSuite := self;
    callback();
    // the suite is a main suite if it doesn't have a parent context
    if (context = nil) then
      MainSuites.Add(self);
  finally
    // the suite should no longer be active once its callback completes
    ActiveSuite := TSuite(context);
  end;
end;

procedure TSuite.Execute;
var
  i: Integer;
  test: TTest;
begin
  try
    inherited;
    ActiveSuite := self;
    // Setup before all tests if given
    if Assigned(BeforeAll) then BeforeAll;

    // execute each test in the suite
    try
      for i := 0 to Pred(children.Count) do begin
        // Setup before each test if given
        if Assigned(BeforeEach) then BeforeEach;
        // execute the test
        try
          test := TTest(children[i]);
          if test is TSuite then
            TSuite(test).Execute
          else if test is TSpec then
            TSpec(test).Execute;
        finally
          // Tear down after each test if given
          if Assigned(AfterEach) then AfterEach;
        end;
      end;
    finally
      // Tear down after all tests if given
      if Assigned(AfterAll) then AfterAll;
    end;
  except
    on x: Exception do
      Fail(x);
  end;
end;

function TSuite.GetSpecsCount: Integer;
var
  i: Integer;
  test: TTest;
begin
  Result := 0;
  for i := 0 to Pred(children.Count) do begin
    test := TTest(children[i]);
    if test is TSuite then
      Inc(Result, TSuite(test).GetSpecsCount)
    else
      Inc(Result);
  end;
end;

function TSuite.GetTestsCount: Integer;
begin
  Result := children.Count;
end;

function TSuite.GetFailsCount: Integer;
var
  i: Integer;
  test: TTest;
begin
  Result := 0;
  for i := 0 to Pred(children.Count) do begin
    test := TTest(children[i]);
    if not test.passed then
      Inc(Result);
  end;
end;

procedure TSuite.AddChild(child: TTest);
begin
  children.Add(child);
end;

destructor TSuite.Destroy;
begin
  children.Free;
  inherited;
end;

{ TSpec }
constructor TSpec.Create(description: String; callback: TProc; failAll: Boolean);
begin                
  // enumerate the spec in the active suite's children
  context := ActiveSuite as TTest;
  depth := ActiveSuite.depth + 1;  
  ActiveSuite.AddChild(self as TTest);
  // set input properties
  self.description := description;
  self.callback := callback;
  self.failAll := failAll;
  // the spec passes if it has no expectations
  passed := true;
end;

procedure TSpec.Execute;
begin
  try
    inherited;
    callback();
  except
    on x: Exception do begin
      fail(x);
      if failAll then
        raise Exception.Create('Critical Failure');
    end;
  end;
end;

{ TFailure }
constructor TFailure.Create(context: TTest; exception: Exception);
var
  spacing: String;
begin
  if Assigned(LogMessage) then begin
    spacing := StringOfChar(' ', (context.depth + 1) * 2);
    LogMessage(spacing + 'FAILED: ' + exception.message);
  end;
  self.context := context;
  self.exception := exception; 
  MarkContextFailed;
  Failures.Add(self);
end;

procedure TFailure.MarkContextFailed;
const
  maxTestDepth = 100;
  MaxTestDepthError = 'Max test depth of 100 reached, check for recursive contexts.';
var
  currentContext: TTest;
  i: Integer;
begin
  // prepare to loop 
  currentContext := self.context;
  i := 1;

  // loop from the failure's context up the stack
  while Assigned(currentContext) do begin
    currentContext.passed := false;
    // recurse until we've gone up the maximum test depth 
    // this saves us from an infinite loop if there is a recursive context
    currentContext := currentContext.context;  
    if (i = maxTestDepth) then 
      raise Exception.Create(MaxTestDepthError);
    Inc(i);
  end;
end;

{ Initialization and Finalization }
initialization
begin
  ActiveSuite := nil;
  MainSuites := TList.Create;
  Failures := TList.Create;
end;

finalization
begin
  MainSuites.Free;
  Failures.Free;
end;

end.