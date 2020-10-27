 (*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                        Sempare Templating Engine                                                 *
 *                                                                                                  *
 *                                                                                                  *
 *         https://github.com/sempare/sempare-delphi-template-engine                                *
 ****************************************************************************************************
 *                                                                                                  *
 * Copyright (c) 2020 Sempare Limited                                                               *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License                             *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                                     *
 * https://github.com/sempare/sempare-delphi-template-engine/blob/master/docs/commercial.license.md *
 *                                                                                                  *
 * Unless required by applicable law or agreed to in writing, software                              *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,                               *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                         *
 * See the License for the specific language governing permissions and                              *
 * limitations under the License.                                                                   *
 *                                                                                                  *
 *************************************************************************************************%*)
unit Sempare.Template.Evaluate;

interface

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.Diagnostics,
  System.Generics.Collections,
  Sempare.Template.AST,
  Sempare.Template.StackFrame,
  Sempare.Template.Common,
  Sempare.Template.PrettyPrint,
  Sempare.Template.Context,
  Sempare.Template.Visitor;

type
  TLoopOption = (coContinue, coBreak);
  TLoopOptions = set of TLoopOption;

  TNewLineStreamWriter = class(TStreamWriter)
  type
    TNLState = (nlsStartOfLine, nlsHasText, nlsNewLine);
  private
    FOptions: TTemplateEvaluationOptions;
    FNL: string;
    FState: TNLState;
    FBuffer: TStringBuilder;
    FIgnoreNewline: boolean;
    procedure TrimEndOfLine;
    procedure DoWrite();
  public
    constructor Create(const AStream: TStream; const AEncoding: TEncoding; const ANL: string; const AOptions: TTemplateEvaluationOptions);
    destructor Destroy; override;
    procedure Write(const AString: string); override;
    property IgnoreNewLine: boolean read FIgnoreNewline write FIgnoreNewline;
  end;

  TEvaluationTemplateVisitor = class(TBaseTemplateVisitor)
  private
    FStopWatch: TStopWatch;
    FStackFrames: TObjectStack<TStackFrame>;
    FEvalStack: TStack<TValue>;
    FStream: TStream;
    FStreamWriter: TNewLineStreamWriter;
    FLoopOptions: TLoopOptions;
    FContext: ITemplateContext;
    FAllowRootDeref: boolean;
    FLocalTemplates: TDictionary<string, ITemplate>;

    function HasBreakOrContinue: boolean; inline;
    function EncodeVariable(const AValue: TValue): TValue;
    procedure CheckRunTime(APosition: IPosition);
    function ExprListArgs(AExprList: IExprList): TArray<TValue>;
    function Invoke(AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>): TValue; overload;
    function Invoke(AExpr: IMethodCallExpr; const AObject: TValue; const AArgs: TArray<TValue>): TValue; overload;

  public
    constructor Create(AContext: ITemplateContext; const AValue: TValue; const AStream: TStream); overload;
    constructor Create(AContext: ITemplateContext; const AStackFrame: TStackFrame; const AStream: TStream); overload;
    destructor Destroy; override;

    procedure Visit(AExpr: IBinopExpr); overload; override;
    procedure Visit(AExpr: IUnaryExpr); overload; override;
    procedure Visit(AExpr: IVariableExpr); overload; override;
    procedure Visit(AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(AExpr: IValueExpr); overload; override;
    procedure Visit(AExprList: IExprList); overload; override;
    procedure Visit(AExpr: ITernaryExpr); overload; override;
    procedure Visit(AExpr: IEncodeExpr); overload; override;
    procedure Visit(AExpr: IFunctionCallExpr); overload; override;
    procedure Visit(AExpr: IMethodCallExpr); overload; override;
    procedure Visit(AExpr: IArrayExpr); overload; override;

    procedure Visit(AStmt: IAssignStmt); overload; override;
    procedure Visit(AStmt: IContinueStmt); overload; override;
    procedure Visit(AStmt: IBreakStmt); overload; override;
    procedure Visit(AStmt: IEndStmt); overload; override;
    procedure Visit(AStmt: IIncludeStmt); overload; override;
    procedure Visit(AStmt: IRequireStmt); overload; override;

    procedure Visit(AStmt: IPrintStmt); overload; override;
    procedure Visit(AStmt: IIfStmt); overload; override;
    procedure Visit(AStmt: IWhileStmt); overload; override;
    procedure Visit(AStmt: IForInStmt); overload; override;
    procedure Visit(AStmt: IForRangeStmt); overload; override;

    procedure Visit(AStmt: IProcessTemplateStmt); overload; override;
    procedure Visit(AStmt: IDefineTemplateStmt); overload; override;
    procedure Visit(AStmt: IWithStmt); overload; override;

  end;

implementation

uses
  Sempare.Template.Rtti,
  Sempare.Template.Util;

{$WARN WIDECHAR_REDUCED OFF}

const
  WHITESPACE: set of char = [#9, ' '];
  NEWLINE: set of char = [#10, #13];

{$WARN WIDECHAR_REDUCED ON}

function IsWhitespace(const AChar: char): boolean; inline;
begin
{$WARN WIDECHAR_REDUCED OFF}
  result := AChar in WHITESPACE;
{$WARN WIDECHAR_REDUCED ON}
end;

function IsNewline(const AChar: char): boolean; inline;
begin
{$WARN WIDECHAR_REDUCED OFF}
  result := AChar in NEWLINE;
{$WARN WIDECHAR_REDUCED ON}
end;

{ TEvaluationTemplateVisitor }

constructor TEvaluationTemplateVisitor.Create(AContext: ITemplateContext; const AValue: TValue; const AStream: TStream);
var
  StackFrame: TStackFrame;
begin
  StackFrame := TStackFrame.Create(AValue, nil);
  Create(AContext, StackFrame, AStream);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IValueExpr);
var
  val: TValue;
begin
  val := AExpr.Value;
  if val.IsType<TValue> then
    val := val.AsType<TValue>();
  FEvalStack.push(val);
end;

procedure TEvaluationTemplateVisitor.Visit(AExprList: IExprList);
var
  i: integer;
begin
  for i := AExprList.Count - 1 downto 0 do
  begin
    acceptvisitor(AExprList.Expr[i], self);
  end;
  // push count onto stack
  FEvalStack.push(AExprList.Count);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IContinueStmt);
begin
  include(FLoopOptions, TLoopOption.coContinue);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IVariableDerefExpr);
var
  Derefvar: TValue;
  variable: TValue;
  val: TValue;
  AllowRootDeref: IPreserveValue<boolean>;
begin
  AllowRootDeref := Preseve.Value(FAllowRootDeref, true);

  acceptvisitor(AExpr.variable, self);
  variable := FEvalStack.pop;

  AllowRootDeref.SetValue(AExpr.DerefType = dtArray);
  acceptvisitor(AExpr.DerefExpr, self);
  Derefvar := FEvalStack.pop;

  val := Deref(Position(AExpr), variable, Derefvar, eoRaiseErrorWhenVariableNotFound in FContext.Options);
  if val.IsType<TValue> then
    val := val.AsType<TValue>();
  FEvalStack.push(val);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IBinopExpr);
var
  left: TValue;
  right: TValue;
  res: TValue;
begin
  acceptvisitor(AExpr.LeftExpr, self);
  acceptvisitor(AExpr.RightExpr, self);
  right := FEvalStack.pop;
  left := FEvalStack.pop;
  case AExpr.BinOp of
    boIN:
      res := contains(Position(AExpr.RightExpr), left, right);
    boAND:
      begin
        AssertBoolean(Position(AExpr.LeftExpr), left, right);
        res := AsBoolean(left) and AsBoolean(right);
      end;
    boOR:
      begin
        AssertBoolean(Position(AExpr.LeftExpr), left, right);
        res := AsBoolean(left) or AsBoolean(right);
      end;
    boPlus:
      if isNumLike(left) and isNumLike(right) then
        res := Asnum(left) + Asnum(right)
      else if isStrLike(left) and isStrLike(right) then
        res := left.AsString + right.AsString
      else if isStrLike(left) and isNumLike(right) then
        res := left.AsString + AsString(right)
      else
        RaiseError(Position(AExpr.LeftExpr), 'String or numeric types expected');
    boMinus:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), left, right);
        res := Asnum(left) - Asnum(right);
      end;
    boSlash:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), left, right);
        res := Asnum(left) / Asnum(right);
      end;
    boDiv:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), left, right);
        res := trunc(Asnum(left)) div trunc(Asnum(right));
      end;
    boMult:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), left, right);
        res := Asnum(left) * Asnum(right);
      end;
    boMod:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), left, right);
        res := asint(left) mod asint(right);
      end;
    boEQ:
      res := isEqual(left, right);
    boNotEQ:
      res := not isEqual(left, right);
    boLT:
      res := isLessThan(left, right);
    boLTE:
      res := not isGreaterThan(left, right);
    boGT:
      res := isGreaterThan(left, right);
    boGTE:
      res := not isLessThan(left, right);
  else
    RaiseError(Position(AExpr.LeftExpr), 'Binop not supported');
  end;
  FEvalStack.push(res);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IUnaryExpr);
var
  v: TValue;
begin
  acceptvisitor(AExpr.Condition, self);
  v := FEvalStack.pop;
  case AExpr.UnaryOp of
    uoMinus:
      begin
        FEvalStack.push(-Asnum(v));
      end;
    uoNot:
      begin
        FEvalStack.push(not AsBoolean(v));
      end
  else
    RaiseError(Position(AExpr), 'Unary op not supported');
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IVariableExpr);
var
  StackFrame: TStackFrame;
  val: TValue;
begin
  if FAllowRootDeref then
  begin
    StackFrame := FStackFrames.peek;
    val := StackFrame[AExpr.variable];
    if val.IsEmpty then
    begin
      try
        val := Deref(Position(AExpr), StackFrame.Root, AExpr.variable, eoRaiseErrorWhenVariableNotFound in FContext.Options);
      except
        on e: exception do
        begin
          if (eoRaiseErrorWhenVariableNotFound in FContext.Options) then
            RaiseError(Position(AExpr), 'Variable ''%s'' could not be found.', [AExpr.variable]);
        end;
      end;
    end;
    if val.IsEmpty and (eoRaiseErrorWhenVariableNotFound in FContext.Options) then
      RaiseError(Position(AExpr), 'Variable ''%s'' could not be found.', [AExpr.variable]);
  end
  else
  begin
    val := AExpr.variable;
  end;
  if val.IsType<TValue> then
    val := val.AsType<TValue>();
  FEvalStack.push(val);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IBreakStmt);
begin
  include(FLoopOptions, TLoopOption.coBreak);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IWhileStmt);
var
  LoopOptions: IPreserveValue<TLoopOptions>;
begin
  if HasBreakOrContinue then
    exit;
  LoopOptions := Preseve.Value<TLoopOptions>(FLoopOptions, []);
  FStackFrames.push(FStackFrames.peek.Clone);
  while true do
  begin
    acceptvisitor(AStmt.Condition, self);
    if not AsBoolean(FEvalStack.pop) or (coBreak in FLoopOptions) then
      break;
    CheckRunTime(Position(AStmt));
    exclude(FLoopOptions, coContinue);
    acceptvisitor(AStmt.Container, self);
  end;
  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IForInStmt);
var
  LoopOptions: IPreserveValue<TLoopOptions>;
  e: TObject;
  v: string;
  val, Eval: TValue;
  T: TRttiType;
  m, movenext: TRttiMethod;
  current: TRttiProperty;

  procedure visitobject;
  begin
    m := T.GetMethod('GetEnumerator');
    if m = nil then
      RaiseError(Position(AStmt), 'GetEnumerator not found on object.');
    val := m.Invoke(Eval.AsObject, []).AsObject;
    if val.IsEmpty then
      raise exception.Create('Value is not enumerable');
    try
      e := val.AsObject;
      T := GRttiContext.GetType(e.ClassType);
      movenext := T.GetMethod('MoveNext');
      current := T.GetProperty('Current');

      while movenext.Invoke(e, []).AsBoolean do
      begin
        if coBreak in FLoopOptions then
          break;
        FStackFrames.peek[v] := current.GetValue(e);
        exclude(FLoopOptions, coContinue);
        CheckRunTime(Position(AStmt));
        acceptvisitor(AStmt.Container, self);
      end;
    finally
      e.Free;
    end;
  end;

  procedure visitarray;
  var
    at: TRttiArrayType;
    Dt: TRttiOrdinalType;
    i, min: int64;
    c: TRttiContext;
  begin
    c := TRttiContext.Create;
    at := T as TRttiArrayType;
    if at.DimensionCount <> 1 then
      RaiseError(Position(AStmt), 'Only one dimensional arrays are supported.');
    Dt := at.Dimensions[0] as TRttiOrdinalType;
    if Dt = nil then
      min := 0
    else
      min := Dt.MinValue;
    for i := 0 to Eval.GetArrayLength - 1 do
    begin
      if coBreak in FLoopOptions then
        break;
      FStackFrames.peek[v] := i + min;
      exclude(FLoopOptions, coContinue);
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

  procedure visitdynarray;
  var
    i: int64;
  begin
    for i := 0 to Eval.GetArrayLength - 1 do
    begin
      if coBreak in FLoopOptions then
        break;
      FStackFrames.peek[v] := i;
      exclude(FLoopOptions, coContinue);
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

begin
  if HasBreakOrContinue then
    exit;
  LoopOptions := Preseve.Value<TLoopOptions>(FLoopOptions, []);
  FStackFrames.push(FStackFrames.peek.Clone);
  v := AStmt.variable;

  acceptvisitor(AStmt.Expr, self);
  Eval := FEvalStack.pop;
  if Eval.IsType<TValue> then
    Eval := Eval.AsType<TValue>();
  if not Eval.IsEmpty then
  begin
    T := GRttiContext.GetType(Eval.typeinfo);

    case T.TypeKind of
      tkClass, tkClassRef:
        visitobject;
      tkArray:
        visitarray;
      tkDynArray:
        visitdynarray;
    else
      RaiseError(Position(AStmt), 'GetEnumerator not found on object.');
    end;
  end;
  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.CheckRunTime(APosition: IPosition);
begin
  if FStopWatch.ElapsedMilliseconds > FContext.MaxRunTimeMs then
    RaiseError(APosition, 'Max runtime of %dms has been exceeded.', [FContext.MaxRunTimeMs]);
end;

constructor TEvaluationTemplateVisitor.Create(AContext: ITemplateContext; const AStackFrame: TStackFrame; const AStream: TStream);
var
  apply: ITemplateContextForScope;
begin
  FAllowRootDeref := true;
  FStopWatch := TStopWatch.Create;
  FStopWatch.Start;

  FLocalTemplates := TDictionary<string, ITemplate>.Create;

  FLoopOptions := [];
  FContext := AContext;
  FStream := AStream;

  FStreamWriter := TNewLineStreamWriter.Create(FStream, FContext.Encoding, FContext.NEWLINE, FContext.Options);

  FEvalStack := TStack<TValue>.Create;
  FStackFrames := TObjectStack<TStackFrame>.Create;
  FStackFrames.push(AStackFrame);

  if AContext.QueryInterface(ITemplateContextForScope, apply) = s_ok then
    apply.ApplyTo(FStackFrames.peek);
end;

destructor TEvaluationTemplateVisitor.Destroy;
begin
  FLocalTemplates.Free;
  FStreamWriter.Free;
  FStopWatch.Stop;
  FEvalStack.Free;
  FStackFrames.Free;
  FContext := nil;
  inherited;
end;

function TEvaluationTemplateVisitor.EncodeVariable(const AValue: TValue): TValue;
begin
  if FContext.VariableEncoder = nil then
    exit(AValue);
  result := FContext.VariableEncoder(AsString(AValue));
end;

function TEvaluationTemplateVisitor.ExprListArgs(AExprList: IExprList): TArray<TValue>;
var
  i: integer;
  Count: integer;
begin
  AExprList.Accept(self);

  Count := asint(FEvalStack.pop());
  if Count <> AExprList.Count then // this should not happen
    RaiseError(nil, 'Number of arguments mismatch');
  setlength(result, Count);
  for i := 0 to Count - 1 do
  begin
    result[i] := FEvalStack.pop;
  end;
end;

function TEvaluationTemplateVisitor.HasBreakOrContinue: boolean;
begin
  result := (coContinue in FLoopOptions) or (coBreak in FLoopOptions);
end;

function ForToCond(const ALow: integer; const AHigh: integer): boolean;
begin
  result := ALow <= AHigh;
end;

function ForDownToCond(const ALow: integer; const AHigh: integer): boolean;
begin
  result := ALow >= AHigh;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IForRangeStmt);
type
  TCompare = function(const ALow: integer; const AHigh: integer): boolean;

var
  LoopOptions: IPreserveValue<TLoopOptions>;
  i, lowVal, highVal: int64;
  delta: integer;
  v: string;
  comp: TCompare;

begin
  if HasBreakOrContinue then
    exit;
  v := AStmt.variable;
  LoopOptions := Preseve.Value<TLoopOptions>(FLoopOptions, []);

  acceptvisitor(AStmt.LowExpr, self);
  lowVal := asint(FEvalStack.pop);

  acceptvisitor(AStmt.HighExpr, self);
  highVal := asint(FEvalStack.pop);

  case AStmt.ForOp of
    foTo:
      begin
        delta := 1;
        comp := ForToCond;
      end;
    foDownto:
      begin
        delta := -1;
        comp := ForDownToCond;
      end
  else
    raise exception.Create('ForOp not supported');
  end;
  FStackFrames.push(FStackFrames.peek.Clone);
  i := lowVal;
  while comp(i, highVal) do
  begin
    FStackFrames.peek[v] := i;
    if coBreak in FLoopOptions then
      break;
    exclude(FLoopOptions, coContinue);
    CheckRunTime(Position(AStmt));
    acceptvisitor(AStmt.Container, self);
    inc(i, delta);
  end;
  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IAssignStmt);
var
  v: string;
  val: TValue;
begin
  v := AStmt.variable;
  acceptvisitor(AStmt.Expr, self);
  val := FEvalStack.pop;
  if val.IsType<TValue> then
    val := val.AsType<TValue>;
  FStackFrames.peek[v] := val;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IIfStmt);
begin
  if HasBreakOrContinue then
    exit;
   acceptvisitor(AStmt.Condition, self);
  if AsBoolean(FEvalStack.pop) then
    acceptvisitor(AStmt.TrueContainer, self)
  else if AStmt.FalseContainer <> nil then
    acceptvisitor(AStmt.FalseContainer, self);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IEndStmt);
begin
  // nothing to do
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IIncludeStmt);
var
  Name: string;
  T: ITemplate;

begin
  if HasBreakOrContinue then
    exit;
  acceptvisitor(AStmt.Expr, self);
  name := FEvalStack.pop.AsString;

  if FLocalTemplates.TryGetValue(name, T) or FContext.TryGetTemplate(name, T) then
  begin
    FStackFrames.push(FStackFrames.peek.Clone());
    try
      Visit(T);
    finally
      FStackFrames.pop;
    end;
  end
  else
    raise exception.Createfmt('Template not found: %s', [name]);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IPrintStmt);
begin
  if HasBreakOrContinue then
    exit;
  acceptvisitor(AStmt.Expr, self);
  FStreamWriter.Write(AsString(FEvalStack.pop));
end;

function CastArg(const AValue: TValue; const AType: TRttiType): TValue;
begin
  case AType.TypeKind of
    tkInteger, tkInt64:
      if not(AValue.Kind in [tkInteger, tkInt64]) then
        exit(asint(AValue));
    tkFloat:
      if AValue.Kind <> tkFloat then
        exit(Asnum(AValue));
    tkString, tkWString, tkAnsiString, tkUString: // WideString, UnicodeString
      if not(AValue.Kind in [tkString, tkWString, tkAnsiString, tkUString]) then
        exit(AsString(AValue));
    tkEnumeration:
      if AType.Handle = typeinfo(boolean) then
        exit(AsBoolean(AValue));
    tkrecord:
      if AType.Handle = typeinfo(tvarrec) then
        exit(AValue.AsVarRec);
  end;
  exit(AValue);
end;

function GetArgs(const AMethod: TRttiMethod; const AArgs: TArray<TValue>): TArray<TValue>;
var
  arg: TRttiParameter;
  i: integer;
  v: TValue;
begin
  if (length(AMethod.GetParameters) = 1) and (AMethod.GetParameters[0].ParamType.TypeKind = tkDynArray) then
  begin
    setlength(result, 1);
    result[0] := TValue.From(AArgs);
    exit;
  end;
  setlength(result, length(AArgs));
  i := 0;
  for arg in AMethod.GetParameters do
  begin
    v := CastArg(AArgs[i], arg.ParamType);
    if v.IsType<TValue> then
      v := v.AsType<TValue>();
    result[i] := v;
    inc(i);
  end;
end;

function TEvaluationTemplateVisitor.Invoke(AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>): TValue;
var
  m: TRttiMethod;
begin
  // there should always be at least one element
  m := AFuncCall.FunctionInfo[0];
  if length(AFuncCall.FunctionInfo) > 1 then
  begin
    for m in AFuncCall.FunctionInfo do
      if length(m.GetParameters) = length(AArgs) then
        break;
  end;
  if length(m.GetParameters) = 0 then
    result := m.Invoke(nil, [])
  else
    result := m.Invoke(nil, GetArgs(m, AArgs));
  if result.IsType<TValue> then
    result := result.AsType<TValue>();
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IFunctionCallExpr);
begin
  try
    FEvalStack.push(Invoke(AExpr, ExprListArgs(AExpr.exprlist)));
  except
    on e: exception do
      RaiseError(Position(AExpr), e.Message);
  end;
end;

function TEvaluationTemplateVisitor.Invoke(AExpr: IMethodCallExpr; const AObject: TValue; const AArgs: TArray<TValue>): TValue;
var
  RttiType: TRttiType;
begin
  if AExpr.RttiMethod = nil then
  begin
    RttiType := GRttiContext.GetType(AObject.typeinfo);
    AExpr.RttiMethod := RttiType.GetMethod(AExpr.Method);
  end;
  result := AExpr.RttiMethod.Invoke(AObject, AArgs);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IMethodCallExpr);
var
  obj: TValue;
  args: TArray<TValue>;
begin
  acceptvisitor(AExpr.ObjectExpr, self);
  obj := FEvalStack.pop;
  args := ExprListArgs(AExpr.exprlist);
  try
    FEvalStack.push(Invoke(AExpr, obj, args));
  except
    on e: exception do
      RaiseError(Position(AExpr), e.Message);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IEncodeExpr);
begin
  acceptvisitor(AExpr.Expr, self);
  FEvalStack.push(EncodeVariable(FEvalStack.pop));
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IProcessTemplateStmt);
var
  prevState: boolean;
begin
  prevState := FStreamWriter.IgnoreNewLine;
  FStreamWriter.IgnoreNewLine := not AStmt.AllowNewLine;
  acceptvisitor(AStmt.Container, self);
  FStreamWriter.IgnoreNewLine := prevState;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IDefineTemplateStmt);
var
  Name: TValue;
begin
  acceptvisitor(AStmt.Name, self);
  name := FEvalStack.pop;
  AssertString(Position(AStmt), name);
  FLocalTemplates.AddOrSetValue(AsString(name), AStmt.Container);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IWithStmt);

var
  StackFrame: TStackFrame;

procedure ScanClass(const ARttiType: TRttiType; const AClass: TValue); forward;
procedure ScanRecord(const ARttiType: TRttiType; const ARecord: TValue); forward;

  procedure ScanValue(const ARecord: TValue);
  var
    RttiType: TRttiType;
  begin
    RttiType := GRttiContext.GetType(ARecord.typeinfo);
    case RttiType.TypeKind of
      tkClass, tkClassRef:
        ScanClass(RttiType, ARecord);
      tkrecord:
        ScanRecord(RttiType, ARecord);
    else
      raise exception.Create('StackFrame must be defined on a class or record.');
    end;
  end;

  procedure ScanClass(const ARttiType: TRttiType; const AClass: TValue);
  var
    field: TRttiField;
    prop: TRttiProperty;
    obj: TObject;
  begin
    obj := AClass.AsObject;

    if PopulateStackFrame(StackFrame, ARttiType, AClass) then
      exit;
    // we can't do anything on generic collections. we can loop using _ if we need to do anything.
    if obj.ClassType.QualifiedClassName.StartsWith('System.Generics.Collections') then
      exit;
    for field in ARttiType.GetFields do
      StackFrame[field.Name] := field.GetValue(obj);
    for prop in ARttiType.GetProperties do
      StackFrame[prop.Name] := prop.GetValue(obj);
  end;

  procedure ScanRecord(const ARttiType: TRttiType; const ARecord: TValue);
  var
    field: TRttiField;
    obj: pointer;
  begin
    obj := ARecord.GetReferenceToRawData;
    for field in ARttiType.GetFields do
      StackFrame[field.Name] := field.GetValue(obj);
  end;

var
  Expr: TValue;
begin
  acceptvisitor(AStmt.Expr, self);
  Expr := FEvalStack.pop;
  StackFrame := FStackFrames.peek.Clone;
  FStackFrames.push(StackFrame);

  ScanValue(Expr);
  acceptvisitor(AStmt.Container, self);

  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IRequireStmt);
var
  exprs: TArray<TValue>;
  InputType: string;
  i: integer;
begin
  InputType := GRttiContext.GetType(FStackFrames.peek['_'].typeinfo).Name.ToLower;
  exprs := ExprListArgs(AStmt.exprlist);
  if length(exprs) = 0 then
    exit;
  for i := 0 to length(exprs) do
  begin
    if AsString(exprs[i]).ToLower = InputType then
      exit;
  end;
  RaiseError(Position(AStmt), 'Input of required tpe not found');
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IArrayExpr);
var
  exprs: TArray<TValue>;
  v: TValue;
begin
  exprs := ExprListArgs(AExpr.exprlist);
  v := TValue.From < TArray < TValue >> (exprs);
  FEvalStack.push(v);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: ITernaryExpr);
begin
  acceptvisitor(AExpr.Condition, self);
  if AsBoolean(FEvalStack.pop) then
    acceptvisitor(AExpr.TrueExpr, self)
  else
    acceptvisitor(AExpr.FalseExpr, self);
end;

{ TNewLineStreamWriter }

constructor TNewLineStreamWriter.Create(const AStream: TStream; const AEncoding: TEncoding; const ANL: string; const AOptions: TTemplateEvaluationOptions);
begin
  inherited Create(AStream, AEncoding);
  FBuffer := TStringBuilder.Create;
  FOptions := AOptions;
  FNL := ANL;
  FState := nlsStartOfLine;
end;

destructor TNewLineStreamWriter.Destroy;
begin
  TrimEndOfLine();
  if FBuffer.length > 0 then
    DoWrite;
  FBuffer.Free;
  inherited;
end;

procedure TNewLineStreamWriter.DoWrite();
begin
  inherited write(FBuffer.ToString());
  FBuffer.clear;
end;

procedure TNewLineStreamWriter.TrimEndOfLine;
begin
  if eoTrimLines in FOptions then
  begin
    while (FBuffer.length >= 1) and IsWhitespace(FBuffer.Chars[FBuffer.length - 1]) do
      FBuffer.length := FBuffer.length - 1;
  end;
end;

procedure TNewLineStreamWriter.Write(const AString: string);
var
  c: char;

  procedure Append(const ANext: TNLState);
  begin
    FBuffer.Append(c);
    FState := ANext;
  end;

begin
  for c in AString do
  begin
    case FState of
      nlsStartOfLine:
        if IsWhitespace(c) then
        begin
          if not(eoTrimLines in FOptions) then
            Append(nlsHasText);
        end
        else if IsNewline(c) then
        begin
          if not FIgnoreNewline and not(eoStripRecurringNewlines in FOptions) then
          begin
            Append(nlsNewLine);
            DoWrite();
          end;
        end
        else
          Append(nlsHasText);
      nlsHasText:
        if IsWhitespace(c) then
          Append(nlsHasText)
        else if IsNewline(c) then
        begin
          if not FIgnoreNewline then
          begin
            TrimEndOfLine();
            FBuffer.Append(FNL);
            FState := nlsNewLine;
            DoWrite();
          end;
        end
        else
          Append(nlsHasText);
      nlsNewLine:
        if IsWhitespace(c) then
        begin
          if not(eoTrimLines in FOptions) then
            Append(nlsNewLine);
        end
        else if IsNewline(c) then
        begin
          if not FIgnoreNewline and not(eoStripRecurringNewlines in FOptions) then
          begin
            Append(nlsNewLine);
            DoWrite();
          end;
        end
        else
          Append(nlsHasText);
    end;
  end;
end;

end.
