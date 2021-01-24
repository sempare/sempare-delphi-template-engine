(*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                          Sempare Template Engine                                                 *
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
  ETemplateEval = class(ETemplate);

  TLoopOption = (coContinue, coBreak);
  TLoopOptions = set of TLoopOption;

  TNewLineStreamWriter = class(TStreamWriter)
  private
    FOptions: TTemplateEvaluationOptions;
    FNL: string;
    FBuffer: TStringBuilder;
    FIgnoreNewline: boolean;
    FStartOfLine: boolean;
    FLastChar: char;
    procedure TrimEndOfLine;
    procedure TrimLast;
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
    FStreamWriter: TStreamWriter;
    FIsNLStreamWriter: boolean;
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
  Data.DB,
  System.TypInfo, // needed for XE6 and below to access the TTypeKind variables
  Sempare.Template.ResourceStrings,
  Sempare.Template.Rtti,
  Sempare.Template.Util;

{$WARN WIDECHAR_REDUCED OFF}

const
  WHITESPACE: set of char = [#9, ' '];

{$WARN WIDECHAR_REDUCED ON}

function IsWhitespace(const AChar: char): boolean; inline;
begin
{$WARN WIDECHAR_REDUCED OFF}
  exit(AChar in WHITESPACE);
{$WARN WIDECHAR_REDUCED ON}
end;

function IsNewline(const AChar: char): boolean; inline;
begin
  exit(AChar = #10);
end;

function IsCR(const AChar: char): boolean; inline;
begin
  exit(AChar = #13);
end;

{ TEvaluationTemplateVisitor }

constructor TEvaluationTemplateVisitor.Create(AContext: ITemplateContext; const AValue: TValue; const AStream: TStream);
begin
  Create(AContext, TStackFrame.Create(AValue, nil), AStream);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IValueExpr);
var
  LValue: TValue;
begin
  LValue := AExpr.Value;
  if LValue.IsType<TValue> then
    LValue := LValue.AsType<TValue>();
  FEvalStack.push(LValue);
end;

procedure TEvaluationTemplateVisitor.Visit(AExprList: IExprList);
var
  LIdx: integer;
begin
  for LIdx := AExprList.Count - 1 downto 0 do
  begin
    acceptvisitor(AExprList.Expr[LIdx], self);
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
  LDerefKey: TValue;
  LDerefObj: TValue;
  LDerefedValue: TValue;
  LAllowRootDeref: IPreserveValue<boolean>;
begin
  LAllowRootDeref := Preserve.Value<boolean>(FAllowRootDeref, true);

  acceptvisitor(AExpr.variable, self);
  LDerefObj := FEvalStack.pop;

  LAllowRootDeref.SetValue(AExpr.DerefType = dtArray);
  acceptvisitor(AExpr.DerefExpr, self);
  LDerefKey := FEvalStack.pop;

  LDerefedValue := Deref(Position(AExpr), LDerefObj, LDerefKey, eoRaiseErrorWhenVariableNotFound in FContext.Options);
  if LDerefedValue.IsType<TValue> then
    LDerefedValue := LDerefedValue.AsType<TValue>();
  FEvalStack.push(LDerefedValue);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IBinopExpr);
var
  LLeft: TValue;
  LRight: TValue;
  LResult: TValue;
begin
  acceptvisitor(AExpr.LeftExpr, self);
  acceptvisitor(AExpr.RightExpr, self);
  LRight := FEvalStack.pop;
  LLeft := FEvalStack.pop;
  case AExpr.BinOp of
    boIN:
      LResult := contains(Position(AExpr.RightExpr), LLeft, LRight);
    boAND:
      begin
        AssertBoolean(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsBoolean(LLeft) and AsBoolean(LRight);
      end;
    boOR:
      begin
        AssertBoolean(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsBoolean(LLeft) or AsBoolean(LRight);
      end;
    boPlus:
      if isNumLike(LLeft) and isNumLike(LRight) then
        LResult := AsNum(LLeft) + AsNum(LRight)
      else if isStrLike(LLeft) and isStrLike(LRight) then
        LResult := LLeft.AsString + LRight.AsString
      else if isStrLike(LLeft) and isNumLike(LRight) then
        LResult := LLeft.AsString + AsString(LRight)
      else
        RaiseError(Position(AExpr.LeftExpr), SStringOrNumericTypesExpected);
    boMinus:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsNum(LLeft) - AsNum(LRight);
      end;
    boSlash:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsNum(LLeft) / AsNum(LRight);
      end;
    boDiv:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := trunc(AsNum(LLeft)) div trunc(AsNum(LRight));
      end;
    boMult:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsNum(LLeft) * AsNum(LRight);
      end;
    boMod:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsInt(LLeft) mod AsInt(LRight);
      end;
    boEQ:
      LResult := isEqual(LLeft, LRight);
    boNotEQ:
      LResult := not isEqual(LLeft, LRight);
    boLT:
      LResult := isLessThan(LLeft, LRight);
    boLTE:
      LResult := not isGreaterThan(LLeft, LRight);
    boGT:
      LResult := isGreaterThan(LLeft, LRight);
    boGTE:
      LResult := not isLessThan(LLeft, LRight);
  else
    RaiseError(Position(AExpr.LeftExpr), SBinOpNotSupported);
  end;
  FEvalStack.push(LResult);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IUnaryExpr);
var
  LValue: TValue;
begin
  acceptvisitor(AExpr.Condition, self);
  LValue := FEvalStack.pop;
  case AExpr.UnaryOp of
    uoMinus:
      begin
        FEvalStack.push(-AsNum(LValue));
      end;
    uoNot:
      begin
        FEvalStack.push(not AsBoolean(LValue));
      end
  else
    RaiseError(Position(AExpr), SUnaryOpNotSupported);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IVariableExpr);
var
  LStackFrame: TStackFrame;
  LValue: TValue;
begin
  if FAllowRootDeref then
  begin
    LStackFrame := FStackFrames.peek;
    LValue := LStackFrame[AExpr.variable];
    if LValue.IsEmpty then
    begin
      try
        LValue := Deref(Position(AExpr), LStackFrame.Root, AExpr.variable, eoRaiseErrorWhenVariableNotFound in FContext.Options);
      except
        on e: exception do
        begin
          if (eoRaiseErrorWhenVariableNotFound in FContext.Options) then
            RaiseError(Position(AExpr), SCannotFindValiable, [AExpr.variable]);
        end;
      end;
    end;
    if LValue.IsEmpty and (eoRaiseErrorWhenVariableNotFound in FContext.Options) then
      RaiseError(Position(AExpr), SCannotFindValiable, [AExpr.variable]);
  end
  else
  begin
    LValue := AExpr.variable;
  end;
  if LValue.IsType<TValue> then
    LValue := LValue.AsType<TValue>();
  FEvalStack.push(LValue);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IBreakStmt);
begin
  include(FLoopOptions, TLoopOption.coBreak);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IWhileStmt);
var
  LLoopOptions: IPreserveValue<TLoopOptions>;
begin
  if HasBreakOrContinue then
    exit;
  LLoopOptions := Preserve.Value<TLoopOptions>(FLoopOptions, []);
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
  LLoopOptions: IPreserveValue<TLoopOptions>;
  LVariableName: string;
  LLoopExpr: TValue;
  LLoopExprType: TRttiType;

  procedure VisitDataSet;
  var
    LIdx: integer;
    LDataSetFirstMethod: TRttiMethod;
    LDataSetNextMethod: TRttiMethod;
    LDataSetEOFProperty: TRttiProperty;
    LObj: TObject;
  begin
    LIdx := 1;
    LDataSetFirstMethod := LLoopExprType.GetMethod('First');
    LObj := LLoopExpr.AsObject;
    LDataSetFirstMethod.Invoke(LObj, []);
    LDataSetEOFProperty := LLoopExprType.getProperty('EOF');
    LDataSetNextMethod := LLoopExprType.GetMethod('Next');
    while not LDataSetEOFProperty.GetValue(LObj).AsBoolean do
    begin
      if coBreak in FLoopOptions then
        break;
      FStackFrames.peek[LVariableName] := LIdx;
      exclude(FLoopOptions, coContinue);
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
      LDataSetNextMethod.Invoke(LObj, []);
      inc(LIdx);
    end;
  end;

  procedure visitobject;
  var
    LEnumGetEnumeratorMethod: TRttiMethod;
    LEnumObj: TObject;
    LEnumMoveNextMethod: TRttiMethod;
    LEnumCurrentProperty: TRttiProperty;
    LEnumValue: TValue;
  begin
    if LLoopExprType.AsInstance.MetaclassType.InheritsFrom(TDataSet) then
    begin
      VisitDataSet;
      exit;
    end;
    LEnumGetEnumeratorMethod := LLoopExprType.GetMethod('GetEnumerator');
    if LEnumGetEnumeratorMethod = nil then
      RaiseError(Position(AStmt), SGetEnumeratorNotFoundOnObject);
    LEnumValue := LEnumGetEnumeratorMethod.Invoke(LLoopExpr.AsObject, []);
    if LEnumValue.IsEmpty then
      raise ETemplateEval.Create(SValueIsNotEnumerable);
    LEnumObj := LEnumValue.AsObject;
    try
      LLoopExprType := GRttiContext.GetType(LEnumObj.ClassType);
      LEnumMoveNextMethod := LLoopExprType.GetMethod('MoveNext');
      LEnumCurrentProperty := LLoopExprType.getProperty('Current');

      while LEnumMoveNextMethod.Invoke(LEnumObj, []).AsBoolean do
      begin
        if coBreak in FLoopOptions then
          break;
        FStackFrames.peek[LVariableName] := LEnumCurrentProperty.GetValue(LEnumObj);
        exclude(FLoopOptions, coContinue);
        CheckRunTime(Position(AStmt));
        acceptvisitor(AStmt.Container, self);
      end;
    finally
      LEnumObj.Free;
    end;
  end;

  procedure visitarray;
  var
    LArrayType: TRttiArrayType;
    LDimOrdType: TRttiOrdinalType;
    LIdx: integer;
    LMin: integer;
  begin
    LArrayType := LLoopExprType as TRttiArrayType;
    if LArrayType.DimensionCount <> 1 then
      RaiseError(Position(AStmt), SOnlyOneDimensionalArraysAreSupported);
    LDimOrdType := LArrayType.Dimensions[0] as TRttiOrdinalType;
    if LDimOrdType = nil then
      LMin := 0
    else
      LMin := LDimOrdType.MinValue;
    for LIdx := 0 to LLoopExpr.GetArrayLength - 1 do
    begin
      if coBreak in FLoopOptions then
        break;
      FStackFrames.peek[LVariableName] := LIdx + LMin;
      exclude(FLoopOptions, coContinue);
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

  procedure visitdynarray;
  var
    LIdx: integer;
  begin
    for LIdx := 0 to LLoopExpr.GetArrayLength - 1 do
    begin
      if coBreak in FLoopOptions then
        break;
      FStackFrames.peek[LVariableName] := LIdx;
      exclude(FLoopOptions, coContinue);
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

begin
  if HasBreakOrContinue then
    exit;
  LLoopOptions := Preserve.Value<TLoopOptions>(FLoopOptions, []);
  FStackFrames.push(FStackFrames.peek.Clone);
  LVariableName := AStmt.variable;

  acceptvisitor(AStmt.Expr, self);
  LLoopExpr := FEvalStack.pop;
  if LLoopExpr.IsType<TValue> then
    LLoopExpr := LLoopExpr.AsType<TValue>();
  if not LLoopExpr.IsEmpty then
  begin
    LLoopExprType := GRttiContext.GetType(LLoopExpr.typeinfo);

    case LLoopExprType.TypeKind of
      tkClass, tkClassRef:
        visitobject;
      tkArray:
        visitarray;
      tkDynArray:
        visitdynarray;
    else
      RaiseError(Position(AStmt), SGetEnumeratorNotFoundOnObject);
    end;
  end;
  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.CheckRunTime(APosition: IPosition);
begin
  if FStopWatch.ElapsedMilliseconds > FContext.MaxRunTimeMs then
    RaiseError(APosition, SMaxRuntimeOfMsHasBeenExceeded, [FContext.MaxRunTimeMs]);
end;

constructor TEvaluationTemplateVisitor.Create(AContext: ITemplateContext; const AStackFrame: TStackFrame; const AStream: TStream);
var
  LApply: ITemplateContextForScope;
begin
  inherited Create();
  FAllowRootDeref := true;
  FStopWatch := TStopWatch.Create;
  FStopWatch.Start;

  FLocalTemplates := TDictionary<string, ITemplate>.Create;

  FLoopOptions := [];
  FContext := AContext;
  FStream := AStream;

  FStreamWriter := FContext.StreamWriterProvider(FStream, FContext);
  FIsNLStreamWriter := FStreamWriter is TNewLineStreamWriter;

  FEvalStack := TStack<TValue>.Create;
  FStackFrames := TObjectStack<TStackFrame>.Create;
  FStackFrames.push(AStackFrame);

  if supports(AContext, ITemplateContextForScope, LApply) then
    LApply.ApplyTo(FStackFrames.peek);
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
  exit(FContext.VariableEncoder(AsString(AValue)));
end;

function TEvaluationTemplateVisitor.ExprListArgs(AExprList: IExprList): TArray<TValue>;
var
  LIdx: integer;
  LCount: integer;
begin
  AExprList.Accept(self);

  LCount := AsInt(FEvalStack.pop());
  if LCount <> AExprList.Count then // this should not happen
    RaiseError(nil, SNumberOfArgsMismatch);
  setlength(result, LCount);
  for LIdx := 0 to LCount - 1 do
  begin
    result[LIdx] := FEvalStack.pop;
  end;
end;

function TEvaluationTemplateVisitor.HasBreakOrContinue: boolean;
begin
  exit((coContinue in FLoopOptions) or (coBreak in FLoopOptions));
end;

function ForToCond(const ALow: integer; const AHigh: integer): boolean;
begin
  exit(ALow <= AHigh);
end;

function ForDownToCond(const ALow: integer; const AHigh: integer): boolean;
begin
  exit(ALow >= AHigh);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IForRangeStmt);
type
  TCompare = function(const ALow: integer; const AHigh: integer): boolean;

var
  LLoopOptions: IPreserveValue<TLoopOptions>;
  LIdx: int64;
  LStartVal: int64;
  LEndVal: int64;
  LDelta: integer;
  LVariable: string;
  LDirectionTestFunc: TCompare;

begin
  if HasBreakOrContinue then
    exit;
  LVariable := AStmt.variable;
  LLoopOptions := Preserve.Value<TLoopOptions>(FLoopOptions, []);

  acceptvisitor(AStmt.LowExpr, self);
  LStartVal := AsInt(FEvalStack.pop);

  acceptvisitor(AStmt.HighExpr, self);
  LEndVal := AsInt(FEvalStack.pop);

  case AStmt.ForOp of
    foTo:
      begin
        LDelta := 1;
        LDirectionTestFunc := ForToCond;
      end;
    foDownto:
      begin
        LDelta := -1;
        LDirectionTestFunc := ForDownToCond;
      end
  else
    raise ETemplateEval.Create(STypeNotSupported);
  end;
  FStackFrames.push(FStackFrames.peek.Clone);
  LIdx := LStartVal;
  while LDirectionTestFunc(LIdx, LEndVal) do
  begin
    FStackFrames.peek[LVariable] := LIdx;
    if coBreak in FLoopOptions then
      break;
    exclude(FLoopOptions, coContinue);
    CheckRunTime(Position(AStmt));
    acceptvisitor(AStmt.Container, self);
    inc(LIdx, LDelta);
  end;
  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IAssignStmt);
var
  LVariable: string;
  LValue: TValue;
begin
  LVariable := AStmt.variable;
  acceptvisitor(AStmt.Expr, self);
  LValue := FEvalStack.pop;
  if LValue.IsType<TValue> then
    LValue := LValue.AsType<TValue>;
  FStackFrames.peek[LVariable] := LValue;
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
  LTemplateName: string;
  LTemplate: ITemplate;

begin
  if HasBreakOrContinue then
    exit;
  acceptvisitor(AStmt.Expr, self);
  LTemplateName := FEvalStack.pop.AsString;

  if FLocalTemplates.TryGetValue(LTemplateName, LTemplate) or FContext.TryGetTemplate(LTemplateName, LTemplate) then
  begin
    FStackFrames.push(FStackFrames.peek.Clone());
    try
      Visit(LTemplate);
    finally
      FStackFrames.pop;
    end;
  end
  else
    raise ETemplateEval.Createfmt(STemplateNotFound, [LTemplateName]);
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
        exit(AsInt(AValue));
    tkFloat:
      if AValue.Kind <> tkFloat then
        exit(AsNum(AValue));
    tkString, tkWString, tkLString, tkUString:
      if not(AValue.Kind in [tkString, tkWString, tkLString, tkUString]) then
        exit(AsString(AValue));
    tkEnumeration:
      if AType.Handle = typeinfo(boolean) then
        exit(AsBoolean(AValue));
  end;
  exit(AValue);
end;

function GetArgs(const AMethod: TRttiMethod; const AArgs: TArray<TValue>): TArray<TValue>;
var
  LParameter: TRttiParameter;
  LParamIdx: integer;
  LParamValue: TValue;
begin
  if (length(AMethod.GetParameters) = 1) and (AMethod.GetParameters[0].ParamType.TypeKind = tkDynArray) then
  begin
    setlength(result, 1);
    result[0] := TValue.From(AArgs);
    exit;
  end;
  setlength(result, length(AArgs));
  LParamIdx := 0;
  for LParameter in AMethod.GetParameters do
  begin
    LParamValue := CastArg(AArgs[LParamIdx], LParameter.ParamType);
    if LParamValue.IsType<TValue> then
      LParamValue := LParamValue.AsType<TValue>();
    result[LParamIdx] := LParamValue;
    inc(LParamIdx);
  end;
end;

function TEvaluationTemplateVisitor.Invoke(AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>): TValue;
var
  LMethod: TRttiMethod;
begin
  // there should always be at least one element
  LMethod := AFuncCall.FunctionInfo[0];
  if length(AFuncCall.FunctionInfo) > 1 then
  begin
    for LMethod in AFuncCall.FunctionInfo do
      if length(LMethod.GetParameters) = length(AArgs) then
        break;
  end;
  if length(LMethod.GetParameters) = 0 then
    result := LMethod.Invoke(nil, [])
  else
    result := LMethod.Invoke(nil, GetArgs(LMethod, AArgs));
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
  LObjType: TRttiType;
begin
  if AExpr.RttiMethod = nil then
  begin
    LObjType := GRttiContext.GetType(AObject.typeinfo);
    AExpr.RttiMethod := LObjType.GetMethod(AExpr.Method);
  end;
  exit(AExpr.RttiMethod.Invoke(AObject, AArgs));
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IMethodCallExpr);
var
  LObj: TValue;
  LArgs: TArray<TValue>;
begin
  acceptvisitor(AExpr.ObjectExpr, self);
  LObj := FEvalStack.pop;
  LArgs := ExprListArgs(AExpr.exprlist);
  try
    FEvalStack.push(Invoke(AExpr, LObj, LArgs));
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
  LPrevNewLineState: boolean;
  LSWriter: TNewLineStreamWriter;
begin
  if FIsNLStreamWriter then
  begin
    LSWriter := FStreamWriter as TNewLineStreamWriter;
    LPrevNewLineState := LSWriter.IgnoreNewLine;
    try
      LSWriter.IgnoreNewLine := not AStmt.AllowNewLine;
      acceptvisitor(AStmt.Container, self);
    finally
      LSWriter.IgnoreNewLine := LPrevNewLineState;
    end;
  end
  else
  begin
    acceptvisitor(AStmt.Container, self);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IDefineTemplateStmt);
var
  LTemplateName: TValue;
begin
  acceptvisitor(AStmt.Name, self);
  LTemplateName := FEvalStack.pop;
  AssertString(Position(AStmt), LTemplateName);
  FLocalTemplates.AddOrSetValue(AsString(LTemplateName), AStmt.Container);
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IWithStmt);

var
  LStackFrame: TStackFrame;

procedure ScanClass(const ARttiType: TRttiType; const AClass: TValue); forward;
procedure ScanRecord(const ARttiType: TRttiType; const ARecord: TValue); forward;

  procedure ScanValue(const ARecord: TValue);
  var
    LRttiType: TRttiType;
  begin
    LRttiType := GRttiContext.GetType(ARecord.typeinfo);
    case LRttiType.TypeKind of
      tkClass, tkClassRef:
        ScanClass(LRttiType, ARecord);
      tkrecord:
        ScanRecord(LRttiType, ARecord);
    else
      raise ETemplateEval.Create(SStackFrameCanOnlyBeDefinedOnAClassOrRecord);
    end;
  end;

  procedure ScanClass(const ARttiType: TRttiType; const AClass: TValue);
  var
    LField: TRttiField;
    LProperty: TRttiProperty;
    LObj: TObject;
  begin
    LObj := AClass.AsObject;
    if PopulateStackFrame(LStackFrame, ARttiType, AClass) then
      exit;
    // we can't do anything on generic collections. we can loop using _ if we need to do anything.
    if LObj.ClassType.QualifiedClassName.StartsWith('System.Generics.Collections') then
      exit;
    for LField in ARttiType.GetFields do
      LStackFrame[LField.Name] := LField.GetValue(LObj);
    for LProperty in ARttiType.GetProperties do
      LStackFrame[LProperty.Name] := LProperty.GetValue(LObj);
  end;

  procedure ScanRecord(const ARttiType: TRttiType; const ARecord: TValue);
  var
    LField: TRttiField;
    LRecordPtr: pointer;
  begin
    LRecordPtr := ARecord.GetReferenceToRawData;
    for LField in ARttiType.GetFields do
      LStackFrame[LField.Name] := LField.GetValue(LRecordPtr);
  end;

var
  LExpr: TValue;
begin
  acceptvisitor(AStmt.Expr, self);
  LExpr := FEvalStack.pop;
  LStackFrame := FStackFrames.peek.Clone;
  FStackFrames.push(LStackFrame);

  ScanValue(LExpr);
  acceptvisitor(AStmt.Container, self);

  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.Visit(AStmt: IRequireStmt);
var
  LExprs: TArray<TValue>;
  LInputType: string;
  LIndex: integer;
begin
  LInputType := GRttiContext.GetType(FStackFrames.peek['_'].typeinfo).Name.ToLower;
  LExprs := ExprListArgs(AStmt.exprlist);
  if length(LExprs) = 0 then
    exit;
  for LIndex := 0 to high(LExprs) do
  begin
    if AsString(LExprs[LIndex]).ToLower = LInputType then
      exit;
  end;
  RaiseError(Position(AStmt), SInputOfRequiredTypeNotFound);
end;

procedure TEvaluationTemplateVisitor.Visit(AExpr: IArrayExpr);
begin
  FEvalStack.push(TValue.From < TArray < TValue >> (ExprListArgs(AExpr.exprlist)));
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
  FStartOfLine := true;
  FLastChar := #0;
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

procedure TNewLineStreamWriter.TrimLast;
begin
  if (FBuffer.length >= 1) then
  begin
    FBuffer.length := FBuffer.length - 1;
    if FBuffer.length = 0 then
      FLastChar := #0
    else
      FLastChar := FBuffer.Chars[FBuffer.length - 1];
    FStartOfLine := FBuffer.length = 0;
  end;
end;

procedure TNewLineStreamWriter.Write(const AString: string);
var
  LChar: char;
  LTrimLines: boolean;
  LStripRecurringNL: boolean;
  LIgnoreNewline: boolean;
  LIdx: integer;

begin
  LTrimLines := eoTrimLines in FOptions;
  LStripRecurringNL := eoStripRecurringNewlines in FOptions;
  LIgnoreNewline := FIgnoreNewline;
  for LIdx := 1 to length(AString) do
  begin
    LChar := AString[LIdx];
    if IsWhitespace(LChar) and FStartOfLine and LTrimLines then
    begin
      FLastChar := LChar;
      continue;
    end;
    if IsNewline(LChar) then
    begin
      if IsCR(FLastChar) then
      begin
        TrimLast;
      end;
      if LTrimLines then
      begin
        TrimEndOfLine;
      end;
      if not LIgnoreNewline then
      begin
        if not LStripRecurringNL or not IsNewline(FLastChar) then
        begin
          FBuffer.Append(FNL);
          FStartOfLine := true;
        end;
      end;
    end
    else
    begin
      FBuffer.Append(LChar);
      FStartOfLine := false;
    end;
    FLastChar := LChar;
  end;
end;

end.
