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
 * Copyright (c) 2019-2023 Sempare Limited                                                          *
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

{$I 'Sempare.Template.Compiler.inc'}

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
    FTrimLines: boolean;
    FStripRecurringNL: boolean;
    FRemoveEmpty: boolean;
    FLastNL: boolean;
    procedure TrimEndOfLine;
    procedure TrimLast;
    procedure DoWrite();
  public
    constructor Create(const AStream: TStream; const AEncoding: TEncoding; const ANL: string; const AOptions: TTemplateEvaluationOptions);
    destructor Destroy; override;
    procedure Write(const AString: string); override;
    property IgnoreNewLine: boolean read FIgnoreNewline write FIgnoreNewline;
  end;

  TEvaluationTemplateVisitor = class(TBaseTemplateVisitor, IEvaluationTemplateVisitor)
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
    procedure CheckRunTime(const APosition: IPosition);
    function ExprListArgs(const AExprList: IExprList): TArray<TValue>;
    function Invoke(const AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue; overload;
    function Invoke(const AExpr: IMethodCallExpr; const AObject: TValue; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue; overload;
    function EvalExpr(const AExpr: IExpr): TValue;
    function EvalExprAsString(const AExpr: IExpr): string;
    function EvalExprAsInt(const AExpr: IExpr): int64;
    function EvalExprAsNum(const AExpr: IExpr): extended;
    function EvalExprAsBoolean(const AExpr: IExpr): boolean;
    procedure VisitStmt(const AStmt: IStmt);
    procedure VisitContainer(const AContainer: ITemplate);
    function ResolveTemplate(const AExpr: IExpr): ITemplate; overload;
    function ResolveTemplate(const APosition: IPosition; const AName: string): ITemplate; overload;
  public
    constructor Create(const AContext: ITemplateContext; const AValue: TValue; const AStream: TStream); overload;
    constructor Create(const AContext: ITemplateContext; const AStackFrame: TStackFrame; const AStream: TStream); overload;
    destructor Destroy; override;

    procedure Visit(const AExpr: IBinopExpr); overload; override;
    procedure Visit(const AExpr: IUnaryExpr); overload; override;
    procedure Visit(const AExpr: IVariableExpr); overload; override;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(const AExpr: IValueExpr); overload; override;
    procedure Visit(const AExprList: IExprList); overload; override;
    procedure Visit(const AExpr: ITernaryExpr); overload; override;
    procedure Visit(const AExpr: IEncodeExpr); overload; override;
    procedure Visit(const AExpr: IFunctionCallExpr); overload; override;
    procedure Visit(const AExpr: IMethodCallExpr); overload; override;
    procedure Visit(const AExpr: IArrayExpr); overload; override;

    procedure Visit(const AStmt: IAssignStmt); overload; override;
    procedure Visit(const AStmt: IContinueStmt); overload; override;
    procedure Visit(const AStmt: IBreakStmt); overload; override;
    procedure Visit(const AStmt: IEndStmt); overload; override;
    procedure Visit(const AStmt: IIncludeStmt); overload; override;
    procedure Visit(const AStmt: IRequireStmt); overload; override;

    procedure Visit(const AStmt: IPrintStmt); overload; override;
    procedure Visit(const AStmt: IIfStmt); overload; override;
    procedure Visit(const AStmt: IWhileStmt); overload; override;
    procedure Visit(const AStmt: IForInStmt); overload; override;
    procedure Visit(const AStmt: IForRangeStmt); overload; override;

    procedure Visit(const AStmt: IProcessTemplateStmt); overload; override;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload; override;
    procedure Visit(const AStmt: IWithStmt); overload; override;
    procedure Visit(const AStmt: ICycleStmt); overload; override;
    procedure Visit(const AStmt: IDebugStmt); overload; override;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;

  end;

implementation

uses
  Data.DB,
  System.TypInfo, // needed for XE6 and below to access the TTypeKind variables
  Sempare.Template.BlockResolver,
  Sempare.Template.BlockReplacer,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Rtti,
  Sempare.Template.Util;

const
  LOOP_IDX_NAME: string = '_loop_idx_';

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

constructor TEvaluationTemplateVisitor.Create(const AContext: ITemplateContext; const AValue: TValue; const AStream: TStream);
begin
  Create(AContext, TStackFrame.Create(AValue, nil), AStream);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IValueExpr);
var
  LValue: TValue;
begin
  LValue := AExpr.Value;
  FEvalStack.push(LValue);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExprList: IExprList);
var
  LIdx: integer;
begin
  for LIdx := AExprList.Count - 1 downto 0 do
  begin
    AcceptVisitor(AExprList.Expr[LIdx], self);
  end;
  // push count onto stack
  FEvalStack.push(AExprList.Count);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IContinueStmt);
begin
  include(FLoopOptions, TLoopOption.coContinue);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IVariableDerefExpr);
var
  LDerefKey: TValue;
  LDerefObj: TValue;
  LDerefedValue: TValue;
  LAllowRootDeref: IPreserveValue<boolean>;
begin
  LAllowRootDeref := Preserve.Value<boolean>(FAllowRootDeref, true);

  LDerefObj := EvalExpr(AExpr.variable);

  LAllowRootDeref.SetValue(AExpr.DerefType = dtArray);
  LDerefKey := EvalExpr(AExpr.DerefExpr);
  LDerefedValue := Deref(Position(AExpr), LDerefObj, LDerefKey, eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext);

  FEvalStack.push(LDerefedValue);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IBinopExpr);
var
  LLeft: TValue;
  LRight: TValue;
  LResult: TValue;
begin
  LLeft := EvalExpr(AExpr.LeftExpr);
  LRight := EvalExpr(AExpr.RightExpr);
  case AExpr.BinOp of
    boIN:
      LResult := contains(Position(AExpr.RightExpr), LLeft, LRight, FContext);
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
        LResult := AsNum(LLeft, FContext) + AsNum(LRight, FContext)
      else if isStrLike(LLeft) and isStrLike(LRight) then
        LResult := LLeft.AsString + LRight.AsString
      else if isStrLike(LLeft) and isNumLike(LRight) then
        LResult := LLeft.AsString + AsString(LRight, FContext)
      else
        RaiseError(Position(AExpr.LeftExpr), SStringOrNumericTypesExpected);
    boMinus:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsNum(LLeft, FContext) - AsNum(LRight, FContext);
      end;
    boSlash:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsNum(LLeft, FContext) / AsNum(LRight, FContext);
      end;
    boDiv:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := trunc(AsNum(LLeft, FContext)) div trunc(AsNum(LRight, FContext));
      end;
    boMult:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsNum(LLeft, FContext) * AsNum(LRight, FContext);
      end;
    boMod:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), LLeft, LRight);
        LResult := AsInt(LLeft, FContext) mod AsInt(LRight, FContext);
      end;
    boEQ:
      LResult := isEqual(LLeft, LRight, FContext);
    boNotEQ:
      LResult := not isEqual(LLeft, LRight, FContext);
    boLT:
      LResult := isLessThan(LLeft, LRight, FContext);
    boLTE:
      LResult := not isGreaterThan(LLeft, LRight, FContext);
    boGT:
      LResult := isGreaterThan(LLeft, LRight, FContext);
    boGTE:
      LResult := not isLessThan(LLeft, LRight, FContext);
  else
    RaiseError(Position(AExpr.LeftExpr), SBinOpNotSupported);
  end;
  FEvalStack.push(LResult);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IUnaryExpr);
var
  LValue: TValue;
begin
  LValue := EvalExpr(AExpr.Expr);
  case AExpr.UnaryOp of
    uoMinus:
      begin
        FEvalStack.push(-AsNum(LValue, FContext));
      end;
    uoNot:
      begin
        FEvalStack.push(not AsBoolean(LValue));
      end
  else
    RaiseError(Position(AExpr), SUnaryOpNotSupported);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IVariableExpr);
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
        LValue := Deref(Position(AExpr), LStackFrame.Root, AExpr.variable, eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext);
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
  FEvalStack.push(LValue);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IBreakStmt);
begin
  include(FLoopOptions, TLoopOption.coBreak);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IWhileStmt);
var
  LLoopOptions: IPreserveValue<TLoopOptions>;
  LOffset: int64;
  LLimit: int64;
  i, LLoops: int64;
  LFirst: boolean;
  function GetValue(const AExpr: IExpr): int64;
  begin
    if AExpr = nil then
    begin
      exit(-1);
    end
    else
    begin
      exit(EvalExprAsInt(AExpr));
    end;
  end;

begin
  if HasBreakOrContinue then
    exit;
  LLoopOptions := Preserve.Value<TLoopOptions>(FLoopOptions, []);
  FStackFrames.push(FStackFrames.peek.Clone);

  LOffset := GetValue(AStmt.OffsetExpr);
  LLimit := GetValue(AStmt.LimitExpr);

  i := 0;
  LLoops := 0;
  LFirst := true;

  while ((LLimit = -1) or (LLoops < LLimit)) do
  begin
    if not EvalExprAsBoolean(AStmt.Condition) or (coBreak in FLoopOptions) then
      break;
    CheckRunTime(Position(AStmt));
    if (LOffset = -1) or (i >= LOffset) then
    begin
      if LFirst then
      begin
        LFirst := false;
        if AStmt.OnFirstContainer <> nil then
          AcceptVisitor(AStmt.OnFirstContainer, self);
      end
      else
      begin
        if AStmt.BetweenItemsContainer <> nil then
          AcceptVisitor(AStmt.BetweenItemsContainer, self);
      end;
      exclude(FLoopOptions, coContinue);
      AcceptVisitor(AStmt.Container, self);
      inc(LLoops);
    end;
    inc(i);
  end;
  FStackFrames.pop;
  if LLoops = 0 then
  begin
    if AStmt.OnEmptyContainer <> nil then
      AcceptVisitor(AStmt.OnEmptyContainer, self);
  end
  else
  begin
    if AStmt.OnEndContainer <> nil then
      AcceptVisitor(AStmt.OnEndContainer, self);
  end;
end;

type
  TGetForInValueIndex = function(const ALoopExpr: TValue; const AIndex: integer; const AMin: integer): TValue;

function GetForInValueIndex(const ALoopExpr: TValue; const AIndex: integer; const AMin: integer): TValue;
begin
  exit(AIndex);
end;

function GetForInValueMinPlusIndex(const ALoopExpr: TValue; const AIndex: integer; const AMin: integer): TValue;
begin
  exit(AIndex + AMin);
end;

function GetForInValueLoopExprIndex(const ALoopExpr: TValue; const AIndex: integer; const AMin: integer): TValue;
begin
  exit(ALoopExpr.GetArrayElement(AIndex));
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IForInStmt);
var
  LLoopOptions: IPreserveValue<TLoopOptions>;
  LVariableName: string;
  LLoopExpr: TValue;
  LLoopExprType: TRttiType;
  LOffset: int64;
  LLimit: int64;
  i, LLoops: int64;
  LFirst: boolean;

  function HandleLoop: boolean;
  begin
    if coBreak in FLoopOptions then
      exit(true);
    FStackFrames.peek[LOOP_IDX_NAME] := i;
    exclude(FLoopOptions, coContinue);
    CheckRunTime(Position(AStmt));
    if (LOffset = -1) or (i >= LOffset) then
    begin
      if LFirst then
      begin
        LFirst := false;
        if AStmt.OnFirstContainer <> nil then
          AcceptVisitor(AStmt.OnFirstContainer, self);
      end
      else
      begin
        if AStmt.BetweenItemsContainer <> nil then
          AcceptVisitor(AStmt.BetweenItemsContainer, self);
      end;
      AcceptVisitor(AStmt.Container, self);
      inc(LLoops);
    end;
    inc(i);
    exit(false);
  end;

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

    while not LDataSetEOFProperty.GetValue(LObj).AsBoolean and ((LLimit = -1) or (LLoops < LLimit)) do
    begin
      FStackFrames.peek[LVariableName] := LIdx;
      if HandleLoop then
        break;
      LDataSetNextMethod.Invoke(LObj, []);
      inc(LIdx);
    end;
  end;

  procedure VisitObject;
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
      RaiseErrorRes(Position(AStmt), @SValueIsNotEnumerable);
    LEnumObj := LEnumValue.AsObject;
    try
      LLoopExprType := GRttiContext.GetType(LEnumObj.ClassType);
      LEnumMoveNextMethod := LLoopExprType.GetMethod('MoveNext');
      LEnumCurrentProperty := LLoopExprType.getProperty('Current');
      while LEnumMoveNextMethod.Invoke(LEnumObj, []).AsBoolean and ((LLimit = -1) or (LLoops < LLimit)) do
      begin
        FStackFrames.peek[LVariableName] := LEnumCurrentProperty.GetValue(LEnumObj);
        if HandleLoop then
          break;
      end;
    finally
      LEnumObj.Free;
    end;
  end;

  procedure VisitArray;
  var
    LArrayType: TRttiArrayType;
    LDimOrdType: TRttiOrdinalType;
    LIdx: integer;
    LMin: integer;
    LGetValue: TGetForInValueIndex;
  begin
    LArrayType := LLoopExprType as TRttiArrayType;
    if LArrayType.DimensionCount <> 1 then
      RaiseError(Position(AStmt), SOnlyOneDimensionalArraysAreSupported);
    LDimOrdType := LArrayType.Dimensions[0] as TRttiOrdinalType;
    if LDimOrdType = nil then
      LMin := 0
    else
      LMin := LDimOrdType.MinValue;
    if AStmt.ForOp = foIn then
      LGetValue := GetForInValueMinPlusIndex
    else
      LGetValue := GetForInValueLoopExprIndex;
    LIdx := 0;
    while (LIdx <= LLoopExpr.GetArrayLength - 1) and ((LLimit = -1) or (LLoops < LLimit)) do
    begin
      FStackFrames.peek[LVariableName] := LGetValue(LLoopExpr, LIdx, LMin);
      if HandleLoop then
        break;
      inc(LIdx);
    end;
  end;

  procedure VisitDynArray;
  var
    LIdx: integer;
    LGetValue: TGetForInValueIndex;
  begin
    if AStmt.ForOp = foIn then
      LGetValue := GetForInValueIndex
    else
      LGetValue := GetForInValueLoopExprIndex;
    LIdx := 0;
    while (LIdx <= LLoopExpr.GetArrayLength - 1) and ((LLimit = -1) or (LLoops < LLimit)) do
    begin
      FStackFrames.peek[LVariableName] := LGetValue(LLoopExpr, LIdx, 0);
      if HandleLoop then
        break;
      inc(LIdx);
    end;
  end;

  function GetValue(const AExpr: IExpr): int64;
  begin
    if AExpr = nil then
    begin
      exit(-1);
    end
    else
    begin
      exit(EvalExprAsInt(AExpr));
    end;
  end;

begin
  if HasBreakOrContinue then
    exit;
  LLoopOptions := Preserve.Value<TLoopOptions>(FLoopOptions, []);
  FStackFrames.push(FStackFrames.peek.Clone);
  LVariableName := AStmt.variable;

  LLoopExpr := EvalExpr(AStmt.Expr);
  LOffset := GetValue(AStmt.OffsetExpr);
  LLimit := GetValue(AStmt.LimitExpr);

  i := 0;
  LLoops := 0;
  LFirst := true;
  if not LLoopExpr.IsEmpty then
  begin
    LLoopExprType := GRttiContext.GetType(LLoopExpr.TypeInfo);

    case LLoopExprType.TypeKind of
      tkClass, tkClassRef:
        VisitObject;
      tkArray:
        VisitArray;
      tkDynArray:
        VisitDynArray;
    else
      RaiseError(Position(AStmt), SGetEnumeratorNotFoundOnObject);
    end;
  end;
  FStackFrames.pop;
  if LLoops = 0 then
  begin
    if AStmt.OnEmptyContainer <> nil then
      AcceptVisitor(AStmt.OnEmptyContainer, self);
  end
  else
  begin
    if AStmt.OnEndContainer <> nil then
      AcceptVisitor(AStmt.OnEndContainer, self);
  end;
end;

procedure TEvaluationTemplateVisitor.CheckRunTime(const APosition: IPosition);
begin
  if FStopWatch.ElapsedMilliseconds > FContext.MaxRunTimeMs then
    RaiseError(APosition, SMaxRuntimeOfMsHasBeenExceeded, [FContext.MaxRunTimeMs]);
end;

constructor TEvaluationTemplateVisitor.Create(const AContext: ITemplateContext; const AStackFrame: TStackFrame; const AStream: TStream);
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
  exit(FContext.VariableEncoder(AsString(AValue, FContext)));
end;

function TEvaluationTemplateVisitor.EvalExprAsBoolean(const AExpr: IExpr): boolean;
begin
  exit(AsBoolean(EvalExpr(AExpr)));
end;

function TEvaluationTemplateVisitor.EvalExpr(const AExpr: IExpr): TValue;
begin
  AcceptVisitor(AExpr, self);
  exit(FEvalStack.pop);
end;

function TEvaluationTemplateVisitor.EvalExprAsInt(const AExpr: IExpr): int64;
begin
  exit(AsInt(EvalExpr(AExpr), FContext));
end;

function TEvaluationTemplateVisitor.EvalExprAsNum(const AExpr: IExpr): extended;
begin
  exit(AsNum(EvalExpr(AExpr), FContext));
end;

function TEvaluationTemplateVisitor.EvalExprAsString(const AExpr: IExpr): string;
begin
  exit(AsString(EvalExpr(AExpr), FContext));
end;

function TEvaluationTemplateVisitor.ExprListArgs(const AExprList: IExprList): TArray<TValue>;
var
  LIdx: integer;
  LCount: integer;
begin
  AExprList.Accept(self);
  LCount := AsInt(FEvalStack.pop(), FContext);
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

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IForRangeStmt);
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
  i: int64;
  LStep: int64;
  LFirst: boolean;
  function GetValue(const AExpr: IExpr): int64;
  begin
    if AExpr = nil then
    begin
      exit(-1);
    end
    else
    begin
      exit(EvalExprAsInt(AExpr));
    end;
  end;

begin
  if HasBreakOrContinue then
    exit;
  LVariable := AStmt.variable;
  LLoopOptions := Preserve.Value<TLoopOptions>(FLoopOptions, []);
  LStartVal := EvalExprAsInt(AStmt.LowExpr);
  LEndVal := EvalExprAsInt(AStmt.HighExpr);

  LStep := GetValue(AStmt.StepExpr);
  LFirst := true;
  LDelta := 0;
  LDirectionTestFunc := nil;
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
    begin
      RaiseErrorRes(Position(AStmt), @STypeNotSupported);
    end;
  end;
  if LStep <> -1 then
  begin
    LDelta := LDelta * abs(LStep);
  end;
  FStackFrames.push(FStackFrames.peek.Clone);
  LIdx := LStartVal;
  i := 0;
  while LDirectionTestFunc(LIdx, LEndVal) do
  begin
    FStackFrames.peek[LVariable] := LIdx;
    FStackFrames.peek[LOOP_IDX_NAME] := i;
    if coBreak in FLoopOptions then
      break;
    if LFirst then
    begin
      LFirst := false;
      if AStmt.OnFirstContainer <> nil then
        AcceptVisitor(AStmt.OnFirstContainer, self);
    end
    else
    begin
      if AStmt.BetweenItemsContainer <> nil then
        AcceptVisitor(AStmt.BetweenItemsContainer, self);
    end;
    exclude(FLoopOptions, coContinue);
    CheckRunTime(Position(AStmt));
    AcceptVisitor(AStmt.Container, self);
    inc(LIdx, LDelta);
    inc(i);
  end;
  FStackFrames.pop;
  if i = 0 then
  begin
    if AStmt.OnEmptyContainer <> nil then
      AcceptVisitor(AStmt.OnEmptyContainer, self);
  end
  else
  begin
    if AStmt.OnEndContainer <> nil then
      AcceptVisitor(AStmt.OnEndContainer, self);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IAssignStmt);
var
  LVariable: string;
  LValue: TValue;
begin
  LVariable := AStmt.variable;
  LValue := EvalExpr(AStmt.Expr);
  FStackFrames.peek[LVariable] := LValue;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IIfStmt);
var
  LExpr: TValue;
  LIsObject: boolean;
begin
  if HasBreakOrContinue then
    exit;
  LExpr := EvalExpr(AStmt.Condition);
  LIsObject := LExpr.IsObject;
  if LIsObject and not IsEmptyObject(LExpr.AsObject) or not LIsObject and AsBoolean(LExpr) then
    AcceptVisitor(AStmt.TrueContainer, self)
  else if AStmt.FalseContainer <> nil then
    AcceptVisitor(AStmt.FalseContainer, self);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IEndStmt);
begin
  // nothing to do
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IIncludeStmt);
var
  LTemplateName: string;
  LTemplate: ITemplate;
begin
  if HasBreakOrContinue then
    exit;
  LTemplateName := EvalExprAsString(AStmt.Expr);
  LTemplate := ResolveTemplate(Position(AStmt.Expr), LTemplateName);
  if assigned(LTemplate) then
  begin
    FStackFrames.push(FStackFrames.peek.Clone());
    try
      Visit(LTemplate);
    finally
      FStackFrames.pop;
    end;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IPrintStmt);
begin
  if HasBreakOrContinue then
    exit;
  FStreamWriter.Write(EvalExprAsString(AStmt.Expr));
end;

function CastArg(const AValue: TValue; const AType: TRttiType; const AContext: ITemplateContext): TValue;
begin
  case AType.TypeKind of
    tkInteger, tkInt64:
      if not(AValue.Kind in [tkInteger, tkInt64]) then
        exit(AsInt(AValue, AContext));
    tkFloat:
      if AValue.Kind <> tkFloat then
        exit(AsNum(AValue, AContext));
    tkString, tkWString, tkLString, tkUString:
      if not(AValue.Kind in [tkString, tkWString, tkLString, tkUString]) then
        exit(AsString(AValue, AContext));
    tkEnumeration:
      if AType.Handle = TypeInfo(boolean) then
        exit(AsBoolean(AValue));
  end;
  exit(AValue);
end;

function GetArgs(const APosition: IPosition; const AMethod: TRttiMethod; const AArgs: TArray<TValue>; const AContext: ITemplateContext): TArray<TValue>;
var
  LParameter: TRttiParameter;
  LParamIdx: integer;
  LParamValue: TValue;
  LOffset: integer;
  LNumParams: integer;
  LParams: TArray<TRttiParameter>;
begin
  LParams := AMethod.GetParameters;
  LNumParams := length(LParams);
  LOffset := 0;
  setlength(result, LNumParams);
  if (LNumParams > 0) and (LParams[0].ParamType.Handle = TypeInfo(ITemplateContext)) then
  begin
    result[0] := TValue.From<ITemplateContext>(AContext);
    LOffset := 1;
  end;
  if (LNumParams > 0) and (LParams[LNumParams - 1].ParamType.TypeKind = tkDynArray) then
  begin
    if LNumParams > 2 then
      RaiseErrorRes(APosition, @STooManyParameters);
    result[LOffset] := TValue.From(AArgs);
    exit;
  end;
  for LParamIdx := LOffset to LNumParams - 1 do
  begin
    LParameter := LParams[LParamIdx];
    LParamValue := CastArg(AArgs[LParamIdx - LOffset], LParameter.ParamType, AContext);
    result[LParamIdx] := LParamValue;
  end;
end;

function TEvaluationTemplateVisitor.Invoke(const AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue;
var
  LMethod: TRttiMethod;

  function GetParamCount: integer;
  begin
    result := length(LMethod.GetParameters);
    if result > 0 then
    begin
      if LMethod.GetParameters[0].ParamType.Handle = TypeInfo(ITemplateContext) then
        dec(result);
    end;
  end;

begin
  // there should always be at least one element
  LMethod := AFuncCall.FunctionInfo[0];
  if length(AFuncCall.FunctionInfo) > 1 then
  begin
    for LMethod in AFuncCall.FunctionInfo do
      if GetParamCount = length(AArgs) then
        break;
  end;
  AHasResult := LMethod.ReturnType <> nil;
  if length(LMethod.GetParameters) = 0 then
    result := LMethod.Invoke(nil, [])
  else
    result := LMethod.Invoke(nil, GetArgs(Position(AFuncCall), LMethod, AArgs, FContext));
  if result.IsType<TValue> then
    result := result.AsType<TValue>();
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IFunctionCallExpr);
var
  LResult: TValue;
  LHasResult: boolean;
begin
  try
    LResult := Invoke(AExpr, ExprListArgs(AExpr.exprlist), LHasResult);
    if not LHasResult then
      LResult := '';
    FEvalStack.push(LResult);
  except
    on e: exception do
      RaiseError(Position(AExpr), e.Message);
  end;
end;

function TEvaluationTemplateVisitor.Invoke(const AExpr: IMethodCallExpr; const AObject: TValue; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue;
var
  LObjType: TRttiType;
begin
  if AExpr.RttiMethod = nil then
  begin
    LObjType := GRttiContext.GetType(AObject.TypeInfo);
    AExpr.RttiMethod := LObjType.GetMethod(AExpr.Method);
  end;

  AHasResult := AExpr.RttiMethod.ReturnType <> nil;
  exit(AExpr.RttiMethod.Invoke(AObject, AArgs));
end;

function TEvaluationTemplateVisitor.ResolveTemplate(const APosition: IPosition; const AName: string): ITemplate;
begin
  if not FLocalTemplates.TryGetValue(AName, result) and not FContext.TryGetTemplate(AName, result) then
  begin
    RaiseErrorRes(APosition, @STemplateNotFound, [AName]);
  end;
end;

function TEvaluationTemplateVisitor.ResolveTemplate(const AExpr: IExpr): ITemplate;
begin
  exit(ResolveTemplate(Position(AExpr), EvalExprAsString(AExpr)));
end;

procedure TEvaluationTemplateVisitor.VisitStmt(const AStmt: IStmt);
begin
  AcceptVisitor(AStmt, self);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IMethodCallExpr);
var
  LObj: TValue;
  LArgs: TArray<TValue>;
  LResult: TValue;
  LHasResult: boolean;
begin
  AcceptVisitor(AExpr.ObjectExpr, self);
  LObj := FEvalStack.pop;
  LArgs := ExprListArgs(AExpr.exprlist);
  try
    LResult := Invoke(AExpr, LObj, LArgs, LHasResult);
    if not LHasResult then
      LResult := '';
    FEvalStack.push(LResult);
  except
    on e: exception do
      RaiseError(Position(AExpr), AExpr.Method + ':' + e.Message);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IEncodeExpr);
begin
  FEvalStack.push(EncodeVariable(EvalExpr(AExpr.Expr)));
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IProcessTemplateStmt);
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
      AcceptVisitor(AStmt.Container, self);
    finally
      LSWriter.IgnoreNewLine := LPrevNewLineState;
    end;
  end
  else
  begin
    AcceptVisitor(AStmt.Container, self);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin
  FLocalTemplates.AddOrSetValue(EvalExprAsString(AStmt.Name), AStmt.Container);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IWithStmt);
var
  LStackFrame: TStackFrame;
begin

  LStackFrame := FStackFrames.peek.Clone(EvalExpr(AStmt.Expr));
  FStackFrames.push(LStackFrame);

  AcceptVisitor(AStmt.Container, self);

  FStackFrames.pop;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IRequireStmt);
var
  LExprs: TArray<TValue>;
  LInputType: string;
  LIndex: integer;
begin
  LInputType := GRttiContext.GetType(FStackFrames.peek['_'].TypeInfo).Name.ToLower;
  LExprs := ExprListArgs(AStmt.exprlist);
  if length(LExprs) = 0 then
    exit;
  for LIndex := 0 to high(LExprs) do
  begin
    if AsString(LExprs[LIndex], FContext).ToLower = LInputType then
      exit;
  end;
  RaiseError(Position(AStmt), SInputOfRequiredTypeNotFound);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IArrayExpr);
begin
  FEvalStack.push(TValue.From < TArray < TValue >> (ExprListArgs(AExpr.exprlist)));
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: ITernaryExpr);
begin
  if EvalExprAsBoolean(AExpr.Condition) then
    AcceptVisitor(AExpr.TrueExpr, self)
  else
    AcceptVisitor(AExpr.FalseExpr, self);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: ICycleStmt);
var
  LStackFrame: TStackFrame;
  LValue: TValue;
begin
  try
    LStackFrame := FStackFrames.peek;
    LValue := LStackFrame[LOOP_IDX_NAME];
  except
    RaiseErrorRes(Position(AStmt), @SCycleStatementMustBeInALoop);
  end;
  FStreamWriter.Write(EvalExprAsString(AStmt.List[LValue.AsInt64 mod AStmt.List.Count]));
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IDebugStmt);
begin
  try
    AcceptVisitor(AStmt.Stmt, self);
  except
    on e: exception do
    begin
      FStreamWriter.Write(Format(FContext.DebugErrorFormat, [e.Message]));
    end;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IBlockStmt);
begin
  if assigned(AStmt.Container) then
    AcceptVisitor(AStmt.Container, self);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IExtendsStmt);

  function CommonNames(const ANames: TArray<string>; const BNames: TArray<string>): TArray<string>;
  var
    a, b: string;
  begin
    result := nil;
    // not mega optimal, but lists should be sort
    for a in ANames do
    begin
      for b in BNames do
      begin
        if a = b then
          insert(a, result, length(result));
      end;
    end;
  end;

var
  LName: string;
  LSymbol: ITemplateSymbol;
  LBlockResolver: IBlockResolverVisitor;
  LBlockReplacer: IBlockReplacerVisitor;
  LBlockName: string;
  LBlockNames: TArray<string>;
  LTemplateNames: TArray<string>;
  LReplacementBlocks: TArray<IBlockStmt>;
  LTemplate: ITemplate;
  LBlock: IBlockStmt;
  LResolveNames: boolean;
begin
  LResolveNames := false;
  if not assigned(AStmt.Container) then
  begin
    LName := AStmt.NameAsString(self);
    LTemplate := ResolveTemplate(Position(AStmt), LName);

    AStmt.Container := LTemplate;
    LResolveNames := true;

  end
  else
  begin
    LTemplate := AStmt.Container;
  end;

  // we need to clone the original as we do replacements
  // not currently planning to support 'inherited', to allow to reference the original body in the original template from within the child body.
  LTemplate := CloneTemplate(LTemplate);

  if LResolveNames then
  begin
    LBlockResolver := TBlockResolverVisitor.Create(self, AStmt.Container);
    LTemplateNames := LBlockResolver.GetBlockNames();

    LBlockResolver := TBlockResolverVisitor.Create(self, AStmt.BlockContainer);
    LBlockNames := LBlockResolver.GetBlockNames();
    AStmt.BlockNames := CommonNames(LTemplateNames, LBlockNames);
  end;

  // we resolve the blocks aimed for replacing defined withing the block container.
  LBlockReplacer := TBlockReplacerVisitor.Create(self);
  for LBlockName in AStmt.BlockNames do
  begin
    LReplacementBlocks := LBlockResolver.GetBlocks(LBlockName);
    for LBlock in LReplacementBlocks do
    begin
      LBlockReplacer.Replace(LTemplate, LBlockName, LBlock.Container);
    end;
  end;

  AcceptVisitor(LTemplate, self);
end;

procedure TEvaluationTemplateVisitor.VisitContainer(const AContainer: ITemplate);
begin
  AcceptVisitor(AContainer, self);
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
  FTrimLines := eoTrimLines in FOptions;
  FRemoveEmpty := eoStripEmptyLines in FOptions;
  FStripRecurringNL := eoStripRecurringNewlines in FOptions;
  FLastNL := true;
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
  LIdx: integer;
begin
  for LIdx := 1 to length(AString) do
  begin
    LChar := AString[LIdx];
    if IsWhitespace(LChar) and FStartOfLine and FTrimLines then
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
      if FTrimLines then
      begin
        TrimEndOfLine;
      end;

      if not FIgnoreNewline then
      begin
        if not FStripRecurringNL or not IsNewline(FLastChar) then
        begin
          if not FRemoveEmpty or not FLastNL then
          begin
            FBuffer.Append(FNL);
            FLastNL := true;
          end;
          FStartOfLine := true;
        end;
      end;
    end
    else
    begin
      FBuffer.Append(LChar);
      FLastNL := false;
      FStartOfLine := false;
    end;
    FLastChar := LChar;
  end;
end;

end.
