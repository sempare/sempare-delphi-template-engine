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
 * Copyright (c) 2019-2024 Sempare Limited                                                          *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the Apache Version 2.0 or the Sempare Commercial License                          *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.apache.org/licenses/LICENSE-2.0                                                      *
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
  Sempare.Template.Context,
  Sempare.Template.Visitor,
  Sempare.Template.Util;

type
  ETemplateEval = class(ETemplate);

  TLoopOption = (coContinue, coBreak);
  TLoopOptions = set of TLoopOption;

  TEvaluationTemplateVisitor = class(TBaseTemplateVisitor, IEvaluationTemplateVisitor)
  private
    FStopWatch: TStopWatch;
    FStackFrames: TObjectStack<TStackFrame>;
    FEvalStack: TStack<TValue>;
    FStream: TStream;
    FStreamWriter: TTemplateStreamWriter;
    FLoopOptions: TLoopOptions;
    FContext: ITemplateContext;
    FEvaluationContext: ITemplateEvaluationContext;
    FAllowRootDeref: boolean;
    FLocalTemplates: TDictionary<string, ITemplate>;
    FResolveContext: TTemplateValue;
    FTemplate: ITemplate;

    function HasBreakOrContinue: boolean; inline;
    function EncodeVariable(const AValue: TValue): TValue;
    procedure CheckRunTime(const APosition: IPosition);
    function ExprListArgs(const AExprList: IExprList): TArray<TValue>;
    function GetArgs(const APosition: IPosition; const AMethod: TRttiMethod; const AArgs: TArray<TValue>; const AContext: ITemplateContext): TArray<TValue>;
    function DoInvoke(const APosition: IPosition; const AMethod: TRttiMethod; const AObject: TValue; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue; overload;
    function Invoke(const AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue; overload;
    function Invoke(const AExpr: IMethodCallExpr; const AObject: TValue; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue; overload;
    function EvalExpr(const AExpr: IExpr): TValue;
    function EvalExprAsString(const AExpr: IExpr): string;
    function EvalExprAsInt(const AExpr: IExpr): int64;
    function EvalExprAsNum(const AExpr: IExpr): extended;
    function EvalExprAsBoolean(const AExpr: IExpr): boolean;
    procedure VisitStmt(const AStmt: IStmt);
    function ResolveTemplate(const AExpr: IExpr): ITemplate; overload;
    function ResolveTemplate(const APosition: IPosition; const AName: string): ITemplate; overload;
  public
    constructor Create(const AContext: ITemplateContext; const AResolveContext: TTemplateValue; const AValue: TValue; const AStream: TStream; const ATemplate: ITemplate); overload;
    constructor Create(const AContext: ITemplateContext; const AResolveContext: TTemplateValue; const AStackFrame: TStackFrame; const AStream: TStream; const ATemplate: ITemplate); overload;
    destructor Destroy; override;
    procedure Visit(const AExpr: IBinopExpr); overload; override;
    procedure Visit(const AExpr: IUnaryExpr); overload; override;
    procedure Visit(const AExpr: IVariableExpr); overload; override;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(const AExpr: IValueExpr); overload; override;
    procedure Visit(const AExpr: IMapExpr); overload; override;
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
    procedure Visit(const AStmt: ICompositeStmt); overload; override;
    procedure Visit(const AStmt: IStripStmt); overload; override;
    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;
    procedure Visit(const AStmt: IIgnoreNLStmt); overload; override;
    procedure Visit(const AStmt: IIgnoreWSStmt); overload; override;
  end;

implementation

uses
  Data.DB,
  System.TypInfo, // needed for XE6 and below to access the TTypeKind variables
  Sempare.Template.BlockResolver,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Rtti;

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

constructor TEvaluationTemplateVisitor.Create(const AContext: ITemplateContext; const AResolveContext: TTemplateValue; const AValue: TValue; const AStream: TStream; const ATemplate: ITemplate);
begin
  Create(AContext, AResolveContext, TStackFrame.Create(AValue, nil), AStream, ATemplate);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IValueExpr);
var
  LValue: TValue;
begin
  LValue := AExpr.Value;
  if LValue.IsType<TValue> then
    LValue := LValue.AsType<TValue>;
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
  LAllowRootDeref: boolean;
begin
  LAllowRootDeref := FAllowRootDeref;
  FAllowRootDeref := true;
  try
    LDerefObj := EvalExpr(AExpr.variable);

    FAllowRootDeref := AExpr.DerefType = dtArray;
    LDerefKey := EvalExpr(AExpr.DerefExpr);
    LDerefedValue := Deref(AExpr, LDerefObj, LDerefKey, eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext);
    if LDerefedValue.IsType<TValue> then
      LDerefedValue := LDerefedValue.AsType<TValue>();
    FEvalStack.push(LDerefedValue);
  finally
    FAllowRootDeref := LAllowRootDeref;
  end;
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
      LResult := contains(AExpr.RightExpr, LLeft, LRight, FContext);
    boAND:
      begin
        AssertBoolean(AExpr.LeftExpr, LLeft, LRight);
        LResult := AsBoolean(LLeft) and AsBoolean(LRight);
      end;
    boOR:
      begin
        AssertBoolean(AExpr.LeftExpr, LLeft, LRight);
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
        RaiseError(AExpr.LeftExpr, SStringOrNumericTypesExpected);
    boMinus:
      begin
        AssertNumeric(AExpr.LeftExpr, LLeft, LRight);
        LResult := AsNum(LLeft, FContext) - AsNum(LRight, FContext);
      end;
    boSlash:
      begin
        AssertNumeric(AExpr.LeftExpr, LLeft, LRight);
        LResult := AsNum(LLeft, FContext) / AsNum(LRight, FContext);
      end;
    boDiv:
      begin
        AssertNumeric(AExpr.LeftExpr, LLeft, LRight);
        LResult := trunc(AsNum(LLeft, FContext)) div trunc(AsNum(LRight, FContext));
      end;
    boMult:
      begin
        AssertNumeric(AExpr.LeftExpr, LLeft, LRight);
        LResult := AsNum(LLeft, FContext) * AsNum(LRight, FContext);
      end;
    boMod:
      begin
        AssertNumeric(AExpr.LeftExpr, LLeft, LRight);
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
    RaiseError(AExpr.LeftExpr, SBinOpNotSupported);
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
    RaiseError(AExpr, SUnaryOpNotSupported);
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
        if not TryDeref(AExpr, LStackFrame.Root, AExpr.variable, eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext, LValue) then
          FContext.TryGetVariable(AExpr.variable, LValue);
      except
        on e: exception do
        begin
          if (eoRaiseErrorWhenVariableNotFound in FContext.Options) then
            RaiseError(AExpr, SCannotFindValiable, [AExpr.variable]);
        end;
      end;
    end;
    if LValue.IsEmpty and (eoRaiseErrorWhenVariableNotFound in FContext.Options) then
      RaiseError(AExpr, SCannotFindValiable, [AExpr.variable]);
  end
  else
  begin
    LValue := AExpr.variable;
  end;
  if LValue.IsType<TValue> then
    LValue := LValue.AsType<TValue>();
  FEvalStack.push(LValue);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IBreakStmt);
begin
  include(FLoopOptions, TLoopOption.coBreak);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IWhileStmt);
var
  LLoopOptions: TLoopOptions;
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
  LLoopOptions := FLoopOptions;
  FLoopOptions := [];
  try
    FStackFrames.push(FStackFrames.peek.Clone);

    LOffset := GetValue(AStmt.OffsetExpr);
    LLimit := GetValue(AStmt.LimitExpr);

    i := 0;
    LLoops := 0;
    LFirst := true;
    try
      while ((LLimit = -1) or (LLoops < LLimit)) do
      begin
        if not EvalExprAsBoolean(AStmt.Condition) or (coBreak in FLoopOptions) then
          break;
        CheckRunTime(AStmt);
        FStackFrames.peek[LOOP_IDX_NAME] := i;
        if (LOffset = -1) or (i >= LOffset) then
        begin
          if LFirst then
          begin
            LFirst := false;
            if AStmt.OnBeginContainer <> nil then
              AcceptVisitor(AStmt.OnBeginContainer, self);
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
    finally
      FStackFrames.pop;
    end;
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
  finally
    FLoopOptions := LLoopOptions;
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
  result := ALoopExpr.GetArrayElement(AIndex);
  if result.IsType<TValue> then
    result := result.AsType<TValue>;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IForInStmt);
var
  LLoopOptions: TLoopOptions;
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
    CheckRunTime(AStmt);
    if (LOffset = -1) or (i >= LOffset) then
    begin
      if LFirst then
      begin
        LFirst := false;
        if AStmt.OnBeginContainer <> nil then
          AcceptVisitor(AStmt.OnBeginContainer, self);
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
    LValue: TValue;
    LValue2: TValue;
    LRaiseIfMissing: boolean;
    LIdx: integer;
  begin
    if LLoopExprType.AsInstance.MetaclassType.InheritsFrom(TDataSet) then
    begin
      VisitDataSet;
      exit;
    end;
    LEnumGetEnumeratorMethod := LLoopExprType.GetMethod('GetEnumerator');
    if LEnumGetEnumeratorMethod = nil then
      RaiseError(AStmt, SGetEnumeratorNotFoundOnObject);
    LEnumValue := LEnumGetEnumeratorMethod.Invoke(LLoopExpr.AsObject, []);
    if LEnumValue.IsEmpty then
      RaiseErrorRes(AStmt, @SValueIsNotEnumerable);
    LEnumObj := LEnumValue.AsObject;
    LIdx := 0;
    try
      LLoopExprType := FContext.RttiContext().GetType(LEnumObj.ClassType);
      LEnumMoveNextMethod := LLoopExprType.GetMethod('MoveNext');
      LEnumCurrentProperty := LLoopExprType.getProperty('Current');
      LRaiseIfMissing := eoRaiseErrorWhenVariableNotFound in FContext.Options;
      while LEnumMoveNextMethod.Invoke(LEnumObj, []).AsBoolean and ((LLimit = -1) or (LLoops < LLimit)) do
      begin
        LValue := LEnumCurrentProperty.GetValue(LEnumObj);
        TryDeref(AStmt, LValue, LRaiseIfMissing, FContext, LValue2);

        if AStmt.ForOp = foIn then
          FStackFrames.peek[LVariableName] := LIdx
        else
          FStackFrames.peek[LVariableName] := LValue2;
        if HandleLoop then
          break;
        inc(LIdx);
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
    LRaiseIfMissing: boolean;
    LValue: TValue;
    LValue2: TValue;
  begin
    LArrayType := LLoopExprType as TRttiArrayType;
    if LArrayType.DimensionCount <> 1 then
      RaiseError(AStmt, SOnlyOneDimensionalArraysAreSupported);
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
    LRaiseIfMissing := eoRaiseErrorWhenVariableNotFound in FContext.Options;
    while (LIdx <= LLoopExpr.GetArrayLength - 1) and ((LLimit = -1) or (LLoops < LLimit)) do
    begin
      LValue := LGetValue(LLoopExpr, LIdx, LMin);
      TryDeref(AStmt, LValue, LRaiseIfMissing, FContext, LValue2);
      FStackFrames.peek[LVariableName] := LValue2;
      if HandleLoop then
        break;
      inc(LIdx);
    end;
  end;

  procedure VisitDynArray(const AOp: TForOp);
  var
    LIdx: integer;
    LGetValue: TGetForInValueIndex;
    LRaiseIfMissing: boolean;
    LValue: TValue;
    LValue2: TValue;
  begin
    if AOp = foIn then
      LGetValue := GetForInValueIndex
    else
      LGetValue := GetForInValueLoopExprIndex;
    LIdx := 0;
    LRaiseIfMissing := eoRaiseErrorWhenVariableNotFound in FContext.Options;
    while (LIdx <= LLoopExpr.GetArrayLength - 1) and ((LLimit = -1) or (LLoops < LLimit)) do
    begin
      LValue := LGetValue(LLoopExpr, LIdx, 0);
      TryDeref(AStmt, LValue, LRaiseIfMissing, FContext, LValue2);
      FStackFrames.peek[LVariableName] := LValue;
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

  procedure VisitInterface;
  begin
    if LLoopExpr.IsType<IMapExpr> then
    begin
      if AStmt.ForOp = foIn then
        LLoopExpr := TValue.From(LLoopExpr.AsType<IMapExpr>.GetMap.ToKeyArray)
      else
        LLoopExpr := TValue.From(LLoopExpr.AsType<IMapExpr>.GetMap.ToValueArray);
      VisitDynArray(TForOp.foOf);
      exit;
    end;

    RaiseError(AStmt, SGetEnumeratorNotFoundOnObject);
  end;

  procedure VisitRecord;
  begin
    if MatchMap(FContext, LLoopExpr.TypeInfo) then
    begin
      if AStmt.ForOp = foIn then
        LLoopExpr := TValue.From(LLoopExpr.AsType<TMap>.ToKeyArray)
      else
        LLoopExpr := TValue.From(LLoopExpr.AsType<TMap>.ToValueArray);
      VisitDynArray(TForOp.foOf);
      exit;
    end;

    RaiseError(AStmt, SGetEnumeratorNotFoundOnObject);
  end;

begin
  if HasBreakOrContinue then
    exit;
  LLoopOptions := FLoopOptions;
  FLoopOptions := [];
  try
    FStackFrames.push(FStackFrames.peek.Clone);
    LVariableName := AStmt.variable;

    LLoopExpr := EvalExpr(AStmt.Expr);
    if LLoopExpr.IsType<TValue> then
      LLoopExpr := LLoopExpr.AsType<TValue>();
    LOffset := GetValue(AStmt.OffsetExpr);
    LLimit := GetValue(AStmt.LimitExpr);

    i := 0;
    LLoops := 0;
    LFirst := true;
    try
      if not LLoopExpr.IsEmpty then
      begin
        LLoopExprType := FContext.RttiContext().GetType(LLoopExpr.TypeInfo);

        case LLoopExprType.TypeKind of
          tkInterface:
            VisitInterface;
          tkRecord{$IFDEF SUPPORT_CUSTOM_MANAGED_RECORDS}, tkMRecord{$ENDIF}:
            VisitRecord;
          tkClass, tkClassRef:
            VisitObject;
          tkArray:
            VisitArray;
          tkDynArray:
            VisitDynArray(AStmt.ForOp);
          tkString, tkWString, tkLString, tkUString:
            exit; // we return empty when there are issues, so lets ignore it
        else
          RaiseError(AStmt, SGetEnumeratorNotFoundOnObject);
        end;
      end;
    finally
      FStackFrames.pop;
    end;
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
  finally
    FLoopOptions := LLoopOptions;
  end;
end;

procedure TEvaluationTemplateVisitor.CheckRunTime(const APosition: IPosition);
begin
  if FStopWatch.ElapsedMilliseconds > FContext.MaxRunTimeMs then
    RaiseError(APosition, SMaxRuntimeOfMsHasBeenExceeded, [FContext.MaxRunTimeMs]);
end;

constructor TEvaluationTemplateVisitor.Create(const AContext: ITemplateContext; const AResolveContext: TTemplateValue; const AStackFrame: TStackFrame; const AStream: TStream; const ATemplate: ITemplate);
var
  LApply: ITemplateContextForScope;
begin
  inherited Create();

  FTemplate := ATemplate;
  FResolveContext := AResolveContext;
  FAllowRootDeref := true;
  FStopWatch := TStopWatch.Create;
  FStopWatch.Start;

  FLocalTemplates := TDictionary<string, ITemplate>.Create;

  FLoopOptions := [];
  FContext := AContext;

  FEvaluationContext := FContext as ITemplateEvaluationContext;
  FEvaluationContext.StartEvaluation;

  FStream := AStream;

  FStreamWriter := TTemplateStreamWriter.Create(FContext.StreamWriterProvider(FStream, FContext));

  if eoTrimLines in AContext.Options then
    FStreamWriter.TrimLines := true;

  FEvalStack := TStack<TValue>.Create;
  FStackFrames := TObjectStack<TStackFrame>.Create;
  FStackFrames.push(AStackFrame);

  if supports(AContext, ITemplateContextForScope, LApply) then
    LApply.ApplyTo(FStackFrames.peek);
end;

destructor TEvaluationTemplateVisitor.Destroy;
begin
  FEvaluationContext.EndEvaluation;
  FLocalTemplates.Free;
  FStreamWriter.Free;
  FStopWatch.Stop;
  FEvalStack.Free;
  FStackFrames.Free;
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
  LLoopOptions: TLoopOptions;
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
  LLoopOptions := FLoopOptions;
  FLoopOptions := [];
  try
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
        RaiseErrorRes(AStmt, @STypeNotSupported);
      end;
    end;
    if LStep <> -1 then
    begin
      LDelta := LDelta * abs(LStep);
    end;
    FStackFrames.push(FStackFrames.peek.Clone);
    LIdx := LStartVal;
    i := 0;
    try
      while LDirectionTestFunc(LIdx, LEndVal) do
      begin
        FStackFrames.peek[LVariable] := LIdx;
        FStackFrames.peek[LOOP_IDX_NAME] := i;
        if coBreak in FLoopOptions then
          break;
        if LFirst then
        begin
          LFirst := false;
          if AStmt.OnBeginContainer <> nil then
            AcceptVisitor(AStmt.OnBeginContainer, self);
        end
        else
        begin
          if AStmt.BetweenItemsContainer <> nil then
            AcceptVisitor(AStmt.BetweenItemsContainer, self);
        end;
        exclude(FLoopOptions, coContinue);
        CheckRunTime(AStmt);
        AcceptVisitor(AStmt.Container, self);
        inc(LIdx, LDelta);
        inc(i);
      end;
    finally
      FStackFrames.pop;
    end;
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
  finally
    FLoopOptions := LLoopOptions;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IAssignStmt);
var
  LVariable: string;
  LValue: TValue;
begin
  if HasBreakOrContinue then
    exit;
  LVariable := AStmt.variable;
  LValue := EvalExpr(AStmt.Expr);
  if LValue.IsType<TValue> then
    LValue := LValue.AsType<TValue>;
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
  if LIsObject and not IsEmptyObject(FContext, LExpr.AsObject) or isStrLike(LExpr) and (AsString(LExpr, FContext) <> '') or not LIsObject and AsBoolean(LExpr) then
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
  LTemplate := ResolveTemplate(AStmt.Expr, LTemplateName);
  if not assigned(LTemplate) then
  begin
    exit;
  end;
  FStackFrames.push(FStackFrames.peek.Clone());
  try
    Visit(LTemplate);
  finally
    FStackFrames.pop;
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

function TEvaluationTemplateVisitor.GetArgs(const APosition: IPosition; const AMethod: TRttiMethod; const AArgs: TArray<TValue>; const AContext: ITemplateContext): TArray<TValue>;
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
  if (LNumParams > 0) then
  begin
    if LParams[LOffset].ParamType.Handle = TypeInfo(ITemplateContext) then
    begin
      result[LOffset] := TValue.From<ITemplateContext>(AContext);
      inc(LOffset);
    end;
    if LParams[LOffset].ParamType.Handle = TypeInfo(TObjectStack<TStackFrame>) then
    begin
      result[LOffset] := TValue.From < TObjectStack < TStackFrame >> (FStackFrames);
      inc(LOffset);
    end;

    if LParams[LNumParams - 1].ParamType.TypeKind = tkDynArray then
    begin
      if LNumParams > 2 then
        RaiseErrorRes(APosition, @STooManyParameters);
      result[LOffset] := TValue.From(AArgs);
      exit;
    end;
  end;

  for LParamIdx := LOffset to LNumParams - 1 do
  begin
    LParameter := LParams[LParamIdx];
    LParamValue := CastArg(AArgs[LParamIdx - LOffset], LParameter.ParamType, AContext);
    if LParamValue.IsType<TValue> then
      LParamValue := LParamValue.AsType<TValue>();
    result[LParamIdx] := LParamValue;
  end;
end;

function TEvaluationTemplateVisitor.Invoke(const AFuncCall: IFunctionCallExpr; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue;
var
  LMethod: TRttiMethod;

  function GetParamCount: integer;
  var
    LParams: TArray<TRttiParameter>;
    LParam: TRttiParameter;
  begin
    LParams := LMethod.GetParameters;
    result := length(LParams);
    if result > 0 then
    begin
      LParam := LParams[0];
      if LParam.ParamType.Handle = TypeInfo(ITemplateContext) then
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
  exit(DoInvoke(AFuncCall, LMethod, nil, AArgs, AHasResult));
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
      RaiseError(AExpr, e.Message);
  end;
end;

function TEvaluationTemplateVisitor.DoInvoke(const APosition: IPosition; const AMethod: TRttiMethod; const AObject: TValue; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue;

  function GetParamCount: integer;
  var
    LParams: TArray<TRttiParameter>;
    LParam: TRttiParameter;
  begin
    LParams := AMethod.GetParameters;
    result := length(LParams);
    if result > 0 then
    begin
      LParam := LParams[0];
      if LParam.ParamType.Handle = TypeInfo(ITemplateContext) then
        dec(result);
    end;
  end;

begin
  AHasResult := AMethod.ReturnType <> nil;
  if length(AMethod.GetParameters) = 0 then
    result := AMethod.Invoke(AObject, [])
  else
    result := AMethod.Invoke(AObject, GetArgs(APosition, AMethod, AArgs, FContext));
  if result.IsType<TValue> then
    result := result.AsType<TValue>();
end;

function TEvaluationTemplateVisitor.Invoke(const AExpr: IMethodCallExpr; const AObject: TValue; const AArgs: TArray<TValue>; out AHasResult: boolean): TValue;
var
  LObjType: TRttiType;
  LObject: TValue;
  LMethod: TRttiMethod;
  LIntf: IInterface;
begin
  LObjType := FContext.RttiContext().GetType(AObject.TypeInfo);
  LObject := AObject;
  if LObject.Kind = tkInterface then
  begin
    LIntf := LObject.AsInterface;
    LObject := TObject(LIntf);
    LObjType := FContext.RttiContext.GetType(LObject.TypeInfo);
  end
  else
  begin
    if AObject.IsType<TValue> then
      LObject := AObject.AsType<TValue>;
  end;
  LMethod := LObjType.GetMethod(AExpr.Method);
  if LMethod = nil then
  begin
    RaiseErrorRes(AExpr, @SMethodNotRegisteredOnObject, [LObject.TypeInfo.Name, AExpr.Method]);
  end;
  exit(DoInvoke(AExpr, LMethod, LObject, AArgs, AHasResult));
end;

function TEvaluationTemplateVisitor.ResolveTemplate(const APosition: IPosition; const AName: string): ITemplate;
begin
  if not FLocalTemplates.TryGetValue(AName, result) and not FContext.TryGetTemplate(AName, result, FResolveContext) then
  begin
    RaiseErrorRes(APosition, @STemplateNotFound, [AName]);
  end;
end;

function TEvaluationTemplateVisitor.ResolveTemplate(const AExpr: IExpr): ITemplate;
begin
  exit(ResolveTemplate(AExpr, EvalExprAsString(AExpr)));
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
    on ETemplateEvaluationError do
      raise;
    on e: exception do
      RaiseError(AExpr, AExpr.Method + ':' + e.Message);
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IEncodeExpr);
begin
  FEvalStack.push(EncodeVariable(EvalExpr(AExpr.Expr)));
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
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
  try
    AcceptVisitor(AStmt.Container, self);
  finally
    FStackFrames.pop;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IRequireStmt);
var
  LExprs: TArray<TValue>;
  LInputType: string;
  LIndex: integer;
begin
  if HasBreakOrContinue then
    exit;
  LInputType := FContext.RttiContext().GetType(FStackFrames.peek['_'].TypeInfo).Name.ToLower;
  LExprs := ExprListArgs(AStmt.exprlist);
  if length(LExprs) = 0 then
    exit;
  for LIndex := 0 to high(LExprs) do
  begin
    if AsString(LExprs[LIndex], FContext).ToLower = LInputType then
      exit;
  end;
  RaiseError(AStmt, SInputOfRequiredTypeNotFound);
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IArrayExpr);
var
  LArray: TArray<TValue>;
  LArrayValue: TValue;
begin
  LArray := ExprListArgs(AExpr.exprlist);
  LArrayValue := TValue.From < TArray < TValue >> (LArray);
  FEvalStack.push(LArrayValue);
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
  LInt: int64;
begin
  if HasBreakOrContinue then
    exit;
  try
    LStackFrame := FStackFrames.peek;
    LValue := LStackFrame[LOOP_IDX_NAME];
  except
    RaiseErrorRes(AStmt, @SCycleStatementMustBeInALoop);
  end;
  LInt := AsInt(LValue, FContext);
  LInt := LInt mod AStmt.List.Count;
  FStreamWriter.Write(EvalExprAsString(AStmt.List[LInt]));
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IDebugStmt);
begin
  if HasBreakOrContinue then
    exit;
  try
    AcceptVisitor(AStmt.Stmt, self);
  except
    on e: exception do
    begin
      FStreamWriter.Write(Format(FContext.DebugErrorFormat, [e.Message]));
    end;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IStripStmt);
begin
  // do nothing
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: ICompositeStmt);
begin
  if HasBreakOrContinue then
    exit;
  AcceptVisitor(AStmt.FirstStmt, self);
  AcceptVisitor(AStmt.SecondStmt, self);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IBlockStmt);
var
  LBlockName: string;
  LReplacementBlock: IBlockStmt;
begin
  LBlockName := AStmt.NameAsString(self);
  FStackFrames.push(FStackFrames.peek.Clone());
  try
    if FEvaluationContext.TryGetBlock(LBlockName, LReplacementBlock) then
    begin
      Visit(LReplacementBlock.Container);
    end
    else
    begin
      Visit(AStmt.Container);
    end;
  finally
    FStackFrames.pop;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IExtendsStmt);
var
  LName: string;
  LBlockName: string;
  LReplacementBlock: IBlockStmt;
  LTemplate: ITemplate;
  LBlockResolver: IBlockResolverVisitor;
  LBlockPair: TPair<string, IBlockStmt>;
  LBlocks: TDictionary<string, IBlockStmt>;
begin
  LName := AStmt.NameAsString(self);
  LTemplate := ResolveTemplate(AStmt, LName);
  if not assigned(LTemplate) then
  begin
    exit;
  end;
  LBlocks := TDictionary<string, IBlockStmt>.Create;
  try
    LBlockResolver := TBlockResolverVisitor.Create(self, AStmt.BlockContainer);
    LBlockResolver.Discover;
    for LBlockName in LBlockResolver.GetBlockNames do
    begin
      LReplacementBlock := LBlockResolver.GetBlock(LBlockName);
      LBlocks.AddOrSetValue(LBlockName.ToLower, LReplacementBlock);
    end;
    for LBlockPair in LBlocks do
    begin
      FEvaluationContext.AddBlock(LBlockPair.Key, LBlockPair.Value);
    end;
    FStackFrames.push(FStackFrames.peek.Clone());
    try
      Visit(LTemplate);
    finally
      FStackFrames.pop;
    end;
    for LBlockPair in LBlocks do
    begin
      FEvaluationContext.RemoveBlock(LBlockPair.Key);
    end;
  finally
    LBlocks.Free;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AExpr: IMapExpr);
var
  LMap: TMap;
  LMapExpr: IMapExpr;
  LValue: TValue;
  i: integer;
begin
  LMap := AExpr.GetMap.Clone;
  LMapExpr := TMapExpr.Create(AExpr, LMap);
  for i := 0 to LMap.Count - 1 do
  begin
    LValue := LMap.Values[i];
    if LValue.IsType<IExpr> then
      LMap.Values[i] := EvalExpr(LValue.AsType<IExpr>);
  end;
  LValue := TValue.From<IMapExpr>(LMapExpr);
  FEvalStack.push(LValue);
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IIgnoreWSStmt);
var
  LStripWS: boolean;
begin
  LStripWS := FStreamWriter.Stripws;
  FStreamWriter.Stripws := true;
  try
    AcceptVisitor(AStmt.Container, self);
  finally
    FStreamWriter.Stripws := LStripWS;
  end;
end;

procedure TEvaluationTemplateVisitor.Visit(const AStmt: IIgnoreNLStmt);
var
  LStripNL: boolean;
begin
  LStripNL := FStreamWriter.StripNL;
  FStreamWriter.StripNL := true;
  try
    AcceptVisitor(AStmt.Container, self);
  finally
    FStreamWriter.StripNL := LStripNL;
  end;
end;

end.
