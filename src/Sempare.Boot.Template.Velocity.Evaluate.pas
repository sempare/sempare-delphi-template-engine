(*%****************************************************************************
 *  ___                                             ___               _       *
 * / __|  ___   _ __    _ __   __ _   _ _   ___    | _ )  ___   ___  | |_     *
 * \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)   | _ \ / _ \ / _ \ |  _|    *
 * |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|   |___/ \___/ \___/  \__|    *
 *                     |_|                                                    *
 ******************************************************************************
 *                                                                            *
 *                        VELOCITY TEMPLATE ENGINE                            *
 *                                                                            *
 *                                                                            *
 *          https://www.github.com/sempare/sempare.boot.velocity.oss          *
 ******************************************************************************
 *                                                                            *
 * Copyright (c) 2019 Sempare Limited,                                        *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>           *
 *                                                                            *
 * Contact: info@sempare.ltd                                                  *
 *                                                                            *
 * Licensed under the Apache License, Version 2.0 (the "License");            *
 * you may not use this file except in compliance with the License.           *
 * You may obtain a copy of the License at                                    *
 *                                                                            *
 *   http://www.apache.org/licenses/LICENSE-2.0                               *
 *                                                                            *
 * Unless required by applicable law or agreed to in writing, software        *
 * distributed under the License is distributed on an "AS IS" BASIS,          *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   *
 * See the License for the specific language governing permissions and        *
 * limitations under the License.                                             *
 *                                                                            *
 ****************************************************************************%*)
unit Sempare.Boot.Template.Velocity.Evaluate;

interface

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.Diagnostics,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Scope,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.PrettyPrint,
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity.Visitor;

type
  TLoopOption = (coInLoop = 1, coContinue = 2, coBreak = 4, coContinueOrBreak = 6);
  TLoopOptions = set of TLoopOption;

  TEvaluationVelocityVisitor = class(TBaseVelocityVisitor)
  private
    FPretty: IVelocityVisitor;
    FStopWatch: TStopWatch;
    FScopeStack: TObjectStack<TVariableScope>;
    FEvalStack: TStack<TValue>;
    FStream: TStream;
    FStreamWriter: TStreamWriter;
    FInLoop: boolean;
    FContinue: boolean;
    FBreak: boolean;
    FContext: IVelocityContext;
    FAllowRootDeref: boolean;
    FLocalTemplates: TDictionary<string, IVelocityTemplate>;

    function EncodeVariable(const AValue: TValue): TValue;
    procedure CheckRunTime(const APosition: IPosition);
    function ExprListArgs(const AExprList: IExprList): TArray<TValue>;

  public
    constructor Create(const AContext: IVelocityContext; const AValue: TValue; const AStream: TStream); overload;
    constructor Create(const AContext: IVelocityContext; const AScope: TVariableScope; const AStream: TStream); overload;
    destructor Destroy; override;

    procedure Visit(const AExpr: IBinopExpr); overload; override;
    procedure Visit(const AExpr: IUnaryExpr); overload; override;
    procedure Visit(const AExpr: IVariableExpr); overload; override;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(const AExpr: IValueExpr); overload; override;
    procedure Visit(const AExprList: IExprList); overload; override;
    procedure Visit(const AExpr: ITernaryExpr); overload; override;
    procedure Visit(const AExpr: IEncodeExpr); overload; override;

    procedure Visit(const AStmt: IAssignStmt); overload; override;
    procedure Visit(const AStmt: IContinueStmt); overload; override;
    procedure Visit(const AStmt: IBreakStmt); overload; override;
    procedure Visit(const AStmt: IEndStmt); overload; override;
    procedure Visit(const AStmt: IIncludeStmt); overload; override;

    procedure Visit(const AStmt: IPrintStmt); overload; override;
    procedure Visit(const AStmt: IIfStmt); overload; override;
    procedure Visit(const AStmt: IWhileStmt); overload; override;
    procedure Visit(const AStmt: IForInStmt); overload; override;
    procedure Visit(const AStmt: IForRangeStmt); overload; override;
    procedure Visit(const AStmt: IFunctionCallExpr); overload; override;
    procedure Visit(const AStmt: IMethodCallExpr); overload; override;

    procedure Visit(const AStmt: IProcessTemplateStmt); overload; override;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload; override;
    procedure Visit(const AStmt: IWithStmt); overload; override;

  end;

implementation

uses
  System.JSON,
  Sempare.Boot.Template.Velocity.Rtti,
  Sempare.Boot.Template.Velocity.Util;

const
  BREAK_OR_CONTINUE: TLoopOptions = [coContinue, coBreak];

  { TEvaluationVelocityVisitor }

constructor TEvaluationVelocityVisitor.Create(const AContext: IVelocityContext; const AValue: TValue; const AStream: TStream);
var
  Scope: TVariableScope;
begin
  Scope := TVariableScope.Create(AValue, nil);
  Create(AContext, Scope, AStream);
end;

procedure TEvaluationVelocityVisitor.Visit(const AExpr: IValueExpr);
begin
  FEvalStack.push(AExpr.Value);
end;

procedure TEvaluationVelocityVisitor.Visit(const AExprList: IExprList);
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

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IContinueStmt);
begin
  FContinue := true;
end;

procedure TEvaluationVelocityVisitor.Visit(const AExpr: IVariableDerefExpr);

var
  Derefvar: TValue;
  variable: TValue;
  AllowRootDeref: IPreserveValue<boolean>;
begin
  AllowRootDeref := Preseve.Value(FAllowRootDeref, true);

  acceptvisitor(AExpr.variable, self);
  variable := FEvalStack.pop;

  AllowRootDeref.SetValue(AExpr.DerefType = dtArray);
  acceptvisitor(AExpr.DerefExpr, self);
  Derefvar := FEvalStack.pop;

  FEvalStack.push(Deref(variable, Derefvar));
end;

procedure TEvaluationVelocityVisitor.Visit(const AExpr: IBinopExpr);
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
    boDiv:
      begin
        AssertNumeric(Position(AExpr.LeftExpr), left, right);
        res := Asnum(left) / Asnum(right);
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
    roEQ:
      res := isEqual(left, right);
    roNotEQ:
      res := not isEqual(left, right);
    roLT:
      res := isLessThan(left, right);
    roLTE:
      res := not isGreaterThan(left, right);
    roGT:
      res := isGreaterThan(left, right);
    roGTE:
      res := not isLessThan(left, right);
  else
    RaiseError(Position(AExpr.LeftExpr), 'Binop not supported');
  end;
  FEvalStack.push(res);
end;

procedure TEvaluationVelocityVisitor.Visit(const AExpr: IUnaryExpr);
var
  v: TValue;
begin
  acceptvisitor(AExpr.Expr, self);
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

procedure TEvaluationVelocityVisitor.Visit(const AExpr: IVariableExpr);
var
  Scope: TVariableScope;
  val: TValue;
begin
  if FAllowRootDeref then
  begin
    Scope := FScopeStack.peek;
    val := Scope[AExpr.variable];
    if val.IsEmpty then
    begin
      val := Deref(Scope.Root, AExpr.variable);
    end;
  end
  else
  begin
    val := AExpr.variable;
  end;
  FEvalStack.push(val);
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IBreakStmt);
begin
  FBreak := true;
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IWhileStmt);
var
  InLoop: IPreserveValue<boolean>;
  LContinue: IPreserveValue<boolean>;
  LBreak: IPreserveValue<boolean>;
begin
  if FBreak or FContinue then
    exit;
  InLoop := Preseve.Value<boolean>(FInLoop, true);
  LContinue := Preseve.Value<boolean>(FContinue, false);
  LBreak := Preseve.Value<boolean>(FBreak, false);
  FScopeStack.push(FScopeStack.peek.Clone);
  while true do
  begin
    acceptvisitor(AStmt.Condition, self);
    if not AsBoolean(FEvalStack.pop) or FBreak then
      break;
    CheckRunTime(Position(AStmt));
    FContinue := false;
    acceptvisitor(AStmt.Container, self);
  end;
  FScopeStack.pop;
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IForInStmt);
var
  InLoop: IPreserveValue<boolean>;
  LContinue: IPreserveValue<boolean>;
  LBreak: IPreserveValue<boolean>;
  e: tobject;
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
      raise Exception.Create('Value is not enumerable');
    e := val.AsObject;
    T := GRttiContext.GetType(e.ClassType);
    movenext := T.GetMethod('MoveNext');
    current := T.GetProperty('Current');

    while movenext.Invoke(e, []).AsBoolean do
    begin
      if FBreak then
        break;
      FScopeStack.peek[v] := current.GetValue(e);
      FContinue := false;
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

  procedure visitarray;
  var
    at: TRttiArrayType;
    Dt: TRttiOrdinalType;
    i, ai: integer;
  begin
    at := T as TRttiArrayType;
    if at.DimensionCount > 1 then
      RaiseError(Position(AStmt), 'Only one dimensional arrays are supported.');
    Dt := at.Dimensions[0] as TRttiOrdinalType;

    for i := 0 to Eval.GetArrayLength - 1 do
    begin
      if FBreak then
        break;
      ai := i + Dt.MinValue;
      FScopeStack.peek[v] := ai;
      FContinue := false;
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

  procedure visitdynarray;
  var
    i: integer;
  begin
    for i := 0 to Eval.GetArrayLength - 1 do
    begin
      if FBreak then
        break;
      FScopeStack.peek[v] := i;
      FContinue := false;
      CheckRunTime(Position(AStmt));
      acceptvisitor(AStmt.Container, self);
    end;
  end;

begin
  if FBreak or FContinue then
    exit;
  InLoop := Preseve.Value(FInLoop, true);
  LContinue := Preseve.Value(FContinue, false);
  LBreak := Preseve.Value(FBreak, false);
  FScopeStack.push(FScopeStack.peek.Clone);
  v := AStmt.variable;

  acceptvisitor(AStmt.Expr, self);
  Eval := FEvalStack.pop;
  T := GRttiContext.GetType(Eval.TypeInfo);

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
  FScopeStack.pop;
end;

procedure TEvaluationVelocityVisitor.CheckRunTime(const APosition: IPosition);
begin
  if FStopWatch.ElapsedMilliseconds > FContext.MaxRunTimeMs then
    RaiseError(APosition, 'Max runtime of %dms has been exceeded.', [FContext.MaxRunTimeMs]);
end;

constructor TEvaluationVelocityVisitor.Create(const AContext: IVelocityContext; const AScope: TVariableScope; const AStream: TStream);
var
  apply: IVelocityContextForScope;
begin
  FPretty := TPrettyPrintVelocityVisitor.Create();
  FAllowRootDeref := true;
  FStopWatch := TStopWatch.Create;
  FStopWatch.Start;

  FLocalTemplates := TDictionary<string, IVelocityTemplate>.Create;

  FInLoop := false;
  FContinue := false;
  FBreak := false;
  FContext := AContext;
  FStream := AStream;
  FStreamWriter := TStreamWriter.Create(FStream, FContext.Encoding);

  FEvalStack := TStack<TValue>.Create;
  FScopeStack := TObjectStack<TVariableScope>.Create;
  FScopeStack.push(AScope);

  if AContext.QueryInterface(IVelocityContextForScope, apply) = s_ok then
    apply.ApplyTo(FScopeStack.peek);
end;

destructor TEvaluationVelocityVisitor.Destroy;
begin
  FLocalTemplates.Free;
  FStreamWriter.Free;
  FStopWatch.Stop;
  FEvalStack.Free;
  FScopeStack.Free;
  FContext := nil;
  FPretty := nil;
  inherited;
end;

function TEvaluationVelocityVisitor.EncodeVariable(const AValue: TValue): TValue;
begin
  if FContext.VariableEncoder = nil then
    exit(AValue);
  result := FContext.VariableEncoder(AsString(AValue));
end;

function TEvaluationVelocityVisitor.ExprListArgs(const AExprList: IExprList): TArray<TValue>;
var
  args: TArray<TValue>;
  i: integer;
  Count: integer;
begin
  AExprList.Accept(self);

  Count := asint(FEvalStack.pop());
  if Count <> AExprList.Count then // this should not happen
    RaiseError(nil, 'Number of arguments mismatch');

  for i := 1 to Count do
  begin
    insert(FEvalStack.pop, args, length(args));
  end;
  result := args;
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IForRangeStmt);
var
  InLoop: IPreserveValue<boolean>;
  LContinue: IPreserveValue<boolean>;
  LBreak: IPreserveValue<boolean>;
  i, lowVal, highVal: int64;
  delta: integer;
  v: string;

begin
  if FBreak or FContinue then
    exit;
  v := AStmt.variable;
  InLoop := Preseve.Value(FInLoop, true);
  LContinue := Preseve.Value(FContinue, false);
  LBreak := Preseve.Value(FBreak, false);

  acceptvisitor(AStmt.LowExpr, self);
  lowVal := asint(FEvalStack.pop);

  acceptvisitor(AStmt.HighExpr, self);
  highVal := asint(FEvalStack.pop);

  case AStmt.ForOp of
    foTo:
      delta := 1;
    foDownto:
      delta := -1;
  else
    raise Exception.Create('ForOp not supported');
  end;
  FScopeStack.push(FScopeStack.peek.Clone);
  i := lowVal;
  while i <= highVal do
  begin
    FScopeStack.peek[v] := i;
    if FBreak then
      break;
    FContinue := false;
    CheckRunTime(Position(AStmt));
    acceptvisitor(AStmt.Container, self);
    inc(i, delta);
  end;
  FScopeStack.pop;
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IAssignStmt);
var
  v: string;
begin
  v := AStmt.variable;
  acceptvisitor(AStmt.Expr, self);
  FScopeStack.peek[v] := FEvalStack.pop;
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IIfStmt);
begin
  if FBreak or FContinue then
    exit;
  acceptvisitor(AStmt.Condition, self);
  if AsBoolean(FEvalStack.pop) then
    acceptvisitor(AStmt.TrueContainer, self)
  else if AStmt.FalseContainer <> nil then
    acceptvisitor(AStmt.FalseContainer, self);
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IEndStmt);
begin
  // nothing to do
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IIncludeStmt);
var
  Name: string;
  T: IVelocityTemplate;

begin
  if FBreak or FContinue then
    exit;
  acceptvisitor(AStmt.Expr, self);
  name := FEvalStack.pop.AsString;

  if FLocalTemplates.TryGetValue(name, T) or FContext.TryGetTemplate(name, T) then
  begin
    FScopeStack.push(FScopeStack.peek.Clone());
    try
      Visit(T);
    finally
      FScopeStack.pop;
    end;
  end
  else
    raise Exception.Createfmt('Template not found: %s', [name]);
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IPrintStmt);
begin
  if FBreak or FContinue then
    exit;
  acceptvisitor(AStmt.Expr, self);
  FStreamWriter.Write(AsString(FEvalStack.pop));
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IFunctionCallExpr);
var
  args: TArray<TValue>;
begin
  args := ExprListArgs(AStmt.exprlist);
  try
    FEvalStack.push(AStmt.Invoke(args));
  except
    on e: Exception do
      RaiseError(Position(AStmt), e.Message);
  end;
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IMethodCallExpr);
var
  obj: TValue;
  args: TArray<TValue>;
begin
  acceptvisitor(AStmt.ObjectExpr, self);
  obj := FEvalStack.pop;
  args := ExprListArgs(AStmt.exprlist);
  try
    FEvalStack.push(AStmt.Invoke(obj.AsObject, args));
  except
    on e: Exception do
      RaiseError(Position(AStmt), e.Message);
  end;
end;

procedure TEvaluationVelocityVisitor.Visit(const AExpr: IEncodeExpr);
begin
  acceptvisitor(AExpr.Expr, self);
  FEvalStack.push(EncodeVariable(FEvalStack.pop));
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  acceptvisitor(AStmt.Container, self);
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IDefineTemplateStmt);
var
  Name: TValue;
begin
  acceptvisitor(AStmt.Name, self);
  name := FEvalStack.pop;
  AssertString(Position(AStmt), name);
  FLocalTemplates.AddOrSetValue(AsString(name), AStmt.Container);
end;

procedure TEvaluationVelocityVisitor.Visit(const AStmt: IWithStmt);

var
  VarScope: TVariableScope;

procedure scopeClass(const ARttiType: TRttiType; const ARecord: TValue); forward;
procedure scopeDictionary(const ARttiType: TRttiType; const ADict: tobject); forward;
procedure scopeJsonObject(const ARttiType: TRttiType; const AObj: tjsonobject); forward;
procedure scopeRecord(const ARttiType: TRttiType; const ARecord: TValue); forward;

  procedure Scope(const ARecord: TValue);
  var
    RttiType: TRttiType;
  begin
    RttiType := GRttiContext.GetType(ARecord.TypeInfo);
    case RttiType.TypeKind of
      tkClass, tkClassRef:
        scopeClass(RttiType, ARecord);
      tkRecord:
        scopeRecord(RttiType, ARecord);
    else
      raise Exception.Create('Scope must be defined on a class or record.');
    end;
  end;

  procedure scopeClass(const ARttiType: TRttiType; const ARecord: TValue);
  var
    field: TRttiField;
    prop: TRttiProperty;
    obj: tobject;
  begin
    obj := ARecord.AsObject;
    if obj.ClassType.QualifiedClassName.StartsWith('System.Generics.Collections.TDictionary<System.string') then
    begin
      scopeDictionary(ARttiType, obj);
      exit;
    end;
    if obj.ClassType = tjsonobject then
    begin
      scopeJsonObject(ARttiType, tjsonobject(obj));
      exit;
    end;
    // we can't do anything on generic collections. we can loop using _ if we need to do anything.
    if obj.ClassType.QualifiedClassName.StartsWith('System.Generics.Collections') then
      exit;
    for field in ARttiType.GetFields do
      VarScope[field.Name] := field.GetValue(obj);
    for prop in ARttiType.GetProperties do
      VarScope[prop.Name] := prop.GetValue(obj);
  end;

  procedure scopeDictionary(const ARttiType: TRttiType; const ADict: tobject);
  var
    e: tobject;
    T: TRttiType;
    m, movenext: TRttiMethod;
    current: TRttiProperty;
    key, Value: TRttiField;
    val: TValue;
    k: string;
    o: TValue;
    obj: pointer;

  begin
    m := ARttiType.GetMethod('GetEnumerator');
    val := m.Invoke(ADict, []).AsObject;
    e := val.AsObject;
    T := GRttiContext.GetType(e.ClassType);
    k := e.ClassName;
    movenext := T.GetMethod('MoveNext');
    current := T.GetProperty('Current');
    key := nil;
    Value := nil;
    while movenext.Invoke(e, []).AsBoolean do
    begin
      o := current.GetValue(e); // this returns TPair record
      obj := o.GetReferenceToRawData;
      if key = nil then
      begin
        T := GRttiContext.GetType(o.TypeInfo);
        key := T.GetField('Key');
        Value := T.GetField('Value');
      end;
      k := key.GetValue(obj).AsString;
      val := Value.GetValue(obj);
      VarScope[k] := val;
    end;
  end;

  procedure scopeJsonObject(const ARttiType: TRttiType; const AObj: tjsonobject);
  var
    p: tjsonpair;
    k: string;
    v: tjsonvalue;
  begin
    for p in AObj do
    begin
      k := p.JsonString.AsType<string>;
      v := p.JsonValue;
      if v is TJSONBool then
      begin
        VarScope[k] := v.AsType<boolean>;
      end
      else if v is TJSONString then
      begin
        VarScope[k] := v.AsType<string>;
      end
      else if v is TJSONNumber then
      begin
        VarScope[k] := v.AsType<extended>;
      end
      else if v is tjsonobject then
      begin
        VarScope[k] := v;
      end
      else if v is TJSONNull then
      begin
        VarScope[k] := nil;
      end;
    end;
  end;

  procedure scopeRecord(const ARttiType: TRttiType; const ARecord: TValue);
  var
    field: TRttiField;
    obj: pointer;
  begin
    obj := ARecord.GetReferenceToRawData;
    for field in ARttiType.GetFields do
      VarScope[field.Name] := field.GetValue(obj);
  end;

var
  Expr: TValue;
begin
  acceptvisitor(AStmt.Expr, self);
  Expr := FEvalStack.pop;
  VarScope := FScopeStack.peek.Clone;
  FScopeStack.push(VarScope);

  Scope(Expr);
  acceptvisitor(AStmt.Container, self);

  FScopeStack.pop;
end;

procedure TEvaluationVelocityVisitor.Visit(const AExpr: ITernaryExpr);
begin
  acceptvisitor(AExpr.Condition, self);
  if AsBoolean(FEvalStack.pop) then
    acceptvisitor(AExpr.TrueExpr, self)
  else
    acceptvisitor(AExpr.FalseExpr, self);
end;

end.
