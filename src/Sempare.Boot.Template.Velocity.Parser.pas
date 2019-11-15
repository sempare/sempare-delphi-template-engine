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
unit Sempare.Boot.Template.Velocity.Parser;

interface

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses
  System.Classes,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Context;

type
  IVelocityParser = interface
    function Parse(const AStream: TStream; const AManagedStream: boolean = true): IVelocityTemplate; overload;
    function Parse(const AString: string): IVelocityTemplate; overload;
  end;

function CreateVelocityParser(Const AContext: IVelocityContext): IVelocityParser;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Rtti,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity,
  Sempare.Boot.Template.Velocity.Evaluate,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.Lexer,
  Sempare.Boot.Template.Velocity.Rtti,
  Sempare.Boot.Template.Velocity.Util;

type

  TTemplate = class(TInterfacedObject, IVelocityTemplate, ITemplateAdd, IVelocityVisitorHost)
  private
    FArray: TArray<IVelocityVisitorHost>;
    function GetItem(const AOffset: integer): IVelocityVisitorHost;
    function GetCount: integer; inline;
    procedure Add(const AItem: IVelocityVisitorHost);
    function GetLastItem: IVelocityVisitorHost; inline;

    procedure Accept(const AVisitor: IVelocityVisitor); inline;
  public

  end;

  TAbstractBase = class abstract(TInterfacedObject, IPositional, IVelocityVisitorHost)
  private
    FPosition: IPosition;
    function GetPosition: IPosition;
  public
    constructor Create(const APosition: IPosition);
    destructor Destroy; override;
    procedure Accept(const AVisitor: IVelocityVisitor); virtual; abstract;
  end;

  TAbstractStmt = class abstract(TAbstractBase, IStmt)
  end;

  TEndStmt = class(TAbstractStmt, IEndStmt)
  private
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  end;

  TElseStmt = class(TAbstractStmt, IElseStmt)
  private
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  end;

  TContinueStmt = class(TAbstractStmt, IContinueStmt)
  private
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  end;

  TBreakStmt = class(TAbstractStmt, IBreakStmt)
  private
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  end;

  TCommentStmt = class(TAbstractStmt, ICommentStmt)
  private
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  end;

  TElIfStmt = class(TAbstractStmt, IElIfStmt)
  private
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  end;

  TPrintStmt = class(TAbstractStmt, IPrintStmt)
  private
    FExpr: IExpr;
    function GetExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AExpr: IExpr);
  end;

  TIncludeStmt = class(TAbstractStmt, IIncludeStmt)
  private
    FExpr: IExpr;
    function GetExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AExpr: IExpr);
  end;

  TIfStmt = class(TAbstractStmt, IIfStmt)
  private
    FCondition: IExpr;
    FTrueContainer: IVelocityTemplate;
    FFalseContainer: IVelocityTemplate;
    function GetCondition: IExpr; inline;
    function GetTrueContainer: IVelocityTemplate; inline;
    function GetFalseContainer: IVelocityTemplate; inline;

    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const ACondition: IExpr; const ATrueContainer: IVelocityTemplate; const AFalseContainer: IVelocityTemplate);
  end;

  TProcessTemplateStmt = class(TAbstractStmt, IProcessTemplateStmt)
  private
    FContainer: IVelocityTemplate;
    function GetContainer: IVelocityTemplate;

    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AContainer: IVelocityTemplate);
  end;

  TDefineTemplateStmt = class(TAbstractStmt, IDefineTemplateStmt)
  private
    FName: IExpr;
    FContainer: IVelocityTemplate;
    function GetName: IExpr;
    function GetContainer: IVelocityTemplate;

    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const AContainer: IVelocityTemplate);
  end;

  TWithStmt = class(TAbstractStmt, IWithStmt)
  private
    FExpr: IExpr;
    FContainer: IVelocityTemplate;
    function GetContainer: IVelocityTemplate;
    function GetExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AExpr: IExpr; const AContainer: IVelocityTemplate);
  end;

  TWhileStmt = class(TAbstractStmt, IWhileStmt)
  private
    FCondition: IExpr;
    FContainer: IVelocityTemplate;
    function GetCondition: IExpr; inline;
    function GetContainer: IVelocityTemplate; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const ACondition: IExpr; const AContainer: IVelocityTemplate);
  end;

  TForInStmt = class(TAbstractStmt, IForInStmt)
  private
    FVariable: string;
    FExpr: IExpr;
    FContainer: IVelocityTemplate;
    function GetVariable: string; inline;
    function GetExpr: IExpr; inline;
    function GetContainer: IVelocityTemplate; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr; const AContainer: IVelocityTemplate);
  end;

  TForRangeStmt = class(TAbstractStmt, IForRangeStmt)
  private
    FVariable: string;
    FForIp: TForOp;
    FLowExpr: IExpr;
    FHighExpr: IExpr;
    FContainer: IVelocityTemplate;
    function GetVariable: string; inline;
    function GetForOp: TForOp; inline;
    function GetLowExpr: IExpr; inline;
    function GetHighExpr: IExpr; inline;
    function GetContainer: IVelocityTemplate; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AForIp: TForOp; const ALowExpr: IExpr; const AHighExpr: IExpr; const AContainer: IVelocityTemplate);
  end;

  TAssignStmt = class(TAbstractStmt, IAssignStmt)
  private
    FVariable: string;
    FExpr: IExpr;
    function GetVariable: string; inline;
    function GetExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr);
  end;

  TExprList = class(TAbstractBase, IExprList)
  private
    FExprs: TArray<IExpr>;
    function GetExpr(const AOffset: integer): IExpr; inline;
    procedure AddExpr(const AExpr: IExpr);
    function GetExprCount: integer; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
  end;

  TAbstractExpr = class abstract(TAbstractBase, IExpr)
  end;

  TValueExpr = class(TAbstractExpr, IValueExpr)
  private
    FValue: TValue;
    function GetValue: TValue; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AValue: TValue);
  end;

  TVariableExpr = class(TAbstractExpr, IVariableExpr)
  private
    FVariable: string;
    function GetVariable: string; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AVariable: string);
  end;

  TEncodeExpr = class(TAbstractExpr, IEncodeExpr)
  private
    FExpr: IExpr;
    function GetExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AExpr: IExpr);
  end;

  TVariableDerefExpr = class(TAbstractExpr, IVariableDerefExpr)
  private
    FVariable: IExpr;
    FDeref: IExpr;
    FDerefType: TDerefType;
    function GetDerefType: TDerefType;
    function GetVariable: IExpr; inline;
    function GetDerefExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;
  public
    constructor Create(const APosition: IPosition; const ADerefType: TDerefType; const AVariable: IExpr; const ADeref: IExpr);
  end;

  TFunctionCallExpr = class(TAbstractExpr, IFunctionCallExpr)
  private
    FFunctionInfo: TVelocityFunctionInfo;
    FExprList: IExprList;

    function GetFunctionInfo: TVelocityFunctionInfo; inline;
    function GetExprList: IExprList; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

    function Invoke(const AArgs: TArray<TValue>): TValue;

  public
    constructor Create(const APosition: IPosition; AFunction: TVelocityFunctionInfo; const ExprList: IExprList);
  end;

  TMethodCallExpr = class(TAbstractExpr, IMethodCallExpr)
  private
    FObjectExpr: IExpr;
    FMethod: string;
    FExprList: IExprList;
    FRttiMethod: TRttiMethod;

    function GetMethod: string; inline;
    function GetObject: IExpr; inline;
    function GetExprList: IExprList; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

    function Invoke(const AObject: TObject; const AArgs: TArray<TValue>): TValue;

  public
    constructor Create(const APosition: IPosition; const AObjectExpr: IExpr; const AMethod: string; const AExprList: IExprList);
  end;

  TBinopExpr = class(TAbstractExpr, IBinopExpr)
  private
    FLeft: IExpr;
    FBinop: TBinOp;
    FRight: IExpr;
    function GetBinOp: TBinOp; inline;
    function GetLeftExpr: IExpr; inline;
    function GetRightExpr: IExpr; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const ALeft: IExpr; const ABinop: TBinOp; const ARight: IExpr);
    destructor Destroy; override;
  end;

  TUnaryExpr = class(TAbstractExpr, IUnaryExpr)
  private
    FUnaryOp: TUnaryOp;
    FExpr: IExpr;
    function GetExpr: IExpr; inline;
    function GetUnaryOp: TUnaryOp; inline;
    procedure Accept(const AVisitor: IVelocityVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AUnaryOp: TUnaryOp; const AExpr: IExpr);

  end;

  EEndOfBlock = class(Exception);

  TVelocitySymbolSet = set of TVelocitySymbol;

  TVelocityParser = class(TInterfacedObject, IVelocityParser)
  private
    FContext: IVelocityContext;
    FLookahead: IvelocitySymbol;
    FLexer: IVelocityLexer;
    FContainerStack: TStack<IVelocityTemplate>;
    FAllowEnd: boolean;
    FAllowElse: boolean;
    FAllowElIf: boolean;
    FHasElse: boolean;
    FInFor: boolean;
    FBinopPrecedents: TDictionary<TBinOp, integer>;

    function PushContainer: IVelocityTemplate;
    function PopContainer: IVelocityTemplate;
    function CurrentContainer: IVelocityTemplate; inline;

    function LookaheadValue: string;
    function matchValue(const ASymbol: TVelocitySymbol): string;
    procedure match(const ASymbol: TVelocitySymbol);
    function MatchNumber(const ASymbol: TVelocitySymbol): integer;

  private
    // stmts: (text|script)*
    procedure RuleStmts(const Container: IVelocityTemplate; const AEndToken: TVelocitySymbolSet);

    // stmt: if
    // stmt: elif
    // stmt: assign
    // stmt: variable
    // stmt: for
    // stmt: while
    // stmt: with
    // stmt: print
    // stmt: id
    // stmt: include
    // stmt: break                 # only valid in FOR/WHILE block
    // stmt: continue              # only valid in FOR/WHILE block
    // stmt: else                  # only valid in IF block
    function RuleStmt(): IStmt;

    function RuleCommentStmt: IStmt;

    // id: ID
    // id: ID := expr
    // id: ID ( exprlist )
    function ruleIdStmt: IStmt;

    // include: INCLUDE ( expr )
    function ruleIncludeStmt: IStmt;

    // print: PRINT(expr);
    function RulePrintStmt: IStmt;

    // end: END
    function ruleEndStmt: IStmt;

    // continue: CONTINUE
    function ruleContinueStmt: IStmt;

    // break: BREAK
    function ruleBreakStmt: IStmt;

    // if: IF (expr)
    function RuleIfStmt: IStmt;
    function RuleElIfStmt: IStmt;

    // exprlist: expr (, expr)*
    function RuleExprList: IExprList;

    // assign: ID := expr
    function RuleAssignStmt(const ASymbol: IExpr): IStmt;

    function RulePrintStmtVariable(const AExpr: IExpr): IStmt; overload;

    // for: FOR ID IN ID stmts END
    // for: FOR ID := expr (TO|DOWNTO) expr stmts END
    function RuleForStmt: IStmt;

    // while: WHILE (expr) stmts END
    function RuleWhileStmt: IStmt;

    // while: WITH (expr) stmts END
    function RuleWithStmt: IStmt;

    // while: template (expr) stmts END
    function RuleTemplateStmt: IStmt;

    // expr: primary (relop expr)*
    function RuleExpr(const minPrec: integer = 0): IExpr;

    // primary: ( expr )
    // primary: - expr
    // primary: NOT expr
    // primary: id
    // primary: literal
    function RulePrimaryExpr: IExpr;

    // literal: BOOL
    // literal: NUM
    // literal: STR
    function RuleLiteralExpr: IExpr;

    // id: function
    // id: ID
    // id: ID.ID
    function RuleIdentifierExpr: IExpr;

    // ID ( expr_list )
    function ruleFunctionExpr(const ASymbol: string): IExpr;

    // expr.method ( expr_list )
    function ruleMethodExpr(const AExpr: IExpr; const AMethodExpr: IExpr): IExpr;

  public
    constructor Create(Const AContext: IVelocityContext);
    destructor Destroy; override;
    function Parse(const AStream: TStream; const AManagedStream: boolean): IVelocityTemplate; overload;
    function Parse(const AString: string): IVelocityTemplate; overload;
  end;

function IsValue(const AExpr: IExpr): boolean;
begin
  result := supports(AExpr, IValueExpr);
end;

function AsValue(const AExpr: IExpr): TValue;
var
  v: IValueExpr;
begin
  AExpr.QueryInterface(IValueExpr, v);
  result := v.Value;
end;

function AsVarString(const AExpr: IExpr): string; inline;
begin
  result := (AExpr as IVariableExpr).Variable;
end;

function IsEnd(const AStmt: IStmt): boolean;
begin
  if AStmt = nil then
    exit(false);
  result := supports(AStmt, IEndStmt);
end;

function VelocityForop(const ASymbol: TVelocitySymbol): TForOp;

begin
  case ASymbol of
    vsTo:
      result := foTo;
    vsDownto:
      result := foDownto;
    vsin:
      result := foIn;
  else
    raise EParserError.Createfmt('Forop not supported: %s', [VelocitySymbolToString(ASymbol)]);
  end;
end;

function VelocityBinop(const ASymbol: TVelocitySymbol; out BinOp: TBinOp): boolean;

begin
  result := true;
  case ASymbol of
    VsAND:
      BinOp := boAND;
    vsOR:
      BinOp := boOR;
    VsPLUS:
      BinOp := boPlus;
    VsMinus:
      BinOp := boMinus;
    VsMULT:
      BinOp := boMult;
    VsDIV:
      BinOp := boDiv;
    VsMOD:
      BinOp := boMod;
    vsLT:
      BinOp := roLT;
    vsLTE:
      BinOp := roLTE;
    vsGT:
      BinOp := roGT;
    vsGTE:
      BinOp := roGTE;
    vsEQ:
      BinOp := roEQ;
    VsNotEQ:
      BinOp := roNotEQ;
  else
    result := false;
  end;
end;

function GetVelocityParser(Const AContext: IVelocityContext): IVelocityParser;
begin
  result := TVelocityParser.Create(AContext);
end;

{ TVelocityParser }

constructor TVelocityParser.Create(Const AContext: IVelocityContext);
begin
  FContext := AContext;
  FContainerStack := TStack<IVelocityTemplate>.Create;
  FBinopPrecedents := TDictionary<TBinOp, integer>.Create;

  FBinopPrecedents.Add(TBinOp.boOR, 1);
  FBinopPrecedents.Add(TBinOp.boAND, 2);

  FBinopPrecedents.Add(TBinOp.roEQ, 5);
  FBinopPrecedents.Add(TBinOp.roNotEQ, 5);

  FBinopPrecedents.Add(TBinOp.roLT, 10);
  FBinopPrecedents.Add(TBinOp.roLTE, 10);
  FBinopPrecedents.Add(TBinOp.roGT, 10);
  FBinopPrecedents.Add(TBinOp.roGTE, 10);

  FBinopPrecedents.Add(TBinOp.boPlus, 15);
  FBinopPrecedents.Add(TBinOp.boMinus, 15);

  FBinopPrecedents.Add(TBinOp.boMult, 20);
  FBinopPrecedents.Add(TBinOp.boDiv, 20);
  FBinopPrecedents.Add(TBinOp.boMod, 20);

end;

function TVelocityParser.RuleIdentifierExpr: IExpr;
var
  sym: IvelocitySymbol;

function Inspect(const AExpr: IExpr): IExpr; forward;

  function matchFunction(const AExpr: IExpr): IExpr;
  begin
    result := Inspect(ruleFunctionExpr(AsVarString(AExpr)));
  end;

  function matchArrayDeref(const AExpr: IExpr): IExpr;
  begin
    match(VsOpenSquareBracket);
    result := TVariableDerefExpr.Create(sym.Position, dtArray, AExpr, RuleExpr);
    match(VsCloseSquareBracket);
    result := Inspect(result);
  end;

  function MatchDeref(const AExpr: IExpr): IExpr;
  var
    derefexpr: IExpr;
  begin
    // aexpr . id
    // aexpr . id ( )
    match(VsDOT);
    if FLookahead.Token = VsID then
    begin
      derefexpr := TVariableExpr.Create(sym.Position, matchValue(VsID));
      if FLookahead.Token = VsOpenRoundBracket then
        result := self.ruleMethodExpr(AExpr, derefexpr)
      else
        result := TVariableDerefExpr.Create(sym.Position, dtObject, AExpr, derefexpr);

      result := Inspect(result);
      exit;
    end;
    RaiseError(Position(AExpr), 'Identified expected');
  end;

  function Inspect(const AExpr: IExpr): IExpr;
  begin
    case FLookahead.Token of
      VsOpenRoundBracket:
        result := matchFunction(AExpr);
      VsOpenSquareBracket:
        result := matchArrayDeref(AExpr);
      VsDOT:
        result := MatchDeref(AExpr);
    else
      result := AExpr;
    end;
  end;

begin
  sym := FLookahead;
  // TODO: review what we can do at parse time
  result := Inspect(TVariableExpr.Create(sym.Position, matchValue(VsID)));
end;

const
  IF_ELIF_END: TVelocitySymbolSet = [VsELIF, vsElse, vsEND];
  IF_END: TVelocitySymbolSet = [vsElse, vsEND];

function TVelocityParser.RuleIfStmt: IStmt;
var
  Condition: IExpr;
  TrueContainer: IVelocityTemplate;
  FalseContainer: IVelocityTemplate;
  ContainerAdd: ITemplateAdd;
  AllowElse: IPreserveValue<boolean>;
  HasElse: IPreserveValue<boolean>;
  AllowEnd: IPreserveValue<boolean>;
  AllowElIf: IPreserveValue<boolean>;
  symbol: IvelocitySymbol;
begin
  // TODO: review parse time evaluation. if condition is false, then block can be excluded
  AllowElse := Preseve.Value(FAllowElse, true);
  HasElse := Preseve.Value(FHasElse, false);
  AllowEnd := Preseve.Value(FAllowEnd, true);
  AllowElIf := Preseve.Value(FAllowElIf, true);
  symbol := FLookahead;
  match(VsIF);
  Condition := RuleExpr;

  match(VsEndScript);
  // create new container for true condition
  PushContainer;
  TrueContainer := self.CurrentContainer;

  RuleStmts(TrueContainer, IF_ELIF_END);
  PopContainer;

  PushContainer;
  FalseContainer := self.CurrentContainer;
  FalseContainer.QueryInterface(ITemplateAdd, ContainerAdd);

  if FLookahead.Token = VsELIF then
  begin
    while (FLookahead.Token = VsELIF) do
    begin
      ContainerAdd.Add(AsVisitorHost(RuleElIfStmt()));
    end;
  end
  else if FLookahead.Token = vsElse then
  begin
    match(vsElse);
    match(VsEndScript);
    RuleStmts(FalseContainer, [vsEND]);

  end;
  PopContainer;
  match(vsEND);
  match(VsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(Condition) then
  begin
    if AsBoolean(AsValue(Condition)) then
      result := TProcessTemplateStmt.Create(symbol.Position, TrueContainer)
    else if FalseContainer <> nil then
      result := TProcessTemplateStmt.Create(symbol.Position, FalseContainer);
  end
  else
    result := TIfStmt.Create(symbol.Position, Condition, TrueContainer, FalseContainer);
end;

function TVelocityParser.ruleIncludeStmt: IStmt;
var
  symbol: IvelocitySymbol;
  include: IExpr;
  scope: IExpr;
  Container: TTemplate;
begin
  symbol := FLookahead;
  match(vsInclude);
  match(VsOpenRoundBracket);
  include := self.RuleExpr;

  if FLookahead.Token = vsComma then
  begin
    match(vsComma);
    scope := self.RuleExpr;
  end;

  match(VsCloseRoundBracket);
  match(VsEndScript);

  if scope <> nil then
  begin
    Container := TTemplate.Create();
    Container.Add(TIncludeStmt.Create(symbol.Position, include));
    result := TWithStmt.Create(symbol.Position, scope, Container);
  end
  else
  begin
    result := TIncludeStmt.Create(symbol.Position, include);
  end;
end;

function TVelocityParser.RuleLiteralExpr: IExpr;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  case FLookahead.Token of
    vsString:
      result := TValueExpr.Create(symbol.Position, matchValue(vsString));
    vsNumber:
      result := TValueExpr.Create(symbol.Position, MatchNumber(vsNumber));
    VsBoolean:
      result := TValueExpr.Create(symbol.Position, matchValue(VsBoolean) = 'true');
  else
    RaiseError(symbol.Position, 'Literal expected');
  end;
end;

function TVelocityParser.ruleMethodExpr(const AExpr: IExpr; const AMethodExpr: IExpr): IExpr;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  match(VsOpenRoundBracket);
  result := TMethodCallExpr.Create(symbol.Position, AExpr, AsVarString(AMethodExpr), RuleExprList);
  match(VsCloseRoundBracket);
end;

procedure TVelocityParser.RuleStmts(const Container: IVelocityTemplate; const AEndToken: TVelocitySymbolSet);

var
  stmt: IStmt;
  parentContainer: ITemplateAdd;
  sym: IvelocitySymbol;
  loop: boolean;

  function AddPrintStmt: IStmt;
  var
    txt: string;
  begin
    txt := matchValue(VsText);
    if txt = '' then
      exit(nil);
    result := RulePrintStmtVariable(TValueExpr.Create(sym.Position, txt));
  end;

begin
  Container.QueryInterface(ITemplateAdd, parentContainer);
  loop := true;
  while loop do
  begin
    sym := FLookahead;
    if (sym.Token = VsEOF) or (sym.Token in AEndToken) then
      break;
    stmt := nil;
    case sym.Token of
      VsText:
        stmt := AddPrintStmt;
      VsStartScript:
        begin
          stmt := RuleStmt;
          if stmt = nil then
            loop := false;
        end;
    end;
    if (stmt <> nil) and not supports(stmt, IElseStmt) then
    begin
      parentContainer.Add(AsVisitorHost(stmt));
    end;
  end;
end;

function TVelocityParser.RuleTemplateStmt: IStmt;
var
  expr: IExpr;
  symbol: IvelocitySymbol;
  AllowEnd: IPreserveValue<boolean>;
  Container: IVelocityTemplate;
begin
  AllowEnd := Preseve.Value(FAllowEnd, true);
  symbol := FLookahead;

  match(vstemplate);
  expr := RuleExpr;
  match(VsEndScript);
  PushContainer;
  Container := CurrentContainer;

  RuleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);
  PopContainer;

  result := TDefineTemplateStmt.Create(symbol.Position, expr, Container);

end;

function TVelocityParser.RuleStmt: IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  result := nil;
  match(VsStartScript);
  case FLookahead.Token of
    vsBreak:
      result := ruleBreakStmt;
    vsContinue:
      result := ruleContinueStmt;
    vsComment:
      result := RuleCommentStmt;
    vsInclude:
      result := ruleIncludeStmt;
    vsEND:
      result := ruleEndStmt;
    vsElse: // we don't do anything
      ;
    VsIF:
      result := RuleIfStmt;
    VsELIF: // we don't do anything
      ;
    VsFor:
      result := RuleForStmt;
    vsPrint:
      result := RulePrintStmt;
    vsWhile:
      result := RuleWhileStmt;
    vswith:
      result := RuleWithStmt;
    vstemplate:
      result := RuleTemplateStmt;
    VsID:
      result := ruleIdStmt;
  else
    RaiseError(symbol.Position, 'Invalid statement');
  end;
end;

function TVelocityParser.RuleAssignStmt(const ASymbol: IExpr): IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  match(VsCOLONEQ);
  result := TAssignStmt.Create(symbol.Position, (ASymbol as IVariableExpr).Variable, RuleExpr);
end;

function TVelocityParser.ruleBreakStmt: IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  match(vsBreak);
  match(VsEndScript);
  if not FInFor then
    RaiseError(symbol.Position, 'Continue should be in a for/while Stmt');
  result := TBreakStmt.Create(symbol.Position);
end;

function TVelocityParser.RuleCommentStmt: IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  match(vsComment);
  match(VsEndScript);
  result := TCommentStmt.Create(symbol.Position);
end;

function TVelocityParser.ruleContinueStmt: IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  match(vsContinue);
  match(VsEndScript);
  if not FInFor then
    RaiseError(symbol.Position, 'Continue should be in a for/while Stmt');

  result := TContinueStmt.Create(symbol.Position);
end;

function TVelocityParser.RuleElIfStmt: IStmt;

var
  Condition: IExpr;
  TrueContainer: IVelocityTemplate;
  FalseContainer: IVelocityTemplate;

  AllowElse: IPreserveValue<boolean>;
  HasElse: IPreserveValue<boolean>;
  AllowEnd: IPreserveValue<boolean>;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  if not FAllowElIf then
    RaiseError(symbol.Position, 'ElIF expected');

  AllowElse := Preseve.Value(FAllowElse, true);
  HasElse := Preseve.Value(FHasElse, false);
  AllowEnd := Preseve.Value(FAllowEnd, true);

  match(VsELIF);

  Condition := RuleExpr;

  match(VsEndScript);
  // create new container for true condition
  PushContainer;
  TrueContainer := self.CurrentContainer;

  RuleStmts(TrueContainer, IF_ELIF_END);
  PopContainer;

  if FLookahead.Token = vsElse then
  begin

    match(vsElse);
    match(VsEndScript);

    PushContainer;
    FalseContainer := self.CurrentContainer;

    RuleStmts(FalseContainer, [vsEND, VsELIF]);

    PopContainer;
  end;

  if (eoEvalEarly in FContext.Options) and IsValue(Condition) then
  begin
    if AsBoolean(AsValue(Condition)) then
      result := TProcessTemplateStmt.Create(symbol.Position, TrueContainer)
    else if FalseContainer <> nil then
      result := TProcessTemplateStmt.Create(symbol.Position, FalseContainer);
  end
  else
    result := TIfStmt.Create(symbol.Position, Condition, TrueContainer, FalseContainer);
end;

function TVelocityParser.ruleEndStmt: IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  // NOTE: we do not match anything as we want lookahead functions to continue to work
  if not FAllowEnd then
    RaiseError(symbol.Position, 'End not expected');
  result := TEndStmt.Create(symbol.Position);
end;

function TVelocityParser.RuleExpr(const minPrec: integer): IExpr;
var
  prec: integer;
  BinOp: TBinOp;
  symbol: IvelocitySymbol;
  right: IExpr;
begin
  symbol := FLookahead;
  result := self.RulePrimaryExpr;
  // this loop is a nicer way of applying precedents rather than having more rules like traditional factor, term, etc...
  while VelocityBinop(FLookahead.Token, BinOp) do
  begin
    prec := self.FBinopPrecedents[BinOp];
    if prec < minPrec then
      break;
    match(FLookahead.Token);
    right := self.RuleExpr(prec);
    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(right) then
    begin
      case BinOp of
        boAND:
          result := TValueExpr.Create(symbol.Position, AsBoolean(AsValue(result)) and AsBoolean(AsValue(right)));
        boOR:
          result := TValueExpr.Create(symbol.Position, AsBoolean(AsValue(result)) or AsBoolean(AsValue(right)));
        boPlus:
          begin
            if isNumLike(AsValue(result)) and isNumLike(AsValue(right)) then
              result := TValueExpr.Create(symbol.Position, asnum(AsValue(result)) + asnum(AsValue(right)))
            else if isStrLike(AsValue(result)) and isStrLike(AsValue(right)) then
              result := TValueExpr.Create(symbol.Position, asString(AsValue(result)) + asString(AsValue(right)))
            else if isStrLike(AsValue(result)) and isNumLike(AsValue(right)) then
              result := TValueExpr.Create(symbol.Position, asString(AsValue(result)) + floattostr(asnum(AsValue(right))))
            else
              RaiseError(symbol.Position, 'Evaluation not supported.');
          end;
        boMinus:
          result := TValueExpr.Create(symbol.Position, asnum(AsValue(result)) - asnum(AsValue(right)));
        boMult:
          result := TValueExpr.Create(symbol.Position, asnum(AsValue(result)) * asnum(AsValue(right)));
        boDiv:
          result := TValueExpr.Create(symbol.Position, asnum(AsValue(result)) / asnum(AsValue(right)));
        boMod:
          result := TValueExpr.Create(symbol.Position, AsInt(AsValue(result)) mod AsInt(AsValue(right)));
      end;
    end
    else
      result := TBinopExpr.Create(symbol.Position, result, BinOp, right);
  end;
end;

function TVelocityParser.RuleForStmt: IStmt;
var
  id: string;
  range: IExpr;
  lowValue, highValue: IExpr;
  ForOp: TForOp;
  InFor: IPreserveValue<boolean>;
  AllowEnd: IPreserveValue<boolean>;
  AllowElIf: IPreserveValue<boolean>;
  Container: IVelocityTemplate;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  InFor := Preseve.Value(FInFor, true);
  AllowEnd := Preseve.Value(FAllowEnd, true);
  AllowElIf := Preseve.Value(FAllowElIf, false);
  match(VsFor);
  PushContainer;
  Container := CurrentContainer;
  id := matchValue(VsID);
  if FLookahead.Token = vsin then
  begin
    ForOp := VelocityForop(FLookahead.Token);
    match(vsin);
    range := RuleExpr;
  end
  else
  begin
    match(VsCOLONEQ);
    lowValue := RuleExpr();
    ForOp := VelocityForop(FLookahead.Token);
    if FLookahead.Token in [vsDownto, vsTo] then
      match(FLookahead.Token)
    else
      RaiseError(symbol.Position, 'downto/to token expected in for loop.');
    highValue := RuleExpr();
  end;

  match(VsEndScript);

  RuleStmts(Container, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  if ForOp = TForOp.foIn then
    result := TForInStmt.Create(symbol.Position, id, range, Container)
  else
    result := TForRangeStmt.Create(symbol.Position, id, ForOp, lowValue, highValue, Container);
  PopContainer;
end;

function TVelocityParser.ruleFunctionExpr(const ASymbol: string): IExpr;
var
  fn: TVelocityFunctionInfo;
  f: TFunctionCallExpr;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  if not FContext.TryGetFunction(ASymbol, fn) then
    RaiseError(symbol.Position, 'Function %s not registered in context.', [ASymbol]);
  match(VsOpenRoundBracket);
  f := TFunctionCallExpr.Create(symbol.Position, fn, RuleExprList);
  result := f;
  match(VsCloseRoundBracket);
  if (fn.MinArgs > 0) and (fn.MinArgs = fn.MaxArgs) and (f.FExprList.Count <> fn.MaxArgs) then
    RaiseError(symbol.Position, 'Function %s expects %d parameters but %d was given.', [ASymbol, fn.MinArgs, f.FExprList.Count])
  else
  begin
    if (fn.MinArgs > 0) and (f.FExprList.Count < fn.MinArgs) then
      RaiseError(symbol.Position, 'Function %s expects at least %d parameters but %d was given.', [ASymbol, fn.MinArgs, f.FExprList.Count]);
    if (fn.MaxArgs > 0) and (f.FExprList.Count > fn.MaxArgs) then
      RaiseError(symbol.Position, 'Function %s expects at most %d parameters but %d was given.', [ASymbol, fn.MinArgs, f.FExprList.Count]);
  end;
end;

function TVelocityParser.ruleIdStmt: IStmt;
var
  symbol: IvelocitySymbol;
  expr: IExpr;
  method: string;
begin
  symbol := FLookahead;
  expr := self.RuleIdentifierExpr;
  if FLookahead.Token = VsCOLONEQ then
  begin
    result := RuleAssignStmt(expr);
  end
  else
  begin
    expr := TEncodeExpr.Create(symbol.Position, expr);
    result := RulePrintStmtVariable(expr);
  end;
  match(VsEndScript);
end;

function TVelocityParser.RuleWhileStmt: IStmt;
var
  Condition: IExpr;
  InFor: IPreserveValue<boolean>;
  AllowEnd: IPreserveValue<boolean>;
  AllowElIf: IPreserveValue<boolean>;
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  InFor := Preseve.Value(FInFor, true);
  AllowEnd := Preseve.Value(FAllowEnd, true);
  AllowElIf := Preseve.Value(FAllowElIf, false);
  PushContainer;

  match(vsWhile);
  Condition := RuleExpr;
  match(VsEndScript);

  RuleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(Condition) and not AsBoolean(AsValue(Condition)) then
    result := nil
  else
    result := TWhileStmt.Create(symbol.Position, Condition, CurrentContainer);
  PopContainer;

end;

function TVelocityParser.RuleWithStmt: IStmt;
var
  expr: IExpr;
  symbol: IvelocitySymbol;
  AllowEnd: IPreserveValue<boolean>;
  Container: IVelocityTemplate;
begin
  AllowEnd := Preseve.Value(FAllowEnd, true);

  symbol := FLookahead;

  match(vswith);
  expr := RuleExpr;
  match(VsEndScript);

  PushContainer;
  Container := CurrentContainer;

  RuleStmts(Container, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  PopContainer;
  result := TWithStmt.Create(symbol.Position, expr, Container);

end;

function TVelocityParser.RulePrimaryExpr: IExpr;
var
  symbol: IvelocitySymbol;

begin
  symbol := FLookahead;
  case FLookahead.Token of
    VsOpenRoundBracket:
      begin
        match(VsOpenRoundBracket);
        result := RuleExpr();
        match(VsCloseRoundBracket);
      end;
    VsMinus:
      begin
        match(VsMinus);
        result := RuleExpr;
        if (eoEvalEarly in FContext.Options) and IsValue(result) then
          result := TValueExpr.Create(symbol.Position, -asnum(AsValue(result)))
        else
          result := TUnaryExpr.Create(symbol.Position, uoMinus, result);
      end;
    vsNot:
      begin
        match(vsNot);
        result := RuleExpr;
        if (eoEvalEarly in FContext.Options) and IsValue(result) then
          result := TValueExpr.Create(symbol.Position, not AsBoolean(AsValue(result)))
        else
          result := TUnaryExpr.Create(symbol.Position, uoNot, result);
      end;
    VsID:
      result := RuleIdentifierExpr;
  else
    result := RuleLiteralExpr;
  end;
end;

function TVelocityParser.RulePrintStmt: IStmt;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  match(vsPrint);
  result := TPrintStmt.Create(symbol.Position, RuleExpr);
end;

function TVelocityParser.RulePrintStmtVariable(const AExpr: IExpr): IStmt;
var
  symbol: IvelocitySymbol;
  val: IValueExpr;
begin
  symbol := FLookahead;
  if supports(AExpr, IValueExpr, val) and (asString(val.Value) = '') then
    exit(nil);
  result := TPrintStmt.Create(symbol.Position, AExpr);
end;

function TVelocityParser.RuleExprList: IExprList;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  result := TExprList.Create(symbol.Position);
  if FLookahead.Token <> VsCloseRoundBracket then
    result.AddExpr(RuleExpr);
  while FLookahead.Token = vsComma do
  begin
    match(vsComma);
    result.AddExpr(RuleExpr);
  end;
end;

function TVelocityParser.CurrentContainer: IVelocityTemplate;
begin
  if FContainerStack.Count <> 0 then
    result := FContainerStack.Peek;
end;

destructor TVelocityParser.Destroy;
begin
  FBinopPrecedents.Free;
  FContainerStack.Free;
  inherited;
end;

function TVelocityParser.LookaheadValue: string;
var
  val: IVelocityValueSymbol;
begin
  if FLookahead.QueryInterface(IVelocityValueSymbol, val) = 0 then
    result := val.Value
  else
    result := '';
end;

procedure TVelocityParser.match(const ASymbol: TVelocitySymbol);
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  if ASymbol = FLookahead.Token then
  begin
    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(symbol.Position, format('Parsing error expecting %s', [VelocitySymbolToString(ASymbol)]));
end;

function TVelocityParser.MatchNumber(const ASymbol: TVelocitySymbol): integer;
begin
  result := strtoint(matchValue(ASymbol));
end;

function TVelocityParser.matchValue(const ASymbol: TVelocitySymbol): string;
var
  symbol: IvelocitySymbol;
begin
  symbol := FLookahead;
  if ASymbol = FLookahead.Token then
  begin
    result := LookaheadValue;
    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(symbol.Position, format('Parsing error expecting %s', [VelocitySymbolToString(ASymbol)]));
end;

function TVelocityParser.Parse(const AString: string): IVelocityTemplate;
begin
  result := Parse(tstringstream.Create(AString), true);
end;

function TVelocityParser.Parse(const AStream: TStream; const AManagedStream: boolean): IVelocityTemplate;
begin
  FContainerStack.Clear;
  PushContainer;
  FLexer := CreateVelocityLexer(FContext, AStream, '', AManagedStream);
  FLookahead := FLexer.GetToken;
  RuleStmts(CurrentContainer, []);
  match(VsEOF);
  result := CurrentContainer;
  if eoDebug in FContext.Options then
    writeln(Velocity.PrettyPrint(result));
end;

function TVelocityParser.PopContainer: IVelocityTemplate;
begin
  result := self.CurrentContainer;
  FContainerStack.Pop;
end;

function TVelocityParser.PushContainer: IVelocityTemplate;
begin
  result := CurrentContainer;
  FContainerStack.Push(TTemplate.Create());
end;

{ TValueExpr }

procedure TValueExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TValueExpr.Create(const APosition: IPosition; const AValue: TValue);
begin
  inherited Create(APosition);
  FValue := AValue;
end;

function TValueExpr.GetValue: TValue;
begin
  result := FValue;
end;

{ TExprList }

procedure TExprList.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

procedure TExprList.AddExpr(const AExpr: IExpr);
begin
  insert(AExpr, FExprs, length(FExprs));
end;

function TExprList.GetExpr(const AOffset: integer): IExpr;
begin
  result := FExprs[AOffset];
end;

function TExprList.GetExprCount: integer;
begin
  result := length(FExprs);
end;

{ TUnaryExpr }

procedure TUnaryExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TUnaryExpr.Create(const APosition: IPosition; const AUnaryOp: TUnaryOp; const AExpr: IExpr);
begin
  inherited Create(APosition);
  FUnaryOp := AUnaryOp;
  FExpr := AExpr;
end;

function TUnaryExpr.GetExpr: IExpr;
begin
  result := FExpr;
end;

function TUnaryExpr.GetUnaryOp: TUnaryOp;
begin
  result := FUnaryOp;
end;

{ TVariableExpr }

procedure TVariableExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TVariableExpr.Create(const APosition: IPosition; const AVariable: string);
begin
  inherited Create(APosition);
  FVariable := AVariable;
end;

function TVariableExpr.GetVariable: string;
begin
  result := FVariable;
end;

{ TFunctionCallExpr }

procedure TFunctionCallExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TFunctionCallExpr.Create(const APosition: IPosition; AFunction: TVelocityFunctionInfo; const ExprList: IExprList);

begin
  inherited Create(APosition);
  FFunctionInfo := AFunction;
  FExprList := ExprList;
end;

function TFunctionCallExpr.Invoke(const AArgs: TArray<TValue>): TValue;
begin
  result := FFunctionInfo.fn(AArgs);
end;

function TFunctionCallExpr.GetExprList: IExprList;
begin
  result := FExprList;
end;

function TFunctionCallExpr.GetFunctionInfo: TVelocityFunctionInfo;
begin
  result := FFunctionInfo;
end;

{ TIfStmt }

procedure TIfStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TIfStmt.Create(const APosition: IPosition; const ACondition: IExpr; const ATrueContainer, AFalseContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FCondition := ACondition;
  FTrueContainer := ATrueContainer;
  FFalseContainer := AFalseContainer;
end;

function TIfStmt.GetCondition: IExpr;
begin
  result := FCondition;
end;

function TIfStmt.GetFalseContainer: IVelocityTemplate;
begin
  result := FFalseContainer;
end;

function TIfStmt.GetTrueContainer: IVelocityTemplate;
begin
  result := FTrueContainer;
end;

{ TBinopExpr }

procedure TBinopExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TBinopExpr.Create(const APosition: IPosition; const ALeft: IExpr; const ABinop: TBinOp; const ARight: IExpr);
begin
  inherited Create(APosition);
  FLeft := ALeft;
  FBinop := ABinop;
  FRight := ARight;
end;

destructor TBinopExpr.Destroy;
begin
  FLeft := nil;
  FRight := nil;
end;

function TBinopExpr.GetBinOp: TBinOp;
begin
  result := FBinop;
end;

function TBinopExpr.GetLeftExpr: IExpr;
begin
  result := FLeft;
end;

function TBinopExpr.GetRightExpr: IExpr;
begin
  result := FRight;
end;

{ TPrintStmt }

procedure TPrintStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TPrintStmt.Create(const APosition: IPosition; const AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TPrintStmt.GetExpr: IExpr;
begin
  result := FExpr;
end;

{ TForInStmt }

procedure TForInStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TForInStmt.Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr; const AContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FVariable := AVariable;
  FExpr := AExpr;
  FContainer := AContainer;
end;

function TForInStmt.GetContainer: IVelocityTemplate;
begin
  result := FContainer;
end;

function TForInStmt.GetExpr: IExpr;
begin
  result := FExpr;
end;

function TForInStmt.GetVariable: string;
begin
  result := FVariable;
end;

{ TForRangeStmt }

procedure TForRangeStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TForRangeStmt.Create(const APosition: IPosition; const AVariable: string; const AForIp: TForOp; const ALowExpr, AHighExpr: IExpr; const AContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FVariable := AVariable;
  FForIp := AForIp;
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
  FContainer := AContainer;
end;

function TForRangeStmt.GetContainer: IVelocityTemplate;
begin
  result := FContainer;
end;

function TForRangeStmt.GetForOp: TForOp;
begin
  result := FForIp;
end;

function TForRangeStmt.GetHighExpr: IExpr;
begin
  result := FHighExpr;
end;

function TForRangeStmt.GetLowExpr: IExpr;
begin
  result := FLowExpr;
end;

function TForRangeStmt.GetVariable: string;
begin
  result := FVariable;
end;

{ TAssignStmt }

procedure TAssignStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TAssignStmt.Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr);
begin
  inherited Create(APosition);
  FVariable := AVariable;
  FExpr := AExpr;
end;

function TAssignStmt.GetExpr: IExpr;
begin
  result := FExpr;
end;

function TAssignStmt.GetVariable: string;
begin
  result := FVariable;
end;

{ TTemplateContainer }

procedure TTemplate.Accept(const AVisitor: IVelocityVisitor);
var
  i: IVelocityVisitorHost;
begin
  for i in FArray do
    i.Accept(AVisitor);
end;

procedure TTemplate.Add(const AItem: IVelocityVisitorHost);
begin
  insert(AItem, FArray, length(FArray));
end;

function TTemplate.GetCount: integer;
begin
  result := length(FArray)
end;

function TTemplate.GetLastItem: IVelocityVisitorHost;
begin
  if GetCount = 0 then
    exit(nil);
  result := GetItem(GetCount - 1);
end;

function TTemplate.GetItem(const AOffset: integer): IVelocityVisitorHost;
begin
  result := FArray[AOffset];
end;

{ TWhileStmt }

procedure TWhileStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TWhileStmt.Create(const APosition: IPosition; const ACondition: IExpr; const AContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FCondition := ACondition;
  FContainer := AContainer;
end;

function TWhileStmt.GetCondition: IExpr;
begin
  result := FCondition;
end;

function TWhileStmt.GetContainer: IVelocityTemplate;
begin
  result := FContainer;
end;

{ TContinueStmt }

procedure TContinueStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

{ TBreakStmt }

procedure TBreakStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

{ TEndStmt }

procedure TEndStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

{ TVariableDerefExpr }

procedure TVariableDerefExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TVariableDerefExpr.Create(const APosition: IPosition; const ADerefType: TDerefType; const AVariable: IExpr; const ADeref: IExpr);
begin
  inherited Create(APosition);
  FDerefType := ADerefType;
  FVariable := AVariable;
  FDeref := ADeref;
end;

function TVariableDerefExpr.GetDerefExpr: IExpr;
begin
  result := FDeref;
end;

function TVariableDerefExpr.GetDerefType: TDerefType;
begin
  result := FDerefType;
end;

function TVariableDerefExpr.GetVariable: IExpr;
begin
  result := FVariable;
end;

{ TIncludeStmt }

procedure TIncludeStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TIncludeStmt.Create(const APosition: IPosition; const AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TIncludeStmt.GetExpr: IExpr;
begin
  result := FExpr;
end;

function CreateVelocityParser(Const AContext: IVelocityContext): IVelocityParser;
begin
  result := TVelocityParser.Create(AContext);
end;

{ TElseStmt }

procedure TElseStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

{ TElIfStmt }

procedure TElIfStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  // AVisitor.Visit(self);
end;

{ TCommentStmt }

procedure TCommentStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  // AVisitor.Visit(self);
end;

{ TAbstractBase }

constructor TAbstractBase.Create(const APosition: IPosition);
begin
  FPosition := APosition;
end;

destructor TAbstractBase.Destroy;
begin
  FPosition := nil;
  inherited;
end;

function TAbstractBase.GetPosition: IPosition;
begin
  result := FPosition;
end;

{ TMethodCallExpr }

procedure TMethodCallExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TMethodCallExpr.Create(const APosition: IPosition; const AObjectExpr: IExpr; const AMethod: string; const AExprList: IExprList);
begin
  FPosition := APosition;
  FObjectExpr := AObjectExpr;
  FMethod := AMethod;
  FExprList := AExprList;
end;

function TMethodCallExpr.GetExprList: IExprList;
begin
  result := FExprList;
end;

function TMethodCallExpr.GetMethod: string;
begin
  result := FMethod;
end;

function TMethodCallExpr.GetObject: IExpr;
begin
  result := FObjectExpr;
end;

function TMethodCallExpr.Invoke(const AObject: TObject; const AArgs: TArray<TValue>): TValue;
var
  RttiType: TRttiType;
begin
  if FRttiMethod = nil then
  begin
    RttiType := GRttiContext.GetType(AObject.ClassType);
    FRttiMethod := RttiType.GetMethod(FMethod);
  end;
  result := FRttiMethod.Invoke(AObject, AArgs);
end;

{ TEncodeStmt }

procedure TEncodeExpr.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TEncodeExpr.Create(const APosition: IPosition; const AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TEncodeExpr.GetExpr: IExpr;
begin
  result := FExpr;
end;

{ TProcessTemplateStmt }

procedure TProcessTemplateStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TProcessTemplateStmt.Create(const APosition: IPosition; const AContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FContainer := AContainer;
end;

function TProcessTemplateStmt.GetContainer: IVelocityTemplate;
begin
  result := FContainer;
end;

{ TDefineTemplateStmt }

procedure TDefineTemplateStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TDefineTemplateStmt.Create(const APosition: IPosition; const AName: IExpr; const AContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FName := AName;
  FContainer := AContainer;
end;

function TDefineTemplateStmt.GetName: IExpr;
begin
  result := FName;
end;

function TDefineTemplateStmt.GetContainer: IVelocityTemplate;
begin
  result := FContainer;
end;

{ TWithStmt }

procedure TWithStmt.Accept(const AVisitor: IVelocityVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TWithStmt.Create(const APosition: IPosition; const AExpr: IExpr; const AContainer: IVelocityTemplate);
begin
  inherited Create(APosition);
  FExpr := AExpr;
  FContainer := AContainer;
end;

function TWithStmt.GetContainer: IVelocityTemplate;
begin
  result := FContainer;
end;

function TWithStmt.GetExpr: IExpr;
begin
  result := FExpr;
end;

end.
