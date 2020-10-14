(*%*********************************************************************************
 *                 ___                                                             *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                     *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                    *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                    *
 *                                    |_|                                          *
 ***********************************************************************************
 *                                                                                 *
 *                        Sempare Templating Engine                                *
 *                                                                                 *
 *                                                                                 *
 *               https://github.com/sempare/sempare.template                       *
 ***********************************************************************************
 *                                                                                 *
 * Copyright (c) 2020 Sempare Limited,                                             *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>                *
 *                                                                                 *
 * Contact: info@sempare.ltd                                                       *
 *                                                                                 *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License            *
 * You may not use this file except in compliance with one of these Licenses.      *
 * You may obtain a copy of the Licenses at                                        *
 *                                                                                 *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                    *
 * https://github.com/sempare/sempare.template/docs/commercial.license.md          *
 *                                                                                 *
 * Unless required by applicable law or agreed to in writing, software             *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,              *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.        *
 * See the License for the specific language governing permissions and             *
 * limitations under the License.                                                  *
 *                                                                                 *
 ********************************************************************************%*)
unit Sempare.Template.Parser;

interface

uses
  System.Classes,
  Sempare.Template.AST,
  Sempare.Template.Context;

type
  ITemplateParser = interface
    ['{DAF8A08D-9158-4D2C-9E76-BE80E9DA50A3}']

    function Parse(const AStream: TStream; const AManagedStream: boolean = true): ITemplate;
  end;

function CreateTemplateParser(AContext: ITemplateContext): ITemplateParser;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Rtti,
  System.Generics.Collections,
  Sempare.Template,
  Sempare.Template.PrettyPrint,
  Sempare.Template.Evaluate,
  Sempare.Template.Common,
  Sempare.Template.Lexer,
  Sempare.Template.Rtti,
  Sempare.Template.Util;

type

  TTemplate = class(TInterfacedObject, ITemplate, ITemplateAdd, ITemplateVisitorHost)
  private
    FArray: TArray<ITemplateVisitorHost>;
    function GetItem(const AOffset: integer): ITemplateVisitorHost;
    function GetCount: integer;
    procedure Add(AItem: ITemplateVisitorHost);
    function GetLastItem: ITemplateVisitorHost;

    procedure Accept(AVisitor: ITemplateVisitor);
  public

  end;

  TAbstractBase = class abstract(TInterfacedObject, IPositional, ITemplateVisitorHost)
  private
    FPosition: IPosition;
    function GetPosition: IPosition;
  public
    constructor Create(APosition: IPosition);
    destructor Destroy; override;
    procedure Accept(AVisitor: ITemplateVisitor); virtual; abstract;
  end;

  TAbstractStmt = class abstract(TAbstractBase, IStmt)
  end;

  TEndStmt = class(TAbstractStmt, IEndStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TElseStmt = class(TAbstractStmt, IElseStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TContinueStmt = class(TAbstractStmt, IContinueStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TBreakStmt = class(TAbstractStmt, IBreakStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TCommentStmt = class(TAbstractStmt, ICommentStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TElIfStmt = class(TAbstractStmt, IElIfStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TAbstractStmtWithExpr = class abstract(TAbstractStmt)
  private
    FExpr: IExpr;
    function GetExpr: IExpr;
  public
    constructor Create(APosition: IPosition; AExpr: IExpr);
  end;

  TPrintStmt = class(TAbstractStmtWithExpr, IPrintStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TIncludeStmt = class(TAbstractStmtWithExpr, IIncludeStmt)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TRequireStmt = class(TAbstractStmt, IRequireStmt)
  private
    FExprList: IExprList;
    function GetExprList: IExprList;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; AExprList: IExprList);
  end;

  TIfStmt = class(TAbstractStmt, IIfStmt)
  private
    FCondition: IExpr;
    FTrueContainer: ITemplate;
    FFalseContainer: ITemplate;
    function GetCondition: IExpr;
    function GetTrueContainer: ITemplate;
    function GetFalseContainer: ITemplate;

    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; ACondition: IExpr; ATrueContainer: ITemplate; AFalseContainer: ITemplate);
  end;

  TAbstractStmtWithContainer = class abstract(TAbstractStmt)
  private
    FContainer: ITemplate;
    function GetContainer: ITemplate;
  public
    constructor Create(APosition: IPosition; AContainer: ITemplate);
  end;

  TProcessTemplateStmt = class(TAbstractStmtWithContainer, IProcessTemplateStmt)
  private
    FAllowNewline: boolean;
    function GetAllowNewLine: boolean;
    procedure SetAllowNewLine(const AAllow: boolean);
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; AContainer: ITemplate; const AAllowNewLine: boolean = true);
  end;

  TDefineTemplateStmt = class(TAbstractStmtWithContainer, IDefineTemplateStmt)
  private
    FName: IExpr;
    function GetName: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; AName: IExpr; AContainer: ITemplate);
  end;

  TWithStmt = class(TAbstractStmtWithContainer, IWithStmt)
  private
    FExpr: IExpr;
    function GetExpr: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; AExpr: IExpr; AContainer: ITemplate);
  end;

  TWhileStmt = class(TAbstractStmtWithContainer, IWhileStmt)
  private
    FCondition: IExpr;
    function GetCondition: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; ACondition: IExpr; AContainer: ITemplate);
  end;

  TForInStmt = class(TAbstractStmtWithContainer, IForInStmt)
  private
    FVariable: string;
    FExpr: IExpr;
    function GetVariable: string;
    function GetExpr: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const AVariable: string; AExpr: IExpr; AContainer: ITemplate);
  end;

  TForRangeStmt = class(TAbstractStmtWithContainer, IForRangeStmt)
  private
    FVariable: string;
    FForIp: TForOp;
    FLowExpr: IExpr;
    FHighExpr: IExpr;
    function GetVariable: string;
    function GetForOp: TForOp;
    function GetLowExpr: IExpr;
    function GetHighExpr: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const AVariable: string; const AForIp: TForOp; ALowExpr: IExpr; AHighExpr: IExpr; AContainer: ITemplate);
  end;

  TAssignStmt = class(TAbstractStmtWithExpr, IAssignStmt)
  private
    FVariable: string;
    function GetVariable: string;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const AVariable: string; AExpr: IExpr);
  end;

  TExprList = class(TAbstractBase, IExprList)
  private
    FExprs: TArray<IExpr>;
    function GetExpr(const AOffset: integer): IExpr;
    procedure AddExpr(AExpr: IExpr);
    function GetExprCount: integer;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
  end;

  TAbstractExpr = class abstract(TAbstractBase, IExpr)
  end;

  TValueExpr = class(TAbstractExpr, IValueExpr)
  private
    FValue: TValue;
    function GetValue: TValue;
    procedure Accept(AVisitor: ITemplateVisitor); override;

  public
    constructor Create(APosition: IPosition; const AValue: TValue);
  end;

  TAbstractExprWithExprList = class abstract(TAbstractExpr)
  private
    FExprList: IExprList;
    function GetExprList: IExprList;
  public
    constructor Create(APosition: IPosition; AExprList: IExprList);
  end;

  TAbstractExprWithExpr = class abstract(TAbstractExpr)
  private
    FExpr: IExpr;
    function GetExpr: IExpr;
  public
    constructor Create(APosition: IPosition; AExpr: IExpr);
  end;

  TArrayExpr = class(TAbstractExprWithExprList, IArrayExpr)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TVariableExpr = class(TAbstractExpr, IVariableExpr)
  private
    FVariable: string;
    function GetVariable: string;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const AVariable: string);
  end;

  TEncodeExpr = class(TAbstractExprWithExpr, IEncodeExpr)
  private
    procedure Accept(AVisitor: ITemplateVisitor); override;
  end;

  TVariableDerefExpr = class(TAbstractExprWithExpr, IVariableDerefExpr)
  private
    FDeref: IExpr;
    FDerefType: TDerefType;
    function GetDerefType: TDerefType;
    function GetDerefExpr: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const ADerefType: TDerefType; AVariable: IExpr; ADeref: IExpr);
  end;

  TFunctionCallExpr = class(TAbstractExprWithExprList, IFunctionCallExpr)
  private
    FFunctionInfo: TArray<TRttiMethod>;
    function GetFunctionInfo: TArray<TRttiMethod>;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; AFunction: TArray<TRttiMethod>; ExprList: IExprList);
  end;

  TMethodCallExpr = class(TAbstractExprWithExprList, IMethodCallExpr)
  private
    FObjectExpr: IExpr;
    FMethod: string;
    FRttiMethod: TRttiMethod;
    function GetMethod: string;
    function GetObject: IExpr;
    function GetRttiMethod: TRttiMethod;
    procedure SetRttiMethod(const ARttiMethod: TRttiMethod);
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; AObjectExpr: IExpr; const AMethod: string; AExprList: IExprList);
  end;

  TBinopExpr = class(TAbstractExpr, IBinopExpr)
  private
    FLeft: IExpr;
    FBinop: TBinOp;
    FRight: IExpr;
    function GetBinOp: TBinOp;
    function GetLeftExpr: IExpr;
    function GetRightExpr: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; ALeft: IExpr; const ABinop: TBinOp; ARight: IExpr);
    destructor Destroy; override;
  end;

  TTernaryExpr = class(TAbstractExprWithExpr, ITernaryExpr)
  private
    FTrueExpr: IExpr;
    FFalseExpr: IExpr;
    function GetTrueExpr: IExpr;
    function GetFalseExpr: IExpr;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; ACondition: IExpr; ATrueExpr: IExpr; AFalseExpr: IExpr);
  end;

  TUnaryExpr = class(TAbstractExprWithExpr, IUnaryExpr)
  private
    FUnaryOp: TUnaryOp;
    function GetUnaryOp: TUnaryOp;
    procedure Accept(AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const AUnaryOp: TUnaryOp; ACondition: IExpr);
  end;

  EEndOfBlock = class(Exception);

  TTemplateSymbolSet = set of TTemplateSymbol;

  TParserOption = (poAllowEnd, poAllowElse, poAllowElIf, poHasElse, poInLoop);
  TParserOptions = set of TParserOption;

  TTemplateParser = class(TInterfacedObject, ITemplateParser)
  private
    FContext: ITemplateContext;
    FLookahead: ITemplateSymbol;
    FLexer: ITemplateLexer;
    FContainerStack: TStack<ITemplate>;
    FOptions: TParserOptions;

    function PushContainer: ITemplate;
    function PopContainer: ITemplate;
    function CurrentContainer: ITemplate;

    function lookaheadValue: string;
    function matchValue(const ASymbol: TTemplateSymbol): string;
    procedure match(ASymbol: ITemplateSymbol); overload; inline;
    procedure match(const ASymbol: TTemplateSymbol); overload;
    function matchNumber(const ASymbol: TTemplateSymbol): extended;

  private
    procedure ruleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet);
    function ruleStmt: IStmt;
    function ruleIgnoreNewline: IStmt;
    function ruleCommentStmt: IStmt;
    function ruleIdStmt: IStmt;
    function ruleExprStmt: IStmt;
    function ruleIncludeStmt: IStmt;
    function rulePrintStmt: IStmt;
    function ruleEndStmt: IStmt;
    function ruleContinueStmt: IStmt;
    function ruleBreakStmt: IStmt;
    function ruleIfStmt: IStmt;
    function ruleElIfStmt: IStmt;
    function ruleExprList(const AEndToken: TTemplateSymbol = VsCloseRoundBracket): IExprList;
    function ruleAssignStmt(ASymbol: IExpr): IStmt;
    function rulePrintStmtVariable(AExpr: IExpr): IStmt; overload;
    function ruleForStmt: IStmt;
    function ruleWhileStmt: IStmt;
    function ruleWithStmt: IStmt;
    function ruleTemplateStmt: IStmt;

    function ruleExpression: IExpr;
    function ruleSimpleExpression: IExpr;
    function ruleTerm: IExpr;
    function ruleSignedFactor: IExpr;
    function ruleFactor: IExpr;
    function ruleVariable: IExpr;

    function ruleFunctionExpr(const ASymbol: string): IExpr;
    function ruleMethodExpr(AExpr: IExpr; AMethodExpr: IExpr): IExpr;
    function ruleRequireStmt: IStmt;
  public
    constructor Create(AContext: ITemplateContext);
    destructor Destroy; override;
    function Parse(const AStream: TStream; const AManagedStream: boolean): ITemplate;
  end;

function CreateTemplateParser(AContext: ITemplateContext): ITemplateParser;
begin
  result := TTemplateParser.Create(AContext);
end;

function IsValue(AExpr: IExpr): boolean;
begin
  result := supports(AExpr, IValueExpr);
end;

function AsValue(AExpr: IExpr): TValue;
var
  v: IValueExpr;
begin
  AExpr.QueryInterface(IValueExpr, v);
  result := v.Value;
end;

function AsVarString(AExpr: IExpr): string;
begin
  result := (AExpr as IVariableExpr).Variable;
end;

function IsEnd(AStmt: IStmt): boolean;
begin
  if AStmt = nil then
    exit(false);
  result := supports(AStmt, IEndStmt);
end;

function TemplateForop(const ASymbol: TTemplateSymbol): TForOp;

begin
  case ASymbol of
    vsTo:
      result := foTo;
    vsDownto:
      result := foDownto;
    vsin:
      result := foIn;
  else
    raise EParserError.Createfmt('Forop not supported: %s', [TemplateSymbolToString(ASymbol)]);
  end;
end;

const
  pInvalid: byte = 255;

var
  GTemplateBinOps: array [TTemplateSymbol] of TBinOp;

function TemplateBinop(const ASymbol: TTemplateSymbol; out BinOp: TBinOp): boolean;

begin
  BinOp := GTemplateBinOps[ASymbol];
  result := BinOp <> boInvalid;
end;

function GetTemplateParser(AContext: ITemplateContext): ITemplateParser;
begin
  result := TTemplateParser.Create(AContext);
end;

{ TTemplateParser }

constructor TTemplateParser.Create(AContext: ITemplateContext);
begin
  FOptions := [];
  FContext := AContext;
  FContainerStack := TStack<ITemplate>.Create;
end;

const
  IF_ELIF_END: TTemplateSymbolSet = [VsELIF, vsElse, vsEND];
  IF_END: TTemplateSymbolSet = [vsElse, vsEND];

function TTemplateParser.ruleIfStmt: IStmt;
var
  Condition: IExpr;
  TrueContainer: ITemplate;
  FalseContainer: ITemplate;
  ContainerAdd: ITemplateAdd;
  Options: IPreserveValue<TParserOptions>;
  symbol: ITemplateSymbol;
begin
  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poAllowElse, poAllowEnd, poAllowElIf]);
  symbol := FLookahead;
  match(VsIF);
  Condition := ruleExpression;

  match(VsEndScript);
  // create new container for true condition
  PushContainer;
  TrueContainer := self.CurrentContainer;

  ruleStmts(TrueContainer, IF_ELIF_END);
  PopContainer;

  PushContainer;
  FalseContainer := self.CurrentContainer;
  FalseContainer.QueryInterface(ITemplateAdd, ContainerAdd);

  if FLookahead.Token = VsELIF then
  begin
    while (FLookahead.Token = VsELIF) do
    begin
      ContainerAdd.Add(AsVisitorHost(ruleElIfStmt()));
    end;
  end
  else if FLookahead.Token = vsElse then
  begin
    match(vsElse);
    match(VsEndScript);
    ruleStmts(FalseContainer, [vsEND]);

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

function TTemplateParser.ruleIgnoreNewline: IStmt;
var
  symbol: ITemplateSymbol;
  Container: ITemplate;
  Options: IPreserveValue<TParserOptions>;
begin
  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  symbol := FLookahead;
  match(vsIgnoreNL);
  match(VsEndScript);
  PushContainer;
  Container := CurrentContainer;

  ruleStmts(Container, [vsEND]);

  match(vsEND);
  match(VsEndScript);
  PopContainer;

  result := TProcessTemplateStmt.Create(symbol.Position, Container, false);
end;

function TTemplateParser.ruleIncludeStmt: IStmt;
var
  symbol: ITemplateSymbol;
  include: IExpr;
  scope: IExpr;
  Container: TTemplate;
begin
  symbol := FLookahead;
  match(vsInclude);
  match(VsOpenRoundBracket);
  include := ruleExpression;

  if FLookahead.Token = vsComma then
  begin
    match(vsComma);
    scope := ruleExpression;
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

function TTemplateParser.ruleRequireStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  match(vsRequire);
  match(VsOpenRoundBracket);
  result := TRequireStmt.Create(symbol.Position, self.ruleExprList());
  match(VsCloseRoundBracket);
  match(VsEndScript);
end;

function TTemplateParser.ruleMethodExpr(AExpr: IExpr; AMethodExpr: IExpr): IExpr;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  match(VsOpenRoundBracket);
  result := TMethodCallExpr.Create(symbol.Position, AExpr, AsVarString(AMethodExpr), ruleExprList);
  match(VsCloseRoundBracket);
end;

procedure TTemplateParser.ruleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet);

var
  stmt: IStmt;
  parentContainer: ITemplateAdd;
  sym: ITemplateSymbol;
  loop: boolean;

  function AddPrintStmt: IStmt;
  var
    txt: string;
  begin
    txt := matchValue(VsText);
    if txt = '' then
      exit(nil);
    result := rulePrintStmtVariable(TValueExpr.Create(sym.Position, txt));
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
          stmt := ruleStmt;
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

function TTemplateParser.ruleTemplateStmt: IStmt;
var
  expr: IExpr;
  symbol: ITemplateSymbol;
  Options: IPreserveValue<TParserOptions>;
  Container: ITemplate;
begin
  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);
  symbol := FLookahead;

  match(vstemplate);
  expr := ruleExpression;
  match(VsEndScript);
  PushContainer;
  Container := CurrentContainer;

  ruleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);
  PopContainer;

  result := TDefineTemplateStmt.Create(symbol.Position, expr, Container);

end;

function TTemplateParser.ruleSignedFactor: IExpr;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;

  if symbol.Token in [VsMinus, VsPLUS] then
  begin
    match(symbol.Token);
  end;

  result := ruleFactor;
  if symbol.Token = VsMinus then
  begin
    result := ruleFactor;
    if (eoEvalEarly in FContext.Options) and IsValue(result) then
      result := TValueExpr.Create(symbol.Position, -asnum(AsValue(result)))
    else
      result := TUnaryExpr.Create(symbol.Position, uoMinus, result);
  end;
end;

function TTemplateParser.ruleSimpleExpression: IExpr;
var
  right: IExpr;
  BinOp: TBinOp;
  symbol: ITemplateSymbol;
begin
  result := ruleTerm();
  symbol := FLookahead;
  if symbol.Token in [VsPLUS, VsMinus, VsOR] then
  begin
    TemplateBinop(symbol.Token, BinOp);
    match(symbol.Token);
    right := ruleSimpleExpression;
    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(right) then
    begin
      case BinOp of
        boOR:
          exit(TValueExpr.Create(symbol.Position, AsBoolean(AsValue(result)) or AsBoolean(AsValue(right))));
        boPlus:
          begin
            if isNumLike(AsValue(result)) and isNumLike(AsValue(right)) then
              exit(TValueExpr.Create(symbol.Position, asnum(AsValue(result)) + asnum(AsValue(right))))
            else if isStrLike(AsValue(result)) and isStrLike(AsValue(right)) then
              exit(TValueExpr.Create(symbol.Position, asString(AsValue(result)) + asString(AsValue(right))))
            else if isStrLike(AsValue(result)) and isNumLike(AsValue(right)) then
              exit(TValueExpr.Create(symbol.Position, asString(AsValue(result)) + floattostr(asnum(AsValue(right)))))
            else
              RaiseError(symbol.Position, 'Evaluation not supported.');
          end;
        boMinus:
          exit(TValueExpr.Create(symbol.Position, asnum(AsValue(result)) - asnum(AsValue(right))));
      end;
    end;
    result := TBinopExpr.Create(symbol.Position, result, BinOp, right);
  end;
end;

function TTemplateParser.ruleTerm: IExpr;
var
  right: IExpr;
  BinOp: TBinOp;
  symbol: ITemplateSymbol;
begin
  result := ruleSignedFactor;
  symbol := FLookahead;
  if symbol.Token in [VsMULT, VsDIV, vsSLASH, VsMOD, VsAND] then
  begin
    TemplateBinop(symbol.Token, BinOp);
    match(symbol.Token);
    right := ruleTerm;

    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(right) then
    begin
      case BinOp of
        boAND:
          exit(TValueExpr.Create(symbol.Position, AsBoolean(AsValue(result)) and AsBoolean(AsValue(right))));
        boMult:
          exit(TValueExpr.Create(symbol.Position, asnum(AsValue(result)) * asnum(AsValue(right))));
        boDiv:
          exit(TValueExpr.Create(symbol.Position, trunc(asnum(AsValue(result))) div trunc(asnum(AsValue(right)))));
        boSlash:
          exit(TValueExpr.Create(symbol.Position, asnum(AsValue(result)) / asnum(AsValue(right))));
        boMod:
          exit(TValueExpr.Create(symbol.Position, AsInt(AsValue(result)) mod AsInt(AsValue(right))));
      end;
    end;

    result := TBinopExpr.Create(symbol.Position, result, BinOp, right);
  end;

end;

function TTemplateParser.ruleStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  result := nil;
  match(VsStartScript);
  case FLookahead.Token of
    VsEndScript: // we don't do anything
      ;
    vsBreak:
      result := ruleBreakStmt;
    vsContinue:
      result := ruleContinueStmt;
    vsIgnoreNL:
      result := ruleIgnoreNewline;
    vsComment:
      result := ruleCommentStmt;
    vsInclude:
      result := ruleIncludeStmt;
    vsEND:
      result := ruleEndStmt;
    vsElse: // we don't do anything
      ;
    VsIF:
      result := ruleIfStmt;
    VsELIF: // we don't do anything
      ;
    VsFor:
      result := ruleForStmt;
    vsPrint:
      result := rulePrintStmt;
    vsWhile:
      result := ruleWhileStmt;
    vswith:
      result := ruleWithStmt;
    vsRequire:
      result := ruleRequireStmt;
    vstemplate:
      result := ruleTemplateStmt;
    VsID:
      result := ruleIdStmt;
  else
    result := ruleExprStmt;
  end;
end;

function TTemplateParser.ruleVariable: IExpr;
var
  Variable: string;
  VarVal: TValue;
  symbol: ITemplateSymbol;
  expr: IExpr;
  done: boolean;
begin
  done := false;
  symbol := FLookahead;
  Variable := matchValue(VsID);
  if (eoEvalVarsEarly in FContext.Options) and FContext.TryGetVariable(Variable, VarVal) then
    result := TValueExpr.Create(symbol.Position, VarVal)
  else
    result := TVariableExpr.Create(symbol.Position, Variable);
  while not done do
  begin
    symbol := FLookahead;
    case symbol.Token of
      VsOpenRoundBracket:
        begin
          result := self.ruleFunctionExpr(Variable);
        end;
      VsOpenSquareBracket:
        begin
          match(VsOpenSquareBracket);
          expr := self.ruleExpression;
          if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(expr) then
            result := TValueExpr.Create(symbol.Position, deref(symbol.Position, AsValue(result), AsValue(expr), eoRaiseErrorWhenVariableNotFound in FContext.Options))
          else
            result := TVariableDerefExpr.Create(symbol.Position, dtArray, result, expr);
          match(VsCloseSquareBracket);
        end;
      VsDOT:
        begin
          match(VsDOT);
          expr := TVariableExpr.Create(symbol.Position, matchValue(VsID));
          if FLookahead.Token = VsOpenRoundBracket then
            result := ruleMethodExpr(result, expr)
          else
          begin
            if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(expr) then
              result := TValueExpr.Create(symbol.Position, deref(symbol.Position, AsValue(result), AsValue(expr), eoRaiseErrorWhenVariableNotFound in FContext.Options))
            else
              result := TVariableDerefExpr.Create(symbol.Position, dtObject, result, expr);
          end;
        end;
    else
      done := true;
    end;
    if done then
      break;
  end;
end;

function TTemplateParser.ruleAssignStmt(ASymbol: IExpr): IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  match(VsCOLONEQ);
  result := TAssignStmt.Create(symbol.Position, (ASymbol as IVariableExpr).Variable, ruleExpression);
end;

function TTemplateParser.ruleBreakStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  match(vsBreak);
  match(VsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(symbol.Position, 'Continue should be in a for/while Stmt');
  result := TBreakStmt.Create(symbol.Position);
end;

function TTemplateParser.ruleCommentStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  match(vsComment);
  match(VsEndScript);
  result := TCommentStmt.Create(symbol.Position);
end;

function TTemplateParser.ruleContinueStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  match(vsContinue);
  match(VsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(symbol.Position, 'Continue should be in a for/while Stmt');

  result := TContinueStmt.Create(symbol.Position);
end;

function TTemplateParser.ruleElIfStmt: IStmt;

var
  Condition: IExpr;
  TrueContainer: ITemplate;
  FalseContainer: ITemplate;

  Options: IPreserveValue<TParserOptions>;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  if not(poAllowElIf in FOptions) then
    RaiseError(symbol.Position, 'ElIF expected');

  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poAllowElse, poHasElse, poAllowEnd]);

  match(VsELIF);

  Condition := ruleExpression;

  match(VsEndScript);
  // create new container for true condition
  PushContainer;
  TrueContainer := self.CurrentContainer;

  ruleStmts(TrueContainer, IF_ELIF_END);
  PopContainer;

  if FLookahead.Token = vsElse then
  begin

    match(vsElse);
    match(VsEndScript);

    PushContainer;
    FalseContainer := self.CurrentContainer;

    ruleStmts(FalseContainer, [vsEND, VsELIF]);

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

function TTemplateParser.ruleEndStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  // NOTE: we do not match anything as we want lookahead functions to continue to work
  if not(poAllowEnd in FOptions) then
    RaiseError(symbol.Position, 'End not expected');
  result := TEndStmt.Create(symbol.Position);
end;

function TTemplateParser.ruleExpression: IExpr;
var
  symbol: ITemplateSymbol;
  right, trueExpr, falseExpr: IExpr;
  BinOp: TBinOp;
begin
  result := ruleSimpleExpression();
  symbol := FLookahead;
  if symbol.Token in [VsEQ, VsNotEQ, VsLT, VsLTE, VsGT, VsGTE, vsin] then
  begin
    TemplateBinop(symbol.Token, BinOp);
    match(symbol);
    right := ruleExpression;
    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(right) then
    begin
      case BinOp of
        boEQ:
          exit(TValueExpr.Create(symbol.Position, isequal(AsValue(result), AsValue(right))));
        boNotEQ:
          exit(TValueExpr.Create(symbol.Position, not isequal(AsValue(result), AsValue(right))));
        boLT:
          exit(TValueExpr.Create(symbol.Position, isLessThan(AsValue(result), AsValue(right))));
        boGTE:
          exit(TValueExpr.Create(symbol.Position, not isLessThan(AsValue(result), AsValue(right))));
        boGT:
          exit(TValueExpr.Create(symbol.Position, isGreaterThan(AsValue(result), AsValue(right))));
        boLTE:
          exit(TValueExpr.Create(symbol.Position, not isGreaterThan(AsValue(result), AsValue(right))));
      end;
    end;
    result := TBinopExpr.Create(symbol.Position, result, BinOp, right);
  end;

  if FLookahead.Token = vsQUESTION then
  begin
    match(vsQUESTION);
    trueExpr := ruleExpression();
    match(vsColon);
    falseExpr := ruleExpression();

    if (eoEvalEarly in FContext.Options) and IsValue(result) then
    begin
      if AsBoolean(AsValue(result)) then
        result := trueExpr
      else
        result := falseExpr;
    end
    else
      result := TTernaryExpr.Create(symbol.Position, result, trueExpr, falseExpr);
  end;

end;

function TTemplateParser.ruleFactor: IExpr;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  case symbol.Token of
    VsOpenSquareBracket:
      begin
        match(VsOpenSquareBracket);
        result := TArrayExpr.Create(symbol.Position, ruleExprList(VsCloseSquareBracket));
        match(VsCloseSquareBracket);
      end;
    VsOpenRoundBracket:
      begin
        match(VsOpenRoundBracket);
        result := ruleExpression;
        match(VsCloseRoundBracket);
      end;
    vsString:
      result := TValueExpr.Create(symbol.Position, matchValue(vsString));
    vsNumber:
      result := TValueExpr.Create(symbol.Position, matchNumber(vsNumber));
    VsBoolean:
      result := TValueExpr.Create(symbol.Position, matchValue(VsBoolean) = 'true');
    VsID:
      result := self.ruleVariable();
    VsNOT:
      begin
        match(VsNOT);
        result := ruleExpression;
        if (eoEvalEarly in FContext.Options) and IsValue(result) then
          result := TValueExpr.Create(symbol.Position, not AsBoolean(AsValue(result)))
        else
          result := TUnaryExpr.Create(symbol.Position, uoNot, result);
      end;
  end;
end;

function TTemplateParser.ruleForStmt: IStmt;
var
  id: string;
  range: IExpr;
  lowValue, highValue: IExpr;
  ForOp: TForOp;
  Options: IPreserveValue<TParserOptions>;
  Container: ITemplate;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poInLoop, poAllowEnd]);
  match(VsFor);
  PushContainer;
  Container := CurrentContainer;
  id := matchValue(VsID);
  if FLookahead.Token = vsin then
  begin
    ForOp := TemplateForop(FLookahead.Token);
    match(vsin);
    range := ruleExpression;
  end
  else
  begin
    match(VsCOLONEQ);
    lowValue := ruleExpression();
    ForOp := TemplateForop(FLookahead.Token);
    if FLookahead.Token in [vsDownto, vsTo] then
      match(FLookahead.Token)
    else
      RaiseError(symbol.Position, 'downto/to token expected in for loop.');
    highValue := ruleExpression();
  end;

  match(VsEndScript);

  ruleStmts(Container, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  if ForOp = TForOp.foIn then
    result := TForInStmt.Create(symbol.Position, id, range, Container)
  else
    result := TForRangeStmt.Create(symbol.Position, id, ForOp, lowValue, highValue, Container);
  PopContainer;
end;

function TTemplateParser.ruleFunctionExpr(const ASymbol: string): IExpr;
var
  fn: TArray<TRttiMethod>;
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  if not FContext.TryGetFunction(ASymbol, fn) then
    RaiseError(symbol.Position, 'Function %s not registered in context.', [ASymbol]);
  match(VsOpenRoundBracket);
  result := TFunctionCallExpr.Create(symbol.Position, fn, ruleExprList);
  match(VsCloseRoundBracket);
end;

function TTemplateParser.ruleIdStmt: IStmt;
var
  symbol: ITemplateSymbol;
  expr: IExpr;
begin
  symbol := FLookahead;
  expr := ruleVariable;
  if FLookahead.Token = VsCOLONEQ then
  begin
    result := ruleAssignStmt(expr);
  end
  else
  begin
    expr := TEncodeExpr.Create(symbol.Position, expr);
    result := rulePrintStmtVariable(expr);
  end;
  match(VsEndScript);
end;

function TTemplateParser.ruleWhileStmt: IStmt;
var
  Condition: IExpr;
  Options: IPreserveValue<TParserOptions>;
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poInLoop, poAllowEnd]);
  PushContainer;

  match(vsWhile);
  Condition := ruleExpression;
  match(VsEndScript);

  ruleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(Condition) and not AsBoolean(AsValue(Condition)) then
    result := nil
  else
    result := TWhileStmt.Create(symbol.Position, Condition, CurrentContainer);
  PopContainer;

end;

function TTemplateParser.ruleWithStmt: IStmt;
var
  expr: IExpr;
  symbol: ITemplateSymbol;
  Options: IPreserveValue<TParserOptions>;
  Container: ITemplate;
begin
  Options := Preseve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  symbol := FLookahead;

  match(vswith);
  expr := ruleExpression;
  match(VsEndScript);

  PushContainer;
  Container := CurrentContainer;

  ruleStmts(Container, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  PopContainer;
  result := TWithStmt.Create(symbol.Position, expr, Container);

end;

function TTemplateParser.rulePrintStmt: IStmt;
var
  symbol: ITemplateSymbol;
  expr: IExpr;
begin
  symbol := FLookahead;
  match(vsPrint);
  match(VsOpenRoundBracket);
  expr := ruleExpression;
  match(VsCloseRoundBracket);
  match(VsEndScript);
  result := TPrintStmt.Create(symbol.Position, expr);
end;

function TTemplateParser.rulePrintStmtVariable(AExpr: IExpr): IStmt;
var
  symbol: ITemplateSymbol;
  val: IValueExpr;
begin
  symbol := FLookahead;
  if supports(AExpr, IValueExpr, val) and (asString(val.Value) = '') then
    exit(nil);
  result := TPrintStmt.Create(symbol.Position, AExpr);
end;

function TTemplateParser.ruleExprList(const AEndToken: TTemplateSymbol): IExprList;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  result := TExprList.Create(symbol.Position);
  if FLookahead.Token <> AEndToken then
    result.AddExpr(ruleExpression);
  while FLookahead.Token = vsComma do
  begin
    match(vsComma);
    result.AddExpr(ruleExpression);
  end;
end;

function TTemplateParser.ruleExprStmt: IStmt;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  result := rulePrintStmtVariable(TEncodeExpr.Create(symbol.Position, ruleExpression));
  match(VsEndScript);
end;

function TTemplateParser.CurrentContainer: ITemplate;
begin
  if FContainerStack.Count <> 0 then
    result := FContainerStack.Peek;
end;

destructor TTemplateParser.Destroy;
begin
  FContainerStack.Free;
  inherited;
end;

function TTemplateParser.lookaheadValue: string;
var
  val: ITemplateValueSymbol;
begin
  if FLookahead.QueryInterface(ITemplateValueSymbol, val) = 0 then
    result := val.Value
  else
    result := '';
end;

procedure TTemplateParser.match(const ASymbol: TTemplateSymbol);
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  if ASymbol = FLookahead.Token then
  begin
    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(symbol.Position, format('Parsing error expecting %s', [TemplateSymbolToString(ASymbol)]));
end;

procedure TTemplateParser.match(ASymbol: ITemplateSymbol);
begin
  match(ASymbol.Token);
end;

function TTemplateParser.matchNumber(const ASymbol: TTemplateSymbol): extended;
begin
  result := strtofloat(matchValue(ASymbol));
end;

function TTemplateParser.matchValue(const ASymbol: TTemplateSymbol): string;
var
  symbol: ITemplateSymbol;
begin
  symbol := FLookahead;
  if ASymbol = FLookahead.Token then
  begin
    result := lookaheadValue;
    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(symbol.Position, format('Parsing error expecting %s', [TemplateSymbolToString(ASymbol)]));
end;

function TTemplateParser.Parse(const AStream: TStream; const AManagedStream: boolean): ITemplate;
begin
  FContainerStack.Clear;
  PushContainer;
  FLexer := CreateTemplateLexer(FContext, AStream, '', AManagedStream);
  FLookahead := FLexer.GetToken;
  ruleStmts(CurrentContainer, []);
  match(VsEOF);
  result := CurrentContainer;
  if eoPrettyPrint in FContext.Options then
    writeln(Template.PrettyPrint(result));
end;

function TTemplateParser.PopContainer: ITemplate;
begin
  result := CurrentContainer;
  FContainerStack.Pop;
end;

function TTemplateParser.PushContainer: ITemplate;
begin
  result := CurrentContainer;
  FContainerStack.Push(TTemplate.Create());
end;

{ TValueExpr }

procedure TValueExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TValueExpr.Create(APosition: IPosition; const AValue: TValue);
begin
  inherited Create(APosition);
  FValue := AValue;
end;

function TValueExpr.GetValue: TValue;
begin
  result := FValue;
end;

{ TExprList }

procedure TExprList.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

procedure TExprList.AddExpr(AExpr: IExpr);
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

procedure TUnaryExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TUnaryExpr.Create(APosition: IPosition; const AUnaryOp: TUnaryOp; ACondition: IExpr);
begin
  inherited Create(APosition, ACondition);
  FUnaryOp := AUnaryOp;
end;

function TUnaryExpr.GetUnaryOp: TUnaryOp;
begin
  result := FUnaryOp;
end;

{ TVariableExpr }

procedure TVariableExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TVariableExpr.Create(APosition: IPosition; const AVariable: string);
begin
  inherited Create(APosition);
  FVariable := AVariable;
end;

function TVariableExpr.GetVariable: string;
begin
  result := FVariable;
end;

{ TFunctionCallExpr }

procedure TFunctionCallExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TFunctionCallExpr.Create(APosition: IPosition; AFunction: TArray<TRttiMethod>; ExprList: IExprList);

begin
  inherited Create(APosition, ExprList);
  FFunctionInfo := AFunction;
end;

function TFunctionCallExpr.GetFunctionInfo: TArray<TRttiMethod>;
begin
  result := FFunctionInfo;
end;

{ TIfStmt }

procedure TIfStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TIfStmt.Create(APosition: IPosition; ACondition: IExpr; ATrueContainer, AFalseContainer: ITemplate);
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

function TIfStmt.GetFalseContainer: ITemplate;
begin
  result := FFalseContainer;
end;

function TIfStmt.GetTrueContainer: ITemplate;
begin
  result := FTrueContainer;
end;

{ TBinopExpr }

procedure TBinopExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TBinopExpr.Create(APosition: IPosition; ALeft: IExpr; const ABinop: TBinOp; ARight: IExpr);
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

procedure TPrintStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TForInStmt }

procedure TForInStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TForInStmt.Create(APosition: IPosition; const AVariable: string; AExpr: IExpr; AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FVariable := AVariable;
  FExpr := AExpr;
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

procedure TForRangeStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TForRangeStmt.Create(APosition: IPosition; const AVariable: string; const AForIp: TForOp; ALowExpr, AHighExpr: IExpr; AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FVariable := AVariable;
  FForIp := AForIp;
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
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

procedure TAssignStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TAssignStmt.Create(APosition: IPosition; const AVariable: string; AExpr: IExpr);
begin
  inherited Create(APosition, AExpr);
  FVariable := AVariable;
end;

function TAssignStmt.GetVariable: string;
begin
  result := FVariable;
end;

{ TTemplateContainer }

procedure TTemplate.Accept(AVisitor: ITemplateVisitor);
var
  i: ITemplateVisitorHost;
begin
  for i in FArray do
    i.Accept(AVisitor);
end;

procedure TTemplate.Add(AItem: ITemplateVisitorHost);
begin
  insert(AItem, FArray, length(FArray));
end;

function TTemplate.GetCount: integer;
begin
  result := length(FArray)
end;

function TTemplate.GetLastItem: ITemplateVisitorHost;
begin
  if GetCount = 0 then
    exit(nil);
  result := GetItem(GetCount - 1);
end;

function TTemplate.GetItem(const AOffset: integer): ITemplateVisitorHost;
begin
  result := FArray[AOffset];
end;

{ TWhileStmt }

procedure TWhileStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TWhileStmt.Create(APosition: IPosition; ACondition: IExpr; AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FCondition := ACondition;
end;

function TWhileStmt.GetCondition: IExpr;
begin
  result := FCondition;
end;

{ TContinueStmt }

procedure TContinueStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TBreakStmt }

procedure TBreakStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TEndStmt }

procedure TEndStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TVariableDerefExpr }

procedure TVariableDerefExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TVariableDerefExpr.Create(APosition: IPosition; const ADerefType: TDerefType; AVariable: IExpr; ADeref: IExpr);
begin
  inherited Create(APosition, AVariable);
  FDerefType := ADerefType;
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

{ TIncludeStmt }

procedure TIncludeStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TElseStmt }

procedure TElseStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TElIfStmt }

procedure TElIfStmt.Accept(AVisitor: ITemplateVisitor);
begin
  // AVisitor.Visit(self);
end;

{ TCommentStmt }

procedure TCommentStmt.Accept(AVisitor: ITemplateVisitor);
begin
  // AVisitor.Visit(self);
end;

{ TAbstractBase }

constructor TAbstractBase.Create(APosition: IPosition);
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

procedure TMethodCallExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TMethodCallExpr.Create(APosition: IPosition; AObjectExpr: IExpr; const AMethod: string; AExprList: IExprList);
begin
  inherited Create(APosition, AExprList);
  FObjectExpr := AObjectExpr;
  FMethod := AMethod;
end;

function TMethodCallExpr.GetMethod: string;
begin
  result := FMethod;
end;

function TMethodCallExpr.GetObject: IExpr;
begin
  result := FObjectExpr;
end;

function TMethodCallExpr.GetRttiMethod: TRttiMethod;
begin
  result := FRttiMethod;
end;

procedure TMethodCallExpr.SetRttiMethod(const ARttiMethod: TRttiMethod);
begin
  FRttiMethod := ARttiMethod;
end;

{ TEncodeStmt }

procedure TEncodeExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TProcessTemplateStmt }

procedure TProcessTemplateStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TProcessTemplateStmt.Create(APosition: IPosition; AContainer: ITemplate; const AAllowNewLine: boolean);
begin
  inherited Create(APosition, AContainer);
  FAllowNewline := AAllowNewLine;
end;

function TProcessTemplateStmt.GetAllowNewLine: boolean;
begin
  result := FAllowNewline;
end;

procedure TProcessTemplateStmt.SetAllowNewLine(const AAllow: boolean);
begin
  FAllowNewline := AAllow;
end;

{ TDefineTemplateStmt }

procedure TDefineTemplateStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TDefineTemplateStmt.Create(APosition: IPosition; AName: IExpr; AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FName := AName;
end;

function TDefineTemplateStmt.GetName: IExpr;
begin
  result := FName;
end;

{ TWithStmt }

procedure TWithStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TWithStmt.Create(APosition: IPosition; AExpr: IExpr; AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FExpr := AExpr;
end;

function TWithStmt.GetExpr: IExpr;
begin
  result := FExpr;
end;

{ TIfExpr }

procedure TTernaryExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TTernaryExpr.Create(APosition: IPosition; ACondition, ATrueExpr, AFalseExpr: IExpr);
begin
  inherited Create(APosition, ACondition);
  FTrueExpr := ATrueExpr;
  FFalseExpr := AFalseExpr;
end;

function TTernaryExpr.GetFalseExpr: IExpr;
begin
  result := FFalseExpr;
end;

function TTernaryExpr.GetTrueExpr: IExpr;
begin
  result := FTrueExpr;
end;

{ TArrayExpr }

procedure TArrayExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

procedure initOps;
var
  s: TTemplateSymbol;
  bo: TBinOp;
begin
  for s := Low(TTemplateSymbol) to High(TTemplateSymbol) do
    GTemplateBinOps[s] := boInvalid;
  for bo := Low(TBinOp) to High(TBinOp) do
    GTemplateBinOps[vsin] := boIN;
  GTemplateBinOps[VsAND] := boAND;
  GTemplateBinOps[VsOR] := boOR;
  GTemplateBinOps[VsPLUS] := boPlus;
  GTemplateBinOps[VsMinus] := boMinus;
  GTemplateBinOps[VsMULT] := boMult;
  GTemplateBinOps[vsSLASH] := boSlash;
  GTemplateBinOps[VsDIV] := boDiv;
  GTemplateBinOps[VsMOD] := boMod;
  GTemplateBinOps[VsLT] := boLT;
  GTemplateBinOps[VsLTE] := boLTE;
  GTemplateBinOps[VsGT] := boGT;
  GTemplateBinOps[VsGTE] := boGTE;
  GTemplateBinOps[VsEQ] := boEQ;
  GTemplateBinOps[VsNotEQ] := boNotEQ;
end;

{ TRequireStmt }

procedure TRequireStmt.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TRequireStmt.Create(APosition: IPosition; AExprList: IExprList);
begin
  inherited Create(APosition);
  FExprList := AExprList;
end;

function TRequireStmt.GetExprList: IExprList;
begin
  result := FExprList;
end;

{ TAbstractStmtWithExpr }

constructor TAbstractStmtWithExpr.Create(APosition: IPosition; AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TAbstractStmtWithExpr.GetExpr: IExpr;
begin
  result := FExpr;
end;

{ TAbstractStmtWithContainer }

constructor TAbstractStmtWithContainer.Create(APosition: IPosition; AContainer: ITemplate);
begin
  inherited Create(APosition);
  FContainer := AContainer;
end;

function TAbstractStmtWithContainer.GetContainer: ITemplate;
begin
  result := FContainer;
end;

{ TAbstractExprWithExprList }

constructor TAbstractExprWithExprList.Create(APosition: IPosition; AExprList: IExprList);
begin
  inherited Create(APosition);
  FExprList := AExprList;
end;

function TAbstractExprWithExprList.GetExprList: IExprList;
begin
  result := FExprList;
end;

{ TAbstractExprWithExpr }

constructor TAbstractExprWithExpr.Create(APosition: IPosition; AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TAbstractExprWithExpr.GetExpr: IExpr;
begin
  result := FExpr;
end;

initialization

initOps;

end.
