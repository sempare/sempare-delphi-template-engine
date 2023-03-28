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
unit Sempare.Template.Parser;

interface

uses
  System.Classes,
  Sempare.Template.AST,
  Sempare.Template.Context;

type
  ETemplateParser = class(ETemplate);

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
  Sempare.Template.BlockResolver,
  Sempare.Template.BlockReplacer,
  Sempare.Template.ResourceStrings,
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
    procedure Add(const AItem: ITemplateVisitorHost);
    function GetLastItem: ITemplateVisitorHost;
    procedure Accept(const AVisitor: ITemplateVisitor);
    function Clone: IInterface;
  public

  end;

  TAbstractBase = class abstract(TInterfacedObject, IPositional, ITemplateVisitorHost)
  private
    FPosition: IPosition;
    function GetPosition: IPosition;
  public
    constructor Create(const APosition: IPosition);
    destructor Destroy; override;
    procedure Accept(const AVisitor: ITemplateVisitor); virtual; abstract;
    function Clone: IInterface; virtual; abstract;
  end;

  TAbstractStmt = class abstract(TAbstractBase, IStmt)
  end;

  TDebugStmt = class(TAbstractStmt, IDebugStmt)
  private
    FStmt: IStmt;
    function GetStmt: IStmt;
  public
    constructor Create(const AStmt: IStmt);
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TEndStmt = class(TAbstractStmt, IEndStmt)
  public
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TExtendsStmt = class(TAbstractStmt, IExtendsStmt)
  private
    FName: IExpr;
    FBlockContainer: ITemplate; // NOTE: FContainer is something that is resolved
    FContainer: ITemplate; // NOTE: FContainer is something that is resolved
    FBlockNames: TArray<string>; // NOTE: names in the container
    function GetName: IExpr;
    function GetBlockContainer: ITemplate;
    function GetContainer: ITemplate;
    procedure SetContainer(const AContainer: ITemplate);
    function GetBlockNames: TArray<string>;
    procedure SetBlockNames(const ANames: TArray<string>);
    function NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const ABlockContainer: ITemplate);
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TBlockStmt = class(TAbstractStmt, IBlockStmt)
  private
    FName: IExpr;
    FContainer: ITemplate;
    function GetName: IExpr;
    function GetContainer: ITemplate;
    procedure SetContainer(const AContainer: ITemplate);
    function NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const AContainer: ITemplate);
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TElseStmt = class(TAbstractStmt, IElseStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TContinueStmt = class(TAbstractStmt, IContinueStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TBreakStmt = class(TAbstractStmt, IBreakStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TCommentStmt = class(TAbstractStmt, ICommentStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TElIfStmt = class(TAbstractStmt, IElIfStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
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
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TIncludeStmt = class(TAbstractStmtWithExpr, IIncludeStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  end;

  TRequireStmt = class(TAbstractStmt, IRequireStmt)
  private
    FExprList: IExprList;
    function GetExprList: IExprList;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AExprList: IExprList);
  end;

  TIfStmt = class(TAbstractStmt, IIfStmt)
  private
    FCondition: IExpr;
    FTrueContainer: ITemplate;
    FFalseContainer: ITemplate;
    function GetCondition: IExpr;
    function GetTrueContainer: ITemplate;
    function GetFalseContainer: ITemplate;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const ACondition: IExpr; const ATrueContainer: ITemplate; const AFalseContainer: ITemplate);
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
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AContainer: ITemplate; const AAllowNewLine: boolean = true);
  end;

  TDefineTemplateStmt = class(TAbstractStmtWithContainer, IDefineTemplateStmt)
  private
    FName: IExpr;
    function GetName: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const AContainer: ITemplate);
  end;

  TWithStmt = class(TAbstractStmtWithContainer, IWithStmt)
  private
    FExpr: IExpr;
    function GetExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AExpr: IExpr; const AContainer: ITemplate);
  end;

  TLoopStmt = class(TAbstractStmtWithContainer, ILoopStmt)
  protected
    FOnFirst: ITemplate;
    FOnLast: ITemplate;
    FOnEmpty: ITemplate;
    FBetweenItem: ITemplate;

    function GetOnFirstContainer: ITemplate;
    function GetOnEndContainer: ITemplate;
    function GetOnEmptyContainer: ITemplate;
    function GetBetweenItemContainer: ITemplate;
  public
    constructor Create(APosition: IPosition; AContainer: ITemplate; AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
  end;

  TWhileStmt = class(TLoopStmt, IWhileStmt)
  private
    FCondition: IExpr;
    FOffsetExpr: IExpr;
    FLimitExpr: IExpr;
    function GetCondition: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function GetOffsetExpr: IExpr;
    function GetLimitExpr: IExpr;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const ACondition: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
  end;

  TForInStmt = class(TLoopStmt, IForInStmt)
  private
    FVariable: string;
    FForOp: TForOp;
    FExpr: IExpr;
    FOffsetExpr: IExpr;
    FLimitExpr: IExpr;
    function GetVariable: string;
    function GetForOp: TForOp;
    function GetExpr: IExpr;
    function GetOffsetExpr: IExpr;
    function GetLimitExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const AExpr: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
  end;

  TForRangeStmt = class(TLoopStmt, IForRangeStmt)
  private
    FVariable: string;
    FForOp: TForOp;
    FLowExpr: IExpr;
    FHighExpr: IExpr;
    FStepExpr: IExpr;
    function GetVariable: string;
    function GetForOp: TForOp;
    function GetLowExpr: IExpr;
    function GetHighExpr: IExpr;
    function GetStepExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const ALowExpr: IExpr; const AHighExpr: IExpr; const AStep: IExpr; const AContainer: ITemplate; const AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
  end;

  TAssignStmt = class(TAbstractStmtWithExpr, IAssignStmt)
  private
    FVariable: string;
    function GetVariable: string;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr);
  end;

  TCycleStmt = class(TAbstractStmt, ICycleStmt)
  private
    FExprList: IExprList;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
    constructor Create(const APosition: IPosition; const AList: IExprList);
    function GetList: IExprList;
  end;

  TExprList = class(TAbstractBase, IExprList)
  private
    FExprs: TArray<IExpr>;
    function GetExpr(const AOffset: integer): IExpr;
    procedure AddExpr(const AExpr: IExpr);
    function GetExprCount: integer;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Clone: IInterface; override;
  public
  end;

  TAbstractExpr = class abstract(TAbstractBase, IExpr)
    function Clone: IInterface; override;
  end;

  TValueExpr = class(TAbstractExpr, IValueExpr)
  private
    FValue: TValue;
    function GetValue: TValue;
    procedure Accept(const AVisitor: ITemplateVisitor); override;

  public
    constructor Create(const APosition: IPosition; const AValue: TValue);
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
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TVariableExpr = class(TAbstractExpr, IVariableExpr)
  private
    FVariable: string;
    function GetVariable: string;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AVariable: string);
  end;

  TEncodeExpr = class(TAbstractExprWithExpr, IEncodeExpr)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TVariableDerefExpr = class(TAbstractExprWithExpr, IVariableDerefExpr)
  private
    FDeref: IExpr;
    FDerefType: TDerefType;
    function GetDerefType: TDerefType;
    function GetDerefExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(APosition: IPosition; const ADerefType: TDerefType; AVariable: IExpr; ADeref: IExpr);
  end;

  TFunctionCallExpr = class(TAbstractExprWithExprList, IFunctionCallExpr)
  private
    FFunctionInfo: TArray<TRttiMethod>;
    function GetFunctionInfo: TArray<TRttiMethod>;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AFunction: TArray<TRttiMethod>; const ExprList: IExprList);
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
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AObjectExpr: IExpr; const AMethod: string; const AExprList: IExprList);
  end;

  TBinopExpr = class(TAbstractExpr, IBinopExpr)
  private
    FLeft: IExpr;
    FBinop: TBinOp;
    FRight: IExpr;
    function GetBinOp: TBinOp;
    function GetLeftExpr: IExpr;
    function GetRightExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const ALeft: IExpr; const ABinop: TBinOp; const ARight: IExpr);
    destructor Destroy; override;
  end;

  TTernaryExpr = class(TAbstractExprWithExpr, ITernaryExpr)
  private
    FTrueExpr: IExpr;
    FFalseExpr: IExpr;
    function GetTrueExpr: IExpr;
    function GetFalseExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const ACondition: IExpr; const ATrueExpr: IExpr; const AFalseExpr: IExpr);
  end;

  TUnaryExpr = class(TAbstractExprWithExpr, IUnaryExpr)
  private
    FUnaryOp: TUnaryOp;
    function GetUnaryOp: TUnaryOp;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AUnaryOp: TUnaryOp; const ACondition: IExpr);
  end;

  EEndOfBlock = class(ETemplate);

  TTemplateSymbolSet = set of TTemplateSymbol;

  TParserOption = (poAllowEnd, poAllowElse, poAllowElIf, poHasElse, poInLoop, poStripWS);
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
    function ruleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet): TTemplateSymbol;
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
    function ruleExprList(const AEndToken: TTemplateSymbol = vsCloseRoundBracket): IExprList;
    function ruleAssignStmt(ASymbol: IExpr): IStmt;
    function rulePrintStmtVariable(AExpr: IExpr): IStmt; overload;
    function ruleForStmt: IStmt;
    function ruleWhileStmt: IStmt;
    function ruleWithStmt: IStmt;
    function ruleCycleStmt: IStmt;
    function ruleTemplateStmt: IStmt;

    function ruleBlockStmt: IStmt;
    function ruleExtendsStmt: IStmt;

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
  exit(TTemplateParser.Create(AContext));
end;

function IsValue(AExpr: IExpr): boolean;
begin
  exit(supports(AExpr, IValueExpr));
end;

function AsValue(AExpr: IExpr): TValue;
var
  LValueExpr: IValueExpr;
begin
  AExpr.QueryInterface(IValueExpr, LValueExpr);
  exit(LValueExpr.Value);
end;

function AsVarString(AExpr: IExpr): string;
begin
  exit((AExpr as IVariableExpr).Variable);
end;

function IsEnd(AStmt: IStmt): boolean;
begin
  if AStmt = nil then
    exit(false);
  exit(supports(AStmt, IEndStmt));
end;

function TemplateForop(const APosition: IPosition; const ASymbol: TTemplateSymbol): TForOp;
begin
  result := foTo;
  case ASymbol of
    vsTo:
      exit;
    vsDownto:
      exit(foDownto);
    vsIn:
      exit(foIn);
    vsOf:
      exit(foOf);
  else
    RaiseErrorRes(APosition, @SForOpNotSupported, [TemplateSymbolToString(ASymbol)]);
  end;
end;

var
  GTemplateBinOps: array [TTemplateSymbol] of TBinOp;

function TemplateBinop(const ASymbol: TTemplateSymbol; out BinOp: TBinOp): boolean;

begin
  BinOp := GTemplateBinOps[ASymbol];
  exit(BinOp <> boInvalid);
end;

function GetTemplateParser(AContext: ITemplateContext): ITemplateParser;
begin
  exit(TTemplateParser.Create(AContext));
end;

{ TTemplateParser }

constructor TTemplateParser.Create(AContext: ITemplateContext);
begin
  FOptions := [];
  FContext := AContext;
  FContainerStack := TStack<ITemplate>.Create;
end;

const
  IF_ELIF_END: TTemplateSymbolSet = [vsELIF, vsElse, vsEND];

function TTemplateParser.ruleIfStmt: IStmt;
var
  LConditionalExpr: IExpr;
  LTrueContainer: ITemplate;
  LFalseContainer: ITemplate;
  LContainerAdd: ITemplateAdd;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowElse, poAllowEnd, poAllowElIf]);
  LSymbol := FLookahead;
  match(vsIF);
  LConditionalExpr := ruleExpression;

  match(vsEndScript);
  // create new container for true condition
  PushContainer;
  LTrueContainer := self.CurrentContainer;

  ruleStmts(LTrueContainer, IF_ELIF_END);
  PopContainer;

  PushContainer;
  LFalseContainer := self.CurrentContainer;
  LFalseContainer.QueryInterface(ITemplateAdd, LContainerAdd);

  if FLookahead.Token = vsELIF then
  begin
    while (FLookahead.Token = vsELIF) do
    begin
      LContainerAdd.Add(AsVisitorHost(ruleElIfStmt()));
    end;
  end
  else if FLookahead.Token = vsElse then
  begin
    match(vsElse);
    match(vsEndScript);
    ruleStmts(LFalseContainer, [vsEND]);
  end;
  PopContainer;
  match(vsEND);
  match(vsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(LConditionalExpr) then
  begin
    if AsBoolean(AsValue(LConditionalExpr)) then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LTrueContainer))
    else if LFalseContainer <> nil then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LFalseContainer))
  end;
  exit(TIfStmt.Create(LSymbol.Position, LConditionalExpr, LTrueContainer, LFalseContainer));
end;

function TTemplateParser.ruleIgnoreNewline: IStmt;
var
  LSymbol: ITemplateSymbol;
  LContainerTemplate: ITemplate;
  LOptions: IPreserveValue<TParserOptions>;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;
  match(vsIgnoreNL);
  match(vsEndScript);
  PushContainer;
  LContainerTemplate := CurrentContainer;

  ruleStmts(LContainerTemplate, [vsEND]);

  match(vsEND);
  match(vsEndScript);
  PopContainer;

  exit(TProcessTemplateStmt.Create(LSymbol.Position, LContainerTemplate, false));
end;

function TTemplateParser.ruleIncludeStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LIncludeExpr: IExpr;
  LScopeExpr: IExpr;
  LContainerTemplate: TTemplate;
begin
  LSymbol := FLookahead;
  match(vsInclude);
  match(vsOpenRoundBracket);

  LIncludeExpr := ruleExpression;

  if FLookahead.Token = vsComma then
  begin
    match(vsComma);
    LScopeExpr := ruleExpression;
  end;

  match(vsCloseRoundBracket);
  match(vsEndScript);

  if LScopeExpr <> nil then
  begin
    LContainerTemplate := TTemplate.Create();
    LContainerTemplate.Add(TIncludeStmt.Create(LSymbol.Position, LIncludeExpr));
    exit(TWithStmt.Create(LSymbol.Position, LScopeExpr, LContainerTemplate));
  end
  else
  begin
    exit(TIncludeStmt.Create(LSymbol.Position, LIncludeExpr));
  end;
end;

function TTemplateParser.ruleRequireStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsRequire);
  match(vsOpenRoundBracket);
  result := TRequireStmt.Create(LSymbol.Position, self.ruleExprList());
  match(vsCloseRoundBracket);
  match(vsEndScript);
end;

function TTemplateParser.ruleMethodExpr(AExpr: IExpr; AMethodExpr: IExpr): IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsOpenRoundBracket);
  result := TMethodCallExpr.Create(LSymbol.Position, AExpr, AsVarString(AMethodExpr), ruleExprList);
  match(vsCloseRoundBracket);
end;

function TTemplateParser.ruleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet): TTemplateSymbol;
var
  LStmt: IStmt;
  LParentContainer: ITemplateAdd;
  LSymbol: ITemplateSymbol;
  LLoop: boolean;
  LEndToken: TTemplateSymbolSet;

  function AddPrintStmt: IStmt;
  var
    LText: string;
  begin
    LText := matchValue(vsText);
    if LText = '' then
      exit(nil);
    exit(rulePrintStmtVariable(TValueExpr.Create(LSymbol.Position, LText)));
  end;

  procedure SkipStmt;
  begin
    matchValue(vsText)
  end;

begin
  result := vsInvalid;
  LEndToken := AEndToken + [vsEOF];
  Container.QueryInterface(ITemplateAdd, LParentContainer);
  LLoop := true;
  while LLoop do
  begin
    LSymbol := FLookahead;
    if LSymbol.Token in LEndToken then
    begin
      exit(LSymbol.Token);
    end;
    LStmt := nil;
    case LSymbol.Token of
      vsText:
        begin
          if poStripWS in FOptions then
            SkipStmt
          else
            LStmt := AddPrintStmt;
        end;
      vsStartScript:
        begin
          if LSymbol.StripWS then
            exclude(FOptions, poStripWS);
          LStmt := ruleStmt;
          if LStmt = nil then
            LLoop := false;
        end;
    end;
    if (LStmt <> nil) and not supports(LStmt, IElseStmt) then
    begin
      LParentContainer.Add(AsVisitorHost(LStmt));
    end;
  end;
end;

function TTemplateParser.ruleTemplateStmt: IStmt;
var
  LExpr: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);
  LSymbol := FLookahead;

  match(vsTemplate);
  LExpr := ruleExpression;
  match(vsEndScript);
  PushContainer;
  LContainer := CurrentContainer;

  ruleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(vsEndScript);
  PopContainer;

  exit(TDefineTemplateStmt.Create(LSymbol.Position, LExpr, LContainer));
end;

function TTemplateParser.ruleSignedFactor: IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;

  if LSymbol.Token in [vsMinus, vsPLUS] then
    match(LSymbol.Token);

  result := ruleFactor;

  if LSymbol.Token = vsMinus then
  begin
    result := ruleFactor;
    if (eoEvalEarly in FContext.Options) and IsValue(result) then
      exit(TValueExpr.Create(LSymbol.Position, -asnum(AsValue(result), FContext)))
    else
      exit(TUnaryExpr.Create(LSymbol.Position, uoMinus, result))
  end;
end;

function TTemplateParser.ruleSimpleExpression: IExpr;
var
  LRightExpr: IExpr;
  LBinOp: TBinOp;
  LSymbol: ITemplateSymbol;
begin
  result := ruleTerm();
  LSymbol := FLookahead;
  if LSymbol.Token in [vsPLUS, vsMinus, vsOR] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    match(LSymbol.Token);
    LRightExpr := ruleSimpleExpression;
    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(LRightExpr) then
    begin
      case LBinOp of
        boOR:
          exit(TValueExpr.Create(LSymbol.Position, AsBoolean(AsValue(result)) or AsBoolean(AsValue(LRightExpr))));
        boPlus:
          begin
            if isNumLike(AsValue(result)) and isNumLike(AsValue(LRightExpr)) then
              exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result), FContext) + asnum(AsValue(LRightExpr), FContext)))
            else if isStrLike(AsValue(result)) and isStrLike(AsValue(LRightExpr)) then
              exit(TValueExpr.Create(LSymbol.Position, asString(AsValue(result), FContext) + asString(AsValue(LRightExpr), FContext)))
            else if isStrLike(AsValue(result)) and isNumLike(AsValue(LRightExpr)) then
              exit(TValueExpr.Create(LSymbol.Position, asString(AsValue(result), FContext) + floattostr(asnum(AsValue(LRightExpr), FContext), FContext.FormatSettings)));
          end;
        boMinus:
          exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result), FContext) - asnum(AsValue(LRightExpr), FContext)));
      end;
    end;
    exit(TBinopExpr.Create(LSymbol.Position, result, LBinOp, LRightExpr));
  end;
end;

function TTemplateParser.ruleTerm: IExpr;
var
  LRightExpr: IExpr;
  LBinOp: TBinOp;
  LSymbol: ITemplateSymbol;
begin
  result := ruleSignedFactor;
  LSymbol := FLookahead;
  if LSymbol.Token in [vsMULT, vsDIV, vsSLASH, vsMOD, vsAND] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    match(LSymbol.Token);
    LRightExpr := ruleTerm;

    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(LRightExpr) then
    begin
      case LBinOp of
        boAND:
          exit(TValueExpr.Create(LSymbol.Position, AsBoolean(AsValue(result)) and AsBoolean(AsValue(LRightExpr))));
        boMult:
          exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result), FContext) * asnum(AsValue(LRightExpr), FContext)));
        boDiv:
          exit(TValueExpr.Create(LSymbol.Position, trunc(asnum(AsValue(result), FContext)) div trunc(asnum(AsValue(LRightExpr), FContext))));
        boSlash:
          exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result), FContext) / asnum(AsValue(LRightExpr), FContext)));
        boMod:
          exit(TValueExpr.Create(LSymbol.Position, AsInt(AsValue(result), FContext) mod AsInt(AsValue(LRightExpr), FContext)));
      end;
    end;

    exit(TBinopExpr.Create(LSymbol.Position, result, LBinOp, LRightExpr));
  end;

end;

function TTemplateParser.ruleStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  result := nil;
  LSymbol := FLookahead;
  match(vsStartScript);
  case FLookahead.Token of
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
    vsEndScript, vsElse, vsELIF, vsOnBegin, vsOnEnd, vsOnEmpty, vsBetweenItem:
      ;
    vsIF:
      result := ruleIfStmt;
    vsFor:
      result := ruleForStmt;
    vsCycle:
      result := ruleCycleStmt;
    vsPrint:
      result := rulePrintStmt;
    vsWhile:
      result := ruleWhileStmt;
    vswith:
      result := ruleWithStmt;
    vsRequire:
      result := ruleRequireStmt;
    vsTemplate:
      result := ruleTemplateStmt;
    vsID:
      result := ruleIdStmt;
    vsBlock:
      result := ruleBlockStmt;
    vsExtends:
      result := ruleExtendsStmt;
  else
    result := ruleExprStmt;
  end;
  if (eoEmbedException in FContext.Options) and (result <> nil) then
  begin
    result := TDebugStmt.Create(result);
  end;
end;

function TTemplateParser.ruleVariable: IExpr;
var
  LId: string;
  LIdVal: TValue;
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
  LDone: boolean;
begin
  LDone := false;
  LSymbol := FLookahead;
  LId := matchValue(vsID);
  if (eoEvalVarsEarly in FContext.Options) and FContext.TryGetVariable(LId, LIdVal) then
    result := TValueExpr.Create(LSymbol.Position, LIdVal)
  else
    result := TVariableExpr.Create(LSymbol.Position, LId);
  while not LDone do
  begin
    LSymbol := FLookahead;
    case LSymbol.Token of
      vsOpenRoundBracket:
        begin
          result := self.ruleFunctionExpr(LId);
        end;
      vsOpenSquareBracket:
        begin
          match(vsOpenSquareBracket);
          LExpr := self.ruleExpression;
          if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(LExpr) then
            result := TValueExpr.Create(LSymbol.Position, deref(LSymbol.Position, AsValue(result), AsValue(LExpr), eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext))
          else
            result := TVariableDerefExpr.Create(LSymbol.Position, dtArray, result, LExpr);
          match(vsCloseSquareBracket);
        end;
      vsDOT:
        begin
          match(vsDOT);
          LExpr := TVariableExpr.Create(LSymbol.Position, matchValue(vsID));
          if FLookahead.Token = vsOpenRoundBracket then
            result := ruleMethodExpr(result, LExpr)
          else
          begin
            if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(LExpr) then
              result := TValueExpr.Create(LSymbol.Position, deref(LSymbol.Position, AsValue(result), AsValue(LExpr), eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext))
            else
              result := TVariableDerefExpr.Create(LSymbol.Position, dtObject, result, LExpr);
          end;
        end;
    else
      LDone := true;
    end;
    if LDone then
      break;
  end;
end;

function TTemplateParser.ruleAssignStmt(ASymbol: IExpr): IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsCOLONEQ);
  exit(TAssignStmt.Create(LSymbol.Position, (ASymbol as IVariableExpr).Variable, ruleExpression));
end;

function TTemplateParser.ruleBlockStmt: IStmt;
var
  LName: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;

  match(vsBlock);
  LName := ruleExpression;
  match(vsEndScript);

  PushContainer;
  LContainer := CurrentContainer;

  ruleStmts(LContainer, [vsEND]);

  match(vsEND);
  match(vsEndScript);

  PopContainer;
  exit(TBlockStmt.Create(LSymbol.Position, LName, LContainer));

end;

function TTemplateParser.ruleBreakStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsBreak);
  match(vsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(LSymbol.Position, SContinueShouldBeInALoop);
  exit(TBreakStmt.Create(LSymbol.Position));
end;

function TTemplateParser.ruleCommentStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsComment);
  match(vsEndScript);
  exit(TCommentStmt.Create(LSymbol.Position));
end;

function TTemplateParser.ruleContinueStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsContinue);
  match(vsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(LSymbol.Position, SContinueShouldBeInALoop);
  exit(TContinueStmt.Create(LSymbol.Position));
end;

function TTemplateParser.ruleCycleStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LListExpr: IExprList;
begin
  LSymbol := FLookahead;
  match(vsCycle);
  match(vsOpenRoundBracket);

  LListExpr := ruleExprList();

  match(vsCloseRoundBracket);
  match(vsEndScript);

  exit(TCycleStmt.Create(LSymbol.Position, LListExpr));
end;

function TTemplateParser.ruleElIfStmt: IStmt;
var
  LConditionExpr: IExpr;
  LTrueContainer: ITemplate;
  LFalseContainer: ITemplate;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  if not(poAllowElIf in FOptions) then
    RaiseError(LSymbol.Position, SElIfExpected);

  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowElse, poHasElse, poAllowEnd]);

  match(vsELIF);

  LConditionExpr := ruleExpression;

  match(vsEndScript);
  // create new container for true condition
  PushContainer;
  LTrueContainer := self.CurrentContainer;

  ruleStmts(LTrueContainer, IF_ELIF_END);
  PopContainer;

  if FLookahead.Token = vsElse then
  begin
    match(vsElse);
    match(vsEndScript);

    PushContainer;
    LFalseContainer := self.CurrentContainer;

    ruleStmts(LFalseContainer, [vsEND, vsELIF]);

    PopContainer;
  end;

  if (eoEvalEarly in FContext.Options) and IsValue(LConditionExpr) then
  begin
    if AsBoolean(AsValue(LConditionExpr)) then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LTrueContainer))
    else if LFalseContainer <> nil then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LFalseContainer))
  end;
  exit(TIfStmt.Create(LSymbol.Position, LConditionExpr, LTrueContainer, LFalseContainer));
end;

function TTemplateParser.ruleEndStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  // NOTE: we do not match anything as we want lookahead functions to continue to work
  if not(poAllowEnd in FOptions) then
    RaiseError(LSymbol.Position, SEndNotExpected);
  exit(TEndStmt.Create(LSymbol.Position));
end;

function TTemplateParser.ruleExpression: IExpr;
var
  LSymbol: ITemplateSymbol;
  LRight: IExpr;
  LTrueExpr: IExpr;
  LFalseExpr: IExpr;
  LBinOp: TBinOp;
begin
  result := ruleSimpleExpression();
  LSymbol := FLookahead;
  if LSymbol.Token in [vsEQ, vsNotEQ, vsLT, vsLTE, vsGT, vsGTE, vsIn] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    match(LSymbol);
    LRight := ruleExpression;
    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(LRight) then
    begin
      case LBinOp of
        boEQ:
          exit(TValueExpr.Create(LSymbol.Position, isequal(AsValue(result), AsValue(LRight), FContext)));
        boNotEQ:
          exit(TValueExpr.Create(LSymbol.Position, not isequal(AsValue(result), AsValue(LRight), FContext)));
        boLT:
          exit(TValueExpr.Create(LSymbol.Position, isLessThan(AsValue(result), AsValue(LRight), FContext)));
        boGTE:
          exit(TValueExpr.Create(LSymbol.Position, not isLessThan(AsValue(result), AsValue(LRight), FContext)));
        boGT:
          exit(TValueExpr.Create(LSymbol.Position, isGreaterThan(AsValue(result), AsValue(LRight), FContext)));
        boLTE:
          exit(TValueExpr.Create(LSymbol.Position, not isGreaterThan(AsValue(result), AsValue(LRight), FContext)));
      end;
    end;
    result := TBinopExpr.Create(LSymbol.Position, result, LBinOp, LRight);
  end;

  if FLookahead.Token = vsQUESTION then
  begin
    match(vsQUESTION);
    LTrueExpr := ruleExpression();
    match(vsColon);
    LFalseExpr := ruleExpression();

    if (eoEvalEarly in FContext.Options) and IsValue(result) then
    begin
      if AsBoolean(AsValue(result)) then
        exit(LTrueExpr)
      else
        exit(LFalseExpr);
    end;
    exit(TTernaryExpr.Create(LSymbol.Position, result, LTrueExpr, LFalseExpr));
  end;
end;

function TTemplateParser.ruleFactor: IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  case LSymbol.Token of
    vsOpenSquareBracket:
      begin
        match(vsOpenSquareBracket);
        result := TArrayExpr.Create(LSymbol.Position, ruleExprList(vsCloseSquareBracket));
        match(vsCloseSquareBracket);
        exit;
      end;
    vsOpenRoundBracket:
      begin
        match(vsOpenRoundBracket);
        result := ruleExpression;
        match(vsCloseRoundBracket);
        exit;
      end;
    vsString:
      exit(TValueExpr.Create(LSymbol.Position, matchValue(vsString)));
    vsNumber:
      exit(TValueExpr.Create(LSymbol.Position, matchNumber(vsNumber)));
    vsBoolean:
      exit(TValueExpr.Create(LSymbol.Position, matchValue(vsBoolean) = 'true'));
    vsID:
      exit(self.ruleVariable());
    vsNOT:
      begin
        match(vsNOT);
        result := ruleExpression;
        if (eoEvalEarly in FContext.Options) and IsValue(result) then
          exit(TValueExpr.Create(LSymbol.Position, not AsBoolean(AsValue(result))))
        else
          exit(TUnaryExpr.Create(LSymbol.Position, uoNot, result));
      end;
  end;
end;

const
  ONFIRST_ONEND_ONLOOP_ELSE: TTemplateSymbolSet = [vsOnBegin, vsOnEnd, vsOnEmpty, vsBetweenItem, vsEND];

function TTemplateParser.ruleForStmt: IStmt;
var
  LId: string;
  LRangeExpr: IExpr;
  LLowValueExpr, LHighValueExpr: IExpr;
  LOffsetExpr, LLimitExpr, LStep: IExpr;
  LForOp: TForOp;
  LOptions: IPreserveValue<TParserOptions>;
  LContainerTemplate: ITemplate;
  LSymbol: ITemplateSymbol;
  LOnFirst, LOnEnd, LOnLoop, LOnEmpty, LBetweenItem: ITemplate;
  LPrevSymbol, LBlockSymbol: TTemplateSymbol;
  i: integer;
  procedure ResolveTemplate(const ASymbol: TTemplateSymbol);
  begin
    case ASymbol of
      vsInvalid:
        begin
          LOnLoop := LContainerTemplate;
        end;
      vsOnBegin:
        begin
          LOnFirst := LContainerTemplate;
        end;
      vsOnEnd:
        begin
          LOnEnd := LContainerTemplate;
        end;
      vsOnEmpty:
        begin
          LOnEmpty := LContainerTemplate;
        end;
      vsBetweenItem:
        begin
          LBetweenItem := LContainerTemplate;
        end;
    end;
  end;

begin
  LSymbol := FLookahead;
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poInLoop, poAllowEnd]);
  match(vsFor);
  LId := matchValue(vsID);
  if FLookahead.Token in [vsIn, vsOf] then
  begin
    LForOp := TemplateForop(LSymbol.Position, FLookahead.Token);
    match(FLookahead.Token);
    LRangeExpr := ruleExpression;
  end
  else
  begin
    match(vsCOLONEQ);
    LLowValueExpr := ruleExpression();
    LForOp := TemplateForop(LSymbol.Position, FLookahead.Token);
    if FLookahead.Token in [vsDownto, vsTo] then
      match(FLookahead.Token)
    else
      RaiseError(LSymbol.Position, SUnexpectedToken);
    LHighValueExpr := ruleExpression();
  end;

  if FLookahead.Token = vsOffset then
  begin
    match(vsOffset);
    LOffsetExpr := ruleExpression();
  end;

  if FLookahead.Token = vsLimit then
  begin
    match(vsLimit);
    LLimitExpr := ruleExpression();
  end;

  if FLookahead.Token = vsStep then
  begin
    match(vsStep);
    LStep := ruleExpression();
  end;

  match(vsEndScript);
  LBlockSymbol := vsInvalid;
  LPrevSymbol := vsInvalid;
  i := 0;
  repeat
    if (i > 1) and (i mod 2 = 0) then
    begin
      match(LBlockSymbol);
      match(vsEndScript);
    end;
    PushContainer;
    LContainerTemplate := CurrentContainer;
    LBlockSymbol := ruleStmts(LContainerTemplate, ONFIRST_ONEND_ONLOOP_ELSE);
    PopContainer;
    if (i mod 2 = 0) then
    begin
      ResolveTemplate(LPrevSymbol);
    end;
    LPrevSymbol := LBlockSymbol;
    inc(i);
  until LBlockSymbol = vsEND;
  match(vsEND);
  if LOnLoop = nil then
  begin
    LOnLoop := LContainerTemplate;
  end;

  match(vsEndScript);

  if LForOp in [TForOp.foIn, TForOp.foOf] then
    result := TForInStmt.Create(LSymbol.Position, LId, LForOp, LRangeExpr, LOffsetExpr, LLimitExpr, LOnLoop, LOnFirst, LOnEnd, LOnEmpty, LBetweenItem)
  else
    result := TForRangeStmt.Create(LSymbol.Position, LId, LForOp, LLowValueExpr, LHighValueExpr, LStep, LOnLoop, LOnFirst, LOnEnd, LOnEmpty, LBetweenItem);

end;

function TTemplateParser.ruleFunctionExpr(const ASymbol: string): IExpr;
var
  LFunctions: TArray<TRttiMethod>;
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  if not FContext.TryGetFunction(ASymbol, LFunctions) then
    RaiseError(LSymbol.Position, SFunctionNotRegisteredInContext, [ASymbol]);
  match(vsOpenRoundBracket);
  result := TFunctionCallExpr.Create(LSymbol.Position, LFunctions, ruleExprList);
  match(vsCloseRoundBracket);
end;

function TTemplateParser.ruleIdStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
begin
  LSymbol := FLookahead;
  LExpr := ruleVariable;
  if FLookahead.Token = vsCOLONEQ then
  begin
    result := ruleAssignStmt(LExpr);
  end
  else
  begin
    LExpr := TEncodeExpr.Create(LSymbol.Position, LExpr);
    result := rulePrintStmtVariable(LExpr);
  end;
  match(vsEndScript);
end;

function TTemplateParser.ruleWhileStmt: IStmt;
var
  LCondition: IExpr;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
  LOffsetExpr, LLimitExpr: IExpr;
  LContainerTemplate: ITemplate;
  LOnFirst, LOnEnd, LOnLoop, LOnEmpty, LBetweenItem: ITemplate;
  LPrevSymbol, LBlockSymbol: TTemplateSymbol;
  i: integer;
  procedure ResolveTemplate(const ASymbol: TTemplateSymbol);
  begin
    case ASymbol of
      vsInvalid:
        begin
          LOnLoop := LContainerTemplate;
        end;
      vsOnBegin:
        begin
          LOnFirst := LContainerTemplate;
        end;
      vsOnEnd:
        begin
          LOnEnd := LContainerTemplate;
        end;
      vsOnEmpty:
        begin
          LOnEmpty := LContainerTemplate;
        end;
      vsBetweenItem:
        begin
          LBetweenItem := LContainerTemplate;
        end;
    end;
  end;

begin
  LSymbol := FLookahead;
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poInLoop, poAllowEnd]);

  match(vsWhile);
  LCondition := ruleExpression;

  if FLookahead.Token = vsOffset then
  begin
    match(vsOffset);
    LOffsetExpr := ruleExpression();
  end;

  if FLookahead.Token = vsLimit then
  begin
    match(vsLimit);
    LLimitExpr := ruleExpression();
  end;

  match(vsEndScript);
  LBlockSymbol := vsInvalid;
  LPrevSymbol := vsInvalid;
  i := 0;
  repeat
    if (i > 1) and (i mod 2 = 0) then
    begin
      match(LBlockSymbol);
      match(vsEndScript);
    end;
    PushContainer;
    LContainerTemplate := CurrentContainer;
    LBlockSymbol := ruleStmts(LContainerTemplate, ONFIRST_ONEND_ONLOOP_ELSE);
    PopContainer;
    if (i mod 2 = 0) then
    begin
      ResolveTemplate(LPrevSymbol);
    end;
    LPrevSymbol := LBlockSymbol;
    inc(i);
  until LBlockSymbol = vsEND;
  match(vsEND);
  if LOnLoop = nil then
  begin
    LOnLoop := LContainerTemplate;
  end;
  match(vsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(LCondition) and not AsBoolean(AsValue(LCondition)) then
    result := nil
  else
    result := TWhileStmt.Create(LSymbol.Position, LCondition, LOffsetExpr, LLimitExpr, LOnLoop, LOnFirst, LOnEnd, LOnEmpty, LBetweenItem);
end;

function TTemplateParser.ruleWithStmt: IStmt;
var
  LExpr: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;

  match(vswith);
  LExpr := ruleExpression;
  match(vsEndScript);

  PushContainer;
  LContainer := CurrentContainer;

  ruleStmts(LContainer, [vsEND]);

  match(vsEND);
  match(vsEndScript);

  PopContainer;
  exit(TWithStmt.Create(LSymbol.Position, LExpr, LContainer));

end;

function TTemplateParser.rulePrintStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
begin
  LSymbol := FLookahead;
  match(vsPrint);
  match(vsOpenRoundBracket);
  LExpr := ruleExpression;
  match(vsCloseRoundBracket);
  match(vsEndScript);
  exit(TPrintStmt.Create(LSymbol.Position, LExpr));
end;

function TTemplateParser.rulePrintStmtVariable(AExpr: IExpr): IStmt;
var
  LSymbol: ITemplateSymbol;
  LValueExpr: IValueExpr;
begin
  LSymbol := FLookahead;
  if supports(AExpr, IValueExpr, LValueExpr) and (asString(LValueExpr.Value, FContext) = '') then
    exit(nil);
  exit(TPrintStmt.Create(LSymbol.Position, AExpr));
end;

function TTemplateParser.ruleExprList(const AEndToken: TTemplateSymbol): IExprList;
var
  LSymbol: ITemplateSymbol;
  LValueSeparator: TTemplateSymbol;
begin
  LSymbol := FLookahead;
  result := TExprList.Create(LSymbol.Position);
  if FLookahead.Token <> AEndToken then
    result.AddExpr(ruleExpression);
  if FContext.ValueSeparator = ';' then
    LValueSeparator := vsSemiColon
  else
    LValueSeparator := vsComma;
  while FLookahead.Token = LValueSeparator do
  begin
    match(LValueSeparator);
    result.AddExpr(ruleExpression);
  end;
end;

function TTemplateParser.ruleExprStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  result := rulePrintStmtVariable(TEncodeExpr.Create(LSymbol.Position, ruleExpression));
  match(vsEndScript);
end;

function TTemplateParser.ruleExtendsStmt: IStmt;
var
  LName: IExpr;
  LScopeExpr: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
  LContainerTemplate: TTemplate;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;

  match(vsExtends);
  match(vsOpenRoundBracket);
  LName := ruleExpression;

  if FLookahead.Token = vsComma then
  begin
    match(vsComma);
    LScopeExpr := ruleExpression;
  end;

  match(vsCloseRoundBracket);

  match(vsEndScript);

  PushContainer;
  LContainer := CurrentContainer;

  ruleStmts(LContainer, [vsEND]);

  match(vsEND);
  match(vsEndScript);

  PopContainer;

  if LScopeExpr <> nil then
  begin
    LContainerTemplate := TTemplate.Create();
    LContainerTemplate.Add(TExtendsStmt.Create(LSymbol.Position, LName, LContainer));
    exit(TWithStmt.Create(LSymbol.Position, LScopeExpr, LContainerTemplate));
  end
  else
  begin
    exit(TExtendsStmt.Create(LSymbol.Position, LName, LContainer));
  end;
end;

function TTemplateParser.CurrentContainer: ITemplate;
begin
  if FContainerStack.Count <> 0 then
    exit(FContainerStack.Peek)
  else
    exit(nil);
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
    exit(val.Value)
  else
    exit('');
end;

procedure TTemplateParser.match(const ASymbol: TTemplateSymbol);
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  if ASymbol = FLookahead.Token then
  begin
    if LSymbol.StripWS then
    begin
      case ASymbol of
        vsStartScript:
          exclude(FOptions, poStripWS);
        vsEndScript:
          include(FOptions, poStripWS);
      end;
    end;
    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(LSymbol.Position, format(SParsingErrorExpecting, [TemplateSymbolToString(ASymbol)]));
end;

procedure TTemplateParser.match(ASymbol: ITemplateSymbol);
begin
  match(ASymbol.Token);
end;

function TTemplateParser.matchNumber(const ASymbol: TTemplateSymbol): extended;
begin
  exit(StrToFloat(matchValue(ASymbol), FContext.FormatSettings));
end;

function TTemplateParser.matchValue(const ASymbol: TTemplateSymbol): string;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  if ASymbol = FLookahead.Token then
  begin
    result := lookaheadValue;

    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(LSymbol.Position, format(SParsingErrorExpecting, [TemplateSymbolToString(ASymbol)]));
end;

function TTemplateParser.Parse(const AStream: TStream; const AManagedStream: boolean): ITemplate;
begin
  FContainerStack.Clear;
  PushContainer;
  FLexer := CreateTemplateLexer(FContext, AStream, '', AManagedStream);
  FLookahead := FLexer.GetToken;
  ruleStmts(CurrentContainer, []);
  match(vsEOF);
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

procedure TValueExpr.Accept(const AVisitor: ITemplateVisitor);
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
  exit(FValue);
end;

{ TExprList }

procedure TExprList.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

procedure TExprList.AddExpr(const AExpr: IExpr);
var
  LOffset: integer;
begin
  LOffset := length(FExprs);
  setlength(FExprs, LOffset + 1);
  FExprs[LOffset] := AExpr;
end;

function TExprList.Clone: IInterface;
begin
  exit(self);
end;

function TExprList.GetExpr(const AOffset: integer): IExpr;
begin
  exit(FExprs[AOffset]);
end;

function TExprList.GetExprCount: integer;
begin
  exit(length(FExprs));
end;

{ TUnaryExpr }

procedure TUnaryExpr.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TUnaryExpr.Create(const APosition: IPosition; const AUnaryOp: TUnaryOp; const ACondition: IExpr);
begin
  inherited Create(APosition, ACondition);
  FUnaryOp := AUnaryOp;
end;

function TUnaryExpr.GetUnaryOp: TUnaryOp;
begin
  exit(FUnaryOp);
end;

{ TVariableExpr }

procedure TVariableExpr.Accept(const AVisitor: ITemplateVisitor);
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
  exit(FVariable);
end;

{ TFunctionCallExpr }

procedure TFunctionCallExpr.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TFunctionCallExpr.Create(const APosition: IPosition; const AFunction: TArray<TRttiMethod>; const ExprList: IExprList);

begin
  inherited Create(APosition, ExprList);
  FFunctionInfo := AFunction;
end;

function TFunctionCallExpr.GetFunctionInfo: TArray<TRttiMethod>;
begin
  exit(FFunctionInfo);
end;

{ TIfStmt }

procedure TIfStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TIfStmt.Clone: IInterface;
begin
  exit(TIfStmt.Create(FPosition, FCondition, CloneTemplate(FTrueContainer), CloneTemplate(FFalseContainer)));
end;

constructor TIfStmt.Create(const APosition: IPosition; const ACondition: IExpr; const ATrueContainer: ITemplate; const AFalseContainer: ITemplate);
begin
  inherited Create(APosition);
  FCondition := ACondition;
  FTrueContainer := ATrueContainer;
  FFalseContainer := AFalseContainer;
end;

function TIfStmt.GetCondition: IExpr;
begin
  exit(FCondition);
end;

function TIfStmt.GetFalseContainer: ITemplate;
begin
  exit(FFalseContainer);
end;

function TIfStmt.GetTrueContainer: ITemplate;
begin
  exit(FTrueContainer);
end;

{ TBinopExpr }

procedure TBinopExpr.Accept(const AVisitor: ITemplateVisitor);
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
  inherited;
end;

function TBinopExpr.GetBinOp: TBinOp;
begin
  exit(FBinop);
end;

function TBinopExpr.GetLeftExpr: IExpr;
begin
  exit(FLeft);
end;

function TBinopExpr.GetRightExpr: IExpr;
begin
  exit(FRight);
end;

{ TPrintStmt }

procedure TPrintStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TPrintStmt.Clone: IInterface;
begin
  exit(TPrintStmt.Create(FPosition, FExpr));
end;

{ TForInStmt }

procedure TForInStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TForInStmt.Clone: IInterface;
begin
  exit(TForInStmt.Create(FPosition, FVariable, FForOp, FExpr, FOffsetExpr, FLimitExpr, CloneTemplate(FContainer), CloneTemplate(FOnFirst), CloneTemplate(FOnLast), CloneTemplate(FOnEmpty), CloneTemplate(FBetweenItem)));
end;

constructor TForInStmt.Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const AExpr: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer, AOnFirst, AOnEnd, AOnEmpty, ABetweenItem);
  FVariable := AVariable;
  FForOp := AForOp;
  FExpr := AExpr;
  FOffsetExpr := AOffsetExpr;
  FLimitExpr := ALimitExpr;
end;

function TForInStmt.GetForOp: TForOp;
begin
  exit(FForOp);
end;

function TForInStmt.GetExpr: IExpr;
begin
  exit(FExpr);
end;

function TForInStmt.GetLimitExpr: IExpr;
begin
  exit(FLimitExpr);
end;

function TForInStmt.GetOffsetExpr: IExpr;
begin
  exit(FOffsetExpr);
end;

function TForInStmt.GetVariable: string;
begin
  exit(FVariable);
end;

{ TForRangeStmt }

procedure TForRangeStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TForRangeStmt.Clone: IInterface;
begin
  exit(TForRangeStmt.Create(FPosition, FVariable, FForOp, FLowExpr, FHighExpr, FStepExpr, CloneTemplate(FContainer), CloneTemplate(FOnFirst), CloneTemplate(FOnLast), CloneTemplate(FOnEmpty), CloneTemplate(FBetweenItem)));
end;

constructor TForRangeStmt.Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const ALowExpr: IExpr; const AHighExpr: IExpr; const AStep: IExpr; const AContainer: ITemplate; const AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer, AOnFirst, AOnEnd, AOnEmpty, ABetweenItem);
  FVariable := AVariable;
  FForOp := AForOp;
  FLowExpr := ALowExpr;
  FHighExpr := AHighExpr;
  FStepExpr := AStep;
end;

function TForRangeStmt.GetForOp: TForOp;
begin
  exit(FForOp);
end;

function TForRangeStmt.GetHighExpr: IExpr;
begin
  exit(FHighExpr);
end;

function TForRangeStmt.GetLowExpr: IExpr;
begin
  exit(FLowExpr);
end;

function TForRangeStmt.GetStepExpr: IExpr;
begin
  exit(FStepExpr);
end;

function TForRangeStmt.GetVariable: string;
begin
  exit(FVariable);
end;

{ TAssignStmt }

procedure TAssignStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TAssignStmt.Clone: IInterface;
begin
  exit(self);
end;

constructor TAssignStmt.Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr);
begin
  inherited Create(APosition, AExpr);
  FVariable := AVariable;
end;

function TAssignStmt.GetVariable: string;
begin
  exit(FVariable);
end;

{ TTemplateContainer }

procedure TTemplate.Accept(const AVisitor: ITemplateVisitor);
var
  i: ITemplateVisitorHost;
begin
  for i in FArray do
  begin
    i.Accept(AVisitor);
  end;
end;

procedure TTemplate.Add(const AItem: ITemplateVisitorHost);
var
  LOffset: integer;
begin
  LOffset := length(FArray);
  setlength(FArray, LOffset + 1);
  FArray[LOffset] := AItem;
end;

function TTemplate.Clone: IInterface;
var
  i: ITemplateVisitorHost;
  LTemplate: TTemplate;
begin
  LTemplate := TTemplate.Create;
  result := LTemplate;
  for i in FArray do
  begin
    LTemplate.Add(CloneVisitorHost(i));
  end;
end;

function TTemplate.GetCount: integer;
begin
  exit(length(FArray));
end;

function TTemplate.GetLastItem: ITemplateVisitorHost;
begin
  if GetCount = 0 then
    exit(nil)
  else
    exit(GetItem(GetCount - 1));
end;

function TTemplate.GetItem(const AOffset: integer): ITemplateVisitorHost;
begin
  exit(FArray[AOffset]);
end;

{ TWhileStmt }

procedure TWhileStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TWhileStmt.Clone: IInterface;
begin
  exit(TWhileStmt.Create(FPosition, FCondition, FOffsetExpr, FLimitExpr, CloneTemplate(FContainer), CloneTemplate(FOnFirst), CloneTemplate(FOnLast), CloneTemplate(FOnEmpty), CloneTemplate(FBetweenItem)));
end;

constructor TWhileStmt.Create(const APosition: IPosition; const ACondition: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer, AOnFirst, AOnEnd, AOnEmpty, ABetweenItem);
  FCondition := ACondition;
  FOffsetExpr := AOffsetExpr;
  FLimitExpr := ALimitExpr;
end;

function TWhileStmt.GetCondition: IExpr;
begin
  exit(FCondition);
end;

function TWhileStmt.GetLimitExpr: IExpr;
begin
  exit(FLimitExpr);
end;

function TWhileStmt.GetOffsetExpr: IExpr;
begin
  exit(FOffsetExpr);
end;

{ TContinueStmt }

procedure TContinueStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TContinueStmt.Clone: IInterface;
begin
  exit(self);
end;

{ TBreakStmt }

procedure TBreakStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TBreakStmt.Clone: IInterface;
begin
  exit(self);
end;

{ TEndStmt }

procedure TEndStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TEndStmt.Clone: IInterface;
begin
  exit(self);
end;

{ TVariableDerefExpr }

procedure TVariableDerefExpr.Accept(const AVisitor: ITemplateVisitor);
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
  exit(FDeref);
end;

function TVariableDerefExpr.GetDerefType: TDerefType;
begin
  exit(FDerefType);
end;

{ TIncludeStmt }

procedure TIncludeStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TIncludeStmt.Clone: IInterface;
begin
  exit(self);
end;

{ TElseStmt }

procedure TElseStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TElseStmt.Clone: IInterface;
begin
  exit(self);
end;

{ TElIfStmt }

procedure TElIfStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  // AVisitor.Visit(self);   // this is on purpose
end;

function TElIfStmt.Clone: IInterface;
begin
  exit(self);
end;

{ TCommentStmt }

procedure TCommentStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  // AVisitor.Visit(self);     // this is on purpose
end;

function TCommentStmt.Clone: IInterface;
begin
  exit(self);
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
  exit(FPosition);
end;

{ TMethodCallExpr }

procedure TMethodCallExpr.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TMethodCallExpr.Create(const APosition: IPosition; const AObjectExpr: IExpr; const AMethod: string; const AExprList: IExprList);
begin
  inherited Create(APosition, AExprList);
  FObjectExpr := AObjectExpr;
  FMethod := AMethod;
end;

function TMethodCallExpr.GetMethod: string;
begin
  exit(FMethod);
end;

function TMethodCallExpr.GetObject: IExpr;
begin
  exit(FObjectExpr);
end;

function TMethodCallExpr.GetRttiMethod: TRttiMethod;
begin
  exit(FRttiMethod);
end;

procedure TMethodCallExpr.SetRttiMethod(const ARttiMethod: TRttiMethod);
begin
  FRttiMethod := ARttiMethod;
end;

{ TEncodeStmt }

procedure TEncodeExpr.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TProcessTemplateStmt }

procedure TProcessTemplateStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TProcessTemplateStmt.Clone: IInterface;
begin
  exit(TProcessTemplateStmt.Create(FPosition, CloneTemplate(FContainer), FAllowNewline));
end;

constructor TProcessTemplateStmt.Create(const APosition: IPosition; const AContainer: ITemplate; const AAllowNewLine: boolean);
begin
  inherited Create(APosition, AContainer);
  FAllowNewline := AAllowNewLine;
end;

function TProcessTemplateStmt.GetAllowNewLine: boolean;
begin
  exit(FAllowNewline);
end;

procedure TProcessTemplateStmt.SetAllowNewLine(const AAllow: boolean);
begin
  FAllowNewline := AAllow;
end;

{ TDefineTemplateStmt }

procedure TDefineTemplateStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TDefineTemplateStmt.Clone: IInterface;
begin
  exit(TDefineTemplateStmt.Create(FPosition, FName, CloneTemplate(FContainer)));
end;

constructor TDefineTemplateStmt.Create(const APosition: IPosition; const AName: IExpr; const AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FName := AName;
end;

function TDefineTemplateStmt.GetName: IExpr;
begin
  exit(FName);
end;

{ TWithStmt }

procedure TWithStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TWithStmt.Clone: IInterface;
begin
  exit(TWithStmt.Create(FPosition, FExpr, CloneTemplate(FContainer)));
end;

constructor TWithStmt.Create(const APosition: IPosition; const AExpr: IExpr; const AContainer: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FExpr := AExpr;
end;

function TWithStmt.GetExpr: IExpr;
begin
  exit(FExpr);
end;

{ TIfExpr }

procedure TTernaryExpr.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TTernaryExpr.Create(const APosition: IPosition; const ACondition, ATrueExpr, AFalseExpr: IExpr);
begin
  inherited Create(APosition, ACondition);
  FTrueExpr := ATrueExpr;
  FFalseExpr := AFalseExpr;
end;

function TTernaryExpr.GetFalseExpr: IExpr;
begin
  exit(FFalseExpr);
end;

function TTernaryExpr.GetTrueExpr: IExpr;
begin
  exit(FTrueExpr);
end;

{ TArrayExpr }

procedure TArrayExpr.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

procedure initOps;
var
  LSymbol: TTemplateSymbol;
begin
  for LSymbol := low(TTemplateSymbol) to high(TTemplateSymbol) do
    GTemplateBinOps[LSymbol] := boInvalid;
  GTemplateBinOps[vsIn] := boIN;
  GTemplateBinOps[vsAND] := boAND;
  GTemplateBinOps[vsOR] := boOR;
  GTemplateBinOps[vsPLUS] := boPlus;
  GTemplateBinOps[vsMinus] := boMinus;
  GTemplateBinOps[vsMULT] := boMult;
  GTemplateBinOps[vsSLASH] := boSlash;
  GTemplateBinOps[vsDIV] := boDiv;
  GTemplateBinOps[vsMOD] := boMod;
  GTemplateBinOps[vsLT] := boLT;
  GTemplateBinOps[vsLTE] := boLTE;
  GTemplateBinOps[vsGT] := boGT;
  GTemplateBinOps[vsGTE] := boGTE;
  GTemplateBinOps[vsEQ] := boEQ;
  GTemplateBinOps[vsNotEQ] := boNotEQ;
end;

{ TRequireStmt }

procedure TRequireStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TRequireStmt.Clone: IInterface;
begin
  exit(self);
end;

constructor TRequireStmt.Create(const APosition: IPosition; const AExprList: IExprList);
begin
  inherited Create(APosition);
  FExprList := AExprList;
end;

function TRequireStmt.GetExprList: IExprList;
begin
  exit(FExprList);
end;

{ TAbstractStmtWithExpr }

constructor TAbstractStmtWithExpr.Create(APosition: IPosition; AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TAbstractStmtWithExpr.GetExpr: IExpr;
begin
  exit(FExpr);
end;

{ TAbstractStmtWithContainer }

constructor TAbstractStmtWithContainer.Create(APosition: IPosition; AContainer: ITemplate);
begin
  inherited Create(APosition);
  FContainer := AContainer;
end;

function TAbstractStmtWithContainer.GetContainer: ITemplate;
begin
  exit(FContainer);
end;

{ TAbstractExprWithExprList }

constructor TAbstractExprWithExprList.Create(APosition: IPosition; AExprList: IExprList);
begin
  inherited Create(APosition);
  FExprList := AExprList;
end;

function TAbstractExprWithExprList.GetExprList: IExprList;
begin
  exit(FExprList);
end;

{ TAbstractExprWithExpr }

constructor TAbstractExprWithExpr.Create(APosition: IPosition; AExpr: IExpr);
begin
  inherited Create(APosition);
  FExpr := AExpr;
end;

function TAbstractExprWithExpr.GetExpr: IExpr;
begin
  exit(FExpr);
end;

{ TCycleStmt }

procedure TCycleStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TCycleStmt.Clone: IInterface;
begin
  exit(self);
end;

constructor TCycleStmt.Create(const APosition: IPosition; const AList: IExprList);
begin
  inherited Create(APosition);
  FExprList := AList;
end;

function TCycleStmt.GetList: IExprList;
begin
  exit(FExprList);
end;

{ TDebugStmt }

procedure TDebugStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TDebugStmt.Clone: IInterface;
begin
  exit(TDebugStmt.Create(CloneStmt(FStmt)));
end;

constructor TDebugStmt.Create(const AStmt: IStmt);
begin
  FStmt := AStmt;
end;

function TDebugStmt.GetStmt: IStmt;
begin
  exit(FStmt);
end;

{ TLoopStmt }

constructor TLoopStmt.Create(APosition: IPosition; AContainer, AOnFirst, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FOnFirst := AOnFirst;
  FOnLast := AOnEnd;
  FOnEmpty := AOnEmpty;
  FBetweenItem := ABetweenItem;
end;

function TLoopStmt.GetBetweenItemContainer: ITemplate;
begin
  exit(FBetweenItem);
end;

function TLoopStmt.GetOnEmptyContainer: ITemplate;
begin
  exit(FOnEmpty);
end;

function TLoopStmt.GetOnEndContainer: ITemplate;
begin
  exit(FOnLast);
end;

function TLoopStmt.GetOnFirstContainer: ITemplate;
begin
  exit(FOnFirst);
end;

{ TBlockStmt }

procedure TBlockStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TBlockStmt.Clone: IInterface;
begin
  exit(TBlockStmt.Create(FPosition, FName, CloneTemplate(FContainer)));
end;

constructor TBlockStmt.Create(const APosition: IPosition; const AName: IExpr; const AContainer: ITemplate);
begin
  inherited Create(APosition);
  FName := AName;
  FContainer := AContainer;
end;

function TBlockStmt.GetContainer: ITemplate;
begin
  exit(FContainer);
end;

function TBlockStmt.GetName: IExpr;
begin
  exit(FName);
end;

function TBlockStmt.NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
begin
  exit(AEvalVisitor.EvalExprAsString(FName));
end;

procedure TBlockStmt.SetContainer(const AContainer: ITemplate);
begin
  FContainer := AContainer;
end;

{ TExtendsStmt }

procedure TExtendsStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

function TExtendsStmt.Clone: IInterface;
begin
  exit(TExtendsStmt.Create(FPosition, FName, CloneTemplate(FBlockContainer)));
end;

constructor TExtendsStmt.Create(const APosition: IPosition; const AName: IExpr; const ABlockContainer: ITemplate);
begin
  inherited Create(APosition);
  FBlockContainer := ABlockContainer;
  FName := AName;
end;

function TExtendsStmt.GetBlockContainer: ITemplate;
begin
  exit(FBlockContainer);
end;

function TExtendsStmt.GetBlockNames: TArray<string>;
begin
  exit(FBlockNames);
end;

function TExtendsStmt.GetContainer: ITemplate;
begin
  exit(FContainer);
end;

function TExtendsStmt.GetName: IExpr;
begin
  exit(FName);
end;

function TExtendsStmt.NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
begin
  exit(AEvalVisitor.EvalExprAsString(FName));
end;

procedure TExtendsStmt.SetBlockNames(const ANames: TArray<string>);
begin
  FBlockNames := ANames;
end;

procedure TExtendsStmt.SetContainer(const AContainer: ITemplate);
begin
  FContainer := AContainer;
end;

{ TAbstractExpr }

function TAbstractExpr.Clone: IInterface;
begin
  exit(self);
end;

initialization

initOps;

end.
