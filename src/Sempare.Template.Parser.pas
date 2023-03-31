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
  System.Rtti,
  System.Generics.Collections,
  Sempare.Template,
  Sempare.Template.BlockResolver,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Common,
  Sempare.Template.Lexer,
  Sempare.Template.Rtti,
  Sempare.Template.Util;

type

  TTemplate = class(TInterfacedObject, ITemplate, ITemplateAdd, ITemplateVisitorHost)
  private
    FPosition: IPosition;
    FArray: TArray<IStmt>;
    function Flatten: TArray<IStmt>;
    procedure Optimise;
    function GetFilename: string;
    procedure SetFilename(const AFilename: string);
    function GetLine: integer;
    procedure SetLine(const Aline: integer);
    function GetPos: integer;
    procedure SetPos(const Apos: integer);
    function GetItem(const AOffset: integer): IStmt;
    function GetCount: integer;
    procedure Add(const AItem: IStmt);
    function GetLastItem: IStmt;
    procedure Accept(const AVisitor: ITemplateVisitor);

  public
    constructor Create();
  end;

  TAbstractBase = class abstract(TInterfacedObject, IPositional, ITemplateVisitorHost)
  private
    FPosition: IPosition;
    function GetPosition: IPosition;
    function GetFilename: string;
    procedure SetFilename(const AFilename: string);
    function GetLine: integer;
    procedure SetLine(const Aline: integer);
    function GetPos: integer;
    procedure SetPos(const Apos: integer);
  public
    constructor Create(const APosition: IPosition);
    destructor Destroy; override;
    procedure Accept(const AVisitor: ITemplateVisitor); virtual; abstract;
  end;

  TAbstractStmt = class abstract(TAbstractBase, IStmt)
  private
    function Flatten: TArray<IStmt>; virtual;
  public
    constructor Create(const APosition: IPosition);
  end;

  TDebugStmt = class(TAbstractStmt, IDebugStmt)
  private
    FStmt: IStmt;
    function GetStmt: IStmt;
  public
    constructor Create(const AStmt: IStmt);
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TEndStmt = class(TAbstractStmt, IEndStmt)
  public
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TExtendsStmt = class(TAbstractStmt, IExtendsStmt)
  private
    FName: IExpr;
    FBlockContainer: ITemplate;
    function GetName: IExpr;
    function GetBlockContainer: ITemplate;
    function NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const ABlockContainer: ITemplate);
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TBlockStmt = class(TAbstractStmt, IBlockStmt)
  private
    FName: IExpr;
    FContainer: ITemplate;
    function GetName: IExpr;
    function GetContainer: ITemplate;
    function NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const AContainer: ITemplate);
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TElseStmt = class(TAbstractStmt, IElseStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TNoopStmt = class(TAbstractStmt, INoopStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TContinueStmt = class(TAbstractStmt, IContinueStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TBreakStmt = class(TAbstractStmt, IBreakStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TCommentStmt = class(TAbstractStmt, ICommentStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TElIfStmt = class(TAbstractStmt, IElIfStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
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
  end;

  TStripStmt = class(TAbstractStmt, IStripStmt)
  private
    FDirection: TStripDirection;
    FAction: TStripAction;
    function GetDirection: TStripDirection;
    function GetAction: TStripAction;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const ADirection: TStripDirection; const AAction: TStripAction);
  end;

  TCompositeStmt = class(TAbstractStmt, ICompositeStmt)
  private
    FFirstStmt: IStmt;
    FSecondStmt: IStmt;
    function GetFirstStmt: IStmt;
    function GetSecondStmt: IStmt;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
    function Flatten: TArray<IStmt>; override;
  public
    constructor Create(const AFirstStmt, ASecondStmt: IStmt);
  end;

  TIncludeStmt = class(TAbstractStmtWithExpr, IIncludeStmt)
  private
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  end;

  TRequireStmt = class(TAbstractStmt, IRequireStmt)
  private
    FExprList: IExprList;
    function GetExprList: IExprList;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
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
  public
    constructor Create(const APosition: IPosition; const AContainer: ITemplate; const AAllowNewLine: boolean = true);
  end;

  TDefineTemplateStmt = class(TAbstractStmtWithContainer, IDefineTemplateStmt)
  private
    FName: IExpr;
    function GetName: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AName: IExpr; const AContainer: ITemplate);
  end;

  TWithStmt = class(TAbstractStmtWithContainer, IWithStmt)
  private
    FExpr: IExpr;
    function GetExpr: IExpr;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AExpr: IExpr; const AContainer: ITemplate);
  end;

  TLoopStmt = class(TAbstractStmtWithContainer, ILoopStmt)
  protected
    FOnBegin: ITemplate;
    FOnEnd: ITemplate;
    FOnEmpty: ITemplate;
    FBetweenItem: ITemplate;

    function GetOnBeginContainer: ITemplate;
    function GetOnEndContainer: ITemplate;
    function GetOnEmptyContainer: ITemplate;
    function GetBetweenItemContainer: ITemplate;
  public
    constructor Create(APosition: IPosition; AContainer: ITemplate; AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
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
  public
    constructor Create(const APosition: IPosition; const ACondition: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
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
  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const AExpr: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
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
  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const ALowExpr: IExpr; const AHighExpr: IExpr; const AStep: IExpr; const AContainer: ITemplate; const AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
  end;

  TAssignStmt = class(TAbstractStmtWithExpr, IAssignStmt)
  private
    FVariable: string;
    function GetVariable: string;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
  public
    constructor Create(const APosition: IPosition; const AVariable: string; const AExpr: IExpr);
  end;

  TCycleStmt = class(TAbstractStmt, ICycleStmt)
  private
    FExprList: IExprList;
    procedure Accept(const AVisitor: ITemplateVisitor); override;
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
  public
  end;

  TAbstractExpr = class abstract(TAbstractBase, IExpr)
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
    function MatchValue(const ASymbol: TTemplateSymbol): string;
    procedure Match(ASymbol: ITemplateSymbol); overload; inline;
    function Match(const ASymbol: TTemplateSymbol): TStripAction; overload;
    function MatchNumber(const ASymbol: TTemplateSymbol): extended;
    procedure MatchClosingBracket(const AExpect: TTemplateSymbol);
    function AddStripStmt(const AStmt: IStmt; const AStripAction: TStripAction; const ADirection: TStripDirection): IStmt;
    function AddEndStripStmt(const ATemplate: ITemplate; const AStripAction: TStripAction): ITemplate;
  private
    function RuleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet): TTemplateSymbol;
    function RuleStmt: IStmt;
    function RuleIgnoreNewline: IStmt;
    function RuleCommentStmt: IStmt;
    function RuleIdStmt: IStmt;
    function RuleExprStmt: IStmt;
    function RuleIncludeStmt: IStmt;
    function RulePrintStmt: IStmt;
    function RuleEndStmt: IStmt;
    function RuleContinueStmt: IStmt;
    function RuleBreakStmt: IStmt;
    function RuleIfStmt: IStmt;
    function RuleElIfStmt: IStmt;
    function RuleExprList(const AEndToken: TTemplateSymbol = vsCloseRoundBracket): IExprList;
    function RuleAssignStmt(ASymbol: IExpr): IStmt;
    function RulePrintStmtVariable(AExpr: IExpr): IStmt; overload;
    function RuleForStmt: IStmt;
    function RuleWhileStmt: IStmt;
    function RuleWithStmt: IStmt;
    function RuleCycleStmt: IStmt;
    function RuleTemplateStmt: IStmt;

    function RuleBlockStmt: IStmt;
    function RuleExtendsStmt: IStmt;

    function RuleExpression: IExpr;
    function RuleSimpleExpression: IExpr;
    function RuleTerm: IExpr;
    function RuleSignedFactor: IExpr;
    function RuleFactor: IExpr;
    function RuleVariable: IExpr;

    function RuleFunctionExpr(const ASymbol: string): IExpr;
    function RuleMethodExpr(AExpr: IExpr; AMethodExpr: IExpr): IExpr;
    function RuleRequireStmt: IStmt;
    function RuleNoopStmt: IStmt;

    function GetValueSeparatorSymbol: TTemplateSymbol;
  public
    constructor Create(AContext: ITemplateContext);
    destructor Destroy; override;
    function Parse(const AStream: TStream; const AManagedStream: boolean): ITemplate;
  end;

function Flatten(const AStmts: TArray<IStmt>): TArray<IStmt>;
var
  LStmt: IStmt;
begin
  result := nil;
  for LStmt in AStmts do
  begin
    result := result + LStmt.Flatten;
  end;
end;

procedure Optimise(const ATemplates: TArray<ITemplate>);
var
  LTemplate: ITemplate;
begin
  for LTemplate in ATemplates do
  begin
    if assigned(LTemplate) then
    begin
      LTemplate.Optimise;
    end;
  end;
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
  LValueExpr := AExpr as IValueExpr;
  exit(LValueExpr.Value);
end;

function AsVarString(AExpr: IExpr): string;
var
  LVar: IVariableExpr;
begin
  if supports(AExpr, IVariableExpr, LVar) then
    exit(LVar.Variable)
  else
    exit('');
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
const
  ValueSeparators = [vsComma, vsSemiColon];

function OppositeValueSepartor(const ASep: TTemplateSymbol): TTemplateSymbol;
begin
  if ASep = vsComma then
    exit(vsSemiColon)
  else
    exit(vsComma);
end;

function TTemplateParser.GetValueSeparatorSymbol: TTemplateSymbol;
begin
  if FContext.ValueSeparator = ';' then
    exit(vsSemiColon)
  else
    exit(vsComma);
end;

procedure TTemplateParser.MatchClosingBracket(const AExpect: TTemplateSymbol);
begin
  assert(AExpect in [vsCloseRoundBracket, vsCloseSquareBracket]);
  if FLookahead.Token in ValueSeparators then
    Match(OppositeValueSepartor(FLookahead.Token));
  Match(AExpect);
end;

function TTemplateParser.AddEndStripStmt(const ATemplate: ITemplate; const AStripAction: TStripAction): ITemplate;
var
  LAdd: ITemplateAdd;
begin
  if AStripAction = saNone then
    exit(ATemplate);
  result := ATemplate;
  supports(result, ITemplateAdd, LAdd);
  LAdd.Add(TStripStmt.Create(sdRight, AStripAction));
end;

function TTemplateParser.AddStripStmt(const AStmt: IStmt; const AStripAction: TStripAction; const ADirection: TStripDirection): IStmt;
var
  LStripStmt: IStmt;
begin
  if AStripAction = saNone then
    exit(AStmt)
  else
  begin
    LStripStmt := TStripStmt.Create(ADirection, AStripAction);
    if ADirection = sdLeft then
      exit(TCompositeStmt.Create(LStripStmt, AStmt))
    else
      exit(TCompositeStmt.Create(AStmt, LStripStmt));
  end;
end;

constructor TTemplateParser.Create(AContext: ITemplateContext);
begin
  FOptions := [];
  FContext := AContext;
  FContainerStack := TStack<ITemplate>.Create;
end;

const
  IF_ELIF_END: TTemplateSymbolSet = [vsELIF, vsElse, vsEND];

function TTemplateParser.RuleIfStmt: IStmt;
var
  LConditionalExpr: IExpr;
  LTrueContainer: ITemplate;
  LFalseContainer: ITemplate;
  LContainerAdd: ITemplateAdd;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
  LEndStripAction: TStripAction;
  LTrueStripAction: TStripAction;
  LFalseStripAction: TStripAction;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowElse, poAllowEnd, poAllowElIf]);
  LSymbol := FLookahead;
  Match(vsIF);
  LConditionalExpr := RuleExpression;

  LTrueStripAction := Match(vsEndScript);
  // create new container for true condition
  PushContainer;
  LTrueContainer := self.CurrentContainer;
  AddEndStripStmt(LTrueContainer, LTrueStripAction);

  RuleStmts(LTrueContainer, IF_ELIF_END);
  PopContainer;

  PushContainer;
  LFalseContainer := self.CurrentContainer;
  LContainerAdd := LFalseContainer as ITemplateAdd;

  if FLookahead.Token = vsELIF then
  begin
    while (FLookahead.Token = vsELIF) do
    begin
      LContainerAdd.Add(RuleElIfStmt());
    end;
  end
  else if FLookahead.Token = vsElse then
  begin
    Match(vsElse);
    LFalseStripAction := Match(vsEndScript);
    AddEndStripStmt(LFalseContainer, LFalseStripAction);
    RuleStmts(LFalseContainer, [vsEND]);
  end;
  PopContainer;
  Match(vsEND);
  LEndStripAction := Match(vsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(LConditionalExpr) then
  begin
    if AsBoolean(AsValue(LConditionalExpr)) then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LTrueContainer))
    else if LFalseContainer <> nil then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LFalseContainer))
  end;
  result := TIfStmt.Create(LSymbol.Position, LConditionalExpr, LTrueContainer, LFalseContainer);
  result := AddStripStmt(result, LEndStripAction, sdRight);
end;

function TTemplateParser.RuleIgnoreNewline: IStmt;
var
  LSymbol: ITemplateSymbol;
  LContainerTemplate: ITemplate;
  LOptions: IPreserveValue<TParserOptions>;
  LStripAction: TStripAction;
  LEndStripAction: TStripAction;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;
  Match(vsIgnoreNL);
  LStripAction := Match(vsEndScript);
  PushContainer;
  LContainerTemplate := CurrentContainer;
  AddEndStripStmt(LContainerTemplate, LStripAction);

  RuleStmts(LContainerTemplate, [vsEND]);

  Match(vsEND);
  LEndStripAction := Match(vsEndScript);
  PopContainer;

  result := TProcessTemplateStmt.Create(LSymbol.Position, LContainerTemplate, false);
  result := AddStripStmt(result, LEndStripAction, sdRight);
end;

function TTemplateParser.RuleIncludeStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LIncludeExpr: IExpr;
  LScopeExpr: IExpr;
  LContainerTemplate: TTemplate;
  LValueSeparator: TTemplateSymbol;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  Match(vsInclude);
  Match(vsOpenRoundBracket);

  LIncludeExpr := RuleExpression;

  LValueSeparator := GetValueSeparatorSymbol;
  if FLookahead.Token = LValueSeparator then
  begin
    Match(LValueSeparator);
    LScopeExpr := RuleExpression;
  end;

  MatchClosingBracket(vsCloseRoundBracket);
  LStripAction := Match(vsEndScript);

  if LScopeExpr <> nil then
  begin
    LContainerTemplate := TTemplate.Create();
    LContainerTemplate.Add(TIncludeStmt.Create(LSymbol.Position, LIncludeExpr));
    result := TWithStmt.Create(LSymbol.Position, LScopeExpr, LContainerTemplate);
  end
  else
  begin
    result := TIncludeStmt.Create(LSymbol.Position, LIncludeExpr);
  end;
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleRequireStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  Match(vsRequire);
  Match(vsOpenRoundBracket);
  result := TRequireStmt.Create(LSymbol.Position, self.RuleExprList());
  MatchClosingBracket(vsCloseRoundBracket);
  LStripAction := Match(vsEndScript);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleMethodExpr(AExpr: IExpr; AMethodExpr: IExpr): IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  Match(vsOpenRoundBracket);
  result := TMethodCallExpr.Create(LSymbol.Position, AExpr, AsVarString(AMethodExpr), RuleExprList);
  MatchClosingBracket(vsCloseRoundBracket);
end;

function TTemplateParser.RuleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet): TTemplateSymbol;
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
    LText := MatchValue(vsText);
    if LText = '' then
      exit(nil);
    exit(RulePrintStmtVariable(TValueExpr.Create(LSymbol.Position, LText)));
  end;

  procedure SkipStmt;
  begin
    MatchValue(vsText)
  end;

begin
  result := vsInvalid;
  LEndToken := AEndToken + [vsEOF];
  LParentContainer := Container as ITemplateAdd;
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
          LStmt := RuleStmt;
          if LStmt = nil then
            LLoop := false;
        end;
    end;
    if (LStmt <> nil) and not supports(LStmt, IElseStmt) then
    begin
      LParentContainer.Add(LStmt);
    end;
  end;
end;

function TTemplateParser.RuleTemplateStmt: IStmt;
var
  LExpr: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
  LStripAction: TStripAction;
  LContainerStripAction: TStripAction;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);
  LSymbol := FLookahead;

  Match(vsTemplate);
  LExpr := RuleExpression;
  LContainerStripAction := Match(vsEndScript);
  PushContainer;
  LContainer := CurrentContainer;
  AddEndStripStmt(LContainer, LContainerStripAction);
  RuleStmts(CurrentContainer, [vsEND]);

  Match(vsEND);
  LStripAction := Match(vsEndScript);
  PopContainer;

  result := TDefineTemplateStmt.Create(LSymbol.Position, LExpr, LContainer);
  result := AddStripStmt(result, LStripAction, sdRight);
  LContainer.Optimise;
end;

function TTemplateParser.RuleSignedFactor: IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;

  if LSymbol.Token in [vsMinus, vsPLUS] then
    Match(LSymbol.Token);

  result := RuleFactor;

  if LSymbol.Token = vsMinus then
  begin
    result := RuleFactor;
    if (eoEvalEarly in FContext.Options) and IsValue(result) then
      exit(TValueExpr.Create(LSymbol.Position, -asnum(AsValue(result), FContext)))
    else
      exit(TUnaryExpr.Create(LSymbol.Position, uoMinus, result))
  end;
end;

function TTemplateParser.RuleSimpleExpression: IExpr;
var
  LRightExpr: IExpr;
  LBinOp: TBinOp;
  LSymbol: ITemplateSymbol;
begin
  result := RuleTerm();
  LSymbol := FLookahead;
  if LSymbol.Token in [vsPLUS, vsMinus, vsOR] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    Match(LSymbol.Token);
    LRightExpr := RuleSimpleExpression;
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

function TTemplateParser.RuleTerm: IExpr;
var
  LRightExpr: IExpr;
  LBinOp: TBinOp;
  LSymbol: ITemplateSymbol;
begin
  result := RuleSignedFactor;
  LSymbol := FLookahead;
  if LSymbol.Token in [vsMULT, vsDIV, vsSLASH, vsMOD, vsAND] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    Match(LSymbol.Token);
    LRightExpr := RuleTerm;

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

function TTemplateParser.RuleStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStripAction: TStripAction;
begin
  result := nil;
  LSymbol := FLookahead;
  LStripAction := Match(vsStartScript);
  case FLookahead.Token of
    vsEndScript:
      result := RuleNoopStmt;
    vsBreak:
      result := RuleBreakStmt;
    vsContinue:
      result := RuleContinueStmt;
    vsIgnoreNL:
      result := RuleIgnoreNewline;
    vsComment:
      result := RuleCommentStmt;
    vsInclude:
      result := RuleIncludeStmt;
    vsEND:
      result := RuleEndStmt;
    vsElse, vsELIF, vsOnBegin, vsOnEnd, vsOnEmpty, vsBetweenItem:
      result := nil;
    vsIF:
      result := RuleIfStmt;
    vsFor:
      result := RuleForStmt;
    vsCycle:
      result := RuleCycleStmt;
    vsPrint:
      result := RulePrintStmt;
    vsWhile:
      result := RuleWhileStmt;
    vswith:
      result := RuleWithStmt;
    vsRequire:
      result := RuleRequireStmt;
    vsTemplate:
      result := RuleTemplateStmt;
    vsID:
      result := RuleIdStmt;
    vsBlock:
      result := RuleBlockStmt;
    vsExtends:
      result := RuleExtendsStmt;
  else
    result := RuleExprStmt;
  end;
  if (eoEmbedException in FContext.Options) and (result <> nil) then
  begin
    result := TDebugStmt.Create(result);
  end;
  result := AddStripStmt(result, LStripAction, sdLeft);
end;

function TTemplateParser.RuleVariable: IExpr;
var
  LId: string;
  LIdVal: TValue;
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
  LDone: boolean;
begin
  LDone := false;
  LSymbol := FLookahead;
  LId := MatchValue(vsID);
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
          result := self.RuleFunctionExpr(LId);
        end;
      vsOpenSquareBracket:
        begin
          Match(vsOpenSquareBracket);
          LExpr := self.RuleExpression;
          if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(LExpr) then
            result := TValueExpr.Create(LSymbol.Position, deref(LSymbol.Position, AsValue(result), AsValue(LExpr), eoRaiseErrorWhenVariableNotFound in FContext.Options, FContext))
          else
            result := TVariableDerefExpr.Create(LSymbol.Position, dtArray, result, LExpr);
          Match(vsCloseSquareBracket);
        end;
      vsDOT:
        begin
          Match(vsDOT);
          LExpr := TVariableExpr.Create(LSymbol.Position, MatchValue(vsID));
          if FLookahead.Token = vsOpenRoundBracket then
            result := RuleMethodExpr(result, LExpr)
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

function TTemplateParser.RuleAssignStmt(ASymbol: IExpr): IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  Match(vsCOLONEQ);
  exit(TAssignStmt.Create(LSymbol.Position, AsVarString(ASymbol), RuleExpression));
end;

function TTemplateParser.RuleBlockStmt: IStmt;
var
  LName: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
  LStripAction: TStripAction;
  LContainerStripAction: TStripAction;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;

  Match(vsBlock);
  LName := RuleExpression;
  LContainerStripAction := Match(vsEndScript);

  PushContainer;
  LContainer := CurrentContainer;
  AddEndStripStmt(LContainer, LContainerStripAction);
  RuleStmts(LContainer, [vsEND]);

  Match(vsEND);
  LStripAction := Match(vsEndScript);

  PopContainer;
  result := TBlockStmt.Create(LSymbol.Position, LName, LContainer);
  result := AddStripStmt(result, LStripAction, sdRight);
  LContainer.Optimise;
end;

function TTemplateParser.RuleNoopStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  LStripAction := Match(vsEndScript);
  result := TNoopStmt.Create(LSymbol.Position);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleBreakStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  Match(vsBreak);
  LStripAction := Match(vsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(LSymbol.Position, SContinueShouldBeInALoop);
  result := TBreakStmt.Create(LSymbol.Position);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleCommentStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStartStripAction: TStripAction;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  LStartStripAction := Match(vsComment);
  LStripAction := Match(vsEndScript);
  result := TCommentStmt.Create(LSymbol.Position);
  result := AddStripStmt(result, LStartStripAction, sdLeft);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleContinueStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  Match(vsContinue);
  LStripAction := Match(vsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(LSymbol.Position, SContinueShouldBeInALoop);
  result := TContinueStmt.Create(LSymbol.Position);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleCycleStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LListExpr: IExprList;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  Match(vsCycle);
  Match(vsOpenRoundBracket);

  LListExpr := RuleExprList();

  MatchClosingBracket(vsCloseRoundBracket);
  LStripAction := Match(vsEndScript);

  result := TCycleStmt.Create(LSymbol.Position, LListExpr);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleElIfStmt: IStmt;
var
  LConditionExpr: IExpr;
  LTrueContainer: ITemplate;
  LFalseContainer: ITemplate;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
  LTrueStripAction: TStripAction;
  LFalseStripAction: TStripAction;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  if not(poAllowElIf in FOptions) then
    RaiseError(LSymbol.Position, SElIfExpected);

  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowElse, poHasElse, poAllowEnd]);

  LStripAction := Match(vsELIF);

  LConditionExpr := RuleExpression;

  LTrueStripAction := Match(vsEndScript);
  // create new container for true condition
  PushContainer;
  LTrueContainer := self.CurrentContainer;
  AddEndStripStmt(LTrueContainer, LTrueStripAction);
  RuleStmts(LTrueContainer, IF_ELIF_END);
  PopContainer;

  if FLookahead.Token = vsElse then
  begin
    Match(vsElse);
    LFalseStripAction := Match(vsEndScript);

    PushContainer;
    LFalseContainer := self.CurrentContainer;
    AddEndStripStmt(LFalseContainer, LFalseStripAction);
    RuleStmts(LFalseContainer, [vsEND, vsELIF]);

    PopContainer;
  end;

  if (eoEvalEarly in FContext.Options) and IsValue(LConditionExpr) then
  begin
    if AsBoolean(AsValue(LConditionExpr)) then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LTrueContainer))
    else if LFalseContainer <> nil then
      exit(TProcessTemplateStmt.Create(LSymbol.Position, LFalseContainer))
  end;
  result := TIfStmt.Create(LSymbol.Position, LConditionExpr, LTrueContainer, LFalseContainer);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleEndStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  // NOTE: we do not Match anything as we want lookahead functions to continue to work
  if not(poAllowEnd in FOptions) then
    RaiseError(LSymbol.Position, SEndNotExpected);
  exit(TEndStmt.Create(LSymbol.Position));
end;

function TTemplateParser.RuleExpression: IExpr;
var
  LSymbol: ITemplateSymbol;
  LRight: IExpr;
  LTrueExpr: IExpr;
  LFalseExpr: IExpr;
  LBinOp: TBinOp;
begin
  result := RuleSimpleExpression();
  LSymbol := FLookahead;
  if LSymbol.Token in [vsEQ, vsNotEQ, vsLT, vsLTE, vsGT, vsGTE, vsIn] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    Match(LSymbol);
    LRight := RuleExpression;
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
    Match(vsQUESTION);
    LTrueExpr := RuleExpression();
    Match(vsColon);
    LFalseExpr := RuleExpression();

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

function TTemplateParser.RuleFactor: IExpr;
var
  LSymbol: ITemplateSymbol;

begin
  LSymbol := FLookahead;
  case LSymbol.Token of
    vsOpenSquareBracket:
      begin
        Match(vsOpenSquareBracket);
        result := TArrayExpr.Create(LSymbol.Position, RuleExprList(vsCloseSquareBracket));
        MatchClosingBracket(vsCloseSquareBracket);
        exit;
      end;
    vsOpenRoundBracket:
      begin
        Match(vsOpenRoundBracket);
        result := RuleExpression;
        MatchClosingBracket(vsCloseRoundBracket);
        exit;
      end;
    vsString:
      exit(TValueExpr.Create(LSymbol.Position, MatchValue(vsString)));
    vsNumber:
      exit(TValueExpr.Create(LSymbol.Position, MatchNumber(vsNumber)));
    vsBoolean:
      exit(TValueExpr.Create(LSymbol.Position, MatchValue(vsBoolean) = 'true'));
    vsID:
      exit(self.RuleVariable());
    vsNOT:
      begin
        Match(vsNOT);
        result := RuleExpression;
        if (eoEvalEarly in FContext.Options) and IsValue(result) then
          exit(TValueExpr.Create(LSymbol.Position, not AsBoolean(AsValue(result))))
        else
          exit(TUnaryExpr.Create(LSymbol.Position, uoNot, result));
      end;
  end;
end;

const
  ONFIRST_ONEND_ONLOOP_ELSE: TTemplateSymbolSet = [vsOnBegin, vsOnEnd, vsOnEmpty, vsBetweenItem, vsEND];

function TTemplateParser.RuleForStmt: IStmt;
var
  LId: string;
  LRangeExpr: IExpr;
  LLowValueExpr, LHighValueExpr: IExpr;
  LOffsetExpr, LLimitExpr, LStep: IExpr;
  LForOp: TForOp;
  LOptions: IPreserveValue<TParserOptions>;
  LContainerTemplate: ITemplate;
  LSymbol: ITemplateSymbol;
  LOnBegin, LOnEnd, LOnLoop, LOnEmpty, LBetweenItem: ITemplate;
  LPrevSymbol, LBlockSymbol: TTemplateSymbol;
  i: integer;
  LContainerStripAction: TStripAction;
  LEndStripAction: TStripAction;

  procedure ResolveTemplate(const ASymbol: TTemplateSymbol);
  begin
    case ASymbol of
      vsInvalid:
        begin
          LOnLoop := LContainerTemplate;
        end;
      vsOnBegin:
        begin
          LOnBegin := LContainerTemplate;
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
  Match(vsFor);
  LId := MatchValue(vsID);
  if FLookahead.Token in [vsIn, vsOf] then
  begin
    LForOp := TemplateForop(LSymbol.Position, FLookahead.Token);
    Match(FLookahead.Token);
    LRangeExpr := RuleExpression;
  end
  else
  begin
    Match(vsCOLONEQ);
    LLowValueExpr := RuleExpression();
    LForOp := TemplateForop(LSymbol.Position, FLookahead.Token);
    if FLookahead.Token in [vsDownto, vsTo] then
      Match(FLookahead.Token)
    else
      RaiseError(LSymbol.Position, SUnexpectedToken);
    LHighValueExpr := RuleExpression();
  end;

  while FLookahead.Token in [vsOffset, vsLimit, vsStep] do
  begin
    case FLookahead.Token of
      vsOffset:
        begin
          Match(vsOffset);
          LOffsetExpr := RuleExpression();
        end;

      vsLimit:
        begin
          Match(vsLimit);
          LLimitExpr := RuleExpression();
        end;

      vsStep:
        begin
          Match(vsStep);
          LStep := RuleExpression();
        end;
    end;
  end;

  LContainerStripAction := Match(vsEndScript);
  LBlockSymbol := vsInvalid;
  LPrevSymbol := vsInvalid;
  i := 0;
  repeat
    if (i > 1) and (i mod 2 = 0) then
    begin
      Match(LBlockSymbol);
      LContainerStripAction := Match(vsEndScript);
    end;
    PushContainer;
    LContainerTemplate := CurrentContainer;
    AddEndStripStmt(LContainerTemplate, LContainerStripAction);
    LBlockSymbol := RuleStmts(LContainerTemplate, ONFIRST_ONEND_ONLOOP_ELSE);
    PopContainer;
    if (i mod 2 = 0) then
    begin
      ResolveTemplate(LPrevSymbol);
    end;
    LPrevSymbol := LBlockSymbol;
    inc(i);
  until LBlockSymbol = vsEND;
  Match(vsEND);
  if LOnLoop = nil then
  begin
    LOnLoop := LContainerTemplate;
  end;

  LEndStripAction := Match(vsEndScript);

  if LForOp in [TForOp.foIn, TForOp.foOf] then
    result := TForInStmt.Create(LSymbol.Position, LId, LForOp, LRangeExpr, LOffsetExpr, LLimitExpr, LOnLoop, LOnBegin, LOnEnd, LOnEmpty, LBetweenItem)
  else
    result := TForRangeStmt.Create(LSymbol.Position, LId, LForOp, LLowValueExpr, LHighValueExpr, LStep, LOnLoop, LOnBegin, LOnEnd, LOnEmpty, LBetweenItem);
  result := AddStripStmt(result, LEndStripAction, sdRight);
  Optimise([LOnLoop, LOnBegin, LOnEnd, LOnEmpty, LBetweenItem]);
end;

function TTemplateParser.RuleFunctionExpr(const ASymbol: string): IExpr;
var
  LFunctions: TArray<TRttiMethod>;
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  if not FContext.TryGetFunction(ASymbol, LFunctions) then
    RaiseError(LSymbol.Position, SFunctionNotRegisteredInContext, [ASymbol]);
  Match(vsOpenRoundBracket);
  result := TFunctionCallExpr.Create(LSymbol.Position, LFunctions, RuleExprList);
  MatchClosingBracket(vsCloseRoundBracket);
end;

function TTemplateParser.RuleIdStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  LExpr := RuleVariable;
  if FLookahead.Token = vsCOLONEQ then
  begin
    result := RuleAssignStmt(LExpr);
  end
  else
  begin
    LExpr := TEncodeExpr.Create(LSymbol.Position, LExpr);
    result := RulePrintStmtVariable(LExpr);
  end;
  LStripAction := Match(vsEndScript);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleWhileStmt: IStmt;
var
  LCondition: IExpr;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
  LOffsetExpr, LLimitExpr: IExpr;
  LContainerTemplate: ITemplate;
  LOnBegin, LOnEnd, LOnLoop, LOnEmpty, LBetweenItem: ITemplate;
  LPrevSymbol, LBlockSymbol: TTemplateSymbol;
  i: integer;
  LContainerStripAction: TStripAction;
  LEndStripAction: TStripAction;
  procedure ResolveTemplate(const ASymbol: TTemplateSymbol);
  begin
    case ASymbol of
      vsInvalid:
        begin
          LOnLoop := LContainerTemplate;
        end;
      vsOnBegin:
        begin
          LOnBegin := LContainerTemplate;
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

  Match(vsWhile);
  LCondition := RuleExpression;

  while FLookahead.Token in [vsOffset, vsLimit] do
  begin
    case FLookahead.Token of
      vsOffset:
        begin
          Match(vsOffset);
          LOffsetExpr := RuleExpression();
        end;
      vsLimit:
        begin
          Match(vsLimit);
          LLimitExpr := RuleExpression();
        end;
    end;
  end;

  LContainerStripAction := Match(vsEndScript);
  LBlockSymbol := vsInvalid;
  LPrevSymbol := vsInvalid;
  i := 0;
  repeat
    if (i > 1) and (i mod 2 = 0) then
    begin
      Match(LBlockSymbol);
      LContainerStripAction := Match(vsEndScript);
    end;
    PushContainer;
    LContainerTemplate := CurrentContainer;
    AddEndStripStmt(LContainerTemplate, LContainerStripAction);
    LBlockSymbol := RuleStmts(LContainerTemplate, ONFIRST_ONEND_ONLOOP_ELSE);
    PopContainer;
    if (i mod 2 = 0) then
    begin
      ResolveTemplate(LPrevSymbol);
    end;
    LPrevSymbol := LBlockSymbol;
    inc(i);
  until LBlockSymbol = vsEND;
  Match(vsEND);
  if LOnLoop = nil then
  begin
    LOnLoop := LContainerTemplate;
  end;
  LEndStripAction := Match(vsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(LCondition) and not AsBoolean(AsValue(LCondition)) then
    result := nil
  else
    result := TWhileStmt.Create(LSymbol.Position, LCondition, LOffsetExpr, LLimitExpr, LOnLoop, LOnBegin, LOnEnd, LOnEmpty, LBetweenItem);
  result := AddStripStmt(result, LEndStripAction, sdRight);
  Optimise([LOnLoop, LOnBegin, LOnEnd, LOnEmpty, LBetweenItem]);
end;

function TTemplateParser.RuleWithStmt: IStmt;
var
  LExpr: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
  LContainerStripAction: TStripAction;
  LEndStripAction: TStripAction;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;

  Match(vswith);
  LExpr := RuleExpression;
  LContainerStripAction := Match(vsEndScript);

  PushContainer;
  LContainer := CurrentContainer;
  AddEndStripStmt(LContainer, LContainerStripAction);
  RuleStmts(LContainer, [vsEND]);

  Match(vsEND);
  LEndStripAction := Match(vsEndScript);

  PopContainer;
  result := TWithStmt.Create(LSymbol.Position, LExpr, LContainer);
  result := AddStripStmt(result, LEndStripAction, sdRight);
  LContainer.Optimise;
end;

function TTemplateParser.RulePrintStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  Match(vsPrint);
  Match(vsOpenRoundBracket);
  LExpr := RuleExpression;
  MatchClosingBracket(vsCloseRoundBracket);
  LStripAction := Match(vsEndScript);
  result := TPrintStmt.Create(LSymbol.Position, LExpr);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RulePrintStmtVariable(AExpr: IExpr): IStmt;
var
  LSymbol: ITemplateSymbol;
  LValueExpr: IValueExpr;
begin
  LSymbol := FLookahead;
  if supports(AExpr, IValueExpr, LValueExpr) and (asString(LValueExpr.Value, FContext) = '') then
    exit(nil);
  exit(TPrintStmt.Create(LSymbol.Position, AExpr));
end;

function TTemplateParser.RuleExprList(const AEndToken: TTemplateSymbol): IExprList;
var
  LSymbol: ITemplateSymbol;
  LValueSeparator: TTemplateSymbol;
begin
  LSymbol := FLookahead;
  result := TExprList.Create(LSymbol.Position);
  if FLookahead.Token <> AEndToken then
    result.AddExpr(RuleExpression);
  LValueSeparator := GetValueSeparatorSymbol;
  while FLookahead.Token = LValueSeparator do
  begin
    Match(LValueSeparator);
    result.AddExpr(RuleExpression);
  end;
end;

function TTemplateParser.RuleExprStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LStripAction: TStripAction;
begin
  LSymbol := FLookahead;
  result := RulePrintStmtVariable(TEncodeExpr.Create(LSymbol.Position, RuleExpression));
  LStripAction := Match(vsEndScript);
  result := AddStripStmt(result, LStripAction, sdRight);
end;

function TTemplateParser.RuleExtendsStmt: IStmt;
var
  LName: IExpr;
  LScopeExpr: IExpr;
  LSymbol: ITemplateSymbol;
  LOptions: IPreserveValue<TParserOptions>;
  LContainer: ITemplate;
  LContainerTemplate: TTemplate;
  LEndStripAction: TStripAction;
begin
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poAllowEnd]);

  LSymbol := FLookahead;

  Match(vsExtends);
  Match(vsOpenRoundBracket);
  LName := RuleExpression;

  if FLookahead.Token = vsComma then
  begin
    Match(vsComma);
    LScopeExpr := RuleExpression;
  end;

  Match(vsCloseRoundBracket);

  Match(vsEndScript); // we don't care about the strip action on this as content is ignored inside an extends block

  PushContainer;
  LContainer := CurrentContainer;

  RuleStmts(LContainer, [vsEND]);

  Match(vsEND);
  LEndStripAction := Match(vsEndScript);

  PopContainer;

  if LScopeExpr <> nil then
  begin
    LContainerTemplate := TTemplate.Create();
    LContainerTemplate.Add(TExtendsStmt.Create(LSymbol.Position, LName, LContainer));
    result := TWithStmt.Create(LSymbol.Position, LScopeExpr, LContainerTemplate);
  end
  else
  begin
    result := TExtendsStmt.Create(LSymbol.Position, LName, LContainer);
  end;
  result := AddStripStmt(result, LEndStripAction, sdRight);
  LContainer.Optimise;
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
  val := FLookahead as ITemplateValueSymbol;
  exit(val.Value)
end;

function TTemplateParser.Match(const ASymbol: TTemplateSymbol): TStripAction;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  result := TStripAction.saNone;
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
    case ASymbol of
      vsStartScript, vsEndScript:
        result := LSymbol.StripAction;
    end;
    FLookahead := FLexer.GetToken;
    exit;
  end;
  RaiseError(LSymbol.Position, format(SParsingErrorExpecting, [TemplateSymbolToString(ASymbol)]));
end;

procedure TTemplateParser.Match(ASymbol: ITemplateSymbol);
begin
  Match(ASymbol.Token);
end;

function TTemplateParser.MatchNumber(const ASymbol: TTemplateSymbol): extended;
begin
  exit(StrToFloat(MatchValue(ASymbol), FContext.FormatSettings));
end;

function TTemplateParser.MatchValue(const ASymbol: TTemplateSymbol): string;
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
  result := CurrentContainer;
  if AStream.Size > 0 then
  begin
    RuleStmts(result, []);
  end;
  Match(vsEOF);
  result.Optimise;
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

{ TForInStmt }

procedure TForInStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TForInStmt.Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const AExpr: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer, AOnBegin, AOnEnd, AOnEmpty, ABetweenItem);
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

constructor TForRangeStmt.Create(const APosition: IPosition; const AVariable: string; const AForOp: TForOp; const ALowExpr: IExpr; const AHighExpr: IExpr; const AStep: IExpr; const AContainer: ITemplate; const AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer, AOnBegin, AOnEnd, AOnEmpty, ABetweenItem);
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

procedure TTemplate.Add(const AItem: IStmt);
var
  LOffset: integer;
begin
  LOffset := length(FArray);
  setlength(FArray, LOffset + 1);
  FArray[LOffset] := AItem;
end;

constructor TTemplate.Create;
begin
  FPosition := TPosition.Create('', 1, 1);
end;

function TTemplate.Flatten: TArray<IStmt>;
begin
  result := Sempare.Template.Parser.Flatten(FArray);
end;

function TTemplate.GetCount: integer;
begin
  exit(length(FArray));
end;

function TTemplate.GetFilename: string;
begin
  exit(FPosition.FileName);
end;

function TTemplate.GetLastItem: IStmt;
begin
  if GetCount = 0 then
    exit(nil)
  else
    exit(GetItem(GetCount - 1));
end;

function TTemplate.GetLine: integer;
begin
  exit(FPosition.line);
end;

function TTemplate.GetPos: integer;
begin
  exit(FPosition.pos);
end;

procedure TTemplate.Optimise;

  function Strip(const AArray: TArray<IStmt>): TArray<IStmt>;
  var
    LStmt: IStmt;
  begin
    result := nil;
    for LStmt in AArray do
    begin
      if supports(LStmt, IEndStmt) or supports(LStmt, ICommentStmt) or supports(LStmt, IElseStmt) or supports(LStmt, INoopStmt) then
      begin
        continue;
      end;
      result := result + [LStmt];
    end;
  end;

begin
  FArray := Strip(Flatten);
end;

procedure TTemplate.SetFilename(const AFilename: string);
begin
  FPosition.FileName := AFilename;
end;

procedure TTemplate.SetLine(const Aline: integer);
begin
  FPosition.line := Aline;
end;

procedure TTemplate.SetPos(const Apos: integer);
begin
  FPosition.pos := Apos;
end;

function TTemplate.GetItem(const AOffset: integer): IStmt;
begin
  exit(FArray[AOffset]);
end;

{ TWhileStmt }

procedure TWhileStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TWhileStmt.Create(const APosition: IPosition; const ACondition: IExpr; const AOffsetExpr: IExpr; const ALimitExpr: IExpr; const AContainer: ITemplate; const AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer, AOnBegin, AOnEnd, AOnEmpty, ABetweenItem);
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

{ TBreakStmt }

procedure TBreakStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TEndStmt }

procedure TEndStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
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

{ TElseStmt }

procedure TElseStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

{ TElIfStmt }

procedure TElIfStmt.Accept(const AVisitor: ITemplateVisitor);
begin
end;

{ TCommentStmt }

procedure TCommentStmt.Accept(const AVisitor: ITemplateVisitor);
begin
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

function TAbstractBase.GetFilename: string;
begin
  exit(FPosition.FileName);
end;

function TAbstractBase.GetLine: integer;
begin
  exit(FPosition.line);
end;

function TAbstractBase.GetPos: integer;
begin
  exit(FPosition.pos);
end;

function TAbstractBase.GetPosition: IPosition;
begin
  exit(FPosition);
end;

procedure TAbstractBase.SetFilename(const AFilename: string);
begin
  FPosition.FileName := AFilename;
end;

procedure TAbstractBase.SetLine(const Aline: integer);
begin
  FPosition.line := Aline;
end;

procedure TAbstractBase.SetPos(const Apos: integer);
begin
  FPosition.pos := Apos;
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

constructor TDebugStmt.Create(const AStmt: IStmt);
begin
  FStmt := AStmt;
end;

function TDebugStmt.GetStmt: IStmt;
begin
  exit(FStmt);
end;

{ TLoopStmt }

constructor TLoopStmt.Create(APosition: IPosition; AContainer, AOnBegin, AOnEnd, AOnEmpty, ABetweenItem: ITemplate);
begin
  inherited Create(APosition, AContainer);
  FOnBegin := AOnBegin;
  FOnEnd := AOnEnd;
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
  exit(FOnEnd);
end;

function TLoopStmt.GetOnBeginContainer: ITemplate;
begin
  exit(FOnBegin);
end;

{ TCompositeStmt }

procedure TCompositeStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TCompositeStmt.Create(const AFirstStmt, ASecondStmt: IStmt);
begin
  FFirstStmt := AFirstStmt;
  FSecondStmt := ASecondStmt;
end;

function TCompositeStmt.Flatten: TArray<IStmt>;
begin
  result := Sempare.Template.Parser.Flatten([FFirstStmt, FSecondStmt]);
end;

function TCompositeStmt.GetFirstStmt: IStmt;
begin
  exit(FFirstStmt);
end;

function TCompositeStmt.GetSecondStmt: IStmt;
begin
  exit(FSecondStmt);
end;

{ TStripStmt }

procedure TStripStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

constructor TStripStmt.Create(const ADirection: TStripDirection; const AAction: TStripAction);
begin
  FDirection := ADirection;
  FAction := AAction;
end;

function TStripStmt.GetAction: TStripAction;
begin
  exit(FAction);
end;

function TStripStmt.GetDirection: TStripDirection;
begin
  exit(FDirection);
end;

{ TBlockStmt }

procedure TBlockStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
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

{ TExtendsStmt }

procedure TExtendsStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
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

function TExtendsStmt.GetName: IExpr;
begin
  exit(FName);
end;

function TExtendsStmt.NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
begin
  exit(AEvalVisitor.EvalExprAsString(FName));
end;

{ TAbstractStmt }

constructor TAbstractStmt.Create(const APosition: IPosition);
begin
  inherited Create(APosition);
end;

function TAbstractStmt.Flatten: TArray<IStmt>;
begin
  result := [self];
end;

{ TNoopStmt }

procedure TNoopStmt.Accept(const AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

initialization

initOps;

end.
