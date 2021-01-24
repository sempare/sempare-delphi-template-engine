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

function TemplateForop(const ASymbol: TTemplateSymbol): TForOp;

begin
  case ASymbol of
    vsTo:
      exit(foTo);
    vsDownto:
      exit(foDownto);
    vsin:
      exit(foIn);
  else
    raise ETemplateParser.Createfmt(SForOpNotSupported, [TemplateSymbolToString(ASymbol)]);
  end;
end;

// const
// pInvalid: byte = 255;

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
  IF_ELIF_END: TTemplateSymbolSet = [VsELIF, vsElse, vsEND];
  // IF_END: TTemplateSymbolSet = [vsElse, vsEND];

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
  match(VsIF);
  LConditionalExpr := ruleExpression;

  match(VsEndScript);
  // create new container for true condition
  PushContainer;
  LTrueContainer := self.CurrentContainer;

  ruleStmts(LTrueContainer, IF_ELIF_END);
  PopContainer;

  PushContainer;
  LFalseContainer := self.CurrentContainer;
  LFalseContainer.QueryInterface(ITemplateAdd, LContainerAdd);

  if FLookahead.Token = VsELIF then
  begin
    while (FLookahead.Token = VsELIF) do
    begin
      LContainerAdd.Add(AsVisitorHost(ruleElIfStmt()));
    end;
  end
  else if FLookahead.Token = vsElse then
  begin
    match(vsElse);
    match(VsEndScript);
    ruleStmts(LFalseContainer, [vsEND]);
  end;
  PopContainer;
  match(vsEND);
  match(VsEndScript);

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
  match(VsEndScript);
  PushContainer;
  LContainerTemplate := CurrentContainer;

  ruleStmts(LContainerTemplate, [vsEND]);

  match(vsEND);
  match(VsEndScript);
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
  match(VsOpenRoundBracket);
  LIncludeExpr := ruleExpression;

  if FLookahead.Token = vsComma then
  begin
    match(vsComma);
    LScopeExpr := ruleExpression;
  end;

  match(VsCloseRoundBracket);
  match(VsEndScript);

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
  match(VsOpenRoundBracket);
  result := TRequireStmt.Create(LSymbol.Position, self.ruleExprList());
  match(VsCloseRoundBracket);
  match(VsEndScript);
end;

function TTemplateParser.ruleMethodExpr(AExpr: IExpr; AMethodExpr: IExpr): IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(VsOpenRoundBracket);
  result := TMethodCallExpr.Create(LSymbol.Position, AExpr, AsVarString(AMethodExpr), ruleExprList);
  match(VsCloseRoundBracket);
end;

procedure TTemplateParser.ruleStmts(Container: ITemplate; const AEndToken: TTemplateSymbolSet);

var
  LStmt: IStmt;
  LParentContainer: ITemplateAdd;
  LSymbol: ITemplateSymbol;
  LLoop: boolean;

  function AddPrintStmt: IStmt;
  var
    LText: string;
  begin
    LText := matchValue(VsText);
    if LText = '' then
      exit(nil);
    exit(rulePrintStmtVariable(TValueExpr.Create(LSymbol.Position, LText)));
  end;

  procedure SkipStmt;
  begin
    matchValue(VsText)
  end;

begin
  Container.QueryInterface(ITemplateAdd, LParentContainer);
  LLoop := true;
  while LLoop do
  begin
    LSymbol := FLookahead;
    if (LSymbol.Token = VsEOF) or (LSymbol.Token in AEndToken) then
      break;
    LStmt := nil;
    case LSymbol.Token of
      VsText:
        begin
          if poStripWS in FOptions then
            SkipStmt
          else
            LStmt := AddPrintStmt;
        end;
      VsStartScript:
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

  match(vstemplate);
  LExpr := ruleExpression;
  match(VsEndScript);
  PushContainer;
  LContainer := CurrentContainer;

  ruleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);
  PopContainer;

  exit(TDefineTemplateStmt.Create(LSymbol.Position, LExpr, LContainer));
end;

function TTemplateParser.ruleSignedFactor: IExpr;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;

  if LSymbol.Token in [VsMinus, VsPLUS] then
    match(LSymbol.Token);

  result := ruleFactor;

  if LSymbol.Token = VsMinus then
  begin
    result := ruleFactor;
    if (eoEvalEarly in FContext.Options) and IsValue(result) then
      exit(TValueExpr.Create(LSymbol.Position, -asnum(AsValue(result))))
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
  if LSymbol.Token in [VsPLUS, VsMinus, VsOR] then
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
              exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result)) + asnum(AsValue(LRightExpr))))
            else if isStrLike(AsValue(result)) and isStrLike(AsValue(LRightExpr)) then
              exit(TValueExpr.Create(LSymbol.Position, asString(AsValue(result)) + asString(AsValue(LRightExpr))))
            else if isStrLike(AsValue(result)) and isNumLike(AsValue(LRightExpr)) then
              exit(TValueExpr.Create(LSymbol.Position, asString(AsValue(result)) + floattostr(asnum(AsValue(LRightExpr)))));
          end;
        boMinus:
          exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result)) - asnum(AsValue(LRightExpr))));
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
  if LSymbol.Token in [VsMULT, VsDIV, vsSLASH, VsMOD, VsAND] then
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
          exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result)) * asnum(AsValue(LRightExpr))));
        boDiv:
          exit(TValueExpr.Create(LSymbol.Position, trunc(asnum(AsValue(result))) div trunc(asnum(AsValue(LRightExpr)))));
        boSlash:
          exit(TValueExpr.Create(LSymbol.Position, asnum(AsValue(result)) / asnum(AsValue(LRightExpr))));
        boMod:
          exit(TValueExpr.Create(LSymbol.Position, AsInt(AsValue(result)) mod AsInt(AsValue(LRightExpr))));
      end;
    end;

    exit(TBinopExpr.Create(LSymbol.Position, result, LBinOp, LRightExpr));
  end;

end;

function TTemplateParser.ruleStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(VsStartScript);
  case FLookahead.Token of
    VsEndScript: // we don't do anything
      ;
    vsBreak:
      exit(ruleBreakStmt);
    vsContinue:
      exit(ruleContinueStmt);
    vsIgnoreNL:
      exit(ruleIgnoreNewline);
    vsComment:
      exit(ruleCommentStmt);
    vsInclude:
      exit(ruleIncludeStmt);
    vsEND:
      exit(ruleEndStmt);
    vsElse: // we don't do anything
      ;
    VsIF:
      exit(ruleIfStmt);
    VsELIF: // we don't do anything
      ;
    VsFor:
      exit(ruleForStmt);
    vsPrint:
      exit(rulePrintStmt);
    vsWhile:
      exit(ruleWhileStmt);
    vswith:
      exit(ruleWithStmt);
    vsRequire:
      exit(ruleRequireStmt);
    vstemplate:
      exit(ruleTemplateStmt);
    VsID:
      exit(ruleIdStmt);
  else
    exit(ruleExprStmt);
  end;
  exit(nil);
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
  LId := matchValue(VsID);
  if (eoEvalVarsEarly in FContext.Options) and FContext.TryGetVariable(LId, LIdVal) then
    result := TValueExpr.Create(LSymbol.Position, LIdVal)
  else
    result := TVariableExpr.Create(LSymbol.Position, LId);
  while not LDone do
  begin
    LSymbol := FLookahead;
    case LSymbol.Token of
      VsOpenRoundBracket:
        begin
          result := self.ruleFunctionExpr(LId);
        end;
      VsOpenSquareBracket:
        begin
          match(VsOpenSquareBracket);
          LExpr := self.ruleExpression;
          if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(LExpr) then
            result := TValueExpr.Create(LSymbol.Position, deref(LSymbol.Position, AsValue(result), AsValue(LExpr), eoRaiseErrorWhenVariableNotFound in FContext.Options))
          else
            result := TVariableDerefExpr.Create(LSymbol.Position, dtArray, result, LExpr);
          match(VsCloseSquareBracket);
        end;
      VsDOT:
        begin
          match(VsDOT);
          LExpr := TVariableExpr.Create(LSymbol.Position, matchValue(VsID));
          if FLookahead.Token = VsOpenRoundBracket then
            result := ruleMethodExpr(result, LExpr)
          else
          begin
            if (eoEvalVarsEarly in FContext.Options) and IsValue(result) and IsValue(LExpr) then
              result := TValueExpr.Create(LSymbol.Position, deref(LSymbol.Position, AsValue(result), AsValue(LExpr), eoRaiseErrorWhenVariableNotFound in FContext.Options))
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
  match(VsCOLONEQ);
  exit(TAssignStmt.Create(LSymbol.Position, (ASymbol as IVariableExpr).Variable, ruleExpression));
end;

function TTemplateParser.ruleBreakStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsBreak);
  match(VsEndScript);
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
  match(VsEndScript);
  exit(TCommentStmt.Create(LSymbol.Position));
end;

function TTemplateParser.ruleContinueStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  match(vsContinue);
  match(VsEndScript);
  if not(poInLoop in FOptions) then
    RaiseError(LSymbol.Position, SContinueShouldBeInALoop);
  exit(TContinueStmt.Create(LSymbol.Position));
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

  match(VsELIF);

  LConditionExpr := ruleExpression;

  match(VsEndScript);
  // create new container for true condition
  PushContainer;
  LTrueContainer := self.CurrentContainer;

  ruleStmts(LTrueContainer, IF_ELIF_END);
  PopContainer;

  if FLookahead.Token = vsElse then
  begin

    match(vsElse);
    match(VsEndScript);

    PushContainer;
    LFalseContainer := self.CurrentContainer;

    ruleStmts(LFalseContainer, [vsEND, VsELIF]);

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
  if LSymbol.Token in [VsEQ, VsNotEQ, VsLT, VsLTE, VsGT, VsGTE, vsin] then
  begin
    TemplateBinop(LSymbol.Token, LBinOp);
    match(LSymbol);
    LRight := ruleExpression;
    if (eoEvalEarly in FContext.Options) and IsValue(result) and IsValue(LRight) then
    begin
      case LBinOp of
        boEQ:
          exit(TValueExpr.Create(LSymbol.Position, isequal(AsValue(result), AsValue(LRight))));
        boNotEQ:
          exit(TValueExpr.Create(LSymbol.Position, not isequal(AsValue(result), AsValue(LRight))));
        boLT:
          exit(TValueExpr.Create(LSymbol.Position, isLessThan(AsValue(result), AsValue(LRight))));
        boGTE:
          exit(TValueExpr.Create(LSymbol.Position, not isLessThan(AsValue(result), AsValue(LRight))));
        boGT:
          exit(TValueExpr.Create(LSymbol.Position, isGreaterThan(AsValue(result), AsValue(LRight))));
        boLTE:
          exit(TValueExpr.Create(LSymbol.Position, not isGreaterThan(AsValue(result), AsValue(LRight))));
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
    VsOpenSquareBracket:
      begin
        match(VsOpenSquareBracket);
        result := TArrayExpr.Create(LSymbol.Position, ruleExprList(VsCloseSquareBracket));
        match(VsCloseSquareBracket);
        exit;
      end;
    VsOpenRoundBracket:
      begin
        match(VsOpenRoundBracket);
        result := ruleExpression;
        match(VsCloseRoundBracket);
        exit;
      end;
    vsString:
      exit(TValueExpr.Create(LSymbol.Position, matchValue(vsString)));
    vsNumber:
      exit(TValueExpr.Create(LSymbol.Position, matchNumber(vsNumber)));
    VsBoolean:
      exit(TValueExpr.Create(LSymbol.Position, matchValue(VsBoolean) = 'true'));
    VsID:
      exit(self.ruleVariable());
    VsNOT:
      begin
        match(VsNOT);
        result := ruleExpression;
        if (eoEvalEarly in FContext.Options) and IsValue(result) then
          exit(TValueExpr.Create(LSymbol.Position, not AsBoolean(AsValue(result))))
        else
          exit(TUnaryExpr.Create(LSymbol.Position, uoNot, result));
      end;
  end;
end;

function TTemplateParser.ruleForStmt: IStmt;
var
  LId: string;
  LRangeExpr: IExpr;
  LLowValueExpr, LHighValueExpr: IExpr;
  LForOp: TForOp;
  LOptions: IPreserveValue<TParserOptions>;
  LContainerTemplate: ITemplate;
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poInLoop, poAllowEnd]);
  match(VsFor);
  PushContainer;
  LContainerTemplate := CurrentContainer;
  LId := matchValue(VsID);
  if FLookahead.Token = vsin then
  begin
    LForOp := TemplateForop(FLookahead.Token);
    match(vsin);
    LRangeExpr := ruleExpression;
  end
  else
  begin
    match(VsCOLONEQ);
    LLowValueExpr := ruleExpression();
    LForOp := TemplateForop(FLookahead.Token);
    if FLookahead.Token in [vsDownto, vsTo] then
      match(FLookahead.Token)
    else
      RaiseError(LSymbol.Position, SUnexpectedToken);
    LHighValueExpr := ruleExpression();
  end;

  match(VsEndScript);

  ruleStmts(LContainerTemplate, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  if LForOp = TForOp.foIn then
    result := TForInStmt.Create(LSymbol.Position, LId, LRangeExpr, LContainerTemplate)
  else
    result := TForRangeStmt.Create(LSymbol.Position, LId, LForOp, LLowValueExpr, LHighValueExpr, LContainerTemplate);
  PopContainer;
end;

function TTemplateParser.ruleFunctionExpr(const ASymbol: string): IExpr;
var
  LFunctions: TArray<TRttiMethod>;
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  if not FContext.TryGetFunction(ASymbol, LFunctions) then
    RaiseError(LSymbol.Position, SFunctionNotRegisteredInContext, [ASymbol]);
  match(VsOpenRoundBracket);
  result := TFunctionCallExpr.Create(LSymbol.Position, LFunctions, ruleExprList);
  match(VsCloseRoundBracket);
end;

function TTemplateParser.ruleIdStmt: IStmt;
var
  LSymbol: ITemplateSymbol;
  LExpr: IExpr;
begin
  LSymbol := FLookahead;
  LExpr := ruleVariable;
  if FLookahead.Token = VsCOLONEQ then
  begin
    result := ruleAssignStmt(LExpr);
  end
  else
  begin
    LExpr := TEncodeExpr.Create(LSymbol.Position, LExpr);
    result := rulePrintStmtVariable(LExpr);
  end;
  match(VsEndScript);
end;

function TTemplateParser.ruleWhileStmt: IStmt;
var
  LCondition: IExpr;
  LOptions: IPreserveValue<TParserOptions>;
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  LOptions := Preserve.Value<TParserOptions>(FOptions, FOptions + [poInLoop, poAllowEnd]);
  PushContainer;

  match(vsWhile);
  LCondition := ruleExpression;
  match(VsEndScript);

  ruleStmts(CurrentContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);

  if (eoEvalEarly in FContext.Options) and IsValue(LCondition) and not AsBoolean(AsValue(LCondition)) then
    result := nil
  else
    result := TWhileStmt.Create(LSymbol.Position, LCondition, CurrentContainer);
  PopContainer;

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
  match(VsEndScript);

  PushContainer;
  LContainer := CurrentContainer;

  ruleStmts(LContainer, [vsEND]);

  match(vsEND);
  match(VsEndScript);

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
  match(VsOpenRoundBracket);
  LExpr := ruleExpression;
  match(VsCloseRoundBracket);
  match(VsEndScript);
  exit(TPrintStmt.Create(LSymbol.Position, LExpr));
end;

function TTemplateParser.rulePrintStmtVariable(AExpr: IExpr): IStmt;
var
  LSymbol: ITemplateSymbol;
  LValueExpr: IValueExpr;
begin
  LSymbol := FLookahead;
  if supports(AExpr, IValueExpr, LValueExpr) and (asString(LValueExpr.Value) = '') then
    exit(nil);
  exit(TPrintStmt.Create(LSymbol.Position, AExpr));
end;

function TTemplateParser.ruleExprList(const AEndToken: TTemplateSymbol): IExprList;
var
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  result := TExprList.Create(LSymbol.Position);
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
  LSymbol: ITemplateSymbol;
begin
  LSymbol := FLookahead;
  result := rulePrintStmtVariable(TEncodeExpr.Create(LSymbol.Position, ruleExpression));
  match(VsEndScript);
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
        VsStartScript:
          exclude(FOptions, poStripWS);
        VsEndScript:
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
  exit(strtofloat(matchValue(ASymbol)));
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
  exit(FValue);
end;

{ TExprList }

procedure TExprList.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

procedure TExprList.AddExpr(AExpr: IExpr);
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
  exit(FUnaryOp);
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
  exit(FVariable);
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
  exit(FFunctionInfo);
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
  exit(FExpr);
end;

function TForInStmt.GetVariable: string;
begin
  exit(FVariable);
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
  exit(FForIp);
end;

function TForRangeStmt.GetHighExpr: IExpr;
begin
  exit(FHighExpr);
end;

function TForRangeStmt.GetLowExpr: IExpr;
begin
  exit(FLowExpr);
end;

function TForRangeStmt.GetVariable: string;
begin
  exit(FVariable);
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
  exit(FVariable);
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
var
  LOffset: integer;
begin
  LOffset := length(FArray);
  setlength(FArray, LOffset + 1);
  FArray[LOffset] := AItem;
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
  exit(FCondition);
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
  exit(FDeref);
end;

function TVariableDerefExpr.GetDerefType: TDerefType;
begin
  exit(FDerefType);
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
  // AVisitor.Visit(self);   // this is on purpose
end;

{ TCommentStmt }

procedure TCommentStmt.Accept(AVisitor: ITemplateVisitor);
begin
  // AVisitor.Visit(self);     // this is on purpose
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
  exit(FPosition);
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
  exit(FAllowNewline);
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
  exit(FName);
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
  exit(FExpr);
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
  exit(FFalseExpr);
end;

function TTernaryExpr.GetTrueExpr: IExpr;
begin
  exit(FTrueExpr);
end;

{ TArrayExpr }

procedure TArrayExpr.Accept(AVisitor: ITemplateVisitor);
begin
  AVisitor.Visit(self);
end;

procedure initOps;
var
  LSymbol: TTemplateSymbol;
begin
  for LSymbol := low(TTemplateSymbol) to high(TTemplateSymbol) do
    GTemplateBinOps[LSymbol] := boInvalid;
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

initialization

initOps;

end.
