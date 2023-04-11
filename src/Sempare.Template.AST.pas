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
unit Sempare.Template.AST;

interface

uses
  System.SysUtils,
  System.Rtti;

type
  ETemplate = class(Exception);

  TForOp = ( //
    foTo, foDownto, // for var := low to/downto high
    foIn, foOf // for var in/of enumerable
    );

  TBinOp = ( //
    boInvalid, //
    boAND, boOR, // logical
    boPlus, boMinus, boSlash, boDiv, boMult, boMod, // numeric
    boEQ, boNotEQ, boLT, boLTE, boGT, boGTE, // comparison
    boIN);

  TStripAction = ( //
    saWhitespace, //
    saNL, //
    saKeepOneSpace //
    );

  TStripActionSet = set of TStripAction;

  TTemplateSymbol = ( //
    // general parsing
    vsInvalid, //
    vsEOF, //
    vsStartScript, //
    vsEndScript, //
    vsOpenRoundBracket, //
    vsCloseRoundBracket, //
    vsComment, //

    // statements
    vsText, // this is normally just written to stream
    vsIf, //
    vsElIf, //
    vsElse, //
    vsWhile, //
    vsFor, //
    vsWith, //
    vsTemplate, //
    vsBreak, //
    vsContinue, //
    vsPrint, //
    vsInclude, //
    vsRequire, //
    vsIgnoreNL, //
    vsIgnoreWS, //
    vsOffset, //
    vsStep, //
    vsLimit, //
    vsCycle, //
    vsEnd, //
    vsOnBegin, //
    vsOnEnd, //
    vsOnEmpty, //
    vsBetweenItem, //

    // for statements
    vsIn, //
    vsOf, //
    vsTo, //
    vsDownto, //

    // assignment statement
    vsCOLONEQ, //

    // identifier
    vsID, //
    vsDOT, //
    vsOpenSquareBracket, //
    vsCloseSquareBracket, //

    // types
    vsNumber, //
    vsBoolean, //
    vsString, //

    // logical operations
    vsAND, //
    vsOR, //
    vsNOT, //

    // ternary
    vsQUESTION, //
    vsCOLON, //

    // comparison operations
    vsEQ, //
    vsNotEQ, //
    vsLT, //
    vsLTE, //
    vsGT, //
    vsGTE, //

    // numeric expressions
    vsPLUS, //
    vsMinus, //
    vsSLASH, //
    vsDIV, //
    vsMULT, //
    vsMOD, //

    // for expression list
    vsComma, //
    vsSemiColon, //

    vsExtends, //
    vsBlock, //

    vsNewLine, //
    vsWhiteSpace //
    );

  TTemplateSymbolSet = set of TTemplateSymbol;

  IPosition = interface
    ['{2F087E1F-42EE-44D4-B928-047AA1788F50}']
    function GetFilename: string;
    procedure SetFilename(const AFilename: string);
    function GetLine: integer;
    procedure SetLine(const Aline: integer);
    function GetPos: integer;
    procedure SetPos(const Apos: integer);
    property FileName: string read GetFilename write SetFilename;
    property Line: integer read GetLine write SetLine;
    property Pos: integer read GetPos write SetPos;
  end;

  ITemplateSymbol = interface
    ['{3EC6C60C-164F-4BF5-AF2E-8F3CFC30C594}']
    function GetPosition: IPosition;
    function GetToken: TTemplateSymbol;
    function GetStripActions: TStripActionSet;
    procedure SetToken(const AToken: TTemplateSymbol);
    property Token: TTemplateSymbol read GetToken write SetToken;
    property Position: IPosition read GetPosition;
    property StripActions: TStripActionSet read GetStripActions;
  end;

  ITemplateVisitor = interface;
  IEvaluationTemplateVisitor = interface;

  IPositional = interface
    ['{DFA45EC1-7F39-4FB2-9894-8BD8D4ABA975}']
    function GetPosition: IPosition;
    property Position: IPosition read GetPosition;
  end;

  ITemplateVisitorHost = interface(IPosition)
    ['{BB5F2BF7-390D-4E20-8FD2-DB7609519143}']
    procedure Accept(const AVisitor: ITemplateVisitor);

  end;

  IExpr = interface(ITemplateVisitorHost)
    ['{8C539211-ED84-4963-B894-C569C2F7B2FE}']
  end;

  IStmt = interface(ITemplateVisitorHost)
    ['{6D37028E-A0C0-41F1-8A59-EDC0C9ADD9C7}']
    function Flatten: TArray<IStmt>;
    function GetHasEnd: boolean;
    property HasEnd: boolean read GetHasEnd;
  end;

  IDebugStmt = interface(IStmt)
    ['{1052F5A0-00BC-4BBB-A3BF-C214B1FB2166}']
    function GetStmt: IStmt;
    property Stmt: IStmt read GetStmt;
  end;

  TParserOption = (poAllowEnd, poAllowElse, poAllowElIf, poHasElse, poInLoop, poStripNL, poStripWS);
  TParserOptions = set of TParserOption;

  ITemplate = interface(ITemplateVisitorHost)
    ['{93AAB971-5B4B-4959-93F2-6C7DAE15C91B}']
    function GetItem(const AOffset: integer): IStmt;
    function GetCount: integer;
    function GetLastItem: IStmt;
    procedure FlattenTemplate;
    procedure OptimiseTemplate(const AOptions: TParserOptions);
    property Items[const AOffset: integer]: IStmt read GetItem;
    property Count: integer read GetCount;
    property LastItem: IStmt read GetLastItem;
  end;

  TAddLocation = (alLast, alBeforeNL, alAfterNL);

  ITemplateAdd = interface(ITemplate)
    ['{64465D68-0E9D-479F-9EF3-A30E75967809}']
    procedure Add(const AItem: IStmt; const AAddLocation: TAddLocation = alLast);
  end;

  IBlockStmt = interface;

  IExtendsStmt = interface(IStmt)
    ['{220D7E83-280D-454B-BA60-622C97EBE131}']
    function GetName: IExpr;
    function NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
    function GetBlockContainer: ITemplate;
    property Name: IExpr read GetName;
    property BlockContainer: ITemplate read GetBlockContainer;
  end;

  IBlockStmt = interface(IStmt)
    ['{EBAC38C4-9790-4D7C-844C-BE7C94E7C822}']
    function GetName: IExpr;
    function NameAsString(const AEvalVisitor: IEvaluationTemplateVisitor): string;
    function GetContainer: ITemplate;
    property Name: IExpr read GetName;
    property Container: ITemplate read GetContainer;
  end;

  IContinueStmt = interface(IStmt)
    ['{FB4CC3AB-BFEC-4189-B555-153DDA490D15}']
  end;

  TStripDirection = (sdEnd, sdLeft, sdRight, sdBeforeNewLine, sdAfterNewLine);

  IStripStmt = interface(IStmt)
    ['{3313745B-D635-4453-9808-660DC462E15C}']
    function GetDirection: TStripDirection;
    function GetAction: TStripActionSet;
    function GetHasEnd: boolean;
    procedure SetHasEnd(const AHasEnd: boolean);
    function GetIndent: string;
    procedure SetIndent(const AIndent: string);
    property Direction: TStripDirection read GetDirection;
    property Action: TStripActionSet read GetAction;
    property HasEnd: boolean read GetHasEnd write SetHasEnd;
    property Indent: string read GetIndent write SetIndent;
  end;

  ICompositeStmt = interface(IStmt)
    ['{790FB188-9763-401F-A0B1-FC9CCF4EF18D}']
    function GetFirstStmt: IStmt;
    function GetSecondStmt: IStmt;
    property FirstStmt: IStmt read GetFirstStmt;
    property SecondStmt: IStmt read GetSecondStmt;
  end;

  IEndStmt = interface(IStmt)
    ['{926DED70-2F66-4810-9DF1-FCFD83FF7E5D}']
  end;

  IElseStmt = interface(IStmt)
    ['{C82384C1-73D8-47D8-8A8C-068BA613FDD8}']
  end;

  INoopStmt = interface(IStmt)
    ['{0F11CAFA-E6FB-487E-95F4-B9E96BA2F175}']
  end;

  IBreakStmt = interface(IStmt)
    ['{41186941-96DA-4ED1-9641-A8E5F2AAB839}']
  end;

  ICommentStmt = interface(IStmt)
    ['{97E8C72D-B17C-41AB-8911-9B1493FE8C19}']
  end;

  IPrintStmt = interface(IStmt)
    ['{56430198-758A-412A-9A11-18399BBC3AD4}']
    function GetExpr: IExpr;
    property Expr: IExpr read GetExpr;
  end;

  IIncludeStmt = interface(IStmt)
    ['{017F5D58-320A-46A3-B128-4134C6E3440A}']
    function GetExpr: IExpr;
    property Expr: IExpr read GetExpr;
  end;

  IExprList = interface;

  IRequireStmt = interface(IStmt)
    ['{DCFF1F48-C9E7-45A9-BD59-C9767FDAB0D2}']
    function GetExprList: IExprList;
    property ExprList: IExprList read GetExprList;
  end;

  IEncodeExpr = interface(IExpr)
    ['{F192426F-5D83-4DF7-84C4-182CF90AFFA8}']
    function GetExpr: IExpr;
    property Expr: IExpr read GetExpr;
  end;

  IIfStmt = interface(IStmt)
    ['{A4FE6DA9-95A4-4DF7-8FD7-40529CC83D58}']
    function GetCondition: IExpr;
    function GetTrueContainer: ITemplate;
    function GetFalseContainer: ITemplate;
    property Condition: IExpr read GetCondition;
    property TrueContainer: ITemplate read GetTrueContainer;
    property FalseContainer: ITemplate read GetFalseContainer;
  end;

  IElIfStmt = interface(IStmt)
    ['{F7924A72-B732-46F4-9F27-F0994C165861}']
  end;

  IProcessTemplateStmt = interface(IStmt)
    ['{01CB121F-41D3-46DB-971E-032CF5853662}']
    function GetContainer: ITemplate;
    function GetAllowNewLine: boolean;
    procedure SetAllowNewLine(const AAllow: boolean);
    property Container: ITemplate read GetContainer;
    property AllowNewLine: boolean read GetAllowNewLine write SetAllowNewLine;
  end;

  IDefineTemplateStmt = interface(IStmt)
    ['{F07047CF-BA4E-43C9-B994-A2A942DE3936}']
    function GetName: IExpr;
    function GetContainer: ITemplate;
    property Name: IExpr read GetName;
    property Container: ITemplate read GetContainer;
  end;

  IWithStmt = interface(IStmt)
    ['{235E83C8-B4B2-4593-889A-F9CD8AE5E77B}']
    function GetExpr: IExpr;
    function GetContainer: ITemplate;
    property Expr: IExpr read GetExpr;
    property Container: ITemplate read GetContainer;
  end;

  ILoopStmt = interface(IStmt)
    ['{D6C26A41-3250-4EB9-A776-8952DE3931BD}']
    function GetOnBeginContainer: ITemplate;
    function GetOnEndContainer: ITemplate;
    function GetOnEmptyContainer: ITemplate;
    function GetBetweenItemContainer: ITemplate;
    property OnBeginContainer: ITemplate read GetOnBeginContainer;
    property OnEndContainer: ITemplate read GetOnEndContainer;
    property OnEmptyContainer: ITemplate read GetOnEmptyContainer;
    property BetweenItemsContainer: ITemplate read GetBetweenItemContainer;
  end;

  IWhileStmt = interface(ILoopStmt)
    ['{A353861E-BFEB-4630-9DC4-19A19A896613}']
    function GetCondition: IExpr;
    function GetContainer: ITemplate;
    function GetOffsetExpr: IExpr;
    function GetLimitExpr: IExpr;
    property Condition: IExpr read GetCondition;
    property Container: ITemplate read GetContainer;
    property OffsetExpr: IExpr read GetOffsetExpr;
    property LimitExpr: IExpr read GetLimitExpr;
  end;

  IForInStmt = interface(ILoopStmt)
    ['{DE078CDD-B50A-4036-987E-E2FD241950F6}']
    function GetVariable: string;
    function GetForOp: TForOp;
    function GetExpr: IExpr;
    function GetContainer: ITemplate;
    function GetOffsetExpr: IExpr;
    function GetLimitExpr: IExpr;
    property Variable: string read GetVariable;
    property ForOp: TForOp read GetForOp;
    property Expr: IExpr read GetExpr;
    property Container: ITemplate read GetContainer;
    property OffsetExpr: IExpr read GetOffsetExpr;
    property LimitExpr: IExpr read GetLimitExpr;
  end;

  IForRangeStmt = interface(ILoopStmt)
    ['{16645615-2E4E-4F1C-85BA-0EA3F1012F47}']
    function GetVariable: string;
    function GetForOp: TForOp;
    function GetLowExpr: IExpr;
    function GetHighExpr: IExpr;
    function GetContainer: ITemplate;
    function GetStepExpr: IExpr;
    property Variable: string read GetVariable;
    property ForOp: TForOp read GetForOp;
    property LowExpr: IExpr read GetLowExpr;
    property HighExpr: IExpr read GetHighExpr;
    property Container: ITemplate read GetContainer;
    property StepExpr: IExpr read GetStepExpr;
  end;

  IExprList = interface(ITemplateVisitorHost)
    ['{DAA177EA-B3CB-434C-A8C1-9E7571F6441B}']
    function GetExpr(const AOffset: integer): IExpr;
    procedure AddExpr(const AExpr: IExpr);
    function GetExprCount: integer;
    property Expr[const AOffset: integer]: IExpr read GetExpr; default;
    property Count: integer read GetExprCount;
  end;

  IArrayExpr = interface(IExpr)
    ['{3EB3BEBF-9BB0-4E8E-8919-CFE913394427}']
    function GetExprList: IExprList;
    property ExprList: IExprList read GetExprList;
  end;

  IValueExpr = interface(IExpr)
    ['{69D51BDD-C007-4ECE-926D-146CD1CD26D0}']
    function GetValue: TValue;
    property Value: TValue read GetValue;
  end;

  INewLineExpr = interface(IValueExpr)
    ['{A140C174-1C92-4C4C-AD8C-5A46066B2338}']
  end;

  IWhitespaceExpr = interface(IValueExpr)
    ['{65ACCA2B-B671-42E9-8ED4-179B11C36AA0}']
  end;

  IVariableExpr = interface(IExpr)
    ['{AE35E829-A756-4A1C-9F41-01CF3DD34096}']
    function GetVariable: string;
    property Variable: string read GetVariable;
  end;

  ICycleStmt = interface(IStmt)
    ['{EDABBF0D-8118-49F8-BE20-01143C11B377}']
    function GetList: IExprList;
    property List: IExprList read GetList;
  end;

  ITernaryExpr = interface(IExpr)
    ['{112A2FAA-B411-4F31-B873-B85EFC26AC5B}']
    function GetExpr: IExpr;
    function GetTrueExpr: IExpr;
    function GetFalseExpr: IExpr;
    property Condition: IExpr read GetExpr;
    property TrueExpr: IExpr read GetTrueExpr;
    property FalseExpr: IExpr read GetFalseExpr;
  end;

  TDerefType = (dtObject, dtArray);

  IVariableDerefExpr = interface(IExpr)
    ['{9EB744DC-B8FE-4923-B9B1-C07D95C5314F}']
    function GetExpr: IExpr;
    function GetDerefExpr: IExpr;
    function GetDerefType: TDerefType;
    property DerefType: TDerefType read GetDerefType;
    property Variable: IExpr read GetExpr;
    property DerefExpr: IExpr read GetDerefExpr;
  end;

  TTemplateValue = TValue;

  IFunctionCallExpr = interface(IExpr)
    ['{9F83BF0D-DFA2-47A6-BEEB-5A1D0C829BD4}']
    function GetFunctionInfo: TArray<TRttiMethod>;
    function GetExprList: IExprList;
    property FunctionInfo: TArray<TRttiMethod> read GetFunctionInfo;
    property ExprList: IExprList read GetExprList;
  end;

  IMethodCallExpr = interface(IExpr)
    ['{E1CCD143-58E9-4A37-AD0D-5DBC941E3D95}']
    function GetMethod: string;
    function GetObject: IExpr;
    function GetExprList: IExprList;
    function GetRttiMethod: TRttiMethod;
    procedure SetRttiMethod(const ARttiMethod: TRttiMethod);
    property Method: string read GetMethod;
    property ObjectExpr: IExpr read GetObject;
    property ExprList: IExprList read GetExprList;
    property RttiMethod: TRttiMethod read GetRttiMethod write SetRttiMethod;
  end;

  IAssignStmt = interface(IStmt)
    ['{77812845-8978-428E-B714-9B8047F03DDC}']
    function GetVariable: string;
    function GetExpr: IExpr;
    property Variable: string read GetVariable;
    property Expr: IExpr read GetExpr;
  end;

  IBinopExpr = interface(IExpr)
    ['{362A6CE1-D93E-4830-A176-3A4740883ECF}']
    function GetBinOp: TBinOp;
    function GetLeftExpr: IExpr;
    function GetRightExpr: IExpr;
    property BinOp: TBinOp read GetBinOp;
    property LeftExpr: IExpr read GetLeftExpr;
    property RightExpr: IExpr read GetRightExpr;
  end;

  TUnaryOp = (uoMinus, uoNot);

  IUnaryExpr = interface(IExpr)
    ['{CE9AAC1A-F8BF-481C-81A7-BEC923356DCC}']
    function GetUnaryOp: TUnaryOp;
    function GetExpr: IExpr;
    property UnaryOp: TUnaryOp read GetUnaryOp;
    property Expr: IExpr read GetExpr;
  end;

  ITemplateVisitor = interface
    ['{3AF4EFB5-2E01-4FCF-B609-DC2D322CE150}']
    procedure Visit(const AContainer: ITemplate); overload;
    procedure Visit(const AExpr: IExpr); overload;
    procedure Visit(const AExpr: IBinopExpr); overload;
    procedure Visit(const AExpr: IUnaryExpr); overload;
    procedure Visit(const AExpr: IVariableExpr); overload;
    procedure Visit(const AExpr: IWhitespaceExpr); overload;
    procedure Visit(const AExpr: INewLineExpr); overload;
    procedure Visit(const AExpr: IValueExpr); overload;
    procedure Visit(const AExpr: ITernaryExpr); overload;
    procedure Visit(const AExpr: IArrayExpr); overload;
    procedure Visit(const AExpr: IFunctionCallExpr); overload;
    procedure Visit(const AExpr: IMethodCallExpr); overload;
    procedure Visit(const AExpr: IVariableDerefExpr); overload;
    procedure Visit(const AExprList: IExprList); overload;
    procedure Visit(const AStmt: IStmt); overload;
    procedure Visit(const AStmt: IElseStmt); overload;
    procedure Visit(const AStmt: IAssignStmt); overload;
    procedure Visit(const AStmt: IContinueStmt); overload;
    procedure Visit(const AStmt: IBreakStmt); overload;
    procedure Visit(const AStmt: IEndStmt); overload;
    procedure Visit(const AStmt: IIncludeStmt); overload;
    procedure Visit(const AStmt: IRequireStmt); overload;
    procedure Visit(const AStmt: IEncodeExpr); overload;
    procedure Visit(const AStmt: IPrintStmt); overload;
    procedure Visit(const AStmt: IIfStmt); overload;
    procedure Visit(const AStmt: IWhileStmt); overload;
    procedure Visit(const AStmt: IForInStmt); overload;
    procedure Visit(const AStmt: IForRangeStmt); overload;
    procedure Visit(const AStmt: IProcessTemplateStmt); overload;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload;
    procedure Visit(const AStmt: IWithStmt); overload;
    procedure Visit(const AStmt: ICycleStmt); overload;
    procedure Visit(const AStmt: IDebugStmt); overload;
    procedure Visit(const AStmt: IBlockStmt); overload;
    procedure Visit(const AStmt: IExtendsStmt); overload;
    procedure Visit(const AStmt: ICompositeStmt); overload;
    procedure Visit(const AStmt: IStripStmt); overload;
  end;

  IEvaluationTemplateVisitor = interface(ITemplateVisitor)
    ['{D7993669-463E-4DBD-ACA2-76A7A6FF059A}']
    function EvalExpr(const AExpr: IExpr): TValue;
    function EvalExprAsString(const AExpr: IExpr): string;
    function EvalExprAsInt(const AExpr: IExpr): int64;
    function EvalExprAsNum(const AExpr: IExpr): extended;
    function EvalExprAsBoolean(const AExpr: IExpr): boolean;
    function ResolveTemplate(const AExpr: IExpr): ITemplate;
    procedure VisitStmt(const AStmt: IStmt);
  end;

const
  StripDirectionStr: array [TStripDirection] of string = ( //
    'sdEnd', 'sdLeft', 'sdRight', 'sdBeforeNewLine', 'sdAfterNewLine');

  StripActionStr: array [TStripAction] of string = ( //
    'saWhitespace', //
    'saNL', //
    'saKeepOneSpace' //
    );

type
  TStringActionsHelper = record helper for TStripActionSet
    function ToString: string;
  end;

implementation

function TStringActionsHelper.ToString: string;
var
  LAction: TStripAction;
begin
  result := '';
  for LAction in self do
  begin
    if result.Length > 0 then
      result := result + ',';
    result := result + StripActionStr[LAction];
  end;
end;

end.
