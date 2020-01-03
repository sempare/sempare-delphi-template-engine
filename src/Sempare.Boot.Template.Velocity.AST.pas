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
unit Sempare.Boot.Template.Velocity.AST;

interface

uses
  System.Rtti;

type
  TForOp = ( //
    foTo, foDownto, // for var := low to high
    foIn // for var in enumerable
    );

  TBinOp = ( //
    boInvalid,
    boAND, boOR, // logical
    boPlus, boMinus, boDiv, boMult, boMod, // numeric
    roEQ, roNotEQ, roLT, roLTE, roGT, roGTE, // comparison
    boIN
    );

  TVelocitySymbol = ( //
    // general parsing
    VsInvalid, //
    VsEOF, //
    VsStartScript, //
    VsEndScript, //
    VsOpenRoundBracket, //
    VsCloseRoundBracket, //
    VsComment, //

    // statements
    VsText, // this is normally just written to stream
    VsIF, //
    VsELIF, //
    vsElse, //
    vsWhile, //
    VsFOR, //
    VsWith, //
    vsTemplate, //
    VsBREAK, //
    VsCONTINUE, //
    VsPRINT, //
    VsINCLUDE, //
    VSRequire, //
    VsEND, //

    // for statements
    VsIN, //
    vsTo, //
    vsDownto, //

    // assignment statement
    VsCOLONEQ, //

    // identifier
    VsID, //
    VsDOT, //
    VsOpenSquareBracket, //
    VsCloseSquareBracket, //

    // types
    VsNumber, //
    VsBoolean, //
    vsString, //

    // logical operations
    VsAND, //
    VsOR, //
    VsNOT, //

    // ternary
    vsQUESTION, //
    vsCOLON, //

    // comparison operations
    VsEQ, //
    VsNotEQ, //
    vsLT, //
    vsLTE, //
    vsGT, //
    vsGTE, //

    // numeric expressions
    VsPLUS, //
    VsMinus, //
    VsDIV, //
    VsMULT, //
    VsMOD, //

    // for expression list
    vsComma, //
    vsSemiColon //
    );

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

  IVelocitySymbol = interface
    ['{3EC6C60C-164F-4BF5-AF2E-8F3CFC30C594}']

    function GetPosition: IPosition;
    function GetToken: TVelocitySymbol;
    procedure SetToken(const AToken: TVelocitySymbol);

    property Token: TVelocitySymbol read GetToken write SetToken;
    property Position: IPosition read GetPosition;
  end;

  IVelocityVisitor = interface;

  IPositional = interface
    ['{DFA45EC1-7F39-4FB2-9894-8BD8D4ABA975}']

    function GetPosition: IPosition;

    property Position: IPosition read GetPosition;
  end;

  IVelocityVisitorHost = interface
    ['{BB5F2BF7-390D-4E20-8FD2-DB7609519143}']

    procedure Accept(const AVisitor: IVelocityVisitor);
  end;

  IExpr = interface
    ['{8C539211-ED84-4963-B894-C569C2F7B2FE}']
  end;

  IStmt = interface
    ['{6D37028E-A0C0-41F1-8A59-EDC0C9ADD9C7}']
  end;

  IVelocityTemplate = interface
    ['{93AAB971-5B4B-4959-93F2-6C7DAE15C91B}']

    function GetItem(const AOffset: integer): IVelocityVisitorHost;
    function GetCount: integer;
    function GetLastItem: IVelocityVisitorHost;

    property Items[const AOffset: integer]: IVelocityVisitorHost read GetItem;
    property Count: integer read GetCount;
    property LastItem: IVelocityVisitorHost read GetLastItem;
  end;

  ITemplateAdd = interface(IVelocityTemplate)
    ['{64465D68-0E9D-479F-9EF3-A30E75967809}']

    procedure Add(const AItem: IVelocityVisitorHost);
  end;

  IContinueStmt = interface(IStmt)
    ['{FB4CC3AB-BFEC-4189-B555-153DDA490D15}']
  end;

  IEndStmt = interface(IStmt)
    ['{926DED70-2F66-4810-9DF1-FCFD83FF7E5D}']
  end;

  IElseStmt = interface(IStmt)
    ['{C82384C1-73D8-47D8-8A8C-068BA613FDD8}']
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

  IEncodeExpr = interface(IExpr)
    ['{F192426F-5D83-4DF7-84C4-182CF90AFFA8}']

    function GetExpr: IExpr;

    property Expr: IExpr read GetExpr;
  end;

  IIfStmt = interface(IStmt)
    ['{A4FE6DA9-95A4-4DF7-8FD7-40529CC83D58}']

    function GetCondition: IExpr;
    function GetTrueContainer: IVelocityTemplate;
    function GetFalseContainer: IVelocityTemplate;

    property Condition: IExpr read GetCondition;
    property TrueContainer: IVelocityTemplate read GetTrueContainer;
    property FalseContainer: IVelocityTemplate read GetFalseContainer;
  end;

  IElIfStmt = interface(IStmt)
    ['{F7924A72-B732-46F4-9F27-F0994C165861}']
  end;

  IProcessTemplateStmt = interface(IStmt)
    ['{01CB121F-41D3-46DB-971E-032CF5853662}']

    function GetContainer: IVelocityTemplate;

    property Container: IVelocityTemplate read GetContainer;
  end;

  IDefineTemplateStmt = interface(IStmt)
    ['{F07047CF-BA4E-43C9-B994-A2A942DE3936}']

    function GetName: IExpr;
    function GetContainer: IVelocityTemplate;

    property Name: IExpr read GetName;
    property Container: IVelocityTemplate read GetContainer;
  end;

  IWithStmt = interface(IStmt)
    ['{235E83C8-B4B2-4593-889A-F9CD8AE5E77B}']

    function GetExpr: IExpr;
    function GetContainer: IVelocityTemplate;

    property Expr: IExpr read GetExpr;
    property Container: IVelocityTemplate read GetContainer;
  end;

  IWhileStmt = interface(IStmt)
    ['{A353861E-BFEB-4630-9DC4-19A19A896613}']

    function GetCondition: IExpr;
    function GetContainer: IVelocityTemplate;

    property Condition: IExpr read GetCondition;
    property Container: IVelocityTemplate read GetContainer;
  end;

  IForInStmt = interface(IStmt)
    ['{DE078CDD-B50A-4036-987E-E2FD241950F6}']

    function GetVariable: string;
    function GetExpr: IExpr;
    function GetContainer: IVelocityTemplate;

    property Variable: string read GetVariable;
    property Expr: IExpr read GetExpr;
    property Container: IVelocityTemplate read GetContainer;
  end;

  IForRangeStmt = interface(IStmt)
    ['{DE078CDD-B50A-4036-987E-E2FD241950F6}']

    function GetVariable: string;
    function GetForOp: TForOp;
    function GetLowExpr: IExpr;
    function GetHighExpr: IExpr;
    function GetContainer: IVelocityTemplate;

    property Variable: string read GetVariable;
    property ForOp: TForOp read GetForOp;
    property LowExpr: IExpr read GetLowExpr;
    property HighExpr: IExpr read GetHighExpr;
    property Container: IVelocityTemplate read GetContainer;
  end;

  IExprList = interface(IVelocityVisitorHost)
    ['{DAA177EA-B3CB-434C-A8C1-9E7571F6441B}']

    function GetExpr(const AOffset: integer): IExpr;
    procedure AddExpr(const AExpr: IExpr);
    function GetExprCount: integer;

    property Expr[const AOffset: integer]: IExpr read GetExpr; default;
    property Count: integer read GetExprCount;
  end;

  IArrayExpr = interface(IExpr)
    ['{3EB3BEBF-9BB0-4E8E-8919-CFE913394427}']

    function GetValue: IExprList;

    property Value: IExprList read GetValue;
  end;

  IValueExpr = interface(IExpr)
    ['{69D51BDD-C007-4ECE-926D-146CD1CD26D0}']

    function GetValue: TValue;

    property Value: TValue read GetValue;
  end;

  IVariableExpr = interface(IExpr)
    ['{AE35E829-A756-4A1C-9F41-01CF3DD34096}']

    function GetVariable: string;

    property Variable: string read GetVariable;
  end;

  ITernaryExpr = interface(IExpr)
    ['{112A2FAA-B411-4F31-B873-B85EFC26AC5B}']

    function GetCondition: IExpr;
    function GetTrueExpr: IExpr;
    function GetFalseExpr: IExpr;

    property Condition: IExpr read GetCondition;
    property TrueExpr: IExpr read GetTrueExpr;
    property FalseExpr: IExpr read GetFalseExpr;
  end;

  TDerefType = (dtObject, dtArray);

  IVariableDerefExpr = interface(IExpr)
    ['{9EB744DC-B8FE-4923-B9B1-C07D95C5314F}']

    function GetVariable: IExpr;
    function GetDerefExpr: IExpr;
    function GetDerefType: TDerefType;

    property DerefType: TDerefType read GetDerefType;
    property Variable: IExpr read GetVariable;
    property DerefExpr: IExpr read GetDerefExpr;
  end;

  TVelocityValue = TValue;

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

  IVelocityVisitor = interface
    ['{3AF4EFB5-2E01-4FCF-B609-DC2D322CE150}']

    procedure Visit(const AContainer: IVelocityTemplate); overload;
    procedure Visit(const AExpr: IExpr); overload;
    procedure Visit(const AExpr: IBinopExpr); overload;
    procedure Visit(const AExpr: IUnaryExpr); overload;
    procedure Visit(const AExpr: IVariableExpr); overload;
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
    procedure Visit(const AStmt: IEncodeExpr); overload;
    procedure Visit(const AStmt: IPrintStmt); overload;
    procedure Visit(const AStmt: IIfStmt); overload;
    procedure Visit(const AStmt: IWhileStmt); overload;
    procedure Visit(const AStmt: IForInStmt); overload;
    procedure Visit(const AStmt: IForRangeStmt); overload;
    procedure Visit(const AStmt: IProcessTemplateStmt); overload;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload;
    procedure Visit(const AStmt: IWithStmt); overload;

  end;

implementation

end.
