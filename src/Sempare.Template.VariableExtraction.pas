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
unit Sempare.Template.VariableExtraction;

interface

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

  TTemplateReferenceExtractionVisitor = class(TBaseTemplateVisitor)
  private
    // TODO: performance wise, could review dictionary
    FLocalVariables: TList<string>;
    FVariables: TList<string>;
    FFunctions: TList<string>;
    function GetFunctions: TArray<string>;
    function GetVariables: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Visit(AExpr: IBinopExpr); overload; override;
    procedure Visit(AExpr: IUnaryExpr); overload; override;
    procedure Visit(AExpr: IVariableExpr); overload; override;
    procedure Visit(AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(AExpr: IValueExpr); overload; override;
    procedure Visit(AExprList: IExprList); overload; override;
    procedure Visit(AExpr: IEncodeExpr); overload; override;
    procedure Visit(AExpr: ITernaryExpr); overload; override;
    procedure Visit(AExpr: IArrayExpr); overload; override;

    procedure Visit(AStmt: IAssignStmt); overload; override;
    procedure Visit(AStmt: IContinueStmt); overload; override;
    procedure Visit(AStmt: IBreakStmt); overload; override;
    procedure Visit(AStmt: IIncludeStmt); overload; override;
    procedure Visit(AStmt: IRequireStmt); overload; override;
    procedure Visit(AStmt: IPrintStmt); overload; override;
    procedure Visit(AStmt: IIfStmt); overload; override;
    procedure Visit(AStmt: IWhileStmt); overload; override;
    procedure Visit(AStmt: IForInStmt); overload; override;
    procedure Visit(AStmt: IForRangeStmt); overload; override;
    procedure Visit(AStmt: IFunctionCallExpr); overload; override;
    procedure Visit(AStmt: IMethodCallExpr); overload; override;

    procedure Visit(AStmt: IProcessTemplateStmt); overload; override;
    procedure Visit(AStmt: IDefineTemplateStmt); overload; override;
    procedure Visit(AStmt: IWithStmt); overload; override;
    property Variables: TArray<string> read GetVariables;
    property Functions: TArray<string> read GetFunctions;
  end;

implementation

{ TTemplateReferenceExtractionVisitor }

constructor TTemplateReferenceExtractionVisitor.Create;
begin
  FLocalVariables := TList<string>.Create;
  FVariables := TList<string>.Create;
  FFunctions := TList<string>.Create;
end;

destructor TTemplateReferenceExtractionVisitor.Destroy;
begin
  FLocalVariables.Free;
  FVariables.Free;
  FFunctions.Free;
  inherited;
end;

function TTemplateReferenceExtractionVisitor.GetFunctions: TArray<string>;
begin
  exit(FFunctions.ToArray);
end;

function TTemplateReferenceExtractionVisitor.GetVariables: TArray<string>;
begin
  exit(FVariables.ToArray);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: ITernaryExpr);
begin
  AcceptVisitor(AExpr.Condition, self);
  AcceptVisitor(AExpr.TrueExpr, self);
  AcceptVisitor(AExpr.FalseExpr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IArrayExpr);
var
  LIdx: integer;
begin
  for LIdx := 0 to AExpr.ExprList.Count - 1 do
  begin
    AcceptVisitor(AExpr.ExprList.Expr[LIdx], self);
  end;
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IAssignStmt);
begin
  if not FLocalVariables.contains(AStmt.Variable) then
    FLocalVariables.Add(AStmt.Variable);
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IContinueStmt);
begin
  //
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IBreakStmt);
begin
  //
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IEncodeExpr);
begin
  AcceptVisitor(AExpr.Expr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IBinopExpr);
begin
  AcceptVisitor(AExpr.LeftExpr, self);
  AcceptVisitor(AExpr.RightExpr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IUnaryExpr);
begin
  AcceptVisitor(AExpr.Condition, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IVariableDerefExpr);
begin
  AcceptVisitor(AExpr.Variable, self);
  AcceptVisitor(AExpr.DerefExpr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IValueExpr);
begin
  //
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExprList: IExprList);
var
  i: integer;
begin
  for i := 0 to AExprList.Count - 1 do
  begin
    AcceptVisitor(AExprList[i], self);
  end;
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IForRangeStmt);
begin
  if not FLocalVariables.contains(AStmt.Variable) then
    FLocalVariables.Add(AStmt.Variable);

  AcceptVisitor(AStmt.LowExpr, self);
  AcceptVisitor(AStmt.HighExpr, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IMethodCallExpr);
begin
  Visit(AStmt.ObjectExpr);
  Visit(AStmt.ExprList);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IProcessTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IDefineTemplateStmt);
begin
  AcceptVisitor(AStmt.Name, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IWithStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IFunctionCallExpr);
begin
  if not FFunctions.contains(AStmt.FunctionInfo[0].Name) then
    FFunctions.Add(AStmt.FunctionInfo[0].Name);
  Visit(AStmt.ExprList);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IForInStmt);
begin
  if not FLocalVariables.contains(AStmt.Variable) then
    FLocalVariables.Add(AStmt.Variable);

  AcceptVisitor(AStmt.Expr, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IIncludeStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IRequireStmt);
var
  LIdx: integer;
begin
  for LIdx := 0 to AStmt.ExprList.Count - 1 do
  begin
    AcceptVisitor(AStmt.ExprList.Expr[LIdx], self);
  end;
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IPrintStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IIfStmt);
begin
  AcceptVisitor(AStmt.Condition, self);
  AcceptVisitor(AStmt.TrueContainer, self);
  if AStmt.FalseContainer <> nil then
  begin
    AcceptVisitor(AStmt.FalseContainer, self);
  end;
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AStmt: IWhileStmt);
begin
  AcceptVisitor(AStmt.Condition, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(AExpr: IVariableExpr);
begin
  if FLocalVariables.contains(AExpr.Variable) then
    exit;

  if not FVariables.contains(AExpr.Variable) then
    FVariables.Add(AExpr.Variable);
end;

end.
