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
unit Sempare.Template.Visitor;

interface

uses
  Sempare.Template.AST,
  Sempare.Template.Common;

type
  ETemplateVisitor = class(ETemplate);

  TBaseTemplateVisitor = class(TInterfacedObject, ITemplateVisitor)
  public
    procedure Visit(const AContainer: ITemplate); overload; virtual;

    procedure Visit(const AExpr: IExpr); overload; virtual;
    procedure Visit(const AExpr: IBinopExpr); overload; virtual;
    procedure Visit(const AExpr: IUnaryExpr); overload; virtual;
    procedure Visit(const AExpr: IVariableExpr); overload; virtual;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; virtual;
    procedure Visit(const AExpr: IValueExpr); overload; virtual;
    procedure Visit(const AExpr: INewLineExpr); overload; virtual;
    procedure Visit(const AExpr: IWhitespaceExpr); overload; virtual;
    procedure Visit(const AExprList: IExprList); overload; virtual;
    procedure Visit(const AExpr: ITernaryExpr); overload; virtual;
    procedure Visit(const AExpr: IArrayExpr); overload; virtual;
    procedure Visit(const AStmt: IFunctionCallExpr); overload; virtual;
    procedure Visit(const AStmt: IMethodCallExpr); overload; virtual;
    procedure Visit(const AStmt: IEncodeExpr); overload; virtual;

    procedure Visit(const AStmt: IAssignStmt); overload; virtual;
    procedure Visit(const AStmt: IContinueStmt); overload; virtual;
    procedure Visit(const AStmt: IElseStmt); overload; virtual;
    procedure Visit(const AStmt: IBreakStmt); overload; virtual;
    procedure Visit(const AStmt: IEndStmt); overload; virtual;
    procedure Visit(const AStmt: IIncludeStmt); overload; virtual;
    procedure Visit(const AStmt: IRequireStmt); overload; virtual;
    procedure Visit(const AStmt: IPrintStmt); overload; virtual;
    procedure Visit(const AStmt: IIfStmt); overload; virtual;
    procedure Visit(const AStmt: IWhileStmt); overload; virtual;
    procedure Visit(const AStmt: IForInStmt); overload; virtual;
    procedure Visit(const AStmt: IForRangeStmt); overload; virtual;
    procedure Visit(const AStmt: IProcessTemplateStmt); overload; virtual;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload; virtual;
    procedure Visit(const AStmt: IWithStmt); overload; virtual;
    procedure Visit(const AStmt: ICycleStmt); overload; virtual;
    procedure Visit(const AStmt: IDebugStmt); overload; virtual;
    procedure Visit(const AStmt: ICompositeStmt); overload; virtual;
    procedure Visit(const AStmt: IStripStmt); overload; virtual;
    procedure Visit(const AStmt: IStmt); overload; virtual;
    procedure Visit(const AStmt: INoopStmt); overload; virtual;

    procedure Visit(const AStmt: IBlockStmt); overload; virtual;
    procedure Visit(const AStmt: IExtendsStmt); overload; virtual;
  end;

  TNoExprTemplateVisitor = class(TBaseTemplateVisitor, ITemplateVisitor)
  public

    procedure Visit(const AExpr: IBinopExpr); overload; override;
    procedure Visit(const AExpr: IUnaryExpr); overload; override;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(const AExprList: IExprList); overload; override;
    procedure Visit(const AExpr: ITernaryExpr); overload; override;
    procedure Visit(const AExpr: IArrayExpr); overload; override;
    procedure Visit(const AStmt: IFunctionCallExpr); overload; override;
    procedure Visit(const AStmt: IMethodCallExpr); overload; override;
    procedure Visit(const AStmt: IEncodeExpr); overload; override;

  end;

implementation

uses
  Sempare.Template.ResourceStrings,
  System.SysUtils;

{ TBaseTemplateVisitor }

procedure TBaseTemplateVisitor.Visit(const AExpr: IVariableExpr);
begin
  // don't do anything
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IValueExpr);
begin
  // don't do anything
end;

procedure TBaseTemplateVisitor.Visit(const AExprList: IExprList);
begin
  AcceptVisitor(AExprList, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IStmt);
begin
  raise ETemplateVisitor.Create(SStatementNotSupportedInVisitor);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IExpr);
begin
  raise ETemplateVisitor.Create(SExpressionNotSupportedInVisitor);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IBinopExpr);
begin
  AcceptVisitor(AExpr.LeftExpr, self);
  AcceptVisitor(AExpr.RightExpr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IUnaryExpr);
begin
  AcceptVisitor(AExpr.Expr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IIfStmt);
begin
  AcceptVisitor(AStmt.Condition, self);
  AcceptVisitor(AStmt.TrueContainer, self);
  AcceptVisitor(AStmt.FalseContainer, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IWhileStmt);
begin
  AcceptVisitor(AStmt.Condition, self);
  AcceptVisitor(AStmt.Container, self);
  AcceptVisitor(AStmt.OffsetExpr, self);
  AcceptVisitor(AStmt.LimitExpr, self);
  AcceptVisitor(AStmt.OnBeginContainer, self);
  AcceptVisitor(AStmt.OnEndContainer, self);
  AcceptVisitor(AStmt.OnEmptyContainer, self);
  AcceptVisitor(AStmt.BetweenItemsContainer, self);

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IForInStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
  AcceptVisitor(AStmt.Container, self);
  AcceptVisitor(AStmt.OffsetExpr, self);
  AcceptVisitor(AStmt.LimitExpr, self);
  AcceptVisitor(AStmt.OnBeginContainer, self);
  AcceptVisitor(AStmt.OnEndContainer, self);
  AcceptVisitor(AStmt.OnEmptyContainer, self);
  AcceptVisitor(AStmt.BetweenItemsContainer, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IForRangeStmt);
begin
  AcceptVisitor(AStmt.LowExpr, self);
  AcceptVisitor(AStmt.HighExpr, self);
  AcceptVisitor(AStmt.StepExpr, self);
  AcceptVisitor(AStmt.Container, self);
  AcceptVisitor(AStmt.OnBeginContainer, self);
  AcceptVisitor(AStmt.OnEndContainer, self);
  AcceptVisitor(AStmt.OnEmptyContainer, self);
  AcceptVisitor(AStmt.BetweenItemsContainer, self);
end;

procedure TBaseTemplateVisitor.Visit(const AContainer: ITemplate);
begin
  AcceptVisitor(AContainer, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IAssignStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IIncludeStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IVariableDerefExpr);
begin
  AcceptVisitor(AExpr.Variable, self);
  AcceptVisitor(AExpr.DerefExpr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IContinueStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IBreakStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IEndStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IPrintStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IFunctionCallExpr);
begin
  AcceptVisitor(AStmt.ExprList, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IElseStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IMethodCallExpr);
begin
  AcceptVisitor(AStmt.ObjectExpr, self);
  AcceptVisitor(AStmt.ExprList, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IEncodeExpr);
begin
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin
  AcceptVisitor(AStmt.Name, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IWithStmt);
begin
  AcceptVisitor(AStmt.Expr, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IRequireStmt);
begin
  AcceptVisitor(AStmt.ExprList, self);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IArrayExpr);
begin
  AcceptVisitor(AExpr.ExprList, self);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: ITernaryExpr);
begin
  AcceptVisitor(AExpr.Condition, self);
  AcceptVisitor(AExpr.TrueExpr, self);
  AcceptVisitor(AExpr.FalseExpr, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: ICycleStmt);
begin
  AcceptVisitor(AStmt.List, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IDebugStmt);
begin
  AcceptVisitor(AStmt.Stmt, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IBlockStmt);
begin
  AcceptVisitor(AStmt.Name, self);
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IStripStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: ICompositeStmt);
begin
  AcceptVisitor(AStmt.FirstStmt, self);
  AcceptVisitor(AStmt.SecondStmt, self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IExtendsStmt);
begin
  AcceptVisitor(AStmt.Name, self);
  AcceptVisitor(AStmt.BlockContainer, self);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IWhitespaceExpr);
begin
  Visit(AExpr as IValueExpr);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: INewLineExpr);
begin
  Visit(AExpr as IValueExpr);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: INoopStmt);
begin

end;

{ TNoExprTemplateVisitor }

procedure TNoExprTemplateVisitor.Visit(const AExprList: IExprList);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AExpr: IVariableDerefExpr);
begin

end;

procedure TNoExprTemplateVisitor.Visit(const AExpr: IUnaryExpr);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AExpr: IBinopExpr);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AExpr: ITernaryExpr);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AStmt: IEncodeExpr);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AStmt: IMethodCallExpr);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AStmt: IFunctionCallExpr);
begin
  // do nothing
end;

procedure TNoExprTemplateVisitor.Visit(const AExpr: IArrayExpr);
begin
  // do nothing
end;

end.
