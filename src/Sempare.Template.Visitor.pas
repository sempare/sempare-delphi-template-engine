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
unit Sempare.Template.Visitor;

interface

uses
  Sempare.Template.AST,
  Sempare.Template.Common;

type
  ETemplateVisitor = class(ETemplate);

  TBaseTemplateVisitor = class(TInterfacedObject, ITemplateVisitor)
  public
    procedure Visit(AContainer: ITemplate); overload; virtual;
    procedure Visit(AContainer: ITemplateVisitorHost); overload; virtual;

    procedure Visit(AExpr: IExpr); overload; virtual;
    procedure Visit(AExpr: IBinopExpr); overload; virtual;
    procedure Visit(AExpr: IUnaryExpr); overload; virtual;
    procedure Visit(AExpr: IVariableExpr); overload; virtual;
    procedure Visit(AExpr: IVariableDerefExpr); overload; virtual;
    procedure Visit(AExpr: IValueExpr); overload; virtual;
    procedure Visit(AExprList: IExprList); overload; virtual;
    procedure Visit(AExpr: ITernaryExpr); overload; virtual;
    procedure Visit(AExpr: IArrayExpr); overload; virtual;

    procedure Visit(AStmt: IStmt); overload; virtual;
    procedure Visit(AStmt: IAssignStmt); overload; virtual;
    procedure Visit(AStmt: IContinueStmt); overload; virtual;
    procedure Visit(AStmt: IElseStmt); overload; virtual;
    procedure Visit(AStmt: IBreakStmt); overload; virtual;
    procedure Visit(AStmt: IEndStmt); overload; virtual;
    procedure Visit(AStmt: IIncludeStmt); overload; virtual;
    procedure Visit(AStmt: IRequireStmt); overload; virtual;
    procedure Visit(AStmt: IEncodeExpr); overload; virtual;
    procedure Visit(AStmt: IPrintStmt); overload; virtual;
    procedure Visit(AStmt: IIfStmt); overload; virtual;
    procedure Visit(AStmt: IWhileStmt); overload; virtual;
    procedure Visit(AStmt: IForInStmt); overload; virtual;
    procedure Visit(AStmt: IForRangeStmt); overload; virtual;
    procedure Visit(AStmt: IFunctionCallExpr); overload; virtual;
    procedure Visit(AStmt: IMethodCallExpr); overload; virtual;
    procedure Visit(AStmt: IProcessTemplateStmt); overload; virtual;
    procedure Visit(AStmt: IDefineTemplateStmt); overload; virtual;
    procedure Visit(AStmt: IWithStmt); overload; virtual;

  end;

implementation

uses
  Sempare.Template.ResourceStrings,
  System.SysUtils;

{ TBaseTemplateVisitor }

procedure TBaseTemplateVisitor.Visit(AExpr: IVariableExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExpr: IValueExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExprList: IExprList);
var
  LIndex: integer;
begin
  for LIndex := 0 to AExprList.Count - 1 do
    AcceptVisitor(AExprList[LIndex], self);
end;

procedure TBaseTemplateVisitor.Visit(AStmt: IStmt);
begin
  raise ETemplateVisitor.Create(SStatementNotSupportedInVisitor);
end;

procedure TBaseTemplateVisitor.Visit(AContainer: ITemplateVisitorHost);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExpr: IExpr);
begin
  raise ETemplateVisitor.Create(SExpressionNotSupportedInVisitor);
end;

procedure TBaseTemplateVisitor.Visit(AExpr: IBinopExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExpr: IUnaryExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IIfStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IWhileStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IForInStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IForRangeStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AContainer: ITemplate);
var
  LIdx: integer;
begin
  for LIdx := 0 to AContainer.Count - 1 do
    AContainer.Items[LIdx].Accept(self);
end;

procedure TBaseTemplateVisitor.Visit(AStmt: IAssignStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IIncludeStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExpr: IVariableDerefExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IContinueStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IBreakStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IEndStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IPrintStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IFunctionCallExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IElseStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IMethodCallExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IEncodeExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IProcessTemplateStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IDefineTemplateStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IWithStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AStmt: IRequireStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExpr: IArrayExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(AExpr: ITernaryExpr);
begin

end;

end.
