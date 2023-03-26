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
    procedure Visit(const AContainer: ITemplateVisitorHost); overload; virtual;

    procedure Visit(const AExpr: IExpr); overload; virtual;
    procedure Visit(const AExpr: IBinopExpr); overload; virtual;
    procedure Visit(const AExpr: IUnaryExpr); overload; virtual;
    procedure Visit(const AExpr: IVariableExpr); overload; virtual;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; virtual;
    procedure Visit(const AExpr: IValueExpr); overload; virtual;
    procedure Visit(const AExprList: IExprList); overload; virtual;
    procedure Visit(const AExpr: ITernaryExpr); overload; virtual;
    procedure Visit(const AExpr: IArrayExpr); overload; virtual;

    procedure Visit(const AStmt: IStmt); overload; virtual;
    procedure Visit(const AStmt: IAssignStmt); overload; virtual;
    procedure Visit(const AStmt: IContinueStmt); overload; virtual;
    procedure Visit(const AStmt: IElseStmt); overload; virtual;
    procedure Visit(const AStmt: IBreakStmt); overload; virtual;
    procedure Visit(const AStmt: IEndStmt); overload; virtual;
    procedure Visit(const AStmt: IIncludeStmt); overload; virtual;
    procedure Visit(const AStmt: IRequireStmt); overload; virtual;
    procedure Visit(const AStmt: IEncodeExpr); overload; virtual;
    procedure Visit(const AStmt: IPrintStmt); overload; virtual;
    procedure Visit(const AStmt: IIfStmt); overload; virtual;
    procedure Visit(const AStmt: IWhileStmt); overload; virtual;
    procedure Visit(const AStmt: IForInStmt); overload; virtual;
    procedure Visit(const AStmt: IForRangeStmt); overload; virtual;
    procedure Visit(const AStmt: IFunctionCallExpr); overload; virtual;
    procedure Visit(const AStmt: IMethodCallExpr); overload; virtual;
    procedure Visit(const AStmt: IProcessTemplateStmt); overload; virtual;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload; virtual;
    procedure Visit(const AStmt: IWithStmt); overload; virtual;
    procedure Visit(const AStmt: ICycleStmt); overload; virtual;
    procedure Visit(const AStmt: IDebugStmt); overload; virtual;
    procedure Visit(const AStmt: IBlockStmt); overload; virtual;
    procedure Visit(const AStmt: IExtendsStmt); overload; virtual;
  end;

implementation

uses
  Sempare.Template.ResourceStrings,
  System.SysUtils;

{ TBaseTemplateVisitor }

procedure TBaseTemplateVisitor.Visit(const AExpr: IVariableExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IValueExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExprList: IExprList);
var
  LIndex: integer;
begin
  for LIndex := 0 to AExprList.Count - 1 do
    AcceptVisitor(AExprList[LIndex], self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IStmt);
begin
  raise ETemplateVisitor.Create(SStatementNotSupportedInVisitor);
end;

procedure TBaseTemplateVisitor.Visit(const AContainer: ITemplateVisitorHost);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IExpr);
begin
  raise ETemplateVisitor.Create(SExpressionNotSupportedInVisitor);
end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IBinopExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IUnaryExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IIfStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IWhileStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IForInStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IForRangeStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AContainer: ITemplate);
var
  LIdx: integer;
begin
  for LIdx := 0 to AContainer.Count - 1 do
    AContainer.Items[LIdx].Accept(self);
end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IAssignStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IIncludeStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IVariableDerefExpr);
begin

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

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IFunctionCallExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IElseStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IMethodCallExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IEncodeExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IWithStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IRequireStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExpr: IArrayExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AExpr: ITernaryExpr);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: ICycleStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IDebugStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IBlockStmt);
begin

end;

procedure TBaseTemplateVisitor.Visit(const AStmt: IExtendsStmt);
begin

end;

end.
