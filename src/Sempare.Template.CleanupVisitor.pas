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
unit Sempare.Template.CleanupVisitor;

interface

uses
  Sempare.Template.AST,
  Sempare.Template.Common,
  Sempare.Template.Visitor;

type
  ICleanupVisitor = interface(ITemplateVisitor)
    ['{E4E25DDD-CA4F-48B9-B3A2-C265EDF611B2}']
    procedure Cleanup(const ATemplate: ITemplate);
  end;

  TCleanupVisitor = class(TNoExprTemplateVisitor, ICleanupVisitor)
  private
    FEvalVisitor: IEvaluationTemplateVisitor;
  public
    constructor Create(const AEvalVisitor: IEvaluationTemplateVisitor);

    procedure Cleanup(const ATemplate: ITemplate);

    procedure Visit(const AStmt: ITemplate); overload; override;
    procedure Visit(const AContainer: ITemplateVisitorHost); overload; override;
    procedure Visit(const AStmt: IIncludeStmt); overload; override;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;

  end;

implementation

uses
  System.SysUtils;

{ TCleanupVisitor }

procedure TCleanupVisitor.Visit(const AStmt: IBlockStmt);
begin
  inherited Visit(AStmt);
end;

procedure TCleanupVisitor.Visit(const AStmt: IIncludeStmt);
begin
  AcceptVisitor(FEvalVisitor.ResolveTemplate(AStmt.Expr), self);
end;

procedure TCleanupVisitor.Cleanup(const ATemplate: ITemplate);
begin
  AcceptVisitor(ATemplate, self);
end;

constructor TCleanupVisitor.Create(const AEvalVisitor: IEvaluationTemplateVisitor);
begin
  FEvalVisitor := AEvalVisitor;
end;

procedure TCleanupVisitor.Visit(const AContainer: ITemplateVisitorHost);
begin
  inherited Visit(AContainer);
end;

procedure TCleanupVisitor.Visit(const AStmt: ITemplate);
begin
  inherited Visit(AStmt);
end;

procedure TCleanupVisitor.Visit(const AStmt: IExtendsStmt);
begin
  AcceptVisitor(FEvalVisitor.ResolveTemplate(AStmt.Name), self);
end;

end.
