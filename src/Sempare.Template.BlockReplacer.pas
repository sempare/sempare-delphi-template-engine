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
unit Sempare.Template.BlockReplacer;

interface

uses
  Sempare.Template.AST,
  Sempare.Template.Common,
  Sempare.Template.Visitor;

type
  IBlockReplacerVisitor = interface(ITemplateVisitor)
    ['{81560D41-C5D1-49DF-9A26-98F2D7951E3A}']
    procedure Replace(const ATemplate: ITemplate; const ABlockName: string; const ABlock: ITemplate);
  end;

  TBlockReplacerVisitor = class(TNoExprTemplateVisitor, IBlockReplacerVisitor)
  private
    FBlockName: string;
    FReplacementBlock: ITemplate;
    FEvalVisitor: IEvaluationTemplateVisitor;

  public
    constructor Create(const AEvalVisitor: IEvaluationTemplateVisitor);

    procedure Visit(const AStmt: IIncludeStmt); overload; override;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;

    procedure Replace(const ATemplate: ITemplate; const ABlockName: string; const ABlock: ITemplate);

  end;

implementation

uses
  System.SysUtils;

{ TBlockReplacerVisitor }

procedure TBlockReplacerVisitor.Visit(const AStmt: IBlockStmt);
begin
  AcceptVisitor(AStmt.Container, self);
  if FEvalVisitor.EvalExprAsString(AStmt.Name) = FBlockName then
  begin
    AStmt.Container := FReplacementBlock;
  end;
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IIncludeStmt);
var
  LTemplate: ITemplate;
begin
  LTemplate := FEvalVisitor.ResolveTemplate(AStmt.Expr);
  AcceptVisitor(LTemplate, self);
end;

constructor TBlockReplacerVisitor.Create(const AEvalVisitor: IEvaluationTemplateVisitor);
begin
  FEvalVisitor := AEvalVisitor;
end;

procedure TBlockReplacerVisitor.Replace(const ATemplate: ITemplate; const ABlockName: string; const ABlock: ITemplate);
begin
  if not assigned(ABlock) then
    exit;
  FBlockName := ABlockName;
  FReplacementBlock := ABlock;
  AcceptVisitor(ATemplate, self);
  FReplacementBlock := nil;
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IExtendsStmt);
var
  LTemplate: ITemplate;
begin
  LTemplate := FEvalVisitor.ResolveTemplate(AStmt.Name);
  AcceptVisitor(LTemplate, self);
end;

end.
