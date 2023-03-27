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

  TBlockReplacerVisitor = class(TBaseTemplateVisitor, IBlockReplacerVisitor)
  private
    FBlockName: string;
    FReplacementBlock: ITemplate;
  public

    procedure Visit(const AStmt: IIfStmt); overload; override;
    procedure Visit(const AStmt: IWhileStmt); overload; override;
    procedure Visit(const AStmt: IForInStmt); overload; override;
    procedure Visit(const AStmt: IForRangeStmt); overload; override;

    procedure Visit(const AStmt: IProcessTemplateStmt); overload; override;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload; override;
    procedure Visit(const AStmt: IWithStmt); overload; override;

    procedure Visit(const AStmt: IDebugStmt); overload; override;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;

    procedure Replace(const ATemplate: ITemplate; const ABlockName: string; const ABlock: ITemplate);

  end;

implementation

uses
  System.SysUtils;

{ TBlockReplacerVisitor }

procedure TBlockReplacerVisitor.Visit(const AStmt: IForRangeStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IWithStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IForInStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IIfStmt);
begin
  AcceptVisitor(AStmt.TrueContainer, self);
  if AStmt.FalseContainer <> nil then
  begin
    AcceptVisitor(AStmt.FalseContainer, self);
  end;
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IWhileStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IDebugStmt);
begin
  AcceptVisitor(AStmt.Stmt, self);
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IBlockStmt);
begin
  if not assigned(AStmt.Container) then
    exit;
  AcceptVisitor(AStmt.Container, self);
  if AStmt.Name = FBlockName then
  begin
    AStmt.Container := FReplacementBlock;
  end;
end;

procedure TBlockReplacerVisitor.Visit(const AStmt: IExtendsStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockReplacerVisitor.Replace(const ATemplate: ITemplate; const ABlockName: string; const ABlock: ITemplate);
var
  LVisitor: ITemplateVisitor;
begin
  if not supports(self, ITemplateVisitor, LVisitor) then
    exit;
  if not assigned(ABlock) then
    exit;
  FBlockName := ABlockName;
  FReplacementBlock := ABlock;
  AcceptVisitor(ATemplate, LVisitor);
  FReplacementBlock := nil;
end;

end.
