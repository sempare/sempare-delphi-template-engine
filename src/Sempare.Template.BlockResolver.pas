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
unit Sempare.Template.BlockResolver;

interface

uses
  System.Generics.Collections,
  Sempare.Template.AST,
  Sempare.Template.Common,
  Sempare.Template.Visitor;

type
  IBlockResolverVisitor = interface(ITemplateVisitor)
    ['{623A7C4A-3592-46BD-A3C5-FE354E0E67C0}']
    procedure Discover;
    function GetBlockNames: TArray<string>;
    function GetBlock(const AName: string): IBlockStmt;
  end;

  TBlockResolverVisitor = class(TNoExprTemplateVisitor, IBlockResolverVisitor)
  private
    FBlocks: TDictionary<string, IBlockStmt>;
    FEvalVisitor: IEvaluationTemplateVisitor;
    FTemplate: ITemplate;
  public
    constructor Create(const AEvalVisitor: IEvaluationTemplateVisitor; const ATemplate: ITemplate);
    destructor Destroy; override;
    procedure Discover;

    function GetBlockNames: TArray<string>;
    function GetBlock(const AName: string): IBlockStmt;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;
    procedure Visit(const AStmt: IIncludeStmt); overload; override;
  end;

implementation

{ TBlockResolverVisitor }

procedure TBlockResolverVisitor.Visit(const AStmt: IBlockStmt);
var
  LBlock: IBlockStmt;
  LName: string;
begin
  LName := FEvalVisitor.EvalExprAsString(AStmt.Name);
  if FBlocks.TryGetValue(LName, LBlock) then
  begin
    exit;
  end;
  FBlocks.AddOrSetValue(LName, AStmt);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IExtendsStmt);
var
  LTemplate: ITemplate;
begin
  LTemplate := FEvalVisitor.ResolveTemplate(AStmt.Name);
  AcceptVisitor(LTemplate, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IIncludeStmt);
var
  LTemplate: ITemplate;
begin
  LTemplate := FEvalVisitor.ResolveTemplate(AStmt.Expr);
  AcceptVisitor(LTemplate, self);
end;

constructor TBlockResolverVisitor.Create(const AEvalVisitor: IEvaluationTemplateVisitor; const ATemplate: ITemplate);
begin
  FEvalVisitor := AEvalVisitor;
  FBlocks := TDictionary<string, IBlockStmt>.Create();
  FTemplate := ATemplate;
end;

destructor TBlockResolverVisitor.Destroy;
begin
  FBlocks.Free;
  inherited;
end;

procedure TBlockResolverVisitor.Discover;
begin
  AcceptVisitor(FTemplate, self);
end;

function TBlockResolverVisitor.GetBlockNames: TArray<string>;
begin
  exit(FBlocks.Keys.ToArray);
end;

function TBlockResolverVisitor.GetBlock(const AName: string): IBlockStmt;
begin
  if not FBlocks.TryGetValue(AName, result) then
    exit(nil);
end;

end.
