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
  Sempare.Template.Evaluate,
  Sempare.Template.Visitor;

type
  IBlockResolverVisitor = interface(ITemplateVisitor)
    ['{623A7C4A-3592-46BD-A3C5-FE354E0E67C0}']
    function GetBlockNames: TArray<string>;
    function GetBlocks(const AName: string): TArray<IBlockStmt>;
  end;

  TBlockResolverVisitor = class(TBaseTemplateVisitor, IBlockResolverVisitor)
  private
    FBlocks: TDictionary<string, TArray<IBlockStmt>>;
    FEvalVisitor: IEvaluationTemplateVisitor;
  public
    constructor Create(const AEvalVisitor: IEvaluationTemplateVisitor);
    destructor Destroy; override;

    function GetBlockNames: TArray<string>;
    function GetBlocks(const AName: string): TArray<IBlockStmt>;

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

  end;

implementation

{ TBlockResolverVisitor }

procedure TBlockResolverVisitor.Visit(const AStmt: IForRangeStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IWithStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IForInStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IIfStmt);
begin
  AcceptVisitor(AStmt.TrueContainer, self);
  if AStmt.FalseContainer <> nil then
  begin
    AcceptVisitor(AStmt.FalseContainer, self);
  end;
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IWhileStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IDebugStmt);
begin
  AcceptVisitor(AStmt.Stmt, self);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IBlockStmt);
var
  LList: TArray<IBlockStmt>;
  LName: string;
begin
  LName := FEvalVisitor.EvalExprAsString(AStmt.Name);
  if not FBlocks.TryGetValue(LName, LList) then
  begin
    LList := nil;
  end;
  insert(AStmt, LList, length(LList));
  FBlocks.AddOrSetValue(LName, LList);
end;

procedure TBlockResolverVisitor.Visit(const AStmt: IExtendsStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

constructor TBlockResolverVisitor.Create(const AEvalVisitor: IEvaluationTemplateVisitor);
begin
  FEvalVisitor := AEvalVisitor;
  FBlocks := TDictionary < string, TArray < IBlockStmt >>.Create();
end;

destructor TBlockResolverVisitor.Destroy;
begin
  FBlocks.Clear;
  FBlocks.Free;
  inherited;
end;

function TBlockResolverVisitor.GetBlockNames: TArray<string>;
begin
  exit(FBlocks.Keys.ToArray);
end;

function TBlockResolverVisitor.GetBlocks(const AName: string): TArray<IBlockStmt>;
var
  LList: TArray<IBlockStmt>;
begin
  if FBlocks.TryGetValue(AName, LList) then
    exit(LList)
  else
    exit(nil);
end;

end.
