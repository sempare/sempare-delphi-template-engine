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
    procedure Visit(const AExpr: IVariableExpr); overload; override;

    procedure Visit(const AStmt: IAssignStmt); overload; override;
    procedure Visit(const AStmt: IFunctionCallExpr); overload; override;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;
    procedure Visit(const AStmt: ICompositeStmt); overload; override;
    procedure Visit(const AStmt: IStripStmt); overload; override;

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

procedure TTemplateReferenceExtractionVisitor.Visit(const AStmt: IAssignStmt);
begin
  if not FLocalVariables.contains(AStmt.Variable) then
    FLocalVariables.Add(AStmt.Variable);
  AcceptVisitor(AStmt.Expr, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(const AStmt: IFunctionCallExpr);
begin
  if not FFunctions.contains(AStmt.FunctionInfo[0].Name) then
    FFunctions.Add(AStmt.FunctionInfo[0].Name);
  Visit(AStmt.ExprList);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(const AExpr: IVariableExpr);
begin
  if FLocalVariables.contains(AExpr.Variable) then
    exit;

  if not FVariables.contains(AExpr.Variable) then
    FVariables.Add(AExpr.Variable);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(const AStmt: IBlockStmt);
begin
  AcceptVisitor(AStmt.Container, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(const AStmt: IExtendsStmt);
begin
  AcceptVisitor(AStmt.BlockContainer, self);
end;

procedure TTemplateReferenceExtractionVisitor.Visit(const AStmt: IStripStmt);
begin

end;

procedure TTemplateReferenceExtractionVisitor.Visit(const AStmt: ICompositeStmt);
begin
  AcceptVisitor(AStmt.FirstStmt, self);
  AcceptVisitor(AStmt.SecondStmt, self);
end;

end.
