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
unit Sempare.Template.PrettyPrint;

interface

uses
  System.SysUtils,
  Sempare.Template.AST,
  Sempare.Template.Common,
  Sempare.Template.Visitor;

type
  TPrettyPrintTemplateVisitor = class(TBaseTemplateVisitor)
  private
    FIndent: integer;
    FTab: string;
    FStringBuilder: TStringBuilder;

    procedure delta(const ADelta: integer);
    procedure tab;
    procedure Write(const AFMT: string); overload;
    procedure Write(const AFMT: string; const args: array of const); overload;
    procedure writeln(const AFMT: string); overload;
    procedure writeln(const AFMT: string; const args: array of const); overload;
  public
    constructor Create();
    destructor Destroy; override;
    function ToString: string; override;
    procedure Visit(const AExpr: IBinopExpr); overload; override;
    procedure Visit(const AExpr: IUnaryExpr); overload; override;
    procedure Visit(const AExpr: IVariableExpr); overload; override;
    procedure Visit(const AExpr: IVariableDerefExpr); overload; override;
    procedure Visit(const AExpr: IValueExpr); overload; override;
    procedure Visit(const AExprList: IExprList); overload; override;
    procedure Visit(const AExpr: IEncodeExpr); overload; override;
    procedure Visit(const AExpr: ITernaryExpr); overload; override;
    procedure Visit(const AExpr: IArrayExpr); overload; override;

    procedure Visit(const AStmt: IAssignStmt); overload; override;
    procedure Visit(const AStmt: IContinueStmt); overload; override;
    procedure Visit(const AStmt: IBreakStmt); overload; override;
    procedure Visit(const AStmt: IIncludeStmt); overload; override;
    procedure Visit(const AStmt: IRequireStmt); overload; override;
    procedure Visit(const AStmt: IPrintStmt); overload; override;
    procedure Visit(const AStmt: IIfStmt); overload; override;
    procedure Visit(const AStmt: IWhileStmt); overload; override;
    procedure Visit(const AStmt: IForInStmt); overload; override;
    procedure Visit(const AStmt: IForRangeStmt); overload; override;
    procedure Visit(const AStmt: IFunctionCallExpr); overload; override;
    procedure Visit(const AStmt: IMethodCallExpr); overload; override;

    procedure Visit(const AStmt: IProcessTemplateStmt); overload; override;
    procedure Visit(const AStmt: IDefineTemplateStmt); overload; override;
    procedure Visit(const AStmt: IWithStmt); overload; override;
    procedure Visit(const AStmt: ICycleStmt); overload; override;
    procedure Visit(const AStmt: ICompositeStmt); overload; override;
    procedure Visit(const AStmt: IStripStmt); overload; override;
    procedure Visit(const AStmt: IDebugStmt); overload; override;

    procedure Visit(const AStmt: IBlockStmt); overload; override;
    procedure Visit(const AStmt: IExtendsStmt); overload; override;

  end;

function BinOpToStr(const ASymbol: TBinOp): string;
function ForopToStr(const ASymbol: TForOp): string;
function UnaryToStr(const ASymbol: TUnaryOp): string;

implementation

var
  GUnaryStrings: array [TUnaryOp] of string;
  GBinopStrings: array [TBinOp] of string;
  GForOpStrings: array [TForOp] of string;

function UnaryToStr(const ASymbol: TUnaryOp): string;
begin
  exit(GUnaryStrings[ASymbol]);
end;

function BinOpToStr(const ASymbol: TBinOp): string;
begin
  exit(GBinopStrings[ASymbol]);
end;

function ForopToStr(const ASymbol: TForOp): string;

begin
  exit(GForOpStrings[ASymbol]);
end;

{ TPrettyPrintTemplateVisitor }

constructor TPrettyPrintTemplateVisitor.Create();
begin
  inherited Create();
  FIndent := 0;
  FStringBuilder := TStringBuilder.Create;
end;

procedure TPrettyPrintTemplateVisitor.delta(const ADelta: integer);
begin
  inc(FIndent, ADelta);
  if FIndent < 0 then
    FIndent := 0;
  FTab := ''.PadRight(FIndent);
end;

destructor TPrettyPrintTemplateVisitor.Destroy;
begin
  if FStringBuilder <> nil then
    FreeAndNil(FStringBuilder);
  inherited;
end;

procedure TPrettyPrintTemplateVisitor.tab;
begin
  FStringBuilder.append(FTab);
end;

function TPrettyPrintTemplateVisitor.ToString: string;
begin
  exit(FStringBuilder.ToString);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IBreakStmt);
begin
  tab();
  writeln('<%% break %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IIfStmt);
begin
  tab();
  write('<%% if ');
  AcceptVisitor(AStmt.Condition, self);
  writeln(' %%>');
  delta(4);
  AcceptVisitor(AStmt.TrueContainer, self);
  delta(-4);
  if AStmt.FalseContainer <> nil then
  begin
    tab();
    writeln('<%% else %%>');
    delta(4);
    AcceptVisitor(AStmt.FalseContainer, self);
    delta(-4);
  end;
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IContinueStmt);
begin
  tab();
  writeln('<%% continue %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IPrintStmt);
begin
  tab();
  write('<%% print(');
  AcceptVisitor(AStmt.Expr, self);
  writeln(') %%>');
end;

procedure TPrettyPrintTemplateVisitor.Write(const AFMT: string; const args: array of const);
begin
  FStringBuilder.append(format(AFMT, args));
end;

procedure TPrettyPrintTemplateVisitor.Write(const AFMT: string);
begin
  write(AFMT, []);
end;

procedure TPrettyPrintTemplateVisitor.writeln(const AFMT: string);
begin
  writeln(AFMT, []);
end;

procedure TPrettyPrintTemplateVisitor.writeln(const AFMT: string; const args: array of const);
begin
  write(AFMT, args);
  write(#13#10, []);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IWhileStmt);
begin
  tab();
  write('<%% while (');
  AcceptVisitor(AStmt.Condition, self);
  write(')');
  if AStmt.OffsetExpr <> nil then
  begin
    write(' offset ');
    AcceptVisitor(AStmt.OffsetExpr, self);
  end;
  if AStmt.LimitExpr <> nil then
  begin
    write(' limit ');
    AcceptVisitor(AStmt.LimitExpr, self);
  end;
  writeln(' %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  if AStmt.OnBeginContainer <> nil then
  begin
    tab();
    writeln('<%% onbegin %%>');
    delta(4);
    AcceptVisitor(AStmt.OnBeginContainer, self);
    delta(-4);
  end;
  if AStmt.OnEndContainer <> nil then
  begin
    tab();
    writeln('<%% onend %%>');
    delta(4);
    AcceptVisitor(AStmt.OnEndContainer, self);
    delta(-4);
  end;
  if AStmt.OnEmptyContainer <> nil then
  begin
    tab();
    writeln('<%% onempty %%>');
    delta(4);
    AcceptVisitor(AStmt.OnEmptyContainer, self);
    delta(-4);
  end;
  if AStmt.BetweenItemsContainer <> nil then
  begin
    tab();
    writeln('<%% betweenitems %%>');
    delta(4);
    AcceptVisitor(AStmt.BetweenItemsContainer, self);
    delta(-4);
  end;
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IForInStmt);
begin
  tab();
  write('<%% for ' + AStmt.Variable + ' in (');
  AcceptVisitor(AStmt.Expr, self);
  write(')');
  if AStmt.OffsetExpr <> nil then
  begin
    write(' offset ');
    AcceptVisitor(AStmt.OffsetExpr, self);
  end;
  if AStmt.LimitExpr <> nil then
  begin
    write(' limit ');
    AcceptVisitor(AStmt.LimitExpr, self);
  end;
  writeln(' %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  if AStmt.OnBeginContainer <> nil then
  begin
    tab();
    writeln('<%% onbegin %%>');
    delta(4);
    AcceptVisitor(AStmt.OnBeginContainer, self);
    delta(-4);
  end;
  if AStmt.OnEndContainer <> nil then
  begin
    tab();
    writeln('<%% onend %%>');
    delta(4);
    AcceptVisitor(AStmt.OnEndContainer, self);
    delta(-4);
  end;
  if AStmt.OnEmptyContainer <> nil then
  begin
    tab();
    writeln('<%% onempty %%>');
    delta(4);
    AcceptVisitor(AStmt.OnEmptyContainer, self);
    delta(-4);
  end;
  if AStmt.BetweenItemsContainer <> nil then
  begin
    tab();
    writeln('<%% betweenitems %%>');
    delta(4);
    AcceptVisitor(AStmt.BetweenItemsContainer, self);
    delta(-4);
  end;
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IForRangeStmt);
begin
  tab();
  write('<%% for %s := ', [AStmt.Variable]);
  AcceptVisitor(AStmt.LowExpr, self);
  write(' %s ', [ForopToStr(AStmt.ForOp)]);
  AcceptVisitor(AStmt.HighExpr, self);
  write(' ');
  if AStmt.StepExpr <> nil then
  begin
    write(' step ');
    AcceptVisitor(AStmt.StepExpr, self);
  end;
  writeln(' %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  if AStmt.OnBeginContainer <> nil then
  begin
    tab();
    writeln('<%% onbegin %%>');
    delta(4);
    AcceptVisitor(AStmt.OnBeginContainer, self);
    delta(-4);
  end;
  if AStmt.OnEndContainer <> nil then
  begin
    tab();
    writeln('<%% onend %%>');
    delta(4);
    AcceptVisitor(AStmt.OnEndContainer, self);
    delta(-4);
  end;
  if AStmt.OnEmptyContainer <> nil then
  begin
    tab();
    writeln('<%% onempty %%>');
    delta(4);
    AcceptVisitor(AStmt.OnEmptyContainer, self);
    delta(-4);
  end;
  if AStmt.BetweenItemsContainer <> nil then
  begin
    tab();
    writeln('<%% betweenitems %%>');
    delta(4);
    AcceptVisitor(AStmt.BetweenItemsContainer, self);
    delta(-4);
  end;
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IAssignStmt);
begin
  tab();
  write('<%% %s := ', [AStmt.Variable]);
  AcceptVisitor(AStmt.Expr, self);
  writeln(' %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IIncludeStmt);
begin
  tab();
  write('<%% include(');
  AcceptVisitor(AStmt.Expr, self);
  writeln(') %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IVariableDerefExpr);
begin
  AcceptVisitor(AExpr.Variable, self);
  write('[');
  AcceptVisitor(AExpr.DerefExpr, self);
  write(']');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IBinopExpr);
begin
  write('(');
  AcceptVisitor(AExpr.LeftExpr, self);
  write(' %s ', [BinOpToStr(AExpr.BinOp)]);
  AcceptVisitor(AExpr.RightExpr, self);
  write(')');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IUnaryExpr);
begin
  write('%s (', [UnaryToStr(AExpr.UnaryOp)]);
  AcceptVisitor(AExpr.Expr, self);
  write(')');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IVariableExpr);
begin
  write('%s', [AExpr.Variable]);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IValueExpr);
begin
  write('''%s''', [AExpr.Value.ToString.Replace(#9, '''#9''', [rfReplaceAll]).Replace(#13, '', [rfReplaceAll]).Replace(#10, '''#13#10''', [rfReplaceAll])]);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExprList: IExprList);
var
  i: integer;
begin
  write('(');
  for i := 0 to AExprList.Count - 1 do
  begin
    if i > 0 then
      write(',');
    AcceptVisitor(AExprList[i], self);
  end;
  write(')');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IFunctionCallExpr);
begin
  write('%s', [AStmt.FunctionInfo[0].Name]);
  Visit(AStmt.ExprList);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IMethodCallExpr);
begin
  Visit(AStmt.ObjectExpr);
  Write('.%s', [AStmt.Method]);
  Visit(AStmt.ExprList);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IEncodeExpr);
begin
  write('Encode(');
  AcceptVisitor(AExpr.Expr, self);
  write(')');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin
  tab();
  write('<%% template ');
  AcceptVisitor(AStmt.Name, self);
  writeln(' %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IWithStmt);
begin
  tab();
  write('<%% with ');
  AcceptVisitor(AStmt.Expr, self);
  writeln(' %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IRequireStmt);
var
  LIdx: integer;
begin
  tab();
  write('<%% require(');
  for LIdx := 0 to AStmt.ExprList.Count - 1 do
  begin
    if LIdx > 0 then
      write(',');
    AcceptVisitor(AStmt.ExprList.Expr[LIdx], self);
  end;
  writeln('%%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: IArrayExpr);
var
  LIdx: integer;
begin
  write('[');
  for LIdx := 0 to AExpr.ExprList.Count - 1 do
  begin
    if LIdx > 0 then
      write(',');
    AcceptVisitor(AExpr.ExprList.Expr[LIdx], self);
  end;
  write(']');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AExpr: ITernaryExpr);
begin
  AcceptVisitor(AExpr.Condition, self);
  write('?');
  AcceptVisitor(AExpr.TrueExpr, self);
  write(':');
  AcceptVisitor(AExpr.FalseExpr, self);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: ICycleStmt);
var
  LIdx: integer;
begin
  tab();
  write('<%% cycle (');
  for LIdx := 0 to AStmt.List.Count - 1 do
  begin
    if LIdx > 0 then
      write(',');
    AcceptVisitor(AStmt.List.Expr[LIdx], self);
  end;
  writeln(') %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IStripStmt);
begin
  tab();
  write('<%% strip(');
  write(StripDirectionStr[AStmt.Direction]);
  write(',');
  write(AStmt.Action.ToString);
  write(')');
  writeln('%%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: ICompositeStmt);
begin
  tab();
  writeln('<%% composite %%>');
  delta(4);
  AcceptVisitor(AStmt.FirstStmt, self);
  AcceptVisitor(AStmt.SecondStmt, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IDebugStmt);
begin
  // just proxy through
  AcceptVisitor(AStmt.Stmt, self);
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IBlockStmt);
begin
  tab();
  write('<%% block ''');
  AcceptVisitor(AStmt.Name, self);
  writeln('''%%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintTemplateVisitor.Visit(const AStmt: IExtendsStmt);
begin
  tab();
  write('<%% extends ''');
  AcceptVisitor(AStmt.Name, self);
  writeln('''%%>');
  delta(4);
  AcceptVisitor(AStmt.BlockContainer, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

initialization

GUnaryStrings[uoMinus] := '-';
GUnaryStrings[uoNot] := 'not ';

GForOpStrings[TForOp.foTo] := 'to';
GForOpStrings[TForOp.foDownto] := 'downto';
GForOpStrings[TForOp.foIn] := 'in';

GBinopStrings[boAND] := 'and';
GBinopStrings[boOR] := 'or';
GBinopStrings[boPlus] := '+';
GBinopStrings[boMinus] := '-';
GBinopStrings[boDiv] := '/';
GBinopStrings[boMult] := '*';
GBinopStrings[boMod] := 'mod';
GBinopStrings[boEQ] := '=';
GBinopStrings[boNotEQ] := '<>';
GBinopStrings[boLT] := '<';
GBinopStrings[boLTE] := '<=';
GBinopStrings[boGT] := '>';
GBinopStrings[boGTE] := '>=';
GBinopStrings[boIN] := 'in';

end.
