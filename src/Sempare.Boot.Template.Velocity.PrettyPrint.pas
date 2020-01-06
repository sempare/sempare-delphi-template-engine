(*%****************************************************************************
 *  ___                                             ___               _       *
 * / __|  ___   _ __    _ __   __ _   _ _   ___    | _ )  ___   ___  | |_     *
 * \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)   | _ \ / _ \ / _ \ |  _|    *
 * |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|   |___/ \___/ \___/  \__|    *
 *                     |_|                                                    *
 ******************************************************************************
 *                                                                            *
 *                        VELOCITY TEMPLATE ENGINE                            *
 *                                                                            *
 *                                                                            *
 *          https://www.github.com/sempare/sempare.boot.velocity.oss          *
 ******************************************************************************
 *                                                                            *
 * Copyright (c) 2019 Sempare Limited,                                        *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>           *
 *                                                                            *
 * Contact: info@sempare.ltd                                                  *
 *                                                                            *
 * Licensed under the Apache License, Version 2.0 (the "License");            *
 * you may not use this file except in compliance with the License.           *
 * You may obtain a copy of the License at                                    *
 *                                                                            *
 *   http://www.apache.org/licenses/LICENSE-2.0                               *
 *                                                                            *
 * Unless required by applicable law or agreed to in writing, software        *
 * distributed under the License is distributed on an "AS IS" BASIS,          *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   *
 * See the License for the specific language governing permissions and        *
 * limitations under the License.                                             *
 *                                                                            *
 ****************************************************************************%*)
unit Sempare.Boot.Template.Velocity.PrettyPrint;

interface

uses
  System.SysUtils,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.Visitor;

type
  TPrettyPrintVelocityVisitor = class(TBaseVelocityVisitor)
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
  result := GUnaryStrings[ASymbol];
end;

function BinOpToStr(const ASymbol: TBinOp): string;
begin
  result := GBinopStrings[ASymbol];
end;

function ForopToStr(const ASymbol: TForOp): string;

begin
  result := GForOpStrings[ASymbol];
end;

{ TPrettyPrintVelocityVisitor }

constructor TPrettyPrintVelocityVisitor.Create();
begin
  FIndent := 0;
  FStringBuilder := TStringBuilder.Create;
end;

procedure TPrettyPrintVelocityVisitor.delta(const ADelta: integer);
var
  i: integer;
begin
  inc(FIndent, ADelta);
  if FIndent < 0 then
    FIndent := 0;
  FTab := '';
  for i := 1 to FIndent do
    FTab := FTab + ' ';
end;

destructor TPrettyPrintVelocityVisitor.Destroy;
begin
  FStringBuilder.Free;
  inherited;
end;

procedure TPrettyPrintVelocityVisitor.tab;
begin
  FStringBuilder.append(FTab);
end;

function TPrettyPrintVelocityVisitor.ToString: string;
begin
  result := FStringBuilder.ToString;
end;


procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IBreakStmt);
begin
  tab();
  writeln('<%% break %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IIfStmt);
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

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IContinueStmt);
begin
  tab();
  writeln('<%% continue %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IPrintStmt);
begin
  tab();
  write('<%% print(');
  AcceptVisitor(AStmt.Expr, self);
  writeln(') %%>');
end;

procedure TPrettyPrintVelocityVisitor.Write(const AFMT: string; const args: array of const);
var
  s: string;
begin
  s := format(AFMT, args);
  FStringBuilder.append(s);
end;

procedure TPrettyPrintVelocityVisitor.Write(const AFMT: string);
begin
  write(AFMT, []);
end;

procedure TPrettyPrintVelocityVisitor.writeln(const AFMT: string);
begin
  writeln(AFMT, []);
end;

procedure TPrettyPrintVelocityVisitor.writeln(const AFMT: string; const args: array of const);
begin
  write(AFMT, args);
  write(#13#10, []);
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IWhileStmt);
begin
  tab();
  write('<%% while (');
  AcceptVisitor(AStmt.Condition, self);
  writeln(') %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IForInStmt);
begin
  tab();
  write('<%% for ' + AStmt.Variable + ' in (');
  AcceptVisitor(AStmt.Expr, self);
  writeln(') %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IForRangeStmt);
begin
  tab();
  write('<%% for %s := (', [AStmt.Variable]);
  AcceptVisitor(AStmt.LowExpr, self);
  write(') %s (', [ForopToStr(AStmt.ForOp)]);
  AcceptVisitor(AStmt.HighExpr, self);
  writeln(') %%>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<%% end %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IAssignStmt);
begin
  tab();
  write('<%% %s := ', [AStmt.Variable]);
  AcceptVisitor(AStmt.Expr, self);
  writeln(' %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IIncludeStmt);
begin
  tab();
  write('<%% include(');
  AcceptVisitor(AStmt.Expr, self);
  writeln(') %%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IVariableDerefExpr);
begin
  AcceptVisitor(AExpr.Variable, self);
  write('[');
  AcceptVisitor(AExpr.DerefExpr, self);
  write(']');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IBinopExpr);
begin
  write('(');
  AcceptVisitor(AExpr.LeftExpr, self);
  write(' %s ', [BinOpToStr(AExpr.BinOp)]);
  AcceptVisitor(AExpr.RightExpr, self);
  write(')');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IUnaryExpr);
begin
  write('%s (', [UnaryToStr(AExpr.UnaryOp)]);
  AcceptVisitor(AExpr.Condition, self);
  write(')');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IVariableExpr);
begin
  write('%s', [AExpr.Variable]);
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IValueExpr);
begin
  write('''%s''', [AExpr.Value.ToString.Replace(#9, '''#9''', [rfReplaceAll]).Replace(#13, '', [rfReplaceAll]).Replace(#10, '''#13#10''', [rfReplaceAll])]);
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExprList: IExprList);
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

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IFunctionCallExpr);
begin
  write('%s', [AStmt.FunctionInfo[0].Name]);
  Visit(AStmt.ExprList);
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IMethodCallExpr);
begin
  Visit(AStmt.ObjectExpr);
  Write('.%s', [AStmt.Method]);
  Visit(AStmt.ExprList);
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IEncodeExpr);
begin
  write('Encode(');
  AcceptVisitor(AExpr.Expr, self);
  write(')');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin
  tab();
  write('<% define ');
  AcceptVisitor(AStmt.Name, self);
  writeln(' %>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<% end %>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IWithStmt);
begin
  tab();
  write('<% with ');
  AcceptVisitor(AStmt.Expr, self);
  writeln(' %>');
  delta(4);
  AcceptVisitor(AStmt.Container, self);
  delta(-4);
  tab();
  writeln('<% end %>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AStmt: IRequireStmt);
var
  i: integer;
begin
  tab();
  write('<% require(');
  for i := 0 to AStmt.ExprList.Count - 1 do
  begin
    if i > 0 then
      write(',');
    AcceptVisitor(AStmt.ExprList.Expr[i], self);
  end;
  writeln('%>');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: IArrayExpr);
var
  i: integer;
begin
  write('[');
  for i := 0 to AExpr.ExprList.Count - 1 do
  begin
    if i > 0 then
      write(',');
    AcceptVisitor(AExpr.ExprList.Expr[i], self);
  end;
  write(']');
end;

procedure TPrettyPrintVelocityVisitor.Visit(const AExpr: ITernaryExpr);
begin
  AcceptVisitor(AExpr.Condition, self);
  write('?');
  AcceptVisitor(AExpr.TrueExpr, self);
  write(':');
  AcceptVisitor(AExpr.FalseExpr, self);
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
