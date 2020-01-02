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
unit Sempare.Boot.Template.Velocity.Visitor;

interface

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Common;

type
  TBaseVelocityVisitor = class(TInterfacedObject, IVelocityVisitor)
  public
    procedure Visit(const AContainer: IVelocityTemplate); overload; virtual;
    procedure Visit(const AContainer: IVelocityVisitorHost); overload; virtual;

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

  end;

implementation

uses
  System.SysUtils;

{ TBaseVelocityVisitor }

procedure TBaseVelocityVisitor.Visit(const AExpr: IVariableExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExpr: IValueExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExprList: IExprList);
var
  i: integer;
begin
  for i := 0 to AExprList.Count - 1 do
    AcceptVisitor(AExprList[i], self);
end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IStmt);
begin
  raise Exception.Create('Statment not supported in visitor');
end;

procedure TBaseVelocityVisitor.Visit(const AContainer: IVelocityVisitorHost);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExpr: IExpr);
begin
  raise Exception.Create('Expression not supported in visitor');
end;

procedure TBaseVelocityVisitor.Visit(const AExpr: IBinopExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExpr: IUnaryExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IIfStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IWhileStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IForInStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IForRangeStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AContainer: IVelocityTemplate);
var
  i: integer;
begin
  for i := 0 to AContainer.Count - 1 do
    AContainer.Items[i].Accept(self);
end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IAssignStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IIncludeStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExpr: IVariableDerefExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IContinueStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IBreakStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IEndStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IPrintStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IFunctionCallExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IElseStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IMethodCallExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IEncodeExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IProcessTemplateStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IDefineTemplateStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AStmt: IWithStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExpr: IArrayExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(const AExpr: ITernaryExpr);
begin

end;

end.
