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

uses
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Common;

type
  TBaseVelocityVisitor = class(TInterfacedObject, IVelocityVisitor)
  public
    procedure Visit(AContainer: IVelocityTemplate); overload; virtual;
    procedure Visit(AContainer: IVelocityVisitorHost); overload; virtual;

    procedure Visit(AExpr: IExpr); overload; virtual;
    procedure Visit(AExpr: IBinopExpr); overload; virtual;
    procedure Visit(AExpr: IUnaryExpr); overload; virtual;
    procedure Visit(AExpr: IVariableExpr); overload; virtual;
    procedure Visit(AExpr: IVariableDerefExpr); overload; virtual;
    procedure Visit(AExpr: IValueExpr); overload; virtual;
    procedure Visit(AExprList: IExprList); overload; virtual;
    procedure Visit(AExpr: ITernaryExpr); overload; virtual;
    procedure Visit(AExpr: IArrayExpr); overload; virtual;

    procedure Visit(AStmt: IStmt); overload; virtual;
    procedure Visit(AStmt: IAssignStmt); overload; virtual;
    procedure Visit(AStmt: IContinueStmt); overload; virtual;
    procedure Visit(AStmt: IElseStmt); overload; virtual;
    procedure Visit(AStmt: IBreakStmt); overload; virtual;
    procedure Visit(AStmt: IEndStmt); overload; virtual;
    procedure Visit(AStmt: IIncludeStmt); overload; virtual;
    procedure Visit(AStmt: IRequireStmt); overload; virtual;
    procedure Visit(AStmt: IEncodeExpr); overload; virtual;
    procedure Visit(AStmt: IPrintStmt); overload; virtual;
    procedure Visit(AStmt: IIfStmt); overload; virtual;
    procedure Visit(AStmt: IWhileStmt); overload; virtual;
    procedure Visit(AStmt: IForInStmt); overload; virtual;
    procedure Visit(AStmt: IForRangeStmt); overload; virtual;
    procedure Visit(AStmt: IFunctionCallExpr); overload; virtual;
    procedure Visit(AStmt: IMethodCallExpr); overload; virtual;
    procedure Visit(AStmt: IProcessTemplateStmt); overload; virtual;
    procedure Visit(AStmt: IDefineTemplateStmt); overload; virtual;
    procedure Visit(AStmt: IWithStmt); overload; virtual;

  end;

implementation

uses
  System.SysUtils;

{ TBaseVelocityVisitor }

procedure TBaseVelocityVisitor.Visit(AExpr: IVariableExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExpr: IValueExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExprList: IExprList);
var
  i: integer;
begin
  for i := 0 to AExprList.Count - 1 do
    AcceptVisitor(AExprList[i], self);
end;

procedure TBaseVelocityVisitor.Visit(AStmt: IStmt);
begin
  raise Exception.Create('Statment not supported in visitor');
end;

procedure TBaseVelocityVisitor.Visit(AContainer: IVelocityVisitorHost);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExpr: IExpr);
begin
  raise Exception.Create('Expression not supported in visitor');
end;

procedure TBaseVelocityVisitor.Visit(AExpr: IBinopExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExpr: IUnaryExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IIfStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IWhileStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IForInStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IForRangeStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AContainer: IVelocityTemplate);
var
  i: integer;
begin
  for i := 0 to AContainer.Count - 1 do
    AContainer.Items[i].Accept(self);
end;

procedure TBaseVelocityVisitor.Visit(AStmt: IAssignStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IIncludeStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExpr: IVariableDerefExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IContinueStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IBreakStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IEndStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IPrintStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IFunctionCallExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IElseStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IMethodCallExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IEncodeExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IProcessTemplateStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IDefineTemplateStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IWithStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AStmt: IRequireStmt);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExpr: IArrayExpr);
begin

end;

procedure TBaseVelocityVisitor.Visit(AExpr: ITernaryExpr);
begin

end;

end.
