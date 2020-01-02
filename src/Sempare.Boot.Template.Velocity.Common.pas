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
unit Sempare.Boot.Template.Velocity.Common;

interface

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses
  System.SysUtils,
  System.Rtti,
  Sempare.Boot.Template.Velocity.AST;

type
  TVelocityEncodeFunction = reference to function(const AArg: string): string;

  TPosition = class(TInterfacedObject, IPosition)
  private
    FFilename: string;
    FLine: integer;
    FPos: integer;
    function GetFilename: string;
    function GetLine: integer;
    function GetPos: integer;
    procedure SetFilename(const AFilename: string);
    procedure SetLine(const Aline: integer);
    procedure SetPos(const Apos: integer);
  public
    constructor Create(const AFilename: string; const Aline, Apos: integer);
  end;

  EVelocityEvaluationError = class(Exception, IPosition)
  private
    FPosition: IPosition;
  public
    constructor Create(const APosition: IPosition; const AMessage: string);
    property Position: IPosition read FPosition write FPosition implements IPosition;
  end;

function AsVisitorHost(const ATemplate: IVelocityTemplate): IVelocityVisitorHost; inline; overload;
function AsVisitorHost(const AExpr: IExpr): IVelocityVisitorHost; inline; overload;
function AsVisitorHost(const AStmt: IStmt): IVelocityVisitorHost; inline; overload;

procedure AcceptVisitor(const ATemplate: IVelocityTemplate; const AVisitor: IVelocityVisitor); overload;
procedure AcceptVisitor(const AExpr: IExpr; const AVisitor: IVelocityVisitor); overload;
procedure AcceptVisitor(const AStmt: IStmt; const AVisitor: IVelocityVisitor); overload;

function Position(const AStmt: IStmt): IPosition; inline; overload;
function Position(const AExpr: IExpr): IPosition; inline; overload;
function Position(const APositional: IPosition): string; inline; overload;

procedure RaiseError(const APositional: IPosition; const AFormat: string; const AArgs: array of const); overload;
procedure RaiseError(const APositional: IPosition; const AFormat: string); overload;



implementation

//uses
//  System.JSON,
//  Sempare.Boot.Template.Velocity.Rtti;


function AsVisitorHost(const ATemplate: IVelocityTemplate): IVelocityVisitorHost; overload;
begin
  ATemplate.QueryInterface(IVelocityVisitorHost, result);
end;

function AsVisitorHost(const AExpr: IExpr): IVelocityVisitorHost;
begin
  AExpr.QueryInterface(IVelocityVisitorHost, result);
end;

function AsVisitorHost(const AStmt: IStmt): IVelocityVisitorHost;
begin
  AStmt.QueryInterface(IVelocityVisitorHost, result);
end;

procedure AcceptVisitor(const ATemplate: IVelocityTemplate; const AVisitor: IVelocityVisitor); overload;
begin
  AsVisitorHost(ATemplate).Accept(AVisitor);
end;

procedure AcceptVisitor(const AExpr: IExpr; const AVisitor: IVelocityVisitor);
begin
  AsVisitorHost(AExpr).Accept(AVisitor);
end;

procedure AcceptVisitor(const AStmt: IStmt; const AVisitor: IVelocityVisitor);
begin
  AsVisitorHost(AStmt).Accept(AVisitor);
end;

function Position(const AStmt: IStmt): IPosition; overload;
var
  symbol: IPositional;
begin
  AStmt.QueryInterface(IPositional, symbol);
  result := symbol.Position;
end;

function Position(const AExpr: IExpr): IPosition; overload;
var
  symbol: IPositional;
begin
  AExpr.QueryInterface(IPositional, symbol);
  result := symbol.Position;
end;

function Position(const APositional: IPosition): string; overload;
var
  Name: string;
begin
  if APositional = nil then
    exit('');
  if APositional.FileName = '' then
    name := ''
  else
    name := APositional.FileName + ':';
  result := format('%s%d[%d]', [name, APositional.Line, APositional.Pos]);
end;

procedure RaiseError(const APositional: IPosition; const AFormat: string; const AArgs: array of const); overload;
begin
  raise EVelocityEvaluationError.Create(APositional, Position(APositional) + format(AFormat, AArgs));
end;

procedure RaiseError(const APositional: IPosition; const AFormat: string); overload;
begin
  RaiseError(APositional, AFormat, []);
end;


{ EVelocityEvaluationError }

constructor EVelocityEvaluationError.Create(const APosition: IPosition; const AMessage: string);
begin
  inherited Create(AMessage);
  FPosition := APosition;
end;

{ TPosition }

constructor TPosition.Create(const AFilename: string; const Aline, Apos: integer);
begin
  FFilename := AFilename;
  FLine := Aline;
  FPos := Apos;
end;

function TPosition.GetFilename: string;
begin
  result := FFilename;
end;

function TPosition.GetLine: integer;
begin
  result := FLine;
end;

function TPosition.GetPos: integer;
begin
  result := FPos;
end;

procedure TPosition.SetFilename(const AFilename: string);
begin
  FFilename := AFilename;
end;

procedure TPosition.SetLine(const Aline: integer);
begin
  FLine := Aline;
end;

procedure TPosition.SetPos(const Apos: integer);
begin
  FPos := Apos;
end;

end.
