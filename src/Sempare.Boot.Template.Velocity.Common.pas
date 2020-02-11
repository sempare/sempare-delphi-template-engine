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

uses
  System.SysUtils,
  System.Generics.Collections,
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

  IVelocityVariables = interface
    ['{8C2D166D-2AC1-47AB-985E-2CFD5D44271D}']
    function GetItem(const AKey: string): TVelocityValue;
    procedure SetItem(const AKey: string; const Value: TVelocityValue);
    function TryGetItem(const AKey: string; out AValue: TVelocityValue): boolean;
    function GetEnumerator: TDictionary<string, TValue>.TPairEnumerator;
    procedure Remove(const AKey: string);
    procedure Clear;
    function GetCount: integer;
    property Count: integer read GetCount;
    property Items[const AKey: string]: TVelocityValue read GetItem write SetItem; default;
  end;

  TVelocityVariables = class(TInterfacedObject, IVelocityVariables)
  private
    FVariables: TDictionary<string, TVelocityValue>;
  public
    constructor Create();
    destructor Destroy; override;
    function GetEnumerator: TDictionary<string, TValue>.TPairEnumerator;
    function ContainsKey(const AKey: string): boolean;
    function TryGetItem(const AKey: string; out AValue: TVelocityValue): boolean;
    procedure Remove(const AKey: string);
    procedure Clear;
    function GetItem(const AKey: string): TVelocityValue;
    procedure SetItem(const AKey: string; const Value: TVelocityValue);
    function GetCount: integer;
    property Variables[const AKey: string]: TVelocityValue read GetItem write SetItem; default;
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

{ TVelocityVariables }

procedure TVelocityVariables.Clear;
begin
  FVariables.Clear;
end;

function TVelocityVariables.ContainsKey(const AKey: string): boolean;
begin
  result := FVariables.ContainsKey(AKey);
end;

constructor TVelocityVariables.Create;
begin
  FVariables := TDictionary<string, TVelocityValue>.Create;
end;

destructor TVelocityVariables.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TVelocityVariables.GetCount: integer;
begin
  result := FVariables.Count;
end;

function TVelocityVariables.GetEnumerator: TDictionary<string, TValue>.TPairEnumerator;
begin
  result := FVariables.GetEnumerator;
end;

function TVelocityVariables.GetItem(const AKey: string): TVelocityValue;
begin
  result := FVariables[AKey];
end;

procedure TVelocityVariables.Remove(const AKey: string);
begin
  FVariables.Remove(AKey);
end;

procedure TVelocityVariables.SetItem(const AKey: string; const Value: TVelocityValue);
begin
  FVariables.AddOrSetValue(AKey, Value);
end;

function TVelocityVariables.TryGetItem(const AKey: string; out AValue: TVelocityValue): boolean;
begin
  result := FVariables.TryGetValue(AKey, AValue);
end;

end.
