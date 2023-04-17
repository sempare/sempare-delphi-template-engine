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
unit Sempare.Template.Common;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  Sempare.Template.AST;

type
  TTemplateEncodeFunction = reference to function(const AArg: string): string;

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

  ETemplateEvaluationError = class(ETemplate, IPosition)
  private
    FPosition: IPosition;
  public
    constructor Create(const APosition: IPosition; const AMessage: string);
    property Position: IPosition read FPosition write FPosition implements IPosition;
  end;

  ITemplateVariables = interface
    ['{8C2D166D-2AC1-47AB-985E-2CFD5D44271D}']
    function GetItem(const AKey: string): TTemplateValue;
    procedure SetItem(const AKey: string; const Value: TTemplateValue);
    function TryGetItem(const AKey: string; out AValue: TTemplateValue): boolean;
    function GetEnumerator: TDictionary<string, TValue>.TPairEnumerator;
    procedure Remove(const AKey: string);
    procedure Clear;
    function GetCount: integer;
    property Count: integer read GetCount;
    property Items[const AKey: string]: TTemplateValue read GetItem write SetItem; default;
  end;

  TTemplateVariables = class(TInterfacedObject, ITemplateVariables)
  private
    FVariables: TDictionary<string, TTemplateValue>;
  public
    constructor Create();
    destructor Destroy; override;
    function GetEnumerator: TDictionary<string, TValue>.TPairEnumerator;
    function ContainsKey(const AKey: string): boolean;
    function TryGetItem(const AKey: string; out AValue: TTemplateValue): boolean;
    procedure Remove(const AKey: string);
    procedure Clear;
    function GetItem(const AKey: string): TTemplateValue;
    procedure SetItem(const AKey: string; const Value: TTemplateValue);
    function GetCount: integer;
    property Variables[const AKey: string]: TTemplateValue read GetItem write SetItem; default;
  end;

procedure AcceptVisitor(const ATemplate: ITemplate; const AVisitor: ITemplateVisitor); inline; overload;
procedure AcceptVisitor(const AExpr: IExpr; const AVisitor: ITemplateVisitor); inline; overload;
procedure AcceptVisitor(const AStmt: IStmt; const AVisitor: ITemplateVisitor); inline; overload;
procedure AcceptVisitor(const AExpr: IExprList; const AVisitor: ITemplateVisitor); inline; overload;

function Position(const APositional: IPosition): string; inline; overload;

procedure RaiseError(const APositional: IPosition; const AFormat: string; const AArgs: array of const); overload;
procedure RaiseError(const APositional: IPosition; const AFormat: string); overload;
procedure RaiseErrorRes(const APositional: IPosition; const ResStringRec: PResStringRec; const AArgs: array of const); overload;
procedure RaiseErrorRes(const APositional: IPosition; const ResStringRec: PResStringRec); overload;

implementation

uses
  Sempare.Template;

procedure AcceptVisitor(const ATemplate: ITemplate; const AVisitor: ITemplateVisitor); overload;
begin
  if not assigned(ATemplate) then
    exit;
  ATemplate.Accept(AVisitor);
end;

procedure AcceptVisitor(const AExpr: IExpr; const AVisitor: ITemplateVisitor);
begin
  if not assigned(AExpr) then
    exit;
  AExpr.Accept(AVisitor);
end;

procedure AcceptVisitor(const AStmt: IStmt; const AVisitor: ITemplateVisitor);
begin
  if not assigned(AStmt) then
    exit;
  AStmt.Accept(AVisitor);
end;

procedure AcceptVisitor(const AExpr: IExprList; const AVisitor: ITemplateVisitor);
var
  i: integer;
begin
  if not assigned(AExpr) then
    exit;
  for i := 0 to AExpr.Count - 1 do
  begin
    AExpr[i].Accept(AVisitor);
  end;
end;

function Position(const APositional: IPosition): string; overload;
var
  LName: string;
begin
  if APositional = nil then
    exit('');
  if APositional.FileName = '' then
    LName := ''
  else
    LName := APositional.FileName + ':';
  exit(format('%s (Line %d, Column %d) ', [LName, APositional.Line, APositional.Pos]));
end;

procedure RaiseError(const APositional: IPosition; const AFormat: string; const AArgs: array of const); overload;
begin
  raise ETemplateEvaluationError.Create(APositional, Position(APositional) + format(AFormat, AArgs));
end;

procedure RaiseError(const APositional: IPosition; const AFormat: string); overload;
begin
  RaiseError(APositional, AFormat, []);
end;

procedure RaiseErrorRes(const APositional: IPosition; const ResStringRec: PResStringRec; const AArgs: array of const);
begin
  raise ETemplateEvaluationError.Create(APositional, Position(APositional) + format(LoadResString(ResStringRec), AArgs));
end;

procedure RaiseErrorRes(const APositional: IPosition; const ResStringRec: PResStringRec);
begin
  RaiseErrorRes(APositional, ResStringRec, []);
end;

{ ETemplateEvaluationError }

constructor ETemplateEvaluationError.Create(const APosition: IPosition; const AMessage: string);
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
  exit(FFilename);
end;

function TPosition.GetLine: integer;
begin
  exit(FLine);
end;

function TPosition.GetPos: integer;
begin
  exit(FPos);
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

{ TTemplateVariables }

procedure TTemplateVariables.Clear;
begin
  FVariables.Clear;
  self['SEMPARE_TEMPLATE_ENGINE_VERSION'] := Template.Version;
  self['CR'] := #13;
  self['NL'] := #10;
  self['CRNL'] := #13#10;
  self['TAB'] := #9;
end;

function TTemplateVariables.ContainsKey(const AKey: string): boolean;
begin
  exit(FVariables.ContainsKey(AKey));
end;

constructor TTemplateVariables.Create;
begin
  FVariables := TDictionary<string, TTemplateValue>.Create;
  Clear;
end;

destructor TTemplateVariables.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TTemplateVariables.GetCount: integer;
begin
  exit(FVariables.Count);
end;

function TTemplateVariables.GetEnumerator: TDictionary<string, TValue>.TPairEnumerator;
begin
  exit(FVariables.GetEnumerator);
end;

function TTemplateVariables.GetItem(const AKey: string): TTemplateValue;
begin
  exit(FVariables[AKey]);
end;

procedure TTemplateVariables.Remove(const AKey: string);
begin
  FVariables.Remove(AKey);
end;

procedure TTemplateVariables.SetItem(const AKey: string; const Value: TTemplateValue);
begin
  FVariables.AddOrSetValue(AKey, Value);
end;

function TTemplateVariables.TryGetItem(const AKey: string; out AValue: TTemplateValue): boolean;
begin
  exit(FVariables.TryGetValue(AKey, AValue));
end;

end.
