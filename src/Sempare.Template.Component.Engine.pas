(*%*********************************************************************************
 *                 ___                                                             *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                     *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                    *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                    *
 *                                    |_|                                          *
 ***********************************************************************************
 *                                                                                 *
 *                        Sempare Templating Engine                                *
 *                                                                                 *
 *                                                                                 *
 *         https://github.com/sempare/sempare-delphi-template-engine               *
 ***********************************************************************************
 *                                                                                 *
 * Copyright (c) 2020 Sempare Limited                                              *
 *                                                                                 *
 * Contact: info@sempare.ltd                                                       *
 *                                                                                 *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License            *
 * You may not use this file except in compliance with one of these Licenses.      *
 * You may obtain a copy of the Licenses at                                        *
 *                                                                                 *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                    *
 * https://github.com/sempare/sempare.boot.velocity.oss/docs/commercial.license.md *
 *                                                                                 *
 * Unless required by applicable law or agreed to in writing, software             *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,              *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.        *
 * See the License for the specific language governing permissions and             *
 * limitations under the License.                                                  *
 *                                                                                 *
 ********************************************************************************%*)
unit Sempare.Template.Component.Engine;

interface

{$R 'TSempareTemplateEngine.dcr' }

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  Sempare.Template.Common,
  Sempare.Template;

type

  [ObservableMember('Text')]
  TSempareBootVelocityEngine = class(TComponent)
  private
    FContext: ISempareBootVelocityContext;
    FTemplate: ISempareBootVelocityTemplate;
    FVariables: IVelocityVariables;
    FText: string;
    FEnabled: boolean;
    FForceTemplateEnable: boolean;
    procedure SetContext(const Value: ISempareBootVelocityContext);
    procedure SetTemplate(const Value: ISempareBootVelocityTemplate);
    procedure SetVariables(const Value: IVelocityVariables);
    procedure Evaluate;
    function GetText: string;
    procedure SetEnabled(const Value: boolean);
    function GetVariable(const AKey: string): TVelocityValue;
    procedure SetVariable(const AKey: string; const Value: TVelocityValue);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Variable[const AKey: string]: TVelocityValue read GetVariable write SetVariable; default;
  published
    property Text: string read GetText;
    property Enabled: boolean read FEnabled write SetEnabled;
    property ForceTemplateEnable: boolean read FForceTemplateEnable write FForceTemplateEnable;
    property Context: TVempareBootVelocityContext read FContext write SetContext;
    property Template: ISempareBootVelocityTemplate read FTemplate write SetTemplate;
    property Variables: IVelocityVariables read FVariables write SetVariables;
  end;

implementation

uses
  Sempare.Template.Context;

{ TSempareBootVelocityEngine }

procedure TSempareBootVelocityEngine.Clear;
begin
  FText := '';
end;

constructor TSempareBootVelocityEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariables := TVelocityVariables.Create;
end;

destructor TSempareBootVelocityEngine.Destroy;
begin
  if FVariables <> nil then
    FVariables.Free;
  inherited;
end;

procedure TSempareBootVelocityEngine.Evaluate;
var
  ctx: IVelocityContext;
begin
  if not FEnabled then
    exit;
  if FTemplate = nil then
    exit;
  if FContext <> nil then
    ctx := FContext.Context
  else
    ctx := Sempare.Template.Template.Context();
  FText := Sempare.Template.Template.Eval(ctx, FTemplate.Template, FVariables);
end;

function TSempareBootVelocityEngine.GetText: string;
begin
  result := FText;
end;

function TSempareBootVelocityEngine.GetVariable(const AKey: string): TVelocityValue;
begin
  result := FVariables[AKey];
end;

procedure TSempareBootVelocityEngine.SetContext(const Value: TSempareBootVelocityContext);
begin
  FContext := Value;
  Evaluate;
end;

procedure TSempareBootVelocityEngine.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  if (FTemplate <> nil) and (not FTemplate.Enabled or (Value and FForceTemplateEnable)) then
    FTemplate.Enabled := true;
  if Value then
    Evaluate;
end;

procedure TSempareBootVelocityEngine.SetTemplate(const Value: TSempareBootVelocityTemplate);
begin
  FTemplate := Value;
  Evaluate;
end;

procedure TSempareBootVelocityEngine.SetVariable(const AKey: string;   Value: IVelocityValue);
begin
  FVariables.SetItem(AKey, Value);
end;

procedure TSempareBootVelocityEngine.SetVariables(const Value: IVelocityVariables);
begin
  FVariables := Value;
  Evaluate;
end;

end.
