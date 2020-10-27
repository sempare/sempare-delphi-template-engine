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
unit Sempare.Template.Component.Context;

interface

{$R 'TSempareTemplateContext.dcr' }

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Template.Common,
  Sempare.Template.Context,
  Sempare.Template;

type
  TSempareBootVelocityContext = class(TComponent)
  private
    FContext: IVelocityContext;

    function GetContext: IVelocityContext;
    function GetEncoding: TEncoding;
    function GetMaxRunTimeMs: integer;
    function GetNewLine: string;
    function GetScriptEndToken: string;
    function GetScriptStartToken: string;
    procedure SetEncoding(const Value: TEncoding);
    procedure SetMaxRunTimeMs(const Value: integer);
    procedure SetNewLine(const Value: string);
    procedure SetScriptEndToken(const Value: string);
    procedure SetScriptStartToken(const Value: string);
    function GetOptions: TVelocityEvaluationOptions;
    procedure SetOptions(const Value: TVelocityEvaluationOptions);
    function GetFunctions: IVelocityFunctions;
    function GetTemplate(const AName: string): IVelocityTemplate;
    function GetTemplateResolver: TVelocityTemplateResolver;
    function GetVariable(const AKey: string): TVelocityValue;
    function GetVariableEncoder: TVelocityEncodeFunction;
    procedure SetFunctions(const Value: IVelocityFunctions);
    procedure SetTemplate(const AName: string; const Value: IVelocityTemplate);
    procedure SetTemplateResolver(const Value: TVelocityTemplateResolver);
    procedure SetVariable(const AKey: string; const Value: TVelocityValue);
    procedure SetVariableEncoder(const Value: TVelocityEncodeFunction);
    function GetVariables: TVelocityVariables;
    procedure SetVariables(const Value: TVelocityVariables);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Context: IVelocityContext read GetContext;

    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property NewLine: string read GetNewLine write SetNewLine;
    property Functions: IVelocityFunctions read GetFunctions write SetFunctions;
    property TemplateResolver: TVelocityTemplateResolver read GetTemplateResolver write SetTemplateResolver;
    property VariableEncoder: TVelocityEncodeFunction read GetVariableEncoder write SetVariableEncoder;
    property Variable[const AKey: string]: TVelocityValue read GetVariable write SetVariable; default;
    property Template[const AName: string]: IVelocityTemplate read GetTemplate write SetTemplate;

  published
    property Options: TVelocityEvaluationOptions read GetOptions write SetOptions;

    property MaxRunTimeMs: integer read GetMaxRunTimeMs write SetMaxRunTimeMs;
    property StartToken: string read GetScriptStartToken write SetScriptStartToken;
    property EndToken: string read GetScriptEndToken write SetScriptEndToken;
    property Variables: TVelocityVariables read GetVariables write SetVariables;
  end;

implementation

{ TSempareBootVelocityContext }

constructor TSempareBootVelocityContext.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContext := Sempare.Template.Template.Context();
end;

destructor TSempareBootVelocityContext.Destroy;
begin
  FContext := nil;
  inherited;
end;

function TSempareBootVelocityContext.GetContext: IVelocityContext;
begin
  result := FContext;
end;

function TSempareBootVelocityContext.GetEncoding: TEncoding;
begin
  result := FContext.Encoding;
end;

function TSempareBootVelocityContext.GetFunctions: IVelocityFunctions;
begin
  result := FContext.Functions;
end;

function TSempareBootVelocityContext.GetMaxRunTimeMs: integer;
begin
  result := FContext.MaxRunTimeMs;
end;

function TSempareBootVelocityContext.GetNewLine: string;
begin
  result := FContext.NewLine;
end;

function TSempareBootVelocityContext.GetOptions: TVelocityEvaluationOptions;
begin
  result := FContext.Options;
end;

function TSempareBootVelocityContext.GetScriptEndToken: string;
begin
  result := FContext.EndToken;
end;

function TSempareBootVelocityContext.GetScriptStartToken: string;
begin
  result := FContext.StartToken;
end;

function TSempareBootVelocityContext.GetTemplate(const AName: string): IVelocityTemplate;
begin
  result := FContext.Template[AName];
end;

function TSempareBootVelocityContext.GetTemplateResolver: TVelocityTemplateResolver;
begin
  result := FContext.TemplateResolver;
end;

function TSempareBootVelocityContext.GetVariable(const AKey: string): TVelocityValue;
begin
  result := FContext.Variable[AKey];
end;

function TSempareBootVelocityContext.GetVariableEncoder: TVelocityEncodeFunction;
begin
  result := FContext.VariableEncoder;
end;

function TSempareBootVelocityContext.GetVariables: TVelocityVariables;
begin
  result := TVelocityVariables(FContext.Variables);
end;

procedure TSempareBootVelocityContext.SetEncoding(const Value: TEncoding);
begin
  FContext.Encoding := Value;
end;

procedure TSempareBootVelocityContext.SetFunctions(const Value: IVelocityFunctions);
begin
  // do nothing
end;

procedure TSempareBootVelocityContext.SetMaxRunTimeMs(const Value: integer);
begin
  FContext.MaxRunTimeMs := Value;
end;

procedure TSempareBootVelocityContext.SetNewLine(const Value: string);
begin
  FContext.NewLine := Value;
end;

procedure TSempareBootVelocityContext.SetOptions(const Value: TVelocityEvaluationOptions);
begin
  FContext.Options := Value;
end;

procedure TSempareBootVelocityContext.SetScriptEndToken(const Value: string);
begin
  FContext.EndToken := Value;
end;

procedure TSempareBootVelocityContext.SetScriptStartToken(const Value: string);
begin
  FContext.StartToken := Value;
end;

procedure TSempareBootVelocityContext.SetTemplate(const AName: string; const Value: IVelocityTemplate);
begin
  FContext.Template[AName] := Value;
end;

procedure TSempareBootVelocityContext.SetTemplateResolver(const Value: TVelocityTemplateResolver);
begin
  FContext.TemplateResolver := Value;
end;

procedure TSempareBootVelocityContext.SetVariable(const AKey: string; const Value: TVelocityValue);
begin
  FContext.Variable[AKey] := Value;
end;

procedure TSempareBootVelocityContext.SetVariableEncoder(const Value: TVelocityEncodeFunction);
begin
  FContext.VariableEncoder := Value;
end;

procedure TSempareBootVelocityContext.SetVariables(const Value: TVelocityVariables);
begin
  // do nothing
end;

end.
