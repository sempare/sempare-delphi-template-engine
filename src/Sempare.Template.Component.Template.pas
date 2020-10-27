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
unit Sempare.Template.Component.Template;

interface

{$R 'TSempareTemplate.dcr' }

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  Sempare.Template.Context,
  Sempare.Template.Component.Context,
  Sempare.Template;

type

  [ObservableMember('TemplateText')]
  TSempareBootVelocityTemplate = class(TComponent)
  private
    FEnabled: boolean;
    FContext: TSempareBootVelocityContext;
    FTemplateText: string;
    FTemplate: IVelocityTemplate;
    FLock: TCriticalSection;
    function GetTemplate: IVelocityTemplate;
    procedure SetTemplateText(const Value: string);
    procedure SetTemplate(const AIfNull: boolean);
    procedure SetEnabled(const Value: boolean);
    procedure SetContext(const Value: TSempareBootVelocityContext);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Template: IVelocityTemplate read GetTemplate;
  published
    property Context: TSempareBootVelocityContext read FContext write SetContext;
    property TemplateText: string read FTemplateText write SetTemplateText;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

implementation

{ TSempareBootVelocityTemplate }

constructor TSempareBootVelocityTemplate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create();
end;

destructor TSempareBootVelocityTemplate.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TSempareBootVelocityTemplate.GetTemplate: IVelocityTemplate;
begin
  SetTemplate(true);
  result := FTemplate;
end;

procedure TSempareBootVelocityTemplate.SetContext(const Value: TSempareBootVelocityContext);
begin
  FContext := Value;
end;

procedure TSempareBootVelocityTemplate.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  if Value then
    SetTemplate(false);
end;

procedure TSempareBootVelocityTemplate.SetTemplate(const AIfNull: boolean);
var
  ctx: IVelocityContext;
begin
  if AIfNull and (FTemplate = nil) then
    exit;
  FLock.Acquire;
  try
    if not FEnabled then
    begin
      FTemplate := nil;
      exit;
    end;
    if FContext <> nil then
      ctx := FContext.Context
    else
      ctx := Sempare.Template.Template.Context();
    FTemplate := Sempare.Template.Template.Parse(ctx, FTemplateText)
  finally
    FLock.Release;
  end;
end;

procedure TSempareBootVelocityTemplate.SetTemplateText(const Value: string);
begin
  FTemplateText := Value;
  SetTemplate(false);
end;

end.
