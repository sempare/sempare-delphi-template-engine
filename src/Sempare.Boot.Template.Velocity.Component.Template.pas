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
unit Sempare.Boot.Template.Velocity.Component.Template;

interface

{$R 'TSempareBootVelocityTemplate.dcr' }

uses
  System.Classes,
  System.SysUtils,
  Sempare.Boot.Template.Velocity.Component.Context,
  Sempare.Boot.Template.Velocity;

type

  [ObservableMember('TemplateText')]
  TSempareBootVelocityTemplate = class(TComponent)
  private
    FEnabled: boolean;
    FContext: TSempareBootVelocityContext;
    FTemplateText: string;
    FTemplate: IVelocityTemplate;
    function GetTemplate: IVelocityTemplate;
    procedure SetTemplateText(const Value: string);
    procedure SetTemplate;
    procedure SetEnabled(const Value: boolean);
    procedure SetContext(const Value: TSempareBootVelocityContext);
  public
    property Template: IVelocityTemplate read GetTemplate;
  published
    property Context: TSempareBootVelocityContext read FContext write SetContext;
    property TemplateText: string read FTemplateText write SetTemplateText;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

implementation

{ TSempareBootVelocityTemplate }

function TSempareBootVelocityTemplate.GetTemplate: IVelocityTemplate;
begin
  if FTemplate = nil then
    SetTemplate;
  result := FTemplate;
end;

procedure TSempareBootVelocityTemplate.SetContext(const Value: TSempareBootVelocityContext);
begin
  FContext := Value;
end;

procedure TSempareBootVelocityTemplate.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
  SetTemplate;
end;

procedure TSempareBootVelocityTemplate.SetTemplate;
var
  ctx: IVelocityContext;
begin
  if not FEnabled then
  begin
    FTemplate := nil;
    exit;
  end;
  if FContext <> nil then
    ctx := FContext.Context
  else
    ctx := Velocity.Context();
  FTemplate := Velocity.Parse(ctx, FTemplateText)
end;

procedure TSempareBootVelocityTemplate.SetTemplateText(const Value: string);
begin
  FTemplateText := Value;
  SetTemplate;
end;

end.
