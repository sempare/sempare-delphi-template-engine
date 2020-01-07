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
unit Sempare.Boot.Template.Velocity.Components.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestComponent = class
  public
    [Test]
    procedure TestContextVariables;
    [Test]
    procedure TestEvalVariables;
  end;

implementation

uses
  Sempare.Boot.Template.Velocity.Components;

{ TTestComponent }

procedure TTestComponent.TestContextVariables;
var
  ctx: TSempareBootVelocityContext;
  tpl: TSempareBootVelocityTemplate;
  evalEngine: TSempareBootVelocityEngine;
begin
  ctx := TSempareBootVelocityContext.Create(nil);
  tpl := TSempareBootVelocityTemplate.Create(nil);
  evalEngine := TSempareBootVelocityEngine.Create(nil);
  try
    ctx.Variable['v'] := 'value';
    tpl.TemplateText := '<% v %>';
    evalEngine.Context := ctx;
    evalEngine.Template := tpl;
    evalEngine.Enabled := true;

    Assert.AreEqual('value', evalEngine.Text);
  finally
    ctx.Free;
    tpl.Free;
    evalEngine.Free;
  end;
end;

procedure TTestComponent.TestEvalVariables;
var
  ctx: TSempareBootVelocityContext;
  tpl: TSempareBootVelocityTemplate;
  evalEngine: TSempareBootVelocityEngine;
begin
  ctx := TSempareBootVelocityContext.Create(nil);
  tpl := TSempareBootVelocityTemplate.Create(nil);
  evalEngine := TSempareBootVelocityEngine.Create(nil);
  try
    ctx.Variable['v'] := 'value';
    tpl.TemplateText := '<% v %> <%_.v%>';
    evalEngine.Variable['v'] := 'another';
    evalEngine.Context := ctx;
    evalEngine.Template := tpl;
    evalEngine.Enabled := true;

    Assert.AreEqual('value another', evalEngine.Text);
  finally
    ctx.Free;
    tpl.Free;
    evalEngine.Free;
  end;
end;

end.
