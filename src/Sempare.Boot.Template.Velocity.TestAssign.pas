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
unit Sempare.Boot.Template.Velocity.TestAssign;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityAssign = class
  public
    [Test]
    procedure TestAssignFunctionCall;
    [Test]
    procedure TestSimpleAssignment;
  end;

implementation

uses
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity;

procedure TTestVelocityAssign.TestSimpleAssignment;
begin
  Velocity.parse('before <% abc := true %> after  <% abc %> final');
end;

type
  TDoSomething = class
  public
    class function sum(const AValues: array of extended): extended; static;
  end;

class function TDoSomething.sum(const AValues: array of extended): extended;
var
  v: double;
begin
  result := 0;
  for v in AValues do
    result := result + v;
end;

procedure TTestVelocityAssign.TestAssignFunctionCall;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.Functions.addfunctions(TDoSomething);
  Velocity.parse(ctx, 'before <% abc := sum(3,4,5) %> after  <% abc %> final');
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityAssign);

end.
