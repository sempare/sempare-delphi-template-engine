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
  SysUtils,
  System.Rtti,
  System.Json,
  System.Generics.Collections,
  System.Classes,
  Sempare.Boot.Template.Velocity,
  Sempare.Boot.Template.Velocity.Rtti,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Evaluate,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.PrettyPrint,
  Sempare.Boot.Template.Velocity.Lexer;

procedure TTestVelocityAssign.TestSimpleAssignment;
begin
  Velocity.parse('before <% abc := true %> after  <% abc %> final');
end;

procedure TTestVelocityAssign.TestAssignFunctionCall;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.AddFunction('doSomething', 3,
    function(const Args: TArray<Tvalue>): Tvalue
    var
      S, i: integer;
    begin
      S := 0;
      for i := low(Args) to high(Args) do
        S := S + round(asnum(Args[i]));
      result := S;
    end);
  Velocity.parse(ctx, 'before <% abc := doSomething(3,4,5) %> after  <% abc %> final');
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityAssign);

end.
