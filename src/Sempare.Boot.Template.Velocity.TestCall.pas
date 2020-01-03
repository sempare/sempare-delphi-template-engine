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
unit Sempare.Boot.Template.Velocity.TestCall;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityCall = class
  public
    [Test]
    procedure TestFunctionCall;

    [Test]
    procedure TestMethodCall;
  end;

implementation

uses
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity;

type
  TAdder = class
  public
    class function add(const a, b: extended): extended; static;
  end;

class function TAdder.add(const a, b: extended): extended;
begin
  result := a + b;
end;

procedure TTestVelocityCall.TestFunctionCall;

var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.functions.addfunctions(TAdder);
  Assert.AreEqual('before 22 coool after ', Velocity.Eval(ctx, 'before <% add(15,7) %> <% trim(''   coool   '') %> after '));
end;

type
  TMethodClass = class
  public
    function Echo(const AArg: string): string;
  end;

  TRec = record
    o: TMethodClass;
  end;

procedure TTestVelocityCall.TestMethodCall;
var
  r: TRec;
begin
  r.o := TMethodClass.Create;
  Assert.AreEqual('a', Velocity.Eval('<% _.Echo(''a'') %>', r.o));
  Assert.AreEqual('b', Velocity.Eval('<% _.o.Echo(''b'') %>', r));
  r.o.Free;
end;

{ TMethodClass }

function TMethodClass.Echo(const AArg: string): string;
begin
  result := AArg;
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityCall);

end.
