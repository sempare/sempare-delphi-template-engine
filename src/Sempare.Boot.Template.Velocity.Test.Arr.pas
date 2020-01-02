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
unit Sempare.Boot.Template.Velocity.Test.Arr;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityArr = class

  public
    [Test]
    procedure TestArray;

    [Test]
    procedure TestDerefArray;

    [Test]
    procedure TestDerefDynArray;

    [Test]
    procedure TestDynArray;
  end;

implementation

uses
  Sempare.Boot.Template.Velocity;

procedure TTestVelocityArr.TestArray;
var
  a: array [5 .. 10] of integer;
  i: integer;
begin
  for i := Low(a) to High(a) do
    a[i] := i * 2;
  Assert.AreEqual('5 6 7 8 9 10 ', Velocity.Eval('<%for i in _%><%i%> <%end%>', a));
end;

procedure TTestVelocityArr.TestDerefArray;

var
  a: array [1 .. 10] of integer;
begin
  a[5] := 123;
  Assert.AreEqual('123', Velocity.Eval('<% _[5] %>', a));
end;

procedure TTestVelocityArr.TestDerefDynArray;
var
  a: tarray<integer>;
begin
  setlength(a, 10);
  a[5] := 123;
  a[6] := 321;
  Assert.AreEqual('123', Velocity.Eval('<% _[5] %>', a));
  Assert.AreEqual('321', Velocity.Eval('<% _[6] %>', a));
end;

procedure TTestVelocityArr.TestDynArray;
var
  a: tarray<integer>;
  i: integer;
begin
  setlength(a, 5);
  for i := Low(a) to High(a) do
    a[i] := i * 2;
  Assert.AreEqual('0 2 4 6 8 ', Velocity.Eval('<%for i in _%><% _[i]%> <%end%>', a));
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityArr);

end.
