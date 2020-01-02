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
unit Sempare.Boot.Template.Velocity.TestIf;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityIf = class
  private
    procedure Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);

  public
    [Test]
    procedure TestElIf;
    [Test]
    procedure TestEmptyStringAsIfCondition;
    [Test]
    procedure TestNestedIf;
    [Test]
    procedure TestSimpleIf;
    [Test]
    procedure TestSimpleIfFalse;
    [Test]
    procedure TestIfWithBreak;
    [Test]
    procedure TestIfWithContinue;
  end;

implementation

uses
  SysUtils,
  Sempare.Boot.Template.Velocity;

type
  TStrCond = record
    Val: string;
  end;

procedure TTestVelocityIf.TestEmptyStringAsIfCondition;
var
  data: TStrCond;
begin
  Assert.AreEqual('ok', Velocity.Eval('<% if not val %>ok<% end %>', data));
  data.Val := 'something';
  Assert.AreEqual('ok', Velocity.Eval('<% if val %>ok<% end %>', data));
end;

procedure TTestVelocityIf.TestIfWithBreak;
begin
  Assert.WillRaise(
    procedure
    begin
      Velocity.parse('<% if (true) %> <% break%> <% end %> ');
    end);
end;

procedure TTestVelocityIf.TestIfWithContinue;
begin
  Assert.WillRaise(
    procedure
    begin
      Velocity.parse('<% if (true) %> <% continue%> <% end %> ');
    end);
end;

procedure TTestVelocityIf.TestNestedIf;
begin
  Velocity.parse('before <% if (true) %> pre <% if (true) %> midd;e <% end %> post <% end %> ');
end;

type
  TIf = record
    //
    showname: boolean;
    Name: string;
  end;

procedure TTestVelocityIf.TestSimpleIf;
var
  x: TIf;

begin
  x.showname := true;
  x.Name := 'conrad';
  Test(x, 'before   conrad  ', 'before <% if showname %>  <% name %> <% end %> ');
end;

procedure TTestVelocityIf.TestSimpleIfFalse;
var
  x: TIf;

begin
  x.showname := false;
  x.Name := 'conrad';
  Test(x, 'hello  conrad ', 'hello <% if showname %> should not be shown <% else %> <% name %> <% end %>');
end;

type
  TElIf = record
    Val: integer;
    other: boolean;
  end;

procedure TTestVelocityIf.Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
var
  c: IVelocityTemplate;
begin
  c := Velocity.parse(ATemplate);
  Assert.AreEqual(AExpect, Velocity.Eval(c, AValue));
end;

procedure TTestVelocityIf.TestElIf;
var
  T: IVelocityTemplate;
  r: TElIf;
begin
  T := Velocity.parse( //
    '<% if val=1 %>' + //
    'one' + //
    '<% elif val=2 %>' + //
    '         <% if other %>2<% else %>two<% end %>' + //
    '<% else %>' + //
    '         <% if other %>3<% else %>three<% end %>' + //
    '<% end %>');
  // writeln(Velocity.PrettyPrint(T));
  r.Val := 1;
  Assert.AreEqual('one', Velocity.Eval(T, r));

  r.Val := 2;
  r.other := true;
  Assert.AreEqual('2', trim(Velocity.Eval(T, r)));

  r.Val := 2;
  r.other := false;
  Assert.AreEqual('two', trim(Velocity.Eval(T, r)));

  r.Val := 3;
  r.other := true;
  Assert.AreEqual('3', trim(Velocity.Eval(T, r)));

  r.Val := 3;
  r.other := false;
  Assert.AreEqual('three', trim(Velocity.Eval(T, r)));

end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityIf);

end.
