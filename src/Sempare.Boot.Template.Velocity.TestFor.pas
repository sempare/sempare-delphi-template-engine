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
unit Sempare.Boot.Template.Velocity.TestFor;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityFor = class
  public
    [Test]
    procedure TestForIn;
    [Test]
    procedure TestForRangeWithBreak;
    [Test]
    procedure TestForRangeWithContinue;
    [Test]
    procedure TestWhileWithBreak;
    [Test]
    procedure TestSimpleFor;
    [Test]
    procedure TestSimpleForDownTo;
    [Test]
    procedure TestSimpleForin;
    [Test]
    procedure TestStructure;
    [Test]
    procedure TestNested;
    [Test]
    procedure TestNestedBreak;

  end;

implementation

uses
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity;

procedure TTestVelocityFor.TestForIn;
type
  TForIn = record
    range: TList<integer>;
  end;
var
  c: IVelocityTemplate;
  x: TForIn;
begin
  x.range := TList<integer>.create;
  x.range.AddRange([1, 10, 100]);
  c := Velocity.parse('<% for i in range %> <% i %> <% end %>');
  Assert.AreEqual(' 1  10  100 ', Velocity.Eval(c, x));
end;

type
  TForRange = record
    count: integer;
  end;

procedure TTestVelocityFor.TestForRangeWithBreak;
var
  c: IVelocityTemplate;
  x: TForRange;
begin
  x.count := 10;
  c := Velocity.parse('<% for i := 0 to count %> <% i %> <% if i = 5%> <% break %> <% end %> <% end %>');
  Assert.AreEqual(' 0   1   2   3   4   5  ', Velocity.Eval(c, x));
end;

procedure TTestVelocityFor.TestForRangeWithContinue;
var
  x: TForRange;
begin
  x.count := 10;
  Assert.AreEqual('0 2 4 6 8 10 ', Velocity.Eval('<% for i := 0 to count %><% if i mod 2 = 1 %><% continue %><% end %><% i %> <% end %>', x));
end;

procedure TTestVelocityFor.TestNested;
begin
  Assert.AreEqual('1:5,6,;2:5,6,;3:5,6,;', Velocity.Eval('<% for i := 1 to 3 %><% i %>:<% for j := 5 to 6 %><% j %>,<% end %>;<% end%>'));
end;

procedure TTestVelocityFor.TestNestedBreak;
begin
  Assert.AreEqual('1:5,6,;2:5,6,;3:5,6,;', Velocity.Eval('<% for i := 1 to 3 %><% i %>:<% for j := 5 to 10 %><% if j > 6%><%break%><%end%><% j %>,<% end %>;<% end%>'));
end;

procedure TTestVelocityFor.TestWhileWithBreak;
var
  x: record //
    count: integer;
end;

begin
  x.count := 9;
  Assert.AreEqual('0123456789', Velocity.Eval('<% i := 0 %><% while i <= count %><% i %><% i := i + 1 %><% end %>', x));
end;

procedure TTestVelocityFor.TestSimpleFor;
begin
  Velocity.parse('before <% for a := 1 to 10 %> after <% end %> ');
end;

procedure TTestVelocityFor.TestSimpleForDownTo;
begin
  Velocity.parse('before <% for a := 1 downto 10 %> after <% end %> ');
end;

procedure TTestVelocityFor.TestSimpleForin;
begin
  Velocity.parse('before <% for a in b %> pre <% a %> post  <% end %> ');
end;

type
  TInfo = record
    Name: string;
    age: integer;
    constructor create(const n: string; const a: integer);
  end;

constructor TInfo.create(const n: string; const a: integer);
begin
  name := n;
  age := a;
end;

procedure TTestVelocityFor.TestStructure;

var
  info: TList<TInfo>;
begin
  info := TList<TInfo>.create;
  info.AddRange([TInfo.create('conrad', 10), TInfo.create('christa', 20)]);
  Assert.AreEqual(' conrad 10 christa 20', Velocity.Eval('<%for i in _ %> <% i.name %> <% i.age %><%end%>', info));
  info.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityFor);

end.
