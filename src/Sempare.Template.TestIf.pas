(*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                          Sempare Template Engine                                                 *
 *                                                                                                  *
 *                                                                                                  *
 *         https://github.com/sempare/sempare-delphi-template-engine                                *
 ****************************************************************************************************
 *                                                                                                  *
 * Copyright (c) 2020 Sempare Limited                                                               *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License                             *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                                     *
 * https://github.com/sempare/sempare-delphi-template-engine/blob/master/docs/commercial.license.md *
 *                                                                                                  *
 * Unless required by applicable law or agreed to in writing, software                              *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,                               *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                         *
 * See the License for the specific language governing permissions and                              *
 * limitations under the License.                                                                   *
 *                                                                                                  *
 *************************************************************************************************%*)
unit Sempare.Template.TestIf;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateIf = class
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
  Sempare.Template;

type
  TStrCond = record
    Val: string;
  end;

procedure TTestTemplateIf.TestEmptyStringAsIfCondition;
var
  data: TStrCond;
begin
  Assert.AreEqual('ok', Template.Eval('<% if not val %>ok<% end %>', data));
  data.Val := 'something';
  Assert.AreEqual('ok', Template.Eval('<% if val %>ok<% end %>', data));
end;

procedure TTestTemplateIf.TestIfWithBreak;
begin
  Assert.WillRaise(
    procedure
    begin
      Template.parse('<% if (true) %> <% break%> <% end %> ');
    end);
end;

procedure TTestTemplateIf.TestIfWithContinue;
begin
  Assert.WillRaise(
    procedure
    begin
      Template.parse('<% if (true) %> <% continue%> <% end %> ');
    end);
end;

procedure TTestTemplateIf.TestNestedIf;
begin
  Template.parse('before <% if (true) %> pre <% if (true) %> midd;e <% end %> post <% end %> ');
end;

type
  TIf = record
    //
    showname: boolean;
    Name: string;
  end;

procedure TTestTemplateIf.TestSimpleIf;
var
  x: TIf;

begin
  x.showname := true;
  x.Name := 'conrad';
  Test(x, 'before   conrad  ', 'before <% if showname %>  <% name %> <% end %> ');
end;

procedure TTestTemplateIf.TestSimpleIfFalse;
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

procedure TTestTemplateIf.Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
var
  c: ITemplate;
begin
  c := Template.parse(ATemplate);
  Assert.AreEqual(AExpect, Template.Eval(c, AValue));
end;

procedure TTestTemplateIf.TestElIf;
var
  T: ITemplate;
  r: TElIf;
begin
  T := Template.parse( //
    '<% if val=1 %>' + //
    'one' + //
    '<% elif val=2 %>' + //
    '         <% if other %>2<% else %>two<% end %>' + //
    '<% else %>' + //
    '         <% if other %>3<% else %>three<% end %>' + //
    '<% end %>');
  // writeln(Template.PrettyPrint(T));
  r.Val := 1;
  Assert.AreEqual('one', Template.Eval(T, r));

  r.Val := 2;
  r.other := true;
  Assert.AreEqual('2', trim(Template.Eval(T, r)));

  r.Val := 2;
  r.other := false;
  Assert.AreEqual('two', trim(Template.Eval(T, r)));

  r.Val := 3;
  r.other := true;
  Assert.AreEqual('3', trim(Template.Eval(T, r)));

  r.Val := 3;
  r.other := false;
  Assert.AreEqual('three', trim(Template.Eval(T, r)));

end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateIf);

end.
