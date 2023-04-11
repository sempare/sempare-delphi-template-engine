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
 * Copyright (c) 2019-2023 Sempare Limited                                                          *
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
unit Sempare.Template.TestLexer;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateLexer = class
  public

    [Test]
    procedure TestDouble;
    [Test]
    procedure TestDoubleLocalisation;
    [Test]
    procedure TestDoubleExponent;
    [Test]
    procedure TestDoubleExponentLocalisation;
    [Test]
    procedure TestLexer;
    [Test]
    procedure TestString;
    [Test]
    procedure TestEscapedString;
    [Test]
    procedure TestDoubleQuotedString;
    [Test]
    procedure TestUnicodeQuotedString;
    [Test]
    procedure TestInvalidChar;
    [Test]
    procedure TestStripCharLeftAndRight;
    [Test]
    procedure TestStripWhitespace;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  Sempare.Template.AST,
  Sempare.Template.Context,
  Sempare.Template.Common,
  Sempare.Template.Lexer,
  Sempare.Template;

procedure TTestTemplateLexer.TestDouble;
var
  LContext: ITemplateContext;
begin
  LContext := Template.Context();
  LContext.DecimalSeparator := ',';
  LContext.ValueSeparator := ';';
  Assert.AreEqual('543,21', Template.Eval(LContext, '<% x:= 543,21 %><% x %>'));
  Assert.AreEqual('5,1234', Template.Eval(LContext, '<% x:= 5,1234 %><% x %>'));
  Assert.AreEqual('12,34 43,21 ', Template.Eval(LContext, '<% vals := [ 12,34 ; 43,21] %><% for x in vals %><% vals[x] %> <% end %>'));
end;

procedure TTestTemplateLexer.TestDoubleExponent;
begin
  Assert.AreEqual('12.345', Template.Eval('<% x:= 12345e-3 %><% x %>'));
  Assert.AreEqual('-12.345', Template.Eval('<% x:= -12345e-3 %><% x %>'));
  Assert.AreEqual('-1234500', Template.Eval('<% x:= -1234.5e3 %><% x %>'));
  Assert.AreEqual('-123450', Template.Eval('<% x:= -1234.5e+2 %><% x %>'));
end;

procedure TTestTemplateLexer.TestDoubleExponentLocalisation;
var
  LContext: ITemplateContext;
begin
  LContext := Template.Context();
  LContext.DecimalSeparator := ',';
  Assert.AreEqual('12,345', Template.Eval(LContext, '<% x:= 12345e-3 %><% x %>'));
  Assert.AreEqual('-12,345', Template.Eval(LContext, '<% x:= -12345e-3 %><% x %>'));
  Assert.AreEqual('-1234500', Template.Eval(LContext, '<% x:= -1234,5e3 %><% x %>'));
  Assert.AreEqual('-123450', Template.Eval(LContext, '<% x:= -1234,5e+2 %><% x %>'));
end;

procedure TTestTemplateLexer.TestDoubleLocalisation;
begin
  Assert.AreEqual('543.21', Template.Eval('<% x:= 543.21 %><% x %>'));
  Assert.AreEqual('5.1234', Template.Eval('<% x:= 5.1234 %><% x %>'));
end;

procedure TTestTemplateLexer.TestDoubleQuotedString;
begin
  Assert.AreEqual('this is a test', Template.Eval('<% "this is a test" %>'));
end;

procedure TTestTemplateLexer.TestEscapedString;
begin
  Assert.AreEqual('that''s mine', Template.Eval('<% ''that\''s mine'' %>'));
  Assert.AreEqual('that''s mine', Template.Eval('<% "that''s mine" %>'));
end;

procedure TTestTemplateLexer.TestInvalidChar;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual('this is a test', Template.Eval('<% ¬this is a test¬ %>'));
    end);
end;

procedure TTestTemplateLexer.TestLexer;

var
  m: tstringstream;
  Lexer: ITemplateLexer;
  symbol: ITemplateSymbol;
  vs: ITemplateValueSymbol;
  val: string;
begin
  m := tstringstream.create('before <% if (true) %>hello<% end %> after');
  Lexer := CreateTemplateLexer(Template.Context, m);
  while true do
  begin
    symbol := Lexer.GetToken;
    if symbol.Token = VsEOF then
      break;
    Assert.AreNotEqual(VsEOF, symbol.Token);
    val := '';
    if supports(symbol, ITemplateValueSymbol, vs) then
    begin
      vs := symbol as ITemplateValueSymbol;
      val := vs.Value;
    end;
  end;
end;

procedure TTestTemplateLexer.TestString;
begin
  Assert.AreEqual('hello world', Template.Eval('<% ''hello'' + '' '' + ''world'' %>'));
end;

procedure TTestTemplateLexer.TestStripCharLeftAndRight;
begin
  Assert.AreEqual('hello world', Template.Eval('<%- ''hello'' + '' '' + ''world'' -%>'));
  Assert.AreEqual('hello world', Template.Eval('<%* ''hello'' + '' '' + ''world'' *%>'));
  Assert.AreEqual('123', Template.Eval('<%- 123%>'));
  Assert.AreEqual('-123', Template.Eval('<%- -123%>'));
end;

procedure TTestTemplateLexer.TestStripWhitespace;
begin
  Assert.AreEqual('helloworld    '#13#10, Template.Eval('hello    <%- "world" %>    '#13#10));
  Assert.AreEqual('helloworld'#13#10, Template.Eval('hello    <%- "world" -%>    '#13#10));

  Assert.AreEqual('hello world    '#13#10, Template.Eval('hello    <%+ "world" %>    '#13#10));
  Assert.AreEqual('hello world ', { .  . } Template.Eval('hello    <%+ "world" +%>    '#13#10));
  Assert.AreEqual('hello world', { .   . } Template.Eval('hello    <%+ "world" *%>    '#13#10));
end;

procedure TTestTemplateLexer.TestUnicodeQuotedString;
begin
  Assert.AreEqual('this is a test', Template.Eval('<% ‘this is a test’ %>'));
  Assert.AreEqual('this is a test', Template.Eval('<% “this is a test” %>'));
  Assert.AreEqual('this is a test', Template.Eval('<% `this is a test` %>'));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateLexer);

end.
