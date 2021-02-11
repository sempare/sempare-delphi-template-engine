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
unit Sempare.Template.Lexer.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateLexer = class
  public

    [test]
    procedure TestDouble;

    [Test]
    procedure TestLexer;

    [test]
    procedure TestString;
    [test]
    procedure TestEscapedString;
    [test]
    procedure TestDoubleQuotedString;
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

initialization

TDUnitX.RegisterTestFixture(TTestTemplateLexer);

end.
