(*%*********************************************************************************
 *                 ___                                                             *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                     *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                    *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                    *
 *                                    |_|                                          *
 ***********************************************************************************
 *                                                                                 *
 *                        Sempare Templating Engine                                *
 *                                                                                 *
 *                                                                                 *
 *               https://github.com/sempare/sempare.template                       *
 ***********************************************************************************
 *                                                                                 *
 * Copyright (c) 2020 Sempare Limited,                                             *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>                *
 *                                                                                 *
 * Contact: info@sempare.ltd                                                       *
 *                                                                                 *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License            *
 * You may not use this file except in compliance with one of these Licenses.      *
 * You may obtain a copy of the Licenses at                                        *
 *                                                                                 *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                    *
 * https://github.com/sempare/sempare.template/docs/commercial.license.md          *
 *                                                                                 *
 * Unless required by applicable law or agreed to in writing, software             *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,              *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.        *
 * See the License for the specific language governing permissions and             *
 * limitations under the License.                                                  *
 *                                                                                 *
 ********************************************************************************%*)
unit Sempare.Template.Lexer.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateLexer = class
  public

    [Test]
    procedure TestLexer;
  end;

implementation

uses
  System.classes,
  Sempare.Template.AST,
  Sempare.Template.Context,
  Sempare.Template.Common,
  Sempare.Template.Lexer,
  Sempare.Template;

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
    if symbol.QueryInterface(ITemplateValueSymbol, vs) = 0 then
    begin
      vs := symbol as ITemplateValueSymbol;
      val := vs.Value;
    end;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateLexer);

end.
