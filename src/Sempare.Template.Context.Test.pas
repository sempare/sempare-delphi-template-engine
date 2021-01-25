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
unit Sempare.Template.Context.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TContextTest = class
  public
    [Test]
    procedure TestVariables();
    [Test]
    procedure TestCRNLTAB;
  end;

implementation

uses
  Sempare.Template.Context,
  Sempare.Template;

{ TContextTest }

procedure TContextTest.TestVariables;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context();
  ctx.Variable['company'] := 'Sempare Limited';

  Assert.AreEqual('Sempare Limited 2019', Template.Eval(ctx, '<% company %> <% _ %>', 2019));
end;

procedure TContextTest.TestCRNLTAB;
begin
  Assert.AreEqual(#10, Template.Eval('<% nl %>'));
  Assert.AreEqual(#13, Template.Eval('<% cr %>'));
  Assert.AreEqual(#13#10, Template.Eval('<% crnl %>'));
  Assert.AreEqual(#9, Template.Eval('<% tab %>'));
end;

end.
