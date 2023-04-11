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
unit Sempare.Template.TestAssign;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateAssign = class
  public
    [Test]
    procedure TestAssignFunctionCall;
    [Test]
    procedure TestSimpleAssignment;
  end;

implementation

uses
  Sempare.Template.Context,
  Sempare.Template;

procedure TTestTemplateAssign.TestSimpleAssignment;
begin
  Assert.IsNotNull(Template.parse('before <% abc := true %> after  <% abc %> final'));
end;

type
  TDoSomething = class
  public
    class function sum(const AValues: array of extended): extended; static;
  end;

class function TDoSomething.sum(const AValues: array of extended): extended;
var
  v: double;
begin
  result := 0;
  for v in AValues do
    result := result + v;
end;

procedure TTestTemplateAssign.TestAssignFunctionCall;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.Functions.addfunctions(TDoSomething);
  Assert.IsNotNull(Template.parse(ctx, 'before <% abc := sum(3,4,5) %> after  <% abc %> final'));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateAssign);

end.
