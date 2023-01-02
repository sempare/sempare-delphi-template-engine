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
unit Sempare.Template.TestStackFrame;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateScope = class
  public

    [Test]
    procedure TestScopeClass;

  end;

implementation

uses
  Sempare.Template.StackFrame;

procedure TTestTemplateScope.TestScopeClass;

var
  S, s2: TStackFrame;
begin
  S := TStackFrame.create();
  S['root'] := 'root123';
  S['name'] := 'conrad';
  Assert.AreEqual('root123', S.Item['root'].AsString);
  Assert.AreEqual('conrad', S.Item['name'].AsString);
  s2 := S.Clone;
  s2['name'] := 'christa';
  Assert.AreEqual('root123', s2.Item['root'].AsString);
  Assert.AreEqual('christa', s2.Item['name'].AsString);
  S.Free;
  s2.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateScope);

end.
