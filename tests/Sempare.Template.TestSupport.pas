unit Sempare.Template.TestSupport;
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
 * Copyright (c) 2019-2025 Sempare Limited                                                          *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the Apache Version 2.0 or the Sempare Commercial License                          *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.apache.org/licenses/LICENSE-2.0                                                      *
 * https://github.com/sempare/sempare-delphi-template-engine/blob/master/docs/commercial.license.md *
 *                                                                                                  *
 * Unless required by applicable law or agreed to in writing, software                              *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,                               *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                         *
 * See the License for the specific language governing permissions and                              *
 * limitations under the License.                                                                   *
 *                                                                                                  *
 *************************************************************************************************%*)

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestSupport = class
  public

    [Test]
    procedure TestSuspectedArrayNumbersNotHandledGHIssue219;

  end;

implementation

uses
  System.SysUtils,
  Sempare.Template;

{ TTestSupport }

procedure TTestSupport.TestSuspectedArrayNumbersNotHandledGHIssue219;
var
  LTemplate: string;
  LResult: string;
  LExpected: string;
begin
  LTemplate := //
    '<% arr := [4,5,6] %>' + sLineBreak + //
    sLineBreak + //
    '<%- for x of arr %>' + sLineBreak + //
    '<% x %>' + sLineBreak + //
    '<% end %>';

  LExpected := //
    sLineBreak + //
    sLineBreak + //
    '4' + sLineBreak + //
    '5' + sLineBreak + //
    '6' + sLineBreak; //

  LResult := Template.Eval(LTemplate);

  Assert.AreEqual(LExpected, LResult);

  LTemplate := //
    '<% arr := [''4'',''5'',''6''] %>' + sLineBreak + //
    sLineBreak + //
    '<%- for x of arr %>' + sLineBreak + //
    '<% x %>' + sLineBreak + //
    '<% end %>';

  LResult := Template.Eval(LTemplate);

  Assert.AreEqual(LExpected, LResult);

end;

initialization

TDUnitX.RegisterTestFixture(TTestSupport);

end.
