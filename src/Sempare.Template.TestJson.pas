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
unit Sempare.Template.TestJson;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateJson = class
  public
    [test]
    procedure TestJson;

  end;

implementation

uses
  Sempare.Template.JSON,
  Sempare.Template;

procedure TTestTemplateJson.TestJson;
var
  o, o2: TJSonObject;
begin
  o := TJSonObject.create;
  o.AddPair('str', 'string');
  o.AddPair('bool', TJSONTrue.create);
  o.AddPair('null', TJSONNull.create);
  o.AddPair('num', TJSONNumber.create(123));

  o2 := TJSonObject.create;
  o2.AddPair('subval', 'value');
  o.AddPair('object', o2);
  Assert.AreEqual('string true  123 value', Template.Eval('<% _.str %> <% _.bool%> <%_.null%> <%_.num%> <% _.object.subval %>', o));
  o.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateJson);

end.
