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
unit Sempare.Template.TestDictionary;

interface

uses
  DUnitX.TestFramework,
  System.Generics.Collections;

type

  [TestFixture]
  TTestTemplateDict = class
  public
    [Test]
    procedure TestDictionary;
    [Test]
    procedure TestNestedDictionary;
  end;

type
  TStringDictionary = TDictionary<string, string>;
  TNestedDictionary = TObjectDictionary<integer, TDictionary<string, string>>;

implementation

uses

  Sempare.Template;

procedure TTestTemplateDict.TestDictionary;
var
  D: TDictionary<string, string>;
begin
  D := TDictionary<string, string>.create;
  D.Add('a', 'value');
  Assert.AreEqual('value', Template.Eval('<% a %>', D));
  D.Free;
end;

procedure TTestTemplateDict.TestNestedDictionary;
var
  dict: TNestedDictionary;
begin
  dict := TNestedDictionary.create([doOwnsValues]);
  dict.Add(1, TStringDictionary.create());
  dict.Add(2, TStringDictionary.create());

  dict[1].Add('v', 'value');
  dict[2].Add('v', 'another');

  Assert.AreEqual('value', Template.Eval('<% _[1][''v'']%>', dict));
  Assert.AreEqual('another', Template.Eval('<% _[2][''v'']%>', dict));

  dict.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateDict);

end.
