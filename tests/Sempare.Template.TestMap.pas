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
 * Copyright (c) 2019-2024 Sempare Limited                                                          *
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
unit Sempare.Template.TestMap;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateMap = class

  public
    [Test]
    procedure TestMap;

    [Test]
    procedure TestMapAssignment;

    [Test]
    procedure TestIsMapFunction;

    [Test]
    procedure TestContainsKeyFunction;

    [Test]
    procedure TestForIn;

    [Test]
    procedure TestForOf;

    [Test]
    procedure TestToJson;

    [Test]
    procedure TestParseJson;

    [test]
    procedure TestMapWithExpression;
  end;

implementation

{$I 'Sempare.Template.Compiler.inc'}

uses
  Sempare.Template,
  Sempare.Template.Util;

procedure TTestTemplateMap.TestMap;
begin
  Assert.AreEqual('false', Template.Eval('<% map := { "a": false } %><% print(map["a"]) %>'));
  Assert.AreEqual('true', Template.Eval('<% map := { "a": true } %><% print(map["a"]) %>'));
  Assert.AreEqual('1', Template.Eval('<% map := { "a": 1 } %><% print(map["a"]) %>'));
  Assert.AreEqual('1.234', Template.Eval('<% map := { "a": 1.234 } %><% print(map["a"]) %>'));
  Assert.AreEqual('str', Template.Eval('<% map := { "a": "str" } %><% print(map["a"]) %>'));
  Assert.AreEqual('123', Template.Eval('<% map := { "a": {"str" : 123} } %><% print(map["a"]["str"]) %>'));
  Assert.AreEqual('123', Template.Eval('<% map := { "a": {"str" : 123} } %><% print(map.a.str) %>'));

end;

procedure TTestTemplateMap.TestMapAssignment;
begin
  Assert.WillRaise(
    procedure
    begin
      Template.Eval('<% map := { "a": false } %><% map.a := 123%><% print(map.a) %>');
    end);

  Assert.AreEqual('123', Template.Eval('<% map := { "a": false } %><% map := { "a": 123 } %><% print(map.a) %>'));

end;

procedure TTestTemplateMap.TestMapWithExpression;
begin
  Assert.AreEqual('false, 129', Template.Eval('<% map := { "a": true and false, "b": 123 + 6 } %><% map.a %>, <% map.b %>'));
end;

procedure TTestTemplateMap.TestParseJson;
begin
  Assert.AreEqual('true, 123', Template.Eval('<% map := parseJson(''{ "a": true, "b": 123 }'') %><% map.a %>, <% map.b %>'));
end;

procedure TTestTemplateMap.TestToJson;
begin
  Assert.AreEqual('{"a":true,"b":123}', Template.Eval('<% map := { "a": true, "b": 123 } %><% ToJson(map) %>'));
end;

procedure TTestTemplateMap.TestContainsKeyFunction;
begin
  Assert.WillRaise(
    procedure
    begin
      Template.Eval('<% containskey(123, "a") %>');
    end);
  Assert.AreEqual('true', Template.Eval('<% map := { "a": {"str" : 123} } %><% containskey(map, "a") %>'));
  Assert.AreEqual('false', Template.Eval('<% map := { "a": {"str" : 123} } %><% containskey(map, "b") %>'));
  Assert.AreEqual('true', Template.Eval('<% map := { "a": {"str" : 123} } %><% containskey(map.a, "str") %>'));
  Assert.AreEqual('false', Template.Eval('<% map := { "a": {"str" : 123} } %><% containskey(map.a, "b") %>'));
end;

procedure TTestTemplateMap.TestForIn;
begin
  Assert.AreEqual('a, b', Template.Eval('<% map := { "a": 1, "b": 2 } %><% for k in map %><% k %><% betweenitems%>, <% end %>'));
end;

procedure TTestTemplateMap.TestForOf;
begin
  Assert.AreEqual('1, 2', Template.Eval('<% map := { "a": 1, "b": 2 } %><% for v of map %><% v %><% betweenitems%>, <% end %>'));
end;

procedure TTestTemplateMap.TestIsMapFunction;
begin
  Assert.AreEqual('false', Template.Eval('<% ismap(123) %>'));
  Assert.AreEqual('false', Template.Eval('<% ismap("123") %>'));
  Assert.AreEqual('false', Template.Eval('<% ismap(false) %>'));
  Assert.AreEqual('true', Template.Eval('<% ismap({}) %>'));
  Assert.AreEqual('true', Template.Eval('<% ismap({"a":123}) %>'));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateMap);

end.
