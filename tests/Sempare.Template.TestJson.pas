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
unit Sempare.Template.TestJson;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateJson = class
  public
    [test]
    procedure TestJsonObject;

    [test]
    procedure TestJsonArray;

    [test]
    procedure TestToJson;

    [test]
    procedure TestParseJson;

    [test]
    procedure TestJsonNonEmptyArray;

    [test]
    procedure TestJsonEmptyArray;

    [test]
    procedure TestJsonDerefArray;

    [test]
    procedure TestJsonDerefAV;

  end;

implementation

uses
  Sempare.Template.JSON,
  Sempare.Template;

procedure TTestTemplateJson.TestJsonNonEmptyArray;
var
  o: TJsonValue;
begin
  o := TJsonValue.ParseJSONValue('[1,2,3,4,5]');
  Assert.AreEqual('not empty', Template.Eval('<% if not isempty(_) %>not empty<% end %>', o));
  o.Free;

  o := TJsonValue.ParseJSONValue('[1,2,3,4,5]');
  Assert.AreEqual('', Template.Eval('<% if   isempty(_) %>not empty<% end %>', o));
  o.Free;

end;

procedure TTestTemplateJson.TestJsonDerefArray;
var
  o: TJsonValue;
begin
  o := TJsonValue.ParseJSONValue('{ "abc": [1,2,3,4,5] }');
  Assert.AreEqual('1 2 3 4 5 ', Template.Eval('<% for i of _["abc"] %><% i %> <% end %>', o));
  o.Free;
end;

procedure TTestTemplateJson.TestJsonDerefAV;
var
  o: TJsonValue;
begin
  o := TJsonValue.ParseJSONValue('{ "abc": [1,2,3,4,5] }');
  Assert.AreEqual('', Template.Eval('<% for i of _.ab %><% i %> <% end %>', o)); // AV took place due to wrong naming
  o.Free;
end;

procedure TTestTemplateJson.TestJsonEmptyArray;
var
  o: TJsonValue;
begin
  o := TJsonValue.ParseJSONValue('[]');
  Assert.AreEqual('', Template.Eval('<% if not isempty(_) %>empty<% end %>', o));
  o.Free;

  o := TJsonValue.ParseJSONValue('[]');
  Assert.AreEqual('empty', Template.Eval('<% if isempty(_) %>empty<% end %>', o));
  o.Free;
end;

procedure TTestTemplateJson.TestJsonArray;
var
  o: TJsonValue;
begin
  o := TJsonValue.ParseJSONValue('[1,2,3,4,5]');
  Assert.AreEqual('1 2 3 4 5 ', Template.Eval('<% for i in _ %><% i %> <% end %>', o));

  o.Free;
end;

procedure TTestTemplateJson.TestJsonObject;
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
  Assert.AreEqual('string true  123 value', Template.Eval('<% _.str %> <% _.bool%> <% _.null%> <% _.num%> <% _.object.subval %>', o));
  o.Free;
end;

procedure TTestTemplateJson.TestParseJson;
begin
  Assert.AreEqual('123.45', Template.Eval('<% ParseJson("123.45") %>'));
  Assert.AreEqual('123', Template.Eval('<% ParseJson("123") %>'));
  Assert.AreEqual('true', Template.Eval('<% ParseJson("true") %>'));
  Assert.AreEqual('false', Template.Eval('<% ParseJson("false") %>'));
  Assert.AreEqual('str', Template.Eval('<% ParseJson(''"str"'') %>'));
  Assert.AreEqual('{"a":1,"b":2}', Template.Eval('<% ToJson(ParseJson(''{ "a": 1, "b" : 2}'')) %>'));
end;

procedure TTestTemplateJson.TestToJson;
begin
  Assert.AreEqual('123.45', Template.Eval('<% ToJson(123.45) %>'));
  Assert.AreEqual('123', Template.Eval('<% ToJson(123) %>'));
  Assert.AreEqual('true', Template.Eval('<% ToJson(true) %>'));
  Assert.AreEqual('false', Template.Eval('<% ToJson(false) %>'));
  Assert.AreEqual('str', Template.Eval('<% ToJson("str") %>'));
  Assert.AreEqual('{"a":1,"b":2}', Template.Eval('<% ToJson({"a":1,"b":2}) %>'));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateJson);

end.
