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
unit Sempare.Template.TestDeref;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateDeref = class
  private
    procedure Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
  public
    [Test]
    procedure TestExprDrefef;
    [Test]
    procedure TestSimpleDrefef;
    [Test]
    procedure TestWith;
    [Test]
    procedure TestDerefError;
    [Test]
    procedure TestDerefNull;

    [Test]
    procedure TestIntfProperty;

    [Test]
    procedure TestIntfMethod;
  end;

  TNullable = class
  public
    Data: string;
    Other: TNullable;
  end;

  ITestData = interface
    ['{7F66CDA3-3C98-41AF-A762-8B48FEBF8700}']
    function GetData: string;
    property Data: string read GetData;
  end;

  TTestData = class(TInterfacedObject, ITestData)
    function GetData: string;
  end;

implementation

uses
  System.Rtti,
  System.SysUtils,
  Sempare.Template.Context,
  Sempare.Template;

{ TTestData }
function TTestData.GetData: string;
begin
  exit('hello');
end;

{ TTestTemplateDeref }

procedure TTestTemplateDeref.Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
var
  c: ITemplate;
begin
  c := Template.parse(ATemplate);
  Assert.AreEqual(AExpect, Template.Eval(c, AValue));
end;

type
  TExprDeref = record
    a: record
      b: record
        abcdef: string;
        abcghi: string;
      end;
    end;
  end;

procedure TTestTemplateDeref.TestDerefError;
var
  ctx: ITemplateContext;
  r: record anothervar: boolean;
end;
begin
  ctx := Template.Context([eoRaiseErrorWhenVariableNotFound]);
  Assert.WillRaise(
    procedure
    begin
      Template.Eval(ctx, '<% notfound %>', r);
    end);
end;

procedure TTestTemplateDeref.TestDerefNull;
var
  nullable: TNullable;
begin
  nullable := TNullable.Create;
  nullable.Other := TNullable.Create;
  nullable.Other.Data := 'hello';
  nullable.Data := 'world';
  try
    Test(nullable, 'hello world', '<% other.data %> <% data %><% other.other.data%>'); // other.other.data is handled correctly
  finally
    nullable.Other.Free;
    nullable.Free;
  end;
end;

procedure TTestTemplateDeref.TestExprDrefef;
var
  rec: TExprDeref;
begin
  rec.a.b.abcdef := '123';
  rec.a.b.abcghi := '456';

  Test(rec, '''before  123 after ''', '''before <% suffix := ''def'' %> <% a.b[''abc'' + suffix] %> after ''');
end;

procedure TTestTemplateDeref.TestIntfMethod;
var
  LIntf: ITestData;
begin
  LIntf := TTestData.Create;

  Assert.AreEqual('hello', Template.Eval('<% _.GetData() %>', TValue.From<ITestData>(LIntf)));

end;

procedure TTestTemplateDeref.TestIntfProperty;
var
  LIntf: ITestData;
begin
  LIntf := TTestData.Create;
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual('hello', Template.Eval('<% _.data %>', TValue.From<ITestData>(LIntf)));
    end);
end;

procedure TTestTemplateDeref.TestSimpleDrefef;
begin
  Assert.IsNotNull(Template.parse('before <% a.b %> after '));
end;

procedure TTestTemplateDeref.TestWith;
var
  rec: TExprDeref;
begin
  rec.a.b.abcdef := '123';
  rec.a.b.abcghi := '456';
  Test(rec, '123456', '<% with a.b %><% abcdef %><% abcghi %><%end%>');

end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateDeref);

end.
