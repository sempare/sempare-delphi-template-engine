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
unit Sempare.Template.Functions.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TFunctionTest = class
  public

    [Test]
    procedure TestTrim();
    [Test]
    procedure TestSubStr();
    [Test]
    procedure TestSubString();
    [Test]
    procedure TestPos();
    [Test]
    procedure TestLen();
    [Test]
    procedure TestDtNow();
    [Test]
    procedure TestFmt();
    [Test]
    procedure TestFmtDate();
    [Test]
    procedure TestStr();
    [Test]
    procedure TestInt();
    [Test]
    procedure TestNum();
    [Test]
    procedure TestSplit();
    [Test]
    procedure TestLowercase();
    [Test]
    procedure TestUppercase();
    [Test]
    procedure TestRev();
    [Test]
    procedure TestUCFirst();
    [Test]
    procedure TestTypeOf;
    [Test]
    procedure TestReplace;
    [Test]
    procedure TestMatch;
    [Test]
    procedure TestSort;
    [Test]
    procedure TestChrOrd();
    [Test]
    procedure TestPadding;
    [Test]
    procedure AddFmtNumTest;
  end;

type
  TMyType = class
    Dummy: integer;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
  Sempare.Template.Functions,
  Sempare.Template.Context,
  Sempare.Template,
  Sempare.Template.Rtti;

{ TFunctionTest }

procedure TFunctionTest.AddFmtNumTest;
begin
  Assert.AreEqual('123.457', Template.Eval('<% x := 123.456789 %><% fmt("%6.3f", x) %>'));
end;

procedure TFunctionTest.TestChrOrd;
begin
  Assert.AreEqual(#10, Template.Eval('<% chr(10) %>'));
  Assert.AreEqual(#9, Template.Eval('<% chr(9) %>'));
  Assert.AreEqual('200', Template.Eval('<% ord(chr(200)) %>'));
end;

procedure TFunctionTest.TestDtNow;
var
  s: string;
begin
  s := Template.Eval('<% dtnow() %>');
  Assert.IsTrue(s <> '');
end;

procedure TFunctionTest.TestFmt;
begin
  Assert.AreEqual('hello world', Template.Eval('<% fmt(''%s %s'', ''hello'', ''world'') %>'));
end;

type
  TDTNow = class
  public
    class function dtnow: tdatetime; static;
  end;

class function TDTNow.dtnow: tdatetime;
begin
  exit(43821);
end;

procedure TFunctionTest.TestFmtDate;
var
  ctx: ITemplateContext;
  Functions: ITemplateFunctions;
begin
  Functions := CreateTemplateFunctions;
  Functions.RegisterDefaults;
  Functions.AddFunctions(TDTNow);

  ctx := Template.Context([eoNoDefaultFunctions]);
  ctx.Functions := Functions;
  Assert.AreEqual('2019-12-22', Template.Eval(ctx, '<% fmtdt(''yyyy-mm-dd'', dtnow()) %>'));
end;

procedure TFunctionTest.TestInt;
begin
  Assert.AreEqual('123', Template.Eval('<% int(''123'') %>'));
end;

procedure TFunctionTest.TestLen;
begin
  Assert.AreEqual('10', Template.Eval('<% len(''0123456789'') %>'));
  Assert.AreEqual('5', Template.Eval('<% len(''01234'') %>'));
end;

procedure TFunctionTest.TestLowercase;
begin
  Assert.AreEqual('hello', Template.Eval('<% lowercase(''HeLlo'') %>'));
end;

procedure TFunctionTest.TestMatch;
begin
  Assert.AreEqual('ok', Template.Eval('<% (match(''aaa'',''a+'')?''ok'':''fail'') %>'));
  Assert.AreEqual('fail', Template.Eval('<% (match('''',''a+'')?''ok'':''fail'') %>'));
  Assert.AreEqual('fail', Template.Eval('<% (match(''b'',''a+'')?''ok'':''fail'') %>'));
  Assert.AreEqual('ok', Template.Eval('<% (match(''aaa'',''a+b*'')?''ok'':''fail'') %>'));
  Assert.AreEqual('ok', Template.Eval('<% (match(''aaaaabbbbb'',''a+b*'')?''ok'':''fail'') %>'));
end;

procedure TFunctionTest.TestNum;
begin
  Assert.AreEqual('123', Template.Eval('<% num(''123'') %>'));
  Assert.AreEqual('123', Template.Eval('<% num(123) %>'));
  Assert.AreEqual('123.45', Template.Eval('<% num(123.45) %>'));

end;

procedure TFunctionTest.TestPadding;
begin
  Assert.AreEqual('   123', Template.Eval('<% padleft(123, 6) %>'));
  Assert.AreEqual('000123', Template.Eval('<% padleft(123, 6, "0") %>'));
  Assert.AreEqual('123   ', Template.Eval('<% padright(123, 6) %>'));
  Assert.AreEqual('123000', Template.Eval('<% padright(123, 6, "0") %>'));
  Assert.AreEqual(#9#9#9#9#9, Template.Eval('<% tabs(5) %>'));
  Assert.AreEqual('     ', Template.Eval('<% spaces(5) %>'));
  Assert.AreEqual(#10#10#10#10#10, Template.Eval('<% nl(5) %>'));
  Assert.AreEqual(#13#10#13#10, Template.Eval('<% crnl(2) %>'));
end;

procedure TFunctionTest.TestPos;
begin
  Assert.AreEqual('4', Template.Eval('<% pos(''3'',''0123456789'') %>'));
end;

procedure TFunctionTest.TestReplace;
begin
  Assert.AreEqual('bb', Template.Eval('<% replace(''a'', ''b'', ''aa'') %>'));
  Assert.AreEqual('hello universe', Template.Eval('<% replace(''world'', ''universe'', ''hello world'') %>'));
end;

procedure TFunctionTest.TestRev;
begin
  Assert.AreEqual('dlrow', Template.Eval('<% rev(''world'') %>'));
  Assert.AreEqual('ddccbbaa', Template.Eval('<% rev(''aabbccdd'') %>'));
end;

procedure TFunctionTest.TestSort;
begin
  Assert.AreEqual('addfgs', Template.Eval('<% values := sort(split(''g,f,d,s,d,a'', '','')) %><% for j in values %><% values[j] %><% end %>'));
  Assert.AreEqual('12345', Template.Eval('<% values := sort([5,4,3,2,1]) %><% for j in values %><% values[j] %><% end %>'));
end;

procedure TFunctionTest.TestSplit;
begin
  Assert.AreEqual('hello', Template.Eval('<% split(''hello_world'', ''_'')[0] %>'));
  Assert.AreEqual('world', Template.Eval('<% split(''hello_world'', ''_'')[1] %>'));
end;

procedure TFunctionTest.TestStr;
begin
  Assert.AreEqual('123', Template.Eval('<% str(123) %>'));
end;

procedure TFunctionTest.TestSubStr;
begin
  Assert.AreEqual('012', Template.Eval('<% substr(''0123456789'', 1,3) %>'));
  Assert.AreEqual('123', Template.Eval('<% substr(''0123456789'', 2,3) %>'));
  Assert.AreEqual('789', Template.Eval('<% substr(''0123456789'', 8,3) %>'));
  Assert.AreEqual('56789', Template.Eval('<% substr(''0123456789'', -5, 5) %>'));
end;

procedure TFunctionTest.TestSubString;
begin
  Assert.AreEqual('678', Template.Eval('<% substring(''0123456789'', 7,9) %>'));
  Assert.AreEqual('56789', Template.Eval('<% substring(''0123456789'', -5) %>'));
  Assert.AreEqual('567', Template.Eval('<% substring(''0123456789'', -5, -3) %>'));

end;

procedure TFunctionTest.TestTrim;
begin
  Assert.AreEqual('trimmed', Template.Eval('<% trim(''   trimmed   '') %>'));
end;

procedure TFunctionTest.TestTypeOf;
var
  MyData: TMyType;
begin
  MyData := TMyType.Create;
  try
    Assert.AreEqual('System.String', Template.Eval('<% typeof(''HELLO'') %>'));
    Assert.AreEqual('System.Extended', Template.Eval('<% typeof(123) %>'));
    Assert.AreEqual('Sempare.Template.Functions.Test.TMyType', Template.Eval('<% typeof(_) %>', MyData))
  finally
    MyData.Free;
  end;
end;

procedure TFunctionTest.TestUCFirst;
begin
  Assert.AreEqual('Hello', Template.Eval('<% ucfirst(''HELLO'') %>'));
  Assert.AreEqual('Hello', Template.Eval('<% ucfirst(''helLO'') %>'));
end;

procedure TFunctionTest.TestUppercase;
begin
  Assert.AreEqual('HELLO', Template.Eval('<% uppercase(''HeLlo'') %>'));
end;

initialization

// TDUnitX.RegisterTestFixture(TFunctionTest);

end.
