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
unit Sempare.Template.TestFunctions;

interface

{$I 'Sempare.Template.Compiler.inc'}

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
    [Test]
    procedure AddCustomFmt;
    [Test]
    procedure AddCustomFmt2;
    [Test]
    procedure AddCustomFmt3;
    [Test]
    procedure TestIsNil;
    [Test]
    procedure TestProcedure;
    [Test]
    procedure TestMd5;
    [Test]
    procedure TestSha1;
    [Test]
    procedure TestSha256;
    [Test]
    procedure TestBase64Encode;
    [Test]
    procedure TestBase64Decode;
    [Test]
    procedure HtmlEscape;
    [Test]
    procedure HtmlUnescape;
    [Test]
    procedure TestMin;
    [Test]
    procedure TestMax;
    [Test]
    procedure TestAbs;
    [Test]
    procedure TestIsObject;
    [Test]
    procedure TestIsRecord;
    [Test]
    procedure TestIsEmpty;
  end;

type
  TMyType = class
    Dummy: integer;
  end;

implementation

uses
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}
  Data.DB,
  FireDAC.Comp.Client,
{$ENDIF}
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Sempare.Template.Functions,
  Sempare.Template.Context,
  Sempare.Template,
  Sempare.Template.Rtti;

{$IFDEF SEMPARE_TEMPLATE_FIREDAC}

function CreateSendungen(): TFDMemTable;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(nil);
  with ds do
  begin
    FieldDefs.Add('Spedition', ftWideString, 20);
    FieldDefs.Add('Sendungsnummer', ftWideString, 20);
    CreateDataSet;
  end;
  with ds do
  begin
    Append;
    FieldByName('Spedition').value := 'blah';
    FieldByName('Sendungsnummer').value := '123';
    Post;
  end;
  exit(ds);
end;

function CreateTracking(): TFDMemTable;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(nil);
  with ds do
  begin
    FieldDefs.Add('Link', ftWideString, 100);
    CreateDataSet;
  end;
  with ds do
  begin
    Append;
    FieldByName('Link').value := 'https://abc.com/%s';
    Post;
  end;
  exit(ds);
end;

{$ENDIF}
{ TFunctionTest }
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}

procedure TFunctionTest.AddCustomFmt2;
type
  TData = record
    Sendungen: TDataSet;
    Tracking: TDataSet;
  end;

var
  Data: TData;
  ctx: ITemplateContext;

begin
  ctx := Template.Context();
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Data.Sendungen := nil;
  Data.Tracking := nil;
  try
    Data.Sendungen := CreateSendungen;
    Data.Tracking := CreateTracking;
    Assert.AreEqual('<li>blah - <a href=''https://abc.com/123''>123</a></li>', Template.Eval(ctx, '<li>{{Sendungen.Spedition}} - <a href=''{{fmt(Tracking.Link, Sendungen.Sendungsnummer)}}''>{{Sendungen.Sendungsnummer}}</a></li>', Data));
  finally
    Data.Sendungen.Free;
    Data.Tracking.Free;
  end;
end;
{$ELSE}

procedure TFunctionTest.AddCustomFmt2;
begin
end;
{$ENDIF}

procedure TFunctionTest.AddCustomFmt3;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context();
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Assert.AreEqual('a', Template.Eval(ctx, '{{fmt("%s", "a")}}'));
end;

procedure TFunctionTest.AddCustomFmt;
type
  TData = record
    Sendungen: record
      Spedition: string;
      Sendungsnummer: string;
    end;

    Tracking: record
      Link: string;
    end;
  end;

var
  Data: TData;
  ctx: ITemplateContext;

begin
  ctx := Template.Context();
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Data.Sendungen.Spedition := 'blah';
  Data.Sendungen.Sendungsnummer := '123';
  Data.Tracking.Link := 'https://abc.com/%s';
  Assert.AreEqual('<li>blah - <a href=''https://abc.com/123''>123</a></li>', Template.Eval(ctx, '<li>{{Sendungen.Spedition}} - <a href=''{{fmt(Tracking.Link, Sendungen.Sendungsnummer)}}''>{{Sendungen.Sendungsnummer}}</a></li>', Data));
end;

procedure TFunctionTest.AddFmtNumTest;
begin
  Assert.AreEqual('123.457', Template.Eval('<% x := 123.456789 %><% fmt("%6.3f", x) %>'));
end;

procedure TFunctionTest.HtmlEscape;
begin
  Assert.AreEqual('&lt;script&gt;alert(&quot;hello&quot;);&lt;/script&gt;', Template.Eval('<% HtmlEscape(''<script>alert("hello");</script>'') %>'));
end;

procedure TFunctionTest.HtmlUnescape;
begin
  Assert.AreEqual('<script>alert("hello");</script>', Template.Eval('<% HtmlUnescape(''&lt;script&gt;alert(&quot;hello&quot;);&lt;/script&gt;'') %>'));
end;

procedure TFunctionTest.TestBase64Decode;
begin
  Assert.AreEqual('hello world', Template.Eval('<% Base64Decode(''aGVsbG8gd29ybGQ='') %>'));
end;

procedure TFunctionTest.TestBase64Encode;
begin
  Assert.AreEqual('aGVsbG8gd29ybGQ=', Template.Eval('<% Base64Encode(''hello world'') %>'));
end;

procedure TFunctionTest.TestChrOrd;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context();
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

  TMyProc = class
  public
    class var myval: string;
    class procedure myproc(const AValue: string); static;
  end;

class procedure TMyProc.myproc(const AValue: string);
begin
  myval := AValue;
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

procedure TFunctionTest.TestIsNil;
type
  TRecord = record
    Obj: TObject;
  end;
var
  LRecord: TRecord;
begin
  LRecord.Obj := nil;
  Assert.AreEqual('', Template.Eval<TRecord>('<% if Obj %>has value<% end %>', LRecord));
  Assert.AreEqual('', Template.Eval<TRecord>('<% if not IsNull(Obj) %>has value<% end %>', LRecord));
  LRecord.Obj := TObject.Create;
  try
    Assert.AreEqual('has value', Template.Eval<TRecord>('<% if Obj %>has value<% end %>', LRecord));
    Assert.AreEqual('has value', Template.Eval<TRecord>('<% if not IsNull(Obj) %>has value<% end %>', LRecord));
  finally
    LRecord.Obj.Free;
  end;
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

procedure TFunctionTest.TestMd5;
begin
  Assert.AreEqual('5eb63bbbe01eeed093cb22bb8f5acdc3', Template.Eval('<% md5(''hello world'') %>'));
end;

procedure TFunctionTest.TestNum;
begin
  Assert.AreEqual('123', Template.Eval('<% num(''123'') %>'));
  Assert.AreEqual('123', Template.Eval('<% num(123) %>'));
  Assert.AreEqual('123.45', Template.Eval('<% num(123.45) %>'));
end;

procedure TFunctionTest.TestPadding;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context();
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

procedure TFunctionTest.TestProcedure;
var
  ctx: ITemplateContext;
  Functions: ITemplateFunctions;
begin
  Functions := CreateTemplateFunctions;
  Functions.RegisterDefaults;
  Functions.AddFunctions(TMyProc);

  ctx := Template.Context([eoNoDefaultFunctions]);
  ctx.Functions := Functions;
  Assert.AreEqual('', TMyProc.myval);
  Assert.AreEqual('', Template.Eval(ctx, '<% myproc(''test'') %>'));
  Assert.AreEqual('test', TMyProc.myval);
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

procedure TFunctionTest.TestSha1;
begin
  Assert.AreEqual('2aae6c35c94fcfb415dbe95f408b9ce91ee846ed', Template.Eval('<% sha1(''hello world'') %>'));
end;

procedure TFunctionTest.TestSha256;
begin
  Assert.AreEqual('b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9', Template.Eval('<% sha256(''hello world'') %>'));
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
    Assert.AreEqual('Sempare.Template.TestFunctions.TMyType', Template.Eval('<% typeof(_) %>', MyData))
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

procedure TFunctionTest.TestMin;
begin
  Assert.AreEqual('1', Template.Eval('<% min(1,2) %>'));
end;

procedure TFunctionTest.TestMax;
begin
  Assert.AreEqual('2', Template.Eval('<% max(1,2) %>'));
end;

procedure TFunctionTest.TestAbs;
begin
  Assert.AreEqual('123.45', Template.Eval('<% abs(-123.45) %>'));
  Assert.AreEqual('123.45', Template.Eval('<% abs(123.45) %>'));
end;

procedure TFunctionTest.TestIsRecord;
var
  LRecord: record end;
  LObject: TObject;
begin
  LObject := TObject.Create;
  Assert.AreEqual('true', Template.Eval('<% isrecord(_) %>', LRecord));
  Assert.AreEqual('false', Template.Eval('<% isrecord(_) %>', LObject));
  LObject.Free;
end;

procedure TFunctionTest.TestIsObject;
var
  LRecord: record end;
  LObject: TObject;
begin
  LObject := TObject.Create;
  Assert.AreEqual('false', Template.Eval('<% isobject(_) %>', LRecord));
  Assert.AreEqual('true', Template.Eval('<% isobject(_) %>', LObject));
  LObject.Free;
end;

procedure TFunctionTest.TestIsEmpty;
var
  LEmpty: TList<string>;
  LNonEmpty: TList<string>;
begin
  LEmpty := TList<string>.Create;
  LNonEmpty := TList<string>.Create;
  LNonEmpty.Add('value');

  Assert.AreEqual('true', Template.Eval('<% isempty(_) %>', LEmpty));
  Assert.AreEqual('false', Template.Eval('<% isempty(_) %>', LNonEmpty));
  LEmpty.Free;
  LNonEmpty.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TFunctionTest);

end.
