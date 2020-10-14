(*%*********************************************************************************
 *                 ___                                                             *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                     *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                    *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                    *
 *                                    |_|                                          *
 ***********************************************************************************
 *                                                                                 *
 *                        Sempare Templating Engine                                *
 *                                                                                 *
 *                                                                                 *
 *               https://github.com/sempare/sempare.template                       *
 ***********************************************************************************
 *                                                                                 *
 * Copyright (c) 2020 Sempare Limited,                                             *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>                *
 *                                                                                 *
 * Contact: info@sempare.ltd                                                       *
 *                                                                                 *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License            *
 * You may not use this file except in compliance with one of these Licenses.      *
 * You may obtain a copy of the Licenses at                                        *
 *                                                                                 *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                    *
 * https://github.com/sempare/sempare.template/docs/commercial.license.md          *
 *                                                                                 *
 * Unless required by applicable law or agreed to in writing, software             *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,              *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.        *
 * See the License for the specific language governing permissions and             *
 * limitations under the License.                                                  *
 *                                                                                 *
 ********************************************************************************%*)
unit Sempare.Template.Test;

interface

uses
  DUnitX.TestFramework;

type

  // [TestFixture]
  TTestTemplate = class
  public
    [Test]
    procedure TestComment;
    [Test]
    procedure TestHtmlEncoding;
    [Test]
    procedure TestNonMutation;
    [Test]
    procedure TestNoSpace;
    [Test]
    procedure TestStartEndToken;
    [Test]
    procedure TestTabToSpace;
    [Test]
    procedure TestTabToSpaceAndNoSpace;
    [Test]
    procedure TestUnderscoreIn;
    [Test]
    procedure TestSubTemplate;
    [Test]
    procedure TestDynamicLoader;
    [Test]
    procedure TestVariableNotFound;
    [Test]
    procedure TestArray;
    [Test, Ignore]
    // This is ignored because this is a potential future feature that is not currently supported.
    procedure TestStmts;
    [Test]
    procedure TestRequire;
    [Test]
    procedure testPrint;
  end;

implementation

{$I 'Sempare.Template.Compiler.inc'}

uses
  System.Generics.Collections,
  Sempare.Template.Context,
  Sempare.Template;

{ TTestTemplate }

procedure TTestTemplate.TestArray;
begin
  Assert.AreEqual('1', Template.Eval('<% a:= [1,''hello world'', 2] %><% a[0]%>'));
  Assert.AreEqual('hello world', Template.Eval('<% a:= [1,''hello world'', 2] %><% a[1]%>'));
  Assert.AreEqual('2', Template.Eval('<% a:= [1,''hello world'', 2] %><% a[2]%>'));
end;

procedure TTestTemplate.TestComment;
begin
  Assert.AreEqual('before after ', Template.Eval( //
    'before ' + //
    '<% (* this is '#13#10#13#10'a comment *) %>' + //
    'after ' //
    ));
end;

procedure TTestTemplate.TestHtmlEncoding;
{$IFDEF SUPPORT_NET_ENCODING}
type
  TRec = record
    content: string;
  end;
var
  ctx: ITemplateContext;
  data: TRec;
begin
  data.content := 'a < b';
  ctx := Template.Context;
  ctx.UseHtmlVariableEncoder;

  Assert.AreEqual('<html><body>a &lt; b</body></html>', Template.Eval(ctx, '<html><body><% content %></body></html>', data));

end;
{$ELSE}

begin
end;
{$ENDIF}

procedure TTestTemplate.TestNonMutation;
type
  TRec = record
    Val: string;
  end;
var
  r: TRec;
begin
  r.Val := 'a value';
  Template.Eval('<% val := ''test'' %>', r);
  Assert.AreEqual('a value', r.Val);
end;

procedure TTestTemplate.TestNoSpace;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.Options := [eoStripRecurringSpaces];
  Assert.AreEqual(' hello world ', Template.Eval(ctx, '  hello   world    '));
end;

procedure TTestTemplate.testPrint;
begin
  Assert.AreEqual('hello', Template.Eval('<% print(''hello'') %>'));
end;

procedure TTestTemplate.TestRequire;
type
  TInfo = record
    Name: string;
  end;
var
  info: TInfo;
begin
  Assert.AreEqual('', Template.Eval('<% require(''TInfo'') %>', info));
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual('', Template.Eval('<% require(''TNotFound'') %>', info));
    end);
end;

procedure TTestTemplate.TestStartEndToken;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Assert.AreEqual('hello', Template.Eval(ctx, '{{ if true }}hello{{else}}bye{{end}}'));
end;

procedure TTestTemplate.TestStmts;
begin
  Assert.AreEqual('1', Template.Eval('<% a := 1; print(a) %>'));
end;

type
  TTemplate = record
    header: record
      company: string;
    end;

    footer: record
      copyright: integer;
    end;

  end;

procedure TTestTemplate.TestSubTemplate;
var
  info: TTemplate;
begin
  info.header.company := 'sempare ltd';
  info.footer.copyright := 2019;
  Assert.AreEqual('sempare ltd  Copyright (c) 2019', Template.Eval('<% template ''prefix'' %><%company%><% end %>' + '<% template ''suffix'' %> Copyright (c) <%copyright%><% end %>' +
    '<%include(''prefix'', _.header)%> <%include(''suffix'', _.footer)%>', info));
end;

procedure TTestTemplate.TestTabToSpace;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.Options := [eoConvertTabsToSpaces];
  Assert.AreEqual(' hello world ', Template.Eval(ctx, #9'hello'#9'world'#9));
end;

procedure TTestTemplate.TestTabToSpaceAndNoSpace;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.Options := [eoStripRecurringSpaces, eoConvertTabsToSpaces];
  Assert.AreEqual(' hello world', Template.Eval(ctx, #9' hello '#9'  world'));
end;

procedure TTestTemplate.TestDynamicLoader;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.TemplateResolver := function(AContext: ITemplateContext; const ATemplate: string): ITemplate
    begin
      result := Template.parse(AContext, '_' + ATemplate + '_');
    end;
  Assert.AreEqual('_abc__def__abc_', Template.Eval(ctx, '<% include(''abc'') %><% include(''def'') %><% include(''abc'') %>'));
end;

procedure TTestTemplate.TestUnderscoreIn;
var
  L: TList<string>;
begin
  L := TList<string>.create;
  try
    L.AddRange(['1', '2', '3']);
    Assert.AreEqual('123', Template.Eval('<% for v in _ %><% v %><% end %>', L));
  finally
    L.Free;
  end;
end;

procedure TTestTemplate.TestVariableNotFound;
begin
  Assert.AreEqual('', Template.Eval('<% abc %>'));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplate);

end.
