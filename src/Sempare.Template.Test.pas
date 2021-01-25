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
unit Sempare.Template.Test;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplate = class
  public
    [Test]
    procedure TestComment;
    [Test {$IFNDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}, Ignore{$ENDIF}]
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

    [Test]
    procedure TestIgnoreNL;

    [Test]
    procedure TestIgnoreNL2;

    [Test]
    procedure TestGenPhp;

    [Test]
    procedure TestList;

    [Test]
    procedure TestHtml;

    [Test]
    procedure TestParseFile;

    [Test]
    procedure TestStripWSScripts;

    [Test]
    // Not Yet Supported
    procedure TestSemiColon;

  end;

type
  TTestClass = class
    data: string;
    constructor Create(const AData: string);
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Template.Util,
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

procedure TTestTemplate.TestHtml;
type
  TRec = record
    content: string;
  end;
var
  ctx: ITemplateContext;
  data: TRec;
begin
  data.content := 'a < b';
  // no encoding
  ctx := Template.Context;
  Assert.AreEqual('a < b', //
    Template.Eval(ctx, '<% content %>', data));

  // encoding
  ctx := Template.Context;
  ctx.UseHtmlVariableEncoder;
  Assert.AreEqual('a &lt; b', //
    Template.Eval(ctx, '<% content %>', data));

  // no encoding - using print
  ctx := Template.Context;
  Assert.AreEqual('a < b', //
    Template.Eval(ctx, '<% print(content) %>', data));

  // mix where print allows for raw output
  ctx := Template.Context;
  ctx.UseHtmlVariableEncoder;
  Assert.AreEqual('a &lt; ba < b', //
    Template.Eval(ctx, '<% content %><% print(content) %>', data));

end;

procedure TTestTemplate.TestHtmlEncoding;
{$IFDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}
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

procedure TTestTemplate.TestIgnoreNL;
var
  LStringBuilder: TStringBuilder;
  LString, LResult: string;
  LPreserveGlobalNL: IPreserveValue<string>;
begin
  LPreserveGlobalNL := Preserve.Value<string>(GNewLine, #10);
  LStringBuilder := TStringBuilder.Create;
  try
    LStringBuilder.append('hello ').append(#10);
    LStringBuilder.append('world').append(#10);
    LString := LStringBuilder.ToString;
  finally
    LStringBuilder.free;
  end;

  LResult := Template.Eval(LString);
  Assert.AreEqual(LString, LResult);

  LString := '<% ignorenl %>' + LString + '<%end%>';
  LResult := Template.Eval(LString, [eoAllowIgnoreNL]);
  Assert.AreEqual('hello world', LResult);
end;

procedure TTestTemplate.TestIgnoreNL2;
var
  LStringBuilder: TStringBuilder;
  LString, LResult: string;
  LPreserveGlobalNL: IPreserveValue<string>;
begin
  LPreserveGlobalNL := Preserve.Value<string>(GNewLine, #10);
  LStringBuilder := TStringBuilder.Create;
  try
    LStringBuilder.append('<table>').append(#10);
    LStringBuilder.append('<tr>').append(#10);
    LStringBuilder.append('<td>col1</td>').append(#10);
    LStringBuilder.append('<td>col2</td>').append(#10);
    LStringBuilder.append('</tr>').append(#10);
    LStringBuilder.append('</table>').append(#10);
    LString := LStringBuilder.ToString;
  finally
    LStringBuilder.free;
  end;

  LResult := Template.Eval(LString);

  Assert.AreEqual(LString, LResult);

  LString := '<% ignorenl %>' + LString + '<%end%>';
  LResult := Template.Eval(LString, [eoAllowIgnoreNL]);
  Assert.AreEqual('<table><tr><td>col1</td><td>col2</td></tr></table>', LResult);
end;

procedure TTestTemplate.TestList;
var
  LList: TObjectList<TTestClass>;
  LContainer: record data: TObjectList<TTestClass>;
end;
LEmptyContainer:
record data: TObjectList<TTestClass>;
end;

begin
  LList := TObjectList<TTestClass>.Create();
  try
    LList.add(TTestClass.Create('a'));
    LList.add(TTestClass.Create('b'));
    LContainer.data := LList;
    LEmptyContainer.data := nil;

    Assert.AreEqual('has data', Template.Eval('<% if data %>has data<% end %>', LContainer));
    Assert.AreEqual('empty', Template.Eval('<% if not data %>empty<% end %>', LEmptyContainer));

    Assert.AreEqual('2', Template.Eval('<% _.count %>', LList));
    Assert.AreEqual('2', Template.Eval('<% data.count %>', LContainer));
    Assert.AreEqual('TObjectList<Sempare.Template.Test.TTestClass>', Template.Eval('<% typeof(data) %>', LContainer));
    Assert.AreEqual(' a b', Template.Eval('<% for x in data %> <% x.data %><% end %>', LContainer));
  finally
    LList.free;
  end;
end;

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

procedure TTestTemplate.TestParseFile;
var
  LTemplate: ITemplate;
begin
  // main thing is that we have no exception here!
  LTemplate := Template.ParseFile('..\..\demo\VelocityDemo\velocity\international.velocity');
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

procedure TTestTemplate.TestSemiColon;
begin
  Assert.WillRaise(
    procedure
    begin
      Assert.AreEqual('hello world', Template.Eval('<% a:= "hello" ; b:= "world" ; print(a + " " + b) %>'));
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

procedure TTestTemplate.TestStripWSScripts;
begin
  Assert.AreEqual('', Template.Eval('<% a := 1 |>2<| a:=3 %>'));
  Assert.AreEqual('12345678910', Template.Eval('<% for i := 1 to 10 |><%print(i)%><| end %>'));
  Assert.AreEqual('12345678910', Template.Eval('<% for i := 1 to 10 |>'#13#10'<%print(i)%>'#13#10'<| end %>'));
  Assert.AreEqual(#$D#$A'1'#$D#$A#$D#$A'2'#$D#$A#$D#$A'3'#$D#$A#$D#$A'4'#$D#$A#$D#$A'5'#$D#$A, Template.Eval('<% for i := 1 to 5 %>'#13#10'<%print(i)%>'#13#10'<% end %>'));
  Assert.AreEqual('hellomiddleworld', Template.Eval('<% print("hello") |> this should '#13#10'<% print("middle") %>'#13#10' go missing<| print("world")  %>'));
  Assert.AreEqual('12345', Template.Eval('<% for i:=1 to 5 |> <%i%>     '#13#10'<| end %>'));
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

  { TTestClass }
constructor TTestClass.Create(const AData: string);
begin
  data := AData;
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
      exit(Template.parse(AContext, '_' + ATemplate + '_'));
    end;
  Assert.AreEqual('_abc__def__abc_', Template.Eval(ctx, '<% include(''abc'') %><% include(''def'') %><% include(''abc'') %>'));
end;

procedure TTestTemplate.TestGenPhp;
begin
  Assert.AreEqual('<?php print("some text"); ?>', Template.Eval('<?php print("<% _ %>"); ?>', 'some text'));
end;

procedure TTestTemplate.TestUnderscoreIn;
var
  L: TList<string>;
begin
  L := TList<string>.Create;
  try
    L.AddRange(['1', '2', '3']);
    Assert.AreEqual('123', Template.Eval('<% for v in _ %><% v %><% end %>', L));
  finally
    L.free;
  end;
end;

procedure TTestTemplate.TestVariableNotFound;
begin
  Assert.AreEqual('', Template.Eval('<% abc %>'));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplate);

end.
