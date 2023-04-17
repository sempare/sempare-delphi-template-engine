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
    procedure TestEmpty;
    [Test]
    procedure TestNonStmt;
    [Test]
    procedure TestHashComment;
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
    procedure TestDynamicLoader;
    [Test]
    procedure TestVariableNotFound;
    [Test]
    procedure TestVariableNotFoundException;
    [Test]
    procedure TestArray;
    [Test]
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
    // Not Yet Supported
    procedure TestSemiColon;

    [Test]
    procedure TestExtractVariablesAndFunctions;

    [Test]
    procedure TestException;

    [Test]
    procedure TestDecimalEncodingErrorWithLists;

    [Test]
    procedure TestDecimalEncodingErrorWithParameters;

    [Test]
    procedure TestValueSeparatorSameAsDecimalSeparator;

    [Test]
    procedure TestDecimalEncodingErrorWithListsDefaultValueSeparator;

    [Test]
    procedure TestVersionPresent;

    [Test]
    procedure TestTemplateAPI;

  end;

type
  TTestClass = class
    data: string;
    constructor Create(const AData: string);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Sempare.Template.Parser,
  Sempare.Template.Util,
  Sempare.Template.Functions,
  Sempare.Template.Context,
  Sempare.Template;

{ TTestTemplate }

procedure TTestTemplate.TestArray;
begin
  Assert.AreEqual('1', Template.Eval('<% a:= [1,''hello world'', 2] %><% a[0]%>'));
  Assert.AreEqual('hello world', Template.Eval('<% a:= [1,''hello world'', 2] %><% a[1]%>'));
  Assert.AreEqual('2', Template.Eval('<% a:= [1,''hello world'', 2] %><% a[2]%>'));
end;

procedure TTestTemplate.TestHashComment;
begin
  Assert.AreEqual('before after ', Template.Eval( //
    'before ' + //
    '<%#  this is '#13#10#13#10'a comment  %>' + //
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
  LResult := Template.Eval(LString);
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
  LResult := Template.Eval(LString, []);
  Assert.AreEqual('<table><tr><td>col1</td><td>col2</td></tr></table>', LResult);
end;

procedure TTestTemplate.TestList;
type
  TContainer = record
    data: TObjectList<TTestClass>;
  end;

var
  LList: TObjectList<TTestClass>;
  LContainer: TContainer;
  LEmptyContainer: TContainer;
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

procedure TTestTemplate.TestNonStmt;
begin
  Assert.AreEqual('', Template.Eval('<% %>'));
  Assert.AreEqual('       ', Template.Eval('   <% %>   <% %> '));
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
  LTemplate := Template.ParseFile('..\..\demo\SempareTemplatePlayground\templates\international.tpl');
  Assert.IsNotNull(LTemplate);
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
  Assert.AreEqual('hello world', Template.Eval('<% a:= "hello" ; b:= "world" ; print(a + " " + b) %>'));
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

{ TTestClass }
constructor TTestClass.Create(const AData: string);
begin
  data := AData;
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

procedure TTestTemplate.TestTemplateAPI;

  function EvalStream(AEval: TProc<TStream>): string;
  var
    LStream: TStringStream;
  begin
    LStream := TStringStream.Create();
    try
      AEval(LStream);
      exit(LStream.DataString);
    finally
      LStream.free;
    end;
  end;

var
  LContext: ITemplateContext;
  LParser: ITemplateParser;
  LTemplate: ITemplate;
  LTemplateRegistry: TTemplateRegistry;
  LStream: TStringStream;
  LPath: string;
  LVariables: TArray<string>;
  LFunctions: TArray<string>;
  LBlocks: TDictionary<string, ITemplate>;
begin
  LContext := Template.Context();
  Assert.IsNotNull(LContext);
  LParser := Template.Parser(LContext);
  Assert.IsNotNull(LParser);
  LParser := Template.Parser();
  Assert.IsNotNull(LParser);
  LTemplate := Template.Parse('<% "test" %>');
  Assert.AreEqual('<% print(Encode(''test'')) %>'#$D#$A, Template.PrettyPrint(LTemplate));

  // class procedure Eval(const ATemplate: string; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
  Assert.AreEqual('test', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval('<% "test" %>', AStream);
    end));

  // class procedure Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
  Assert.AreEqual('test', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval('<% _ %>', 'test', AStream);
    end));

  // class procedure Eval<T>(const ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
  LTemplate := Template.Parse('<% _ %>');
  Assert.AreEqual('test', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval(LTemplate, 'test', AStream);
    end));

  // class procedure Eval(const ATemplate: ITemplate; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
  LTemplate := Template.Parse('<% "test" %>');
  Assert.AreEqual('test', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval(LTemplate, AStream);
    end));

  LContext := Template.Context();
  LContext.Variable['var'] := 'hello';

  // class procedure Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T; const AStream: TStream); overload; static;
  LTemplate := Template.Parse('<% var %> <% _ %>');
  Assert.AreEqual('hello world', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval(LContext, LTemplate, 'world', AStream);
    end));

  // class procedure Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T; const AStream: TStream); overload; static;
  Assert.AreEqual('hello world', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval(LContext, '<% var %> <% _ %>', 'world', AStream);
    end));

  // class procedure Eval(const AContext: ITemplateContext; const ATemplate: string; const AStream: TStream); overload; static;
  Assert.AreEqual('hello world', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval(LContext, '<% "hello world" %>', AStream);
    end));

  // class procedure Eval(const AContext: ITemplateContext; const ATemplate: ITemplate; const AStream: TStream); overload; static;
  LTemplate := Template.Parse('<% var %>');
  Assert.AreEqual('hello', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.Eval(LContext, LTemplate, AStream);
    end));

  LContext := Template.Context();
  LContext.TemplateResolverWithContext := function(const AContext: ITemplateContext; const AName: string; const AResolveContext: TTemplateValue; out ACacheInContext: boolean): ITemplate
    begin
      ACacheInContext := false;
      exit(Template.Parse(AName));
    end;

  // class procedure EvalWithContext(const AContext: ITemplateContext; const ATemplate: ITemplate; const AResolveContext: TTemplateValue; const AValue: TTemplateValue; const AStream: TStream); overload; static;
  LTemplate := Template.Parse('<% include("hello") %> <% _ %>');
  Assert.AreEqual('hello world', EvalStream(
    procedure(AStream: TStream)
    begin
      Template.EvalWithContext(LContext, LTemplate, 'hello', 'world', AStream);
    end));

  // class function EvalWithContext(const AContext: ITemplateContext; const ATemplate: ITemplate; const AResolveContext: TTemplateValue; const AValue: TTemplateValue): string; overload; static;
  Assert.AreEqual('hello world', Template.EvalWithContext(LContext, LTemplate, 'hello', 'world'));

  // class function Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
  Assert.AreEqual('hello world', Template.Eval('<% "hello world" %>'));

  // class function Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
  Assert.AreEqual('hello world', Template.Eval('<% _ %>', 'hello world'));

  // class function Eval<T>(const ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
  LTemplate := Template.Parse('<% _ %>');
  Assert.AreEqual('hello world', Template.Eval(LTemplate, 'hello world'));

  // class function Eval(const ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
  LTemplate := Template.Parse('<% "hello world" %>');
  Assert.AreEqual('hello world', Template.Eval(LTemplate));

  // ---

  LContext := Template.Context();
  LContext.Variable['var'] := 'hello';

  // class function Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T): string; overload; static;
  LTemplate := Template.Parse('<% var %> <% _ %>');
  Assert.AreEqual('hello world', Template.Eval(LContext, LTemplate, 'world'));

  // class function Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T): string; overload; static;
  Assert.AreEqual('hello world', Template.Eval(LContext, '<% var %> <% _ %>', 'world'));

  // class function Eval(const AContext: ITemplateContext; const ATemplate: string): string; overload; static;
  Assert.AreEqual('hello world', Template.Eval(LContext, '<% var %> world'));

  // class function Eval(const AContext: ITemplateContext; const ATemplate: ITemplate): string; overload; static;
  LTemplate := Template.Parse('<% "hello world" %>');
  Assert.AreEqual('hello world', Template.Eval(LTemplate));

  // class function Resolver(): TTemplateRegistry; static; inline;
  LTemplateRegistry := Template.Resolver;
  Assert.IsNotNull(LTemplateRegistry);

  LTemplate := Template.Parse('<% _ %>');
  Template.Resolver.Context.SetTemplate('test', LTemplate);

  //  class procedure Resolve<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream); overload; static;
  Assert.AreEqual('hello world', EvalStream(
    procedure(AOutputStream: TStream)
    begin
      Template.Resolve('test', 'hello world', AOutputStream);
    end));

  // class function Resolve<T>(const ATemplateName: string; const AData: T): string; overload; static;
  Assert.AreEqual('hello world', Template.Resolve('test', 'hello world'));

  // class procedure Resolve(const ATemplateName: string; const AOutputStream: TStream); overload; static;
  LTemplate := Template.Parse('<% "hello world" %>');
  Template.Resolver.Context.SetTemplate('test', LTemplate);
  Assert.AreEqual('hello world', EvalStream(
    procedure(AOutputStream: TStream)
    begin
      Template.Resolve('test', AOutputStream);
    end));

  // class function Resolve(const ATemplateName: string): string; overload; static;
  Assert.AreEqual('hello world', Template.Resolve('test'));

  // ----

  Template.Resolver.Context.ClearTemplates;
  Template.Resolver.ClearTemplates;

  Template.Resolver.ContextNameResolver := function(const AName: string; const AContext: TTemplateValue): string
    begin
      exit(AName + AContext.AsString);
    end;

  LTemplate := Template.Parse('<% _ %>');
  Template.Resolver.Context.SetTemplate('test', LTemplate);

  //  class procedure ResolveWithContext<T, TContext>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream); overload; static;
  Assert.AreEqual('hello world', EvalStream(
    procedure(AOutputStream: TStream)
    begin
      Template.ResolveWithContext('te', 'st', 'hello world', AOutputStream);
    end));

  // class function ResolveWithContext<T, TContext>(const ATemplateName: string; const AContext: TContext; const AData: T): string; overload; static;
  Assert.AreEqual('hello world', Template.ResolveWithContext('te', 'st', 'hello world'));

  // class procedure ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream); overload; static;
  LTemplate := Template.Parse('<% "hello world" %>');
  Template.Resolver.Context.SetTemplate('test', LTemplate);
  Assert.AreEqual('hello world', EvalStream(
    procedure(AOutputStream: TStream)
    begin
      Template.ResolveWithContext('te', 'st', AOutputStream);
    end));

  //  class function ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string; overload; static;
  LTemplate := Template.Parse('hello <% "world" %>');
  Template.Resolver.Context.SetTemplate('test', LTemplate);

  Assert.AreEqual('hello world', Template.ResolveWithContext('te', 'st'));

  // ---
  // class function Version(): string; static;
  Assert.IsNotEmpty(Template.Version);

  // class function Parse(const AString: string): ITemplate; overload; static;
  LTemplate := Template.Parse('<% _ %>');
  Assert.AreEqual('<% print(Encode(_)) %>'#$D#$A, Template.PrettyPrint(LTemplate));

  // class function Parse(const AContext: ITemplateContext; const AString: string): ITemplate; overload; static;
  LContext := Template.Context([eoEvalVarsEarly]);
  LContext.Variable['var'] := 'test';
  LTemplate := Template.Parse(LContext, '<% var %>');
  Assert.AreEqual('<% print(Encode(''test'')) %>'#$D#$A, Template.PrettyPrint(LTemplate));

  // class function Parse(const AStream: TStream; const AManagedStream: boolean = true): ITemplate; overload; static;
  LStream := TStringStream.Create('hello world');
  LTemplate := Template.Parse(LStream);
  Assert.AreEqual('<% print(''hello world'') %>'#13#10, Template.PrettyPrint(LTemplate));

  // class function Parse(const AContext: ITemplateContext; const AStream: TStream; const AManagedStream: boolean = true): ITemplate; overload; static;
  LContext := Template.Context([eoEvalVarsEarly]);
  LContext.Variable['test'] := 'hello world';
  LStream := TStringStream.Create('<% test %>');
  LTemplate := Template.Parse(LContext, LStream);
  Assert.AreEqual('<% print(Encode(''hello world'')) %>'#13#10, Template.PrettyPrint(LTemplate));

  // class function ParseFile(const AFile: string): ITemplate; overload; static;
  LPath := TPath.GetTempFileName;
  try
    TFile.WriteAllText(LPath, 'hello world');
    LTemplate := Template.ParseFile(LContext, LPath);
    Assert.AreEqual('<% print(''hello world'') %>'#13#10, Template.PrettyPrint(LTemplate));

    // class function ParseFile(const AContext: ITemplateContext; const AFile: string): ITemplate; overload; static;
    TFile.WriteAllText(LPath, '<% test %>');
    LTemplate := Template.ParseFile(LContext, LPath);
    Assert.AreEqual('<% print(Encode(''hello world'')) %>'#13#10, Template.PrettyPrint(LTemplate));

  finally
    TFile.Delete(LPath);
  end;

  //  class procedure ExtractReferences(const ATemplate: ITemplate; out AVariables: TArray<string>; out AFunctions: TArray<string>); static;

  LTemplate := Template.Parse('<% if x = 1 ; print(trim(var)); end %>');
  Template.ExtractReferences(LTemplate, LVariables, LFunctions);
  Assert.AreEqual(2, length(LVariables));
  Assert.AreEqual('x', LVariables[0]);
  Assert.AreEqual('var', LVariables[1]);
  Assert.AreEqual(1, length(LFunctions));
  Assert.AreEqual('Trim', LFunctions[0]);

  //   class procedure ExtractBlocks(const ATemplate: ITemplate; var ABlocks: TDictionary<string, ITemplate>); static;
  LTemplate := Template.Parse('<% block "header" %>header<% end %><% block "footer" %>footer<% end %>');
  LBlocks := TDictionary<string, ITemplate>.Create;
  try
    Template.ExtractBlocks(LTemplate, LBlocks);
    Assert.AreEqual(2, LBlocks.Count);
    Assert.IsTrue(LBlocks.ContainsKey('header'));
    Assert.IsTrue(LBlocks.ContainsKey('footer'));
  finally
    LBlocks.free;
  end;
end;

procedure TTestTemplate.TestDecimalEncodingErrorWithLists;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.DecimalSeparator := ',';
  ctx.ValueSeparator := ';';
  Assert.AreEqual('', Template.Eval(ctx, '<% a := ["a"; "b"] %>'));
  Assert.WillRaise(
    procedure
    begin // expecting ;
      Assert.AreEqual('', Template.Eval(ctx, '<% a := ["a", "b"] %>'));
    end);
  ctx.DecimalSeparator := '.';
  ctx.ValueSeparator := ',';
  Assert.AreEqual('', Template.Eval(ctx, '<% a := ["a", "b"] %>'));
  Assert.WillRaise(
    procedure
    begin // expecting ,
      Assert.AreEqual('', Template.Eval(ctx, '<% a := ["a"; "b"] %>'));
    end);
end;

procedure TTestTemplate.TestDecimalEncodingErrorWithListsDefaultValueSeparator;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.DecimalSeparator := ',';
  Assert.AreEqual('', Template.Eval(ctx, '<% a := ["a", "b"] %>'));
  ctx.DecimalSeparator := '.';
  Assert.AreEqual('', Template.Eval(ctx, '<% a := ["a", "b"] %>'));
end;

procedure TTestTemplate.TestDecimalEncodingErrorWithParameters;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.DecimalSeparator := ',';
  ctx.ValueSeparator := ';';
  Assert.AreEqual('a b', Template.Eval(ctx, '<% fmt("%s %s"; "a"; "b") %>'));
  Assert.WillRaise(
    procedure
    begin // expecting ;
      Assert.AreEqual('', Template.Eval(ctx, '<% fmt("%s %s", "a", "b") %>'));
    end);
  ctx.DecimalSeparator := '.';
  ctx.ValueSeparator := ',';
  Assert.AreEqual('a b', Template.Eval(ctx, '<% fmt("%s %s", "a", "b")  %>'));
  Assert.WillRaise(
    procedure
    begin // expecting ,
      Assert.AreEqual('', Template.Eval(ctx, '<% fmt("%s %s"; "a"; "b")  %>'));
    end);
end;

procedure TTestTemplate.TestDynamicLoader;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.TemplateResolver := function(const AContext: ITemplateContext; const ATemplate: string): ITemplate
    begin
      exit(Template.Parse(AContext, '_' + ATemplate + '_'));
    end;
  Assert.AreEqual('_abc__def__abc_', Template.Eval(ctx, '<% include(''abc'') %><% include(''def'') %><% include(''abc'') %>'));
end;

type
  TMyExceptProc = class
  public
    class procedure RaiseExcept(const AValue: string); static;
  end;

class procedure TMyExceptProc.RaiseExcept(const AValue: string);
begin
  raise Exception.Create(AValue);
end;

procedure TTestTemplate.TestEmpty;
begin
  Assert.AreEqual('', Template.Eval(''));
end;

procedure TTestTemplate.TestException;
var
  LContext: ITemplateContext;
  Functions: ITemplateFunctions;
begin
  Functions := CreateTemplateFunctions;
  Functions.RegisterDefaults;
  Functions.AddFunctions(TMyExceptProc);
  LContext := Template.Context([eoEmbedException]);
  LContext.Functions := Functions;
  Assert.AreEqual(#$D#$A#$D#$A'ERROR:  (Line 1, Column 16) test'#$D#$A#$D#$A, Template.Eval(LContext, '<% RaiseExcept(''test'') %>'));
  LContext.DebugErrorFormat := '<b>Error:</b><i>%s</i>';
  Assert.AreEqual('<b>Error:</b><i> (Line 1, Column 16) test2</i>', Template.Eval(LContext, '<% RaiseExcept(''test2'') %>'));
end;

procedure TTestTemplate.TestExtractVariablesAndFunctions;
var
  LVariables: TArray<string>;
  LFunctions: TArray<string>;
  LTemplate: ITemplate;
begin
  LTemplate := Template.Parse('<% v1 %> <% for i in _ %> <% x := substr("abc",1,2) %> <% end %> <% firstname %> <% lastname %>');
  Template.ExtractReferences(LTemplate, LVariables, LFunctions);
  Assert.AreEqual(4, integer(length(LVariables)));
  Assert.AreEqual('v1', LVariables[0]);
  Assert.AreEqual('_', LVariables[1]);
  Assert.AreEqual('firstname', LVariables[2]);
  Assert.AreEqual('lastname', LVariables[3]);
  Assert.AreEqual(1, integer(length(LFunctions)));
  Assert.AreEqual('SubStr', LFunctions[0]);
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

procedure TTestTemplate.TestValueSeparatorSameAsDecimalSeparator;
var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.DecimalSeparator := ',';
  ctx.ValueSeparator := ',';
  Assert.AreEqual('1,12 4,56 ', Template.Eval(ctx, '<% a := [ 1,12 , 4,56 ]  %><% for i of a %><% i %> <% end %>'));
  Assert.WillRaise(
    procedure
    begin // expects ,
      Assert.AreEqual('1,12 4,56 ', Template.Eval(ctx, '<% a := [ 1,12 ; 4,56 ]  %><% for i of a %><% i %> <% end %>'));
    end);
end;

procedure TTestTemplate.TestVariableNotFound;
begin
  Assert.AreEqual('', Template.Eval('<% abc %>'));
end;

procedure TTestTemplate.TestVariableNotFoundException;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context();
  LCtx.Options := LCtx.Options + [eoRaiseErrorWhenVariableNotFound];
  Assert.WillRaise(
    procedure
    begin // expects  abc
      Assert.AreEqual('', Template.Eval(LCtx, '<% abc %>'));
    end);
end;

procedure TTestTemplate.TestVersionPresent;
begin
  Assert.IsNotEmpty(Template.Version);
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplate);

end.
