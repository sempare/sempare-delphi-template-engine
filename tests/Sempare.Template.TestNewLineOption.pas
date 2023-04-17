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
unit Sempare.Template.TestNewLineOption;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestNewLineOption = class
  public
    [Test]
    procedure TestNewLine;
    [Test]
    procedure TestNewLineWithCustomStreamWriter;
    [Test]
    procedure TestNewLineWithContext;
    [Test]
    procedure TestNL;
    [Test]
    procedure TestIgnoreWS;
    [Test]
    procedure TestStripLeft;
    [Test]
    procedure TestStripRight;
    [Test]
    procedure TestStripRecurringSpacesOption;
    [Test]
    procedure TestStripRecurringNewlineOption;
    [Test]
    procedure TestTrimLinesOption;
    [Test]
    procedure TestTabsToSpacesOption;
    [Test]
    procedure TestWhitespaceReplacement;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Sempare.Template,
  Sempare.Template.Context,
  Sempare.Template.Evaluate;

{ TTestNewLineOption }

procedure TTestNewLineOption.TestNewLineWithContext;
type
  TRec = record
    Value: string;
    description: string;
  end;
var
  r: TRec;
  s: string;
  ctx: ITemplateContext;
begin
  ctx := Template.Context();
  ctx.newline := #10;
  r.Value := 'a value';
  r.description := 'some desc';
  s := Template.Eval(ctx, 'Value: <% value %>'#$D#$A'Description: <% description %>', r);
  Assert.AreEqual('Value: a value'#$A'Description: some desc', s);
end;

procedure TTestNewLineOption.TestNewLineWithCustomStreamWriter;
type
  TRec = record
    Value: string;
    description: string;
  end;
var
  r: TRec;
  s: string;
  ctx: ITemplateContext;
begin
  ctx := Template.Context();
  ctx.StreamWriterProvider := function(const AStream: TStream; const AContext: ITemplateContext): TStreamWriter
    begin
      exit(TStreamWriter.Create(AStream));
    end;
  r.Value := 'a value';
  r.description := 'some desc';
  s := Template.Eval(ctx, 'Value: <% value %>'#$D#$A'Description: <% description %>', r);
  Assert.AreEqual('Value: a value'#$D#$A'Description: some desc', s);
end;

procedure TTestNewLineOption.TestNL;
var
  s: string;
begin
  s := Template.Eval('<% print("hello" + chr(13) + chr(10) + crnl) %>');
  Assert.AreEqual('hello'#13#10#13#10, s);
end;

procedure TTestNewLineOption.TestStripLeft;
begin
  Assert.AreEqual('begin'#13#10'x  '#13#10'end', Template.Eval('begin'#13#10'   <%- "x" %>  '#13#10'end'));
  Assert.AreEqual('begin x  '#13#10'end', Template.Eval('begin'#13#10'   <%+ "x" %>  '#13#10'end'));
  Assert.AreEqual('beginx  '#13#10'end', Template.Eval('begin'#13#10'   <%* "x" %>  '#13#10'end'));
end;

procedure TTestNewLineOption.TestStripRecurringNewlineOption;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context([eoStripRecurringNewlines]);
  Assert.AreEqual('abc'#13#10'text'#13#10'text'#13#10'end', Template.Eval(LCtx, 'abc'#13#10#13#10#13#10'text'#13#10#13#10'text'#13#10'end'));
end;

procedure TTestNewLineOption.TestStripRecurringSpacesOption;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context([eoStripRecurringSpaces]);
  Assert.AreEqual('abc text text end', Template.Eval(LCtx, 'abc   text  text end'));
end;

procedure TTestNewLineOption.TestStripRight;
begin
  Assert.AreEqual('begin'#13#10'   x'#13#10'end', Template.Eval('begin'#13#10'   <% "x" -%>  '#13#10'end'));
  Assert.AreEqual('begin'#13#10'   x end', Template.Eval('begin'#13#10'   <% "x" +%>  '#13#10'end'));
  Assert.AreEqual('begin'#13#10'   xend', Template.Eval('begin'#13#10'   <% "x" *%>  '#13#10'end'));
end;

procedure TTestNewLineOption.TestTabsToSpacesOption;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context([eoConvertTabsToSpaces]);
  Assert.AreEqual('abc text  text', Template.Eval(LCtx, 'abc'#9'text'#9#9'text'));
end;

procedure TTestNewLineOption.TestTrimLinesOption;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context([eoTrimLines]);
  Assert.AreEqual('abc1'#13#10'test2'#13#10'test3'#13#10, Template.Eval(LCtx, '    '#9'abc1'#9#13#10'    '#9'test2 '#9#13#10'    '#9'test3 '#9#13#10));
end;

procedure TTestNewLineOption.TestWhitespaceReplacement;
var
  LCtx: ITemplateContext;
begin
  LCtx := Template.Context();
  Assert.AreEqual('  x  ', Template.Eval(LCtx, '  x  '));
  LCtx.WhitespaceChar := #183;
  Assert.AreEqual(#183#183'x'#183#183, Template.Eval(LCtx, '  x  '));
end;

procedure TTestNewLineOption.TestIgnoreWS;
begin
  Assert.AreEqual(#13#10, Template.Eval('<% ignorews %>    '#13#10'<%end%>'));
  Assert.AreEqual(' '#13#10' ', Template.Eval(' <% ignorews %>    '#13#10'<%end%> '));
  Assert.AreEqual('  ', Template.Eval(' <% ignorenl ; ignorews %>    '#13#10'<% end; end %> '));
end;

procedure TTestNewLineOption.TestNewLine;
type
  TRec = record
    Value: string;
    description: string;
  end;
var
  r: TRec;
  s: string;
begin
  r.Value := 'a value';
  r.description := 'some desc';
  s := Template.Eval(#$D#$A'Value: <% value %>'#$D#$A#$D#$A'Description: <% description %>'#$D#$A, r);
  Assert.AreEqual(#$D#$A'Value: a value'#$D#$A#$D#$A'Description: some desc'#$D#$A, s);
end;

initialization

TDUnitX.RegisterTestFixture(TTestNewLineOption);

end.
