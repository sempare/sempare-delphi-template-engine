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
unit Sempare.Template.TestInclude;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateInclude = class
  public
    [Test]
    procedure TestInclude;

    [Test]
    procedure TestIncludeMissingData;

    [Test]
    procedure TestIncludeData;

    [Test]
    procedure TestRecursiveIncludes;

    [Test]
    procedure TestRecursiveIncludes2;

    [Test]
    procedure TestSubTemplate;

    [Test]
    procedure TestExtends;

    [Test]
    procedure TestExtendsBlock;

    [Test]
    procedure TestExtendsBlockWithDynamicNames;

    [Test]
    procedure TestExtendsWebLike;

    [Test]
    procedure TestExtendsNested;

    [Test]
    procedure TestExtendsScopedExpr;

    [Test]
    procedure TestWebForm;

    [Test]
    procedure TestTimedWebForm;

    [Test]
    procedure TestThreadedWebForm;

    [Test]
    procedure TestNestedBody;

    [Test]
    procedure TestNestedBody2;

    [Test]
    procedure TestExtractBlocks;

    [Test]
    procedure TestResolver;

  end;

implementation

uses
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  System.Math,
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Diagnostics,
  System.Generics.Collections,
  Sempare.Template.Util,
  Sempare.Template.Context,
  Sempare.Template;

type
  THeaderContentFooter = record
    header: record
      Name: string;
    end;

    content: TList<string>;

    footer: record
      Year: integer;
    end;
  end;

procedure TTestTemplateInclude.TestIncludeData;
var
  ctx: ITemplateContext;
  x: record value: string;
end;
begin
  ctx := Template.Context;
  ctx.Template['test'] := Template.parse('test <% _ %>');
  ctx.Template['test2'] := Template.parse('test <% value %>');

  // illustrate simple template
  Assert.AreEqual('test 123', Template.Eval(ctx, 'test <% _ %>', '123'));

  // test using include - stackframe is preserved
  Assert.AreEqual('test 123', Template.Eval(ctx, '<%include (''test'') %>', '123'));

  // test using include - variable referenced is not provided
  // important to note: include() uses with() to create a scope, so a record or class must be provided
  x.value := '123';
  Assert.AreEqual('test 123', Template.Eval(ctx, '<%include (''test2'', _) %>', x));

end;

procedure TTestTemplateInclude.TestIncludeMissingData;

begin
  Assert.WillRaise(
    procedure
    var
      ctx: ITemplateContext;
      x: record value: string;
    end;

    begin
      ctx := Template.Context;
      ctx.Template['test'] := Template.parse('test <% value %>');
      x.value := '123';
      Assert.AreEqual('test 123', Template.Eval(ctx, '<%include (''test'', missingvar) %>', x));
    end);
end;

procedure TTestTemplateInclude.TestRecursiveIncludes;
type
  TMember = record
    Name: String;

    Members: TArray<TMember>;
  end;

var
  LTpl: ITemplate;
  LMember: TMember;

begin
  LTpl := Template.parse( //
    '<% template ''showmember'' %>' + //
    '<% name %>' + //
    '<% if members %>' + //
    '<% for member of members %>' + //
    #13#10 + //
    '<% include (''showmember'', member) %>' + //
    '<% end %>' + //
    '<% end %>' + //
    '<% end %>' + //
    '<% include (''showmember'', _) %>');

  LMember.Name := 'Parent';

  SetLength(LMember.Members, 1);
  LMember.Members[0].Name := 'Child';

  Assert.AreEqual('Parent'#13#10'Child', Template.Eval(LTpl, LMember));
end;

procedure TTestTemplateInclude.TestRecursiveIncludes2;
type
  TMember = record
    Name: String;

    Members: TArray<TMember>;
  end;

var
  LTpl: ITemplate;
  LMember: TMember;

begin
  LTpl := Template.parse( //
    '<% template ''showmember'' %>' + //
    '<% name %>' + //
    '<% if members %>' + //
    '<% for member in members %>' + //
    #13#10 + //
    '<% include (''showmember'', members[member]) %>' + //
    '<% end %>' + //
    '<% end %>' + //
    '<% end %>' + //
    '<% include (''showmember'', _) %>');

  LMember.Name := 'Parent';

  SetLength(LMember.Members, 1);
  LMember.Members[0].Name := 'Child';

  Assert.AreEqual('Parent'#13#10'Child', Template.Eval(LTpl, LMember));
end;

procedure TTestTemplateInclude.TestInclude;
var
  c: ITemplate;
  x: THeaderContentFooter;
  ctx: ITemplateContext;

begin
  ctx := Template.Context;

  ctx.Template['header'] := Template.parse('header <% header.name %> <% footer.year %>');
  ctx.Template['footer'] := Template.parse('footer Copyright <% footer.year %>');

  c := Template.parse( //
    '<% suffix := ''er''%><% include (''head'' + suffix) %>' + //
    '<%for v in content %>' + '<% v %>' + //
    '<% end %>' + //
    '<% include (''foot'' + suffix) %>');

  x.content := TList<string>.create;
  x.content.Add('conrad');
  x.content.Add('christa');
  x.header.Name := 'Sempare Ltd';
  x.footer.Year := 2020;
  try
    Assert.AreEqual('header Sempare Ltd 2020conradchristafooter Copyright 2020', Template.Eval(ctx, c, x));
  finally
    x.content.Free;
  end;
end;

procedure TTestTemplateInclude.TestSubTemplate;
type
  TTemplate = record
    header: record
      company: string;
    end;

    footer: record
      copyright: integer;
    end;
  end;
var
  info: TTemplate;
begin
  info.header.company := 'sempare limited';
  info.footer.copyright := 2023;
  Assert.AreEqual('sempare limited'#13#10'Copyright (c) 2023'#13#10, Template.Eval( //
    '<% template ''prefix'' %><% Company %>'#13#10'<% end %>' + //
    '<% template ''suffix'' %>Copyright (c) <% Copyright %>'#13#10'<% end %>' + //
    '<% header.company %>'#13#10 + //
    'Copyright (c) <% footer.copyright %>'#13#10 //
    , info));

  Assert.AreEqual('sempare limited'#13#10, Template.Eval( //
    '<% template ''prefix'' %><% Company %>'#13#10'<% end %>' + //
    '<% include(''prefix'', header) %>' //
    , info));

  Assert.AreEqual('Copyright (c) 2023'#13#10, Template.Eval( //
    '<% template ''suffix'' %>Copyright (c) <% Copyright %>'#13#10'<% end %>' + //
    '<% include(''suffix'', footer) %>', info));

  Assert.AreEqual( //
    'sempare limited'#13#10 + //
    'sempare limited'#13#10 + //
    'Copyright (c) 2023'#13#10 + //
    'Copyright (c) 2023'#13#10, //
    Template.Eval( //
    '<% template ''prefix'' %><% Company %>'#13#10'<% end %>' + //
    '<% template ''suffix'' %>Copyright (c) <% Copyright %>'#13#10'<% end %>' + //
    '<% include(''prefix'', header) %>' + //
    '<% header.company %>'#13#10 + //
    'Copyright (c) <% footer.copyright %>'#13#10 + //
    '<% include(''suffix'', footer) %>' //
    , info));

end;

procedure TTestTemplateInclude.TestExtends;
begin
  Assert.AreEqual('parent parent', Template.Eval( //
    '<% template ''showmember'' %>' + //
    '<% block ''content'' %>parent<% end %>' + //
    '<% end %>' + //
    '<% extends (''showmember'') %>' + //
    '<% end %> ' + //
    '<% extends (''showmember'') %>' + //
    '<% end %>' //
    ));
end;

procedure TTestTemplateInclude.TestExtendsBlock;
begin
  Assert.AreEqual('child child2', Template.Eval( //
    '<% template ''showmember'' %>' + //
    '<% block ''content'' %>parent<% end %>' + //
    '<% end %>' + //
    '<% extends (''showmember'') %>' + //
    '<% block ''content'' %>child<% end %>' + //
    '<% end %> ' + //
    '<% extends (''showmember'') %>' + //
    '<% block ''content'' %>child2<% end %>' + //
    '<% end %>' //
    ));
end;

procedure TTestTemplateInclude.TestExtendsBlockWithDynamicNames;
begin
  Assert.AreEqual('child child2', Template.Eval( //
    '<% for i := 1 to 2 %>' + //
    '<% template ''showmember'' + i %>' + //
    '<% block ''content'' %>parent<% end %>' + //
    '<% end %>' + //
    '<% end %>' + //
    '<% extends (''showmember'' + 1) %>' + //
    '<% block ''content'' %>child<% end %>' + //
    '<% end %> ' + //
    '<% extends (''showmember'' + 2) %>' + //
    '<% block ''content'' %>child2<% end %>' + //
    '<% end %>' //
    ));
end;

procedure TTestTemplateInclude.TestExtendsWebLike;
begin
  Assert.AreEqual('header content footer', Template.Eval( //
    '<% template ''header'' %>' + //
    'header' + //
    '<% end %>' + //
    '<% template ''footer'' %>' + //
    'footer' + //
    '<% end %>' + //
    '<% template ''template'' %>' + //
    '<% include(''header'') %> ' + //
    '<% block ''body'' %>body<% end %> ' + //
    '<% include(''footer'') %>' + //
    '<% end %>' + //
    '<% extends (''template'') %>' + //
    '<% block ''body'' %>content<% end %>' + //
    '<% end %>' //
    ));
end;

procedure TTestTemplateInclude.TestExtendsNested;
begin
  Assert.AreEqual('header default header default body footer default footer', Template.Eval( //
    '<% template ''header'' %>' + //
    'header <% block ''general'' %>default header<% end %>' + //
    '<% end %>' + //
    '<% template ''footer'' %>' + //
    'footer <% block ''general'' %>default footer<% end %>' + //
    '<% end %>' + //
    '<% template ''template'' %>' + //
    '<% include(''header'') %> ' + //
    '<% block ''general'' %>default body<% end %> ' + //
    '<% include(''footer'') %>' + //
    '<% end %>' + //
    '<% extends (''template'') %>' + //
    '<% end %>' //
    ));

  Assert.AreEqual('header general general footer general', Template.Eval( //
    '<% template ''header'' %>' + //
    'header <% block ''general'' %>default header<% end %>' + //
    '<% end %>' + //
    '<% template ''footer'' %>' + //
    'footer <% block ''general'' %>default footer<% end %>' + //
    '<% end %>' + //
    '<% template ''template'' %>' + //
    '<% include(''header'') %> ' + //
    '<% block ''general'' %>default body<% end %> ' + //
    '<% include(''footer'') %>' + //
    '<% end %>' + //
    '<% extends (''template'') %>' + //
    '<% block ''general'' %>general<% end %>' + //
    '<% end %>' //
    ));
end;

procedure TTestTemplateInclude.TestExtendsScopedExpr;
begin
  Assert.AreEqual('first 1 second 2 ', Template.Eval( //
    '<% template ''template'' %>' + //
    '<% block ''general'' %>body<% end %> <% _ %> ' + //
    '<% end %>' + //
    '<% extends (''template'', 1) %>' + //
    '<% block ''general'' %>first<% end %> ' + //
    '<% end %>' + //
    '<% extends (''template'', 2) %>' + //
    '<% block ''general'' %>second<% end %> ' + //
    '<% end %>' //
    ));
end;

type
  TField = record
    Caption: string;
    Name: string;
    FieldType: string;
    constructor create(const ACaption, AName: string; const AFieldType: string = 'TEdit');
  end;

  TButton = record
    Caption: string;
    Name: string;
    constructor create(const ACaption, AName: string);
  end;

constructor TField.create(const ACaption, AName, AFieldType: string);
begin
  Caption := ACaption;
  name := AName;
  FieldType := AFieldType;
end;

constructor TButton.create(const ACaption, AName: string);
begin
  Caption := ACaption;
  name := AName;
end;

{$IFDEF WIN32}

function GetNanoseconds: Int64;
var
  frequency, counter: Int64;
begin
  QueryPerformanceFrequency(frequency);
  QueryPerformanceCounter(counter);
  Result := (counter * 1000000000) div frequency;
end;
{$ENDIF}

type
  TTemplateData = record
    company: string;
    CopyrightYear: integer;
    FormName: string;
    FormAction: string;
    Fields: TArray<TField>;
    Buttons: TArray<TButton>;
  end;

const
  ExpectedResult = #$D#$A#$D#$A#$D#$A#$D#$A#$D#$A#$D#$A#$D#$A#$D#$A#$D#$A'<html>'#$D#$A' <head>'#$D#$A + //
    '  <title>Welcome to my Sempare</title>'#$D#$A' </head>'#$D#$A' <body>'#$D#$A#$D#$A#$D#$A + //
    '   <form method="POST" name="userinfo" action="/userinfo">'#$D#$A'      <table>'#$D#$A'         '#$D#$A + //
    '             <tr><td>FirstName</td><td><input name="firstname"></td></tr>'#$D#$A'         '#$D#$A + //
    '             <tr><td>LastName</td><td><input name="lastname"></td></tr>'#$D#$A'         '#$D#$A + //
    '             <tr><td>Email</td><td><input type="email" name="email"></td></tr>'#$D#$A'         '#$D#$A + //
    '         <tr>'#$D#$A'             <td colspan="2" align="right">'#$D#$A'               '#$D#$A + //
    '                 <input type="button" name="submit" value="Submit">'#$D#$A'               '#$D#$A + //
    '             </td>'#$D#$A'         </tr>'#$D#$A'      </table>'#$D#$A'   </form>'#$D#$A#$D#$A#$D#$A + //
    '  <p>Copyright (c) 2023 </p>'#$D#$A' </body>'#$D#$A'</html>'#$D#$A#$D#$A#$D#$A;

procedure ThreadDetail(const AFailed: pinteger; const ANumEvals: integer; const ATemplate: ITemplate; const ATemplateData: TTemplateData);
var
  LResult: string;
  i: integer;
  LStopWatch: TStopwatch;
  LElapsedMs: double;
begin

  LStopWatch := TStopwatch.create;
  LStopWatch.Start;

  for i := 0 to ANumEvals do
  begin
    try
      LResult := Template.Eval(ATemplate, ATemplateData);
      if ExpectedResult <> LResult then
      begin
        AtomicIncrement(AFailed^);
      end;
    except
      AtomicIncrement(AFailed^);
    end;
  end;
  LStopWatch.Stop;
  LElapsedMs := LStopWatch.ElapsedMilliseconds / ANumEvals;

  if LElapsedMs > GetTestTimeTollerance(0.25, 6.0) then
    AtomicIncrement(AFailed^);

end;

procedure TTestTemplateInclude.TestThreadedWebForm;
var
  LTemplateData: TTemplateData;
  LTemplate: ITemplate;
  i: integer;
  LNumThreads: integer;
  LNumEvals: integer;
  LFailed: integer;
  LEvent: TCountDownEvent;
begin
  LTemplateData.company := 'Sempare';
  LTemplateData.CopyrightYear := 2023;
  LTemplateData.FormName := 'userinfo';
  LTemplateData.FormAction := '/userinfo';
  SetLength(LTemplateData.Fields, 3);
  LTemplateData.Fields[0] := TField.create('FirstName', 'firstname');
  LTemplateData.Fields[1] := TField.create('LastName', 'lastname');
  LTemplateData.Fields[2] := TField.create('Email', 'email', 'TEmail');
  SetLength(LTemplateData.Buttons, 1);
  LTemplateData.Buttons[0] := TButton.create('Submit', 'submit');

  LTemplate := Template.parse( //
    '<% template "TEdit" %><tr><td><% Caption %></td><td><input name="<% name %>"></td></tr><% end %>'#13#10 + // 1

    '<% template "TEmail" %><tr><td><% Caption %></td><td><input type="email" name="<% name %>"></td></tr><% end %>'#13#10 + // 2

    '<% template "TButton" %><input type="button" name="<% name %>" value="<% caption %>"><% end %>'#13#10 + // 3

    '<% template "TForm" %>'#13#10 + // 4
    '   <form method="POST" name="<% FormName %>" action="<% FormAction %>">'#13#10 + // 5
    '      <table>'#13#10 + // 6
    '         <% for field of fields %>'#13#10 + // 7
    '             <% include(field.FieldType, field)%>'#13#10 + // 8
    '         <% end %>'#13#10 + // 9
    '         <tr>'#13#10 + //
    '             <td colspan="2" align="right">'#13#10 + // 10
    '               <% for button of buttons %>'#13#10 + // 11
    '                 <% include("TButton", button) %>'#13#10 + // 12
    '               <% end %>'#13#10 + // 13
    '             </td>'#13#10 + // 14
    '         </tr>'#13#10 + // 14
    '      </table>'#13#10 + // 15
    '   </form>'#13#10 + // 16
    '<% end %>'#13#10 + // 17

    '<% template "header" %>'#13#10 + // 18
    '<html>'#13#10 + // 19
    ' <head>'#13#10 + // 20
    '  <title>Welcome to my <% Company %></title>'#13#10 + // 21
    ' </head>'#13#10 + // 22
    ' <body>'#13#10 + // 23
    '<% end %>'#13#10 + // 24

    '<% template "footer" %>'#13#10 + // 25
    '  <p>Copyright (c) <% CopyrightYear %> </p>'#13#10 + // 26
    ' </body>'#13#10 + // 27
    '</html>'#13#10 + // 28
    '<% end %>'#13#10 + // 29

    '<% template "template" %>'#13#10 + // 30
    '<% include("header") %>'#13#10 + // 31
    '<% block "body" %>Lorem ipsum dolor sit amet, consectetur adipiscing eli...<% end %>'#13#10 + // 32
    '<% include("footer") %>'#13#10 + // 33
    '<% end %>'#13#10 + // 34

    '<% extends ("template") %>' + // 35
    '<% block "body" %><% include("TForm") %><% end %> '#13#10 + // 36
    '<% end %>'#13#10 // 37
    );

  LEvent := nil;
  try
    LNumThreads := min(1, CPUCount);
    LEvent := TCountDownEvent.create(LNumThreads);

    LNumEvals := 500;
    LFailed := 0;

    for i := 1 to LNumThreads do
    begin
      TThread.CreateAnonymousThread(
        procedure
        begin
          ThreadDetail(@LFailed, LNumEvals, LTemplate, LTemplateData);
          LEvent.Signal();
        end).Start;
    end;
    LEvent.WaitFor();
    Assert.AreEqual(0, LFailed);
  finally
    LEvent.Free;
  end;
end;

procedure TTestTemplateInclude.TestTimedWebForm;
type
  TTemplateData = record
    company: string;
    CopyrightYear: integer;
    FormName: string;
    FormAction: string;
    Fields: TArray<TField>;
    Buttons: TArray<TButton>;
  end;

var
  LTemplateData: TTemplateData;
  LTemplate: ITemplate;
  i: integer;
  LStopWatch: TStopwatch;
  LElapsedMs: double;
  LIterations: integer;
begin
  LTemplateData.company := 'Sempare';
  LTemplateData.CopyrightYear := 2023;
  LTemplateData.FormName := 'userinfo';
  LTemplateData.FormAction := '/userinfo';

  SetLength(LTemplateData.Fields, 3);
  LTemplateData.Fields[0] := TField.create('FirstName', 'firstname');
  LTemplateData.Fields[1] := TField.create('LastName', 'lastname');
  LTemplateData.Fields[2] := TField.create('Email', 'email', 'TEmail');
  SetLength(LTemplateData.Buttons, 1);
  LTemplateData.Buttons[0] := TButton.create('Submit', 'submit');

  LTemplate := Template.parse( //
    '<% template "TEdit" %><tr><td><% Caption %></td><td><input name="<% name %>"></td></tr><% end %>'#13#10 + // 1

    '<% template "TEmail" %><tr><td><% Caption %></td><td><input type="email" name="<% name %>"></td></tr><% end %>'#13#10 + // 2

    '<% template "TButton" %><input type="button" name="<% name %>" value="<% caption %>"><% end %>'#13#10 + // 3

    '<% template "TForm" %>'#13#10 + // 4
    '   <form method="POST" name="<% FormName %>" action="<% FormAction %>">'#13#10 + // 5
    '      <table>'#13#10 + // 6
    '         <% for field of fields %>'#13#10 + // 7
    '             <% include(field.FieldType, field)%>'#13#10 + // 8
    '         <% end %>'#13#10 + // 9
    '         <tr>'#13#10 + //
    '             <td colspan="2" align="right">'#13#10 + // 10
    '               <% for button of buttons %>'#13#10 + // 11
    '                 <% include("TButton", button) %>'#13#10 + // 12
    '               <% end %>'#13#10 + // 13
    '             </td>'#13#10 + // 14
    '         </tr>'#13#10 + // 14
    '      </table>'#13#10 + // 15
    '   </form>'#13#10 + // 16
    '<% end %>'#13#10 + // 17

    '<% template "header" %>'#13#10 + // 18
    '<html>'#13#10 + // 19
    ' <head>'#13#10 + // 20
    '  <title>Welcome to my <% Company %></title>'#13#10 + // 21
    ' </head>'#13#10 + // 22
    ' <body>'#13#10 + // 23
    '<% end %>'#13#10 + // 24

    '<% template "footer" %>'#13#10 + // 25
    '  <p>Copyright (c) <% CopyrightYear %> </p>'#13#10 + // 26
    ' </body>'#13#10 + // 27
    '</html>'#13#10 + // 28
    '<% end %>'#13#10 + // 29

    '<% template "template" %>'#13#10 + // 30
    '<% include("header") %>'#13#10 + // 31
    '<% block "body" %>Lorem ipsum dolor sit amet, consectetur adipiscing eli...<% end %>'#13#10 + // 32
    '<% include("footer") %>'#13#10 + // 33
    '<% end %>'#13#10 + // 34

    '<% extends ("template") %>' + // 35
    '<% block "body" %><% include("TForm") %><% end %> '#13#10 + // 36
    '<% end %>'#13#10 // 37
    );

  LIterations := 500;
  LStopWatch := TStopwatch.create;
  LStopWatch.Start;
  for i := 1 to LIterations do
  begin
    Template.Eval(LTemplate, LTemplateData);
  end;
  LStopWatch.Stop;
  LElapsedMs := LStopWatch.ElapsedMilliseconds / LIterations;
{$IF defined( WIN32) OR defined(WIN64)}
  Assert.IsTrue(LElapsedMs <= GetTestTimeTollerance(0.25, 6.0));
{$ENDIF}
end;

procedure TTestTemplateInclude.TestWebForm;

type

  TTemplateData = record
    company: string;
    CopyrightYear: integer;
    FormName: string;
    FormAction: string;
    Fields: TArray<TField>;
    Buttons: TArray<TButton>;
  end;

var
  LTemplateData: TTemplateData;
  LResult: string;
begin
  LTemplateData.company := 'Sempare';
  LTemplateData.CopyrightYear := 2023;
  LTemplateData.FormName := 'userinfo';
  LTemplateData.FormAction := '/userinfo';

  SetLength(LTemplateData.Fields, 3);
  LTemplateData.Fields[0] := TField.create('FirstName', 'firstname');
  LTemplateData.Fields[1] := TField.create('LastName', 'lastname');
  LTemplateData.Fields[2] := TField.create('Email', 'email', 'TEmail');
  SetLength(LTemplateData.Buttons, 1);
  LTemplateData.Buttons[0] := TButton.create('Submit', 'submit');

  LResult := Template.Eval( //
    '<% template "TEdit" %><tr><td><% Caption %></td><td><input name="<% name %>"></td></tr><% end %>'#13#10 + // 1

    '<% template "TEmail" %><tr><td><% Caption %></td><td><input type="email" name="<% name %>"></td></tr><% end %>'#13#10 + // 2

    '<% template "TButton" %><input type="button" name="<% name %>" value="<% caption %>"><% end %>'#13#10 + // 3

    '<% template "TForm" %>'#13#10 + // 4
    '   <form method="POST" name="<% FormName %>" action="<% FormAction %>">'#13#10 + // 5
    '      <table>'#13#10 + // 6
    '         <% for field of fields %>'#13#10 + // 7
    '             <% include(field.FieldType, field)%>'#13#10 + // 8
    '         <% end %>'#13#10 + // 9
    '         <tr>'#13#10 + //
    '             <td colspan="2" align="right">'#13#10 + // 10
    '               <% for button of buttons %>'#13#10 + // 11
    '                 <% include("TButton", button) %>'#13#10 + // 12
    '               <% end %>'#13#10 + // 13
    '             </td>'#13#10 + // 14
    '         </tr>'#13#10 + // 14
    '      </table>'#13#10 + // 15
    '   </form>'#13#10 + // 16
    '<% end %>'#13#10 + // 17

    '<% template "header" %>'#13#10 + // 18
    '<html>'#13#10 + // 19
    ' <head>'#13#10 + // 20
    '  <title>Welcome to my <% Company %></title>'#13#10 + // 21
    ' </head>'#13#10 + // 22
    ' <body>'#13#10 + // 23
    '<% end %>'#13#10 + // 24

    '<% template "footer" %>'#13#10 + // 25
    '  <p>Copyright (c) <% CopyrightYear %> </p>'#13#10 + // 26
    ' </body>'#13#10 + // 27
    '</html>'#13#10 + // 28
    '<% end %>'#13#10 + // 29

    '<% template "template" %>'#13#10 + // 30
    '<% include("header") %>'#13#10 + // 31
    '<% block "body" %>Lorem ipsum dolor sit amet, consectetur adipiscing eli...<% end %>'#13#10 + // 32
    '<% include("footer") %>'#13#10 + // 33
    '<% end %>'#13#10 + // 34

    '<% extends ("template") %>' + // 35
    '<% block "body" %><% include("TForm") %><% end %> '#13#10 + // 36
    '<% end %>'#13#10 // 37
    , LTemplateData);

  // LResult := LResult.Replace(#13#10, '''#13#10''', [rfReplaceAll]);

  Assert.AreEqual(#13#10#13#10#13#10#13#10#13#10#13#10#13#10#13#10#13#10'<html>'#13#10' <head>'#13#10'  <title>Welcome to my Sempare</title>'#13#10 + //
    ' </head>'#13#10' <body>'#13#10''#13#10''#13#10'   <form method="POST" name="userinfo" action="/userinfo">'#13#10 + //
    '      <table>'#13#10'         '#13#10'             <tr><td>FirstName</td><td><input name="firstname">' + //
    '</td></tr>'#13#10'         '#13#10'             <tr><td>LastName</td><td><input name="lastname"></td></tr>'#13#10 + //
    '         '#13#10'             <tr><td>Email</td><td><input type="email" name="email"></td></tr>'#13#10'         '#13#10 + //
    '         <tr>'#13#10'             <td colspan="2" align="right">'#13#10'               '#13#10 + //
    '                 <input type="button" name="submit" value="Submit">'#13#10'               '#13#10'             </td>'#13#10'         </tr>'#13#10 + //
    '      </table>'#13#10'   </form>'#13#10''#13#10''#13#10'  <p>Copyright (c) 2023 </p>'#13#10' </body>'#13#10'</html>'#13#10''#13#10''#13#10, LResult);
end;

procedure TTestTemplateInclude.TestNestedBody;
begin
  Assert.AreEqual('hellohello', Template.Eval( //
    '<% template "tpl1" %>' + //
    '<% block "content" %>tpl1<% end %>' + //
    '<% end %>' + //

    '<% template "template" %>' + //
    '<% include("tpl1") %>' + //
    '<% block "content" %>tpl1<% end %>' + //
    '<% end %>' + //

    '<% extends ("template") %>' + //
    ' // this is ignored ' + //
    '<% block "content" %>hello<% end %>' + //
    ' // this is ignored ' + //
    '<% end %>' + //
    ''));
end;

procedure TTestTemplateInclude.TestNestedBody2;
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.parse( //
    '<% template "tpl1" %>' + //
    '     <% block "content" %>tpl1<% end %>' + //
    '<% end %>' + //

    '<% template "template" %>' + //
    '     <% extends ("tpl1") %>' + //
    '          // this is ignored ' + //
    '          <% block "content" %><% _ %><% end %>' + //
    '          // this is ignored ' + //
    '     <% end %>' + //
    '     <% block "content" %>tpl1<% end %>' + //
    '<% end %>' + //

    '<% extends ("template") %>' + //
    '     // this is ignored ' + //
    '     <% block "content" %>hello<% end %>' + //
    '     // this is ignored ' + //
    '<% end %>' + //
    '');

  Assert.AreEqual('          hello123     hello', Template.Eval(LTemplate, 'hello123'));
  Assert.AreEqual('          hello456     hello', Template.Eval(LTemplate, 'hello456'));
end;

procedure TTestTemplateInclude.TestExtractBlocks;
var
  LBlocks: TDictionary<string, ITemplate>;
  LTemplate: ITemplate;
begin
  LTemplate := Template.parse('<% block "content" %>i am content<% end %><% block "footer" %>i am footer<% end %>');
  LBlocks := TDictionary<string, ITemplate>.create;
  try
    Template.ExtractBlocks(LTemplate, LBlocks);
    Assert.IsTrue(LBlocks.ContainsKey('content'));
    Assert.IsTrue(LBlocks.ContainsKey('footer'));
    Assert.AreEqual('i am content', Template.Eval(LBlocks['content']));
    Assert.AreEqual('i am footer', Template.Eval(LBlocks['footer']));
  finally
    LBlocks.Free;
  end;
end;

procedure TTestTemplateInclude.TestResolver;
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.parse('hello <% _ %>');
  Template.Resolver.Context.SetTemplate('index', LTemplate);

  LTemplate := Template.parse('hola <% _ %>');
  Template.Resolver.Context.SetTemplate('index_es', LTemplate);

  LTemplate := Template.parse('hallo <% _ %>');
  Template.Resolver.Context.SetTemplate('index_de', LTemplate);

  LTemplate := Template.parse('你好 <% _ %>');
  Template.Resolver.Context.SetTemplate('index_zh', LTemplate);

  Template.Resolver.ContextNameResolver := function(const AName: string; const AContext: TTemplateValue): string
    var
      LLang: string;
    begin
      LLang := AContext.AsString;
      if LLang.IsEmpty then
        exit(AName)
      else
        exit(AName + '_' + LLang);
    end;

  Assert.AreEqual('hello world', Template.Resolve('index', 'world'));
  Assert.AreEqual('hola world', Template.ResolveWithContext('index', 'es', 'world'));
  Assert.AreEqual('hallo world', Template.ResolveWithContext('index', 'de', 'world'));
  Assert.AreEqual('你好 world', Template.ResolveWithContext('index', 'zh', 'world'));
  Assert.AreEqual('hello world', Template.ResolveWithContext('index', 'en', 'world')); // not found and should default to 'index'
  Assert.AreEqual('hello world', Template.ResolveWithContext('index', 'af', 'world')); // not found and should default to 'index'

end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateInclude);

end.
