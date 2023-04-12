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
unit Sempare.Template.TestIf;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateIf = class
  private
    procedure Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);

  public
    [Test]
    procedure TestElIf;
    [Test]
    procedure TestEmptyStringAsIfCondition;
    [Test]
    procedure TestNestedIf;
    [Test]
    procedure TestSimpleIf;
    [Test]
    procedure TestSimpleIfFalse;
    [Test]
    procedure TestIfWithBreak;
    [Test]
    procedure TestIfWithContinue;
    [Test]
    procedure TestIfArray;
    [Test]
    procedure TestIfDynArray;
    [Test]
    procedure TestIfList;
    [Test]
    procedure TestIfDict;
    [Test{$IFNDEF SEMPARE_TEMPLATE_FIREDAC}, Ignore {$ENDIF}]
    procedure TestIfDataSet;
    [Test]
    procedure TestElIf2;
  end;

implementation

uses
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}
  Data.DB,
  FireDAC.Comp.Client,
{$ENDIF}
  SysUtils,
  System.Generics.Collections,
  Sempare.Template;

type
  TStrCond = record
    Val: string;
  end;

{$IFDEF SEMPARE_TEMPLATE_FIREDAC}

function CreateMockUsersTable(): TFDMemTable;
var
  ds: TFDMemTable;
begin
  ds := TFDMemTable.Create(nil);
  with ds do
  begin
    FieldDefs.Add('name', ftWideString, 20);
    FieldDefs.Add('age', ftInteger);
    FieldDefs.Add('weight', ftFloat);
    CreateDataSet;
  end;
  with ds do
  begin
    Append;
    FieldByName('name').value := 'joe';
    FieldByName('age').value := 5;
    FieldByName('weight').value := 15;
    Post;
  end;
  exit(ds);
end;

{$ENDIF}

procedure TTestTemplateIf.TestEmptyStringAsIfCondition;
var
  Data: TStrCond;
begin
  Assert.AreEqual('ok', Template.Eval('<% if not val %>ok<% end %>', Data));
  Data.Val := 'something';
  Assert.AreEqual('ok', Template.Eval('<% if val %>ok<% end %>', Data));
end;

procedure TTestTemplateIf.TestIfArray;
var
  Data0: array [0 .. 0] of string;
  DataB: array [boolean] of string;
  Data: array [0 .. 1] of string;
begin
  Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', DataB));
  Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', Data0));
  Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', Data));
end;

{$IFDEF SEMPARE_TEMPLATE_FIREDAC}

procedure TTestTemplateIf.TestIfDataSet;
var
  Data: TDataSet;
begin
  Data := nil;
  Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
  try
    Data := CreateMockUsersTable;
    Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', Data));
    Data.First;
    Data.Delete;
    Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
  finally
    Data.free;
  end;
end;
{$ELSE}

procedure TTestTemplateIf.TestIfDataSet;
begin
end;
{$ENDIF}

procedure TTestTemplateIf.TestIfDict;
var
  Data: TDictionary<string, string>;
begin
  Data := nil;
  Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
  try
    Data := TDictionary<string, string>.Create;
    Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
    Data.Add('test', 'test');
    Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', Data));
  finally
    Data.free;
  end;
end;

procedure TTestTemplateIf.TestIfDynArray;
var
  Data: TArray<string>;
begin
  setlength(Data, 0);
  Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
  setlength(Data, 2);
  Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', Data));
end;

procedure TTestTemplateIf.TestIfList;
var
  Data: TList<string>;
begin
  Data := nil;
  Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
  try
    Data := TList<string>.Create;
    Assert.AreEqual('', Template.Eval('<% if _ %>there are values<% end %>', Data));
    Data.Add('test');
    Assert.AreEqual('there are values', Template.Eval('<% if _ %>there are values<% end %>', Data));
  finally
    Data.free;
  end;
end;

procedure TTestTemplateIf.TestIfWithBreak;
begin
  Assert.WillRaise(
    procedure
    begin
      Template.parse('<% if (true) %> <% break%> <% end %> ');
    end);
end;

procedure TTestTemplateIf.TestIfWithContinue;
begin
  Assert.WillRaise(
    procedure
    begin
      Template.parse('<% if (true) %> <% continue%> <% end %> ');
    end);
end;

procedure TTestTemplateIf.TestNestedIf;
begin
  Assert.IsNotNull(Template.parse('before <% if (true) %> pre <% if (true) %> midd;e <% end %> post <% end %> '));
end;

type
  TIf = record
    //
    showname: boolean;
    Name: string;
  end;

procedure TTestTemplateIf.TestSimpleIf;
var
  x: TIf;

begin
  x.showname := true;
  x.Name := 'conrad';
  Test(x, 'before   conrad  ', 'before <% if showname %>  <% name %> <% end %> ');
end;

procedure TTestTemplateIf.TestSimpleIfFalse;
var
  x: TIf;

begin
  x.showname := false;
  x.Name := 'conrad';
  Test(x, 'hello  conrad ', 'hello <% if showname %> should not be shown <% else %> <% name %> <% end %>');
end;

type
  TElIf = record
    Val: integer;
    other: boolean;
  end;

procedure TTestTemplateIf.Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
var
  c: ITemplate;
begin
  c := Template.parse(ATemplate);
  Assert.AreEqual(AExpect, Template.Eval(c, AValue));
end;

procedure TTestTemplateIf.TestElIf;
var
  T: ITemplate;
  r: TElIf;
begin
  T := Template.parse( //
    '<% if val=1 %>' + //
    'one' + //
    '<% elif val=2 %>' + //
    '         <% if other %>2<% else %>two<% end %>' + //
    '<% else %>' + //
    '         <% if other %>3<% else %>three<% end %>' + //
    '<% end %>');
  // writeln(Template.PrettyPrint(T));
  r.Val := 1;
  Assert.AreEqual('one', Template.Eval(T, r));

  r.Val := 2;
  r.other := true;
  Assert.AreEqual('2', trim(Template.Eval(T, r)));

  r.Val := 2;
  r.other := false;
  Assert.AreEqual('two', trim(Template.Eval(T, r)));

  r.Val := 3;
  r.other := true;
  Assert.AreEqual('3', trim(Template.Eval(T, r)));

  r.Val := 3;
  r.other := false;
  Assert.AreEqual('three', trim(Template.Eval(T, r)));

end;

procedure TTestTemplateIf.TestElIf2;
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.parse('<% i := num(_)%>' + //
    '<% if i = 42 %>42' + //
    '<% elif i = 43 %>43' + //
    '<% elif i = 44 %>44' + //
    '<% else %>other' + //
    '<% end %>');
  Assert.AreEqual('other', Template.Eval(LTemplate, 1));
  Assert.AreEqual('other', Template.Eval(LTemplate, 41));
  Assert.AreEqual('42', Template.Eval(LTemplate, 42));
  Assert.AreEqual('43', Template.Eval(LTemplate, 43));
  Assert.AreEqual('44', Template.Eval(LTemplate, 44));
  Assert.AreEqual('other', Template.Eval(LTemplate, 45));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateIf);

end.
