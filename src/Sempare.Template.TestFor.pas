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
unit Sempare.Template.TestFor;

interface
{$I 'Sempare.Template.Compiler.inc'}

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateFor = class
  public
    [Test]
    procedure TestForIn;
    [Test]
    procedure TestForRangeWithBreak;
    [Test]
    procedure TestForRangeWithContinue;
    [Test]
    procedure TestWhileWithBreak;
    [Test]
    procedure TestSimpleFor;
    [Test]
    procedure TestSimpleForDownTo;
    [Test]
    procedure TestSimpleForin;
    [Test]
    procedure TestStructure;
    [Test]
    procedure TestNested;
    [Test]
    procedure TestNestedBreak;
    [Test]
    procedure TestWithInlineArray;
    [Test]
    procedure TestWithEmptyInlineArray;
    [Test{$IFNDEF SEMPARE_TEMPLATE_FIREDAC}, IGNORE{$ENDIF}]
    procedure TestDataSet;
  end;

implementation

uses
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}
  Data.DB,
  FireDAC.Comp.Client,
{$ENDIF}
  System.Generics.Collections,
  Sempare.Template;

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
    Append;
    FieldByName('name').value := 'pete';
    FieldByName('age').value := 6;
    FieldByName('weight').value := 20;
    Post;
    Append;
    FieldByName('name').value := 'jane';
    FieldByName('age').value := 7;
    FieldByName('weight').value := 25;
    Post;
  end;
  exit(ds);
end;

procedure TTestTemplateFor.TestDataSet;
var
  ds: TDataSet;
begin
  ds := CreateMockUsersTable();
  try
    Assert.AreEqual('joe pete jane ', //
      Template.Eval('<% for i in _ %><% _[''name''] %> <%end%>', ds));
  finally
    ds.Free;
  end;
end;
{$ELSE}

procedure TTestTemplateFor.TestDataSet;
begin
end;
{$ENDIF}

procedure TTestTemplateFor.TestForIn;
type
  TForIn = record
    range: TList<integer>;
  end;
var
  c: ITemplate;
  x: TForIn;
begin
  x.range := TList<integer>.Create;
  try
    x.range.AddRange([1, 10, 100]);
    c := Template.parse('<% for i in range %> <% i %> <% end %>');
    Assert.AreEqual(' 1  10  100 ', Template.Eval(c, x));
  finally
    x.range.Free;
  end;
end;

type
  TForRange = record
    count: integer;
  end;

procedure TTestTemplateFor.TestForRangeWithBreak;
var
  c: ITemplate;
  x: TForRange;
begin
  x.count := 10;
  c := Template.parse('<% for i := 0 to count %> <% i %> <% if i = 5%> <% break %> <% end %> <% end %>');
  Assert.AreEqual(' 0   1   2   3   4   5  ', Template.Eval(c, x));
end;

procedure TTestTemplateFor.TestForRangeWithContinue;
var
  x: TForRange;
begin
  x.count := 10;
  Assert.AreEqual('0 2 4 6 8 10 ', Template.Eval('<% for i := 0 to count %><% if i mod 2 = 1 %><% continue %><% end %><% i %> <% end %>', x));
end;

procedure TTestTemplateFor.TestNested;
begin
  Assert.AreEqual('1:5,6,;2:5,6,;3:5,6,;', Template.Eval('<% for i := 1 to 3 %><% i %>:<% for j := 5 to 6 %><% j %>,<% end %>;<% end%>'));
end;

procedure TTestTemplateFor.TestNestedBreak;
begin
  Assert.AreEqual('1:5,6,;2:5,6,;3:5,6,;', Template.Eval('<% for i := 1 to 3 %><% i %>:<% for j := 5 to 10 %><% if j > 6%><%break%><%end%><% j %>,<% end %>;<% end%>'));
end;

procedure TTestTemplateFor.TestWhileWithBreak;
var
  x: record //
    count: integer;
end;

begin
  x.count := 9;
  Assert.AreEqual('0123456789', Template.Eval('<% i := 0 %><% while i <= count %><% i %><% i := i + 1 %><% end %>', x));
end;

procedure TTestTemplateFor.TestWithEmptyInlineArray;
var
  c: ITemplate;
begin
  c := Template.parse('<%range := []%><% for i in range %> <% range[i] %> <% end %>');
  Assert.AreEqual('', Template.Eval(c));
end;

procedure TTestTemplateFor.TestWithInlineArray;
var
  c: ITemplate;
begin
  c := Template.parse('<%range := [1,10,100]%><% for i in range %> <% range[i] %> <% end %>');
  Assert.AreEqual(' 1  10  100 ', Template.Eval(c));
end;

procedure TTestTemplateFor.TestSimpleFor;
begin
  Template.parse('before <% for a := 1 to 10 %> after <% end %> ');
end;

procedure TTestTemplateFor.TestSimpleForDownTo;
begin
  Template.parse('before <% for a := 1 downto 10 %> after <% end %> ');
end;

procedure TTestTemplateFor.TestSimpleForin;
begin
  Template.parse('before <% for a in b %> pre <% a %> post  <% end %> ');
end;

type
  TInfo = record
    Name: string;
    age: integer;
    constructor Create(const n: string; const a: integer);
  end;

constructor TInfo.Create(const n: string; const a: integer);
begin
  name := n;
  age := a;
end;

procedure TTestTemplateFor.TestStructure;

var
  info: TList<TInfo>;
begin
  info := TList<TInfo>.Create;
  try
    info.AddRange([TInfo.Create('conrad', 10), TInfo.Create('christa', 20)]);
    Assert.AreEqual(' conrad 10 christa 20', Template.Eval('<%for i in _ %> <% i.name %> <% i.age %><%end%>', info));
  finally
    info.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateFor);

end.
