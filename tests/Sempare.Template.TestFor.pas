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
    procedure TestWhileWithEvent;
    [Test]
    procedure TestForRangeWithEvent;
    [Test]
    procedure TestForInEnumWithEvent;
    [Test]
    procedure TestForInWithOffset;
    [Test]
    procedure TestForInWithOffsetAndLimit;
    [Test]
    procedure TestForRangeWithStep;
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
    [Test{$IFNDEF SEMPARE_TEMPLATE_FIREDAC}, Ignore{$ENDIF}]
    procedure TestDataSetWithEvent;
    [Test{$IFNDEF SEMPARE_TEMPLATE_FIREDAC}, Ignore{$ENDIF}]
    procedure TestDataSet;
    [Test{$IFNDEF SEMPARE_TEMPLATE_FIREDAC}, Ignore{$ENDIF}]
    procedure TestDataSetCount;
    [Test]
    procedure TestCycle;
    [Test]
    procedure TestForOnEvent;
    [Test]
    procedure TestForInDict;
    [Test]
    procedure TestForOfArray;
    [Test]
    procedure TestForOfDynArray;
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

procedure TTestTemplateFor.TestDataSetWithEvent;
var
  ds: TDataSet;
begin
  ds := CreateMockUsersTable();
  try
    Assert.AreEqual('<ul><li>joe</li><li>pete</li><li>jane</li></ul>', //
      Template.Eval('<% for i in _ %><li><% _[''name''] %></li><% onbegin%><ul><%onend%></ul><%end%>', ds));

    ds.Delete();
    ds.Delete();
    ds.Delete();

    Assert.AreEqual('<h1>No values</h1>', //
      Template.Eval('<% for i in _ %><li><% _[''name''] %></li><% onbegin%><ul><%onend%></ul><% onempty%><h1>No values</h1><%end%>', ds));

  finally
    ds.Free;
  end;
end;

procedure TTestTemplateFor.TestDataSetCount;
var
  ds: TDataSet;
begin
  ds := CreateMockUsersTable();
  try
    Assert.AreEqual('3', //
      Template.Eval('<% RecordCount(_) %>', ds));
  finally
    ds.Free;
  end;
end;
{$ELSE}

procedure TTestTemplateFor.TestDataSetWithEvent;
begin
end;

procedure TTestTemplateFor.TestDataSet;
begin
end;

procedure TTestTemplateFor.TestDataSetCount;
begin

end;

{$ENDIF}

procedure TTestTemplateFor.TestCycle;
begin
  Assert.AreEqual('odd even odd even ', //
    Template.Eval('<% for i in [ 1, 2, 3, 4] %><% cycle(''odd'',''even'') %> <% end %>'));

  Assert.AreEqual('1 2 1 ', //
    Template.Eval('<% for i in [ 1, 2, 3] %><% cycle(''1'',''2'') %> <% end %>'));

  Assert.AreEqual('odd-row even-row odd-row ', //
    Template.Eval('<% i := 0; while true ; if i = 3; break; end; cycle ("odd-row ","even-row "); i:=i+1; end; %>'));

end;

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

procedure TTestTemplateFor.TestForInEnumWithEvent;
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
    x.range.AddRange([3, 5, 7, 9]);
    c := Template.parse('<% for i in range %><li><%i%></li><%onbegin%><ul><%onend%></ul><%onempty%>empty<%end%>');
    Assert.AreEqual('<ul><li>3</li><li>5</li><li>7</li><li>9</li></ul>', Template.Eval(c, x));

    c := Template.parse('<% for i in range %><%i%><%betweenitems%>|<%onbegin%>begin<%onend%>end<%onempty%>empty<%end%>');
    Assert.AreEqual('begin3|5|7|9end', Template.Eval(c, x));

    x.range.Clear;
    c := Template.parse('<% for i in range %><li><%i%></li><%onbegin%><ul><%onend%></ul><%onempty%>empty<% end %>');
    Assert.AreEqual('empty', Template.Eval(c, x));

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

procedure TTestTemplateFor.TestForRangeWithEvent;
var
  c: ITemplate;
begin
  c := Template.parse('<% for i := 7 to 12 step 2 %><li><%i%></li><%onbegin%><ul><%onend%></ul><%onempty%>empty<% end %>');
  Assert.AreEqual('<ul><li>7</li><li>9</li><li>11</li></ul>', Template.Eval(c));

  c := Template.parse('<% for i := 7 to 6 %><li><%i%></li><%onbegin%><ul><%onend%></ul><%onempty%>empty<% end %>');
  Assert.AreEqual('empty', Template.Eval(c));

  c := Template.parse('<% for i := 7 to 12 step 2 %><%i%><%betweenitems%>|<%onbegin%>begin<%onend%>end<%onempty%>empty<% end %>');
  Assert.AreEqual('begin7|9|11end', Template.Eval(c));
end;

procedure TTestTemplateFor.TestForRangeWithStep;
var
  c: ITemplate;
begin
  c := Template.parse('<% for i := 0 to 10 step 2 %> <% i %> <% end %>');
  Assert.AreEqual(' 0  2  4  6  8  10 ', Template.Eval(c));
end;

procedure TTestTemplateFor.TestForInWithOffset;
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
    x.range.AddRange([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    c := Template.parse('<% for i in range offset 5 %> <% i %> <% end %>');
    Assert.AreEqual(' 6  7  8  9  10 ', Template.Eval(c, x));
  finally
    x.range.Free;
  end;
end;

procedure TTestTemplateFor.TestForInWithOffsetAndLimit;
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
    x.range.AddRange([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    c := Template.parse('<% for i in range offset 5 limit 2 %> <% i %> <% end %>');
    Assert.AreEqual(' 6  7 ', Template.Eval(c, x));
  finally
    x.range.Free;
  end;
end;

procedure TTestTemplateFor.TestForOfArray;
var
  LArray: array [6 .. 10] of integer;
  i: integer;
begin
  for i := Low(LArray) to High(LArray) do
    LArray[i] := i - 5;
  Assert.AreEqual('6 7 8 9 10 | 1 2 3 4 5 ', Template.Eval('<% for i in _ %><% i %> <%end%>| <% for i in _ %><% _[i] %> <%end%>', LArray));
  Assert.AreEqual('1 2 3 4 5 ', Template.Eval('<% for i of _ %><% i %> <%end%>', LArray));
end;

procedure TTestTemplateFor.TestForOfDynArray;
var
  LArray: TArray<integer>;
  i: integer;
begin
  setlength(LArray, 5);
  for i := Low(LArray) to High(LArray) do
    LArray[i] := i * 5;
  Assert.AreEqual('0 1 2 3 4 | 0 5 10 15 20 ', Template.Eval('<% for i in _ %><% i %> <%end%>| <% for i in _ %><% _[i] %> <%end%>', LArray));
  Assert.AreEqual('0 5 10 15 20 ', Template.Eval('<% for i of _ %><% i %> <%end%>', LArray));
end;

procedure TTestTemplateFor.TestForOnEvent;
begin
  Assert.AreEqual('empty', Template.Eval('<% for i in [] %><% i %><% onbegin%>start<% onend %>end<% onempty %>empty<%end%>'));
  Assert.AreEqual('start4321end', Template.Eval('<% v := [4,3,2,1] %><% for i in v %><% v[i] %><% onbegin%>start<% onend %>end<% onempty %>empty<%end%>'));
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

procedure TTestTemplateFor.TestWhileWithEvent;
var
  c: ITemplate;
begin
  c := Template.parse('<% i := 0%><% while i < 3 %><li><%i%><% i := i + 1%></li><%onbegin%><ul><%onend%></ul><%onempty%>empty<% end %>');
  Assert.AreEqual('<ul><li>0</li><li>1</li><li>2</li></ul>', Template.Eval(c));

  c := Template.parse('<% i := 0%><% while i < 0 %><li><%i%><% i := i + 1%></li><%onbegin%><ul><%onend%></ul><%onempty%>empty<% end %>');
  Assert.AreEqual('empty', Template.Eval(c));

  c := Template.parse('<% i := 0%><% while i < 3 %><%i%><% i := i + 1%><%betweenitems%>,<%onbegin%>begin<%onend%>end<%onempty%>empty<% end %>');
  Assert.AreEqual('begin0,1,2end', Template.Eval(c));
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
  Assert.AreEqual('before            after', Template.Eval('before <% for a := 1 to 10 %> <% end %> after'));
end;

procedure TTestTemplateFor.TestSimpleForDownTo;
begin
  Assert.AreEqual('before  after', Template.Eval('before <% for a := 1 downto 10 %> <% end %> after'));
  Assert.AreEqual('before            after', Template.Eval('before <% for a := 10 downto 1 %> <% end %> after'));
end;

procedure TTestTemplateFor.TestSimpleForin;
begin
  Assert.IsNotNull(Template.parse('before <% for a in b %> pre <% a %> post  <% end %> '));
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

procedure TTestTemplateFor.TestForInDict;

var
  LDict: TDictionary<string, integer>;
begin
  LDict := TDictionary<string, integer>.Create();
  try
    LDict.Add('a', 1);
    LDict.Add('b', 2);
    Assert.AreEqual(' b 2 a 1', Template.Eval('<%for i in _ %> <% i.key %> <% i.value %><%end%>', LDict));
  finally
    LDict.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateFor);

end.
