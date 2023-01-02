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
unit Sempare.Template.TestArr;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateArr = class

  public
    [Test]
    procedure TestArray;

    [Test]
    procedure TestDerefArray;

    [Test]
    procedure TestDerefDynArray;

    [Test]
    procedure TestDynArray;
  end;

implementation

{$I 'Sempare.Template.Compiler.inc'}

uses
  Sempare.Template;

procedure TTestTemplateArr.TestArray;
var
  a: array [5 .. 10] of integer;
  i: integer;
  res: string;
begin
  for i := Low(a) to High(a) do
    a[i] := i * 2;
  res := Template.Eval('<%for i in _%><%i%> : <% _[i] %> <%end%>', a);
{$IFDEF BROKEN_ARRAY_BOUNDS}
  Assert.AreEqual('0 : 10 1 : 12 2 : 14 3 : 16 4 : 18 5 : 20 ', res);
{$ELSE}
  Assert.AreEqual('5 : 10 6 : 12 7 : 14 8 : 16 9 : 18 10 : 20 ', res);
{$ENDIF}
end;

procedure TTestTemplateArr.TestDerefArray;

var
  a: array [1 .. 10] of integer;
begin
  a[5] := 123;
{$IFDEF BROKEN_ARRAY_BOUNDS}
  Assert.AreEqual('123', Template.Eval('<% _[4] %>', a));
{$ELSE}
  Assert.AreEqual('123', Template.Eval('<% _[5] %>', a));
{$ENDIF}
end;

procedure TTestTemplateArr.TestDerefDynArray;
var
  a: tarray<integer>;
begin
  setlength(a, 10);
  a[5] := 123;
  a[6] := 321;
  Assert.AreEqual('123', Template.Eval('<% _[5] %>', a));
  Assert.AreEqual('321', Template.Eval('<% _[6] %>', a));
end;

procedure TTestTemplateArr.TestDynArray;
var
  a: tarray<integer>;
  i: integer;
begin
  setlength(a, 5);
  for i := Low(a) to High(a) do
    a[i] := i * 2;
  Assert.AreEqual('0 2 4 6 8 ', Template.Eval('<%for i in _%><% _[i]%> <%end%>', a));
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateArr);

end.
