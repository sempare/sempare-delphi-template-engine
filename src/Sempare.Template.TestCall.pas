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
unit Sempare.Template.TestCall;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateCall = class
  public
    [Test]
    procedure TestFunctionCall;

    [Test]
    procedure TestMethodCall;
  end;

implementation

uses
  Sempare.Template.Context,
  Sempare.Template;

type
  TAdder = class
  public
    class function add(const a, b: extended): extended; static;
  end;

class function TAdder.add(const a, b: extended): extended;
begin
  exit(a + b);
end;

procedure TTestTemplateCall.TestFunctionCall;

var
  ctx: ITemplateContext;
begin
  ctx := Template.Context;
  ctx.functions.addfunctions(TAdder);
  Assert.AreEqual('before 22 coool after ', Template.Eval(ctx, 'before <% add(15,7) %> <% trim(''   coool   '') %> after '));
end;

type
  TMethodClass = class
  public
    function Echo(const AArg: string): string;
  end;

  TRec = record
    o: TMethodClass;
  end;

procedure TTestTemplateCall.TestMethodCall;
var
  r: TRec;
begin
  r.o := TMethodClass.Create;
  Assert.AreEqual('a', Template.Eval('<% _.Echo(''a'') %>', r.o));
  Assert.AreEqual('b', Template.Eval('<% _.o.Echo(''b'') %>', r));
  r.o.Free;
end;

{ TMethodClass }

function TMethodClass.Echo(const AArg: string): string;
begin
  exit(AArg);
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateCall);

end.
