 (*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                        Sempare Templating Engine                                                 *
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
unit Sempare.Template.NewLineOption.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestNewLineOption = class
  public
    [Test]
    procedure TestRecurringSpaces;
    [Test]
    procedure TestRecurringNLAndSpaces;
    [Test]
    procedure TestRecurringOnlyNL;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  Sempare.Template.Context,
  Sempare.Template.Evaluate;

{ TTestNewLineOption }

procedure TTestNewLineOption.TestRecurringNLAndSpaces;
var
  s: TStringStream;
  w: TNewLineStreamWriter;
begin
  s := TStringStream.create;
  w := TNewLineStreamWriter.create(s, TEncoding.ASCII, #10, [eoTrimLines, eoStripRecurringNewlines]);
  try
    w.Write(#10#10#10#10#10'     hello     '#10#10#10#10'    world   '#10#10#10#10);
  finally
    w.Free;
    Assert.AreEqual('hello'#10'world'#10, s.datastring);
    s.Free;
  end;
end;

procedure TTestNewLineOption.TestRecurringOnlyNL;
var
  s: TStringStream;
  w: TNewLineStreamWriter;
begin
  s := TStringStream.create;
  w := TNewLineStreamWriter.create(s, TEncoding.ASCII, #10, [eoStripRecurringNewlines]);
  try
    w.Write(#10#10#10#10#10'     hello     '#10#10#10#10'    world   '#10#10#10#10);
  finally
    w.Free;
    Assert.AreEqual('     hello     '#10'    world   '#10, s.datastring);
    s.Free;
  end;
end;

procedure TTestNewLineOption.TestRecurringSpaces;
var
  s: TStringStream;
  w: TNewLineStreamWriter;
begin
  s := TStringStream.create;
  w := TNewLineStreamWriter.create(s, TEncoding.ASCII, #10, [eoTrimLines]);
  try
    w.Write('     hello     '#10#10'    world   ');
  finally
    w.Free;
    Assert.AreEqual('hello'#10#10'world', s.datastring);
    s.Free;
  end;
end;

end.
