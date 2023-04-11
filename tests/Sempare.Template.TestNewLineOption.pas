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
  ctx.StreamWriterProvider := function(const AStream: TStream; AContext: ITemplateContext): TStreamWriter
    begin
      result := TStreamWriter.Create(AStream);
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
