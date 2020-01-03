(*%****************************************************************************
 *  ___                                             ___               _       *
 * / __|  ___   _ __    _ __   __ _   _ _   ___    | _ )  ___   ___  | |_     *
 * \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)   | _ \ / _ \ / _ \ |  _|    *
 * |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|   |___/ \___/ \___/  \__|    *
 *                     |_|                                                    *
 ******************************************************************************
 *                                                                            *
 *                        VELOCITY TEMPLATE ENGINE                            *
 *                                                                            *
 *                                                                            *
 *          https://www.github.com/sempare/sempare.boot.velocity.oss          *
 ******************************************************************************
 *                                                                            *
 * Copyright (c) 2019 Sempare Limited,                                        *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>           *
 *                                                                            *
 * Contact: info@sempare.ltd                                                  *
 *                                                                            *
 * Licensed under the Apache License, Version 2.0 (the "License");            *
 * you may not use this file except in compliance with the License.           *
 * You may obtain a copy of the License at                                    *
 *                                                                            *
 *   http://www.apache.org/licenses/LICENSE-2.0                               *
 *                                                                            *
 * Unless required by applicable law or agreed to in writing, software        *
 * distributed under the License is distributed on an "AS IS" BASIS,          *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   *
 * See the License for the specific language governing permissions and        *
 * limitations under the License.                                             *
 *                                                                            *
 ****************************************************************************%*)
unit Sempare.Boot.Template.Velocity.NewLineOption.Test;

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
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity.Evaluate;

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
