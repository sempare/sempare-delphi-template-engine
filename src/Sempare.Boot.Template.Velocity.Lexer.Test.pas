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
unit Sempare.Boot.Template.Velocity.Lexer.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityLexer = class
  public

    [Test]
    procedure TestVelocityLexer;
  end;

implementation

uses
  System.classes,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.Lexer,
  Sempare.Boot.Template.Velocity;

procedure TTestVelocityLexer.TestVelocityLexer;

var
  m: tstringstream;
  Lexer: IVelocityLexer;
  symbol: IVelocitySymbol;
  vs: IVelocityValueSymbol;
  val: string;
begin
  m := tstringstream.create('before <% if (true) %>hello<% end %> after');
  Lexer := CreateVelocityLexer(Velocity.Context, m);
  while true do
  begin
    symbol := Lexer.GetToken;
    if symbol.Token = VsEOF then
      break;
    val := '';
    if symbol.QueryInterface(IVelocityValueSymbol, vs) = 0 then
    begin
      vs := symbol as IVelocityValueSymbol;
      val := vs.Value;
    end;
  end;
end;

initialization

// TDUnitX.RegisterTestFixture(TTestVelocityLexer);

end.
