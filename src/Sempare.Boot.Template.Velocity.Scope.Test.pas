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
unit Sempare.Boot.Template.Velocity.Scope.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityScope = class
  public

    [Test]
    procedure TestScopeClass;

  end;

implementation

uses
  System.Rtti,
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity.Scope,
  Sempare.Boot.Template.Velocity;

procedure TTestVelocityScope.TestScopeClass;

var
  S, s2: TVariableScope;
begin
  S := TVariableScope.create();
  S['root'] := 'root123';
  S['name'] := 'conrad';
  Assert.AreEqual('root123', S.Item['root'].AsString);
  Assert.AreEqual('conrad', S.Item['name'].AsString);
  s2 := s.Clone;
  S2['name'] := 'christa';
  Assert.AreEqual('root123', S2.Item['root'].AsString);
  Assert.AreEqual('christa', S2.Item['name'].AsString);
  S.Free;
  S2.Free;
end;

initialization

// TDUnitX.RegisterTestFixture(TTestVelocityScope);

end.
