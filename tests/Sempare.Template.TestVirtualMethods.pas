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
 * Copyright (c) 2019-2024 Sempare Limited                                                          *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the Apache Version 2.0 or the Sempare Commercial License                          *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.apache.org/licenses/LICENSE-2.0                                                      *
 * https://github.com/sempare/sempare-delphi-template-engine/blob/master/docs/commercial.license.md *
 *                                                                                                  *
 * Unless required by applicable law or agreed to in writing, software                              *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,                               *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                         *
 * See the License for the specific language governing permissions and                              *
 * limitations under the License.                                                                   *
 *                                                                                                  *
 *************************************************************************************************%*)
unit Sempare.Template.TestVirtualMethods;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVirtualMethods = class

  public
    // Author: Sean Bogie (https://github.com/sbogie-gdi)
    // https://github.com/sempare/sempare-delphi-template-engine/issues/145
    [Test]
    procedure TestVirtualCall;

  end;

implementation

uses
  Sempare.Template;

type
  TBase = class
  public
    function Name: string; virtual; abstract;
  end;

  TFoo = class(TBase)
  public
    function Name: string; override;
  end;

  TBar = class(TBase)
  public
    function Name: string; override;
  end;

function TFoo.Name: string;
begin
  Result := 'Foo';
end;

function TBar.Name: string;
begin
  Result := 'Bar';
end;

procedure TTestVirtualMethods.TestVirtualCall;
var
  l: array [0 .. 1] of TBase;
begin
  l[0] := nil;
  l[1] := nil;
  try
    l[0] := TFoo.Create;
    l[1] := TBar.Create;
    Assert.AreEqual('FooBar', Template.Eval('<% print(_[0].Name() + _[1].Name()) %>', l));
    Assert.AreEqual('FooBar', Template.Eval('<% for v of _ %><% v.Name() %><% end %>', l));
    Assert.AreEqual('FooBar', Template.Eval('<% for i := 0 to 1; print( _[i].Name()); end %>', l));
  finally
    l[0].Free;
    l[1].Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestVirtualMethods);

end.
