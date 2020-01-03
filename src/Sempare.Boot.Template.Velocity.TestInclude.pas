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
unit Sempare.Boot.Template.Velocity.TestInclude;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestVelocityInclude = class
  public
    [Test]
    procedure TestInclude;

  end;

implementation

uses
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity;

type
  THeaderContentFooter = record
    header: record
      Name: string;
    end;

    content: TList<string>;

    footer: record
      Year: integer;
    end;
  end;

procedure TTestVelocityInclude.TestInclude;
var
  c: IVelocityTemplate;
  x: THeaderContentFooter;
  ctx: IVelocityContext;

begin
  ctx := Velocity.Context;

  ctx.AddTemplate('header', Velocity.parse('header <% header.name %> <% footer.year %>'));
  ctx.AddTemplate('footer', Velocity.parse('footer Copyright <% footer.year %>'));

  c := Velocity.parse('<% suffix := ''er''%><% include (''head'' + suffix) %>' + '<%for v in content %>' + '<% v %>' + '<% end %>' + '<% include (''foot'' + suffix) %>');

  x.content := TList<string>.create;
  x.content.Add('conrad');
  x.content.Add('christa');
  x.header.Name := 'Sempare Ltd';
  x.footer.Year := 2019;
  try
    Assert.AreEqual('header Sempare Ltd 2019conradchristafooter Copyright 2019', Velocity.Eval(ctx, c, x));
  finally
    x.content.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocityInclude);

end.
