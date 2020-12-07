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
unit Sempare.Template.TestInclude;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TTestTemplateInclude = class
  public
    [Test]
    procedure TestInclude;

    [Test]
    procedure TestIncludeMissingData;

    [Test]
    procedure TestIncludeData;

  end;

implementation

uses
  System.Generics.Collections,
  Sempare.Template.Context,
  Sempare.Template;

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

procedure TTestTemplateInclude.TestIncludeData;
var
  ctx: ITemplateContext;
  x: record value: string;
end;
begin
  ctx := Template.Context;
  ctx.Template['test'] := Template.parse('test <% _ %>');
  ctx.Template['test2'] := Template.parse('test <% value %>');

  // illustrate simple template
  Assert.AreEqual('test 123', Template.Eval(ctx, 'test <%_%>', '123'));

  // test using include - stackframe is preserved
  Assert.AreEqual('test 123', Template.Eval(ctx, '<%include (''test'') %>', '123'));

  // test using include - variable referenced is not provided
  // important to note: include() uses with() to create a scope, so a record or class must be provided
  x.value := '123';
  Assert.AreEqual('test 123', Template.Eval(ctx, '<%include (''test2'', _) %>', x));

end;

procedure TTestTemplateInclude.TestIncludeMissingData;

begin
  Assert.WillRaise(
    procedure
    var
      ctx: ITemplateContext;
      x: record value: string;
    end;
    begin
      ctx := Template.Context;
      ctx.Template['test'] := Template.parse('test <% value %>');
      x.value := '123';
      Assert.AreEqual('test 123', Template.Eval(ctx, '<%include (''test'', missingvar) %>', x));
    end);
end;

procedure TTestTemplateInclude.TestInclude;
var
  c: ITemplate;
  x: THeaderContentFooter;
  ctx: ITemplateContext;

begin
  ctx := Template.Context;

  ctx.Template['header'] := Template.parse('header <% header.name %> <% footer.year %>');
  ctx.Template['footer'] := Template.parse('footer Copyright <% footer.year %>');

  c := Template.parse('<% suffix := ''er''%><% include (''head'' + suffix) %>' + '<%for v in content %>' + '<% v %>' + '<% end %>' + '<% include (''foot'' + suffix) %>');

  x.content := TList<string>.create;
  x.content.Add('conrad');
  x.content.Add('christa');
  x.header.Name := 'Sempare Ltd';
  x.footer.Year := 2020;
  try
    Assert.AreEqual('header Sempare Ltd 2020conradchristafooter Copyright 2020', Template.Eval(ctx, c, x));
  finally
    x.content.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTemplateInclude);

end.
