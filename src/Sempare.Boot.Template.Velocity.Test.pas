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
unit Sempare.Boot.Template.Velocity.Test;

interface

uses
  DUnitX.TestFramework;

type

  // [TestFixture]
  TTestVelocity = class
  private
    procedure Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
  public
    [Test]
    procedure TestComment;
    [Test]
    procedure TestHtmlEncoding;
    [Test]
    procedure TestNonMutation;
    [Test]
    procedure TestNoSpace;
    [Test]
    procedure TestStartEndToken;
    [Test]
    procedure TestStructure;
    [Test]
    procedure TestTabToSpace;
    [Test]
    procedure TestTabToSpaceAndNoSpace;
    [Test]
    procedure TestUnderscoreIn;
    [Test]
    procedure TestSubTemplate;
    [Test]
    procedure TestDynamicLoader;
  end;

implementation

uses
  SysUtils,
  System.Rtti,
  System.Json,
  System.Generics.Collections,
  System.Classes,
  Sempare.Boot.Template.Velocity,
  Sempare.Boot.Template.Velocity.Rtti,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Evaluate,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.PrettyPrint,
  Sempare.Boot.Template.Velocity.Lexer;

{ TTestVelocity }

procedure TTestVelocity.Test<T>(const AValue: T; const AExpect: string; const ATemplate: string);
var
  c: IVelocityTemplate;
begin
  c := Velocity.parse(ATemplate);
  Assert.AreEqual(AExpect, Velocity.Eval(c, AValue));
end;

procedure TTestVelocity.TestComment;
var
  S: tstringstream;

begin
  S := tstringstream.create;
  try
    Velocity.Eval( //
      'before ' + //
      '<% (* this is '#13#10#13#10'a comment *) %>' + //
      'after ' //
      , S);
  finally
    S.Free;
  end;

end;

procedure TTestVelocity.TestHtmlEncoding;
type
  TRec = record
    content: string;
  end;
var
  ctx: IVelocityContext;
  data: TRec;
begin
  data.content := 'a < b';
  ctx := Velocity.Context;
  ctx.UseHtmlVariableEncoder;

  Assert.AreEqual('<html><body>a &lt; b</body></html>', Velocity.Eval(ctx, '<html><body><% content %></body></html>', data));
end;

procedure TTestVelocity.TestNonMutation;
type
  TRec = record
    Val: string;
  end;
var
  r: TRec;
  S: tstringstream;
begin
  r.Val := 'a value';
  S := tstringstream.create;
  try
    Velocity.Eval('<% val := ''test'' %>', r, S);
    Assert.AreEqual('a value', r.Val);
  finally
    S.Free;
  end;
end;

procedure TTestVelocity.TestNoSpace;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.Options := [eoStripRecurringSpaces];
  Assert.AreEqual(' hello world ', Velocity.Eval(ctx, '  hello   world    '));
end;

procedure TTestVelocity.TestStartEndToken;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Assert.AreEqual('hello', Velocity.Eval(ctx, '{{ if true }}hello{{else}}bye{{end}}'));
end;

procedure TTestVelocity.TestStructure;
begin

end;

type
  TTemplate = record
    header: record
      company: string;
    end;

    footer: record
      copyright: integer;
    end;

  end;

procedure TTestVelocity.TestSubTemplate;
var
  info: TTemplate;
begin
  info.header.company := 'sempare ltd';
  info.footer.copyright := 2019;
  Assert.AreEqual('sempare ltd  Copyright (c) 2019', Velocity.Eval('<% template ''prefix'' %><%company%><% end %>' + '<% template ''suffix'' %> Copyright (c) <%copyright%><% end %>' +
    '<%include(''prefix'', _.header)%> <%include(''suffix'', _.footer)%>', info));
end;

procedure TTestVelocity.TestTabToSpace;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.Options := [eoConvertTabsToSpaces];
  Assert.AreEqual(' hello world ', Velocity.Eval(ctx, #9'hello'#9'world'#9));
end;

procedure TTestVelocity.TestTabToSpaceAndNoSpace;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.Options := [eoStripRecurringSpaces, eoConvertTabsToSpaces];
  Assert.AreEqual(' hello world', Velocity.Eval(ctx, #9' hello '#9'  world'));
end;

procedure TTestVelocity.TestDynamicLoader;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context;
  ctx.TemplateResolver := function(const AContext: IVelocityContext; const ATemplate: string): IVelocityTemplate
    begin
      result := Velocity.parse(AContext, '_<% _ %>_');
    end;
  Assert.AreEqual('_abc__def__abc_', Velocity.Eval(ctx, '<% include(''abc'') %><% include(''def'') %><% include(''abc'') %>'));
end;

procedure TTestVelocity.TestUnderscoreIn;
var
  L: TList<string>;
begin
  L := TList<string>.create;
  L.AddRange(['1', '2', '3']);
  Assert.AreEqual('123', Velocity.Eval('<% for v in _ %><% v %><% end %>', L));
  L.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TTestVelocity);

end.
