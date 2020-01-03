unit Sempare.Boot.Template.Velocity.Context.Test;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TContextTest = class
  public

    [Test]
    procedure TestVariables();
  end;

implementation

uses
  Sempare.Boot.Template.Velocity;

{ TContextTest }

procedure TContextTest.TestVariables;
var
  ctx: IVelocityContext;
begin
  ctx := Velocity.Context();
  ctx.Variable['company'] := 'Sempare Limited';

  Assert.AreEqual('Sempare Limited 2019', Velocity.Eval(ctx, '<% company %> <% _ %>', 2019));
end;

end.
