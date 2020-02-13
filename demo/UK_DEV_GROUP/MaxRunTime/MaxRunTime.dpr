program MaxRunTime;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity;

var
  ctx : IVelocityContext;

begin
  try
  ctx := Velocity.Context();
  ctx.MaxRunTimeMs := 5;
  Velocity.Eval(ctx, '<% i := 0 %> <% while i < 10000 %> <% i %> <% i := i + 1 %> <% end %>');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  writeln('done');
  readln;
end.
