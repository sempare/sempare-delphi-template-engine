program MaxRunTime;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Template;

var
  ctx : ITemplateContext;

begin
  try
  ctx := Template.Context();
  ctx.MaxRunTimeMs := 5;
  Template.Eval(ctx, '<% i := 0 %> <% while i < 10000 %> <% i %> <% i := i + 1 %> <% end %>');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  writeln('done');
  readln;
end.
