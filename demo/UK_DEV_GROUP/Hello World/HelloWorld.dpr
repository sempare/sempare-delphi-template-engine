program HelloWorld;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Sempare.Boot.Template.Velocity;

procedure Demo1;

begin
  writeln(Velocity.Eval('hello <% _ %>', 'uk dev group'));
end;

procedure Demo2;
var
  data: record Name: string;
end;

begin
  data.Name := 'uk dev group';
  writeln(Velocity.Eval('hello <% uppercase(_.name) %>.', data));
end;

begin
  ReportMemoryLeaksOnShutdown := true;
  Demo1;
  Demo2;
  readln;

end.
