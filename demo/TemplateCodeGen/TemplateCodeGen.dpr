program TemplateCodeGen;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Main in 'src\Main.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
