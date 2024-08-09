program MailMerge;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Main in 'src\Main.pas',
  Model in 'src\Model.pas',
  EmailAction in 'src\EmailAction.pas',
  ConsoleLogAction in 'src\ConsoleLogAction.pas';

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
