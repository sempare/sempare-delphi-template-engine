unit ConsoleLogAction;

interface

uses
  System.Classes,
  System.SysUtils,
  Sempare.Template,
  Model;

procedure ConsoleLog(const AList: TArray<TArray<string>>; const ATemplate: TEmailTemplate; const AConfig: TConfig);

implementation

procedure Log(var AUser: TNameEmail; const ATemplate: TEmailTemplate; const AConfig: TConfig);
var
  LData: TTemplateData;
begin
  LData.Config := AConfig;
  LData.User := AUser;

  writeln(format('To: %s <%s>', [AUser.name, AUser.Email]));
  writeln(format('Subject: %s', [Template.Eval(ATemplate.Subject, LData)]));
  writeln(format('Body: %s', [Template.Eval(ATemplate.Body, LData)]));
  writeln('---');
end;

procedure ConsoleLog(const AList: TArray<TArray<string>>; const ATemplate: TEmailTemplate; const AConfig: TConfig);
var
  i: integer;
  LUser: TNameEmail;
begin
  for i := 0 to high(AList) do
  begin
    LUser.name := AList[i][0];
    LUser.Email := AList[i][1];
    Log(LUser, ATemplate, AConfig);
  end;
end;

end.
