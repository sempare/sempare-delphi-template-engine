unit Main;

interface

procedure Run;

implementation

uses
  Model,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  ConsoleLogAction,
  EmailAction;

function LoadList(const APath: string): TArray<TArray<string>>;
var
  LLine: string;
  LLines: TArray<string>;
  LParts: TArray<string>;
  i: integer;
begin
  LLines := TFile.ReadAllLines(APath);
  i := 0;
  for LLine in LLines do
  begin
    if LLine.IsEmpty then
      continue;
    LParts := LLine.split([',']);
    if LParts[0].IsEmpty or not LParts[1].contains('@') then
      continue;
    insert(LParts, result, length(result));
    inc(i);
  end;
  setlength(result, i);
end;

procedure Run;
var
  LTemplatePath: string;
  LTemplate: string;
  LListPath: string;
  LDryRun: boolean;
  LConfig: TConfig;
  LEmailTemplate: TEmailTemplate;
  LList: TArray<TArray<string>>;
  LAction: procedure(const AList: TArray<TArray<string>>; const ATemplate: TEmailTemplate; const AConfig: TConfig);

begin
  if paramcount = 0 then
  begin
    writeln(format('Usage: %s <templatepath> <template> <list> [<action>]', [TPath.GetFileName(paramstr(0))]));
    exit;
  end;
  LTemplatePath := paramstr(1);
  LTemplate := paramstr(2);
  LListPath := paramstr(3);

  LDryRun := not((paramcount > 3) and (paramstr(4) = 'email')); // email or dryrun
  if LDryRun then
    LAction := ConsoleLog
  else
    LAction := EmailLog;

  LEmailTemplate.Load(LTemplatePath, LTemplate);

  LList := LoadList(LListPath);

  LConfig.UnsubscribeURL := 'https://www.example.com/unsubscribe';

  LAction(LList, LEmailTemplate, LConfig);

end;

end.
