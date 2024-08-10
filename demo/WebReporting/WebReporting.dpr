program WebReporting;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Horse,
  Sempare.Template,
  System.SysUtils;

type
  TPerson = record
    FirstName: string;
    LastName: string;
    Score: integer;
    constructor Create(const AFN, ALN: string; const AScore: integer);
  end;

  TTemplateData = record
    HighScores: TArray<TPerson>;
  end;

  TWebReportingFunctions = class
  public
    class function JoinNames(const APerson: TPerson): string; static;

  end;

  { TPerson }

constructor TPerson.Create(const AFN, ALN: string; const AScore: integer);
begin
  FirstName := AFN;
  LastName := ALN;
  Score := AScore;
end;

{ TWebReportingFunctions }

class function TWebReportingFunctions.JoinNames(const APerson: TPerson): string;
begin
  exit(format('%s %s', [APerson.FirstName, APerson.LastName]));
end;

begin
  try
    Template.Resolver.Context.Functions.AddFunctions(TWebReportingFunctions);

    THorse.Get('/',
      procedure(Req: THorseRequest; Res: THorseResponse)
      var
        LData: TTemplateData;
      begin
        LData.HighScores := [ //
          TPerson.Create('joe', 'blogs', 10000), //
          TPerson.Create('pete', 'pan', 954), //
          TPerson.Create('adam', 'smith', 44) //

          ];
        Res.Send(Template.ResolveWithContext('index', Req, LData));
      end);

    THorse.Listen(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
