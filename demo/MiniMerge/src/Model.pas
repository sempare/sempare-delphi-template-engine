unit Model;

interface

uses
  Sempare.Template;

type
  TEmailTemplate = record
    Subject: ITemplate;
    Body: ITemplate;
    procedure Load(const ATemplatePath: string; const ATemplate: string);
  end;

  TNameEmail = record
    Name: string;
    Email: string;
  end;

  TConfig = record
    UnsubscribeURL: string;
  end;

  TTemplateData = record

    Config: TConfig;
    User: TNameEmail;

  end;

implementation

uses
  System.IOUtils;

{ TEmailTemplate }

procedure TEmailTemplate.Load(const ATemplatePath, ATemplate: string);
var
  LSubject: string;
  LBody: string;
begin
  LSubject := TFile.ReadAllText(TPath.Combine(ATemplatePath, ATemplate + '.subject.tpl'));
  LBody := TFile.ReadAllText(TPath.Combine(ATemplatePath, ATemplate + '.body.tpl'));

  Subject := Template.Parse(LSubject);
  Body := Template.Parse(LBody);

end;

end.
