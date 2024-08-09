unit Main;

interface

procedure Run;

implementation

uses
  System.IOUtils,
  Sempare.Template;

type
  TClassField = record
    Name: string;
    FieldType: string;
    constructor Create(const AName, AFieldType: string);
  end;

  TUnit = record
    UnitName: string;
    ClassName: string;
    Fields: TArray<TClassField>;
  end;

procedure Run;
var
  LContext: ITemplateContext;
  LTemplatePath: string;
  LTemplate: ITemplate;
  LUnit: TUnit;
begin
  LContext := Template.Context();
  LContext.StartToken := '{{'; // <%
  LContext.EndToken := '}}';    // %>

  LTemplatePath := TPath.Combine(['..', '..', 'templates', 'unit.tpl']);

  LUnit.UnitName := 'MyUnit';
  LUnit.ClassName := 'TMyClass';
  LUnit.Fields := [ //
    TClassField.Create('Name', 'string'), //
    TClassField.Create('Age', 'integer'), //
    TClassField.Create('BirthDate', 'TDateTime')];

  writeln('--- start: ' + LUnit.UnitName + '.pas -----');
  writeln(Template.Eval(LContext, TFile.ReadAllText(LTemplatePath), LUnit));
  writeln('--- end: ' + LUnit.UnitName + '.pas -----');

  readln;
end;

{ TClassField }

constructor TClassField.Create(const AName, AFieldType: string);
begin
  name := AName;
  FieldType := AFieldType;
end;

end.
