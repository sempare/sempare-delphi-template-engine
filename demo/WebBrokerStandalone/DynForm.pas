unit DynForm;

interface

type
  TField = record
    Caption: string;
    Name: string;
    FieldType: string;
    constructor create(const ACaption, AName: string; const AFieldType: string = 'TEdit');
  end;

  TButton = record
    Caption: string;
    Name: string;
    constructor create(const ACaption, AName: string);
  end;

  TTemplateData = record
    Title : string;
    FormName: string;
    FormAction: string;
    Fields: TArray<TField>;
    Buttons: TArray<TButton>;
  end;

implementation

constructor TField.create(const ACaption, AName, AFieldType: string);
begin
  Caption := ACaption;
  name := AName;
  FieldType := AFieldType;
end;

constructor TButton.create(const ACaption, AName: string);
begin
  Caption := ACaption;
  name := AName;
end;

end.
