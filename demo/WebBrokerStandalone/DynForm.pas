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
    FieldType: string;
    constructor create(const ACaption, AName: string; const AFieldType: string = 'TSubmitButton');
  end;

  TTemplateData = record
    Title: string;
    FormName: string;
    FormAction: string;
    Fields: TArray<TField>;
    Buttons: TArray<TButton>;
  end;

  TFormData = record
    firstname, lastname, email: string;
  end;

implementation

constructor TField.create(const ACaption, AName, AFieldType: string);
begin
  Caption := ACaption;
  Name := AName;
  FieldType := AFieldType;
end;

constructor TButton.create(const ACaption, AName: string; const AFieldType: string);
begin
  Caption := ACaption;
  Name := AName;
  FieldType := AFieldType;
end;

end.
