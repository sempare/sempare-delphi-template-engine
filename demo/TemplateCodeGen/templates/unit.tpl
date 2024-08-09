unit {{UnitName}};

interface

type
  {{ClassName}} = class
  private
    {{ for Field of Fields }}
    F{{Field.Name}}: {{Field.FieldType}};
    {{ end }}
  public
    constructor Create;
    {{ for Field of Fields }}
    property {{Field.Name}}: {{Field.FieldType}} read F{{Field.Name}} write F{{Field.Name}};
    {{ end }}
  end;

implementation

constructor {{ClassName}}.Create;
begin
  {{ for Field of Fields }}
  F{{Field.Name}} := Default({{Field.FieldType}});
  {{ end }}
end;

end.