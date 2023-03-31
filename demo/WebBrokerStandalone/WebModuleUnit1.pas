unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1FormInputAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  DynForm,
  TemplateRegistry;

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := TTemplateRegistry.Instance.ProcessTemplate('index');
  Handled := true;
end;

procedure TWebModule1.WebModule1FormInputAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LTemplateData: TTemplateData;
begin
  LTemplateData.Title := 'User Details';
  LTemplateData.FormName := 'userinfo';
  LTemplateData.FormAction := '/userinfo';
  LTemplateData.Fields := [TField.create('FirstName', 'firstname'), TField.create('LastName', 'lastname'), TField.create('Email', 'email', 'TEmail')];
  LTemplateData.Buttons := [TButton.create('Submit', 'submit')];
  Response.Content := TTemplateRegistry.Instance.ProcessTemplate('dynform', LTemplateData);
  Handled := true;
end;

end.
