unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1IndexHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1FormInputAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1FormInputHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1ErrorHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
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
  Sempare.Template;

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

type
  TDemo = record
    Name: string;
    FrameworkUrl: string;
    Url: string;
    Current: Boolean;
    constructor Create(const AName: String; const AFrameworkUrl, AUrl: string; const ACurrent: Boolean = false);
  end;

procedure TWebModule1.WebModule1IndexHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LDemos: TArray<TDemo>;
begin
  LDemos := [ //
    TDemo.Create('Web Broker', 'https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Creating_WebBroker_Applications', 'https://github.com/sempare/sempare-delphi-template-engine/tree/main/demo/WebBrokerStandalone', true), //
    TDemo.Create('Horse', 'https://github.com/HashLoad/horse', 'https://github.com/sempare/sempare-delphi-template-engine-horse-demo') //
    ];
  Response.Content := TTemplateRegistry.Instance.Eval('index', LDemos);
  Handled := true;
end;

procedure TWebModule1.WebModule1ErrorHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := TTemplateRegistry.Instance.Eval('error404');
  Response.StatusCode := 404;
  Handled := true;
end;

procedure TWebModule1.WebModule1FormInputAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LTemplateData: TTemplateData;
begin
  LTemplateData.Title := 'User Details';
  LTemplateData.FormName := 'userinfo';
  LTemplateData.FormAction := Request.PathInfo;
  LTemplateData.Fields := [ //
    TField.Create('FirstName', 'firstname'), //
    TField.Create('LastName', 'lastname'), //
    TField.Create('Email', 'email', 'TEmail') //
    ];
  LTemplateData.Buttons := [TButton.Create('Submit', 'submit')];
  Response.Content := TTemplateRegistry.Instance.Eval('dynform', LTemplateData);
  Handled := true;
end;

procedure TWebModule1.WebModule1FormInputHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LFormData: TFormData;
  PostData: string;
  Params: TStrings;
begin
  PostData := Request.Content;
  Params := TStringList.Create;
  try
    ExtractStrings(['&'], [], PChar(PostData), Params);
    LFormData.firstname := Params.Values['firstname'];
    LFormData.lastname := Params.Values['lastname'];
    LFormData.email := Params.Values['email'];
    Response.Content := TTemplateRegistry.Instance.Eval('submitted', LFormData);
  finally
    Params.Free;
  end;
  Handled := true;
end;

{ TDemo }

constructor TDemo.Create(const AName, AFrameworkUrl, AUrl: string; const ACurrent: Boolean);
begin
  name := AName;
  FrameworkUrl := AFrameworkUrl;
  Url := AUrl;
  Current := ACurrent;
end;

end.
