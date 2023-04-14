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
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  System.Rtti,
  IdHTTPWebBrokerBridge,
  IdCustomHTTPServer,
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

  // This is a workaround to get AcceptLanguage as the TIdHTTPAppRequest does not expose headerss
function GetAcceptLanguage(const Req: TWebRequest): string;
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LField: TRttiField;
  LRequestInfo: TIdHTTPRequestInfo;
begin
  LRttiType := LContext.GetType(TIdHTTPAppRequest);
  LField := LRttiType.GetField('FRequestInfo');
  LRequestInfo := LField.GetValue(Req).AsType<TIdHTTPRequestInfo>;
  exit(LRequestInfo.AcceptLanguage);
end;

procedure TWebModule1.WebModule1IndexHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LDemos: TArray<TDemo>;
begin
  setlength(LDemos, 2);
  LDemos[0] := TDemo.Create('Web Broker', 'https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Creating_WebBroker_Applications', 'https://github.com/sempare/sempare-delphi-template-engine/tree/main/demo/WebBrokerStandalone', true);
  LDemos[1] := TDemo.Create('Horse', 'https://github.com/HashLoad/horse', 'https://github.com/sempare/sempare-delphi-template-engine-horse-demo');
  Response.Content := Template.ResolveWithContext('index', Request, LDemos);
  Handled := true;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  Template.Resolver.ContextNameResolver := function(const AName: string; const AContext: TTemplateValue): string
    var
      LLang: string;
      LReq: TWebRequest;
    begin
      LReq := AContext.AsType<TWebRequest>;
      LLang := GetAcceptLanguage(LReq).Substring(0, 2);
      if LLang.IsEmpty then
        exit(AName)
      else
        exit(AName + '_' + LLang);
    end;
end;

procedure TWebModule1.WebModule1ErrorHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := Template.ResolveWithContext('error404', Request);
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
  setlength(LTemplateData.Fields, 3);
  LTemplateData.Fields[0] := TField.Create('FirstName', 'firstname');
  LTemplateData.Fields[1] := TField.Create('LastName', 'lastname');
  LTemplateData.Fields[2] := TField.Create('Email', 'email', 'TEmail');
  setlength(LTemplateData.Buttons, 1);
  LTemplateData.Buttons[0] := TButton.Create('Submit', 'submit');
  Response.Content := Template.ResolveWithContext('dynform', Request, LTemplateData);
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
    Response.Content := Template.ResolveWithContext('submitted', Request, LFormData);
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
