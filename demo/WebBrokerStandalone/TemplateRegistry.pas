unit TemplateRegistry;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.SyncObjs,
  Sempare.Template;

type
  TTemplateRegistry = class
  private
    class var FTemplateRegistry: TTemplateRegistry;
    class constructor Create;
    class destructor Destroy;
  private
    FLock: TCriticalSection;
    FTemplates: TDictionary<string, ITemplate>;
    FContext: ITemplateContext;
  public
    constructor Create;
    destructor Destroy; override;
    function GetTemplate(const ATemplateName: string): ITemplate;
    function ProcessTemplate<T>(const ATemplateName: string; const AData: T): string; overload;
    function ProcessTemplate(const ATemplateName: string): string; overload;
    class property Instance: TTemplateRegistry read FTemplateRegistry;
  end;

implementation

uses
  System.DateUtils;

{ TTemplateRegistry }

constructor TTemplateRegistry.Create;
begin
  FLock := TCriticalSection.Create;
  FTemplates := TDictionary<string, ITemplate>.Create;
  FContext := Template.Context([eoEmbedException]);

  FContext.Variable['Company'] := 'My Company  - Sempare Limited';
  FContext.Variable['CopyrightYear'] := YearOf(Now);

  FContext.TemplateResolver := function(const AContext: ITemplateContext; const AName: string): ITemplate
    begin
      exit(GetTemplate(AName));
    end;
end;

class constructor TTemplateRegistry.Create;
begin
  FTemplateRegistry := TTemplateRegistry.Create;
end;

destructor TTemplateRegistry.Destroy;
begin
  FLock.Free;
  FTemplates.Free;
  inherited;
end;

class destructor TTemplateRegistry.Destroy;
begin
  FTemplateRegistry.Free;
end;

function TTemplateRegistry.GetTemplate(const ATemplateName: string): ITemplate;
var
  LStream: TResourceStream;
begin
  FLock.Acquire;
  try
    if FTemplates.TryGetValue(ATemplateName, result) then
      exit;
  finally
    FLock.Release;
  end;
  LStream := TResourceStream.Create(HInstance, ATemplateName.tolower, RT_RCDATA);
  try
    result := Template.Parse(FContext, LStream);
  except
    on e: exception do
    begin
      result := Template.Parse(e.Message);
      exit;
    end;
  end;
  FLock.Acquire;
  try
    FTemplates.Add(ATemplateName, result);
  finally
    FLock.Release;
  end;
end;

function TTemplateRegistry.ProcessTemplate(const ATemplateName: string): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  exit(Template.Eval(FContext, LTemplate));
end;

function TTemplateRegistry.ProcessTemplate<T>(const ATemplateName: string; const AData: T): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  exit(Template.Eval(FContext, LTemplate, AData));
end;

end.
