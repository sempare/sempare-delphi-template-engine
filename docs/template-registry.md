# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2024 [Sempare Limited](http://www.sempare.ltd)

## The Template Registry

Most applications all have the same requirements when it comes to managing templates. The TTemplateRegistry utility class is a productivity booster and is the easiest way to maintain multiple templates in a project, spanning file and application resources.

The default behavour of parsing and evaluating templates is managed using the IContext, ITemplate interfaces and the Template utility class. Management of templates is then left to the developer. 

The TTemplateRegistry wraps the management of templates into a consistent way which can be integrated into any application / framework easily, providing the developer with maximum configurability.

### Example Usage

To illustrate the most basic usage using the Horse web framework.
```
program DemoServer;
{$R *.res}
uses
  Sempare.Template,
  Horse;

begin
  THorse.Get('/',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(Template.Resolve('index'));
    end);
  THorse.All('/*',
    procedure(Req: THorseRequest; Res: THorseResponse)
    begin
      Res.Send(Template.Resolve('error404'));
    end);
  THorse.Listen(8080);  
end;
```

Thats it. The only thing that is required otherwise is a templates directory containing index.tpl and error404.tpl.

e.g. index.tpl: 
```
<h1>Hello World</h1>
<p>The date is <% fmtdt('yyyy-mm-dd',DtNow()) %></p>
```


e.g. error404.tpl: 
```
<h1>Page not found</h1>
<p>Please try another url...</p>
```


e.g. DemoServer.rc: 
```
index_tpl RCDATA "templates\\index.tpl"
error404_tpl RCDATA "templates\\error404.tpl"
```

NOTE: Sempare.Template.RCGenerator can be used to automatically generate the DemoServer.rc so you don't have to.


The framework is also aware of the normal IDE structure where the application is installed in the directory [platform]/[configuration]. While debugging, it will first check for templates relative to the application, and otherwise search ..\\..\\templates.


### TSempareServerPages interface

TSempareServerPages is actually an alias for TTemplateRegistry. The following public methods and properties:
```
  TTemplateLoadStrategy = (tlsLoadResource, tlsLoadFile, tlsLoadCustom);
  TTemplateNameContextResolver = reference to function(const AName: string; const AContext: TTemplateValue): string;
  TTemplateNameResolver = reference to function(const AName: string): string;
  TTempateLogException = reference to procedure(const AException: Exception);
  TTempateLogMessage = reference to procedure(const AMessage: string; const args: array of const);

  TTemplateRegistry = class 

    // Resolve a template by name
    function GetTemplate(const ATemplateName: string): ITemplate; overload; inline;
    function GetTemplate(const ATemplateName: string; const AContext: TTemplateValue): ITemplate; overload;
    function GetTemplate<T>(const ATemplateName: string; const AContext: T): ITemplate; overload; inline;
    procedure RemoveTemplate(const ATemplateName: string);
    procedure ClearTemplates;

    procedure Eval<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream); overload;
    function Eval<T>(const ATemplateName: string; const AData: T): string; overload;

    procedure Eval(const ATemplateName: string; const AOutputStream: TStream); overload;
    function Eval(const ATemplateName: string): string; overload;

    // with context
    procedure Eval<T>(const ATemplateName: string; const AContext: TTemplateValue; const AData: T; const AOutputStream: TStream); overload;
    function Eval<T>(const ATemplateName: string; const AContext: TTemplateValue; const AData: T): string; overload;

    procedure EvalWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream); overload;
    function EvalWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T): string; overload;

    procedure EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream); overload;
    function EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string; overload;

    // Provides access to the singleton instance of the registry
    class property Instance: TTemplateRegistry;                   // read only

    // Provides access to the context used by parsing and evaluating templates
    property Context: ITemplateContext;                           // read only
    
    // Allow a custom name resolver to map onto application resources 
    property ResourceNameResolver: TTemplateNameResolver;         // read/write
    
    // Allow a custom name resolver to map onto file resources
    property FileNameResolver: TTemplateNameResolver;             // read/write
    
    // The folder where templates will be accessed when using tlsLoadFile
    
    property TemplateRootFolder: string;                          // read/write
    
    // The extenstion for templates. Defaults to .tpl
    property TemplateFileExt: string;                             // read/write
    
    // The load strategy order. Defaults to [tlsResource] in RELEASE, and [tlsFile, tlsResource] in DEBUG
    property LoadStrategy: TArray<TTemplateLoadStrategy>;         // read/write
    
    // The template refresh interval when AutomaticRefresh is true when tlsFile in LoadStrategy
    property RefreshIntervalS: integer;                           // read/write
    
    // When true, will refresh templates in the background. 
    property AutomaticRefresh: boolean;                           // read/write
    
    // As Context.TemplateResolver is used by the TTemplateRegistry, this provides for custom loading.
    property CustomTemplateLoader: TTemplateResolver;             // read/write
    
    // Allows for additional contextual information to be provided to the name resolvers
    property NameContextResolver: TTemplateNameContextResolver;   // read/write
    
    // Allow for custom logging when there are exceptions loading or parsing templates
    property ExceptionLogger: TTempateLogException;               // read/write
  end;

```

These can be accessed by using the normal Template helper class:

```
    Template = class
        // ....
       
        class function Resolver(): TTemplateRegistry; static; inline;
        
        class procedure Resolve<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream); overload; static; inline;
        class function Resolve<T>(const ATemplateName: string; const AData: T): string; overload; static; inline;

        class procedure Resolve(const ATemplateName: string; const AOutputStream: TStream); overload; static; inline;
        class function Resolve(const ATemplateName: string): string; overload; static; inline;

        // with context

        class procedure ResolveWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream); overload; static; inline;
        class function ResolveWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T): string; overload; static; inline;

        class procedure ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream); overload; static; inline;
        class function ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string; overload; static; inline;

       
        // ....
    end;
```

## Resource File Creation Tool

The Sempare.Template.RCGenerator.exe is available that can generate your .rc file for you quickly.

```
PS > .\Sempare.Template.RCGenerator.exe
Sempare.Template.RCGenerator.exe <rcfilename> <templatepath> [<ext>+]

<rcfilename> is the path to the filename that will be generated
<templatepath> is the path to the directory listing all the templates
<ext>+ is one one or more extensions to be included. By default: .ico, .png, .jpg, .jpeg, .webp, .tpl, .bmp, .gif, .wbmp
```