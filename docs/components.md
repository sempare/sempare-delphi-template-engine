# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Components

The Sempare Template Engine uses interfaces extensively. This makes memory management much easier as interfaced objects are reference counted and alls for code to be more readable as minimal try/catch/finally block are required.

To ease the use of Sempare Template Engine you need to just include a reference to the _Sempare.Template._ unit:
```
uses
    Sempare.Template;
```

Key components of interest that are exposed to you are:
1. [Template](#Template)
2. [ITemplateContext](#ITemplateContext)
3. [ITemplate](#ITemplate)

Initially, some true Delphi Components (based off TComponent) were also provided. See [Design Considerations](./design-considerations.md) for more information.


### Template

The Template class exposes static class methods acting as an entry point to the Template engine with output being to a TStream or string.

Most common use cases would be to use Template.Eval(), Template.Parse() and Template.Context() methods. More information will be provided later.

```
type
    TInfo = record 
                name : string;
            end;
begin
   var template := Template.parse('hello <% name %>');
   var info : TInfo;
   info.name := 'sue';

   // Eval returning a string
   writeln(Template.Eval(template, info));

   // Eval writing to a stream
   var s:= TStringStream.Create();
   try
   	    Template.Eval(template, info, s);
        writeln(s.DataString);
   finally
	s.Free;
   end;
end;
```
The Template class provides many other useful methods:
```
   Template = class
  public
    class function Context(AOptions: TTemplateEvaluationOptions = []): ITemplateContext; inline; static;
    class function Parser(const AContext: ITemplateContext): ITemplateParser; overload; inline; static;
    class function Parser(): ITemplateParser; overload; inline; static;
    class function PrettyPrint(const ATemplate: ITemplate): string; inline; static;

    // EVAL output to stream

    class procedure Eval(const ATemplate: string; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval(const ATemplate: ITemplate; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval(const AContext: ITemplateContext; const ATemplate: string; const AStream: TStream); overload; static;
    class procedure Eval(const AContext: ITemplateContext; const ATemplate: ITemplate; const AStream: TStream); overload; static;

    // EVAL returning string

    class function Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval(const ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;

    class function Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T): string; overload; static;
    class function Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T): string; overload; static;
    class function Eval(const AContext: ITemplateContext; const ATemplate: string): string; overload; static;
    class function Eval(const AContext: ITemplateContext; const ATemplate: ITemplate): string; overload; static;

    // PARSING

    // string operations
    class function Parse(const AString: string): ITemplate; overload; static;
    class function Parse(const AContext: ITemplateContext; const AString: string): ITemplate; overload; static;

    // stream operations
    class function Parse(const AStream: TStream): ITemplate; overload; static;
    class function Parse(const AContext: ITemplateContext; const AStream: TStream): ITemplate; overload; static;

    // file operations
    class function ParseFile(const AFile: string): ITemplate; overload; static;
    class function ParseFile(const AContext: ITemplateContext; const AFile: string): ITemplate; overload; static;

  end;

```
### ITemplateContext

The Template context object is a container for configuration used when parsing or evaluating templats.
```
    var ctx := Template.Context();
    ctx.MaxRuntimeMs := 5;
    
    // encoder can be an HTML encoder (discussed later)
    ctx.VariableEncoder := encoder;   
    
    // Variables can be set on the context so they can be applied to multiple templates easily
    ctx.Variable['company'] := 'Sempare Limited';
    
    // header can be a template referenced by other templates
    ctx.Template['header'] := header; 
    
    ctx.Options := [eoStripRecurringSpaces, eoConvertTabsToSpaces, eoNoDefaultFunctions, eoDebug, eoNoPosition];

    ctx.StartToken := '<%';
    ctx.EndToken := '%>';
	
	ctx.Options := ctx.Options + [eoEmbedException];
	ctx.DebugErrorFormat := '%s';
	
```

Defining variables in a context allows them to be used in made available to multiple templates easily.

### ITemplate

The output of the Template parser is an object implementing the ITemplate interface.
```
    var ctx := Template.Context();
    var tpl := Template.Parse(ctx, 'this is a template'); 
    writeln(Template.Eval(ctx, tpl));
```

