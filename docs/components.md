# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)


## Components

Sempare Boot Velocity uses interfaces extensively. This makes memory management much easier as interfaced objects are reference counted and alls for code to be more readable as minimal try/catch/finally block are required.

To ease the use of Sempare Boot Velocity, you need to just include a reference to the _Sempare.Boot.Template.Velocity_ unit:
```
uses
    Sempare.Boot.Template.Velocity;
```

Key components of interest that are exposed to you are:
1. [Velocity](#Velocity)
2. [IVelocityContext](#IVelocityContext)
3. [IVelocityTemplate](#IVelocityTemplate)

### Velocity

The Velocity class exposes static class methods acting as an entry point to the Velocity engine with output being to a TStream or string.

Most common use cases would be to use Velocity.Eval(), Velocity.Parse() and Velocity.Context() methods. More information will be provided later.

```
type
    TInfo = record 
                name : string;
            end;
begin
   var template := Velocity.parse('hello <% name %>');
   var info : TInfo;
   info.name := 'sue';

   // Eval returning a string
   writeln(Velocity.Eval(template, info));

   // Eval writing to a stream
   var s:= TStringStream.Create();
   try
   	    Velocity.Eval(template, info, s);
        writeln(s.DataString);
   finally
	s.Free;
   end;
end;
```
The Velocity class provides many other useful methods:
```
   Velocity = class
  public
    class function Context(AOptions: TVelocityEvaluationOptions = []): IVelocityContext; inline; static;
    class function Parser(const AContext: IVelocityContext): IVelocityParser; overload; inline; static;
    class function Parser(): IVelocityParser; overload; inline; static;
    class function PrettyPrint(const ATemplate: IVelocityTemplate): string; inline; static;

    // EVAL output to stream

    class procedure Eval(const ATemplate: string; const AStream: TStream; const AOptions: TVelocityEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TVelocityEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: IVelocityTemplate; const AValue: T; const AStream: TStream; const AOptions: TVelocityEvaluationOptions = []); overload; static;
    class procedure Eval(const ATemplate: IVelocityTemplate; const AStream: TStream; const AOptions: TVelocityEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const AContext: IVelocityContext; const ATemplate: IVelocityTemplate; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval<T>(const AContext: IVelocityContext; const ATemplate: string; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval(const AContext: IVelocityContext; const ATemplate: string; const AStream: TStream); overload; static;
    class procedure Eval(const AContext: IVelocityContext; const ATemplate: IVelocityTemplate; const AStream: TStream); overload; static;

    // EVAL returning string

    class function Eval(const ATemplate: string; const AOptions: TVelocityEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TVelocityEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: IVelocityTemplate; const AValue: T; const AOptions: TVelocityEvaluationOptions = []): string; overload; static;
    class function Eval(const ATemplate: IVelocityTemplate; const AOptions: TVelocityEvaluationOptions = []): string; overload; static;

    class function Eval<T>(const AContext: IVelocityContext; const ATemplate: IVelocityTemplate; const AValue: T): string; overload; static;
    class function Eval<T>(const AContext: IVelocityContext; const ATemplate: string; const AValue: T): string; overload; static;
    class function Eval(const AContext: IVelocityContext; const ATemplate: string): string; overload; static;
    class function Eval(const AContext: IVelocityContext; const ATemplate: IVelocityTemplate): string; overload; static;

    // PARSING

    // string operations
    class function Parse(const AString: string): IVelocityTemplate; overload; static;
    class function Parse(const AContext: IVelocityContext; const AString: string): IVelocityTemplate; overload; static;

    // stream operations
    class function Parse(const AStream: TStream): IVelocityTemplate; overload; static;
    class function Parse(const AContext: IVelocityContext; const AStream: TStream): IVelocityTemplate; overload; static;

    // file operations
    class function ParseFile(const AFile: string): IVelocityTemplate; overload; static;
    class function ParseFile(const AContext: IVelocityContext; const AFile: string): IVelocityTemplate; overload; static;

  end;

```
### IVelocityContext

The Velocity context object is a container for configuration used when parsing or evaluating templats.
```
    var ctx := Velocity.Context();
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
```

Defining variables in a context allows them to be used in made available to multiple templates easily.

### IVelocityTemplate

The output of the Velocity parser is an object implementing the IVelocityTemplate interface.
```
    var ctx := Velocity.Context();
    var tpl := Velocity.Parse(ctx, 'this is a template'); 
    writeln(Velocity.Eval(ctx, tpl));
```
##### Thread Safety
The Sempare Boot Velocity template engine should be totally thread safe. There is no shared state besides potential references a shared Velocity context.

Components that should be safe to share are instances of _IVelocityContext_ and _IVelocityTemplate_. 

##### Example use case

In a threaded environement like a web server, the following would take place:
- the context and templates are initialised at startup.
- templates are mapped to various routes used by various route controllers
- controller methods would interact with a database service and pass data to the template engine.
