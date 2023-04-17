# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Configuration

- [Overview](#Overview)
- [Text encoding](#Text_encoding)
- [HTML Variable Encoding](#HTML_Variable_Encoding)
- [Custom Variable Encoding](#Custom_Variable_Encoding)
- [Setting a maximum runtime](#Setting_a_maximum_runtime)
- [Customise the start and end script tokens](#Customise_the_start_and_end_script_tokens)
- [Custom Variables](#Custom_Variables)
- [Reusing Templates](#Reusing_Templates)
- [Dynamic Template Resolution](#Dynamic_Template_Resolution)
- [Embed exceptions in output](#Embed_exceptions_in_output)
- [Options](#Options)
- [Decimal Separators](#Decimal_Separators)
- [Value Separators](#Value_Separators)
- [Customise Whitespace](#Custom_Whitespace)
- [Customise Newline](#Custom_Newline)

<a name="Overview"><h3>Overview</h3></a>

Configuration is done through the context. If you want to rely on the defaults, many of the eval methods don't require a context to be explictly provided and they will create create a default context for use.
```
    var ctx := Template.Context();
```

<a name="Text_encoding"><h3>Text encoding</h3></a>

The default default encoding is ASCII. You can change this to UTF8 as follows:
```
var ctx := Template.Context;
ctx.Encoding := TEncoding.UTF8;
```

<a name="HTML_Variable_Encoding"><h3>HTML Variable Encoding</h3></a>
Call UseHtmlVariableEncoder on a context:
```
ctx.UseHtmlVariableEncoder;
```
For example:
```
type
  TRec = record
    content: string;
  end;
begin
  var data: TRec;
  data.content := 'a < b';
  var ctx := Template.Context;
  ctx.UseHtmlVariableEncoder;

  Assert.IsEqual('<html><body>a &lt; b</body></html>', Template.Eval(ctx, '<html><body><% content %></body></html>', data));
end;
```

<a name="Custom_Variable_Encoding"><h3>Custom Variable Encoding</h3></a>

Above was an example of HTML encoding. You can create custom encoding mechanism by setting VariableEncoder on a context.

```
type
  TTemplateEncodeFunction = reference to function(const AArg : string): string;
```

<a name="Setting_a_maximum_runtime"><h3>Setting a maximum runtime</h3></a>

```
  var ctx := Template.Context();
  ctx.MaxRunTimeMs = 5;
  // ...
```

<a name="Customise_the_start_and_end_script_tokens"><h3>Customise the start and end script tokens</h3></a>

You may want to change from using '<%' and '%>' to something else by updating the _StartToken_ and _EndToken_ on the context.
```
begin
  var ctx := Template.Context;
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Assert.IsEqual('hello', Template.Eval(ctx, '{{ if true }}hello{{else}}bye{{end}}'));
end;
```

<a name="Custom_Variables"><h3>Custom Variables</h3></a>

If there are variables that should be set by default when using a template, they can
be propulated in the context as illustrated:
```
ctx.Variable['company'] := 'Sempare Limited'; 
```

<a name="Reusing_Templates"><h3>Reusing Templates</h3></a>

Using the _include()_ statement, you can reference precompiled templates that are registered on the context:

```
ctx.RegisterTemplate('header', Template.Parse('<% title %>')) 
ctx.RegisterTemplate('footer', Template.Parse('Copyright (c) <% year %> <% company %>')) 
```

<a name="Dynamic_Template_Resolution"><h3>Dynamic Template Resolution</h3></a>

Templates don't need to be located in a single template. They can also be resolved dynamically using the TemplateResolver or TemplateResolverWithContext method on the context.
Templates could be loaded from file, resources or urls are per your requirements.

```
ctx.TemplateResolver = function(const AContext : ITemplate; const AName : string) : ITemplate
begin
   result := Template.parse('some template loaded from file...');
end;

// or

ctx.TemplateResolverWithContext = function(const AContext : ITemplate; const AName : string; const AResolveContext: TTemplateValue) : ITemplate
begin
   // ...
end;

```

Only one of the resolvers needs to be set as TemplateResolver is wrapped into TemplateResolverWithContext passing an empty string as a context.

Using a resolve context can be useful in scenarios such as web, where a language specific template can be resolved based on request headers (the http request object can be the resolve context).

<a name="Embed_exceptions_in_output"><h3>Embed exceptions in output</h3></a>

Exceptions are normally raised. However, they can be logged in the output with the following configuration.
```
var ctx := Template.Context([eoEmbedException]);
ctx.DebugErrorFormat := '<b>Error:</b> <i>%s</i>';
```

<a name="Options"><h3>Options</h3></a>

The template engine allows for the following options:
- eoEmbedException
  - use this option to get exceptions logged into the output. DebugErrorFormat defines the output format.
- eoStripRecurringSpaces
  - The lexer will ommit only one space when multiple spaces are detected in a sequence. 
- eoConvertTabsToSpaces
  - tabs are converted to spaces (before the strip recurring spaces feature kicks in)
- eoNoDefaultFunctions
  - Disables default functions from being added to the context.
- eoNoPosition
  - disposes of positional information that should minimise memory footprint
- eoEvalEarly
  - evaluate statements/expressions at parse time where possible 
- eoEvalVarsEarly
  - evaluate statements/expressions at parse time that reference variables from the context where possible 
- eoRaiseErrorWhenVariableNotFound
  - raise exception when a variable is not resolved
- eoStripRecurringNewlines 
  - strip newlines that recurr
- eoTrimLines
  - trim whitespace from start and end of lines
- eoPrettyPrint
  - use to review the parsed structure. output is to the console.

<a name="Decimal_Separators"><h3>Decimal Separators</h3></a>

Numbers are commonly formatted using comma and decimal point. e.g. 123.45

However, in some regions, such as Germany, it the coma may be preferred. e.g. 123,45

In order to accomodate this, the context configuration has a DecimalSeparator. These default based on locale.

The DecimalSeparator may be set to '.' or ','. 

<a name="Value_Separators"><h3>Value Separators</h3></a>

The ValueSeparator may be set to ',' or ';'. It must be explicity set.

The motivation for the behaviour is to avoid any confusion with decimal separators.
```
<% Add(1.23 , 4.56) %>
```
When the DecimalSeparator is ',', then the ValueSeparator becomes ';' as illustrated:
```
<% Add(1,23 ; 4,56) %>
```
However, the following does work:
```
<% Add(1,23 , 4,56) %>
```

<a name="Custom_Whitespace"><h3>Custom Whitespace</h3></a>

You can provide overrides as required. e.g.

```
var ctx := Template.Context();
ctx.NewLine := #13#10;
ctx.NewLine := #10;
ctx.NewLine := '<br>'#13#10;
```

<a name="Custom_Newline"><h3>Custom Newline</h3></a>

You can provide overrides as required. e.g.

```
var ctx := Template.Context();
ctx.WhitespaceChar := #32;
ctx.WhitespaceChar := #183;
```
