# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

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
- [Ignoring Whitespace With Multi-Line Statements](#Ignoring_Whitespace_With_Multi_Line_Statements)
- [Options](#Options)

# Overview

Configuration is done through the context. If you want to rely on the defaults, many of the eval methods don't require a context to be explictly provided and they will create create a default context for use.
```
    var ctx := Template.Context();
```
### Text encoding

The default default encoding is ASCII. You can change this to UTF8 as follows:
```
var ctx := Template.Context;
ctx.Encoding := TEncoding.UTF8;
```
### HTML Variable Encoding
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

### Custom Variable Encoding
Above was an example of HTML encoding. You can create custom encoding mechanism by setting VariableEncoder on a context.

```
type
  TTemplateEncodeFunction = reference to function(const AArg : string): string;
```
### Setting a maximum runtime
```
  var ctx := Template.Context();
  ctx.MaxRunTimeMs = 5;
  // ...
```
### Customise the start and end script tokens
You may want to change from using '<%' and '%>' to something else by updating the _StartToken_ and _EndToken_ on the context.
```
begin
  var ctx := Template.Context;
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Assert.IsEqual('hello', Template.Eval(ctx, '{{ if true }}hello{{else}}bye{{end}}'));
end;
```
### Custom Variables
If there are variables that should be set by default when using a template, they can
be propulated in the context as illustrated:
```
ctx.Variable['company'] := 'Sempare Limited'; 
```
### Reusing Templates
Using the _include()_ statement, you can reference precompiled templates that are registered on the context:

```
ctx.RegisterTemplate('header', Template.Parse('<% title %>')) 
ctx.RegisterTemplate('footer', Template.Parse('Copyright (c) <% year %> <% company %>')) 
```
### Dynamic Template Resolution

Templates don't need to be precompiled. They can also be located when being parsed by setting the template resolver property on the context:
```
ctx.TemplateResolver = function(const AContext : ITemplate; const AName : string) : ITemplate
begin
   result := Template.parse('some template loaded from file...');
end;
```

### Ignoring Whitespace With Multi-Line Statements

You may have a template something like:
```
	<% for i:=1 to 10 %>
	<% i %>
	<% end %>
```

Now it may be apparent that there will be a lot of newlines. You can minimise this using <| and |> statement start and end tokens.
```
	<% for i:=1 to 10 |>  this will be ignored
	   as will this <| print(i) |> and this
	and this<| end %>
```

The start/end statement tokens can be changed using the Context.StartStripToken and Context.EndStripToken

e.g.
```
begin
  var ctx := Template.Context;
  ctx.StartStripToken := '{~';
  ctx.EndStripToken := '~}';
  Assert.IsEqual('hello', Template.Eval(ctx, '{{ print('start') ~}ignore me{~ print('end') }}'));
end;
```

### Options

The template engine allows for the following options:
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
