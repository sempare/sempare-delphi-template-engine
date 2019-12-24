# ![](./images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

Contact: <info@sempare.ltd>

License: [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt)

Open Source: https://github.com/sempare/sempare.boot.velocity.oss

Version: 1.0

## Contents
1. [Introduction](#Introduction)
2. [Features](#Features)
3. [Objectives](#Objectives)
4. [Requirements](#Requirements)
5. [Feedback](#Feedback)
6. [Components](#Components)
   1. [Velocity](#Velocity)
   2. [IVelocityContext](#IVelocityContext)
   3. [IVelocityTemplate](#IVelocityTemplate)
7. [Statements](#Statements)
   1. [print](#print)
   2. [assignment](#assignment)
   3. [if](#if)
   4. [for](#for)
   5. [while](#while)
   6. [include](#include)
   7. [with](#with)
   8. [template](#template)
8. [Expressions](#Expressions)
9. [Builtin functions](#Builtin-functions)
10. [Custom functions](#Custom-functions)
11. [Scope](#Scope)
12. [Tricks](#Tricks)
13. [Configuration](#Configuration)
14. [Debugging the script behaviour](#Debugging-the-script-behaviour)
15. [Simplified Grammar definition](#Simplified-Grammar-definition)
16. [Design considerations](#Design-considerations)
17. [Known restrictions / limitations / bugs](#known-restrictions--limitations--bugs)
18. [Todo](#Todo)
## Introduction

Template engines are used often in in the technology where text needs to be customised by substituting variables with values from a data source. Examples where this may take place:
- web sites using template engines (or languages)
- code generation
- mail merge 
- notification messages 

The Sempare Boot Velocity Template Engine is small templating engine for [Delphi](https://www.embarcadero.com/products/delphi) that allows for templates to be created easily and efficiently by providing a simple and easy to use interface.

Example usage:
```
program Example;
uses
	Sempare.Boot.Template.Velocity;
type
    TInformation = record
        name: string;
        favourite_sport : string;
    end;
begin
    var template := Velocity.parse('My name is <% name %>. My favourite sport is <% favourite_sport %>.');
    var information : TInformation;
    information.name := 'conrad';
    information.favourite_sport := 'ultimate';
    writeln(Velocity.eval(template, information));	
end.
```

In the example above, you can see that the '<%' start and '%>' end the scripting statement respectively. Within a scripting statement, you can reference variables, assign variables, use conditions, for and while loops, and include other templates.

## Features
- statements
  - if, elif, else statements
  - for and while statements statements
  - include statement
  - with statement
  - function/method calls
- expressions
  - simple expression evaluation
  - variable references
  - call functions/methods
  - dereference records, classes, arrays and dynamic arrays
- safety
  - max runtime protection
- customisation 
  - custom script token replacement
  - add custom functions

## Objectives

Sempare Boot Velocity is not intended to be a fully featured general purpose programming language such as PHP where the script itself could be a self contained programming language.

Sempare Boot Velocity aims to provide just enough functionality to allow you to easily work with the 'view' aspects of a template. Any enhanced functionality required from the scripting environment should be provided by the function calls written in Pascal.

## Requirements
This should work with most modern versions of [Delphi](https://www.embarcadero.com/products/delphi). No special features have been used, so it should work on Free Pascal or require minimal changes (in the todo list)

Tests currently run on Delphi 10.3.3 using the DUnitX TestFramework.

<!--

Sempare Boot Velocity depends on the other Sempare Boot projects:
- [Sempare Boot Test](https://github.com/sempare/sempare.boot.test.oss)

 # Related work
The Sempare Boot Velocity template engine relates to the following projects:
- [Sempare Boot Common](https://github.com/sempare/sempare.boot.common.oss)
- [Sempare Boot Rtti](https://github.com/sempare/sempare.boot.rtti.oss)
- [Sempare Boot Http](https://github.com/sempare/sempare.boot.http.oss)
- [Sempare Boot Build](https://github.com/sempare/sempare.boot.build.oss)
- [Sempare Boot CLI](https://github.com/sempare/sempare.boot.oss)
-->

To see a full list of Sempare Boot projects visit https://www.sempare.ltd/sempare.boot (coming soon 2020)

## Feedback

If you have a support subscription, you can contact us via support@sempare.ltd and we will address the issue ASAP. 

You can also raise issues on [GitHub](https://github.com/sempare/sempare.boot.velocity.oss) and they will be addressed based on priority.

If you would like to support the development of this project, please feel free to make a financial contribution. Please contact info@sempare.ltd for more information.

Most features have some basic tests in place. If a bug has been discovered, please include a basic test/scenario replicating the issue if possible as this will ease the investigation process. At the end of this document is a listing of todo, design decisions and known bugs.

## Components
Sempare Boot Velocity uses interfaces extensively. This makes memory management much easier as interfaced objects are reference counted and alls for code to be more readable as minimal try/catch/finally block are required.

To make using Velocity easy, you need to just include a reference to the _Sempare.Boot.Template.Velocity_ unit:
```
uses
    Sempare.Boot.Template.Velocity;
```

Components of interest that are exposed to you are:
- Velocity
- IVelocityContext
- IVelocityTemplate

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


## Statements

### print

Within a script, all text outside of the script '<%' start and '%>' end tokens is output.

```
hello world
```

This outputs 'hello world' as expected.

```
<% stmt := 'is a' %>
This <% stmt %> test.
```
The above example results in _'this is a test.'_ being output.

### assignment
Within a script block, you can create temporary variables for use within the template. Basic types such a string, boolean and numbers can be used.
```
<% num := 123 %>
<% str := 'abc' %>
<% bool := false %>
```
### if
You may want to conditionally include content:
```
Some text
<% if condition %>
some conditional text
<% end %>
Some more text
```
In the above scenario, you can conditionally include some text by referencing the 'condition'. This could be a variable or a complex expression. If the condition expression is a non boolean value, such as a string or a number, the expression evaluates to true for a non empty string and a non zero number.

More complicated condition blocks can be created using 'elif' and 'else' statements.
```
some text
<% if condition %>
    block 1
<% elif condition2 %>
    block 2
<% elif condition3 %>
    block 3
<% else %>
    last block
<% end %>
    the end
```
### for
The 'for' loop comes in two forms as it does in Delphi:
```
increasing integers from 1 to 10
<% for i := 1 to 10 %>
    number <% i %>
<% end %>

decreasing integers from 10 down to 1
<% for i := 10 downto 1 %>
    number <% i %>
<% end %>
```
NOTE: Loop variables can be updated within the scope of the block without the loop integrity being compromised. 
The other 'for' variation is as follows:
```
Attendees:
<%for i in _ %> 
   <% i.name %> <% i.age %>
<%end%>'
```
With the following Delphi:
```
type
  TInfo = record
    Name: string;
    age: integer;
    constructor create(const n: string; const a: integer);
  end;
var
  info: TList<TInfo>;
begin
  info := TList<TInfo>.create;
  info.AddRange([TInfo.create('conrad', 10), TInfo.create('christa', 20)]);
  writeln(Velocity.Eval(template, info));
  info.Free;
end;
```
The above produces the output:
```
Attendees:
   conrad 10
   christa 20
```
### while
While blocks are very flexibe looping constructs based on a boolean condition being true.
```
<% i := 1 %>
<% while i <= 3 %>
   <% i %>
   <% i := 1 + 1%>
<% end %
```
This produces the following:
```
   1
   2
   3
```
NOTE: you must ensure the terminating condition is eventually true so that the template can be rendered.

## break / continue

The 'for' and 'while' loops can include the following 'break' and 'continue' statements to assist with flow control.

An example using 'continue':
```
<% for i := 0 to 10 %>
 <% if i mod 2 = 1 %><% continue %><% end %><% i %>
<% end %>
```
This will produce:
```
0 2 4 6 8 10 
```
An example using 'break':
```
<% i := 0 %>
<% while true %>
 <% if i = 3%><% break %><% end %><% i %>
<% end %>
```
This will produce
```
0 1 2
```

### include
You may want to decompose templates into reusable parts. You register templates on a Velocity context. 

```
type
    TInfo = record
        year : integer;
        company : string;
        email : string;
    end;
begin
   var ctx := Velocity.context();
   ctx['year'] := 2019;
   ctx['company'] := 'Sempare Limited';
   ctx['email'] := 'info@sempare.ltd';
   ctx.RegisterTemplate('header', Velocity.Parse('Copyright (c) <% year %> <% company %> '));
   ctx.RegisterTemplate('footer', Velocity.Parse('Contact us <% email %> '));
   var tpl := Velocity.parse('<% include (''header'') %> some content <% include (''footer'') %>');
```

include() can also take a second parameter, allowing for improved scoping of variables, similar to the _with_ statement.

### with

The with() statement is used to localise variables from a nested structure.

To illustrate, the following
```
type
	TInfo = record
		level1 : record
			level2 : record
				value : string;
				level3 : record
					level4 : record
						value : string;
					end;
				end;
			end;
		end;
	end;
begin
	var info : TInfo;
	info.level1.level2.value := 'test';
	info.level1.level2.level3.level4.value := 'hello';	
	Velocity.Eval('<% level1.level2.level3.level4.value %> <% level1.level2.level3.level4.value %> <% level1.level2.level3.level4.value %>', info)

	// can be replaced with
	Velocity.Eval('<% with level1.level2.level3.level4 %><% value %> <% value %> <% value %><% end %>', info)

```

The _with()_ statement will push all fields/properties in a record/class into a new scope/stack frame. 

### template

Localised templates can also be defined locally within a template.

The _include_() statement is used to render a local template as is the normal behaviour. Note that local templates take precedence over templates defined in a context when they are being resolved.

Using the TInfo structure above it could be appli
```
<% template 'mytemplate' %>
	<% value %>
<% end %>
<% include ('mytemplate', level1.level2) %>	
<% include ('mytemplate', level1.level2).level3.level4 %>	
```

## Expressions
You can use different types of expressions.

Conditional expressions:
```
<% a := 10 %>
<% if a + 1 < 10 %>
<% end %>
```

Comparison expressions ( =, <>, <, <=, >, >= ) work on numeric and string values.

Numeric operators (+, -, /, *, mod) work on numeric values.
The + operator also works on strings so you can append values.

# The special _ variable
A special variable _ (underscore) is defined to allow access to the variable/record/class passed into the template evaluator. 

```
<% for i in _ %> <% i %><% end %>
```
where the Delphi code could be:
```
begin
  var l := TList<string>.Create;
  l.AddRange(['1','2','3']);
  Assert.IsEqual('123', Velocity.Eval('<% for v in _ %><% v %><% end %>', l));
  L.Free;
end;
```
When a variable is referenced, the evaluation scope will be referenced. if the variable is not found, the variable will be
referenced on _.

This means that <% a %> will be the same as <% _.a %>.

## Builtin functions
The following builtin functions are available:
## trim(string)
Remove whitespace from a string.

```<% trim('  hello  ') %>```
## substr(string, start[, length])
Return a substring starting at 'start'. If 'start' is less than 0, it will be the offset from the end of the string.

```<% substr('  hello  ', 2, 5) %>```
## substring(string, start[, end])
Return a substring starting at 'start'. If 'start' or 'end' is less than 0, it will be the offset from the end of the string.

```<% substring('  hello   ', 2, 6) %>```
## pos(substr, string[, offset])
Return an offset of substr within string from offset. if offset is less than 0, it will be the offset from the end of the string.

``` pos('lo', 'hello') ```
## dtnow()
Returns the SysUtils now() date time.
``` <% dtnow() %>```
## fmtdt(format[, dt])
Formats a date time using FormatDateTime

``` <% fmtdt('yyyy-mm-dd', dtnow()) %>```
	
For formatting options, see http://docwiki.embarcadero.com/Libraries/Rio/en/System.SysUtils.FormatDateTime#Description
## fmt(format[, args...])
Allows a string to be formatted using SysUtils format().

```<% fmt('%s %s %d', 'hello','world', 123) %> ```
	
For formatting options, see: http://docwiki.embarcadero.com/Libraries/Rio/en/System.SysUtils.Format#Format_Strings
## len(string)
Return the length of a string.

```<% len('hello world') %>```
## str(any) / isstr(any)
str() casts a variable to a string. isstr() checks if a variable is a string.
```
<% str(123) + ' = one two three' %>
```
## int(any) / isint(any)
int() casts a number to an integer. isint() checks if a variable is an integer.
## Custom functions
Defining new functions is very easy by adding it to the context.
```
    ctx.AddFunction('trim', 1,
      function(const Args: TArray<TValue>): TValue
      begin
        result := trim(AsString(Args[0]));
      end);
```
The AddFunction method has a few signature:
```
    procedure AddFunction(const AFunctionInfo: TVelocityFunctionInfo); overload;
    procedure AddFunction(const AFunctionName: string; const AFunction: TVelocityFunction); overload;
    procedure AddFunction(const AFunctionName: string; const ANumArgs: integer; const AFunction: TVelocityFunction); overload;
    procedure AddFunction(const AFunctionName: string; const ASignature: string; const AFunction: TVelocityFunction); overload;
    procedure AddFunction(const AFunctionName: string; const ASignature: string; const AMinArgs, AMaxArgs: integer; const AFunction: TVelocityFunction); overload;
    procedure AddFunction(const AFunctionName: string; const AMinArgs, AMaxArgs: integer; const AFunction: TVelocityFunction); overload;

```
Signature, MinArgs and MaxArgs are helper arguments for script validation. MinArgs and MaxArgs can be -1, where no validation is done. 

Signature describes the function arguments and return type. The following letters are used in a signature defintition:
- S string
- N number
- I integer
- D date time
- B boolean

The first character represents the return type and followed by parameter 1 to parameter n.
e.g. A signature of SSNN would mean that:
- return type is a string
- param 1 is a string
- param 2 is a number
- param 3 is a number

Note an alias TVelocityValue is made (which is System.Rtti.TValue). This is exposed so you don't need to add an extra include to System.Rtti in your unit when using Velocity and adding functions, although you can if you want to.

## Scope
Scopes are created in 'if', 'for', 'with' and 'while' statements. Referencing a variable that is not in the current scope will propogate to parent scopes.

## Tricks

### Reference variables dynamically
You can dereference variables dyanmically
```
type
    TRecord = record
        v1 : string;
        v2 : string;
        v3 : string;
    end;
begin
    var rec : TRecord;
    with rec do begin
        v1 := 'a';
        v2 := 'b';
        v3 := 'c';
        v4 := 'd';
    end;
    writeln(Velocity.Eval('<%for i := 1 to 4 %><% v['v' + i] %><% end %>'));
end;
```
will produce
```
abcd
```
### Improving performance on larger templates

A quick win would be to use buffered stream so that characters in the buffer are processed quickly.

## Configuration
Configuration is done through the context. If you want to rely on the defaults, many of the eval methods don't require a context to be explictly provided and they will create create a default context for use.
```
    var ctx := Velocity.Context();
```
### Text encoding

The default default encoding is ASCII. You can change this to UTF8 as follows:
```
var ctx := Velocity.Context;
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
  var ctx := Velocity.Context;
  ctx.UseHtmlVariableEncoder;

  Assert.IsEqual('<html><body>a &lt; b</body></html>', Velocity.Eval(ctx, '<html><body><% content %></body></html>', data));
end;
```

### Custom Variable Encoding
Above was an example of HTML encoding. You can create custom encoding mechanism by setting VariableEncoder on a context.

```
type
  TVelocityEncodeFunction = reference to function(const AArg : string): string;
```
### Setting a maximum runtime
```
  var ctx := Velocity.Context;
  ctx.MaxRunTimeMs = 5;
  // ...
```
### Customise the start and end script tokens
You may want to change from using '<%' and '%>' to something else by updating the _StartToken_ and _EndToken_ on the context.
```
begin
  var ctx := Velocity.Context;
  ctx.StartToken := '{{';
  ctx.EndToken := '}}';
  Assert.IsEqual('hello', Velocity.Eval(ctx, '{{ if true }}hello{{else}}bye{{end}}'));
end;
```
### Custom Variables
If there are variables that should be set by default when using a template, they can
be propulated in the context as illustrated:
```
ctx.Variable['company'] := 'Sempare Limited'; 
```
### Reusing Templates
Using the _include()_ statement, you can reference precompiled registered templates.

```
ctx.RegisterTemplate('header', Velocity.Parse('<% title %>')) 
ctx.RegisterTemplate('footer', Velocity.Parse('Copyright (c) <% year %> <% company %>')) 
```
### Dynamic Template Resolution

Templates don't need to be precompiled. They can also be located when being parsed by setting
the template resolver property on the context.
```
ctx.TemplateResolver = function(const AContext : IVelocityTemplate; const AName : string) : IVelocityTemplate
begin
   result := Velocity.parse('some template loaded from file...');
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

## Debugging the script behaviour
We use the pretty print feature to peek into the parsed abstract syntax tree of the template.
```
writeln(Velocity.PrettyPrint(Velocity.Parse('<%if true%>true<%else%>false<%end%>')));
```

## Simplified Grammar definition

NOTE: normal precedence rules apply. expression definition below is simplified to keep it short.
```
main     : text 
         | stmts
         ;
text     : textblock
         ;
stmts    : stmt*
         ;
stmt     : if 
         | for
         | while
         | assign
         | include
         | template
         | with
         ;
loopstmts: loopstmt*
         ;
loopstmt : stmt
         | break
         | continue
         ;
include  : 'include' '(' expr [, expr ]')'
         ;
if       : 'if' expr stmts ('elif' expr stmts)* ('else' stmts) 'end'
         ;
for      : 'for' id 'in' expr loopstmts 'end'
         | 'for' id ':=' expr ('to'|'downto') loopstmts 'end'
         ;
while    : 'while' expr loopstmts 'end'
         ;
template : 'template'  expr  stmts 'end'
         ;
with     : 'with'  expr  stmts 'end'
         ;
assign   : id ':=' expr
         ;
exprlist : expr? (',' expr)*
         ;
expr     : id '(' exprlist ')'
         | id
         | id '.' id
         | id '[' expr ']'
         | '-' expr
         | 'not' expr
         | '(' expr ')'
         | expr ('+' | '-' | '/' | '*' | 'mod' ) expr
         | expr ('and' | 'or' ) expr
         ;
literal  : 'true'
         | 'false'
         | string
         | number
         ;
```
The 'string' literal is anything contained within single quoted string.
A 'number' is currently limited to being an integer value (e.g. 1, 2, 3).

## Design considerations

### no repeat/until
- repeat/until is not included as a 'while' loop can accomplish the same.

### no bytecode

The evaluator is a very simple. Parsing is fairly quick, so byte code is considered overkill.

## Known restrictions / limitations / bugs

### Lack of floating point double support
Parsed numbers are limited to integer values, but values returned from function/method calls preserve their types. This has been done to minimise the temptation to do more calculations in the templating layer.

### optimisation on variable dereferencing

this needs to be reviewed. the current implementation is not ideal as the TVariableScope is constantly populated with contents of any structure.
if the structure is large, this may not be ideal. further, for arrays to be dereferenced, the key needs to also be able to be numeric.

## Todo
- add options
  - to not fail but use a default value (blank) if a variable is not resolved
  - to strip recurring newlines 
  - to strip empty lines
  - to trim lines
- evaluate expressions at parse time based on context variables is pending
- validation
  - identify required variables ahead of time where possible.
  - review validation implentation on function calls
- newline to custom newline encoding. e.g. nl -> &lt;br>nl
- grammar
  - in expression, so could do something like 'if a in b'
- interactive script debugger
- create bindings to data sources like TDataSource, TFDMemTable, TFDQuery, etc...
- create design time components for those that like them
		- TemplateComponent (wrapper around TVelocityTemplate)
			- TemplateVariable (to allow for live bindings)
- cleanup (as usual always to be done)
- review generic usage on Velocity class to minimise size of binary.
- review free pascal and support for older versions of Delphi
- review variable not found error - could be custom value, custom error.
### potential grammar extensions from above

```
         
         
expr     : // all of the current expr elements
         | expr 'in' expr
         | '[' exprlist ']'
         ;
         
```