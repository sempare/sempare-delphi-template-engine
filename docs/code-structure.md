# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Code Structure

The main entry point is in the _Sempare.Template.pas_.

The Sempare Template Engine has the following files:

__Sempare.Template.AST.pas__

All interfaces used by the parser and evaluation engine are defined in this file.

_ITemplateVisitor_ is also defined (visitor pattern) which is used by the pretty printer and the evaluation engine.

__Sempare.Template.Parser.pas__

This is a recursive descent parser.

__Sempare.Template.Lexer.pas__

The lexical analyser _GetToken()_ will return an _ITemplateSymbol_

_ITemplateSymbol_ has a _TTemplateSymbol_ (an enum matching the various symbol types). It also includes an IPosition.

_IPosition_ includes:
- filename
- line
- position

Note the lexer has two modes:
- Script Mode
  - This is where statements are defined between <% and %> 
- Text Mode 
  - This is where text appears outside of <% and %>.

__Sempare.Template.Context.pas__

This is where the _TTemplateContext_ is actually defined that implement _ITemplateContext_.

__Sempare.Template.Evaluate.pas__

Contains the template evaluation engine.

__Sempare.Template.PrettyPrint.pas__

Contains the Pretty Printer used for debugging.

__Sempare.Template.Rtti.pas__

This may be useful when defining custom functions that need to operate on TValue variables.

__Sempare.Template.StackFrame.pas__

Defines the stack frame utility used by the evaluation engine.
