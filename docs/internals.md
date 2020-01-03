# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Internals

The main entry point is in the _Sempare.Boot.Template.Velocity.pas_.

The Sempare Boot Velocity template engine has the following files:

__Sempare.Boot.Template.Velocity.AST.pas__

All interfaces used by the parser and evaluation engine are defined in this file.

_IVelocityVisitor_ is also defined (visitor pattern) which is used by the pretty printer and the evaluation engine.

__Sempare.Boot.Template.Velocity.Parser.pas__

This is a recursive descent parser.

__Sempare.Boot.Template.Velocity.Lexer.pas__

The lexical analyser _GetToken()_ will return an _IVelocitySymbol_

_IVelocitySymbol_ has a _TVelocitySymbol_ (an enum matching the various symbol types). It also includes an IPosition.

_IPosition_ includes:
- filename
- line
- position

Note the lexer has two modes:
- Script Mode
  - This is where statements are defined between <% and %> 
- Text Mode 
  - This is where text appears outside of <% and %>.

__Sempare.Boot.Template.Velocity.Context.pas__

This is where the _TVelocityContext_ is actually defined that implement _IVelocityContext_.

__Sempare.Boot.Template.Velocity.Evaluate.pas__

Contains the template evaluation engine.

__Sempare.Boot.Template.Velocity.PrettyPrint.pas__

Contains the Pretty Printer used for debugging.

__Sempare.Boot.Template.Velocity.Rtti.pas__

This may be useful when defining custom functions that need to operate on TValue variables.

__Sempare.Boot.Template.Velocity.StackFrame.pas__

Defines the stack frame utility used by the evaluation engine.