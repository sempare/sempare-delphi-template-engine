# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Debugging the script behaviour

There is currently no interactive debugger. We use the pretty print feature to peek into the parsed abstract syntax tree of the template to confirm what we expect:
```
writeln(Template.PrettyPrint(Template.Parse('<%if true%>true<%else%>false<%end%>')));
```

Use the _eoPrettyPrint_ option on the _context_ to enable when evaluating or parsing a template.

## Strange parser errors

Parser errors could come from one of two locations:
- the lexer - the code that breaks the stream into tokens
- the parser - the code that validates that the tokens are in the correct order according to the gramatical rules


