# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Debugging the script behaviour

There is currently no interactive debugger. We use the pretty print feature to peek into the parsed abstract syntax tree of the template to confirm what we expect:
```
writeln(Template.PrettyPrint(Template.Parse('<%if true%>true<%else%>false<%end%>')));
```

Use the _eoPrettyPrint_ option on the _context_ to enable when evaluating or parsing a template.
