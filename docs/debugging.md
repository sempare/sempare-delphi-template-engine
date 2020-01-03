# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Debugging the script behaviour

There is currently no interactive debugger. We use the pretty print feature to peek into the parsed abstract syntax tree of the template to confirm what we expect:
```
writeln(Velocity.PrettyPrint(Velocity.Parse('<%if true%>true<%else%>false<%end%>')));
```

Use the _eoPrettyPrint_ option on the _context_ to enable when evaluating or parsing a template.
