# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Parse Time Evaluation

Parse time evaluation is done by enabling the options eoEvalEarly and eoEvalVarsEarly in the _context_ options.

The requirement for parse time evaluation is that variables must be defined within the context. This is useful when you know certain information is _static_ and is unlikely to change.

The following is an example of a template that would be eligable for parse time evaluation:
```
<% year := 2019 %>

<% if year = 2019 %>

The year is twenty nineteen

<% else %>

The year is <% year %>

<% end %>
```

In the example above, the variable _year_ was declared in the template itself.

The following would be equivalent:
```
var ctx := Template.Context();
ctx.Options := [eoEvalEarly, eoEvalVarsEarly];
ctx.Variable['year'] := 2019;

var template := Template.Parse('<% if year = 2019 %>twenty nineteen<% else %><%year%><%end%>');

writeln(Template.Eval(ctx, template));


```

In the above scenario, the template is parsed. When the template is evaluated the conditional expression is no longer evaluated as it was preprocessed during the parsing stage.

Parse time evaluation can be applied to most expressions - logical expressions, numeric and string expressions. Functions and method calls are not currently supported, but could be.
