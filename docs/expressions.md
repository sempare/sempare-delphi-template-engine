# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

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

The following Delphi code illustrates the usage:
```
begin
  var l := TList<string>.Create;
  l.AddRange(['1','2','3']);
  Assert.IsEqual('123', Template.Eval('<% for v in _ %><% v %><% end %>', l));
  L.Free;
end;
```

If the variable is not found, the variable will be referenced on _.

This means that <% a %> will be the same as <% _.a %>.

# Strings

Strings can be single quote or double quotes. There is no special meaning on either. The backslash can be used for escaping.

```
<% 'that\'s nice' %>
<% "that's nice" %>
```
