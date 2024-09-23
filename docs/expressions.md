# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2024 [Sempare Limited](http://www.sempare.ltd)

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

Futher, == is an alias for =, and != is an alias for <>.

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

# Arrays

Arrays can be defined with square braces. e.g.

```
<% arr := [ 1, 2, 3] %>
```

There is no requirement for the values to be of the same type.

```
<% arr := [ 1, false, "string" ] %>
```

Dereferencing an array is 0 based. e.g.
```
num := arr[0];
bool := arr[1];
str := arr[2];
```

Also see the [for-in/for-of](./statements.md#for) loop statements.

In the above, the for-in statement will return 1, 2, 3
In the above, the for-of statement will return  1, false, "string"

# Maps

Simple map / dictionary support is provided. You may define a map withing curly braces. e.g.

```
<% map := { 
        "a" : 1, 
        "b": true, 
        "c": "string", 
        "d": { 
            "key" : "value" 
        } 
  } %>
```

Note again that there is no requirement for the types to be of the same type.


Dereferencing a map is as follows:
```
<% key := map['a'] %> // or
<% key := map.a %>    // for keys without spaces and match the identifier syntax 
```

Also see the [for-in/for-of](./statements.md#for) loop statements.


In the above, the for-in statement will enumerate over the keys.
In the above, the for-of statement will enumerate over the values.

*NOTE* Values in a map are immutable. This means that you need to recreate the entire map if you need to change a value.

This is currently due to a limitation in the assignment statement implementation which currently assigns simple variables only. We will review this in future.