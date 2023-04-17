# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Tricks

- [Functional Content](#functional-content)
- [Dynamic variable references](#dynamic-variable-resolution)
- [Loading templates dynamically](#dynamic-loading-templates)
- [Empty Value Evaluation](#empty-value-evaluation)

<a name="functional-content"><h2>Functional content within templates</h2></a>
 

The template engine does not allow for functions to be defined within templates. Developers can expose functions to the scripting environment using [custom functions](./custom-functions.md).

However, what may not necessarily be evident is that the 'include' and 'extends' statements can take an extra parameter to customise behaviour.

The template may be defined as:
```
<% template ('template' %><% _ %><% end %>
```

The _ expression evaluates to the value passed to the template. 

Using includes:
```
<% include ('template', 'hello') %> <% include ('template', 'world') %>
```

Using extends:
```
<% extends ('template', 'hello') %><% end %> <% extends ('template', 'world') %><% end %>
```

The above examples should output
```
hello world
```

The above illustrates some powerful capabilities where the template could actually do anything such as looping through structures and dereferencing very specific content.

<a name="dynamic-variable-resolution"><h2> Reference variables dynamically</h2></a>
 
You can dereference variables dyanmically using the array dereferencing syntax:
```
type
    TRecord = record
        v1 : string;
        v2 : string;
        v3 : string;
        v4 : string;
    end;
begin
    var rec : TRecord;
    with rec do begin
        v1 := 'a';
        v2 := 'b';
        v3 := 'c';
        v4 := 'd';
    end;
    writeln(Template.Eval('<%for i := 1 to 4 %><% _["v" + i] %><% end %>', rec));
end;
```
The above example will produce:
```
abcd
```

<a name="dynamic-loading-templates"><h2>Loading templates dynamically</h2></a>

If you reference templates using the 'include' statement, you can rely on a TemplateResolver or TemplateResolverWithContext to load them if they are not
defined within the template itself. Only TemplateResolver or TemplateResolverWithContext should be set. TemplateResolver is essentially ignoring the context.

Using a resolve context can be useful in scenarios such as web, where a language specific template can be resolved based on request headers (the http request object can be the resolve context).

e.g. This is illustrative:

```
  ctx := Template.Context;
  ctx.TemplateResolver := function(AContext: ITemplateContext; const ATemplate: string): ITemplate
    var
      LStream : TFileStream;
    begin
      LStream := TFileStream.Create(ATemplate + '.tpl', fmOpenRead);
      exit(Template.Parse(AContext, LStream));
    end;
    
  ctx.TemplateResolverWithContext := function(AContext: ITemplateContext; const ATemplate: string; const AResolveContext: TTemplateValue): ITemplate
    begin
      // ... note TTemplateValue is an RTTI TValue. 
    end;
```

Also see the section on the [Template Registry](./template-registry.md).

<a name="empty-value-evaluation"><h2>Empty Value Evaluation</h2></a>

When a variable has a non default value, it can be used in condition statements:
```
<% a := false %>
<% if a %>
  value is set 
<% end %>


<% a := 'some value' %>
<% if a %>
  value is set 
<% end %>
```

If you have passed an object, the boolean check will essentially check that the value is not nil:
```
var x := TPerson.Create;
```
The template could check as follows:
```
<% if x %>
  <% x.name %>
<% end %>

```

This logic also applies to arrays, collections and TDataSet. 

You could do the following:
```
<% if collection.count > 0  %>
  <% collection.count %>
<% end %>

```

You could simply do the following. It is useful knowing that the collection is not nil, but you probably are going to only work with the collection if it actually has values!
```
<% if collection  %>
  <% collection.count %>
<% end %>

```

Similarly for TDataSet.

```
<% if dataset.recordcount > 0  %>
   <% dataset.recordcount %>
<% else %>
   There are no records
<% end %>

```

You could simply do the following. It is useful knowing that the collection is not nil, but you probably are going to only work with the collection if it actually has values!
```
<% if dataset  %>
  <% for record in dataset %>
    <% record.name %>
  <% end %>
<% else %>
   There are no records
<% end %>
```

However, you could simplify the above to the following:
```
<% for record in dataset %>
     <% record.name %>
  <% onempty %>
     There are no records
<% end %>
```
