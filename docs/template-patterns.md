# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Template Patterns

Here are some patterns that may be used when designing templates:
- Extends/Block
- Header/Footer
- Master/Detail

When you have many templates, you may want to leverage a ITemplateContext.TemplateResolver.

### Extends/Block

In this pattern, you may have a parent template parent.tpl:
```
<% block 'body' %>some random content here<% end %>
```

In a child template:
```
<% extends 'parent.tpl'%>
      // this is ignored
      <% block 'body' %>this content will appear in the body<% end %>
      // this is ignored
<% end%>
```

Note: that the extends container is the only container where text outside of a block is ignored.

When the child template is evaluated, it will replace all instances of 'body'.

### Header/Footer

In this pattern, you may have a set of templates like the following:

header.tpl:
```
<html>
<header>
</header>
<body>
```

footer.tpl:
```
</body>
</html>
```

atemplate.tpl:
```
<% include('header') %>

<p>my content</p>

<% include('footer') %>

```

This pattern can be used along with a TemplateLoader which can load the templates from whereever they are stored.

### Master/Detail

In this pattern, it may be easier to maintain the header/footer together.

master.tpl:
```
<html>
<header>
</header>
<body>
<% include(page) %>
</body>
</html>

```

somepage.tpl:
```
<p>my content</p>
```

In this pattern, you rely on the 'page' variable being passed to the template engine, much like any other variable.

illustrative usage:
```
var
   rec : record
      page:string;
      // any other data here
   end;
begin
   // assume ctx.TemplateLoader is defined 
   rec.page := 'somepage.tpl';
   
   var output : string := Template.Eval(masterTpl, rec);
   writeln(output);
end;
```