# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Template Patterns

Here are some patterns that may be used when designing templates:
- Header/Footer
- Master/Detail

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