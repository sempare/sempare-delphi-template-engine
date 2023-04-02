# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Whitespace Removal

v1.7.0 introduces some new concepts is still experimental and will change slightly. The new behaviour that will be provided in an v1.7.1 is described in the 'general notes' below.

Removing whitespace is a tricky problem in templates. In the template engine, attempts have been made to address this in a number of ways. 

### Using script tag hints

Scripts start with <% and close with %>. These tags may have additional hints to assist with the removal of whitespace.

| Hint | Note |
|---|---|
| - | Removes whitespace only.  |
| + | Removes whitespace as well as a newline, but leaves one space.  |
| * | Removes whitespace as well as a newline.  |

#### Using -

```
hello••••<%- 'world' %>••••<NL>
```

This yields:
```
helloworld••••<NL>
```

This stops on the first non whitespace character or newline. The newline will be preserved.

```
hello••••<%- 'world' -%>••••<NL>
```

This yields:
```
helloworld<NL>
```


#### Using +

```
hello••••<%+ 'world' %>••••<NL>
```

This yields:
```
hello•world••••<NL>
```

This stops on the first non whitespace character or newline. The newline will be preserved.

```
hello••••<%+ 'world' +%>••••<NL>
```

This yields:
```
hello•world•
```

#### Using *

```
hello••••<%+ 'world' *%>••••<NL>
```

This yields:
```
hello•world
```

This stops on the first non whitespace character or including newline. The newline will be removed.

#### Using around scripts with content

```
••••<%- if cond *%>••••<NL>
••••<% 'hello' %>••••<NL>
••••<%- end *%>••••<NL>
```

If cond is true, it evaluates to:
```
••••hello••••<NL>
```

Note that content ••••&lt;NL&gt; after *%> is part of the content rendered with the condition.

Similar for •••• before the <%- end *%>.

This is important to appreciate especially when dealing with looping.


#### General notes

- If you want to remove a script block from the output, use <%- *%>
- Use the template engine demo to play to see how it behaves.
- In v1.7.1, the basics described above will remain, however, in the case of script tags that have an end, the stripping action hints will be applied across all lines in the contained block.

e.g.
```
••••<%- if cond *%>••••<NL>
••••<% 'hello' %>••••<NL>
••••<% end %>••••<NL>
```

The above example illustrates what we will be doing in an upcoming release. Rather than having to apply the hinting at each level, the hinting will
be applied to all lines within the block.

Thus the above would result in the following if the cond is true:
```
hello
```