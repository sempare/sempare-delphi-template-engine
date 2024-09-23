# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-202 [Sempare Limited](http://www.sempare.ltd)

## Whitespace Removal

Removing whitespace is a tricky problem in templates. In the template engine, attempts have been made to address this in a number of ways.

There are statements such as __ignorenl__ and __ignorews__ which allow for new lines and whitespaces to be stripped respectively. 

Further, on the context, there are various options: __eoStripRecurringNewlines__, __eoTrimLines__, __eoStripRecurringSpaces__ and __eoConvertTabsToSpaces__.

Below are some other ways in which hints can be provided to help trip whitespace and newlines.


### Using script tag hints

Scripts start with <% and close with %>. These tags may have additional hints to assist with the removal of whitespace.
 

#### Using -

```
hello&bull;&bull;&bull;&bull;<%- 'world' %>&bull;&bull;&bull;&bull;&bull;<NL>
```

This yields:
```
helloworld<NL>
```
 
whitespace is removed before the '<%-', whitespace and a single newline is removed after the '%>'
 
 
#### block statements

This works with block statements as well

e.g.
```
&bull;&bull;&bull;&bull;<%- if cond %>&bull;&bull;&bull;&bull;<NL>
&bull;&bull;&bull;&bull;<% 'hello' %>&bull;&bull;&bull;&bull;<NL>
&bull;&bull;&bull;&bull;<% end %&bull;&bull;&bull;&bull;<NL>
```

This results in:
```
&bull;&bull;&bull;&bull;hello&bull;&bull;&bull;&bull;<NL>
```
