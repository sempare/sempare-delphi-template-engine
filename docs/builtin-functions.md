# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Builtin functions

- [trim](#trim)
- [substr](#substr)
- [substring](#substring)
- [pos](#pos)
- [dtnow](#dtnow)
- [fmtdt](#fmtdt)
- [fmt](#fmt)
- [len](#len)
- [str](#str)
- [isstr](#isstr)
- [int](#int)
- [isint](#isint)
- [num](#num)
- [isnum](#isnum)
- [split](#split)
- [rev](#rev)
- [ucfirst](#ucfirst)
- [uppercase](#uppercase)
- [lowercase](#lowercase)
- [startswith](#startswith)
- [endswith](#endswith)
- [typeof](#typeof)
- [replace](#replace)
- [match](#match)
- [sort](#sort)
- [chr](#chr)
- [ord](#ord)
- [padleft](#padleft)
- [padright](#padright)
- [tabs](#tabs)
- [spaces](#spaces)
- [crnl](#crnl)
- [nl](#nl)


## trim(string)
Remove whitespace from a string.

```<% trim('  hello  ') %>```
## substr(string, start[, length])
Return a substring starting at 'start'. If 'start' is less than 0, it will be the offset from the end of the string.

```<% substr('  hello  ', 2, 5) %>```
## substring(string, start[, end])
Return a substring starting at 'start'. If 'start' or 'end' is less than 0, it will be the offset from the end of the string.

```<% substring('  hello   ', 2, 6) %>```
## pos(substr, string[, offset])
Return an offset of substr within string from offset. if offset is less than 0, it will be the offset from the end of the string.

``` pos('lo', 'hello') ```
## dtnow()
Returns the SysUtils now() date time.
``` <% dtnow() %>```
## fmtdt(format[, dt])
Formats a date time using FormatDateTime

``` <% fmtdt('yyyy-mm-dd', dtnow()) %>```
	
For formatting options, see http://docwiki.embarcadero.com/Libraries/Rio/en/System.SysUtils.FormatDateTime#Description
## fmt(format[, args...])
Allows a string to be formatted using SysUtils format().

```<% fmt('%s %s %d', 'hello','world', 123) %> ```
	
For formatting options, see: http://docwiki.embarcadero.com/Libraries/Rio/en/System.SysUtils.Format#Format_Strings
## len(string)
Return the length of a string or an array.

```<% len('hello world') %>```
## str(any) / isstr(any)
str() casts a variable to a string. isstr() checks if a variable is a string.
```
<% str(123) + ' = one two three' %>
```
## int(any) / isint(any)
int() casts a number to an integer. isint() checks if a variable is an integer.

## num(any) / isnum(any)
num() casts the variable to a number. isnum() checks if a variable is an interfer or vloat.

## split(string, sep)

Split the string using a seperator returning an array of string.

```
<% split('hello world', ' ')[0] %>
```

## rev(string)

Reverse a string.

```
<% rev('abc') %>
```

## ucfirst(string)

Uppercase the first letter of a string with the rest being lowercase.

```
<% ucfirst('hello') %>
```

## uppercase(string)

Uppercase a string.

```
<% uppercase('heLlo') %>
```

## lowercase(string)

Lowercase a string.
```
<% lowercase('heLlo') %>
```

## startswith(string, substr[, ignoreCase=true])
Check if a string starts with another.
```
<% startswith('heLlo', 'he') %>
```

## endswith(string, substr[, ignoreCase=true])
Check if a string ends with another.
```
<% endswith('heLlo', 'lo') %>
```

## typeof(obj)
Return the class name of an object.

```
<% typeof(myObj) %>
```

## replace(search, replacement, str)
Replace some text with another.
```
<% replace('a', 'hello ', 'aaa') %>
```

## match(text, regex)
Matches text in a regular expression.
```
<% match('aaaaaaaaaaaaaa', 'a+') %>
```
For more information, see http://docwiki.embarcadero.com/Libraries/Rio/en/System.RegularExpressions.TRegEx.Matches

## sort(array)
Sorts basic arrays of integer, double, extended and string or something enumerable of these types.
```
<% values := sort(split('g,f,d,s,d,a', ',')) %>
```

## chr(val)
Convert a numeric value to the character counterpart.
```
<% chr(10) %>
```

## ord(char)
Convert a character to the numeric counterpart.
```
<% ord('a') %>
```

## padleft(str, len[, padchar=' '])
Pad string with padchar from the left till the string is len long.
```
<% padleft('123', 6) %>
```

## padright(str, len[, padchar=' '])
Pad string with padchar from the right till the string is len long.
```
<% padright('123', 6) %>
```

## tabs(len)
Return len tabs
```
<% tabs(2) %>
```

## spaces(len)
return len spaces
```
<% spaces(2) %>
```

## crnl(len)
return a string with len #13#10
```
<% crnl(2) %>
```

## nl(len)
return a string with len #10
```
<% nl(2) %>
```
