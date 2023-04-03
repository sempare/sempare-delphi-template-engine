# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Builtin functions

- char functions
  - [chr](#chr)
  - [ord](#ord)

- date functions
  - [dtnow](#dtnow)
  - [fmtdt](#fmtdt)

- dataset functions
  - [isdataset](#isdataset)
  - [recordcount](#recordcount)

- encoding/decoding functions
  - [base64decode](#base64decode)
  - [base64encode](#base64encode)
  - [formdecode](#formdecode)
  - [htmlescape](#htmlescape)
  - [htmlunescape](#htmlunescape)
  - [urldecode](#urldecode)

- hashing functions
  - [md5](#md5)
  - [sha1](#sha1)
  - [sha256](#sha256)

- misc functions
  - [isempty](#isempty)
  - [sort](#sort)
  - [templateexists](#templateexists)

- numeric functions
  - [abs](#abs)
  - [min](#min)
  - [max](#max)

- string functions
  - [crnl](#crnl)
  - [endswith](#endswith)
  - [fmt](#fmt)
  - [len](#len)
  - [lowercase](#lowercase)
  - [match](#match)
  - [nl](#nl)
  - [padleft](#padleft)
  - [padright](#padright)
  - [pos](#pos)
  - [replace](#replace)
  - [rev](#rev)
  - [spaces](#spaces)
  - [split](#split)
  - [startswith](#startswith)
  - [substr](#substr)
  - [substring](#substring)
  - [tabs](#tabs)
  - [trim](#trim)
  - [ucfirst](#ucfirst)
  - [uppercase](#uppercase)

- type functions
  - conversion functions
    - [bool](#bool)
    - [int](#int)
    - [num](#num)
    - [str](#str)
  
  - type checking functions 
    - [isbool](#isbool)
    - [isint](#isint)
    - [isnil](#isnil)
    - [isnum](#isnum)
    - [isobject](#isobject)
    - [isrecord](#isrecord)
    - [isstr](#isstr)
    - [typeof](#typeof)
    
    
# char functions

<a name="chr"><h3>chr(val)</h3></a>
Convert a numeric value to the character counterpart.
```
<% chr(10) %>
```

<a name="ord"><h3>ord(char)</h3></a>
Convert a character to the numeric counterpart.
```
<% ord('a') %>
```

# date functions

<a name="dtnow"><h3>dtnow()</h3></a>
Returns the SysUtils now() date time.
``` <% dtnow() %>```

<a name="fmtdt"><h3>fmtdt(format[, dt])</h3></a>
Formats a date time using FormatDateTime

``` <% fmtdt('yyyy-mm-dd', dtnow()) %>```

For formatting options, see http://docwiki.embarcadero.com/Libraries/Rio/en/System.SysUtils.FormatDateTime#Description


# dataset functions 

<a name="isdataset"><h3>isdataset(object)</h3></a>
return true if the object is a TDataSet
```
<% isdataset(ds) %>
```

<a name="recordcount"><h3>recordcount(dataset)</h3></a>
return the length of a dataset
```
<% RecordCount(ds) %>
```

# encoding/decoding functions

<a name="base64decode"><h3>base64decode(string)</h3></a>
returns the base64 decoded value
```
<% base64decode('aGVsbG8gd29ybGQ=') %>
```

<a name="base64encode"><h3>base64encode(string)</h3></a>
returns the base64 encoded value
```
<% base64encode('hello world') %>
```

<a name="htmlescape"><h3>htmlescape(string)</h3></a>
returns the html escaped value
```
<% htmlescape('<a href="https://www.google.com">google</a>') %>
```

<a name="htmlunescape"><h3>htmlunescape(string)</h3></a>
returns the html escaped value
```
<% htmlunescape('&amp;lt;a href=&amp;quot;https://www.google.com&amp;quot;&amp;gt;google&amp;lt;/a&amp;gt;') %>
```

# hashing functions
<a name="md5"><h3>md5(string)</h3></a>
returns the md5 hash of the string
```
<% md5('hello world') %>
```

<a name="sha1"><h3>sha1(string)</h3></a>
returns the sha1 hash of the string
```
<% sha1('hello world') %>
```

<a name="sha256"><h3>sha256(string)</h3></a>
returns the sha256 hash of the string
```
<% sha1('hello world') %>
```

# misc functions

<a name="isempty"><h3>isempty(value)</h3></a>
Returns true if the collection is an object, it is not nil, and it contains values. If you have custom containers, the rtti unit provides RegisterEmptyObjectCheck() to enhance this method.
```
<% isempty(list) %>
<% isempty(dict) %>
<% isempty(queue) %>
<% isempty(stack) %>
<% isempty(dataset) %>
```

<a name="sort"><h3>sort(array)</h3></a>
Sorts basic arrays of integer, double, extended and string or something enumerable of these types.
```
<% values := sort(split('g,f,d,s,d,a', ',')) %>
```

<a name="templateexists"><h3>templateexists(string)</h3></a>
returns true if the template exists
```
<% templateexists('template') %>
```

# numeric functions

<a name="abs"><h3>abs(value)</h3></a>
return the absolute value of a value
```
<% abs(-123.45) %>
```

<a name="min"><h3>min(adouble, bdouble)</h3></a>
return the minimum of two values
```
<% min(1,2) %>
```

<a name="max"><h3>max(adouble, bdouble)</h3></a>
return the maximum of two values
```
<% max(1,2) %>
```

# string functions

<a name="crnl"><h3>crnl(len)</h3></a>
return a string with len #13#10
```
<% crnl(2) %>
```


<a name="endswith"><h3>endswith(string, substr[, ignoreCase=true])</h3></a>
Check if a string ends with another.
```
<% endswith('heLlo', 'lo') %>
```

<a name="fmt"><h3>fmt(format[, args...])</h3></a>
Allows a string to be formatted using SysUtils format().

```<% fmt('%s %s %d', 'hello','world', 123) %> ```

For formatting options, see: http://docwiki.embarcadero.com/Libraries/Rio/en/System.SysUtils.Format#Format_Strings

<a name="len"><h3>len(string)</h3></a>
Return the length of a string or an array.

```<% len('hello world') %>```

<a name="lowercase"><h3>lowercase(string)</h3></a>
Lowercase a string.
```
<% lowercase('heLlo') %>
```

<a name="match"><h3>match(text, regex)</h3></a>
Matches text in a regular expression.
```
<% match('aaaaaaaaaaaaaa', 'a+') %>
```
For more information, see http://docwiki.embarcadero.com/Libraries/Rio/en/System.RegularExpressions.TRegEx.Matches

<a name="nl"><h3>nl(len)</h3></a>
return a string with len #10
```
<% nl(2) %>
```


<a name="padleft"><h3>padleft(str, len[, padchar=' '])</h3></a>
Pad string with padchar from the left till the string is len long.
```
<% padleft('123', 6) %>
```

<a name="padright"><h3>padright(str, len[, padchar=' '])</h3></a>
Pad string with padchar from the right till the string is len long.
```
<% padright('123', 6) %>
```

<a name="pos"><h3>pos(substr, string[, offset])</h3></a>
Return an offset of substr within string from offset. if offset is less than 0, it will be the offset from the end of the string.

``` pos('lo', 'hello') ```
<a name="replace"><h3>replace(search, replacement, str)</h3></a>
Replace some text with another.
```
<% replace('a', 'hello ', 'aaa') %>
```

<a name="rev"><h3>rev(string)</h3></a>
Reverse a string.

```
<% rev('abc') %>
```

<a name="spaces"><h3>spaces(len)</h3></a>
return len spaces
```
<% spaces(2) %>
```

<a name="split"><h3>split(string, sep)</h3></a>
Split the string using a seperator returning an array of string.

```
<% split('hello world', ' ')[0] %>
```
<a name="startswith"><h3>startswith(string, substr[, ignoreCase=true])</h3></a>
Check if a string starts with another.
```
<% startswith('heLlo', 'he') %>
```

<a name="substr"><h3>substr(string, start[, length])</h3></a>
Return a substring starting at 'start'. If 'start' is less than 0, it will be the offset from the end of the string.

```<% substr('  hello  ', 2, 5) %>```

<a name="substring"><h3>substring(string, start[, end])</h3></a>
Return a substring starting at 'start'. If 'start' or 'end' is less than 0, it will be the offset from the end of the string.

```<% substring('  hello   ', 2, 6) %>```


<a name="tabs"><h3>tabs(len)</h3></a>
Return len tabs
```
<% tabs(2) %>
```

<a name="trim"><h3>trim(string) / trimleft(string) / trimright(string)</h3></a>

Remove whitespace from a string.

```<% trim('  hello  ') %>```

<a name="ucfirst"><h3>ucfirst(string)</h3></a>
Uppercase the first letter of a string with the rest being lowercase.

```
<% ucfirst('hello') %>
```

<a name="uppercase"><h3>uppercase(string)</h3></a>
Uppercase a string.

```
<% uppercase('heLlo') %>
```

# type conversion functions

<a name="bool"><h3>bool(any)</h3></a>
bool() casts to a boolean.

```
<% bool(true) %>    // true
<% bool('true') %>  // false
<% bool(false) %>   // true
<% bool('false') %> // false
<% bool(1) %>       // false
<% bool(0) %>       // false
<% bool(10) %>      // false
<% bool(-10) %>     // false
<% bool(1.23) %>    // false
```

<a name="int"><h3>int(any)</h3></a>
int() casts a number to an integer.

```
<% int(123.45) %>
<% int('123.45') %>
```

<a name="num"><h3>num(any)</h3></a>
num() casts the variable to a number.

```
<% num(true) %>    // 1
<% num(10) %>      // 10
<% num('123') %>   // 123<br>
<% num('1e6') %>   //1000000
```

<a name="str"><h3>str(any)</h3></a>
str() casts a variable to a string. isstr() checks if a variable is a string.
```
<% str(123) + ' = one two three' %>
```

# type checking functions

<a name="isbool"><h3>isbool(any)</h3></a>
isbool() checks if a variable is a boolean.

```
<% isbool(true) %>    // true
<% isbool('true') %>  // false
<% isbool(false) %>   // true
<% isbool('false') %> // false
<% isbool(1) %>       // false
<% isbool(0) %>       // false
<% isbool(1.23) %>    // false
```

<a name="isint"><h3>isint(any)</h3></a>
isint() checks if a variable is an integer.

```
<% isint(123) %>      // true
<% isint(123.45) %>   // false
<% isint('123.45') %> // false
```


<a name="isnil"><h3>isnil(any) / isnull(any)</h3></a>
isnull()/isnil() checks if an object is null or not.

```
<% isnil(dataset) %>    // 
```

<a name="isnum"><h3>isnum(any)</h3></a>
isnum() checks if a variable is an number.

```
<% isnum(123) %>      // true
<% isnum(123.45) %>   // true
<% isnum('123.45') %> // false
```

<a name="isobject"><h3>isobject(any)</h3></a>
isobject() checks if a variable is an object.

```
<% isobject(obj) %>
```

<a name="isrecord"><h3>isrecord(any)</h3></a>
isrecord() checks if a variable is a record.

```
<% isrecord(obj) %>
```

<a name="isstr"><h3>isstr(any)</h3></a>
isstr() checks if a variable is an string.

```
<% isstr(123) %>      // false
<% isstr(bool) %>     // false
<% isstr('123.45') %> // true
```

<a name="typeof"><h3>typeof(obj)</h3></a>
Return the class name of an object.

```
<% typeof(true) %>     // System.Boolean
<% typeof(123) %>      // System.Extended
<% typeof(123.45) %>   // System.Extended
<% typeof('test') %>   // System.string
```


