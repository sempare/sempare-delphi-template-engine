# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Custom functions

1. [Overview](#Overview)
2. [Overloading methods](#Overloading__methods)
3. [Default Values](#Default__Values)
4. [Variable arguments](#Variable__arguments)

### Overview

A number of basic functions are builtin. You may find it necessary to add custom functions.

Defining functions is very easy. Simply add a static class method on a utility class.
```

type
	TMyUtilities = class
	public
		class function DoSomething(const A : integer; const B: string; const AObject: TObject) : string; static;
	end;

```

A requirement is that the methods must be _class_ and _static_. 

Best practice is that functions should be defined. However, we allow you to define procedures in a similar way. e.g.
```
	TMyUtilities = class
	public
		class var SomeOption : boolean;
		class procedure SetSomeOption(const AOption : boolean); static;
	end;
```

### Overloading methods

You can overloaded methods. Method invocation is done using Runtime Type Information (RTTI) and the appropriate method is selected by matching the number of arguments provided with those defined on the function call.

No attempt is made to resolve the correct method at runtime based on variable type. For this reason, it is advised not to 
overload methods with the same number of arguments.

e.g.

```
class function StartsWith(const AString, ASearch: string): boolean; overload; static;
class function StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;    
```    

### Default Values

It is more complex to deal with default values. The easiest approach is to overload a method, where one version sets the default argument.

### Variable arguments

I have not been able to get this to work via RTTI where an combination of argument and array of const are present. I suspect it may be a limitation of the RTTI invocation mechanism. e.g. 

```Format(const AFormat : string; const AArgs: array of const) : string;```


To accomodate this, rather define a function such as:
``` 
class function Fmt(const AArgs: TArray<TValue>): string; static;
```
You need to manually extract the format and args required for the format function as done by the built-in function.

### Accessing context configuration

The function invocation mechansim can detect ITemplateContext parameter as the first parameter:
``` 
class function Fmt(const AContext: ITemplateContext; const AArgs: TArray<TValue>): string; static;
```