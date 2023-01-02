# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Known restrictions / limitations / bugs

- basic arrays must be one dimensional

- numbers are limited to the regex format _[0-9]+\([.][0-9]+)?([eE][-+]?[0-9]+)?_.

- include/require could be functions, but are special and thus part of the grammar. 

- custom methods don't work with 'array of const'. See [Custom Functions](./custom-functions.md) for more information.

- users may need to cast types explitly when calling functions/methods or introduce custom methods to assist with casting.

- max run time is currently based on checking the time within loops. this does not however stop a function from blocking indefinitely.

- review __Sempare.Template.Compiler.inc__ to see conditional compilation

	- SUPPORT_JSON - Allows support of System.Json
	- SUPPORT_JSON_BOOL - Workaround on TJsonBool not being available in early versions.
	- SUPPORT_NET_ENCODING - Disables html encoding if not available. (todo workaround)
	- SUPPORT_BUFFERED_STREAM - Falls back to TFileStream from TBufferedStream when reading from files.
	- SEMPARE_TEMPLATE_CONFIRM_LICENSE - confirms that the developer has reviewed the license conditions.

- In XE4, it seems that the metadata on one dimensional static arrays may be missing from RTTI, so when doing range enumeration, the template library starts from offset 0.
