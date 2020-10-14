# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2020 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Known restrictions / limitations / bugs

- basic arrays must be one dimensional

- numbers are limited to the regex format _[0-9]\([.][0-9]+)_.

- include/require could be functions, but are special and thus part of the grammar. 

- custom methods don't work with 'array of const'. See [Custom Functions](./custom-functions.md) for more information.

- users may need to cast types explitly when calling functions/methods or introduce custom methods to assist with casting.

- review __Sempare.Boot.Template.Velocity.Compiler.inc__ to see conditional compilation

	- SUPPORT_JSON - Allows support of System.Json
	- SUPPORT_JSON_BOOL - Workaround on TJsonBool not being available in early versions.
	- SUPPORT_NET_ENCODING - Disables html encoding if not available. (todo workaround)
	- SUPPORT_BUFFERED_STREAM - Falls back to TFileStream from TBufferedStream when reading from files.
