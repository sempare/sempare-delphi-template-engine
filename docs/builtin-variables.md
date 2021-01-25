# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Builtin variables

### Special Characters

The parser does not support parsing strings the same way Delphi does. So there is no way to encode special characters such as #10 for a newline.

The following variables have been defined:

- TAB

This is the tab character (#9).

- CR

This is the carriage return character (#13).

- NL

This is the new line return character (#10).

- CRNL

This is a composition of both carriage return and newline (#13#10).

If you require others, you can populate the context yourself using the chr() function.