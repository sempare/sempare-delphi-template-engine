# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Known restrictions / limitations / bugs

- numbers are limited to the regex format _[0-9]\([.][0-9]+)_.

- include/require could be functions, but are special and thus part of the grammar. 

- custom methods don't work with 'array of const'. See [Custom Functions](./custom-functions.md) for more information.
