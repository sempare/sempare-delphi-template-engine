# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Design considerations

### no repeat/until
- repeat/until is not included as a 'while' loop can accomplish the same.

### no bytecode

The evaluation engine is a very simple. This may be considered in future.

### custom statment start/end overrides

No validataion is done currently to ensure that <| and |> matchup. They key is that statement start and end tokens matchup,
as the | indicator is used to to toggle the flag allowing for content to be output or not.

Similar to <% and %>, <| and |> can be changed to some other tokens. The lexical analyser is fairly simpilistic regarding the handling
of this override scenario, so it is up to you to ensure that overriding does not conflict with any other symbols.

### No Delphi (TComponent) Components

Some Delphi TComponent based descendents were catered for, but code is currently disabled. The main reason for this is that we didn't
want the template engine to have to be bound to a visual framework like VCL. However, we may review this if requested. 