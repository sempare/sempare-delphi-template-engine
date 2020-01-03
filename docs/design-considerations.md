# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Design considerations

### no repeat/until
- repeat/until is not included as a 'while' loop can accomplish the same.

### no bytecode

The evaluation engine is a very simple. Parsing is fairly quick, so byte code is considered overkill.
