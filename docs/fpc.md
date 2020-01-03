# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Free Pascal support

An initial investigation was done. As RTTI was required, I attempted using FPC trunk with FPC 3.3.1 (This is not generally available yet)

Potential issues:

- System.Generics.Collections is lacking.
- TDictionary substitute must be used.
- Enumeration for in loop might not work (?)
- System.Rtti not fully compatible with Delphi (aka rtti in fpc)
  - maybe in 3.2 when Attributes also become available)
  - it is not complete (e.g. TRttiDynamicArrayType, TRttiField, TRttiArrayType)
- System.JSON (aka fpjson in fpc) incompatability
  - TJsonValue not available
  - TJsonData does exist