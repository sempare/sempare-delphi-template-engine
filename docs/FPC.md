**FPC support**

An initial investigation was done. As Rtti was required, I attempted using FPC trunk with FPC 3.3.1.

Issues: 

- System.Generics.Collections missing.
- TDictionary needs substitute.
- Enumeration for in loop might not work (?)
- System.Rtti not available
  - maybe in 3.2 when Attributes also become available)
  - it is not complete (e.g. TRttiDynamicArrayType, TRttiField, TRttiArrayType).
- System.JSON missing.TJsonValue not available