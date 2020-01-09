# ![](../images/sempare-logo-45px.png) Sempare Boot Velocity Template Engine

Copyright (c) 2019 [Sempare Limited](http://www.sempare.ltd), [Conrad Vermeulen](mailto:conrad.vermeulen@gmail.com)

## Todo

- validation
  - identify required variables ahead of time where possible.
- create bindings to data sources like TDataSource, TFDMemTable, TFDQuery, etc...
- review performance. 
     
   I have not done much on this. Probably a few things that can be done better.
   
   I spotted something I didn't like in TStreamReader however. It uses a TStringBuffer and when the lexer reads a character, 
   it results in a Delete(0,1) on the TStringBuffer - resulting in a block move. This stdlib internal should ideally 
   maintain a read offset into the buffer, and refill the buffer when the offset reaches the end. Will raise something on Embarcadero Quality Portal.

   
- interactive script debugger (but VelocityDemo can be used to observe output)
- review free pascal and support for older versions of Delphi
- review strip recurring spacing post processing
