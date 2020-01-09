{$IF CompilerVersion >= 26} // from XE5 onwards
{$DEFINE SUPPORT_JSON true}
{$ENDIF}

{$IF CompilerVersion >= 28}  // from XE7 onwards
{$DEFINE SUPPORT_NET_ENCODING true}
{$ENDIF}

{$IF CompilerVersion >= 30} // from Delphi 10.0 Seatle onwards
{$DEFINE SUPPORT_JSON_BOOL true}
{$ENDIF}

{$IF CompilerVersion >= 31}  // from Delphi 10.1 Berlin onwards
{$DEFINE SUPPORT_BUFFERED_STREAM true}
{$ENDIF}


