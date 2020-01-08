{$IF CompilerVersion >= 26} // from XE5 onwards
{$DEFINE SUPPORT_JSON true}
{$ENDIF}

{$IF CompilerVersion >= 30} // from Delphi 10.0 Seatle onwards
{$DEFINE SUPPORT_JSON_BOOL true}
{$ENDIF}
