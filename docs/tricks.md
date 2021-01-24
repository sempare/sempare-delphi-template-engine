# ![](../images/sempare-logo-45px.png) Sempare Template Engine

Copyright (c) 2019-2021 [Sempare Limited](http://www.sempare.ltd)

## Tricks

### Reference variables dynamically

You can dereference variables dyanmically using the array dereferencing syntax:
```
type
    TRecord = record
        v1 : string;
        v2 : string;
        v3 : string;
        v4 : string;
    end;
begin
    var rec : TRecord;
    with rec do begin
        v1 := 'a';
        v2 := 'b';
        v3 := 'c';
        v4 := 'd';
    end;
    writeln(Template.Eval('<%for i := 1 to 4 %><% _["v" + i] %><% end %>', rec));
end;
```
The above example will produce:
```
abcd
```
### Improving performance on larger templates

A quick win would be to use buffered stream so that characters in the buffer are processed quickly.

### Loading templates dynamically

If you reference templates using the 'include' statement, you can rely on a TemplateResolver to load them if they are not
defined within the template itself.

e.g. This is illustrative:

```
  ctx := Template.Context;
  ctx.TemplateResolver := function(AContext: ITemplateContext; const ATemplate: string): ITemplate
    var
    	LStream : TFileStream;
    begin
      LStream := TFileStream.Create(ATemplate + '.tpl', fmOpenRead);
      exit(Template.Parse(AContext, LStream));
    end;
```
