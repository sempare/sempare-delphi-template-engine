program UsingDictionary;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Template;

procedure Demo1;

var
  r: record d: TDictionary<string, string>;

  v0: string;
  v1: string;
  v2: string;
end;
i:
integer;

begin
  r.d := TDictionary<string, string>.create;
  r.v0 := 'value0';
  r.v1 := 'value1';
  r.v2 := 'value2';
  try
    r.d.add('firstname', 'conrad');
    writeln(Template.Eval('<% d[''firstname''] %>', r));
    writeln(Template.Eval('<% for i := 0 to 2%> <% _[''v'' + i ] %> <% end %>', r));
  finally
    r.d.Free;
  end;
end;

procedure Demo2;

var
  ctx: ITemplateContext;
  r: record firstname: string;
  d: TDictionary<string, TArray<string>>;
end;

begin
  ctx := Template.Context();
  ctx['firstname'] := 'conrad';
  r.d := TDictionary < string, TArray < string >>.create;
  try
    r.d.add('list1', ['a', 'b', 'c', 'd']);
    r.d.add('list2', ['d', 'e', 'f', 'g', 'h']);
    writeln(Template.Eval(ctx, 'hello <% firstname %>'));
    writeln(Template.Eval(ctx, '<%for i in d[''list1''] %> <% d[''list1''][i] %> <% end %>  ', r));
    writeln(Template.Eval(ctx, '<%for i in d[''list2''] %> <% d[''list2''][i] %> <% end %>  ', r));
  finally
    r.d.Free;
  end;
end;

begin
  try
    Demo1;
    Demo2;

  except
    on e: exception do
    begin
      writeln(e.Message);
    end;
  end;
  readln;

end.
