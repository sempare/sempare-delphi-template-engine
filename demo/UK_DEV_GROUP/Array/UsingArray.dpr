program UsingArray;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity;

type

  TMyRec = record
  public
    FirstName: string;
    LastName: string;
    constructor Create(const FN, LN: string);
    function Num(i : integer): integer;
  end;

  TMyClass = class
    function GetName: string;
  public
    FName: string;
    constructor Create(Const N: string);
    property Name: string read GetName;
  end;

procedure demo1;
var
  a: array [0 .. 3] of TMyClass;
  v: TMyClass;
begin
  a[0] := TMyClass.Create('d');
  a[1] := TMyClass.Create('e');
  a[2] := TMyClass.Create('f');
  a[3] := TMyClass.Create('g');

  writeln(Velocity.Eval('<% for i in _%><%_[i].name%><%end%>  ', a));

  for v in a do
    v.Free;
end;

procedure demo2;

var
  a: array [2 .. 4] of TMyRec;
begin
  a[2] := TMyRec.Create('a', 'A');
  a[3] := TMyRec.Create('b', 'B');
  a[4] := TMyRec.Create('c', 'C');
  writeln(Velocity.Eval('<% for i in _%>' + //
    '<%_[i].firstname%> <% _[i].lastname%>' + //
    '<% _[i].num(i) %>' +
    '<%end%>  ', a));
end;

procedure demo2dynamic;
var
  a: TArray<TMyRec>;
begin
  a := [TMyRec.Create('a', 'A'), TMyRec.Create('b', 'B'), TMyRec.Create('c', 'C')];
  writeln(Velocity.Eval('<% for i in _%><%_[i].firstname%> <% _[i].lastname%><%end%>  ', a));
end;

procedure demo3;
var
  a: TList<TMyRec>;
begin
  a := TList<TMyRec>.Create();
  a.AddRange([ //
    TMyRec.Create('x', 'X'), //
    TMyRec.Create('y', 'Y'), //
    TMyRec.Create('z', 'Z') //
    ]);
  writeln(Velocity.Eval('<% for i in _%><%i.firstname%> <% i.lastname%><%end%>', a));

  a.Free;
end;

procedure demo4;
var
  a: TList<TMyClass>;
begin
  a := TObjectList<TMyClass>.Create;
  a.AddRange([ //
    TMyClass.Create('Q'), //
    TMyClass.Create('W'), //
    TMyClass.Create('E') //
    ]);

  writeln(Velocity.Eval('<% for i in _%><%i.name%><%end%>', a));
  a.Free;
end;

{ TMyRec }

constructor TMyRec.Create(const FN, LN: string);
begin
  FirstName := FN;
  LastName := LN;
end;

function TMyRec.Num(i : integer): integer;
begin
  result := 20 + i;
end;

{ TMyClass }

constructor TMyClass.Create(const N: string);
begin
  FName := N;
end;

function TMyClass.GetName: string;
begin
  result := FName;
end;

begin
  demo1;
  demo2;
  demo2dynamic;
  demo3;
  demo4;
  readln;

end.
