 (*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                        Sempare Templating Engine                                                 *
 *                                                                                                  *
 *                                                                                                  *
 *         https://github.com/sempare/sempare-delphi-template-engine                                *
 ****************************************************************************************************
 *                                                                                                  *
 * Copyright (c) 2020 Sempare Limited                                                               *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License                             *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                                     *
 * https://github.com/sempare/sempare-delphi-template-engine/blob/master/docs/commercial.license.md *
 *                                                                                                  *
 * Unless required by applicable law or agreed to in writing, software                              *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,                               *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                         *
 * See the License for the specific language governing permissions and                              *
 * limitations under the License.                                                                   *
 *                                                                                                  *
 *************************************************************************************************%*)
unit Sempare.Template.Functions;

interface

uses
  System.Rtti,
  Sempare.Template.Context;

function CreateTemplateFunctions(const ARegisterDefaults: boolean = true): ITemplateFunctions;
function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;

var
  GFunctions: ITemplateFunctions;

implementation

uses
  System.SysUtils,
  System.Math,
  System.RegularExpressions,
  System.Generics.Collections,
  System.Generics.Defaults,
  Sempare.Template.Common,
  Sempare.Template.Rtti;

type
  TValueCompare = class(TComparer<TValue>)
  public
    function Compare(const Left, Right: TValue): integer; override;
  end;

  TTemplateFunctions = class(TInterfacedObject, ITemplateFunctions)
  private
    FFunctions: TDictionary<string, TArray<TRttiMethod>>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetIsEmpty: boolean;

    procedure AddFunctions(const AClass: TClass);
    procedure RegisterDefaults;

    function TryGetValue(const AName: string; out AMethods: TArray<TRttiMethod>): boolean;
    function Add(const AMethod: TRttiMethod): boolean;
  end;

var
  GValueCompare: IComparer<TValue>;

function CreateTemplateFunctions(const ARegisterDefaults: boolean): ITemplateFunctions;
begin
  result := TTemplateFunctions.Create;
  if ARegisterDefaults then
    result.RegisterDefaults;
end;

function TTemplateFunctions.Add(const AMethod: TRttiMethod): boolean;
var
  methods: TArray<TRttiMethod>;
  name: string;
begin
  result := AMethod.IsStatic and AMethod.IsClassMethod and (AMethod.ReturnType.TypeKind <> tkProcedure);
  if not result then
    exit;

  name := AMethod.name.ToLower;
  FFunctions.TryGetValue(name, methods);
  insert(AMethod, methods, 0);
  FFunctions.AddOrSetValue(name, methods);
end;

procedure TTemplateFunctions.AddFunctions(const AClass: TClass);
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiType := GRttiContext.GetType(AClass);
  for RttiMethod in RttiType.GetMethods do
  begin
    Add(RttiMethod);
  end;
end;

type
  TInternalFuntions = class
  public
    class function Split(const AString: string): TArray<string>; overload; static;
    class function Split(const AString: string; const ASep: string): TArray<string>; overload; static;
    class function Lowercase(const AString: string): string; static;
    class function Uppercase(const AString: string): string; static;
    class function Trim(const AString: string): string; static;
    class function SubStr(const AString: string; AStartOffset: integer): string; overload; static;
    class function SubStr(const AString: string; AStartOffset, ALength: integer): string; overload; static;
    class function Substring(const AString: string; AStartOffset, AEndOffset: integer): string; overload; static;
    class function Substring(const AString: string; AStartOffset: integer): string; overload; static;
    class function Pos(const search, Str: string; offset: integer): integer; overload; static;
    class function Pos(const search, Str: string): integer; overload; static;
    class function Len(const AString: TValue): integer; static;
    class function Fmt(const AArgs: TArray<TValue>): string; static;
    class function FmtDt(const AFormat: string; const ADateTime: TDateTime): string; static;
    class function DtNow: TDateTime; static;
    class function BoolStr(const AValue: TValue): boolean; static;
    class function Bool(const AValue: TValue): boolean; static;
    class function Int(const AValue: TValue): integer; static;
    class function Str(const AValue: TValue): string; static;
    class function UCFirst(const AString: string): string; static;
    class function Rev(const AString: string): string; static;
    class function IsNull(const AValue: TValue): boolean; static;
    class function IsStr(const AValue: TValue): boolean; static;
    class function IsInt(const AValue: TValue): boolean; static;
    class function isBool(const AValue): boolean; static;
    class function IsNum(const AValue: TValue): boolean; static;
    class function StartsWith(const AString, ASearch: string): boolean; overload; static;
    class function StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;
    class function EndsWith(const AString, ASearch: string): boolean; overload; static;
    class function EndsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;
    class function TypeOf(const AValue: TValue): string; static;
    class function Replace(const AValue, AWith, AIn: string): string; static;
    class function Match(const AValue, ARegex: string): boolean; static;
    class function Sort(const AArray: TValue): TValue; static;
  end;

class function TInternalFuntions.Pos(const search, Str: string): integer;
begin
  result := System.Pos(search, Str);
end;

type
  Internal = class
    class function SortEnum<T>(const AEnum: TValue): TValue; static;
    class function SortArr<T>(const AArray: TValue): TValue; static;
    class function SortArrTValue(const AArray: TValue): TValue; static;
  end;

class function Internal.SortEnum<T>(const AEnum: TValue): TValue;
var
  e: TEnumerable<T>;
  a: TArray<T>;
  v: T;
begin
  e := TEnumerable<T>(AEnum.AsObject);
  for v in e do
    insert(v, a, length(a));
  result := SortArr<T>(TValue.From < TArray < T >> (a));
end;

class function Internal.SortArrTValue(const AArray: TValue): TValue;
var
  v: TArray<TValue>;
begin
  v := AArray.AsType<TArray<TValue>>;
  TArray.Sort<TValue>(v, GValueCompare);
  result := TValue.From < TArray < TValue >> (v);
end;

class function Internal.SortArr<T>(const AArray: TValue): TValue;
var
  v: TArray<T>;
begin
  v := AArray.AsType < TArray < T >> ();
  TArray.Sort<T>(v);
  result := TValue.From < TArray < T >> (v);
end;

class function TInternalFuntions.Sort(const AArray: TValue): TValue;

begin
  if AArray.IsObject then
  begin
    if AArray.AsObject.InheritsFrom(TEnumerable<string>) then
      result := Internal.SortEnum<string>(AArray)
    else if AArray.AsObject.InheritsFrom(TEnumerable<integer>) then
      result := Internal.SortEnum<integer>(AArray)
    else if AArray.AsObject.InheritsFrom(TEnumerable<double>) then
      result := Internal.SortEnum<double>(AArray)
    else if AArray.AsObject.InheritsFrom(TEnumerable<extended>) then
      result := Internal.SortEnum<extended>(AArray)
    else
    begin
      result := AArray;
      writeln(GRttiContext.GetType(AArray.TypeInfo).QualifiedName);
    end;
  end
  else if AArray.TypeInfo = TypeInfo(TArray<string>) then
    result := Internal.SortArr<string>(AArray)
  else if AArray.TypeInfo = TypeInfo(TArray<integer>) then
    result := Internal.SortArr<integer>(AArray)
  else if AArray.TypeInfo = TypeInfo(TArray<double>) then
    result := Internal.SortArr<double>(AArray)
  else if AArray.TypeInfo = TypeInfo(TArray<extended>) then
    result := Internal.SortArr<extended>(AArray)
  else if AArray.TypeInfo = TypeInfo(TArray<TValue>) then
    result := Internal.SortArrTValue(AArray)
  else
  begin
    result := AArray;
    writeln(GRttiContext.GetType(AArray.TypeInfo).QualifiedName);
  end;

end;

class function TInternalFuntions.Split(const AString: string; const ASep: string): TArray<string>;
begin
  result := AString.Split([ASep]);
end;

class function TInternalFuntions.Lowercase(const AString: string): string;
begin
  result := AString.ToLower;
end;

class function TInternalFuntions.Match(const AValue, ARegex: string): boolean;
begin
  result := TRegex.IsMatch(AValue, ARegex);
end;

class function TInternalFuntions.Uppercase(const AString: string): string;
begin
  result := AString.ToUpper;
end;

class function TInternalFuntions.Trim(const AString: string): string;
begin
  result := AString.Trim;
end;

class function TInternalFuntions.TypeOf(const AValue: TValue): string;
begin
  result := GRttiContext.GetType(AValue.AsType<TValue>.TypeInfo).QualifiedName;
end;

class function TInternalFuntions.SubStr(const AString: string; AStartOffset: integer; ALength: integer): string;
begin
  if AStartOffset < 0 then
    AStartOffset := length(AString) + AStartOffset + 1;
  if ALength < 0 then
    ALength := 0;
  result := copy(AString, AStartOffset, ALength);
end;

class function TInternalFuntions.Substring(const AString: string; AStartOffset: integer): string;
begin
  result := Substring(AString, AStartOffset, length(AString));
end;

class function TInternalFuntions.SubStr(const AString: string; AStartOffset: integer): string;
begin
  result := SubStr(AString, AStartOffset, length(AString));
end;

class function TInternalFuntions.Substring(const AString: string; AStartOffset: integer; AEndOffset: integer): string;
begin
  if AStartOffset < 0 then
    AStartOffset := length(AString) + AStartOffset + 1;
  if AEndOffset < 0 then
    AEndOffset := length(AString) + AEndOffset + 1;
  result := copy(AString, AStartOffset, AEndOffset - AStartOffset + 1);
end;

class function TInternalFuntions.Pos(const search: string; const Str: string; offset: integer): integer;
begin
  if offset < 0 then
    offset := offset + length(Str);
  result := System.Pos(search, Str, offset);
end;

class function TInternalFuntions.Len(const AString: TValue): integer;
begin
  if AString.IsType<string> then
    result := length(AString.AsType<string>())
  else if AString.Kind in [tkDynArray, tkArray] then
    result := AString.GetArrayLength
  else
    result := -1;
end;

function UnWrap(const AArg: TValue): TValue;
begin
  if AArg.TypeInfo = TypeInfo(TValue) then
    result := UnWrap(AArg.AsType<TValue>())
  else
    result := AArg;
end;

function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;
var
  i: integer;
begin
  setlength(result, length(AArgs));
  for i := low(AArgs) to high(AArgs) do
    result[i] := UnWrap(AArgs[i]).AsVarRec;
end;

class function TInternalFuntions.EndsWith(const AString, ASearch: string): boolean;
begin
  result := AString.EndsWith(ASearch, true);
end;

class function TInternalFuntions.EndsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean;
begin
  result := AString.EndsWith(ASearch, AIgnoreCase);
end;

class function TInternalFuntions.Fmt(const AArgs: TArray<TValue>): string;
begin
  result := format(asstring(AArgs[0]), ToArrayTVarRec(copy(AArgs, 1, length(AArgs) - 1)));
end;

class function TInternalFuntions.FmtDt(const AFormat: string; const ADateTime: TDateTime): string;
begin
  result := FormatDateTime(AFormat, ADateTime);
end;

class function TInternalFuntions.Bool(const AValue: TValue): boolean;
begin
  result := AsBoolean(AValue);
end;

class function TInternalFuntions.BoolStr(const AValue: TValue): boolean;
begin
  if isStrLike(AValue) then
    result := asstring(AValue) = 'true'
  else
    result := AsBoolean(AValue);
end;

class function TInternalFuntions.DtNow(): TDateTime;
begin
  result := System.SysUtils.Now;
end;

class function TInternalFuntions.Int(const AValue: TValue): integer;
begin
  result := asInt(AValue);
end;

class function TInternalFuntions.StartsWith(const AString, ASearch: string): boolean;
begin
  result := AString.StartsWith(ASearch, true);
end;

class function TInternalFuntions.Split(const AString: string): TArray<string>;
begin
  result := Split(AString, ' ');
end;

class function TInternalFuntions.StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean;
begin
  result := AString.StartsWith(ASearch, AIgnoreCase);
end;

class function TInternalFuntions.Str(const AValue: TValue): string;
begin
  result := asstring(AValue);
end;

function reverse(const AStr: string): string;
var
  i, j, m: integer;
begin
  setlength(result, length(AStr));
  m := length(AStr) div 2 + 1;
  i := 1;
  j := length(AStr);
  while i <= m do
  begin
    result[i] := AStr[j];
    result[j] := AStr[i];
    inc(i);
    dec(j);
  end;
end;

class function TInternalFuntions.UCFirst(const AString: string): string;
begin
  result := Lowercase(AString);
  result[1] := Uppercase(result[1])[1];
end;

class function TInternalFuntions.Replace(const AValue, AWith, AIn: string): string;
begin
  result := AIn.Replace(AValue, AWith, [rfReplaceAll]);
end;

class function TInternalFuntions.Rev(const AString: string): string;
begin
  result := reverse(AString);
end;

class function TInternalFuntions.IsNull(const AValue: TValue): boolean;
begin
  result := IsNull(AValue);
end;

class function TInternalFuntions.IsStr(const AValue: TValue): boolean;
begin
  result := isStrLike(AValue);
end;

class function TInternalFuntions.IsInt(const AValue: TValue): boolean;
begin
  result := isIntLike(AValue);
end;

class function TInternalFuntions.isBool(const AValue): boolean;
begin
  result := isBool(AValue);
end;

class function TInternalFuntions.IsNum(const AValue: TValue): boolean;
begin
  result := isnumlike(AValue);
end;

constructor TTemplateFunctions.Create;
begin
  FFunctions := TDictionary < string, TArray < TRttiMethod >>.Create;
end;

destructor TTemplateFunctions.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

function TTemplateFunctions.GetIsEmpty: boolean;
begin
  result := FFunctions.Count > 0;
end;

procedure TTemplateFunctions.RegisterDefaults;
begin
  AddFunctions(TInternalFuntions);
end;

function TTemplateFunctions.TryGetValue(const AName: string; out AMethods: TArray<TRttiMethod>): boolean;
begin
  result := FFunctions.TryGetValue(AName, AMethods);
end;

{ TValueCompare }

function TValueCompare.Compare(const Left, Right: TValue): integer;
  procedure compareInt;
  var
    l, r: int64;
  begin
    l := Left.AsInt64;
    r := Right.AsInt64;
    if l < r then
      result := -1
    else if l = r then
      result := 0
    else
      result := 1;
  end;
  procedure comparefloat;
  var
    l, r: extended;
  begin
    l := Left.AsExtended;
    r := Right.AsExtended;
    if l < r then
      result := -1
    else if Abs(l - r) < 1E-8 then
      result := 0
    else
      result := 1;
  end;
  procedure compareString;
  var
    l, r: string;
  begin
    l := Left.asstring;
    r := Right.asstring;
    if l < r then
      result := -1
    else if l = r then
      result := 0
    else
      result := 1;
  end;

begin
  if Left.TypeInfo <> Right.TypeInfo then
    raise Exception.Create('Types are not of the same type');
  if Left.TypeInfo = TypeInfo(integer) then
  begin

  end;
  case Left.Kind of
    tkInteger, tkInt64:
      compareInt;
    tkFloat:
      comparefloat;
    tkString, tkLString, tkWString, tkUString:
      compareString;
  else
    raise Exception.Create('Type not supported');
  end;
end;

initialization

GFunctions := CreateTemplateFunctions();
GValueCompare := TValueCompare.Create;

finalization

GFunctions := nil;
GValueCompare := nil;

end.
