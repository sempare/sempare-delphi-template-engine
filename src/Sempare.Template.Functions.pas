(*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                          Sempare Template Engine                                                 *
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
  Sempare.Template.AST,
  Sempare.Template.Context;

type
  ETemplateFunction = class(ETemplate);

function CreateTemplateFunctions(const ARegisterDefaults: boolean = true): ITemplateFunctions;
function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;

var
  GFunctions: ITemplateFunctions;

implementation

uses
  System.TypInfo, // needed for XE6 and below to access the TTypeKind variables
  System.SysUtils,
  System.Math,
  System.RegularExpressions,
  System.Generics.Collections,
  System.Generics.Defaults,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Common,
  Sempare.Template.Rtti;

{$I 'Sempare.Template.Compiler.inc'}

type
  TValueCompare = class(TComparer<TValue>)
  public
    function Compare(const ALeft, ARight: TValue): integer; override;
  end;

  TTemplateFunctions = class(TInterfacedObject, ITemplateFunctions)
  private
    FFunctions: TDictionary<string, TArray<TRttiMethod>>;
  public
    constructor Create;
    destructor Destroy; override;

    function GetIsEmpty: boolean;

    procedure AddFunctions(const AClass: TClass);
    procedure Remove(const AName: string);
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
  LMethods: TArray<TRttiMethod>;
  LMethodName: string;
  LLength: integer;
begin
  result := AMethod.IsStatic and AMethod.IsClassMethod and (AMethod.ReturnType.TypeKind <> tkProcedure);
  if not result then
    exit;
  LMethodName := AMethod.Name.ToLower;
  FFunctions.TryGetValue(LMethodName, LMethods);
  LLength := length(LMethods);
  if LLength = 0 then
  begin
    setlength(LMethods, 1);
  end
  else
  begin
    setlength(LMethods, LLength + 1);
    move(LMethods[0], LMethods[1], sizeof(AMethod) * LLength);
  end;
  LMethods[0] := AMethod;
  FFunctions.AddOrSetValue(LMethodName, LMethods);
end;

procedure TTemplateFunctions.AddFunctions(const AClass: TClass);
var
  LClassType: TRttiType;
  LMethod: TRttiMethod;
begin
  LClassType := GRttiContext.GetType(AClass);
  for LMethod in LClassType.GetMethods do
    Add(LMethod);
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
    class function Num(const AValue: TValue): extended; static;
    class function Int(const AValue: TValue): integer; static;
    class function Str(const AValue: TValue): string; static;
    class function UCFirst(const AString: string): string; static;
    class function Rev(const AString: string): string; static;
    class function IsNull(const AValue: TValue): boolean; static;
    class function IsStr(const AValue: TValue): boolean; static;
    class function IsInt(const AValue: TValue): boolean; static;
    class function IsBool(const AValue): boolean; static;
    class function IsNum(const AValue: TValue): boolean; static;
    class function StartsWith(const AString, ASearch: string): boolean; overload; static;
    class function StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;
    class function EndsWith(const AString, ASearch: string): boolean; overload; static;
    class function EndsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;
    class function TypeOf(const AValue: TValue): string; static;
    class function Replace(const AValue, AWith, AIn: string): string; static;
    class function Match(const AValue, ARegex: string): boolean; static;
    class function Sort(const AArray: TValue): TValue; static;
    class function Chr(const AValue: int64): string; static;
    class function Ord(const AValue: string): int64; static;
    class function tabs(const ANum: integer): string; static;
    class function spaces(const ANum: integer): string; static;
    class function crnl(const ANum: integer): string; static;
    class function nl(const ANum: integer): string; static;
    class function PadLeft(const AStr: string; const ANum: integer; const APadChar: char): string; overload; static;
    class function PadLeft(const AStr: string; const ANum: integer): string; overload; static;
    class function PadRight(const AStr: string; const ANum: integer): string; overload; static;
    class function PadRight(const AStr: string; const ANum: integer; const APadChar: char): string; overload; static;
  end;

class function TInternalFuntions.PadLeft(const AStr: string; const ANum: integer): string;
begin
  exit(AStr.PadLeft(ANum));
end;

class function TInternalFuntions.PadRight(const AStr: string; const ANum: integer; const APadChar: char): string;
begin
  exit(AStr.PadRight(ANum, APadChar));
end;

class function TInternalFuntions.PadRight(const AStr: string; const ANum: integer): string;
begin
  exit(AStr.PadRight(ANum));
end;

class function TInternalFuntions.Pos(const search, Str: string): integer;
begin
  exit(System.Pos(search, Str));
end;

type
  Internal = class
    class function SortEnum<T>(const AEnum: TValue): TValue; static;
    class function SortArr<T>(const AArray: TValue): TValue; static;
    class function SortArrTValue(const AArray: TValue): TValue; static;
  end;

class function Internal.SortEnum<T>(const AEnum: TValue): TValue;
var
  LEnum: TEnumerable<T>;
  LArray: TArray<T>;
  LElement: T;
  LOffset: integer;
begin
  LEnum := TEnumerable<T>(AEnum.AsObject);
  setlength(LArray, 0);
  for LElement in LEnum do
  begin
    LOffset := length(LArray);
    setlength(LArray, LOffset + 1);
    LArray[LOffset] := LElement;
  end;
  exit(SortArr<T>(TValue.From < TArray < T >> (LArray)));
end;

class function Internal.SortArrTValue(const AArray: TValue): TValue;
var
  LArray: TArray<TValue>;
begin
  LArray := AArray.AsType<TArray<TValue>>;
  TArray.Sort<TValue>(LArray, GValueCompare);
  exit(TValue.From < TArray < TValue >> (LArray));
end;

class function Internal.SortArr<T>(const AArray: TValue): TValue;
var
  LArray: TArray<T>;
begin
  LArray := AArray.AsType < TArray < T >> ();
  TArray.Sort<T>(LArray);
  exit(TValue.From < TArray < T >> (LArray));
end;

class function TInternalFuntions.Sort(const AArray: TValue): TValue;

begin
  if AArray.IsObject then
  begin
    if AArray.AsObject.InheritsFrom(TEnumerable<string>) then
      exit(Internal.SortEnum<string>(AArray))
    else if AArray.AsObject.InheritsFrom(TEnumerable<integer>) then
      exit(Internal.SortEnum<integer>(AArray))
    else if AArray.AsObject.InheritsFrom(TEnumerable<double>) then
      exit(Internal.SortEnum<double>(AArray))
    else if AArray.AsObject.InheritsFrom(TEnumerable<extended>) then
      exit(Internal.SortEnum<extended>(AArray))
    else
      exit(AArray)
  end;
  if AArray.TypeInfo = TypeInfo(TArray<string>) then
    exit(Internal.SortArr<string>(AArray))
  else if AArray.TypeInfo = TypeInfo(TArray<integer>) then
    exit(Internal.SortArr<integer>(AArray))
  else if AArray.TypeInfo = TypeInfo(TArray<double>) then
    exit(Internal.SortArr<double>(AArray))
  else if AArray.TypeInfo = TypeInfo(TArray<extended>) then
    exit(Internal.SortArr<extended>(AArray))
  else if AArray.TypeInfo = TypeInfo(TArray<TValue>) then
    exit(Internal.SortArrTValue(AArray))
  else
    exit(AArray)
end;

class function TInternalFuntions.spaces(const ANum: integer): string;
begin
  exit(''.PadLeft(ANum, ' '));
end;

class function TInternalFuntions.Split(const AString: string; const ASep: string): TArray<string>;
begin
  exit(AString.Split([ASep], MaxInt, TStringSplitOptions.None));
end;

class function TInternalFuntions.Lowercase(const AString: string): string;
begin
  exit(AString.ToLower());
end;

class function TInternalFuntions.Match(const AValue, ARegex: string): boolean;
begin
  exit(TRegex.IsMatch(AValue, ARegex));
end;

class function TInternalFuntions.nl(const ANum: integer): string;
begin
  exit(''.PadLeft(ANum, #10));
end;

class function TInternalFuntions.Num(const AValue: TValue): extended;
begin
  exit(AsNum(AValue));
end;

class function TInternalFuntions.Ord(const AValue: string): int64;
begin
  if length(AValue) = 1 then
    exit(System.Ord(AValue[1]));
  exit(0);
end;

class function TInternalFuntions.Uppercase(const AString: string): string;
begin
  exit(AString.ToUpper());
end;

class function TInternalFuntions.Trim(const AString: string): string;
begin
  exit(AString.Trim());
end;

class function TInternalFuntions.TypeOf(const AValue: TValue): string;
var
  lmsg: string;
  lpos: integer;
begin
  try
    exit(GRttiContext.GetType(AValue.AsType<TValue>.TypeInfo).QualifiedName);
  except
    on e: exception do
    begin
      lmsg := e.Message;
      // a big hacky, but a workaround
      lpos := lmsg.IndexOf(''' is not declared');
      if lmsg.StartsWith('Type ''') and (lpos > 0) then
        exit(copy(lmsg, 7, lpos - 6))
      else
        raise e;
    end;
  end;
end;

class function TInternalFuntions.SubStr(const AString: string; AStartOffset: integer; ALength: integer): string;
begin
  if AStartOffset < 0 then
    AStartOffset := length(AString) + AStartOffset + 1;
  if ALength < 0 then
    ALength := 0;
  exit(copy(AString, AStartOffset, ALength));
end;

class function TInternalFuntions.Substring(const AString: string; AStartOffset: integer): string;
begin
  exit(Substring(AString, AStartOffset, length(AString)));
end;

class function TInternalFuntions.tabs(const ANum: integer): string;
begin
  exit(''.PadLeft(ANum, #9));
end;

class function TInternalFuntions.SubStr(const AString: string; AStartOffset: integer): string;
begin
  exit(SubStr(AString, AStartOffset, length(AString)));
end;

class function TInternalFuntions.Substring(const AString: string; AStartOffset: integer; AEndOffset: integer): string;
begin
  if AStartOffset < 0 then
    AStartOffset := length(AString) + AStartOffset + 1;
  if AEndOffset < 0 then
    AEndOffset := length(AString) + AEndOffset + 1;
  exit(copy(AString, AStartOffset, AEndOffset - AStartOffset + 1));
end;

class function TInternalFuntions.Pos(const search: string; const Str: string; offset: integer): integer;
begin
  if offset < 0 then
    offset := offset + length(Str);
  exit(System.Pos(search, Str, offset));
end;

class function TInternalFuntions.Len(const AString: TValue): integer;
begin
  if AString.IsType<string> then
    exit(length(AString.AsType<string>()))
  else if AString.Kind in [tkDynArray, tkArray] then
    exit(AString.GetArrayLength)
  else
    exit(-1);
end;

function UnWrap(const AArg: TValue): TValue;
begin
  if AArg.TypeInfo = TypeInfo(TValue) then
    exit(UnWrap(AArg.AsType<TValue>()))
  else
    exit(AArg);
end;

{$IFNDEF SUPPORT_AS_VARREC}

type
  TValueHelper = record helper for TValue
    function AsLimitedVarRec: TVarrec;
  end;

function TValueHelper.AsLimitedVarRec: TVarrec;
begin
  FillChar(result, sizeof(result), 0);
  with result do
    case Kind of
      tkInteger:
        begin
          Vtype := vtInteger;
          VInteger := AsInteger;
        end;
      tkFloat:
        begin
          Vtype := vtExtended;
          VExtended := GetReferenceToRawData;
        end;
      tkString, tkWString, tkLString, tkUString:
        begin
          Vtype := vtUnicodeString;
          VUnicodeString := Pointer(AsString);
        end;
      tkInt64:
        begin
          Vtype := vtInteger;
          VInt64 := GetReferenceToRawData;
        end;
      tkEnumeration:
        begin
          if IsType<boolean> then
          begin
            Vtype := vtBoolean;
            VBoolean := AsBoolean;
          end
          else
          begin
            Vtype := vtInteger;
            VInteger := AsInteger;
          end;
        end;
    else
      raise ETemplateFunction.Create(STypeNotSupported);
    end;
end;
{$ENDIF}

function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;
var
  LIdx: integer;
begin
  setlength(result, length(AArgs));
  for LIdx := 0 to high(AArgs) do
{$IFNDEF SUPPORT_AS_VARREC}
    result[LIdx] := AArgs[LIdx].AsLimitedVarRec;
{$ELSE}
    result[LIdx] := AArgs[LIdx].AsVarRec;
{$ENDIF}
end;

class function TInternalFuntions.EndsWith(const AString, ASearch: string): boolean;
begin
  exit(AString.EndsWith(ASearch, true));
end;

class function TInternalFuntions.EndsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean;
begin
  exit(AString.EndsWith(ASearch, AIgnoreCase));
end;

class function TInternalFuntions.Fmt(const AArgs: TArray<TValue>): string;
begin
  exit(format(AsString(AArgs[0]), ToArrayTVarRec(copy(AArgs, 1, length(AArgs) - 1))));
end;

class function TInternalFuntions.FmtDt(const AFormat: string; const ADateTime: TDateTime): string;
begin
  exit(FormatDateTime(AFormat, ADateTime));
end;

class function TInternalFuntions.Bool(const AValue: TValue): boolean;
begin
  exit(AsBoolean(AValue));
end;

class function TInternalFuntions.BoolStr(const AValue: TValue): boolean;
begin
  if isStrLike(AValue) then
    exit(AsString(AValue) = 'true')
  else
    exit(AsBoolean(AValue));
end;

class function TInternalFuntions.Chr(const AValue: int64): string;
begin
  exit(System.Chr(AValue));
end;

class function TInternalFuntions.crnl(const ANum: integer): string;
var
  Lsb: tstringbuilder;
  LIdx: integer;
begin
  Lsb := tstringbuilder.Create;
  try
    for LIdx := 1 to ANum do
      Lsb.Append(#13#10);
    exit(Lsb.ToString());
  finally
    Lsb.Free;
  end;
end;

class function TInternalFuntions.DtNow(): TDateTime;
begin
  exit(System.SysUtils.Now);
end;

class function TInternalFuntions.Int(const AValue: TValue): integer;
begin
  exit(asInt(AValue));
end;

class function TInternalFuntions.StartsWith(const AString, ASearch: string): boolean;
begin
  exit(AString.StartsWith(ASearch, true));
end;

class function TInternalFuntions.Split(const AString: string): TArray<string>;
begin
  exit(Split(AString, ' '));
end;

class function TInternalFuntions.StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean;
begin
  exit(AString.StartsWith(ASearch, AIgnoreCase));
end;

class function TInternalFuntions.Str(const AValue: TValue): string;
begin
  exit(AsString(AValue));
end;

function reverse(const AStr: string): string;
var
  LStart, LEnd, LMid: integer;
begin
  setlength(result, length(AStr));
  LMid := length(AStr) div 2 + 1;
  LStart := 1;
  LEnd := length(AStr);
  while LStart <= LMid do
  begin
    result[LStart] := AStr[LEnd];
    result[LEnd] := AStr[LStart];
    inc(LStart);
    dec(LEnd);
  end;
end;

class function TInternalFuntions.UCFirst(const AString: string): string;
begin
  result := AString.ToLower();
  result[1] := Uppercase(result[1])[1];
end;

class function TInternalFuntions.Replace(const AValue, AWith, AIn: string): string;
begin
  exit(AIn.Replace(AValue, AWith, [rfReplaceAll]));
end;

class function TInternalFuntions.Rev(const AString: string): string;
begin
  exit(reverse(AString));
end;

class function TInternalFuntions.IsNull(const AValue: TValue): boolean;
begin
  exit(IsNull(AValue));
end;

class function TInternalFuntions.IsStr(const AValue: TValue): boolean;
begin
  exit(isStrLike(AValue));
end;

class function TInternalFuntions.IsInt(const AValue: TValue): boolean;
begin
  exit(isIntLike(AValue));
end;

class function TInternalFuntions.IsBool(const AValue): boolean;
begin
  exit(IsBool(AValue));
end;

class function TInternalFuntions.IsNum(const AValue: TValue): boolean;
begin
  exit(isnumlike(AValue));
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
  exit(FFunctions.Count > 0);
end;

procedure TTemplateFunctions.RegisterDefaults;
begin
  AddFunctions(TInternalFuntions);
end;

procedure TTemplateFunctions.Remove(const AName: string);
begin
  FFunctions.Remove(AName);
end;

function TTemplateFunctions.TryGetValue(const AName: string; out AMethods: TArray<TRttiMethod>): boolean;
begin
  exit(FFunctions.TryGetValue(AName, AMethods));
end;

{ TValueCompare }

function TValueCompare.Compare(const ALeft, ARight: TValue): integer;
begin
  if ALeft.TypeInfo <> ARight.TypeInfo then
    raise ETemplateFunction.Create(STypesAreNotOfTheSameType);
  case ALeft.Kind of
    tkInteger, tkInt64:
      exit(TComparer<int64>.Default.Compare(ALeft.AsInt64, ARight.AsInt64));
    tkFloat:
      exit(TComparer<extended>.Default.Compare(ALeft.AsExtended, ARight.AsExtended));
    tkString, tkLString, tkWString, tkUString:
      exit(TComparer<string>.Default.Compare(ALeft.AsString, ARight.AsString));
  else
    raise ETemplateFunction.Create(STypeNotSupported);
  end;
end;

class function TInternalFuntions.PadLeft(const AStr: string; const ANum: integer; const APadChar: char): string;
begin
  exit(AStr.PadLeft(ANum, APadChar));
end;

initialization

GFunctions := CreateTemplateFunctions();
GValueCompare := TValueCompare.Create;

finalization

GFunctions := nil;
GValueCompare := nil;

end.
