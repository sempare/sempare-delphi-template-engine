(*%**************************************************************************************************
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
 * Copyright (c) 2019-2025 Sempare Limited                                                          *
 *                                                                                                  *
 * Contact: info@sempare.ltd                                                                        *
 *                                                                                                  *
 * Licensed under the Apache Version 2.0 or the Sempare Commercial License                          *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.apache.org/licenses/LICENSE-2.0                                                      *
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

{$I 'Sempare.Template.Compiler.inc'}

uses
  System.Rtti,
  Sempare.Template.AST,
  Sempare.Template.Context;

type
  ETemplateFunction = class(ETemplate);

function CreateTemplateFunctions(const AContext: ITemplateContext; const ARegisterDefaults: boolean = true): ITemplateFunctions;
function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;

implementation

uses
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}
  Data.DB,
{$ENDIF}
{$IFDEF SUPPORT_HASH}
  System.Hash,
{$ENDIF}
{$IFDEF SUPPORT_ENCODING}
  System.NetEncoding,
{$ENDIF}
  System.TypInfo, // needed for XE6 and below to access the TTypeKind variables
  System.SysUtils,
  System.Math,
  Sempare.Template.JSON,
  System.RegularExpressions,
  System.Generics.Collections,
  System.Generics.Defaults,
  Sempare.Template.ResourceStrings,
  Sempare.Template.StackFrame,
  Sempare.Template.Common,
  Sempare.Template.Util,
  Sempare.Template.Rtti;

{$I 'Sempare.Template.Compiler.inc'}

type
  TValueCompare = class(TComparer<TValue>)
  public
    function Compare(const ALeft, ARight: TValue): integer; override;
  end;

  TTemplateFunctions = class(TInterfacedObject, ITemplateFunctions)
  private
    [Weak]
    FContext: ITemplateContext;
    FFunctions: TDictionary<string, TArray<TRttiMethod>>;
  public
    constructor Create(const AContext: ITemplateContext);
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

function CreateTemplateFunctions(const AContext: ITemplateContext; const ARegisterDefaults: boolean): ITemplateFunctions;
begin
  result := TTemplateFunctions.Create(AContext);
  if ARegisterDefaults then
    result.RegisterDefaults;
end;

function TTemplateFunctions.Add(const AMethod: TRttiMethod): boolean;
var
  LMethods: TArray<TRttiMethod>;
  LMethodName: string;
  LLength: integer;
begin
  result := AMethod.IsStatic and AMethod.IsClassMethod;
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
  LClassType := FContext.RttiContext().GetType(AClass);
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
    class function TrimLeft(const AString: string): string; static;
    class function TrimRight(const AString: string): string; static;
    class function SubStr(const AString: string; AStartOffset: integer): string; overload; static;
    class function SubStr(const AString: string; AStartOffset, ALength: integer): string; overload; static;
    class function Substring(const AString: string; AStartOffset, AEndOffset: integer): string; overload; static;
    class function Substring(const AString: string; AStartOffset: integer): string; overload; static;
    class function Pos(const search, Str: string; offset: integer): integer; overload; static;
    class function Pos(const search, Str: string): integer; overload; static;
    class function Len(const AContext: ITemplateContext; const AString: TValue): integer; static;
    class function Fmt(const AContext: ITemplateContext; const AArgs: TArray<TValue>): string; static;
    class function FmtDt(const AFormat: string; const ADateTime: TDateTime): string; static;
    class function DtNow: TDateTime; static;
    class function BoolStr(const AContext: ITemplateContext; const AValue: TValue): boolean; static;
    class function Bool(const AValue: TValue): boolean; static;
    class function Num(const AContext: ITemplateContext; const AValue: TValue): extended; static;
    class function Int(const AContext: ITemplateContext; const AValue: TValue): integer; static;
    class function Str(const AContext: ITemplateContext; const AValue: TValue): string; static;
    class function UCFirst(const AString: string): string; static;
    class function Rev(const AString: string): string; static;
    class function ContainsKey(const AContext: ITemplateContext; const AValue: TValue; const AKey: string): boolean; static;
    class function IsNull(const AValue: TValue): boolean; static;
    class function IsNil(const AValue: TValue): boolean; static;
    class function IsEmpty(const AContext: ITemplateContext; const AValue: TValue): boolean; static;
    class function IsMap(const AContext: ITemplateContext; const AValue: TValue): boolean; static;
    class function IsObject(const AValue: TValue): boolean; static;
    class function IsRecord(const AValue: TValue): boolean; static;
    class function IsStr(const AValue: TValue): boolean; static;
    class function IsInt(const AValue: TValue): boolean; static;
    class function IsBool(const AValue: TValue): boolean; static;
    class function IsNum(const AValue: TValue): boolean; static;
    class function StartsWith(const AString, ASearch: string): boolean; overload; static;
    class function StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;
    class function EndsWith(const AString, ASearch: string): boolean; overload; static;
    class function EndsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean; overload; static;
    class function TypeOf(const AContext: ITemplateContext; const AValue: TValue): string; static;
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
    class function Min(const AValue, BValue: double): double; static;
    class function Max(const AValue, BValue: double): double; static;
    class function Abs(const AValue: double): double; static;
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}
    class function RecordCount(const ADataset: TDataSet): integer; static;
    class function IsDataSet(const ADataset: TValue): boolean; static;
{$ENDIF}
{$IFDEF SUPPORT_ENCODING}
    class function Base64Encode(const AStr: string): string; static;
    class function Base64Decode(const AStr: string): string; static;
    class function HtmlUnescape(const AStr: string): string; static;
    class function HtmlEscape(const AStr: string): string; static;
{$ENDIF}
{$IFDEF SUPPORT_URL_FORM_ENCODING}
    class function UrlDecode(const AStr: string): string; static;
    class function FormDecode(const AStr: string): string; static;
{$ENDIF}
{$IFDEF SUPPORT_HASH}
    class function Md5(const AStr: string): string; static;
    class function Sha1(const AStr: string): string; static;
    class function Sha256(const AStr: string): string; static;
{$ENDIF}
    class function TemplateExists(const AContext: ITemplateContext; const AStr: string): boolean; static;
    class function Manage(const AContext: ITemplateContext; const AObject: TObject): TObject; static;
    class procedure Unmanage(const AContext: ITemplateContext; const AObject: TObject); static;
    class function ToJson(const AContext: ITemplateContext; const AValue: TValue): string; static;
    class function ParseJson(const AValue: string): TValue; static;
    class function GetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string): TValue; overload; static;
    class function GetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string; const AStackOffset: integer): TValue; overload; static;
    class procedure SetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string; const AValue: TValue); overload; static;
    class procedure SetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string; const AValue: TValue; const AStackOffset: integer); overload; static;
    class function StackDepth(const AStackFrames: TObjectStack<TStackFrame>): integer; static;
    class procedure SetMapValue(const AMap: TValue; const AKey: string; const AValue: TValue); static;
    class function SempareVersion(): string; static;
    class function Default(const AContext: ITemplateContext; const AValue: TValue; const ADefault: TValue): TValue; static;
    class function DomId(const AContext: ITemplateContext; const AValue: TValue; const AExtraContext: string): string; overload; static;
    class function DomId(const AContext: ITemplateContext; const AValue: TValue): string; overload; static;
  end;

class function TInternalFuntions.Min(const AValue, BValue: double): double;
begin
  exit(System.Math.Min(AValue, BValue));
end;

class function TInternalFuntions.Max(const AValue, BValue: double): double;
begin
  exit(System.Math.Max(AValue, BValue));
end;

class function TInternalFuntions.PadLeft(const AStr: string; const ANum: integer): string;
begin
  exit(AStr.PadLeft(ANum));
end;

class function TInternalFuntions.PadRight(const AStr: string; const ANum: integer; const APadChar: char): string;
begin
  exit(AStr.PadRight(ANum, APadChar));
end;

class function TInternalFuntions.ParseJson(const AValue: string): TValue;
var
  LJsonValue: TJsonValue;
  LJsonNum: TJSonNumber absolute LJsonValue;
  LJsonObj: TJSONObject absolute LJsonValue;
  LJsonStr: TJSonString absolute LJsonValue;

begin
  LJsonValue := TJSONObject.ParseJsonValue(AValue);
  try
    if LJsonValue is TJSonNumber then
    begin
      exit(FloatToTValue(LJsonNum.AsDouble));
    end;
    if LJsonValue is TJSonString then
      exit(LJsonStr.Value);
{$IFDEF SUPPORT_JSON_BOOL}
    if LJsonValue is TJSONBool then
      exit(TJSONBool(LJsonValue).AsBoolean);
{$ELSE}
    if LJsonValue is TJSONTrue then
      exit(true);
    if LJsonValue is TJSONFalse then
      exit(false);
{$ENDIF}
    if LJsonValue is TJSONObject then
      exit(TValue.From(TMap.ParseJson(LJsonObj)));
  finally
    LJsonValue.Free;
  end;
  exit(TValue.Empty);
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
{$IFDEF SUPPORT_HASH}

class function TInternalFuntions.Sha1(const AStr: string): string;
begin
  exit(THashSha1.GetHashString(AStr));
end;

class function TInternalFuntions.Sha256(const AStr: string): string;
var
  LHash: THashSha2;
begin
  LHash := THashSha2.Create(THashSha2.TSHA2Version.Sha256);
  LHash.Update(AStr);
  exit(LHash.HashAsString);
end;

class function TInternalFuntions.Md5(const AStr: string): string;
begin
  exit(THashMD5.GetHashString(AStr));
end;
{$ENDIF}

class procedure TInternalFuntions.SetMapValue(const AMap: TValue; const AKey: string; const AValue: TValue);
var
  LMap: IMapExpr;
begin
  LMap := AMap.AsType<IMapExpr>();
  LMap.GetMap.Items[AKey] := AValue;
end;

class procedure TInternalFuntions.SetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string; const AValue: TValue);
begin
  SetVariable(AStackFrames, AVariable, AValue, -1);
end;

class procedure TInternalFuntions.SetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string; const AValue: TValue; const AStackOffset: integer);
var
  LStackFrame: TStackFrame;
begin
  if (AStackFrames = nil) or (AStackFrames.count = 0) or (AStackOffset > 0) or (-AStackOffset > AStackFrames.count) then
    exit;
{$IFDEF DEFINE SUPPORT_STACK_LIST_PROPERTY}
  LStackFrame := AStackFrames.List[AStackFrames.count + AStackOffset];
{$ELSE}
  LStackFrame := AStackFrames.ToArray[AStackFrames.count + AStackOffset];
{$ENDIF}
  LStackFrame[AVariable] := AValue;
end;

class function TInternalFuntions.SempareVersion: string;
begin
  exit(GetSempareVersion());
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

class function TInternalFuntions.Manage(const AContext: ITemplateContext; const AObject: TObject): TObject;
var
  LContext: ITemplateEvaluationContext;
begin
  if supports(AContext, ITemplateEvaluationContext, LContext) then
    LContext.Manage(AObject);
  exit(AObject);
end;

class function TInternalFuntions.Match(const AValue, ARegex: string): boolean;
begin
  exit(TRegex.IsMatch(AValue, ARegex));
end;

class function TInternalFuntions.nl(const ANum: integer): string;
begin
  exit(''.PadLeft(ANum, #10));
end;

class function TInternalFuntions.Num(const AContext: ITemplateContext; const AValue: TValue): extended;
begin
  exit(AsNum(AValue, AContext));
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

class function TInternalFuntions.TemplateExists(const AContext: ITemplateContext; const AStr: string): boolean;
var
  LTemplate: ITemplate;
begin
  exit(AContext.TryGetTemplate(AStr, LTemplate));
end;

class function TInternalFuntions.ToJson(const AContext: ITemplateContext; const AValue: TValue): string;
var
  LMap: TMap;
begin
  if IsStr(AValue) then
    exit(AsString(AValue, AContext));
  if IsNumLike(AValue) then
  begin
    if IsIntLike(AValue) then
      exit(IntToStr(trunc(AValue.AsExtended)))
    else
      exit(FloatToStr(AValue.AsExtended));
  end;
  if IsBool(AValue) then
    exit(BoolToStr(AsBoolean(AValue), true).ToLower);
  if AValue.IsType<TMap> then
  begin
    LMap := AValue.AsType<TMap>;
    exit(LMap.ToJson());
  end
  else if AValue.IsType<IMapExpr> then
  begin
    LMap := AValue.AsType<IMapExpr>.GetMap;
    exit(LMap.ToJson());
  end
  else
    raise ETemplateFunction.CreateRes(@STypeNotSupported);
end;

class function TInternalFuntions.Trim(const AString: string): string;
begin
  exit(AString.Trim());
end;

class function TInternalFuntions.TrimLeft(const AString: string): string;
begin
  exit(AString.TrimLeft);
end;

class function TInternalFuntions.TrimRight(const AString: string): string;
begin
  exit(AString.TrimRight);
end;

class function TInternalFuntions.TypeOf(const AContext: ITemplateContext; const AValue: TValue): string;
var
  lmsg: string;
  lpos: integer;
begin
  try
    exit(AContext.RttiContext().GetType(AValue.AsType<TValue>.TypeInfo).QualifiedName);
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

class function TInternalFuntions.Len(const AContext: ITemplateContext; const AString: TValue): integer;
begin
  if AString.IsType<string> then
    exit(length(AString.AsType<string>()))
  else if AString.Kind in [tkDynArray, tkArray] then
    exit(AString.GetArrayLength)
  else if MatchMap(AContext, AString.TypeInfo) then
    exit(AString.AsType<TMap>.count)
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
      raise ETemplateFunction.CreateRes(@STypeNotSupported);
    end;
end;
{$ENDIF}

function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;
var
  LIdx: integer;
begin
  setlength(result, length(AArgs));
  for LIdx := 0 to high(AArgs) do
  begin
{$IFNDEF SUPPORT_AS_VARREC}
    result[LIdx] := AArgs[LIdx].AsLimitedVarRec;
{$ELSE}
    if AArgs[LIdx].Kind = tkFloat then
      AArgs[LIdx] := extended(AArgs[LIdx].AsExtended);

    result[LIdx] := AArgs[LIdx].AsVarRec;
{$ENDIF}
  end;
end;

class function TInternalFuntions.EndsWith(const AString, ASearch: string): boolean;
begin
  exit(AString.EndsWith(ASearch, true));
end;

class function TInternalFuntions.EndsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean;
begin
  exit(AString.EndsWith(ASearch, AIgnoreCase));
end;

class function TInternalFuntions.Fmt(const AContext: ITemplateContext; const AArgs: TArray<TValue>): string;
begin
  exit(format(AsString(AArgs[0], AContext), ToArrayTVarRec(copy(AArgs, 1, length(AArgs) - 1))));
end;

class function TInternalFuntions.FmtDt(const AFormat: string; const ADateTime: TDateTime): string;
begin
  exit(FormatDateTime(AFormat, ADateTime));
end;

class function TInternalFuntions.GetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string; const AStackOffset: integer): TValue;
var
  LStackFrame: TStackFrame;
begin
  if (AStackFrames = nil) or (AStackFrames.count = 0) or (AStackOffset > 0) or (-AStackOffset > AStackFrames.count) then
    exit;
{$IFDEF DEFINE SUPPORT_STACK_LIST_PROPERTY}
  LStackFrame := AStackFrames.List[AStackFrames.count + AStackOffset];
{$ELSE}
  LStackFrame := AStackFrames.ToArray[AStackFrames.count + AStackOffset];
{$ENDIF}
  exit(LStackFrame[AVariable]);
end;

class function TInternalFuntions.GetVariable(const AStackFrames: TObjectStack<TStackFrame>; const AVariable: string): TValue;
begin
  exit(GetVariable(AStackFrames, AVariable, -1));
end;

{$IFDEF SUPPORT_URL_FORM_ENCODING}

class function TInternalFuntions.FormDecode(const AStr: string): string;
begin
  exit(TNetEncoding.URL.FormDecode(AStr));
end;

class function TInternalFuntions.UrlDecode(const AStr: string): string;
begin
  exit(TNetEncoding.URL.UrlDecode(AStr));
end;
{$ENDIF}
{$IFDEF SUPPORT_ENCODING}

class function TInternalFuntions.HtmlEscape(const AStr: string): string;
begin
  exit(TNetEncoding.HTML.Encode(AStr));
end;

class function TInternalFuntions.HtmlUnescape(const AStr: string): string;
begin
  exit(TNetEncoding.HTML.Decode(AStr));
end;

class function TInternalFuntions.Base64Decode(const AStr: string): string;
begin
  exit(TNetEncoding.Base64.Decode(AStr));
end;

class function TInternalFuntions.Base64Encode(const AStr: string): string;
begin
  exit(TNetEncoding.Base64.Encode(AStr));
end;
{$ENDIF}

class function TInternalFuntions.Abs(const AValue: double): double;
begin
  exit(System.Abs(AValue));
end;

class function TInternalFuntions.Bool(const AValue: TValue): boolean;
begin
  exit(AsBoolean(AValue));
end;

class function TInternalFuntions.BoolStr(const AContext: ITemplateContext; const AValue: TValue): boolean;
begin
  if isStrLike(AValue) then
    exit(AsString(AValue, AContext) = 'true')
  else
    exit(AsBoolean(AValue));
end;

class function TInternalFuntions.Chr(const AValue: int64): string;
begin
  exit(System.Chr(AValue));
end;

class function TInternalFuntions.ContainsKey(const AContext: ITemplateContext; const AValue: TValue; const AKey: string): boolean;
begin
  exit(Contains(nil, AKey, AValue, AContext));
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

class function TInternalFuntions.Default(const AContext: ITemplateContext; const AValue, ADefault: TValue): TValue;
begin
  if IsEmpty(AContext, AValue) then
    exit(ADefault)
  else
    exit(AValue);
end;

class function TInternalFuntions.DomId(const AContext: ITemplateContext; const AValue: TValue): string;
begin
  exit(DomId(AContext, AValue, ''));
end;

class function TInternalFuntions.DomId(const AContext: ITemplateContext; const AValue: TValue; const AExtraContext: string): string;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LField: TRttiField;
  LIsRecord: boolean;
  LIsObject: boolean;
  LPtr: Pointer;
  LValue: string;

  function GetPtr: Pointer;
  begin
    if LIsRecord then
      exit(AValue.GetReferenceToRawData)
    else
      exit(AValue.AsObject);
  end;

begin
  LIsRecord := IsRecord(AValue);
  LIsObject := false;
  if not LIsRecord then
  begin
    LIsObject := IsObject(AValue);
  end;
  if LIsRecord or LIsObject then
  begin
    LType := AContext.RttiContext().GetType(AValue.TypeInfo);
    LField := LType.GetField('id');
    LPtr := GetPtr;
    if LField <> nil then
    begin
      LValue := AsString(LField.GetValue(LPtr), AContext);
    end
    else
    begin
      LProperty := LType.GetProperty('id');
      if LProperty <> nil then
      begin
        LValue := AsString(LProperty.GetValue(LPtr), AContext);
      end
      else
      begin
        raise ETemplateFunction.CreateRes(@SIdFieldOrPropertyExpected);
      end;
    end;
    result := LType.Name + '_' + LValue;
    if not AExtraContext.IsEmpty then
    begin
      result := AExtraContext + '_' + result;
    end;
    exit;
  end;
  raise ETemplateFunction.CreateRes(@SClassOrRecordExpected);
end;

class function TInternalFuntions.DtNow(): TDateTime;
begin
  exit(System.SysUtils.Now);
end;

class function TInternalFuntions.Int(const AContext: ITemplateContext; const AValue: TValue): integer;
begin
  exit(asInt(AValue, AContext));
end;

class function TInternalFuntions.StartsWith(const AString, ASearch: string): boolean;
begin
  exit(AString.StartsWith(ASearch, true));
end;

class function TInternalFuntions.Split(const AString: string): TArray<string>;
begin
  exit(Split(AString, ' '));
end;

class function TInternalFuntions.StackDepth(const AStackFrames: TObjectStack<TStackFrame>): integer;
begin
  if (AStackFrames = nil) then
    exit(-1);
  exit(AStackFrames.count);
end;

class function TInternalFuntions.StartsWith(const AString, ASearch: string; const AIgnoreCase: boolean): boolean;
begin
  exit(AString.StartsWith(ASearch, AIgnoreCase));
end;

class function TInternalFuntions.Str(const AContext: ITemplateContext; const AValue: TValue): string;
begin
  exit(AsString(AValue, AContext));
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

class procedure TInternalFuntions.Unmanage(const AContext: ITemplateContext; const AObject: TObject);
var
  LContext: ITemplateEvaluationContext;
begin
  if supports(AContext, ITemplateEvaluationContext, LContext) then
    LContext.Unmanage(AObject);
end;

class function TInternalFuntions.Replace(const AValue, AWith, AIn: string): string;
begin
  exit(AIn.Replace(AValue, AWith, [rfReplaceAll]));
end;

class function TInternalFuntions.Rev(const AString: string): string;
begin
  exit(reverse(AString));
end;

class function TInternalFuntions.IsNil(const AValue: TValue): boolean;
begin
  exit(IsNull(AValue));
end;

class function TInternalFuntions.IsNull(const AValue: TValue): boolean;
begin
  exit(Sempare.Template.Rtti.IsNull(AValue));
end;

class function TInternalFuntions.IsStr(const AValue: TValue): boolean;
begin
  exit(isStrLike(AValue));
end;

class function TInternalFuntions.IsInt(const AValue: TValue): boolean;
begin
  exit(IsIntLike(AValue));
end;

class function TInternalFuntions.IsMap(const AContext: ITemplateContext; const AValue: TValue): boolean;
begin
  exit(MatchMap(AContext, AValue.TypeInfo) or AValue.IsType<IMapExpr>);
end;

class function TInternalFuntions.IsBool(const AValue: TValue): boolean;
begin
  exit(Sempare.Template.Rtti.IsBool(AValue));
end;

class function TInternalFuntions.IsEmpty(const AContext: ITemplateContext; const AValue: TValue): boolean;
var
  LObject: TObject;
begin
  if not AValue.IsObject then
  begin
    if isStrLike(AValue) then
      exit(AValue.AsString.IsEmpty);
    exit(false);
  end;
  LObject := AValue.AsObject;
  exit(not assigned(LObject) or IsEmptyObject(AContext, LObject));
end;

class function TInternalFuntions.IsNum(const AValue: TValue): boolean;
begin
  exit(IsNumLike(AValue));
end;

class function TInternalFuntions.IsObject(const AValue: TValue): boolean;
begin
  exit(AValue.IsObject);
end;

class function TInternalFuntions.IsRecord(const AValue: TValue): boolean;
begin
  exit(AValue.Kind in [tkRecord{$IFDEF SUPPORT_CUSTOM_MANAGED_RECORDS}, tkMRecord{$ENDIF}]);
end;

constructor TTemplateFunctions.Create(const AContext: ITemplateContext);
begin
  FContext := AContext;
  FFunctions := TDictionary < string, TArray < TRttiMethod >>.Create;
end;

destructor TTemplateFunctions.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

function TTemplateFunctions.GetIsEmpty: boolean;
begin
  exit(FFunctions.count > 0);
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
    raise ETemplateFunction.CreateRes(@STypesAreNotOfTheSameType);
  case ALeft.Kind of
    tkInteger, tkInt64:
      exit(TComparer<int64>.Default.Compare(ALeft.AsInt64, ARight.AsInt64));
    tkFloat:
      exit(TComparer<extended>.Default.Compare(ALeft.AsExtended, ARight.AsExtended));
    tkString, tkLString, tkWString, tkUString:
      exit(TComparer<string>.Default.Compare(ALeft.AsString, ARight.AsString));
  else
    raise ETemplateFunction.CreateRes(@STypeNotSupported);
  end;
end;

class function TInternalFuntions.PadLeft(const AStr: string; const ANum: integer; const APadChar: char): string;
begin
  exit(AStr.PadLeft(ANum, APadChar));
end;
{$IFDEF SEMPARE_TEMPLATE_FIREDAC}

class function TInternalFuntions.IsDataSet(const ADataset: TValue): boolean;
begin
  exit(ADataset.IsObject and ADataset.IsInstanceOf(TDataSet));
end;

class function TInternalFuntions.RecordCount(const ADataset: TDataSet): integer;
begin
  exit(ADataset.RecordCount);
end;
{$ENDIF}

initialization

GValueCompare := TValueCompare.Create;

finalization

GValueCompare := nil;

end.
