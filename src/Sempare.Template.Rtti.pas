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
 * Copyright (c) 2019-2024 Sempare Limited                                                          *
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
unit Sempare.Template.Rtti;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  Sempare.Template.Util,
  Sempare.Template.StackFrame,
  Sempare.Template.Context,
  Sempare.Template.AST;

type
  ETemplateRTTI = class(ETemplate);

function AsBoolean(const AValue: TValue): boolean;
function AsString(const AValue: TValue; const AContext: ITemplateContext): string;
function AsNum(const AValue: TValue; const AContext: ITemplateContext): extended;
function AsDateTime(const AValue: TValue): TDateTime;
function AsInt(const AValue: TValue; const AContext: ITemplateContext): int64;

function IsBool(const AValue: TValue): boolean;
function IsStrLike(const AValue: TValue): boolean;
function IsIntLike(const AValue: TValue): boolean;
function IsNumLike(const AValue: TValue): boolean;
function IsNull(const AValue: TValue): boolean;
function IsEnumerable(const AContext: ITemplateContext; const AValue: TValue): boolean;
function Contains(const APosition: IPosition; const ALeft, ARight: TValue; const AContext: ITemplateContext): boolean;

function ExitEmpty(var AResult: TValue): boolean;

function isEqual(const ALeft: TValue; const ARight: TValue; const AContext: ITemplateContext): boolean;
function isLessThan(const ALeft: TValue; const ARight: TValue; const AContext: ITemplateContext): boolean;
function isGreaterThan(const ALeft: TValue; const ARight: TValue; const AContext: ITemplateContext): boolean;

function IsEmptyValue(const AContext: ITemplateContext; const AValue: TValue): boolean;
function IsEmptyObject(const AContext: ITemplateContext; const AObject: TObject): boolean;

procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue); overload;
procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;

procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue); overload;
procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
procedure AssertString(const APositional: IPosition; const AValue: TValue);
procedure AssertArray(const AContext: ITemplateContext; const APositional: IPosition; const AValue: TValue);

function Deref(const APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext): TValue; overload;
function TryDeref(const APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean; overload;
function TryDeref(const APosition: IPosition; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean; overload;
function Deref(const APosition: IPosition; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext): TValue; overload;

type
  TDerefMatchFunction = function(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
  TDerefMatchInterfaceFunction = function(const AInterface: IInterface): boolean;
  TDerefFunction = function(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
  TDerefWrappedValue = function(const APosition: IPosition; const obj: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
  TPopulateStackFrame = procedure(const AContext: ITemplateContext; const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
  TPopulateMatchFunction = function(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
  TCheckEmptyObjectFunction = function(const AContext: ITemplateContext; const AObject: TObject): boolean;

procedure RegisterDeref(const AMatch: TDerefMatchFunction; const AFunction: TDerefFunction); overload;
procedure RegisterDeref(const AMatch: TDerefMatchInterfaceFunction; const AFunction: TDerefFunction); overload;
procedure RegisterPopulateStackFrame(const AMatch: TPopulateMatchFunction; const APopulator: TPopulateStackFrame); overload;
procedure RegisterEmptyObjectCheck(const AMatch: TDerefMatchFunction; const AFunction: TCheckEmptyObjectFunction);
procedure RegisterDerefWrappedValue(const AMatch: TDerefMatchFunction; const AFunction: TDerefWrappedValue);

function PopulateStackFrame(const AContext: ITemplateContext; const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue): boolean;

function MatchMap(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo): boolean; overload;
function MatchMap(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean; overload;
function MatchMap(const AContext: ITemplateContext; const AValue: TValue): boolean; overload;
function MatchMapExpr(const AInterface: IInterface): boolean;

var
  GRttiContext: TRttiContext;

implementation

uses
{$IFDEF SUPPORT_JSON_DBX}
  Data.DBXJSON,
  Data.DBXPlatform,
{$ELSE}
  System.JSON,
{$ENDIF}
  System.Math,
  Data.DB,
  System.Generics.Collections,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Common;

var
  GPopulateFunctions: TList<TPair<TPopulateMatchFunction, TPopulateStackFrame>>;
  GDerefFunctions: TList<TPair<TDerefMatchFunction, TDerefFunction>>;
  GDerefInterfaceFunctions: TList<TPair<TDerefMatchInterfaceFunction, TDerefFunction>>;
  GEmptyObjectFunctions: TList<TPair<TDerefMatchFunction, TCheckEmptyObjectFunction>>;
  GDerefWrappedValues: TList<TPair<TDerefMatchFunction, TDerefWrappedValue>>;

const
  INT_LIKE: set of TTypeKind = [tkInteger, tkInt64];
  NUMBER_LIKE: set of TTypeKind = [tkInteger, tkInt64, tkFloat];
  STR_LIKE: set of TTypeKind = [tkString, tkWString, tkUString, tkLString];

function MatchValue(const ATypeInfo: PTypeInfo): boolean; forward;

procedure RegisterDerefWrappedValue(const AMatch: TDerefMatchFunction; const AFunction: TDerefWrappedValue);
begin
  GDerefWrappedValues.add(TPair<TDerefMatchFunction, TDerefWrappedValue>.Create(AMatch, AFunction));
end;

procedure RegisterDeref(const AMatch: TDerefMatchInterfaceFunction; const AFunction: TDerefFunction);
begin
  GDerefInterfaceFunctions.add(TPair<TDerefMatchInterfaceFunction, TDerefFunction>.Create(AMatch, AFunction));
end;

procedure RegisterDeref(const AMatch: TDerefMatchFunction; const AFunction: TDerefFunction);
begin
  GDerefFunctions.add(TPair<TDerefMatchFunction, TDerefFunction>.Create(AMatch, AFunction));
end;

procedure RegisterPopulateStackFrame(const AMatch: TPopulateMatchFunction; const APopulator: TPopulateStackFrame); overload;
begin
  GPopulateFunctions.add(TPair<TPopulateMatchFunction, TPopulateStackFrame>.Create(AMatch, APopulator));
end;

procedure RegisterEmptyObjectCheck(const AMatch: TDerefMatchFunction; const AFunction: TCheckEmptyObjectFunction);
begin
  GEmptyObjectFunctions.add(TPair<TDerefMatchFunction, TCheckEmptyObjectFunction>.Create(AMatch, AFunction));
end;

function PopulateStackFrame(const AContext: ITemplateContext; const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue): boolean;
var
  LFuncPair: TPair<TPopulateMatchFunction, TPopulateStackFrame>;
begin
  for LFuncPair in GPopulateFunctions do
  begin
    if LFuncPair.Key(AContext, ARttiType.Handle, ARttiType.ClassType) then
    begin
      LFuncPair.Value(AContext, StackFrame, ARttiType, AClass);
      exit(true);
    end;
  end;
  exit(false);
end;

function IsNull(const AValue: TValue): boolean;
begin
  exit(AValue.IsEmpty);
end;

function ExitEmpty(var AResult: TValue): boolean;
begin
  AResult := TValue.Empty;
  exit(false);
end;

function Contains(const APosition: IPosition; const ALeft, ARight: TValue; const AContext: ITemplateContext): boolean;
var
  TRightType: TRttiType;

  procedure VisitObject;
  var
    LValue: TValue;
    LEnumObject: TObject;
    LObjectGetEnumMethod: TRttiMethod;
    LEnumMoveNextMethod: TRttiMethod;
    LEnumCurrentProperty: TRttiProperty;
  begin
    LObjectGetEnumMethod := TRightType.GetMethod('GetEnumerator');
    if LObjectGetEnumMethod = nil then
      RaiseError(APosition, SGetEnumeratorNotFoundOnObject);
    LValue := LObjectGetEnumMethod.Invoke(ARight.AsObject, []);
    if LValue.IsEmpty then
      RaiseError(APosition, SValueIsNotEnumerable);
    LEnumObject := LValue.AsObject;
    try
      TRightType := AContext.RttiContext.GetType(LEnumObject.ClassType);
      LEnumMoveNextMethod := TRightType.GetMethod('MoveNext');
      LEnumCurrentProperty := TRightType.GetProperty('Current');
      while LEnumMoveNextMethod.Invoke(LEnumObject, []).AsBoolean do
      begin
        LValue := LEnumCurrentProperty.GetValue(LEnumObject);
        if LValue.TypeInfo = TypeInfo(TValue) then
          LValue := LValue.AsType<TValue>();
        if isEqual(ALeft, LValue, AContext) then
        begin
          result := true;
          exit;
        end;
      end;
    finally
      LEnumObject.Free;
    end;
    exit;
  end;

  procedure visitarray;
  var
    LArrayType: TRttiArrayType;
    LIndex: integer;
    LElementValue: TValue;
  begin
    LArrayType := TRightType as TRttiArrayType;
    if LArrayType.DimensionCount > 1 then
      RaiseError(APosition, SOnlyOneDimensionalArraysAreSupported);
    for LIndex := 0 to ARight.GetArrayLength - 1 do
    begin
      LElementValue := ARight.GetArrayElement(LIndex);
      if LElementValue.TypeInfo = TypeInfo(TValue) then
        LElementValue := LElementValue.AsType<TValue>();
      if isEqual(ALeft, LElementValue, AContext) then
      begin
        result := true;
        exit;
      end;
    end;
  end;

  procedure VisitDynArray;
  var
    LIndex: integer;
    LElementValue: TValue;
  begin
    for LIndex := 0 to ARight.GetArrayLength - 1 do
    begin
      LElementValue := ARight.GetArrayElement(LIndex);
      if LElementValue.TypeInfo = TypeInfo(TValue) then
        LElementValue := LElementValue.AsType<TValue>();
      if isEqual(ALeft, LElementValue, AContext) then
      begin
        result := true;
        exit;
      end;
    end;
  end;

  procedure VisitMap(const AMap: TMap);
  begin
    result := AMap.containskey(ALeft.AsType<string>);
  end;

  procedure VisitRecord;
  begin
    if ARight.IsType<TMap> then
    begin
      VisitMap(ARight.AsType<TMap>);
      exit;
    end;
  end;

  procedure VisitInterface;
  begin
    if ARight.IsType<IMapExpr> then
    begin
      if not ALeft.IsType<string> then
      begin
        exit;
      end;
      VisitMap(ARight.AsType<IMapExpr>.GetMap);
      exit;
    end;
  end;

begin
  result := false;
  if not IsEnumerable(AContext, ARight) and not MatchMap(AContext, ARight) then
    RaiseError(APosition, SValueIsNotEnumerableOrMap);
  TRightType := AContext.RttiContext.GetType(ARight.TypeInfo);
  case TRightType.TypeKind of
    tkInterface:
      VisitInterface;
    tkRecord{$IFDEF SUPPORT_CUSTOM_MANAGED_RECORDS}, tkMRecord{$ENDIF}:
      VisitRecord;
    tkClass, tkClassRef:
      VisitObject;
    tkArray:
      visitarray;
    tkDynArray:
      VisitDynArray;
  else
    RaiseError(APosition, SGetEnumeratorNotFoundOnObject);
  end;
end;

function IsEnumerable(const AContext: ITemplateContext; const AValue: TValue): boolean;
begin
  case AValue.Kind of
    tkDynArray, tkArray:
      exit(true);
    tkClass, tkClassRef:
      exit(AContext.RttiContext.GetType(AValue.TypeInfo).GetMethod('GetEnumerator') <> nil);
  else
    exit(false);
  end;
end;

function IsStrLike(const AValue: TValue): boolean;
begin
  exit(AValue.Kind in STR_LIKE);
end;

function IsIntLike(const AValue: TValue): boolean;
begin
  if AValue.Kind = tkFloat then
    exit(SameValue(AValue.asExtended, trunc(AValue.asExtended)));
  exit(AValue.Kind in INT_LIKE);
end;

function IsNumLike(const AValue: TValue): boolean;
begin
  exit(AValue.Kind in NUMBER_LIKE);
end;

function AsNum(const AValue: TValue; const AContext: ITemplateContext): extended;
begin
  case AValue.Kind of
    tkFloat:
      exit(AValue.asExtended);
    tkString, tkWString, tkUString, tkLString:
      exit(StrToFloat(AValue.AsString, AContext.FormatSettings));
    tkInteger, tkInt64:
      exit(AValue.AsInt64);
  else
    exit(0);
  end;
end;

function AsInt(const AValue: TValue; const AContext: ITemplateContext): int64;
begin
  exit(floor(AsNum(AValue, AContext)));
end;

function IsBool(const AValue: TValue): boolean;
begin
  exit(AValue.TypeInfo = TypeInfo(boolean));
end;

function isEqual(const ALeft: TValue; const ARight: TValue; const AContext: ITemplateContext): boolean;
begin
  if IsNumLike(ALeft) and IsNumLike(ARight) then
    exit(SameValue(AsNum(ALeft, AContext), AsNum(ARight, AContext)));
  if IsStrLike(ALeft) and IsStrLike(ARight) then
    exit(ALeft.AsString = ARight.AsString);
  if IsBool(ALeft) and IsBool(ARight) then
    exit(ALeft.AsBoolean = ARight.AsBoolean);
  exit(false);
end;

function isLessThan(const ALeft: TValue; const ARight: TValue; const AContext: ITemplateContext): boolean;
begin
  if IsNumLike(ALeft) and IsNumLike(ARight) then
    exit(AsNum(ALeft, AContext) < AsNum(ARight, AContext));
  if IsStrLike(ALeft) and IsStrLike(ARight) then
    exit(ALeft.AsString < ARight.AsString);
  if IsBool(ALeft) and IsBool(ARight) then
    exit(ALeft.AsBoolean < ARight.AsBoolean);
  exit(false);
end;

function isGreaterThan(const ALeft: TValue; const ARight: TValue; const AContext: ITemplateContext): boolean;
begin
  if IsNumLike(ALeft) and IsNumLike(ARight) then
    exit(AsNum(ALeft, AContext) > AsNum(ARight, AContext));
  if IsStrLike(ALeft) and IsStrLike(ARight) then
    exit(ALeft.AsString > ARight.AsString);
  if IsBool(ALeft) and IsBool(ARight) then
    exit(ALeft.AsBoolean > ARight.AsBoolean);
  exit(false);
end;

function AsBoolean(const AValue: TValue): boolean;
begin
  if AValue.TypeInfo = TypeInfo(boolean) then
    exit(AValue.AsBoolean);
  if AValue.IsEmpty then
    exit(false);
  case AValue.Kind of
    tkEnumeration:
      exit(AValue.AsOrdinal <> 0);
    tkClass:
      exit(AValue.AsObject <> nil);
    tkInteger, tkInt64:
      exit(AValue.AsInt64 <> 0);
    tkFloat:
      exit(AValue.asExtended <> 0);
    tkString, tkWString, tkUString, tkLString:
      exit(AValue.AsString.ToLower = 'true');
    tkDynArray:
      exit(AValue.GetArrayLength > 0);
  else
    exit(true);
  end;
end;

function ArrayAsString(const AValue: TValue; const AContext: ITemplateContext): string;
var
  LStringBuilder: TStringBuilder;
  LIndex: integer;
begin
  LStringBuilder := TStringBuilder.Create;
  LStringBuilder.append('[');
  try
    for LIndex := 0 to AValue.GetArrayLength - 1 do
    begin
      if LIndex > 0 then
        LStringBuilder.append(',');
      LStringBuilder.append(AsString(AValue.GetArrayElement(LIndex), AContext));
    end;
    LStringBuilder.append(']');
    exit(LStringBuilder.ToString);
  finally
    LStringBuilder.Free;
  end;
end;

function AsString(const AValue: TValue; const AContext: ITemplateContext): string;

// Using this is a workaround for backward compatability casting
  function DoubleToDT(const AValue: double): TDateTime; inline;
  begin
    exit(TDateTime(AValue));
  end;

begin
  if AValue.TypeInfo = TypeInfo(boolean) then
    if AValue.AsBoolean then
      exit('true')
    else
      exit('false');
  if AValue.IsEmpty then
    exit('');
  case AValue.Kind of
    tkInteger, tkInt64:
      exit(inttostr(AValue.AsInt64));
    tkFloat:
      if AValue.TypeInfo = TypeInfo(TDateTime) then
        exit(DateTimeToStr(DoubleToDT(AValue.asExtended), AContext.FormatSettings))
      else
        exit(FloatToStr(AValue.asExtended, AContext.FormatSettings));
    tkString, tkWString, tkUString, tkLString:
      exit(AValue.AsString);
    tkDynArray, tkArray:
      exit(ArrayAsString(AValue, AContext));
    tkRecord {$IFDEF SUPPORT_CUSTOM_MANAGED_RECORDS}, tkMRecord{$ENDIF}:
      if AValue.TypeInfo = TypeInfo(TValue) then
        exit(AsString(AValue.AsType<TValue>(), AContext))
      else
        exit(AContext.RttiContext.GetType(AValue.TypeInfo).Name);
  else
    exit('');
  end;
end;

function AsDateTime(const AValue: TValue): TDateTime;

// Using this is a workaround for backward compatability casting
  function DoubleToDT(const AValue: double): TDateTime; inline;
  begin
    exit(TDateTime(AValue));
  end;

begin
  case AValue.Kind of
    tkInteger, tkInt64:
      exit(DoubleToDT(AValue.AsInt64));
    tkFloat:
      exit(DoubleToDT(AValue.asExtended));
    tkString, tkWString, tkUString, tkLString:
      exit(StrToDateTime(AValue.AsString));
  else
    exit(now);
  end;
end;

function TryDerefTemplateVariables(const APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LObjectType: TRttiType;
  LGetItemMethod: TRttiMethod;
  LContainsKeyMethod: TRttiMethod;
begin
  // Ideally wuld like to use TryGetItem, but couldn't get it working
  // with TValue result...
  LObjectType := AContext.RttiContext.GetType(AObj.TypeInfo);
  LContainsKeyMethod := LObjectType.GetMethod('ContainsKey');
  try
    if not LContainsKeyMethod.Invoke(AObj, [ADeref]).AsBoolean then
      exit(ExitEmpty(AResult));
  except
    // TODO: log
    exit(ExitEmpty(AResult));
  end;
  LGetItemMethod := LObjectType.GetMethod('GetItem');
  try
    AResult := LGetItemMethod.Invoke(AObj, [ADeref]);
    exit(true);
  except
    // TODO: log
    exit(ExitEmpty(AResult));
  end;
end;

function TryGetFieldOrProperty(const APtr: pointer; ADerefType: TRttiType; const ADeref: TValue; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LDerefField: TRttiField;
  LDerefProp: TRttiProperty;
  LDerefFieldName: string;
begin
  LDerefFieldName := AsString(ADeref, AContext);
  // first try check if there is a field
  LDerefField := ADerefType.GetField(LDerefFieldName);
  if LDerefField <> nil then
  begin
    AResult := LDerefField.GetValue(APtr);
    exit(true);
  end;
  // next check if it is a property
  LDerefProp := ADerefType.GetProperty(LDerefFieldName);
  if LDerefProp <> nil then
  begin
    AResult := LDerefProp.GetValue(APtr);
    exit(true);
  end;
  exit(ExitEmpty(AResult));
end;

function ExitValue(const AValue: TValue; var AResult: TValue): boolean;
begin
  AResult := AValue;
  exit(true);
end;

function TryDeref(const APosition: IPosition; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean; overload;
var
  LPair: TPair<TDerefMatchFunction, TDerefWrappedValue>;
  LClassType: TClass;
  LClassInfo: PTypeInfo;
  LObject: TObject;
begin
  if not ADeref.IsObject then
    exit(ExitValue(ADeref, AResult));
  LObject := ADeref.AsObject;
  if LObject = nil then
    exit(ExitValue(ADeref, AResult));
  LClassType := LObject.ClassType;
  LClassInfo := ADeref.TypeInfo;
  for LPair in GDerefWrappedValues do
  begin
    if not LPair.Key(AContext, LClassInfo, LClassType) then
      continue;
    if LPair.Value(APosition, ADeref, ARaiseIfMissing, AContext, AResult) then
      exit(true);
  end;
  exit(ExitValue(ADeref, AResult));
end;

function TryDerefClass(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AOwnObject: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LClassType: TClass;
  LClassInfo: PTypeInfo;
  LFuncPair: TPair<TDerefMatchFunction, TDerefFunction>;
begin
  LClassType := obj.AsObject.ClassType;
  LClassInfo := obj.TypeInfo;
  if not AOwnObject then
  begin
    for LFuncPair in GDerefFunctions do
    begin
      if LFuncPair.Key(AContext, LClassInfo, LClassType) then
      begin
        if LFuncPair.Value(APosition, obj, ADeref, ARaiseIfMissing, AContext, AResult) then
          exit(true)
      end;
    end;
  end;
  exit(TryGetFieldOrProperty(obj.AsObject, AContext.RttiContext.GetType(obj.TypeInfo), ADeref, AContext, AResult));
end;

function TryDerefList(const APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LIndexedProperty: TRttiIndexedProperty;
begin
  LType := AContext.RttiContext.GetType(AObj.TypeInfo);
  try
    if IsIntLike(ADeref) then
    begin
      LIndexedProperty := LType.GetIndexedProperty('Items');
      if LIndexedProperty <> nil then
      begin
        AResult := LIndexedProperty.GetValue(AObj.AsObject, [AsInt(ADeref, AContext)]);
        exit(true);
      end;
    end
    else
    begin
      LProperty := LType.GetProperty(AsString(ADeref, AContext));
      if LProperty <> nil then
      begin
        AResult := LProperty.GetValue(AObj.AsObject);
        exit(true);
      end;
    end;
  except
    on e: Exception do
    begin
      if not TryDerefClass(APosition, AObj, ADeref, ARaiseIfMissing, true, AContext, AResult) then
      begin
        if ARaiseIfMissing then
          RaiseError(APosition, SCannotDereferenceValueOnObject, [AsString(ADeref, AContext), LType.QualifiedName]);
      end;
    end;
  end;
  exit(ExitEmpty(AResult));
end;

function TryDerefDictionary(const APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LType: TRttiType;
  LProperty: TRttiProperty;
  LIndexedProperty: TRttiIndexedProperty;
  LDerefValue: TValue;
begin
  LType := AContext.RttiContext.GetType(AObj.TypeInfo);
  try
    if IsStrLike(ADeref) then
    begin
      // lets check if it is a property on the class
      LProperty := LType.GetProperty(AsString(ADeref, AContext));
      if LProperty <> nil then
      begin
        AResult := LProperty.GetValue(AObj.AsObject);
        exit(true);
      end;
    end;

    LIndexedProperty := LType.GetIndexedProperty('Items');
    if LIndexedProperty <> nil then
    begin
      // values are sometime floats, so cast explicitly
      if IsIntLike(ADeref) then
        LDerefValue := AsInt(ADeref, AContext)
      else
        LDerefValue := ADeref;
      AResult := LIndexedProperty.GetValue(AObj.AsObject, [LDerefValue]);
      exit(true);
    end;
  except
    on e: Exception do
    begin
      if not TryDerefClass(APosition, AObj, ADeref, ARaiseIfMissing, true, AContext, AResult) then
      begin
        if ARaiseIfMissing then
          RaiseError(APosition, SCannotDereferenceValueOnObject, [AsString(LDerefValue, AContext), LType.QualifiedName]);
      end;
    end;
  end;
  exit(ExitEmpty(AResult));
end;

function TryJsonValueToTValue(const AJsonValue: TJsonValue; out AValue: TValue): boolean;
begin
{$IFDEF SUPPORT_JSON_BOOL}
  if AJsonValue is TJSONBool then
  begin
    AValue := TJSONBool(AJsonValue).AsBoolean;
    exit(true);
  end;
{$ELSE}
  if AJsonValue is TJSONTrue then
  begin
    AValue := true;
    exit(true);
  end;
  if AJsonValue is TJSONFalse then
  begin
    AValue := false;
    exit(true);
  end;
{$ENDIF}
  if AJsonValue is TJSONString then
  begin
    AValue := TJSONString(AJsonValue).Value;
    exit(true);
  end;
  if AJsonValue is TJSONNumber then
  begin
    AValue := TJSONNumber(AJsonValue).AsDouble;
    exit(true);
  end;
  if AJsonValue is TJsonObject then
  begin
    AValue := TJsonObject(AJsonValue);
    exit(true);
  end;
  if AJsonValue is TJSONNull then
  begin
    AValue := nil;
    exit(true);
  end;
  AValue := AJsonValue;
  exit(true);
end;

procedure PopulateStackFrameFromJsonObject(const AContext: ITemplateContext; const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
var
  LJsonObject: TJsonObject;
  LJsonPair: TJsonPair;
  LJsonValueAsTValue: TValue;
begin
  LJsonObject := TJsonObject(AClass.AsObject);
  for LJsonPair in LJsonObject do
  begin
    if TryJsonValueToTValue(LJsonPair.JsonValue, LJsonValueAsTValue) then
      StackFrame[LJsonPair.JsonString.Value] := LJsonValueAsTValue;
  end;
end;

function TryDerefJsonObject(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LJsonObject: TJsonObject;
  LJsonKey: string;
  LJsonKeyVal: TJsonValue;
  LJsonPair: TJsonPair;
begin
  LJsonObject := obj.AsObject as TJsonObject;
  LJsonKey := AsString(ADeref, AContext);
  LJsonPair := LJsonObject.Get(LJsonKey);
  if LJsonPair <> nil then
  begin
    LJsonKeyVal := LJsonPair.JsonValue;
    if TryJsonValueToTValue(LJsonKeyVal, AResult) then
      exit(true);
  end;
  if ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValueOnObject, [LJsonKey, 'TJsonObject']);
  exit(ExitEmpty(AResult));
end;

function TryDerefJsonArray(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LJsonArray: TJsonArray;
  LJsonKey: integer;
  LJsonKeyVal: TJsonValue;
begin
  LJsonArray := obj.AsObject as TJsonArray;
  LJsonKey := AsInt(ADeref, AContext);
  LJsonKeyVal := LJsonArray.Items[LJsonKey];
  if TryJsonValueToTValue(LJsonKeyVal, AResult) then
    exit(true);
  if ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValueOnArray, [LJsonKey, 'TJsonArray']);
  exit(ExitEmpty(AResult));
end;

function TryDerefJsonValue(const APosition: IPosition; const obj: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LObj: TJsonValue;
begin
  LObj := obj.AsObject as TJsonValue;
  if TryJsonValueToTValue(LObj, AResult) then
    exit(true);
  if ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValiable);
  exit(ExitEmpty(AResult));
end;

function MatchJsonObject(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass = TJsonObject);
end;

function TryDerefMap(const APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LKey: string;
  LMap: TMap;
begin
  LKey := AsString(ADeref, AContext);
  if not ADeref.IsType<string> then
    RaiseError(APosition, SCannotDereferenceValueOnObject, [LKey, SDataSet]);
  LMap := AObj.AsType<TMap>;
  if LMap.TryGetValue(LKey, AResult) then
    exit(true);
  if ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValueOnObject, [LKey, SDataSet]);
  exit(ExitEmpty(AResult));
end;

function TryDerefMapExpr(const APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
begin
  exit(TryDerefMap(APosition, TValue.From(AObj.AsType<IMapExpr>.GetMap), ADeref, ARaiseIfMissing, AContext, AResult));
end;

function TryDerefDataSet(const APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;
var
  LDataSet: TDataSet;
  LKey: string;
begin
  LDataSet := AObj.AsObject as TDataSet;
  LKey := AsString(ADeref, AContext);
  try
    AResult := TValue.FromVariant(LDataSet.FieldByName(LKey).AsVariant);
    exit(true);
  except
    on e: Exception do
      if ARaiseIfMissing then
        RaiseError(APosition, SCannotDereferenceValueOnObject, [LKey, SDataSet]);
  end;
  exit(ExitEmpty(AResult));
end;

function Deref(const APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext): TValue;
begin
  if not TryDeref(APosition, AVar, ADeref, ARaiseIfMissing, AContext, result) then
    exit('');
end;

function Deref(const APosition: IPosition; const ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext): TValue; overload;
begin
  if not TryDeref(APosition, ADeref, ARaiseIfMissing, AContext, result) then
    exit('');
end;

function TryDeref(const APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AResult: TValue): boolean;

  function TryDerefArray(AObj: TValue; const ADeref: TValue; out AResult: TValue): boolean;
  var
    LIndex: int64;
    LElementType: TRttiArrayType;
    LArrayDimType: TRttiType;
    LArrayOrdType: TRttiOrdinalType;
    LMin: int64;
    LMax: int64;
  begin
    LIndex := AsInt(ADeref, AContext);
    LElementType := AContext.RttiContext.GetType(AObj.TypeInfo) as TRttiArrayType;
    LArrayDimType := LElementType.Dimensions[0];
    LMin := 0;
    if LArrayDimType <> nil then
    begin
      LArrayOrdType := LArrayDimType as TRttiOrdinalType;
      LMin := LArrayOrdType.MinValue;
      LMax := LArrayOrdType.MaxValue;
      if (LIndex < LMin) or (LIndex > LMax) then
        RaiseErrorRes(APosition, @SIndexOutOfBounds);
    end;
    AResult := AObj.GetArrayElement(LIndex - LMin);
    exit(true);
  end;

  function TryDerefDynArray(AObj: TValue; const ADeref: TValue; out AResult: TValue): boolean;
  var
    LIndex: integer;
  begin
    LIndex := AsInt(ADeref, AContext);
    if (LIndex < 0) or (LIndex >= AObj.GetArrayLength) then
    begin
      AResult := TValue.Empty;
      exit(false);
    end;
    AResult := AObj.GetArrayElement(LIndex);
    exit(true);
  end;

  function TryDerefRecord(const AObj: TValue; const ADeref: TValue; out AResult: TValue): boolean;
  var
    LObj: TValue;
  begin
    LObj := AObj;
    if MatchMap(AContext, LObj.TypeInfo) then
    begin
      if TryDerefMap(APosition, LObj, ADeref, ARaiseIfMissing, AContext, AResult) then
        exit(true);
    end;

    exit(TryGetFieldOrProperty(LObj.GetReferenceToRawData, AContext.RttiContext.GetType(AObj.TypeInfo), ADeref, AContext, AResult));
  end;

  function TryDerefInterface(const AObj: TValue; const ADeref: TValue; out AResult: TValue): boolean;
  var
    LFunctionPair: TPair<TDerefMatchInterfaceFunction, TDerefFunction>;
    LIntf: IInterface;
  begin
    LIntf := AObj.AsInterface;
    for LFunctionPair in GDerefInterfaceFunctions do
    begin
      if LFunctionPair.Key(LIntf) then
      begin
        if LFunctionPair.Value(APosition, AObj, ADeref, ARaiseIfMissing, AContext, AResult) then
          exit(true);
      end;
    end;
    exit(ExitEmpty(AResult));
  end;

  function FixTValue(var AValue: TValue; const AResult:boolean=true): boolean;
  begin
    if (AValue.Kind = tkRecord) and MatchValue(AValue.TypeInfo) then
    begin
      AValue := AValue.AsType<TValue>;
    end;
    exit(AResult);
  end;

var
  LVar: TValue;
begin
  LVar := AVar;

  if LVar.IsEmpty then
  begin
    exit(FixTValue(LVar, false));
  end;

  case LVar.Kind of
    tkInterface:
      if TryDerefInterface(LVar, ADeref, AResult) then
        exit(FixTValue(AResult));
    tkClass:
      if TryDerefClass(APosition, LVar, ADeref, ARaiseIfMissing, false, AContext, AResult) then
        exit(FixTValue(AResult));
    tkRecord {$IFDEF SUPPORT_CUSTOM_MANAGED_RECORDS}, tkMRecord{$ENDIF}:
      if TryDerefRecord(LVar, ADeref, AResult) then
        exit(FixTValue(AResult));
    tkArray:
      if TryDerefArray(LVar, ADeref, AResult) then
        exit(FixTValue(AResult));
    tkDynArray:
      if TryDerefDynArray(LVar, ADeref, AResult) then
        exit(FixTValue(AResult));
  end;
  if ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValiable);
  exit(ExitEmpty(AResult));
end;

procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue); overload;
begin
  if IsBool(ALeft) then
    exit;
  RaiseError(APositional, SBooleanTypeExpected);
end;

procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
begin
  if IsBool(ALeft) and IsBool(ARight) then
    exit;
  RaiseError(APositional, SBooleanTypeExpected);
end;

procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue); overload;
begin
  if IsNumLike(ALeft) then
    exit;
  RaiseError(APositional, SNumericTypeExpected);
end;

procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
begin
  if IsNumLike(ALeft) and IsNumLike(ARight) then
    exit;
  RaiseError(APositional, SNumericTypeExpected);
end;

procedure AssertString(const APositional: IPosition; const AValue: TValue);
begin
  if IsStrLike(AValue) then
    exit;
  RaiseError(APositional, SStringTypeExpected);
end;

procedure AssertArray(const AContext: ITemplateContext; const APositional: IPosition; const AValue: TValue);
begin
  if IsEnumerable(AContext, AValue) then
    exit;
  RaiseError(APositional, SEnumerableTypeExpected);
end;

function MatchQueue(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TQueue') or LClassName.StartsWith('System.Generics.Collections.TObjectQueue'));
end;

function MatchStack(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TStack') or LClassName.StartsWith('System.Generics.Collections.TObjectStack'));
end;

function MatchList(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TList') or LClassName.StartsWith('System.Generics.Collections.TObjectList'));
end;

function MatchJsonArray(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass = TJsonArray);
end;

function MatchJsonValue(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit( //
    (ATypeInfo = TypeInfo(TJSONString)) or //
    (ATypeInfo = TypeInfo(TJSONNull)) or //
    (ATypeInfo = TypeInfo(TJSONNumber)) or //
    (ATypeInfo = TypeInfo(TJSONString)) or //
    (ATypeInfo = TypeInfo(TJSONBool)) or //
    (ATypeInfo = TypeInfo(TJSONTrue)) or //
    (ATypeInfo = TypeInfo(TJSONFalse)) //
    );
end;

function MatchDictionary(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TDictionary') or LClassName.StartsWith('System.Generics.Collections.TObjectDictionary'));
end;

function MatchMapExpr(const AInterface: IInterface): boolean;
begin
  exit(supports(AInterface, IMapExpr));
end;

function MatchMap(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo): boolean;
begin
  exit(ATypeInfo.Name = 'TMap');
end;

function MatchMap(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(MatchMap(AContext, ATypeInfo));
end;

function MatchValue(const ATypeInfo: PTypeInfo): boolean;
begin
  exit(ATypeInfo.Name = 'TValue');
end;

function MatchMap(const AContext: ITemplateContext; const AValue: TValue): boolean;
begin
  exit(MatchMap(AContext, AValue.TypeInfo) or AValue.IsType<IMapExpr>);
end;

function MatchStringDictionary(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TDictionary<System.string,') or LClassName.StartsWith('System.Generics.Collections.TObjectDictionary<string,'));
end;

function MatchDataSet(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass.InheritsFrom(TDataSet));
end;

procedure PopulateStackFrameFromDictionary(const AContext: ITemplateContext; const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
var
  LEnumObject: TObject;
  LEnumType: TRttiType;
  LMethod: TRttiMethod;
  LEnumMoveNextMethod: TRttiMethod;
  LEnumCurrentProperty: TRttiProperty;
  LPairKeyField, LPairValueField: TRttiField;
  LEnumPair: TValue;
  LPairRecordPtr: pointer;
  LDictionary: TObject;

begin
  LDictionary := AClass.AsObject;
  LMethod := ARttiType.GetMethod('GetEnumerator');
  LEnumObject := LMethod.Invoke(LDictionary, []).AsObject;
  try
    LEnumType := AContext.RttiContext.GetType(LEnumObject.ClassType);
    LEnumMoveNextMethod := LEnumType.GetMethod('MoveNext');
    LEnumCurrentProperty := LEnumType.GetProperty('Current');
    LPairKeyField := nil;
    LPairValueField := nil;
    while LEnumMoveNextMethod.Invoke(LEnumObject, []).AsBoolean do
    begin
      LEnumPair := LEnumCurrentProperty.GetValue(LEnumObject); // this returns TPair record
      LPairRecordPtr := LEnumPair.GetReferenceToRawData;
      if LPairKeyField = nil then
      begin
        LEnumType := AContext.RttiContext.GetType(LEnumPair.TypeInfo);
        LPairKeyField := LEnumType.GetField('Key');
        LPairValueField := LEnumType.GetField('Value');
      end;
      StackFrame[LPairKeyField.GetValue(LPairRecordPtr).AsString] := LPairValueField.GetValue(LPairRecordPtr);
    end;
  finally
    LEnumObject.Free;
  end;
end;

function MatchTemplateVariables(const AInterface: IInterface): boolean;
begin
  exit(supports(AInterface, ITemplateVariables));
end;

function MatchTemplateVariableObject(const AContext: ITemplateContext; const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass = TTemplateVariables);
end;

function IsDataSetEmpty(const AContext: ITemplateContext; const AObject: TObject): boolean;
var
  LType: TRttiType;
  LCount: integer;
  LCountProp: TRttiProperty;
begin
  if AObject = nil then
    exit(true);
  LType := AContext.RttiContext.GetType(AObject.ClassType);
  LCountProp := LType.GetProperty('RecordCount');
  LCount := LCountProp.GetValue(AObject).AsInteger;
  exit(LCount = 0);
end;

function IsRecordCountEmpty(const AContext: ITemplateContext; const AObject: TValue): boolean;
var
  LType: TRttiType;
  LCount: integer;
  LCountProp: TRttiMethod;
begin
  LType := AContext.RttiContext.GetType(AObject.TypeInfo);
  LCountProp := LType.GetMethod('GetCount');
  LCount := LCountProp.Invoke(AObject, []).AsInteger;
  exit(LCount = 0);
end;

function IsGenericCollectionEmpty(const AContext: ITemplateContext; const AObject: TObject): boolean;
var
  LType: TRttiType;
  LCount: integer;
  LCountProp: TRttiProperty;
begin
  if AObject = nil then
    exit(true);
  LType := AContext.RttiContext.GetType(AObject.ClassType);
  LCountProp := LType.GetProperty('Count');
  LCount := LCountProp.GetValue(AObject).AsInteger;
  exit(LCount = 0);
end;

function IsJsonObjectEmpty(const AContext: ITemplateContext; const AObject: TObject): boolean;
var
  LJsonArray: TJsonArray absolute AObject;
  LJsonObject: TJsonObject absolute AObject;
begin
  if AObject = nil then
    exit(true);
  if AObject is TJsonArray then
  begin
{$IFDEF SUPPORT_JSON_DBX}
    exit(LJsonArray.Size = 0);
{$ELSE}
    exit(LJsonArray.Count = 0);
{$ENDIF}
  end
  else if AObject is TJsonObject then
  begin
{$IFDEF SUPPORT_JSON_DBX}
    exit(LJsonArray.Size = 0);
{$ELSE}
    exit(LJsonArray.Count = 0);
{$ENDIF}
  end;
  exit(false);
end;

function IsEmptyValue(const AContext: ITemplateContext; const AValue: TValue): boolean;
begin
  if MatchMap(AContext, AValue.TypeInfo) then
    exit(IsRecordCountEmpty(AContext, AValue))
  else
    exit(false);
end;

function IsEmptyObject(const AContext: ITemplateContext; const AObject: TObject): boolean;
var
  LPair: TPair<TDerefMatchFunction, TCheckEmptyObjectFunction>;
begin
  if AObject = nil then
    exit(true);
  for LPair in GEmptyObjectFunctions do
  begin
    if LPair.Key(AContext, AObject.classinfo, AObject.ClassType) then
    begin
      exit(LPair.Value(AContext, AObject));
    end;
  end;
  exit(false);
end;

initialization

GRttiContext := TRttiContext.Create;

GDerefInterfaceFunctions := TList < TPair < TDerefMatchInterfaceFunction, TDerefFunction >>.Create;
GDerefFunctions := TList < TPair < TDerefMatchFunction, TDerefFunction >>.Create;
GPopulateFunctions := TList < TPair < TPopulateMatchFunction, TPopulateStackFrame >>.Create;
GEmptyObjectFunctions := TList < TPair < TDerefMatchFunction, TCheckEmptyObjectFunction >>.Create;
GDerefWrappedValues := TList < TPair < TDerefMatchFunction, TDerefWrappedValue >>.Create;

RegisterDeref(MatchJsonArray, TryDerefJsonArray);
RegisterDeref(MatchJsonObject, TryDerefJsonObject);
RegisterDerefWrappedValue(MatchJsonValue, TryDerefJsonValue);

RegisterDeref(MatchTemplateVariableObject, TryDerefTemplateVariables);

RegisterDeref(MatchList, TryDerefList);
RegisterDeref(MatchDictionary, TryDerefDictionary);
RegisterDeref(MatchTemplateVariables, TryDerefTemplateVariables);
RegisterDeref(MatchDataSet, TryDerefDataSet);
RegisterDeref(MatchMap, TryDerefMap);
RegisterDeref(MatchMapExpr, TryDerefMapExpr);

RegisterPopulateStackFrame(MatchJsonObject, PopulateStackFrameFromJsonObject);
RegisterPopulateStackFrame(MatchStringDictionary, PopulateStackFrameFromDictionary);

RegisterEmptyObjectCheck(MatchDictionary, IsGenericCollectionEmpty);
RegisterEmptyObjectCheck(MatchList, IsGenericCollectionEmpty);
RegisterEmptyObjectCheck(MatchQueue, IsGenericCollectionEmpty);
RegisterEmptyObjectCheck(MatchStack, IsGenericCollectionEmpty);

RegisterEmptyObjectCheck(MatchJsonArray, IsJsonObjectEmpty);
RegisterEmptyObjectCheck(MatchJsonObject, IsJsonObjectEmpty);
RegisterEmptyObjectCheck(MatchDataSet, IsDataSetEmpty);

finalization

GRttiContext.Free;

GDerefFunctions.Free;
GDerefInterfaceFunctions.Free;
GPopulateFunctions.Free;
GEmptyObjectFunctions.Free;
GDerefWrappedValues.Free;

end.
