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
unit Sempare.Template.Rtti;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  Sempare.Template.StackFrame,
  Sempare.Template.AST;

type
  ETemplateRTTI = class(ETemplate);

function AsBoolean(const AValue: TValue): boolean;
function AsString(const AValue: TValue): string;
function AsNum(const AValue: TValue): extended;
function AsDateTime(const AValue: TValue): TDateTime;
function AsInt(const AValue: TValue): int64;

function isBool(const AValue: TValue): boolean;
function isStrLike(const AValue: TValue): boolean;
function isIntLike(const AValue: TValue): boolean;
function isNumLike(const AValue: TValue): boolean;
function isNull(const AValue: TValue): boolean;
function isEnumerable(const AValue: TValue): boolean;
function Contains(APosition: IPosition; const ALeft, ARight: TValue): boolean;

function isEqual(const ALeft: TValue; const ARight: TValue): boolean;
function isLessThan(const ALeft: TValue; const ARight: TValue): boolean;
function isGreaterThan(const ALeft: TValue; const ARight: TValue): boolean;

procedure AssertBoolean(APositional: IPosition; const ALeft: TValue); overload;
procedure AssertBoolean(APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;

procedure AssertNumeric(APositional: IPosition; const ALeft: TValue); overload;
procedure AssertNumeric(APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
procedure AssertString(APositional: IPosition; const AValue: TValue);
procedure AssertArray(APositional: IPosition; const AValue: TValue);

function Deref(APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

type
  TDerefMatchFunction = function(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
  TDerefMatchInterfaceFunction = function(AInterface: IInterface): boolean;
  TDerefFunction = function(APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
  TPopulateStackFrame = procedure(const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
  TPopulateMatchFunction = function(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;

procedure RegisterDeref(const AMatch: TDerefMatchFunction; const AFunction: TDerefFunction); overload;
procedure RegisterDeref(const AMatch: TDerefMatchInterfaceFunction; const AFunction: TDerefFunction); overload;
procedure RegisterPopulateStackFrame(const AMatch: TPopulateMatchFunction; const APopulator: TPopulateStackFrame); overload;

function PopulateStackFrame(const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue): boolean;

var
  EQUALITY_PRECISION: extended = 1E-8;
  GRttiContext: TRttiContext;

implementation

uses
{$IFDEF SUPPORT_JSON_DBX}
  Data.DBXJSON,
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

const
  INT_LIKE: set of TTypeKind = [tkInteger, tkInt64];
  NUMBER_LIKE: set of TTypeKind = [tkInteger, tkInt64, tkFloat];
  STR_LIKE: set of TTypeKind = [tkString, tkWString, tkUString, tkLString];

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

function PopulateStackFrame(const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue): boolean;
var
  LFuncPair: TPair<TPopulateMatchFunction, TPopulateStackFrame>;
begin
  for LFuncPair in GPopulateFunctions do
  begin
    if LFuncPair.Key(ARttiType.Handle, ARttiType.ClassType) then
    begin
      LFuncPair.Value(StackFrame, ARttiType, AClass);
      exit(true);
    end;
  end;
  exit(false);
end;

function isNull(const AValue: TValue): boolean;
begin
  exit(AValue.IsEmpty);
end;

function Contains(APosition: IPosition; const ALeft, ARight: TValue): boolean;
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
      TRightType := GRttiContext.GetType(LEnumObject.ClassType);
      LEnumMoveNextMethod := TRightType.GetMethod('MoveNext');
      LEnumCurrentProperty := TRightType.GetProperty('Current');
      while LEnumMoveNextMethod.Invoke(LEnumObject, []).AsBoolean do
      begin
        LValue := LEnumCurrentProperty.GetValue(LEnumObject);
        if LValue.TypeInfo = TypeInfo(TValue) then
          LValue := LValue.Astype<TValue>();
        if isEqual(ALeft, LValue) then
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
        LElementValue := LElementValue.Astype<TValue>();
      if isEqual(ALeft, LElementValue) then
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
        LElementValue := LElementValue.Astype<TValue>();
      if isEqual(ALeft, LElementValue) then
      begin
        result := true;
        exit;
      end;
    end;
  end;

begin
  result := false;
  if not isEnumerable(ARight) then
    RaiseError(APosition, SValueIsNotEnumerable);

  TRightType := GRttiContext.GetType(ARight.TypeInfo);

  case TRightType.TypeKind of
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

function isEnumerable(const AValue: TValue): boolean;

begin
  case AValue.Kind of
    tkDynArray, tkArray:
      exit(true);
    tkClass, tkClassRef:
      exit(GRttiContext.GetType(AValue.TypeInfo).GetMethod('GetEnumerator') <> nil);
  else
    exit(false);
  end;
end;

function isStrLike(const AValue: TValue): boolean;
begin
  exit(AValue.Kind in STR_LIKE);
end;

function isIntLike(const AValue: TValue): boolean;
begin
  exit(AValue.Kind in INT_LIKE);
end;

function isNumLike(const AValue: TValue): boolean;
begin
  exit(AValue.Kind in NUMBER_LIKE);
end;

function AsNum(const AValue: TValue): extended;
begin
  case AValue.Kind of
    tkFloat:
      exit(AValue.AsExtended);
    tkString, tkWString, tkUString, tkLString:
      exit(strtofloat(AValue.AsString));
    tkInteger, tkInt64:
      exit(AValue.AsInt64);
  else
    exit(0);
  end;
end;

function AsInt(const AValue: TValue): int64;
begin
  exit(floor(AsNum(AValue)));
end;

function isBool(const AValue: TValue): boolean;
begin
  exit(AValue.TypeInfo = TypeInfo(boolean));
end;

function isEqual(const ALeft: TValue; const ARight: TValue): boolean;
begin
  if isNumLike(ALeft) and isNumLike(ARight) then
    exit(abs(AsNum(ALeft) - AsNum(ARight)) < EQUALITY_PRECISION);
  if isStrLike(ALeft) and isStrLike(ARight) then
    exit(ALeft.AsString = ARight.AsString);
  if isBool(ALeft) and isBool(ARight) then
    exit(ALeft.AsBoolean = ARight.AsBoolean);
  exit(false);
end;

function isLessThan(const ALeft: TValue; const ARight: TValue): boolean;
begin
  if isNumLike(ALeft) and isNumLike(ARight) then
    exit(AsNum(ALeft) < AsNum(ARight));
  if isStrLike(ALeft) and isStrLike(ARight) then
    exit(ALeft.AsString < ARight.AsString);
  if isBool(ALeft) and isBool(ARight) then
    exit(ALeft.AsBoolean < ARight.AsBoolean);
  exit(false);
end;

function isGreaterThan(const ALeft: TValue; const ARight: TValue): boolean;
begin
  if isNumLike(ALeft) and isNumLike(ARight) then
    exit(AsNum(ALeft) > AsNum(ARight));
  if isStrLike(ALeft) and isStrLike(ARight) then
    exit(ALeft.AsString > ARight.AsString);
  if isBool(ALeft) and isBool(ARight) then
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
    tkInteger, tkInt64:
      exit(AValue.AsInt64 <> 0);
    tkFloat:
      exit(AValue.AsExtended <> 0);
    tkString, tkWString, tkUString, tkLString:
      exit(AValue.AsString <> '');
    tkDynArray:
      exit(AValue.GetArrayLength > 0);
  else
    exit(true);
  end;
end;

function ArrayAsString(const AValue: TValue): string;
var
  LStringBuilder: tstringbuilder;
  LIndex: integer;
begin
  LStringBuilder := tstringbuilder.Create;
  LStringBuilder.append('[');
  try
    for LIndex := 0 to AValue.GetArrayLength - 1 do
    begin
      if LIndex > 0 then
        LStringBuilder.append(',');
      LStringBuilder.append(AsString(AValue.GetArrayElement(LIndex)));
    end;
    LStringBuilder.append(']');
    exit(LStringBuilder.ToString);
  finally
    LStringBuilder.Free;
  end;
end;

function AsString(const AValue: TValue): string;

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
        exit(datetimetostr(DoubleToDT(AValue.AsExtended)))
      else
        exit(floattostr(AValue.AsExtended));
    tkString, tkWString, tkUString, tkLString:
      exit(AValue.AsString);
    tkDynArray, tkArray:
      exit(ArrayAsString(AValue));
    tkRecord:
      if AValue.TypeInfo = TypeInfo(TValue) then
        exit(AsString(AValue.Astype<TValue>()))
      else
        exit(GRttiContext.GetType(AValue.TypeInfo).Name);
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
      exit(DoubleToDT(AValue.AsExtended));
    tkString, tkWString, tkUString, tkLString:
      exit(StrToDateTime(AValue.AsString));
  else
    exit(now);
  end;
end;

function processTemplateVariables(APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  LObjectType: TRttiType;
  LGetItemMethod: TRttiMethod;
  LContainsKeyMethod: TRttiMethod;
begin
  // Ideally wuld like to use TryGetItem, but couldn't get it working
  // with TValue result...
  LObjectType := GRttiContext.GetType(AObj.TypeInfo);
  LContainsKeyMethod := LObjectType.GetMethod('ContainsKey');
  AFound := LContainsKeyMethod.Invoke(AObj, [ADeref]).AsBoolean;
  if not AFound then
    exit('');
  LGetItemMethod := LObjectType.GetMethod('GetItem');
  try
    exit(LGetItemMethod.Invoke(AObj, [ADeref]));
  except
    on e: Exception do
    begin
      AFound := false;
      exit('');
    end;
  end;
end;

function GetFieldOrProperty(const APtr: pointer; ADerefType: TRttiType; const ADeref: TValue; out AFound: boolean): TValue;
var
  LDerefField: TRttiField;
  LDerefProp: TRttiProperty;
  LDerefFieldName: string;
begin
  AFound := true;
  LDerefFieldName := AsString(ADeref);
  // first try check if there is a field
  LDerefField := ADerefType.GetField(LDerefFieldName);
  if LDerefField <> nil then
    exit(LDerefField.GetValue(APtr));
  // next check if it is a property
  LDerefProp := ADerefType.GetProperty(LDerefFieldName);
  if LDerefProp <> nil then
    exit(LDerefProp.GetValue(APtr));
  AFound := false;
  exit(TValue.Empty);
end;

function ProcessClass(APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AOwnObject: boolean; out AFound: boolean): TValue;
var
  ClassType: TClass;
  LClassInfo: PTypeInfo;
  LFuncPair: TPair<TDerefMatchFunction, TDerefFunction>;
begin
  AFound := false;
  ClassType := obj.AsObject.ClassType;
  LClassInfo := obj.TypeInfo;
  if not AOwnObject then
  begin
    for LFuncPair in GDerefFunctions do
    begin
      if LFuncPair.Key(LClassInfo, ClassType) then
      begin
        result := LFuncPair.Value(APosition, obj, ADeref, ARaiseIfMissing, AFound);
        if AFound then
          exit;
      end;
    end;
  end;
  exit(GetFieldOrProperty(obj.AsObject, GRttiContext.GetType(obj.TypeInfo), ADeref, AFound));
end;

function processDictionary(APosition: IPosition; const AObj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  LDictionaryType: TRttiType;
  LDictGetItemMethod: TRttiMethod;
  LDerefValue: TValue;
begin
  AFound := false;
  LDictionaryType := GRttiContext.GetType(AObj.TypeInfo);
  LDictGetItemMethod := LDictionaryType.GetMethod('GetItem');

  // values are sometime floats, so cast explicitly
  LDerefValue := ADeref;
  if LDictGetItemMethod.GetParameters[0].ParamType.TypeKind in [tkInteger, tkInt64] then
    LDerefValue := AsInt(LDerefValue);

  try
    result := LDictGetItemMethod.Invoke(AObj, [LDerefValue]);
    AFound := true;
  except
    on e: Exception do
    begin
      result := ProcessClass(APosition, AObj, ADeref, ARaiseIfMissing, true, AFound);
      if not AFound then
      begin
        if ARaiseIfMissing then
          RaiseError(APosition, SCannotDereferenceValueOnObject, [AsString(LDerefValue), LDictionaryType.QualifiedName]);
        exit('');
      end;
    end;
  end;
end;

function JsonValueToTValue(const AJsonValue: TJsonValue; out AValue: TValue): boolean;
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
  exit(false);
end;

procedure PopulateStackFrameFromJsonObject(const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
var
  LJsonObject: TJsonObject;
  LJsonPair: TJsonPair;
  LJsonValueAsTValue: TValue;
begin
  LJsonObject := TJsonObject(AClass.AsObject);
  for LJsonPair in LJsonObject do
  begin
    if JsonValueToTValue(LJsonPair.JsonValue, LJsonValueAsTValue) then
      StackFrame[LJsonPair.JsonString.Value] := LJsonValueAsTValue;
  end;
end;

function processJson(APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  LJsonObject: TJsonObject;
  LJsonKey: string;
  LJsonKeyVal: TJsonValue;
begin
  AFound := true;
  LJsonObject := obj.AsObject as TJsonObject;
  LJsonKey := AsString(ADeref);
  LJsonKeyVal := LJsonObject.Get(LJsonKey).JsonValue;
  if not JsonValueToTValue(LJsonKeyVal, result) then
  begin
    if ARaiseIfMissing then
      RaiseError(APosition, SCannotDereferenceValueOnObject, [LJsonKey, SDictionary]);
    AFound := false;
    exit('');
  end;
end;

function MatchJsonObject(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass = TJsonObject);
end;

function processDataSet(APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  LDataSet: TDataSet;
  LKey: string;
begin
  AFound := true;
  LDataSet := obj.AsObject as TDataSet;
  LKey := AsString(ADeref);
  try
    exit(TValue.FromVariant(LDataSet.FieldByName(LKey).AsVariant));
  except
    on e: Exception do
    begin
      if ARaiseIfMissing then
        RaiseError(APosition, SCannotDereferenceValueOnObject, [LKey, SDataSet]);
      AFound := false;
      exit('');
    end;
  end;
end;

function Deref(APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

  function ProcessArray(AObj: TValue; const ADeref: TValue; out AFound: boolean): TValue;
  var
    LIndex: int64;
    LElementType: TRttiArrayType;
    LArrayDimType: TRttiType;
    LArrayOrdType: TRttiOrdinalType;
    LMin: int64;
    LMax: int64;
  begin
    LIndex := AsInt(ADeref);
    AFound := false;
    LElementType := GRttiContext.GetType(AObj.TypeInfo) as TRttiArrayType;
    LArrayDimType := LElementType.Dimensions[0];
    LMin := 0;
    if LArrayDimType <> nil then
    begin
      LArrayOrdType := LArrayDimType as TRttiOrdinalType;
      LMin := LArrayOrdType.MinValue;
      LMax := LArrayOrdType.MaxValue;
      if (LIndex < LMin) or (LIndex > LMax) then
        raise ETemplateRTTI.Create(SIndexOutOfBounds);
    end;
    AFound := true;
    exit(AObj.GetArrayElement(LIndex - LMin));
  end;

  function ProcessDynArray(AObj: TValue; const ADeref: TValue; out AFound: boolean): TValue;
  var
    LIndex: integer;
  begin
    AFound := false;
    LIndex := AsInt(ADeref);
    if (LIndex < 0) or (LIndex >= AObj.GetArrayLength) then
      exit(TValue.Empty);
    exit(AObj.GetArrayElement(LIndex));
  end;

  function ProcessRecord(const AObj: TValue; const ADeref: TValue; var AFound: boolean): TValue;
  begin
    exit(GetFieldOrProperty(AObj.GetReferenceToRawData, GRttiContext.GetType(AObj.TypeInfo), ADeref, AFound));
  end;

  function ProcessInterface(const AObj: TValue; const ADeref: TValue; out AFound: boolean): TValue;
  var
    LFunctionPair: TPair<TDerefMatchInterfaceFunction, TDerefFunction>;
    LIntf: IInterface;
  begin
    LIntf := AObj.AsInterface;
    for LFunctionPair in GDerefInterfaceFunctions do
    begin
      if LFunctionPair.Key(LIntf) then
      begin
        result := LFunctionPair.Value(APosition, AObj, ADeref, ARaiseIfMissing, AFound);
        if AFound then
          exit(true);
      end;
    end;
    exit(false);
  end;

var
  LVarFound: boolean;

begin
  if AVar.IsEmpty then
    exit(AVar);
  case AVar.Kind of
    tkInterface:
      result := ProcessInterface(AVar, ADeref, LVarFound);
    tkClass:
      result := ProcessClass(APosition, AVar, ADeref, ARaiseIfMissing, false, LVarFound);
    tkRecord:
      result := ProcessRecord(AVar, ADeref, LVarFound);
    tkArray:
      result := ProcessArray(AVar, ADeref, LVarFound);
    tkDynArray:
      result := ProcessDynArray(AVar, ADeref, LVarFound);
  else
    begin
      if ARaiseIfMissing then
        RaiseError(APosition, SCannotDereferenceValiable);
      exit('');
    end;
  end;
  if not LVarFound and ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValiable);
end;

procedure AssertBoolean(APositional: IPosition; const ALeft: TValue); overload;
begin
  if isBool(ALeft) then
    exit;
  RaiseError(APositional, SBooleanTypeExpected);
end;

procedure AssertBoolean(APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
begin
  if isBool(ALeft) and isBool(ARight) then
    exit;
  RaiseError(APositional, SBooleanTypeExpected);
end;

procedure AssertNumeric(APositional: IPosition; const ALeft: TValue); overload;
begin
  if isNumLike(ALeft) then
    exit;
  RaiseError(APositional, SNumericTypeExpected);
end;

procedure AssertNumeric(APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
begin
  if isNumLike(ALeft) and isNumLike(ARight) then
    exit;
  RaiseError(APositional, SNumericTypeExpected);
end;

procedure AssertString(APositional: IPosition; const AValue: TValue);
begin
  if isStrLike(AValue) then
    exit;
  RaiseError(APositional, SStringTypeExpected);
end;

procedure AssertArray(APositional: IPosition; const AValue: TValue);
begin
  if isEnumerable(AValue) then
    exit;
  RaiseError(APositional, SEnumerableTypeExpected);
end;

function MatchDictionary(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TDictionary') or LClassName.StartsWith('System.Generics.Collections.TObjectDictionary'));
end;

function MatchStringDictionary(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  LClassName: string;
begin
  LClassName := AClass.QualifiedClassName;
  exit(LClassName.StartsWith('System.Generics.Collections.TDictionary<System.string,') or LClassName.StartsWith('System.Generics.Collections.TObjectDictionary<string,'));
end;

function MatchDataSet(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass.InheritsFrom(TDataSet));
end;

procedure PopulateStackFrameFromDictionary(const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
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
    LEnumType := GRttiContext.GetType(LEnumObject.ClassType);
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
        LEnumType := GRttiContext.GetType(LEnumPair.TypeInfo);
        LPairKeyField := LEnumType.GetField('Key');
        LPairValueField := LEnumType.GetField('Value');
      end;
      StackFrame[LPairKeyField.GetValue(LPairRecordPtr).AsString] := LPairValueField.GetValue(LPairRecordPtr);
    end;
  finally
    LEnumObject.Free;
  end;
end;

function MatchTemplateVariables(AInterface: IInterface): boolean;
begin
  exit(supports(AInterface, ITemplateVariables));
end;

function MatchTemplateVariableObject(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass = TTemplateVariables);
end;

initialization

GRttiContext := TRttiContext.Create;

GDerefInterfaceFunctions := TList < TPair < TDerefMatchInterfaceFunction, TDerefFunction >>.Create;
GDerefFunctions := TList < TPair < TDerefMatchFunction, TDerefFunction >>.Create;
GPopulateFunctions := TList < TPair < TPopulateMatchFunction, TPopulateStackFrame >>.Create;

RegisterDeref(MatchTemplateVariableObject, processTemplateVariables);
RegisterDeref(MatchDictionary, processDictionary);
RegisterDeref(MatchJsonObject, processJson);
RegisterPopulateStackFrame(MatchJsonObject, PopulateStackFrameFromJsonObject);
RegisterPopulateStackFrame(MatchStringDictionary, PopulateStackFrameFromDictionary);
RegisterDeref(MatchTemplateVariables, processTemplateVariables);
RegisterDeref(MatchDataSet, processDataSet);

finalization

GRttiContext.Free;

GDerefFunctions.Free;
GDerefInterfaceFunctions.Free;
GPopulateFunctions.Free;

end.
