(*%****************************************************************************
 *  ___                                             ___               _       *
 * / __|  ___   _ __    _ __   __ _   _ _   ___    | _ )  ___   ___  | |_     *
 * \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)   | _ \ / _ \ / _ \ |  _|    *
 * |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|   |___/ \___/ \___/  \__|    *
 *                     |_|                                                    *
 ******************************************************************************
 *                                                                            *
 *                        VELOCITY TEMPLATE ENGINE                            *
 *                                                                            *
 *                                                                            *
 *          https://www.github.com/sempare/sempare.boot.velocity.oss          *
 ******************************************************************************
 *                                                                            *
 * Copyright (c) 2019 Sempare Limited,                                        *
 *                    Conrad Vermeulen <conrad.vermeulen@gmail.com>           *
 *                                                                            *
 * Contact: info@sempare.ltd                                                  *
 *                                                                            *
 * Licensed under the Apache License, Version 2.0 (the "License");            *
 * you may not use this file except in compliance with the License.           *
 * You may obtain a copy of the License at                                    *
 *                                                                            *
 *   http://www.apache.org/licenses/LICENSE-2.0                               *
 *                                                                            *
 * Unless required by applicable law or agreed to in writing, software        *
 * distributed under the License is distributed on an "AS IS" BASIS,          *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   *
 * See the License for the specific language governing permissions and        *
 * limitations under the License.                                             *
 *                                                                            *
 ****************************************************************************%*)
unit Sempare.Boot.Template.Velocity.Rtti;

interface

// Moved from Sempare.Boot.Rtti to make this standalone

uses
  System.Rtti,
  System.TypInfo,
  Sempare.Boot.Template.Velocity.AST;

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
function Contains(const APosition: IPosition; const ALeft, ARight: TValue): boolean;

function isEqual(const left: TValue; const right: TValue): boolean;
function isLessThan(const left: TValue; const right: TValue): boolean;
function isGreaterThan(const left: TValue; const right: TValue): boolean;

procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue); overload;
procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;

procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue); overload;
procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
procedure AssertString(const APositional: IPosition; const AValue: TValue);
procedure AssertArray(const APositional: IPosition; const AValue: TValue);

function Deref(const AVar, ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

type
  TDerefMatchFunction = function(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
  TDerefMatchInterfaceFunction = function(const AInterface: IInterface): boolean;
  TDerefFunction = function(const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

procedure RegisterDeref(const AMatch: TDerefMatchFunction; const AFunction: TDerefFunction); overload;
procedure RegisterDeref(const AMatch: TDerefMatchInterfaceFunction; const AFunction: TDerefFunction); overload;

var
  EQUALITY_PRECISION: extended = 1E-8;
  GRttiContext: TRttiContext;

implementation

uses
  System.Math,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.Common;

var
  GDerefFunctions: TList<TPair<TDerefMatchFunction, TDerefFunction>>;
  GDerefInterfaceFunctions: TList<TPair<TDerefMatchInterfaceFunction, TDerefFunction>>;

const
  INT_LIKE: set of TTypeKind = [tkInteger, tkInt64];
  NUMBER_LIKE: set of TTypeKind = [tkInteger, tkInt64, tkfloat];
  STR_LIKE: set of TTypeKind = [tkString, tkWideString, tkUnicodeString, tkLString];

procedure RegisterDeref(const AMatch: TDerefMatchInterfaceFunction; const AFunction: TDerefFunction);
begin
  GDerefInterfaceFunctions.add(TPair<TDerefMatchInterfaceFunction, TDerefFunction>.Create(AMatch, AFunction));
end;

procedure RegisterDeref(const AMatch: TDerefMatchFunction; const AFunction: TDerefFunction);
begin
  GDerefFunctions.add(TPair<TDerefMatchFunction, TDerefFunction>.Create(AMatch, AFunction));
end;

function isNull(const AValue: TValue): boolean;
begin
  result := AValue.IsEmpty;
end;

function Contains(const APosition: IPosition; const ALeft, ARight: TValue): boolean;
var
  T: TRttiType;
  procedure visitobject;
  var
    val: TValue;
    e: TObject;
    m, movenext: TRttiMethod;
    current: TRttiProperty;
  begin
    m := T.GetMethod('GetEnumerator');
    if m = nil then
      RaiseError(APosition, 'GetEnumerator not found on object.');
    val := m.Invoke(ARight.AsObject, []).AsObject;
    if val.IsEmpty then
      raise Exception.Create('Value is not enumerable');
    e := val.AsObject;
    T := GRttiContext.GetType(e.ClassType);
    movenext := T.GetMethod('MoveNext');
    current := T.GetProperty('Current');
    result := false;
    while movenext.Invoke(e, []).AsBoolean do
    begin
      val := current.GetValue(e);
      if val.TypeInfo = TypeInfo(TValue) then
        val := val.Astype<TValue>();
      if isEqual(ALeft, val) then
      begin
        result := true;
        exit;
      end;
    end;
    exit;
  end;

  procedure visitarray;
  var
    at: TRttiArrayType;
    i: integer;
    v: TValue;
  begin
    at := T as TRttiArrayType;
    if at.DimensionCount > 1 then
      RaiseError(APosition, 'Only one dimensional arrays are supported.');
    result := false;
    for i := 0 to ARight.GetArrayLength - 1 do
    begin
      v := ARight.GetArrayElement(i);
      if v.TypeInfo = TypeInfo(TValue) then
        v := v.Astype<TValue>();
      if isEqual(ALeft, v) then
      begin
        result := true;
        exit;
      end;
    end;
  end;

  procedure visitdynarray;
  var
    i: integer;
    v: TValue;
  begin
    result := false;
    for i := 0 to ARight.GetArrayLength - 1 do
    begin
      v := ARight.GetArrayElement(i);
      if v.TypeInfo = TypeInfo(TValue) then
        v := v.Astype<TValue>();
      if isEqual(ALeft, v) then
      begin
        result := true;
        exit;
      end;
    end;
  end;

begin
  if not isEnumerable(ARight) then
    RaiseError(APosition, 'Expression must be enumerable');

  T := GRttiContext.GetType(ARight.TypeInfo);

  case T.TypeKind of
    tkClass, tkClassRef:
      visitobject;
    tkArray:
      visitarray;
    tkDynArray:
      visitdynarray;
  else
    RaiseError(APosition, 'GetEnumerator not found on object.');
  end;
end;

function isEnumerable(const AValue: TValue): boolean;

begin
  case AValue.Kind of
    tkDynArray, tkArray:
      result := true;
    tkClass, tkClassRef:
      result := GRttiContext.GetType(AValue.TypeInfo).GetMethod('GetEnumerator') <> nil;
  else
    result := false;
  end;
end;

function isStrLike(const AValue: TValue): boolean;
begin
  result := AValue.Kind in STR_LIKE;
end;

function isIntLike(const AValue: TValue): boolean;
begin
  result := AValue.Kind in INT_LIKE;
end;

function isNumLike(const AValue: TValue): boolean;
begin
  result := AValue.Kind in NUMBER_LIKE;
end;

function AsNum(const AValue: TValue): extended;
begin
  case AValue.Kind of
    tkfloat:
      result := AValue.AsExtended;
    tkString, tkWideString, tkUnicodeString, tkLString:
      result := strtofloat(AValue.AsString);
    tkInteger, tkInt64:
      result := AValue.AsInt64;
  else
    result := 0;
  end;
end;

function AsInt(const AValue: TValue): int64;
begin
  result := floor(AsNum(AValue));
end;

function isBool(const AValue: TValue): boolean;
begin
  result := AValue.TypeInfo = TypeInfo(boolean);
end;

function isEqual(const left: TValue; const right: TValue): boolean;
begin
  if isNumLike(left) and isNumLike(right) then
    exit(abs(AsNum(left) - AsNum(right)) < EQUALITY_PRECISION);
  if isStrLike(left) and isStrLike(right) then
    exit(left.AsString = right.AsString);
  if isBool(left) and isBool(right) then
    exit(left.AsBoolean = right.AsBoolean);
  exit(false);
end;

function isLessThan(const left: TValue; const right: TValue): boolean;
begin
  if isNumLike(left) and isNumLike(right) then
    exit(AsNum(left) < AsNum(right));
  if isStrLike(left) and isStrLike(right) then
    exit(left.AsString < right.AsString);
  if isBool(left) and isBool(right) then
    exit(left.AsBoolean < right.AsBoolean);
  exit(false);
end;

function isGreaterThan(const left: TValue; const right: TValue): boolean;
begin
  if isNumLike(left) and isNumLike(right) then
    exit(AsNum(left) > AsNum(right));
  if isStrLike(left) and isStrLike(right) then
    exit(left.AsString > right.AsString);
  if isBool(left) and isBool(right) then
    exit(left.AsBoolean > right.AsBoolean);
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
    tkfloat:
      exit(AValue.AsExtended <> 0);
    tkString, tkWideString, tkUnicodeString, tkLString:
      exit(AValue.AsString <> '');
  else
    exit(false);
  end;

end;

function ArrayAsString(const AValue: TValue): string;
var
  s: tstringbuilder;
  i: integer;
begin
  s := tstringbuilder.Create;
  s.append('[');
  try
    for i := 0 to AValue.GetArrayLength - 1 do
    begin
      if i > 0 then
        s.append(',');
      s.append(AsString(AValue.GetArrayElement(i)));
    end;
    s.append(']');
    result := s.ToString;
  finally
    s.Free;
  end;
end;

function AsString(const AValue: TValue): string;
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
    tkfloat:
      if AValue.TypeInfo = TypeInfo(TDateTime) then
        exit(datetimetostr(TDateTime(AValue.AsExtended)))
      else
        exit(floattostr(AValue.AsExtended));
    tkString, tkWideString, tkUnicodeString, tkLString:
      exit(AValue.AsString);
    tkDynArray, tkArray:
      exit(ArrayAsString(AValue));
    tkrecord:
      if AValue.TypeInfo = TypeInfo(TValue) then
        exit(AsString(AValue.Astype<TValue>()))
      else
        exit(GRttiContext.GetType(AValue.TypeInfo).Name);
  else
    exit('');
  end;
end;

function AsDateTime(const AValue: TValue): TDateTime;
begin
  case AValue.Kind of
    tkInteger, tkInt64:
      exit(TDateTime(AValue.AsInt64));
    tkfloat:
      exit(TDateTime(AValue.AsExtended));
    tkString, tkWideString, tkUnicodeString, tkLString:
      exit(StrToDateTime(AValue.AsString));
  else
    exit(now);
  end;
end;

function processVelocityVariables(const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean): TValue;
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiType := GRttiContext.GetType(obj.TypeInfo);
  RttiMethod := RttiType.GetMethod('GetVariable');
  try
    result := RttiMethod.Invoke(obj, [ADeref]);
  except
    on e: Exception do
    begin
      if ARaiseIfMissing then
        raise e;
      result := '';
    end;
  end;
end;

function processDictionary(const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean): TValue;
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiType := GRttiContext.GetType(obj.TypeInfo);
  RttiMethod := RttiType.GetMethod('GetItem');
  try
    result := RttiMethod.Invoke(obj.AsObject, [ADeref]);
  except
    on e: Exception do
    begin
      if ARaiseIfMissing then
        raise e;
      result := '';
    end;
  end;
end;

function processJson(const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean): TValue;
var
  jsonobj: tjsonobject;
  jsonval: tjsonvalue;
  key: string;
begin
  jsonobj := obj.AsObject as tjsonobject;
  key := AsString(ADeref);
  jsonval := jsonobj.GetValue(key);
  if jsonval is TJSONBool then
  begin
    result := jsonval.Astype<boolean>();
  end
  else if jsonval is TJSONString then
  begin
    result := jsonval.Astype<string>();
  end
  else if jsonval is TJSONNumber then
  begin
    result := jsonval.Astype<extended>();
  end
  else if jsonval is tjsonobject then
  begin
    result := jsonval;
  end
  else
  begin
    if ARaiseIfMissing then
      raise Exception.CreateFmt('Cannot dereference %s in json object.', [key]);
    result := nil;
  end;
end;

function Deref(const AVar, ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

  function ProcessArray(obj: TValue; const ADeref: TValue): TValue;
  var
    i: integer;
    RttiType: TRttiArrayType;
  begin
    i := AsInt(ADeref);
    RttiType := GRttiContext.GetType(obj.TypeInfo) as TRttiArrayType;
    result := obj.GetArrayElement(i - (RttiType.Dimensions[0] as TRttiOrdinalType).MinValue);
  end;

  function ProcessDynArray(obj: TValue; const ADeref: TValue): TValue;
  var
    i: integer;
  begin
    i := AsInt(ADeref);
    result := obj.GetArrayElement(i);
  end;

  function GetFieldOrProperty(const APtr: pointer; RttiType: TRttiType; const ADeref: TValue): TValue;
  var
    RttiField: TRttiField;
    RttiProp: TRttiProperty;
    ref: string;
  begin
    ref := AsString(ADeref);
    RttiField := RttiType.GetField(ref);
    if RttiField <> nil then
      exit(RttiField.GetValue(APtr));
    RttiProp := RttiType.GetProperty(ref);
    if RttiProp <> nil then
      exit(RttiProp.GetValue(APtr));
  end;

  function ProcessClass(const obj: TValue; const ADeref: TValue): TValue;
  var
    ClassType: TClass;
    info: PTypeInfo;
    p: TPair<TDerefMatchFunction, TDerefFunction>;
  begin
    ClassType := obj.AsObject.ClassType;
    info := obj.TypeInfo;
    for p in GDerefFunctions do
    begin
      if p.key(info, ClassType) then
        exit(p.Value(obj, ADeref, ARaiseIfMissing));
    end;
    result := GetFieldOrProperty(obj.AsObject, GRttiContext.GetType(obj.TypeInfo), ADeref);
  end;

  function ProcessRecord(const obj: TValue; const ADeref: TValue): TValue;
  begin
    result := GetFieldOrProperty(obj.GetReferenceToRawData, GRttiContext.GetType(obj.TypeInfo), ADeref);
  end;

  function ProcessInterface(const obj: TValue; const ADeref: TValue): TValue;
  var
    p: TPair<TDerefMatchInterfaceFunction, TDerefFunction>;
    i: IInterface;
  begin
    i := obj.AsInterface;
    for p in GDerefInterfaceFunctions do
    begin
      if p.key(i) then
        exit(p.Value(obj, ADeref, ARaiseIfMissing));
    end;
  end;

begin
  case AVar.Kind of
    tkInterface:
      result := ProcessInterface(AVar, ADeref);
    tkClass:
      result := ProcessClass(AVar, ADeref);
    tkrecord:
      result := ProcessRecord(AVar, ADeref);
    tkArray:
      result := ProcessArray(AVar, ADeref);
    tkDynArray:
      result := ProcessDynArray(AVar, ADeref);
  else
    begin
      if ARaiseIfMissing then
        raise Exception.Create('Cannot dereference variable');
      exit('');
    end;
  end;
end;

procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue); overload;
begin
  if isBool(ALeft) then
    exit;
  RaiseError(APositional, 'Boolean type expected');
end;

procedure AssertBoolean(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
begin
  if isBool(ALeft) and isBool(ARight) then
    exit;
  RaiseError(APositional, 'Boolean types expected');
end;

procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue); overload;
begin
  if isNumLike(ALeft) then
    exit;
  RaiseError(APositional, 'Numeric type expected');
end;

procedure AssertNumeric(const APositional: IPosition; const ALeft: TValue; const ARight: TValue); overload;
begin
  if isNumLike(ALeft) and isNumLike(ARight) then
    exit;
  RaiseError(APositional, 'Numeric types expected');
end;

procedure AssertString(const APositional: IPosition; const AValue: TValue);
begin
  if isStrLike(AValue) then
    exit;
  RaiseError(APositional, 'String type expected');
end;

procedure AssertArray(const APositional: IPosition; const AValue: TValue);
begin
  if isEnumerable(AValue) then
    exit;
  RaiseError(APositional, 'Enumerable type expected');
end;

function MatchDictionary(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  result := AClass.QualifiedClassName.StartsWith('System.Generics.Collections.TDictionary');
end;

function MatchJsonObject(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  result := AClass = tjsonobject;
end;

function MatchVelocityVariables(const AInterface: IInterface): boolean;
begin
  result := supports(AInterface, IVelocityVariables);
end;

function MatchVelocityVariableObject(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  result := AClass = TVelocityVariables;
end;

initialization

GRttiContext := TRttiContext.Create;
GRttiContext.KeepContext;

GDerefInterfaceFunctions := TList < TPair < TDerefMatchInterfaceFunction, TDerefFunction >>.Create;
GDerefFunctions := TList < TPair < TDerefMatchFunction, TDerefFunction >>.Create;
RegisterDeref(MatchVelocityVariableObject, processVelocityVariables);
RegisterDeref(MatchDictionary, processDictionary);
RegisterDeref(MatchJsonObject, processJson);
RegisterDeref(MatchVelocityVariables, processVelocityVariables);

finalization

GRttiContext.DropContext;
GRttiContext.Free;

GDerefFunctions.Free;
GDerefInterfaceFunctions.Free;

end.
