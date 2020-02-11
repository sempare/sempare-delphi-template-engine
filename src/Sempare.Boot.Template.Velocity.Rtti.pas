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

{$I 'Sempare.Boot.Template.Velocity.Compiler.inc'}

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
{$IFDEF SUPPORT_JSON}
  System.JSON,
{$ENDIF}
  Sempare.Boot.Template.Velocity.StackFrame,
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

function Deref(const APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

type
  TDerefMatchFunction = function(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
  TDerefMatchInterfaceFunction = function(const AInterface: IInterface): boolean;
  TDerefFunction = function(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
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
  System.Math,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.Common;

var
  GPopulateFunctions: TList<TPair<TPopulateMatchFunction, TPopulateStackFrame>>;
  GDerefFunctions: TList<TPair<TDerefMatchFunction, TDerefFunction>>;
  GDerefInterfaceFunctions: TList<TPair<TDerefMatchInterfaceFunction, TDerefFunction>>;

const
  INT_LIKE: set of TTypeKind = [tkInteger, tkInt64];
  NUMBER_LIKE: set of TTypeKind = [tkInteger, tkInt64, tkfloat];
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
  p: TPair<TPopulateMatchFunction, TPopulateStackFrame>;
begin
  for p in GPopulateFunctions do
  begin
    if p.Key(ARttiType.Handle, ARttiType.ClassType) then
    begin
      p.Value(StackFrame, ARttiType, AClass);
      exit(true);
    end;
  end;
  result := false;
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
      RaiseError(APosition, 'Value is not enumerable');
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
    tkString, tkWString, tkUString, tkLString:
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
    tkString, tkWString, tkUString, tkLString:
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

// Using this is a workaround for backward compatability casting
  function DoubleToDT(const AValue: double): TDateTime; inline;
  begin
    result := TDateTime(AValue);
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
    tkfloat:
      if AValue.TypeInfo = TypeInfo(TDateTime) then
        exit(datetimetostr(DoubleToDT(AValue.AsExtended)))
      else
        exit(floattostr(AValue.AsExtended));
    tkString, tkWString, tkUString, tkLString:
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

// Using this is a workaround for backward compatability casting
  function DoubleToDT(const AValue: double): TDateTime; inline;
  begin
    result := TDateTime(AValue);
  end;

begin
  case AValue.Kind of
    tkInteger, tkInt64:
      exit(DoubleToDT(AValue.AsInt64));
    tkfloat:
      exit(DoubleToDT(AValue.AsExtended));
    tkString, tkWString, tkUString, tkLString:
      exit(StrToDateTime(AValue.AsString));
  else
    exit(now);
  end;
end;

function processVelocityVariables(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  RttiType := GRttiContext.GetType(obj.TypeInfo);
  RttiMethod := RttiType.GetMethod('GetItem');
  try
    result := RttiMethod.Invoke(obj, [ADeref]);
    AFound := true;
  except
    on e: Exception do
    begin
      AFound := false;
      if ARaiseIfMissing then
        RaiseError(APosition, 'Cannot dereference variable %s', [AsString(ADeref)]);
      result := '';
    end;
  end;
end;

function GetFieldOrProperty(const APtr: pointer; RttiType: TRttiType; const ADeref: TValue; out AFound: boolean): TValue;
var
  RttiField: TRttiField;
  RttiProp: TRttiProperty;
  ref: string;
begin
  AFound := true;
  ref := AsString(ADeref);
  RttiField := RttiType.GetField(ref);
  if RttiField <> nil then
    exit(RttiField.GetValue(APtr));
  RttiProp := RttiType.GetProperty(ref);
  if RttiProp <> nil then
    exit(RttiProp.GetValue(APtr));
  AFound := false;
end;

function ProcessClass(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; const AOwnObject: boolean; out AFound: boolean): TValue;
var
  ClassType: TClass;
  info: PTypeInfo;
  p: TPair<TDerefMatchFunction, TDerefFunction>;
begin
  AFound := false;
  ClassType := obj.AsObject.ClassType;
  info := obj.TypeInfo;
  if not AOwnObject then
  begin
    for p in GDerefFunctions do
    begin
      if p.Key(info, ClassType) then
      begin
        result := p.Value(APosition, obj, ADeref, ARaiseIfMissing, AFound);
        if AFound then
          exit;
      end;
    end;
  end;
  result := GetFieldOrProperty(obj.AsObject, GRttiContext.GetType(obj.TypeInfo), ADeref, AFound);
end;

function processDictionary(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Deref: TValue;
begin
  AFound := false;
  RttiType := GRttiContext.GetType(obj.TypeInfo);
  RttiMethod := RttiType.GetMethod('GetItem');

  // values are sometime floats, so cast explicitly
  Deref := ADeref;
  if RttiMethod.GetParameters[0].ParamType.TypeKind in [tkInteger, tkInt64] then
    Deref := AsInt(Deref);

  try
    result := RttiMethod.Invoke(obj, [Deref]);
    AFound := true;
  except
    on e: Exception do
    begin
      result := ProcessClass(APosition, obj, ADeref, ARaiseIfMissing, true, AFound);
      if not AFound then
      begin
        if ARaiseIfMissing then
          RaiseError(APosition, 'Cannot dereference ''%s'' in %s', [AsString(Deref), RttiType.QualifiedName]);
        result := '';
      end;
    end;
  end;
end;

{$IFDEF SUPPORT_JSON}

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
  AObj: TJsonObject;
  p: tjsonpair;
  v: TValue;
begin
  AObj := TJsonObject(AClass.AsObject);
  for p in AObj do
  begin
    if JsonValueToTValue(p.JsonValue, v) then
      StackFrame[p.JsonString.Value] := v;
  end;
end;

function processJson(const APosition: IPosition; const obj: TValue; const ADeref: TValue; const ARaiseIfMissing: boolean; out AFound: boolean): TValue;
var
  jsonobj: TJsonObject;
  jsonval: TJsonValue;
  Key: string;
begin
  AFound := true;
  jsonobj := obj.AsObject as TJsonObject;
  Key := AsString(ADeref);
  jsonval := jsonobj.GetValue(Key);

  if not JsonValueToTValue(jsonval, result) then
  begin
    AFound := false;
    if ARaiseIfMissing then
      RaiseError(APosition, 'Cannot dereference %s in dictionary', [Key]);
    result := '';
  end;
end;

function MatchJsonObject(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  result := AClass = TJsonObject;
end;

{$ENDIF}

function Deref(const APosition: IPosition; const AVar, ADeref: TValue; const ARaiseIfMissing: boolean): TValue;

  function ProcessArray(obj: TValue; const ADeref: TValue; out AFound: boolean): TValue;
  var
    i: integer;
    RttiType: TRttiArrayType;
  begin
    i := AsInt(ADeref);
    AFound := false;
    RttiType := GRttiContext.GetType(obj.TypeInfo) as TRttiArrayType;
    if (i < (RttiType.Dimensions[0] as TRttiOrdinalType).MinValue) or (i >= (RttiType.Dimensions[0] as TRttiOrdinalType).MaxValue) then
    begin
      exit;
    end;
    AFound := true;
    result := obj.GetArrayElement(i - (RttiType.Dimensions[0] as TRttiOrdinalType).MinValue);
  end;

  function ProcessDynArray(obj: TValue; const ADeref: TValue; out AFound: boolean): TValue;
  var
    i: integer;
  begin
    AFound := false;
    i := AsInt(ADeref);
    if (i < 0) or (i >= obj.GetArrayLength) then
    begin
      exit;
    end;
    result := obj.GetArrayElement(i);
  end;

  function ProcessRecord(const obj: TValue; const ADeref: TValue; var AFound: boolean): TValue;
  begin
    result := GetFieldOrProperty(obj.GetReferenceToRawData, GRttiContext.GetType(obj.TypeInfo), ADeref, AFound);
  end;

  function ProcessInterface(const obj: TValue; const ADeref: TValue; out AFound: boolean): TValue;
  var
    p: TPair<TDerefMatchInterfaceFunction, TDerefFunction>;
    i: IInterface;
  begin
    i := obj.AsInterface;
    AFound := false;
    for p in GDerefInterfaceFunctions do
    begin
      if p.Key(i) then
      begin
        result := p.Value(APosition, obj, ADeref, ARaiseIfMissing, AFound);
        if AFound then
          exit;
      end;
    end;
  end;

var
  found: boolean;

begin
  case AVar.Kind of
    tkInterface:
      result := ProcessInterface(AVar, ADeref, found);
    tkClass:
      result := ProcessClass(APosition, AVar, ADeref, ARaiseIfMissing, false, found);
    tkrecord:
      result := ProcessRecord(AVar, ADeref, found);
    tkArray:
      result := ProcessArray(AVar, ADeref, found);
    tkDynArray:
      result := ProcessDynArray(AVar, ADeref, found);
  else
    begin
      if ARaiseIfMissing then
        RaiseError(APosition, 'Cannot dereference variable');
      exit('');
    end;
  end;
  if not found and ARaiseIfMissing then
    RaiseError(APosition, 'Cannot dereference variable');
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
var
  Name: string;
begin
  name := AClass.QualifiedClassName;
  result := name.StartsWith('System.Generics.Collections.TDictionary') or name.StartsWith('System.Generics.Collections.TObjectDictionary');
end;

function MatchStringDictionary(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
var
  Name: string;
begin
  name := AClass.QualifiedClassName;
  result := name.StartsWith('System.Generics.Collections.TDictionary<System.string,') or name.StartsWith('System.Generics.Collections.TObjectDictionary<string,');
end;

procedure PopulateStackFrameFromDictionary(const StackFrame: TStackFrame; const ARttiType: TRttiType; const AClass: TValue);
var
  e: TObject;
  T: TRttiType;
  m, movenext: TRttiMethod;
  current: TRttiProperty;
  Key, Value: TRttiField;
  val: TValue;
  k: string;
  o: TValue;
  obj: pointer;
var
  ADict: TObject;

begin
  ADict := AClass.AsObject;
  m := ARttiType.GetMethod('GetEnumerator');
  val := m.Invoke(ADict, []).AsObject;
  e := val.AsObject;
  T := GRttiContext.GetType(e.ClassType);
  k := e.ClassName;
  movenext := T.GetMethod('MoveNext');
  current := T.GetProperty('Current');
  Key := nil;
  Value := nil;
  while movenext.Invoke(e, []).AsBoolean do
  begin
    o := current.GetValue(e); // this returns TPair record
    obj := o.GetReferenceToRawData;
    if Key = nil then
    begin
      T := GRttiContext.GetType(o.TypeInfo);
      Key := T.GetField('Key');
      Value := T.GetField('Value');
    end;
    k := Key.GetValue(obj).AsString;
    val := Value.GetValue(obj);
    StackFrame[k] := val;
  end;
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

GDerefInterfaceFunctions := TList < TPair < TDerefMatchInterfaceFunction, TDerefFunction >>.Create;
GDerefFunctions := TList < TPair < TDerefMatchFunction, TDerefFunction >>.Create;
GPopulateFunctions := TList < TPair < TPopulateMatchFunction, TPopulateStackFrame >>.Create;

RegisterDeref(MatchVelocityVariableObject, processVelocityVariables);
RegisterDeref(MatchDictionary, processDictionary);
{$IFDEF SUPPORT_JSON}
RegisterDeref(MatchJsonObject, processJson);
RegisterPopulateStackFrame(MatchJsonObject, PopulateStackFrameFromJsonObject);
{$ENDIF}
RegisterPopulateStackFrame(MatchStringDictionary, PopulateStackFrameFromDictionary);

RegisterDeref(MatchVelocityVariables, processVelocityVariables);

finalization

GRttiContext.Free;

GDerefFunctions.Free;
GDerefInterfaceFunctions.Free;
GPopulateFunctions.Free;

end.
