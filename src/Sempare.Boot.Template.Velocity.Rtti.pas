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

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}


// Moved from Sempare.Boot.Rtti to make this standalone

uses
  System.Rtti;

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

function isEqual(const left: TValue; const right: TValue): boolean;
function isLessThan(const left: TValue; const right: TValue): boolean;
function isGreaterThan(const left: TValue; const right: TValue): boolean;

var
  EQUALITY_PRECISION: extended = 1E-8;
  GRttiContext: TRttiContext;

implementation

uses
  System.Math,
  System.SysUtils;

const
  INT_LIKE: set of TTypeKind = [tkInteger, tkInt64];
  NUMBER_LIKE: set of TTypeKind = [tkInteger, tkInt64, tkfloat];
  STR_LIKE: set of TTypeKind = [tkString, tkWideString, tkUnicodeString, tkLString];

function isNull(const AValue: TValue): boolean;
begin
  result := AValue.IsEmpty;
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

initialization

GRttiContext := TRttiContext.Create;
GRttiContext.KeepContext;

finalization

GRttiContext.DropContext;
GRttiContext.Free;

end.
