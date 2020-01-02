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
unit Sempare.Boot.Template.Velocity.Functions;

interface

uses
  System.Rtti,
  Sempare.Boot.Template.Velocity.Context;

function CreateVelocityFunctions(const ARegisterDefaults: boolean = true): IVelocityFunctions;
function ToArrayTVarRec(const AArgs: TArray<TValue>): TArray<TVarrec>;

var
  GFunctions: IVelocityFunctions;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.Rtti;

type
  TVelocityFunctions = class(TInterfacedObject, IVelocityFunctions)
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

function CreateVelocityFunctions(const ARegisterDefaults: boolean): IVelocityFunctions;
begin
  result := TVelocityFunctions.Create;
  if ARegisterDefaults then
    result.RegisterDefaults;
end;

function TVelocityFunctions.Add(const AMethod: TRttiMethod): boolean;
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

procedure TVelocityFunctions.AddFunctions(const AClass: TClass);
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

const
  ARGTYPE_STRING = 'S';
  ARGTYPE_NUMBER = 'N';
  ARGTYPE_DATETIME = 'D';
  ARGTYPE_BOOLEAN = 'B';
  ARGTYPE_INTEGER = 'I';

function IsValidArg(const AArgNo: integer; const ASig: char; const AValue: TValue; out AMsg: string): boolean;
{$IF  defined(DEBUG)}
var
  s: string;
{$ENDIF}
begin
{$IF  defined(DEBUG)}
  s := asstring(AValue);
{$ENDIF}
  case ASig of
    ARGTYPE_STRING:
      begin
        result := isStrLike(AValue);
        if not result then
          AMsg := format('Parameter %d expected to be a string', [AArgNo]);
      end;
    ARGTYPE_DATETIME:
      begin
        result := isnumlike(AValue);
        if not result then
          AMsg := format('Parameter %d expected to be a datetime', [AArgNo]);
      end;
    ARGTYPE_BOOLEAN:
      begin
        result := isBool(AValue);
        if not result then
          AMsg := format('Parameter %d expected to be a boolean', [AArgNo]);
      end;
    ARGTYPE_NUMBER:
      begin
        result := isnumlike(AValue);
        if not result then
          AMsg := format('Parameter %d expected to be numeric', [AArgNo]);
      end;
    ARGTYPE_INTEGER:
      begin
        result := isIntLike(AValue);
        if not result then
          AMsg := format('Parameter %d expected to be an integer', [AArgNo]);
      end;
  else
    result := false;
  end;
end;

type
  TInternalFuntions = class
  public
    class function Split(const AString: sTring; const ASep: string): TArray<string>; static;
    class function Lowercase(const AString: string): string; static;
    class function Uppercase(const AString: string): string; static;
    class function Trim(const AString: string): string; static;
    class function SubStr(const AString: string; AStartOffset: integer): string; overload; static;
    class function SubStr(const AString: string; AStartOffset, ALength: integer): string; overload; static;
    class function SubString(const AString: string; AStartOffset, AEndOffset: integer): string; overload; static;
    class function SubString(const AString: string; AStartOffset: integer): string; overload; static;
    class function Pos(const search, Str: string; offset: integer): integer; overload; static;
    class function Pos(const search, Str: string): integer; overload; static;
    class function Len(const AString: string): integer; static;
    class function Fmt(const AArgs: TArray<TValue>): string; static;
    class function FmtDt(const AFormat: string; const ADateTime: TDateTime): string; static;
    class function DtNow: TDateTime; static;
    class function Int(const AValue: TValue): integer; static;
    class function Str(const AValue: TValue): string; static;
    class function UCFirst(const AString: string): string; static;
    class function Rev(const AString: string): string; static;
    class function IsNull(const AValue: TValue): boolean; static;
    class function IsStr(const AValue: TValue): boolean; static;
    class function Isint(const AValue: TValue): boolean; static;
    class function isBool(const AValue): boolean; static;
    class function IsNum(const AValue: TValue): boolean; static;
  end;

class function TInternalFuntions.Pos(const search, Str: string): integer;
begin
  result := System.Pos(search, Str);
end;

class function TInternalFuntions.Split(const AString: sTring; const ASep: string): TArray<string>;
begin
  result := AString.Split([ASep]);
end;

class function TInternalFuntions.Lowercase(const AString: string): string;
begin
  result := AString.ToLower;
end;

class function TInternalFuntions.Uppercase(const AString: string): string;
begin
  result := AString.ToUpper;
end;

class function TInternalFuntions.Trim(const AString: string): string;
begin
  result := AString.Trim;
end;

class function TInternalFuntions.SubStr(const AString: string; AStartOffset: integer; ALength: integer): string;
begin
  if AStartOffset < 0 then
    AStartOffset := length(AString) + AStartOffset + 1;
  if ALength < 0 then
    ALength := 0;
  result := copy(AString, AStartOffset, ALength);
end;

class function TInternalFuntions.SubString(const AString: string; AStartOffset: integer): string;
begin
  result := SubString(AString, AStartOffset, length(AString));
end;

class function TInternalFuntions.SubStr(const AString: string; AStartOffset: integer): string;
begin
  result := SubStr(AString, AStartOffset, length(AString));
end;

class function TInternalFuntions.SubString(const AString: string; AStartOffset: integer; AEndOffset: integer): string;
begin
  if AStartOffset < 0 then
    AStartOffset := length(AString) + AStartOffset + 1;
  if AEndOffset < 0 then
    AEndOffset := length(AString) + AEndOffset + 1;
  result := copy(AString, AStartOffset, AEndOffset - AStartOffset + 1);
end;

class function TInternalFuntions.Pos(const search: String; const Str: string; offset: integer): integer;
begin
  if offset < 0 then
    offset := offset + length(Str);
  result := System.Pos(search, Str, offset);
end;

class function TInternalFuntions.Len(const AString: string): integer;
begin
  result := length(AString);
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

class function TInternalFuntions.Fmt(const AArgs: TArray<TValue>): string;
begin
  result := format(asstring(AArgs[0]), ToArrayTVarRec(copy(AArgs, 1, length(AArgs) - 1)));
end;

class function TInternalFuntions.FmtDt(const AFormat: string; const ADateTime: TDateTime): string;
begin
  result := FormatDateTime(AFormat, ADateTime);
end;

class function TInternalFuntions.DtNow(): TDateTime;
begin
  result := System.SysUtils.Now;
end;

class function TInternalFuntions.Int(const AValue: TValue): integer;
begin
  result := asInt(AValue);
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

class function TInternalFuntions.Isint(const AValue: TValue): boolean;
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

constructor TVelocityFunctions.Create;
begin
  FFunctions := TDictionary < string, TArray < TRttiMethod >>.Create;
end;

destructor TVelocityFunctions.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

function TVelocityFunctions.GetIsEmpty: boolean;
begin
  result := FFunctions.Count > 0;
end;

procedure TVelocityFunctions.RegisterDefaults;
begin
  AddFunctions(TInternalFuntions);
end;

function TVelocityFunctions.TryGetValue(const AName: string; out AMethods: TArray<TRttiMethod>): boolean;
begin
  result := FFunctions.TryGetValue(AName, AMethods);
end;

initialization

GFunctions := CreateVelocityFunctions();

finalization

GFunctions := nil;

end.
