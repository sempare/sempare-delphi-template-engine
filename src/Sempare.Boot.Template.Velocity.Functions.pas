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

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}

uses
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Context,
  Sempare.Boot.Template.Velocity.Rtti;

procedure AssertArgCount(const AArgInfo: TVelocityFunctionInfo; const AArgs: TArray<TVelocityValue>);

procedure RegisterDefaultFunctions(Const AContext: IVelocityContext);
procedure AddDefaultFunction(const FN: TVelocityFunctionInfo);

var
  GFunctionInfo: TDictionary<string, TVelocityFunctionInfo>;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Rtti,
  System.TypInfo;

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

procedure AssertArgCount(const AArgInfo: TVelocityFunctionInfo; const AArgs: TArray<TVelocityValue>);
var
  i: integer;
  msg: string;
begin
  if (AArgInfo.MinArgs >= 0) and (AArgInfo.MinArgs = AArgInfo.MaxArgs) and (AArgInfo.MinArgs <> length(AArgs)) then
    raise Exception.CreateFmt('''%s'' expected %d parameters but was given %d.', [AArgInfo.Name, AArgInfo.MinArgs, length(AArgs)])
  else
  begin
    if (AArgInfo.MinArgs >= 0) and (length(AArgs) < AArgInfo.MinArgs) then
      raise Exception.CreateFmt('''%s'' expected at least %d parameters but was given %d.', [AArgInfo.Name, AArgInfo.MinArgs, length(AArgs)]);
    if (AArgInfo.MaxArgs >= 0) and (length(AArgs) > AArgInfo.MaxArgs) then
      raise Exception.CreateFmt('''%s'' expected at most %d parameters but was given %d.', [AArgInfo.Name, AArgInfo.MaxArgs, length(AArgs)]);
  end;
  if length(AArgInfo.Signature) > 1 then
  begin
    for i := 2 to min(length(AArgInfo.Signature), length(AArgs) + 1) do
    begin
      if not IsValidArg(i - 1, AArgInfo.Signature[i], AArgs[i - 2], msg) then
        raise Exception.Create(msg);
    end;
  end;
end;

function InternalSplit(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['split'], AArgs);
  result := asstring(AArgs[0]).split(AArgs[1]);
end;

function InternalLowercase(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['lowercase'], AArgs);
  result := asstring(AArgs[0]).tolower;
end;

function InternalUppercase(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['uppercase'], AArgs);
  result := asstring(AArgs[0]).toupper;
end;

function InternalTrim(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['trim'], AArgs);
  result := Trim(asstring(AArgs[0]));
end;

function InternalSubStr(const AArgs: TArray<TVelocityValue>): TVelocityValue;
var
  s: string;
  so, l: integer;
begin
  AssertArgCount(GFunctionInfo['substr'], AArgs);
  s := asstring(AArgs[0]);
  so := asInt(AArgs[1]);
  if so < 0 then
    so := length(s) + so + 1;
  if length(AArgs) = 2 then
    l := length(s)
  else
    l := asInt(AArgs[2]);
  if l < 0 then
    l := 0;
  result := copy(s, so, l);
end;

function InternalSubString(const AArgs: TArray<TVelocityValue>): TVelocityValue;
var
  s: string;
  so, eo: integer;
begin
  AssertArgCount(GFunctionInfo['substring'], AArgs);
  s := asstring(AArgs[0]);
  so := asInt(AArgs[1]);
  if so < 0 then
    so := length(s) + so + 1;
  if length(AArgs) = 2 then
    eo := length(s)
  else
    eo := asInt(AArgs[2]);
  if eo < 0 then
    eo := length(s) + eo + 1;
  result := copy(s, so, eo - so + 1);
end;

function InternalPos(const AArgs: TArray<TVelocityValue>): TVelocityValue;
var
  offset: integer;
  search, str: string;
begin
  AssertArgCount(GFunctionInfo['pos'], AArgs);
  search := asstring(AArgs[0]);
  str := asstring(AArgs[1]);
  if length(AArgs) = 2 then
    exit(pos(search, str));
  offset := asInt(AArgs[2]);
  if offset < 0 then
    result := pos(search, str, length(str) + offset)
  else
    result := pos(search, str, offset);
end;

function InternalLen(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['len'], AArgs);
  result := length(asstring(AArgs[0]));
end;

function InternalFmt(const AArgs: TArray<TVelocityValue>): TVelocityValue;
var
  args: TArray<TVarRec>;
  i: integer;
begin
  AssertArgCount(GFunctionInfo['fmt'], AArgs);
  setlength(args, length(AArgs) - 1);
  for i := 1 to length(AArgs) - 1 do
    args[i - 1] := AArgs[i].AsVarRec;
  result := format(asstring(AArgs[0]), args);
end;

function InternalFmtDate(const AArgs: TArray<TVelocityValue>): TVelocityValue;
var
  dt: tdatetime;
begin
  AssertArgCount(GFunctionInfo['fmtdt'], AArgs);
  if length(AArgs) = 1 then
    dt := now
  else
    dt := AsDateTime(AArgs[1]);
  result := FormatDateTime(asstring(AArgs[0]), dt);
end;

function InternalNow(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['dtnow'], AArgs);
  result := now;
end;

function InternalInt(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['int'], AArgs);
  result := asInt(AArgs[0]);
end;

function InternalStr(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['str'], AArgs);
  result := asstring(AArgs[0]);
end;

function InternalIsNull(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['isnull'], AArgs);
  result := isnull(AArgs[0]);
end;

function InternalIsStr(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['isstr'], AArgs);
  result := isStrLike(AArgs[0]);
end;

function InternalIsint(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['isint'], AArgs);
  result := isIntLike(AArgs[0]);
end;

function InternalIsBool(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['isbool'], AArgs);
  result := isBool(AArgs[0]);
end;

function InternalIsNUM(const AArgs: TArray<TVelocityValue>): TVelocityValue;
begin
  AssertArgCount(GFunctionInfo['isnum'], AArgs);
  result := isnumlike(AArgs[0]);
end;

procedure RegisterDefaultFunctions(Const AContext: IVelocityContext);
var
  FN: TVelocityFunctionInfo;
begin
  for FN in GFunctionInfo.Values do
    AContext.AddFunction(FN);
end;

procedure AddDefaultFunction(const FN: TVelocityFunctionInfo);

begin
  GFunctionInfo.AddOrSetValue(FN.Name, FN);
end;

procedure RegFunc(const AName: string; const ASig: string; const AMinArgs, AMaxArgs: integer; const AFN: TVelocityFunction);
var
  FN: TVelocityFunctionInfo;

begin
  FN.Name := AName;
  FN.Signature := ASig;
  FN.MinArgs := AMinArgs;
  FN.MaxArgs := AMaxArgs;
  FN.FN := AFN;
  AddDefaultFunction(FN);
end;

initialization

GFunctionInfo := TDictionary<string, TVelocityFunctionInfo>.Create;

// TODO: join, ucfirst, ucwords, rev

RegFunc('lowercase', 'SS', 1, 1, InternalLowercase);
RegFunc('uppercase', 'SS', 1, 1, InternalUppercase);
RegFunc('split', 'SS', 2, 2, InternalSplit);
RegFunc('trim', 'SS', 1, 1, InternalTrim);
RegFunc('substr', 'SSNN', 2, 3, InternalSubStr);
RegFunc('substring', 'SSNN', 2, 3, InternalSubString);
RegFunc('pos', 'SSSN', 2, 3, InternalPos);
RegFunc('len', 'NS', 1, 1, InternalLen);
RegFunc('fmt', 'SS', 2, -1, InternalFmt);
RegFunc('fmtdt', 'SSD', 1, 2, InternalFmtDate);
RegFunc('dtnow', 'D', 0, 0, InternalNow);
RegFunc('int', 'NS', 1, 1, InternalInt);
RegFunc('str', '', 1, 1, InternalStr);
RegFunc('isnull', '', 1, 1, InternalIsNull);
RegFunc('isstr', '', 1, 1, InternalIsStr);
RegFunc('isbool', '', 1, 1, InternalIsBool);
RegFunc('isint', '', 1, 1, InternalIsint);
RegFunc('isnum', '', 1, 1, InternalIsNUM);

finalization

GFunctionInfo.Free;

end.
