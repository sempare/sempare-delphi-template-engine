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
unit Sempare.Boot.Template.Velocity.Scope;

interface

{$IF defined(FPC)}
{$MODE Delphi}
{$ENDIF}


uses
  System.Generics.Collections,
  System.Rtti,
  System.Json;

type
  TVariableScope = class
  private
    FParent: TVariableScope;
    FScope: TDictionary<string, TValue>;

    function FindScope(const AKey: string): TVariableScope;
    function GetItem(const AKey: string): TValue;

    procedure SetItem(const AKey: string; const Value: TValue);
  public
    constructor Create(const AParent: TVariableScope = nil); overload;
    constructor Create(const ARecord: TValue; const AParent: TVariableScope = nil); overload;
    destructor Destroy; override;
    function Clone(): TVariableScope;

    function Root: TValue;

    property Item[const AKey: string]: TValue read GetItem write SetItem; default;
  end;

implementation

uses
  System.SysUtils,
  Sempare.Boot.Template.Velocity.Rtti;

{ TScope }

function TVariableScope.Clone: TVariableScope;
begin
  result := TVariableScope.Create(self);
end;

constructor TVariableScope.Create(const AParent: TVariableScope);
begin
  FParent := AParent;
  FScope := TDictionary<string, TValue>.Create;
end;

constructor TVariableScope.Create(const ARecord: TValue; const AParent: TVariableScope);
begin
  Create(AParent);
  FScope.Add('_', ARecord);
end;

destructor TVariableScope.Destroy;
begin
  FreeAndNil(FScope);
  FParent := nil;
  inherited;
end;

function TVariableScope.FindScope(const AKey: string): TVariableScope;
begin
  result := self;
  while not result.FScope.ContainsKey(AKey) do
  begin
    result := result.FParent;
    if result = nil then
      exit;
  end;
end;

function TVariableScope.GetItem(const AKey: string): TValue;
var
  l: string;
  Scope: TVariableScope;
begin
  l := AKey.ToLower;
  Scope := FindScope(l);
  if (Scope <> nil) then
    Scope.FScope.TryGetValue(l, result);
end;

function TVariableScope.Root: TValue;
begin
  result := GetItem('_');
end;

procedure TVariableScope.SetItem(const AKey: string; const Value: TValue);

var
  l: string;
  Scope: TVariableScope;
begin
  l := AKey.ToLower;
  Scope := FindScope(l);
  if Scope <> nil then
    Scope.FScope.AddOrSetValue(l, Value)
  else
    FScope.AddOrSetValue(l, Value);
end;

end.
