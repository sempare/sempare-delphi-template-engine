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
unit Sempare.Boot.Template.Velocity.StackFrame;

interface

uses
  System.Generics.Collections,
  System.Rtti;

type
  TStackFrame = class
  private
    FParent: TStackFrame;
    FScope: TDictionary<string, TValue>;

    function FindScope(const AKey: string): TStackFrame;
    function GetItem(const AKey: string): TValue;

    procedure SetItem(const AKey: string; const Value: TValue);
  public
    constructor Create(const AParent: TStackFrame = nil); overload;
    constructor Create(const ARecord: TValue; const AParent: TStackFrame = nil); overload;
    destructor Destroy; override;
    function Clone(): TStackFrame;

    function Root: TValue;

    property Item[const AKey: string]: TValue read GetItem write SetItem; default;
  end;

implementation

uses
  System.SysUtils;

{ TStackFrame }

function TStackFrame.Clone: TStackFrame;
begin
  result := TStackFrame.Create(self);
end;

constructor TStackFrame.Create(const AParent: TStackFrame);
begin
  FParent := AParent;
  FScope := TDictionary<string, TValue>.Create;
end;

constructor TStackFrame.Create(const ARecord: TValue; const AParent: TStackFrame);
begin
  Create(AParent);
  FScope.Add('_', ARecord);
end;

destructor TStackFrame.Destroy;
begin
  FreeAndNil(FScope);
  FParent := nil;
  inherited;
end;

function TStackFrame.FindScope(const AKey: string): TStackFrame;
begin
  result := self;
  while not result.FScope.ContainsKey(AKey) do
  begin
    result := result.FParent;
    if result = nil then
      exit;
  end;
end;

function TStackFrame.GetItem(const AKey: string): TValue;
var
  l: string;
  Scope: TStackFrame;
begin
  l := AKey.ToLower;
  Scope := FindScope(l);
  if (Scope <> nil) then
    Scope.FScope.TryGetValue(l, result);
end;

function TStackFrame.Root: TValue;
begin
  result := GetItem('_');
end;

procedure TStackFrame.SetItem(const AKey: string; const Value: TValue);

var
  l: string;
  Scope: TStackFrame;
begin
  l := AKey.ToLower;
  Scope := FindScope(l);
  if Scope <> nil then
    Scope.FScope.AddOrSetValue(l, Value)
  else
    FScope.AddOrSetValue(l, Value);
end;

end.
