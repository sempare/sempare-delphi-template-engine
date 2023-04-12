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
 * Copyright (c) 2019-2023 Sempare Limited                                                          *
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
unit Sempare.Template.Util;

interface

{$I 'Sempare.Template.Compiler.inc'}
// This is copied from Sempare.Boot.Common.PreserveValue to make standalone.

type
  IPreserveValue<T> = interface

    // A helper to allow a reference to be maintained
    // so that the object does not get optimised out
    procedure KeepAlive;
    procedure SetValue(const AValue: T);
    procedure NoReset; overload;
    procedure NoReset(const AValue: T); overload;
  end;

type
  Preserve = class
    class function Value<T>(var AValue: T): IPreserveValue<T>; overload; static;
    class function Value<T>(var AValue: T; const NewValue: T): IPreserveValue<T>; overload; static;
  end;

type

  TPreserveValue<T> = class(TInterfacedObject, IPreserveValue<T>)
  type
    PT = ^T;
  private
    FOldValue: T;
    FValuePtr: PT;
    FReset: boolean;
  public
    constructor Create(var AValue: T); overload;
    constructor Create(var AValue: T; const NewValue: T); overload;
    destructor Destroy; override;

    procedure KeepAlive;
    procedure SetValue(const AValue: T); inline;
    procedure NoReset; overload;
    procedure NoReset(const AValue: T); overload;
  end;

function GetTestTimeTollerance(const ANativeTime, AHypervisorTime: double): double;

implementation

{$IFDEF MSWINDOWS}

uses
  SysUtils,
  WinAPI.Windows,
{$IFDEF SUPPORT_WIN_REGISTRY}System.Win.Registry{$ELSE}Registry{$ENDIF};

var
  GVmwareResolved: boolean;
  GIsUnderVmware: boolean;

function IsRunningUnderVMWare: boolean;
var
  Reg: TRegistry;
begin
  if GVmwareResolved then
  begin
    exit(GIsUnderVmware);
  end;
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System\BIOS') then
    begin
      if Pos('VMware', Reg.ReadString('SystemProductName')) > 0 then
        Result := True;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  GIsUnderVmware := Result;
end;

function GetTestTimeTollerance(const ANativeTime, AHypervisorTime: double): double;
begin
  if IsRunningUnderVMWare then
    exit(AHypervisorTime)
  else
    exit(ANativeTime);
end;
{$ELSE}

function GetTestTimeTollerance(const ANativeTime, AHypervisorTime: double): double;
begin
  exit(AHypervisorTime)
end;
{$ENDIF}
{ TPreserveValue<T> }

constructor TPreserveValue<T>.Create(var AValue: T);
begin
  FOldValue := AValue;
  FValuePtr := @AValue;
  FReset := True;
end;

constructor TPreserveValue<T>.Create(var AValue: T; const NewValue: T);
begin
  Create(AValue);
  AValue := NewValue;
end;

procedure TPreserveValue<T>.SetValue(const AValue: T);
begin
  FValuePtr^ := AValue;
end;

destructor TPreserveValue<T>.Destroy;
begin
  if FReset then
    SetValue(FOldValue);
  inherited;
end;

procedure TPreserveValue<T>.KeepAlive;
begin
  // do nothing
end;

procedure TPreserveValue<T>.NoReset(const AValue: T);
begin
  NoReset();
  SetValue(AValue);
end;

procedure TPreserveValue<T>.NoReset;
begin
  FReset := False;
end;

{ Preseve }

class function Preserve.Value<T>(var AValue: T): IPreserveValue<T>;
begin
  exit(TPreserveValue<T>.Create(AValue));
end;

class function Preserve.Value<T>(var AValue: T; const NewValue: T): IPreserveValue<T>;
begin
  exit(TPreserveValue<T>.Create(AValue, NewValue));
end;

initialization

{$IFDEF MSWINDOWS}
  GVmwareResolved := False;
GIsUnderVmware := IsRunningUnderVMWare;
{$ENDIF}

end.
