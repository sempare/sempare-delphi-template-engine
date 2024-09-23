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
 * Licensed under the Apache Version 2.0 or the Sempare Commercial License                          *
 * You may not use this file except in compliance with one of these Licenses.                       *
 * You may obtain a copy of the Licenses at                                                         *
 *                                                                                                  *
 * https://www.apache.org/licenses/LICENSE-2.0                                                      *
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

uses
  System.Rtti,
  System.Classes,
  System.SysUtils,
  Sempare.Template.JSON,
  System.Generics.Collections;

{$I 'Sempare.Template.Compiler.inc'}

type
  TTemplateStreamWriter = class(TTextWriter)
  private
    FWriter: TStreamWriter;
    FStripWS: boolean;
    FStripNL: boolean;
    FTrimLines: boolean;
    FWS: string;
    FLineBuffer: TStringBuilder;
    procedure SetStripNL(const Value: boolean);
    procedure SetStripWS(const Value: boolean);
    procedure NotSupported;

  public
    constructor Create(const AWriter: TStreamWriter); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure Flush; override;
    procedure OwnStream; inline;
    procedure Write(Value: boolean); override;
    procedure Write(Value: Char); override;
    procedure Write(const Value: TCharArray); override;
    procedure Write(Value: double); override;
    procedure Write(Value: integer); override;
    procedure Write(Value: int64); override;
    procedure Write(Value: TObject); override;
    procedure Write(Value: single); override;
    procedure Write(const Value: string); override;
    procedure Write(Value: Cardinal); override;
    procedure Write(Value: UInt64); override;
    procedure Write(const Format: string; Args: array of const); override;
    procedure Write(const Value: TCharArray; Index, Count: integer); override;
    procedure WriteLine; override;
    procedure WriteLine(Value: boolean); override;
    procedure WriteLine(Value: Char); override;
    procedure WriteLine(const Value: TCharArray); override;
    procedure WriteLine(Value: double); override;
    procedure WriteLine(Value: integer); override;
    procedure WriteLine(Value: int64); override;
    procedure WriteLine(Value: TObject); override;
    procedure WriteLine(Value: single); override;
    procedure WriteLine(const Value: string); override;
    procedure WriteLine(Value: Cardinal); override;
    procedure WriteLine(Value: UInt64); override;
    procedure WriteLine(const Format: string; Args: array of const); override;
    procedure WriteLine(const Value: TCharArray; Index, Count: integer); override;

    property StripWS: boolean read FStripWS write SetStripWS;
    property StripNL: boolean read FStripNL write SetStripNL;
    property TrimLines: boolean read FTrimLines write FTrimLines;
  end;

  TMap = record
  private
    FItems: TArray<TPair<string, TValue>>;
    procedure SetItem(const AKey: string; const AValue: TValue);
    function GetKey(const AIdx: integer): string;
    procedure SetKey(const AIdx: integer; const Value: string);
    function GetValue(const AIdx: integer): TValue;
    procedure SetValue(const AIdx: integer; const Value: TValue);
  public
    class function Create(): TMap; overload; static;
    class function Create(const AMap: TMap): TMap; overload; static;
    procedure Clear;
    function Clone: TMap;
    function Add(const AKey: string; const AValue: TValue): boolean; overload;
    function Add<T>(const AKey: string; const AValue: T): boolean; overload;
    function AddNil(const AKey: string): boolean; overload;
    function Delete(const AKey: string): boolean;
    function ContainsKey(const AKey: string): boolean;
    function TryGetValue(const AKey: string; out AValue: TValue): boolean;
    function GetItem(const AKey: string): TValue;
    function GetCount: integer;
    function ToKeyArray: TArray<string>;
    function ToValueArray: TArray<TValue>;
    function ToJSON: string;
    class function ParseJson(const AJson: TJsonObject): TMap; static;
    function GetItems: TArray<TPair<string, TValue>>;
    property Items[const AKey: string]: TValue read GetItem write SetItem; default;
    property Keys[const AIdx: integer]: string read GetKey write SetKey;
    property Values[const AIdx: integer]: TValue read GetValue write SetValue;
    property Count: integer read GetCount;
  end;

function GetTestTimeTollerance(const ANativeTime, AHypervisorTime: double): double;
function FloatToTValue(const AValue: double): TValue;

implementation

uses
  Sempare.Template.Rtti,
  System.TypInfo,
  System.JSON,
  System.Math
{$IFDEF MSWINDOWS}
    , WinAPI.Windows
{$IFDEF SUPPORT_WIN_REGISTRY}, System.Win.Registry{$ELSE}, Registry{$ENDIF};
{$ELSE}
    ;
{$ENDIF}

function FloatToTValue(const AValue: double): TValue;
begin
  if SameValue(AValue, trunc(AValue)) then
    exit(trunc(AValue))
  else
    exit(AValue);
end;

{$IFDEF MSWINDOWS}

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

function GetTestTimeTollerance(const ANativeTime: double; const AHypervisorTime: double): double;
begin
  exit(AHypervisorTime)
end;
{$ENDIF}
{ TMap }

function TMap.Add(const AKey: string; const AValue: TValue): boolean;
var
  LPair: TPair<string, TValue>;

begin
  Result := not ContainsKey(AKey);
  if Result then
  begin
    LPair := TPair<string, TValue>.Create(AKey, AValue);
    SetLength(FItems, length(FItems) + 1);
    FItems[high(FItems)] := LPair;
  end;
end;

class function TMap.Create: TMap;
begin
  Result.FItems := nil;
end;

function TMap.Add<T>(const AKey: string; const AValue: T): boolean;
begin
  exit(Add(AKey, TValue.From<T>(AValue)));
end;

function TMap.AddNil(const AKey: string): boolean;
begin
  exit(Add<pointer>(AKey, nil));
end;

procedure TMap.Clear;
begin
  FItems := nil;
end;

function TMap.Clone: TMap;
begin
  exit(TMap.Create(self));
end;

function TMap.ContainsKey(const AKey: string): boolean;
var
  LPair: TPair<string, TValue>;
begin
  for LPair in FItems do
  begin
    if LPair.Key = AKey then
      exit(True);
  end;
  exit(False);
end;

class function TMap.Create(const AMap: TMap): TMap;
begin
  Result.FItems := copy(AMap.FItems);
end;

function TMap.Delete(const AKey: string): boolean;
var
  i, j: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if FItems[i].Key = AKey then
    begin
      for j := i to High(FItems) - 1 do
        FItems[i] := FItems[i + 1];
      SetLength(FItems, length(FItems) - 1);
      exit(True);
    end;
  end;
  exit(False);
end;

function TMap.GetCount: integer;
begin
  exit(length(FItems));
end;

function TMap.GetItem(const AKey: string): TValue;
begin
  if not TryGetValue(AKey, Result) then
    exit(TValue.Empty);
end;

function TMap.GetItems: TArray<TPair<string, TValue>>;
begin
  exit(FItems);
end;

function TMap.GetKey(const AIdx: integer): string;
begin
  exit(FItems[AIdx].Key);
end;

function TMap.GetValue(const AIdx: integer): TValue;
begin
  exit(FItems[AIdx].Value);
end;

class function TMap.ParseJson(const AJson: TJsonObject): TMap;

  procedure Visit(const AObject: TJsonObject; var AMap: TMap);
  var
    LPair: TJSONPair;
    LMap: TMap;
  begin
    for LPair in AObject do
    begin
      if LPair.JsonValue is TJsonObject then
      begin
        LMap := TMap.Create;
        Visit(TJsonObject(LPair.JsonValue), LMap);
        AMap.Add(LPair.JsonString.Value, TValue.From<TMap>(LMap));
      end
      else if LPair.JsonValue is TJSONNumber then
      begin
        AMap.Add(LPair.JsonString.Value, FloatToTValue(TJSONNumber(LPair.JsonValue).AsDouble));
      end
      else if LPair.JsonValue is TJSONTrue then
      begin
        AMap.Add(LPair.JsonString.Value, True);
      end
      else if LPair.JsonValue is TJSONFalse then
      begin
        AMap.Add(LPair.JsonString.Value, False);
      end
      else if LPair.JsonValue is TJSONNull then
      begin
        AMap.AddNil(LPair.JsonString.Value);
      end
      else if LPair.JsonValue is TJSONString then
      begin
        AMap.Add(LPair.JsonString.Value, LPair.JsonValue.Value);
      end;
    end;
  end;

begin
  Result.FItems := nil;
  if AJson = nil then
    exit;
  Visit(AJson, Result);
end;

procedure TMap.SetItem(const AKey: string; const AValue: TValue);
begin
  Add(AKey, AValue);
end;

procedure TMap.SetKey(const AIdx: integer; const Value: string);
begin
  FItems[AIdx].Key := Value;
end;

procedure TMap.SetValue(const AIdx: integer; const Value: TValue);
begin
  FItems[AIdx].Value := Value;
end;

function TMap.ToJSON: string;
function ToExpr(const AValue: TValue): TJsonValue; forward;
function ToArray(const AValue: TArray<TValue>): TJsonArray; forward;

  function ToMap(const AMap: TMap): TJsonObject;
  var
    LPair: TPair<string, TValue>;
  begin
    Result := TJsonObject.Create;
    for LPair in AMap.FItems do
    begin
      Result.AddPair(LPair.Key, ToExpr(LPair.Value));
    end;
  end;

  function ToArray(const AValue: TArray<TValue>): TJsonArray;
  var
    LValue: TValue;
  begin
    Result := TJsonArray.Create;
    for LValue in AValue do
      Result.AddElement(ToExpr(LValue));
  end;

  function ToExpr(const AValue: TValue): TJsonValue;
  begin
    Result := nil;
    case AValue.Kind of
      tkString, tkWString, tkLString, tkUString:
        exit(TJSONString.Create(AValue.AsString));
      tkInteger, tkInt64:
        exit(TJSONNumber.Create(AValue.AsInt64));
      tkEnumeration:
        if AValue.TypeInfo = TypeInfo(boolean) then
        begin
          if AValue.AsBoolean then
            exit(TJSONTrue.Create)
          else
            exit(TJSONFalse.Create);
        end;
      tkFloat:
        begin
          if IsIntLike(AValue) then
            exit(TJSONNumber.Create(trunc(AValue.AsExtended)))
          else
            exit(TJSONNumber.Create(AValue.AsExtended));
        end;
      tkPointer:
        if AValue.AsType<pointer> = nil then
          exit(TJSONNull.Create());
      tkClass:
        if AValue.AsObject = nil then
          exit(TJSONNull.Create());
      tkRecord{$IFDEF SUPPORT_CUSTOM_MANAGED_RECORDS}, tkMRecord{$ENDIF}:
        exit(ToMap(AValue.AsType<TMap>));
      tkDynArray:
        exit(ToArray(AValue.AsType < TArray < TValue >> ));
    end;
  end;

var
  LObject: TJsonObject;
begin
  LObject := ToMap(self);
  try
    exit(LObject.ToJSON);
  finally
    LObject.Free;
  end;
end;

function TMap.ToKeyArray: TArray<string>;
var
  i: integer;
begin
  SetLength(Result, length(FItems));
  for i := 0 to high(Result) do
    Result[i] := FItems[i].Key;
end;

function TMap.ToValueArray: TArray<TValue>;
var
  i: integer;
begin
  SetLength(Result, length(FItems));
  for i := 0 to high(Result) do
    Result[i] := FItems[i].Value;
end;

function TMap.TryGetValue(const AKey: string; out AValue: TValue): boolean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if FItems[i].Key = AKey then
    begin
      AValue := FItems[i].Value;
      exit(True);
    end;
  end;
  exit(False);
end;

{ TTemplateStreamWriter }

procedure TTemplateStreamWriter.Close;
begin
  Flush;
  FWriter.Close;
end;

constructor TTemplateStreamWriter.Create(const AWriter: TStreamWriter);
begin
  inherited Create;
  FWriter := AWriter;
  FLineBuffer := TStringBuilder.Create;
end;

destructor TTemplateStreamWriter.Destroy;
begin
  FLineBuffer.Free;
  FWriter.Free;
  inherited;
end;

procedure TTemplateStreamWriter.Flush;
begin
  if FTrimLines then
  begin
    if FLineBuffer.length > 0 then
    begin
      var
      Line := FLineBuffer.ToString.Trim;
      FWriter.Write(Line);
      FLineBuffer.Clear;
    end;
  end;
  FWriter.Flush;
end;

procedure TTemplateStreamWriter.NotSupported;
begin
  raise ENotSupportedException.Create('TTemplateStreamWriter.Write');
end;

procedure TTemplateStreamWriter.OwnStream;
begin
  FWriter.OwnStream;
end;

procedure TTemplateStreamWriter.SetStripNL(const Value: boolean);
begin
  FStripNL := Value;
  if Value then
  begin // when we enable stripping, we preseve the old nl
    FWS := FWriter.newline;
    FWriter.newline := '';
  end
  else
  begin
    FWriter.newline := FWS;
  end;
end;

procedure TTemplateStreamWriter.SetStripWS(const Value: boolean);
begin
  FStripWS := Value;
end;

procedure TTemplateStreamWriter.Write(Value: Cardinal);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(const Value: string);
var
  LStartIdx, LEndIdx: integer;
begin
  if FTrimLines then
  begin
    LStartIdx := 1;
    LEndIdx := 1;
    while LEndIdx <= length(Value) do
    begin
      if CharInSet(Value[LEndIdx], [#10, #13]) then
      begin
        FWriter.Write(copy(Value, LStartIdx, LEndIdx - LStartIdx).Trim);
        FWriter.Write(Value[LEndIdx]);
        LStartIdx := LEndIdx + 1;
      end;
      Inc(LEndIdx);
    end;

    if LStartIdx <= length(Value) then
    begin
      FWriter.Write(copy(Value, LStartIdx, length(Value) - LStartIdx + 1).Trim);
    end;
  end
  else
  begin
    if FStripWS or FStripNL then
    begin
      for var LChar in Value do
        Write(LChar);
    end
    else
    begin
      FWriter.Write(Value);
    end;
  end;
end;

procedure TTemplateStreamWriter.Write(Value: boolean);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(const Value: TCharArray; Index, Count: integer);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(const Format: string; Args: array of const);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(Value: UInt64);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(Value: Char);
begin
  if FTrimLines then
  begin
    FLineBuffer.Append(Value);
    if Value = #10 then
    begin
      var
      Line := FLineBuffer.ToString.Trim;
      FWriter.Write(Line);
      FLineBuffer.Clear;
    end;
  end
  else
  begin
    case Value of
      ' ', #9:
        if FStripWS then
          exit;
      #10, #13:
        if FStripNL then
          exit;
    end;
    FWriter.Write(Value);
  end;
end;

procedure TTemplateStreamWriter.Write(Value: int64);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(Value: TObject);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(Value: single);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(const Value: TCharArray);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(Value: double);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.Write(Value: integer);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(const Value: TCharArray);

begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: double);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: integer);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine;
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: boolean);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: Char);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: int64);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: UInt64);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(const Format: string; Args: array of const);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(const Value: TCharArray; Index, Count: integer);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: Cardinal);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: TObject);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(Value: single);
begin
  NotSupported;
end;

procedure TTemplateStreamWriter.WriteLine(const Value: string);
begin
  NotSupported;
end;

initialization

{$IFDEF MSWINDOWS}
  GVmwareResolved := False;
GIsUnderVmware := IsRunningUnderVMWare;
{$ENDIF}

end.
