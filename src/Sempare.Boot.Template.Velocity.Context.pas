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
unit Sempare.Boot.Template.Velocity.Context;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.StackFrame,
  Sempare.Boot.Template.Velocity.Common;

type
  IVelocityFunctions = interface
    ['{D80C777C-086E-4680-A97B-92B8FA08C995}']

    function GetIsEmpty : boolean;
    procedure AddFunctions(const AClass: TClass);
    procedure RegisterDefaults;

    function TryGetValue(const AName: string; out AMethod: TArray<TRttiMethod>): boolean;
    function Add(const AMethod: TRttiMethod) : boolean;

    property IsEmpty : boolean read GetIsEmpty;
  end;

  TVelocityEvaluationOption = ( //
    eoNoPosition, //
    eoEvalEarly, //
    eoEvalVarsEarly, //
    eoStripRecurringNewline, // TODO
    eoTrimLines, // TODO
    //eoDebug, // TODO
    eoPrettyPrint, //
    eoStripRecurringSpaces, //
    eoConvertTabsToSpaces, //
    eoNoDefaultFunctions, //
    eoThrowWhenVariableNotFound //
    );

  TVelocityEvaluationOptions = set of TVelocityEvaluationOption;

  IVelocityContext = interface;

  TTemplateResolver = reference to function(const AContext: IVelocityContext; const AName: string): IVelocityTemplate;

  IVelocityContext = interface
    ['{979D955C-B4BD-46BB-9430-1E74CBB999D4}']

    function TryGetTemplate(const AName: string; out ATemplate: IVelocityTemplate): boolean;
    function GetTemplate(const AName: string): IVelocityTemplate;
    procedure AddTemplate(const AName: string; const ATemplate: IVelocityTemplate);

    function GetTemplateResolver: TTemplateResolver;
    procedure SetTemplateResolver(const AResolver: TTemplateResolver);

    function TryGetVariable(const AName: string; out AValue: TValue): boolean;
    function GetVariable(const AName: string): TValue;
    procedure SetVariable(const AName: string; const AValue: TValue);

    function GetOptions: TVelocityEvaluationOptions;
    procedure SetOptions(const AOptions: TVelocityEvaluationOptions);

    function GetScriptStartToken: string;
    procedure SetScriptStartToken(const AToken: string);
    function GetScriptEndToken: string;
    procedure SetScriptEndToken(const AToken: string);

    function TryGetFunction(const AName: string; out AFunction: TArray<TRttiMethod>): boolean;
    procedure SetFunctions(const AFunctions: IVelocityFunctions);
    function GetFunctions(): IVelocityFunctions; overload;

    function GetMaxRunTimeMs: integer;
    procedure SetMaxRunTimeMs(const ATimeMS: integer);

    function GetEncoding: TEncoding;
    procedure SetEncoding(const AEncoding: TEncoding);

    procedure UseHtmlVariableEncoder;
    function GetVariableEncoder: TVelocityEncodeFunction;
    procedure SetVariableEncoder(const AEncoder: TVelocityEncodeFunction);

    function GetNewLine: string;
    procedure SetNewLine(const ANewLine: string);

    property Functions: IVelocityFunctions read GetFunctions write SetFunctions;
    property NewLine: string read GetNewLine write SetNewLine;
    property TemplateResolver: TTemplateResolver read GetTemplateResolver write SetTemplateResolver;
    property MaxRunTimeMs: integer read GetMaxRunTimeMs write SetMaxRunTimeMs;
    property VariableEncoder: TVelocityEncodeFunction read GetVariableEncoder write SetVariableEncoder;
    property Variable[const AKey: string]: TValue read GetVariable write SetVariable; default;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property Template[const AName: string]: IVelocityTemplate read GetTemplate;
    property Options: TVelocityEvaluationOptions read GetOptions write SetOptions;
    property StartToken: string read GetScriptStartToken write SetScriptStartToken;
    property EndToken: string read GetScriptEndToken write SetScriptEndToken;
  end;

  IVelocityContextForScope = interface
    ['{65466282-2814-42EF-935E-DC45F7B8A3A9}']
    procedure ApplyTo(const AScope: TStackFrame);
  end;

function CreateVelocityContext(const AOptions: TVelocityEvaluationOptions = []): IVelocityContext;

var
  GDefaultRuntimeMS: integer = 6000000;
  GDefaultOpenTag: string = '<%';
  GDefaultCloseTag: string = '%>';
  GNewLine: string = #13#10;

implementation

uses
  System.NetEncoding,
  System.SyncObjs,
  Sempare.Boot.Template.Velocity.Functions;

type
  TVelocityContext = class(TInterfacedObject, IVelocityContext, IVelocityContextForScope)
  private
    FTemplateResolver: TTemplateResolver;
    FTemplates: TDictionary<string, IVelocityTemplate>;
    FScope: TDictionary<string, TValue>;
    FOptions: TVelocityEvaluationOptions;
    FStartToken: string;
    FEndToken: string;
    FEncoding: TEncoding;
    FFunctions: IVelocityFunctions;
    FFunctionsSet: boolean;
    FVariableEncoder: TVelocityEncodeFunction;
    FMaxRuntimeMs: integer;
    FLock: TCriticalSection;
    FNewLine: string;
  public
    constructor Create(const AOptions: TVelocityEvaluationOptions);
    destructor Destroy; override;

    function GetEncoding: TEncoding;
    procedure SetEncoding(const AEncoding: TEncoding);

    function TryGetTemplate(const AName: string; out ATemplate: IVelocityTemplate): boolean;
    function GetTemplate(const AName: string): IVelocityTemplate;
    procedure AddTemplate(const AName: string; const ATemplate: IVelocityTemplate);

    function GetTemplateResolver: TTemplateResolver;
    procedure SetTemplateResolver(const AResolver: TTemplateResolver);

    function TryGetVariable(const AName: string; out AValue: TValue): boolean;
    function GetVariable(const AName: string): TValue;
    procedure SetVariable(const AName: string; const AValue: TValue);

    function GetOptions: TVelocityEvaluationOptions;
    procedure SetOptions(const AOptions: TVelocityEvaluationOptions);

    function GetScriptStartToken: string;
    procedure SetScriptStartToken(const AToken: string);
    function GetScriptEndToken: string;
    procedure SetScriptEndToken(const AToken: string);

    function GetMaxRunTimeMs: integer;
    procedure SetMaxRunTimeMs(const ATimeMS: integer);

    procedure UseHtmlVariableEncoder;
    function GetVariableEncoder: TVelocityEncodeFunction;
    procedure SetVariableEncoder(const AEncoder: TVelocityEncodeFunction);

    function TryGetFunction(const AName: string; out AFunction: TArray<TRttiMethod>): boolean;
    procedure SetFunctions(const AFunctions: IVelocityFunctions);
    function GetFunctions(): IVelocityFunctions; overload;

    function GetNewLine: string;
    procedure SetNewLine(const ANewLine: string);

    procedure ApplyTo(const AScope: TStackFrame);
  end;

function CreateVelocityContext(const AOptions: TVelocityEvaluationOptions): IVelocityContext;
begin
  result := TVelocityContext.Create(AOptions);
end;

{ TVelocityContext }

procedure TVelocityContext.AddTemplate(const AName: string; const ATemplate: IVelocityTemplate);
begin
  FLock.Enter;
  try
    FTemplates.AddOrSetValue(AName, ATemplate);
  finally
    FLock.Leave;
  end;
end;

procedure TVelocityContext.ApplyTo(const AScope: TStackFrame);
var
  p: TPair<string, TValue>;
begin
  for p in FScope do
  begin
    AScope[p.Key] := p.Value;
  end;
end;

constructor TVelocityContext.Create(const AOptions: TVelocityEvaluationOptions);
begin
  FOptions := AOptions;
  FMaxRuntimeMs := GDefaultRuntimeMS;
  FEncoding := TEncoding.ASCII;
  FStartToken := GDefaultOpenTag;
  FEndToken := GDefaultCloseTag;
  FTemplates := TDictionary<string, IVelocityTemplate>.Create;
  FScope := TDictionary<string, TValue>.Create;
  FFunctions := GFunctions;
  FLock := TCriticalSection.Create;
  FNewLine := GNewLine;
end;

destructor TVelocityContext.Destroy;
begin
  FTemplates.Free;
  FScope.Free;
  FFunctions := nil;
  FLock.Free;
  inherited;
end;

function TVelocityContext.TryGetFunction(const AName: string; out AFunction: TArray<TRttiMethod>): boolean;
begin
  FLock.Enter;
  try
    if not FFunctionsSet and not(eoNoDefaultFunctions in FOptions) then
    begin
      if FFunctions.IsEmpty then
        FFunctions.RegisterDefaults;
      FFunctionsSet := true;
    end;
    result := FFunctions.TryGetValue(AName.ToLower, AFunction);
  finally
    FLock.Leave;
  end;
end;

function TVelocityContext.GetEncoding: TEncoding;
begin
  result := FEncoding;
end;

function TVelocityContext.GetFunctions: IVelocityFunctions;
begin
  result := FFunctions;
end;

function TVelocityContext.GetMaxRunTimeMs: integer;
begin
  result := FMaxRuntimeMs;
end;

function TVelocityContext.GetNewLine: string;
begin
  result := FNewLine;
end;

function TVelocityContext.GetOptions: TVelocityEvaluationOptions;
begin
  result := FOptions;
end;

function TVelocityContext.GetVariable(const AName: string): TValue;
begin
  FLock.Enter;
  try
    result := FScope[AName];
  finally
    FLock.Leave;
  end;
end;

function TVelocityContext.GetVariableEncoder: TVelocityEncodeFunction;
begin
  result := FVariableEncoder;
end;

function TVelocityContext.GetScriptEndToken: string;
begin
  result := FEndToken;
end;

function TVelocityContext.GetScriptStartToken: string;
begin
  result := FStartToken;
end;

function TVelocityContext.GetTemplate(const AName: string): IVelocityTemplate;
begin
  if not TryGetTemplate(AName, result) then
    result := nil;
end;

function TVelocityContext.GetTemplateResolver: TTemplateResolver;
begin
  result := FTemplateResolver;
end;

procedure TVelocityContext.SetEncoding(const AEncoding: TEncoding);
begin
  FEncoding := AEncoding;
end;

procedure TVelocityContext.SetFunctions(const AFunctions: IVelocityFunctions);
begin
  FFunctions := AFunctions;
  FFunctionsSet := true;
end;

procedure TVelocityContext.SetMaxRunTimeMs(const ATimeMS: integer);
begin
  FMaxRuntimeMs := ATimeMS;
end;

procedure TVelocityContext.SetNewLine(const ANewLine: string);
begin
  FNewLine := ANewLine;
end;

procedure TVelocityContext.SetOptions(const AOptions: TVelocityEvaluationOptions);
begin
  FOptions := AOptions;
end;

procedure TVelocityContext.SetVariable(const AName: string; const AValue: TValue);
begin
  FLock.Enter;
  try
    FScope.AddOrSetValue(AName, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TVelocityContext.SetVariableEncoder(const AEncoder: TVelocityEncodeFunction);
begin
  FVariableEncoder := AEncoder;
end;

procedure TVelocityContext.SetScriptEndToken(const AToken: string);
begin
  FEndToken := AToken;
end;

procedure TVelocityContext.SetScriptStartToken(const AToken: string);
begin
  FStartToken := AToken;
end;

procedure TVelocityContext.SetTemplateResolver(const AResolver: TTemplateResolver);
begin
  FTemplateResolver := AResolver;
end;

function TVelocityContext.TryGetTemplate(const AName: string; out ATemplate: IVelocityTemplate): boolean;
begin
  FLock.Enter;
  try
    result := FTemplates.TryGetValue(AName, ATemplate);
    if result then
      exit(true);
    if not assigned(FTemplateResolver) then
      exit(false);
    ATemplate := FTemplateResolver(self, AName);
    if ATemplate = nil then
      exit(false);
    AddTemplate(AName, ATemplate);
    exit(true);
  finally
    FLock.Leave;
  end;
end;

function TVelocityContext.TryGetVariable(const AName: string; out AValue: TValue): boolean;
begin
  FLock.Enter;
  try
    result := FScope.TryGetValue(AName, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TVelocityContext.UseHtmlVariableEncoder;
begin
  FVariableEncoder := function(const AValue: string): string
    begin
      result := TNetEncoding.HTML.Encode(AValue);
    end;
end;

end.
