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
unit Sempare.Template.Context;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  System.Rtti,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Sempare.Template.AST,
  Sempare.Template.StackFrame,
  Sempare.Template.Common;

type
  ITemplateContext = interface;

  TStreamWriterProvider = reference to function(const AStream: TStream; const AContext: ITemplateContext): TStreamWriter;

  ITemplateFunctions = interface
    ['{D80C777C-086E-4680-A97B-92B8FA08C995}']

    function GetIsEmpty: boolean;
    procedure AddFunctions(const AClass: TClass);
    procedure Remove(const AName: string);
    procedure RegisterDefaults;
    function TryGetValue(const AName: string; out AMethod: TArray<TRttiMethod>): boolean;
    function Add(const AMethod: TRttiMethod): boolean;
    property IsEmpty: boolean read GetIsEmpty;
  end;

  TTemplateEvaluationOption = ( //
    eoNoPosition, //
    eoEvalEarly, //
    eoEvalVarsEarly, //
    eoStripRecurringNewlines, //
    eoTrimLines, //
    eoEmbedException, //
    eoPrettyPrint, //
    eoStripRecurringSpaces, //
    eoConvertTabsToSpaces, //
    eoNoDefaultFunctions, //
    eoRaiseErrorWhenVariableNotFound, //
    eoFlattenTemplate, //
    eoOptimiseTemplate //
    );

  TTemplateEvaluationOptions = set of TTemplateEvaluationOption;

  TPrettyPrintOutput = reference to procedure(const APrettyPrint: string);
  TTemplateResolver = reference to function(const AContext: ITemplateContext; const AName: string): ITemplate;
  TTemplateResolverWithContext = reference to function(const AContext: ITemplateContext; const AName: string; const AResolveContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;

  ITemplateEvaluationContext = interface
    ['{FCE6891F-3D39-4CC4-8ADB-024D843C7770}']
    function TryGetBlock(const AName: string; out ABlock: IBlockStmt): boolean;
    procedure AddBlock(const AName: string; const ABlock: IBlockStmt);
    procedure RemoveBlock(const AName: string);

    procedure StartEvaluation;
    procedure EndEvaluation;
  end;

  ITemplateContext = interface
    ['{979D955C-B4BD-46BB-9430-1E74CBB999D4}']

    function TryGetContextTemplate(const AName: string; out ATemplate: ITemplate; const AResolveContext: TTemplateValue): boolean; overload;
    function TryGetTemplate(const AName: string; out ATemplate: ITemplate; const AResolveContext: TTemplateValue): boolean; overload;
    function TryGetTemplate(const AName: string; out ATemplate: ITemplate): boolean; overload;
    function GetTemplate(const AName: string): ITemplate;
    procedure SetTemplate(const AName: string; const ATemplate: ITemplate);
    procedure RemoveTemplate(const AName: string);
    procedure ClearTemplates();

    function GetTemplateResolver: TTemplateResolver; overload;
    procedure SetTemplateResolver(const AResolver: TTemplateResolver); overload;
    function GetTemplateResolverWithContext: TTemplateResolverWithContext; overload;
    procedure SetTemplateResolverWithContext(const AResolver: TTemplateResolverWithContext); overload;

    function TryGetVariable(const AName: string; out AValue: TValue): boolean;
    function GetVariable(const AName: string): TValue;
    procedure SetVariable(const AName: string; const AValue: TValue);

    function GetOptions: TTemplateEvaluationOptions;
    procedure SetOptions(const AOptions: TTemplateEvaluationOptions);

    function GetScriptStartToken: string;
    procedure SetScriptStartToken(const AToken: string);
    function GetScriptEndToken: string;
    procedure SetScriptEndToken(const AToken: string);

    function TryGetFunction(const AName: string; out AFunction: TArray<TRttiMethod>): boolean;
    procedure SetFunctions(const AFunctions: ITemplateFunctions);
    function GetFunctions(): ITemplateFunctions; overload;

    function GetMaxRunTimeMs: integer;
    procedure SetMaxRunTimeMs(const ATimeMS: integer);

    function GetEncoding: TEncoding;
    procedure SetEncoding(const AEncoding: TEncoding);

{$IFDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}
    procedure UseHtmlVariableEncoder;
{$ENDIF}
    function GetVariableEncoder: TTemplateEncodeFunction;
    procedure SetVariableEncoder(const AEncoder: TTemplateEncodeFunction);
    function GetVariables: ITemplateVariables;

    function GetNewLine: string;
    procedure SetNewLine(const ANewLine: string);

    function GetStreamWriterProvider: TStreamWriterProvider;
    procedure SetStreamWriterProvider(const AProvider: TStreamWriterProvider);

    function GetScriptEndStripToken: string;
    function GetScriptStartStripToken: string;
    procedure SetScriptEndStripToken(const Value: string);
    procedure SetScriptStartStripToken(const Value: string);

    procedure SetValueSeparator(const ASeparator: char);
    function GetValueSeparator: char;
    function GetDecimalSeparator: char;
    procedure SetDecimalSeparator(const ASeparator: char);
    function GetFormatSettings: TFormatSettings;

    function GetDebugErrorFormat: string;
    procedure SetDebugErrorFormat(const AFormat: string);

    procedure SetPrettyPrintOutput(const APrettyPrintOutput: TPrettyPrintOutput);
    function GetPrettyPrintOutput: TPrettyPrintOutput;

    function GetWhitespace: char;
    procedure SetWhiteSpace(const AWS: char);

    property Functions: ITemplateFunctions read GetFunctions write SetFunctions;
    property NewLine: string read GetNewLine write SetNewLine;
    property WhitespaceChar: char read GetWhitespace write SetWhiteSpace;
    property TemplateResolver: TTemplateResolver read GetTemplateResolver write SetTemplateResolver;
    property TemplateResolverWithContext: TTemplateResolverWithContext read GetTemplateResolverWithContext write SetTemplateResolverWithContext;
    property MaxRunTimeMs: integer read GetMaxRunTimeMs write SetMaxRunTimeMs;
    property VariableEncoder: TTemplateEncodeFunction read GetVariableEncoder write SetVariableEncoder;
    property Variable[const AKey: string]: TValue read GetVariable write SetVariable; default;
    property Variables: ITemplateVariables read GetVariables;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property Template[const AName: string]: ITemplate read GetTemplate write SetTemplate;
    property Options: TTemplateEvaluationOptions read GetOptions write SetOptions;
    property StartToken: string read GetScriptStartToken write SetScriptStartToken;
    property EndToken: string read GetScriptEndToken write SetScriptEndToken;

    property StartStripToken: string read GetScriptStartStripToken write SetScriptStartStripToken;
    property EndStripToken: string read GetScriptEndStripToken write SetScriptEndStripToken;

    property ValueSeparator: char read GetValueSeparator write SetValueSeparator;
    property DecimalSeparator: char read GetDecimalSeparator write SetDecimalSeparator;
    property FormatSettings: TFormatSettings read GetFormatSettings;
    property DebugErrorFormat: string read GetDebugErrorFormat write SetDebugErrorFormat;
    property StreamWriterProvider: TStreamWriterProvider read GetStreamWriterProvider write SetStreamWriterProvider;
    property PrettyPrintOutput: TPrettyPrintOutput read GetPrettyPrintOutput write SetPrettyPrintOutput;
  end;

  ITemplateContextForScope = interface
    ['{65466282-2814-42EF-935E-DC45F7B8A3A9}']
    procedure ApplyTo(const AScope: TStackFrame);
  end;

  TUTF8WithoutPreambleEncoding = class(TUTF8Encoding)
  public
    function GetPreamble: TBytes; override;
  end;

function CreateTemplateContext(const AOptions: TTemplateEvaluationOptions = []): ITemplateContext;

var
  GDefaultRuntimeMS: integer = 60000;
  GDefaultOpenTag: string = '<%';
  GDefaultCloseTag: string = '%>';
  GNewLine: string = #13#10;
  GDefaultEncoding: TEncoding;
  GUTF8WithoutPreambleEncoding: TUTF8WithoutPreambleEncoding;
  GStreamWriterProvider: TStreamWriterProvider;
  GPrettyPrintOutput: TPrettyPrintOutput;
  GDefaultOpenStripWSTag: string = '<|';
  GDefaultCloseWSTag: string = '|>';

implementation

uses
{$IFDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}
{$IFDEF SUPPORT_NET_ENCODING}
  System.NetEncoding,
{$ELSE}
  IdStrings,
{$ENDIF}
{$ENDIF}
  System.SyncObjs,
  Sempare.Template,
  Sempare.Template.Evaluate,
  Sempare.Template.Functions,
  Sempare.Template.ResourceStrings;

type
  TEvaluationContext = class
  private
    FBlocks: TObjectDictionary<string, TStack<IBlockStmt>>;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetBlock(const AName: string; out ABlock: IBlockStmt): boolean;
    procedure AddBlock(const AName: string; const ABlock: IBlockStmt);
    procedure RemoveBlock(const AName: string);
  end;

  TTemplateContext = class(TInterfacedObject, ITemplateContext, ITemplateContextForScope, ITemplateEvaluationContext)
  private
    class threadvar FEvaluationContext: TEvaluationContext;
  private
    FTemplateResolver: TTemplateResolverWithContext;
    FTemplates: TDictionary<string, ITemplate>;
    FVariables: ITemplateVariables;
    FOptions: TTemplateEvaluationOptions;
    FStartToken: string;
    FEndToken: string;
    FStartStripToken: string;
    FEndStripToken: string;
    FEncoding: TEncoding;
    FFunctions: ITemplateFunctions;
    FFunctionsSet: boolean;
    FVariableEncoder: TTemplateEncodeFunction;
    FMaxRuntimeMs: integer;
    FLock: TCriticalSection;
    FStreamWriterProvider: TStreamWriterProvider;
    FNewLine: string;
    FValueSeparator: char;
    FFormatSettings: TFormatSettings;
    FDebugFormat: string;
    FPrettyPrintOutput: TPrettyPrintOutput;
    FWhiteSpace: char;
  public
    constructor Create(const AOptions: TTemplateEvaluationOptions);
    destructor Destroy; override;

    function TryGetBlock(const AName: string; out ABlock: IBlockStmt): boolean;
    procedure AddBlock(const AName: string; const ABlock: IBlockStmt);
    procedure RemoveBlock(const AName: string);

    procedure SetPrettyPrintOutput(const APrettyPrintOutput: TPrettyPrintOutput);
    function GetPrettyPrintOutput: TPrettyPrintOutput;

    procedure StartEvaluation;
    procedure EndEvaluation;

    function GetEncoding: TEncoding;
    procedure SetEncoding(const AEncoding: TEncoding);

    function TryGetContextTemplate(const AName: string; out ATemplate: ITemplate; const AResolveContext: TTemplateValue): boolean; overload;
    function TryGetTemplate(const AName: string; out ATemplate: ITemplate; const AResolveContext: TTemplateValue): boolean; overload;
    function TryGetTemplate(const AName: string; out ATemplate: ITemplate): boolean; overload;
    function GetTemplate(const AName: string): ITemplate;
    procedure SetTemplate(const AName: string; const ATemplate: ITemplate);
    procedure RemoveTemplate(const AName: string);
    procedure ClearTemplates();

    function GetTemplateResolver: TTemplateResolver;
    procedure SetTemplateResolver(const AResolver: TTemplateResolver);

    function GetTemplateResolverWithContext: TTemplateResolverWithContext; overload;
    procedure SetTemplateResolverWithContext(const AResolver: TTemplateResolverWithContext); overload;

    function TryGetVariable(const AName: string; out AValue: TValue): boolean;
    function GetVariable(const AName: string): TValue;
    procedure SetVariable(const AName: string; const AValue: TValue);
    function GetVariables: ITemplateVariables;

    function GetOptions: TTemplateEvaluationOptions;
    procedure SetOptions(const AOptions: TTemplateEvaluationOptions);

    function GetScriptStartToken: string;
    procedure SetScriptStartToken(const AToken: string);
    function GetScriptEndToken: string;
    procedure SetScriptEndToken(const AToken: string);

    function GetScriptEndStripToken: string;
    function GetScriptStartStripToken: string;
    procedure SetScriptEndStripToken(const Value: string);
    procedure SetScriptStartStripToken(const Value: string);

    function GetMaxRunTimeMs: integer;
    procedure SetMaxRunTimeMs(const ATimeMS: integer);

{$IFDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}
    procedure UseHtmlVariableEncoder;
{$ENDIF}
    function GetVariableEncoder: TTemplateEncodeFunction;
    procedure SetVariableEncoder(const AEncoder: TTemplateEncodeFunction);

    function TryGetFunction(const AName: string; out AFunction: TArray<TRttiMethod>): boolean;
    procedure SetFunctions(const AFunctions: ITemplateFunctions);
    function GetFunctions(): ITemplateFunctions; overload;

    function GetNewLine: string;
    procedure SetNewLine(const ANewLine: string);

    procedure ApplyTo(const AScope: TStackFrame);

    function GetStreamWriterProvider: TStreamWriterProvider;
    procedure SetStreamWriterProvider(const AProvider: TStreamWriterProvider);

    function GetValueSeparator: char;
    function GetDecimalSeparator: char;

    function GetFormatSettings: TFormatSettings;

    procedure SetValueSeparator(const ASeparator: char);
    procedure SetDecimalSeparator(const ASeparator: char);

    function GetDebugErrorFormat: string;
    procedure SetDebugErrorFormat(const AFormat: string);

    function GetWhitespace: char;
    procedure SetWhiteSpace(const AWS: char);

  end;

function CreateTemplateContext(const AOptions: TTemplateEvaluationOptions): ITemplateContext;
begin
  exit(TTemplateContext.Create(AOptions));
end;

{ TTemplateContext }

procedure TTemplateContext.SetTemplate(const AName: string; const ATemplate: ITemplate);
begin
  FLock.Acquire;
  try
    FTemplates.AddOrSetValue(AName, ATemplate);
  finally
    FLock.Release;
  end;
end;

procedure TTemplateContext.AddBlock(const AName: string; const ABlock: IBlockStmt);
begin
  if not assigned(FEvaluationContext) then
    exit;
  FEvaluationContext.AddBlock(AName, ABlock);
end;

procedure TTemplateContext.ApplyTo(const AScope: TStackFrame);
var
  LPair: TPair<string, TValue>;
begin
  for LPair in FVariables do
    AScope[LPair.Key] := LPair.Value;
end;

procedure TTemplateContext.ClearTemplates;
begin
  FLock.Acquire;
  try
    FTemplates.Clear();
  finally
    FLock.Release;
  end;
end;

constructor TTemplateContext.Create(const AOptions: TTemplateEvaluationOptions);
begin
  FOptions := AOptions + [eoFlattenTemplate, eoOptimiseTemplate];
  FMaxRuntimeMs := GDefaultRuntimeMS;
  FPrettyPrintOutput := GPrettyPrintOutput;
  SetEncoding(GDefaultEncoding);
  FStartToken := GDefaultOpenTag;
  FEndToken := GDefaultCloseTag;
  FStartStripToken := GDefaultOpenStripWSTag;
  FEndStripToken := GDefaultCloseWSTag;
  FTemplates := TDictionary<string, ITemplate>.Create;
  FVariables := TTemplateVariables.Create;
  FFunctions := GFunctions;
  FLock := TCriticalSection.Create;
  FNewLine := GNewLine;
  FStreamWriterProvider := GStreamWriterProvider;
  FWhiteSpace := #32;
  FFormatSettings := TFormatSettings.Create;
  SetDecimalSeparator(FFormatSettings.DecimalSeparator);
  FDebugFormat := FNewLine + FNewLine + 'ERROR: %s' + FNewLine + FNewLine;
end;

destructor TTemplateContext.Destroy;
begin
  FTemplates.Free;
  FLock.Free;
  inherited;
end;

procedure TTemplateContext.EndEvaluation;
begin
  FreeAndNil(FEvaluationContext);
end;

function TTemplateContext.TryGetBlock(const AName: string; out ABlock: IBlockStmt): boolean;
begin
  if not assigned(FEvaluationContext) then
    exit(false);
  exit(FEvaluationContext.TryGetBlock(AName, ABlock));
end;

function TTemplateContext.TryGetContextTemplate(const AName: string; out ATemplate: ITemplate; const AResolveContext: TTemplateValue): boolean;
begin
  FLock.Acquire;
  try
    exit(FTemplates.TryGetValue(AName, ATemplate));
  finally
    FLock.Release;
  end;
end;

function TTemplateContext.TryGetFunction(const AName: string; out AFunction: TArray<TRttiMethod>): boolean;
begin
  FLock.Acquire;
  try
    if not FFunctionsSet and not(eoNoDefaultFunctions in FOptions) then
    begin
      if FFunctions.IsEmpty then
        FFunctions.RegisterDefaults;
      FFunctionsSet := true;
    end;
    exit(FFunctions.TryGetValue(AName.ToLower, AFunction));
  finally
    FLock.Release;
  end;
end;

function TTemplateContext.TryGetTemplate(const AName: string; out ATemplate: ITemplate): boolean;
begin
  exit(TryGetTemplate(AName, ATemplate, ''));
end;

function TTemplateContext.GetDebugErrorFormat: string;
begin
  exit(FDebugFormat);
end;

function TTemplateContext.GetDecimalSeparator: char;
begin
  exit(FFormatSettings.DecimalSeparator);
end;

function TTemplateContext.GetEncoding: TEncoding;
begin
  exit(FEncoding);
end;

function TTemplateContext.GetFormatSettings: TFormatSettings;
begin
  exit(FFormatSettings);
end;

function TTemplateContext.GetFunctions: ITemplateFunctions;
begin
  exit(FFunctions);
end;

function TTemplateContext.GetMaxRunTimeMs: integer;
begin
  exit(FMaxRuntimeMs);
end;

function TTemplateContext.GetNewLine: string;
begin
  exit(FNewLine);
end;

function TTemplateContext.GetOptions: TTemplateEvaluationOptions;
begin
  exit(FOptions);
end;

function TTemplateContext.GetPrettyPrintOutput: TPrettyPrintOutput;
begin
  result := FPrettyPrintOutput;
end;

function TTemplateContext.GetVariable(const AName: string): TValue;
begin
  FLock.Acquire;
  try
    exit(FVariables[AName]);
  finally
    FLock.Release;
  end;
end;

function TTemplateContext.GetVariableEncoder: TTemplateEncodeFunction;
begin
  result := FVariableEncoder;
end;

function TTemplateContext.GetVariables: ITemplateVariables;
begin
  exit(FVariables);
end;

function TTemplateContext.GetWhitespace: char;
begin
  exit(FWhiteSpace);
end;

procedure TTemplateContext.RemoveBlock(const AName: string);
begin
  if not assigned(FEvaluationContext) then
    exit;
  FEvaluationContext.RemoveBlock(AName);
end;

procedure TTemplateContext.RemoveTemplate(const AName: string);
begin
  FLock.Acquire;
  try
    FTemplates.Remove(AName);
  finally
    FLock.Release;
  end;
end;

function TTemplateContext.GetScriptEndStripToken: string;
begin
  exit(FEndStripToken);
end;

function TTemplateContext.GetScriptEndToken: string;
begin
  exit(FEndToken);
end;

function TTemplateContext.GetScriptStartStripToken: string;
begin
  exit(FStartStripToken);
end;

function TTemplateContext.GetScriptStartToken: string;
begin
  exit(FStartToken);
end;

function TTemplateContext.GetStreamWriterProvider: TStreamWriterProvider;
begin
  result := FStreamWriterProvider;
end;

function TTemplateContext.GetTemplate(const AName: string): ITemplate;
begin
  if not TryGetTemplate(AName, result) then
    exit(nil);
end;

function TTemplateContext.GetTemplateResolver: TTemplateResolver;
begin
  result := function(const AContext: ITemplateContext; const AName: string): ITemplate
    var
      LAddToContext: boolean; // this is actually ignored 
    begin
      exit(FTemplateResolver(AContext, AName, '', LAddToContext));
    end;
end;

function TTemplateContext.GetTemplateResolverWithContext: TTemplateResolverWithContext;
begin
  result := FTemplateResolver;
end;

function TTemplateContext.GetValueSeparator: char;
begin
  exit(FValueSeparator);
end;

procedure TTemplateContext.SetDebugErrorFormat(const AFormat: string);
begin
  FDebugFormat := AFormat;
end;

procedure TTemplateContext.SetDecimalSeparator(const ASeparator: char);
begin
  FFormatSettings.DecimalSeparator := ASeparator;
{$WARN WIDECHAR_REDUCED OFF}
  if not(FFormatSettings.DecimalSeparator in ['.', ',']) then
    raise ETemplate.CreateRes(@SDecimalSeparatorMustBeACommaOrFullStop);
{$WARN WIDECHAR_REDUCED ON}
end;

procedure TTemplateContext.SetEncoding(const AEncoding: TEncoding);
begin
  FEncoding := AEncoding;
end;

procedure TTemplateContext.SetFunctions(const AFunctions: ITemplateFunctions);
begin
  FFunctions := AFunctions;
  FFunctionsSet := true;
end;

procedure TTemplateContext.SetMaxRunTimeMs(const ATimeMS: integer);
begin
  FMaxRuntimeMs := ATimeMS;
end;

procedure TTemplateContext.SetNewLine(const ANewLine: string);
begin
  FNewLine := ANewLine;
end;

procedure TTemplateContext.SetOptions(const AOptions: TTemplateEvaluationOptions);
begin
  FOptions := AOptions;
  if eoOptimiseTemplate in FOptions then
    include(FOptions, eoFlattenTemplate);
end;

procedure TTemplateContext.SetPrettyPrintOutput(const APrettyPrintOutput: TPrettyPrintOutput);
begin
  FPrettyPrintOutput := APrettyPrintOutput;
end;

procedure TTemplateContext.SetValueSeparator(const ASeparator: char);
begin
{$WARN WIDECHAR_REDUCED OFF}
  if not(ASeparator in [',', ';']) then
    raise ETemplate.CreateRes(@SDecimalSeparatorMustBeACommaOrFullStop);
{$WARN WIDECHAR_REDUCED ON}
  FValueSeparator := ASeparator;
end;

procedure TTemplateContext.SetVariable(const AName: string; const AValue: TValue);
begin
  FLock.Acquire;
  try
    FVariables[AName] := AValue;
  finally
    FLock.Release;
  end;
end;

procedure TTemplateContext.SetVariableEncoder(const AEncoder: TTemplateEncodeFunction);
begin
  FVariableEncoder := AEncoder;
end;

procedure TTemplateContext.SetWhiteSpace(const AWS: char);
begin
  FWhiteSpace := AWS;
end;

procedure TTemplateContext.StartEvaluation;
begin
  FEvaluationContext := TEvaluationContext.Create;
end;

procedure TTemplateContext.SetScriptEndStripToken(const Value: string);
begin
  FEndStripToken := Value;
end;

procedure TTemplateContext.SetScriptEndToken(const AToken: string);
begin
  FEndToken := AToken;
end;

procedure TTemplateContext.SetScriptStartStripToken(const Value: string);
begin
  FStartStripToken := Value;
end;

procedure TTemplateContext.SetScriptStartToken(const AToken: string);
begin
  FStartToken := AToken;
end;

procedure TTemplateContext.SetStreamWriterProvider(const AProvider: TStreamWriterProvider);
begin
  FStreamWriterProvider := AProvider;
end;

procedure TTemplateContext.SetTemplateResolver(const AResolver: TTemplateResolver);
begin
  FTemplateResolver := function(const AContext: ITemplateContext; const AName: string; const AResolveContext: TTemplateValue; out AAddToContext: boolean): ITemplate
    begin
      AAddToContext := false;
      exit(AResolver(AContext, AName));
    end
end;

procedure TTemplateContext.SetTemplateResolverWithContext(const AResolver: TTemplateResolverWithContext);
begin
  FTemplateResolver := AResolver;
end;

function TTemplateContext.TryGetTemplate(const AName: string; out ATemplate: ITemplate; const AResolveContext: TTemplateValue): boolean;
var
  LAddToContext: boolean;
begin
  FLock.Acquire;
  try
    result := FTemplates.TryGetValue(AName, ATemplate);
    if result then
      exit(true);
  finally
    FLock.Release;
  end;
  if not assigned(FTemplateResolver) then
    exit(false);
  ATemplate := FTemplateResolver(self, AName, AResolveContext, LAddToContext);
  if ATemplate = nil then
    exit(false);
  if LAddToContext then
    SetTemplate(AName, ATemplate);
  exit(true);
end;

function TTemplateContext.TryGetVariable(const AName: string; out AValue: TValue): boolean;
begin
  FLock.Enter;
  try
    exit(FVariables.TryGetItem(AName, AValue));
  finally
    FLock.Leave;
  end;
end;

{$IFDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}

function HtmlEncode(const AString: string): string;
begin
{$IFDEF SUPPORT_NET_ENCODING}
  exit(TNetEncoding.HTML.Encode(AString));
{$ENDIF}
{$IFDEF SEMPARE_TEMPLATE_INDY}
  exit(StrHtmlEncode(AString));
{$ENDIF}
end;

procedure TTemplateContext.UseHtmlVariableEncoder;
begin
  FVariableEncoder := HtmlEncode;
end;

{$ENDIF}
{ TUTF8WithoutPreambleEncoding }

function TUTF8WithoutPreambleEncoding.GetPreamble: TBytes;
begin
  setlength(result, 0);
end;

{ TEvaluationContext }

procedure TEvaluationContext.AddBlock(const AName: string; const ABlock: IBlockStmt);
var
  LStack: TStack<IBlockStmt>;
begin
  if not FBlocks.TryGetValue(AName, LStack) then
  begin
    LStack := TStack<IBlockStmt>.Create;
    FBlocks.AddOrSetValue(AName, LStack);
  end;
  LStack.Push(ABlock)
end;

constructor TEvaluationContext.Create;
begin
  FBlocks := TObjectDictionary < string, TStack < IBlockStmt >>.Create([doOwnsValues]);
end;

destructor TEvaluationContext.Destroy;
begin
  FBlocks.Free;
  inherited;
end;

procedure TEvaluationContext.RemoveBlock(const AName: string);
var
  LStack: TStack<IBlockStmt>;
begin
  if not FBlocks.TryGetValue(AName, LStack) then
    exit;
  LStack.pop;
  if LStack.Count = 0 then
    FBlocks.Remove(AName);
end;

function TEvaluationContext.TryGetBlock(const AName: string; out ABlock: IBlockStmt): boolean;
var
  LStack: TStack<IBlockStmt>;
begin
  if not FBlocks.TryGetValue(AName, LStack) then
    exit(false);
  ABlock := LStack.Peek;
  exit(true);
end;

initialization

// setup our global
GUTF8WithoutPreambleEncoding := TUTF8WithoutPreambleEncoding.Create;

GDefaultEncoding := TEncoding.UTF8WithoutBOM;
GStreamWriterProvider := function(const AStream: TStream; const AContext: ITemplateContext): TStreamWriter
  begin
    exit(TStreamWriter.Create(AStream, AContext.Encoding, 4096));
  end;

GPrettyPrintOutput := procedure(const APrettyPrint: string)
  begin
  end;

finalization

GUTF8WithoutPreambleEncoding.Free;

end.
