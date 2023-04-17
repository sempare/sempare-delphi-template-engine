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
unit Sempare.Template;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Sempare.Template.Common,
  Sempare.Template.Context,
  Sempare.Template.AST,
  Sempare.Template.TemplateRegistry,
  Sempare.Template.Parser;

const
  eoStripRecurringSpaces = TTemplateEvaluationOption.eoStripRecurringSpaces;
  eoConvertTabsToSpaces = TTemplateEvaluationOption.eoConvertTabsToSpaces;
  eoNoDefaultFunctions = TTemplateEvaluationOption.eoNoDefaultFunctions;
  eoNoPosition = TTemplateEvaluationOption.eoNoPosition;
  eoEvalEarly = TTemplateEvaluationOption.eoEvalEarly;
  eoEvalVarsEarly = TTemplateEvaluationOption.eoEvalVarsEarly;
  eoStripRecurringNewlines = TTemplateEvaluationOption.eoStripRecurringNewlines;
  eoTrimLines = TTemplateEvaluationOption.eoTrimLines;
  eoEmbedException = TTemplateEvaluationOption.eoEmbedException;
  eoPrettyPrint = TTemplateEvaluationOption.eoPrettyPrint;
  eoRaiseErrorWhenVariableNotFound = TTemplateEvaluationOption.eoRaiseErrorWhenVariableNotFound;
  eoFlattenTemplate = TTemplateEvaluationOption.eoFlattenTemplate;
  eoOptimiseTemplate = TTemplateEvaluationOption.eoOptimiseTemplate;

  tlsLoadResource = Sempare.Template.TemplateRegistry.tlsLoadResource;
  tlsLoadFile = Sempare.Template.TemplateRegistry.tlsLoadFile;
  tlsLoadCustom = Sempare.Template.TemplateRegistry.tlsLoadCustom;

type
  TTemplateEvaluationOptions = Sempare.Template.Context.TTemplateEvaluationOptions;
  TTemplateEvaluationOption = Sempare.Template.Context.TTemplateEvaluationOption;
  TTemplateValue = Sempare.Template.AST.TTemplateValue;
  ITemplateContext = Sempare.Template.Context.ITemplateContext;
  ITemplate = Sempare.Template.AST.ITemplate;
  ITemplateFunctions = Sempare.Template.Context.ITemplateFunctions;
  TTemplateResolver = Sempare.Template.Context.TTemplateResolver;
  TTemplateEncodeFunction = Sempare.Template.Common.TTemplateEncodeFunction;
  ITemplateVariables = Sempare.Template.Common.ITemplateVariables;
  TUTF8WithoutPreambleEncoding = Sempare.Template.Context.TUTF8WithoutPreambleEncoding;
  TTemplateRegistry = Sempare.Template.TemplateRegistry.TTemplateRegistry;
  TTemplateLoadStrategy = Sempare.Template.TemplateRegistry.TTemplateLoadStrategy;
  TSempareServerPages = TTemplateRegistry;
  ETemplateNotResolved = Sempare.Template.TemplateRegistry.ETemplateNotResolved;
  TTempateLogMessage = Sempare.Template.TemplateRegistry.TTempateLogMessage;

  Template = class
{$INCLUDE 'Sempare.Template.Version.inc'}
{$IFNDEF SEMPARE_TEMPLATE_CONFIRM_LICENSE}
{$IFDEF MSWINDOWS}
  private
    class var FLicenseShown: boolean;
    class procedure Initialize;
{$ENDIF}
{$ENDIF}
  public
    class function Context(const AOptions: TTemplateEvaluationOptions = [eoOptimiseTemplate]): ITemplateContext; inline; static;
    class function Parser(const AContext: ITemplateContext): ITemplateParser; overload; inline; static;
    class function Parser(): ITemplateParser; overload; inline; static;
    class function PrettyPrint(const ATemplate: ITemplate): string; inline; static;

    // EVAL output to stream

    class procedure Eval(const ATemplate: string; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval(const ATemplate: ITemplate; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval(const AContext: ITemplateContext; const ATemplate: string; const AStream: TStream); overload; static;
    class procedure Eval(const AContext: ITemplateContext; const ATemplate: ITemplate; const AStream: TStream); overload; static;

    class procedure EvalWithContext(const AContext: ITemplateContext; const ATemplate: ITemplate; const AResolveContext: TTemplateValue; const AValue: TTemplateValue; const AStream: TStream); overload; static;
    class function EvalWithContext(const AContext: ITemplateContext; const ATemplate: ITemplate; const AResolveContext: TTemplateValue; const AValue: TTemplateValue): string; overload; static;

    // EVAL returning string

    class function Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval(const ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;

    class function Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T): string; overload; static;
    class function Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T): string; overload; static;
    class function Eval(const AContext: ITemplateContext; const ATemplate: string): string; overload; static;
    class function Eval(const AContext: ITemplateContext; const ATemplate: ITemplate): string; overload; static;

    // TEMPLATE REGISTRY

    class function Resolver(): TTemplateRegistry; static; inline;

    class procedure Resolve<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream); overload; static;
    class function Resolve<T>(const ATemplateName: string; const AData: T): string; overload; static;

    class procedure Resolve(const ATemplateName: string; const AOutputStream: TStream); overload; static;
    class function Resolve(const ATemplateName: string): string; overload; static;

    // with context

    class procedure ResolveWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream); overload; static;
    class function ResolveWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T): string; overload; static;

    class procedure ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream); overload; static;
    class function ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string; overload; static;

    // PARSING

    // string operations
    class function Parse(const AString: string): ITemplate; overload; static;
    class function Parse(const AContext: ITemplateContext; const AString: string): ITemplate; overload; static;

    // stream operations
    class function Parse(const AStream: TStream; const AManagedStream: boolean = true): ITemplate; overload; static;
    class function Parse(const AContext: ITemplateContext; const AStream: TStream; const AManagedStream: boolean = true): ITemplate; overload; static;

    // file operations
    class function ParseFile(const AFile: string): ITemplate; overload; static;
    class function ParseFile(const AContext: ITemplateContext; const AFile: string): ITemplate; overload; static;

    // extract references

    class procedure ExtractReferences(const ATemplate: ITemplate; out AVariables: TArray<string>; out AFunctions: TArray<string>); static;

    class procedure ExtractBlocks(const ATemplate: ITemplate; var ABlocks: TDictionary<string, ITemplate>); static;

    class function Version(): string; static;
  end;

  TEncodingHelper = class helper for TEncoding
    class function GetUTF8WithoutBOM: TEncoding; static;
    class property UTF8WithoutBOM: TEncoding read GetUTF8WithoutBOM;
  end;

implementation

uses
{$IFNDEF SEMPARE_TEMPLATE_CONFIRM_LICENSE}
{$IFDEF MSWINDOWS}
  VCL.Dialogs,
{$ENDIF}
{$ENDIF}
  System.Rtti,
  Sempare.Template.Evaluate,
  Sempare.Template.VariableExtraction,
  Sempare.Template.BlockResolver,
  Sempare.Template.PrettyPrint;

{ Template }

class function Template.Context(const AOptions: TTemplateEvaluationOptions): ITemplateContext;
begin
  exit(Sempare.Template.Context.CreateTemplateContext(AOptions));
end;

class procedure Template.Eval<T>(const ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
var
  LContext: ITemplateContext;
begin
  LContext := Context(AOptions);
  Eval(LContext, ATemplate, AValue, AStream);
end;

class procedure Template.Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T; const AStream: TStream);
var
  LTemplateVisitor: IEvaluationTemplateVisitor;
begin
{$IFNDEF SEMPARE_TEMPLATE_CONFIRM_LICENSE}
{$IFDEF MSWINDOWS}
  if not FLicenseShown then
  begin
    ShowMessage( //
      'Thank you for trying the Sempare Template Engine.'#13#10#13#10 + //
      'To supress this message, set the conditional define SEMPARE_TEMPLATE_CONFIRM_LICENSE in the project options.'#13#10#13#10 + //
      'Please remember the library is dual licensed. You are free to use it under the GPL or you can support the project to keep it alive as per:'#13#10#13#10 + //
      'https://github.com/sempare/sempare-delphi-template-engine/blob/main/docs/commercial.license.md' //
      );
    FLicenseShown := true;
  end;
{$ENDIF}
{$ENDIF}
  LTemplateVisitor := TEvaluationTemplateVisitor.Create(AContext, '', TTemplateValue.From<T>(AValue), AStream);
  AcceptVisitor(ATemplate, LTemplateVisitor);
end;

class procedure Template.Eval(const ATemplate: ITemplate; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(ATemplate, '', AStream, AOptions);
end;

class procedure Template.Eval(const ATemplate: string; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(ATemplate, '', AStream, AOptions);
end;

class procedure Template.Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.Parse(ATemplate);
  Eval(LTemplate, AValue, AStream, AOptions);
end;

class function Template.Parse(const AStream: TStream; const AManagedStream: boolean): ITemplate;
begin
  exit(Parser.Parse(AStream, AManagedStream));
end;

class function Template.Parser(const AContext: ITemplateContext): ITemplateParser;
begin
  exit(CreateTemplateParser(AContext));
end;

class function Template.Parse(const AString: string): ITemplate;
begin
  exit(Parse(Context(), AString));
end;

class function Template.Parser: ITemplateParser;
begin
  exit(CreateTemplateParser(Context));
end;

class function Template.PrettyPrint(const ATemplate: ITemplate): string;
var
  LVisitor: ITemplateVisitor;
  LTemplateVisitor: TPrettyPrintTemplateVisitor;
begin
  if not assigned(ATemplate) then
    exit('');
  LTemplateVisitor := TPrettyPrintTemplateVisitor.Create();
  LVisitor := LTemplateVisitor;
  AcceptVisitor(ATemplate, LVisitor);
  exit(LTemplateVisitor.ToString);
end;

class function Template.Resolve(const ATemplateName: string): string;
begin
  exit(Resolver.Eval(ATemplateName));
end;

class procedure Template.Resolve(const ATemplateName: string; const AOutputStream: TStream);
begin
  Resolver.Eval(ATemplateName, AOutputStream);
end;

class function Template.Resolve<T>(const ATemplateName: string; const AData: T): string;
begin
  exit(Resolver.Eval(ATemplateName, AData));
end;

class function Template.Resolver: TTemplateRegistry;
begin
  exit(TTemplateRegistry.Instance);
end;

class procedure Template.Resolve<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream);
begin
  Resolver.Eval(ATemplateName, AData, AOutputStream);
end;

class procedure Template.ResolveWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream);
begin
  Resolver.EvalWithContext(ATemplateName, AContext, AData, AOutputStream);
end;

class function Template.ResolveWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T): string;
begin
  exit(Resolver.EvalWithContext(ATemplateName, AContext, AData));
end;

class function Template.ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string;
begin
  exit(Resolver.EvalWithContext(ATemplateName, AContext));
end;

class procedure Template.ResolveWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream);
begin
  Resolver.EvalWithContext(ATemplateName, AContext, AOutputStream);
end;

class function Template.Version: string;
begin
  exit(format('%d.%d.%d', [MAJOR_VERSION, MINOR_VERSION, PATCH_VERSION]));
end;

class procedure Template.Eval(const AContext: ITemplateContext; const ATemplate: string; const AStream: TStream);
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.Parse(AContext, ATemplate);
  Eval(AContext, LTemplate, '', AStream);
end;

class procedure Template.Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T; const AStream: TStream);
var
  LTemplate: ITemplate;
begin
  LTemplate := Parse(ATemplate);
  Eval(AContext, LTemplate, AValue, AStream);
end;

class function Template.Parse(const AContext: ITemplateContext; const AStream: TStream; const AManagedStream: boolean): ITemplate;
var
  LParser: ITemplateParser;
begin
  LParser := Parser(AContext);
  exit(LParser.Parse(AStream, AManagedStream));
end;

class function Template.ParseFile(const AContext: ITemplateContext; const AFile: string): ITemplate;
type
{$IFDEF SUPPORT_BUFFERED_STREAM}
  TFStream = TBufferedFileStream;
{$ELSE}
  TFStream = TFileStream;
{$ENDIF}
var
  LFileStream: TFStream;
begin
  LFileStream := TFStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  exit(Parse(AContext, LFileStream));
end;

class function Template.ParseFile(const AFile: string): ITemplate;
begin
  exit(ParseFile(Context, AFile));
end;

class function Template.Parse(const AContext: ITemplateContext; const AString: string): ITemplate;
var
  LParser: ITemplateParser;
  LStream: TStream;
begin
  LParser := Parser(AContext);
  LStream := TStringStream.Create(AString, AContext.Encoding, false);
  exit(LParser.Parse(LStream, true));
end;

class function Template.Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions): string;
var
  LContext: ITemplateContext;
begin
  LContext := Context(AOptions);
  exit(Eval(LContext, ATemplate));
end;

class function Template.Eval(const ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions): string;
var
  LContext: ITemplateContext;
begin
  LContext := Context(AOptions);
  exit(Eval(LContext, ATemplate));
end;

class function Template.Eval(const AContext: ITemplateContext; const ATemplate: string): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.Parse(AContext, ATemplate);
  exit(Eval(AContext, LTemplate));
end;

class function Template.Eval<T>(const ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions): string;
var
  LContext: ITemplateContext;
begin
  LContext := Context(AOptions);
  exit(Eval(LContext, ATemplate, AValue));
end;

class function Template.Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions): string;
var
  LContext: ITemplateContext;
begin
  LContext := Context(AOptions);
  exit(Eval(LContext, ATemplate, AValue));
end;

class function Template.Eval<T>(const AContext: ITemplateContext; const ATemplate: ITemplate; const AValue: T): string;
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create('', AContext.Encoding, false);
  try
    Eval(AContext, ATemplate, AValue, LStringStream);
    exit(LStringStream.DataString);
  finally
    LStringStream.Free;
  end;
end;

class function Template.Eval<T>(const AContext: ITemplateContext; const ATemplate: string; const AValue: T): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := Template.Parse(AContext, ATemplate);
  exit(Eval(AContext, LTemplate, AValue));
end;

class function Template.EvalWithContext(const AContext: ITemplateContext; const ATemplate: ITemplate; const AResolveContext: TTemplateValue; const AValue: TTemplateValue): string;
var
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create('', AContext.Encoding, false);
  try
    EvalWithContext(AContext, ATemplate, AResolveContext, AValue, LStringStream);
    exit(LStringStream.DataString);
  finally
    LStringStream.Free;
  end;
end;

class procedure Template.EvalWithContext(const AContext: ITemplateContext; const ATemplate: ITemplate; const AResolveContext: TTemplateValue; const AValue: TTemplateValue; const AStream: TStream);
var
  LTemplateVisitor: IEvaluationTemplateVisitor;
begin
{$IFNDEF SEMPARE_TEMPLATE_CONFIRM_LICENSE}
{$IFDEF MSWINDOWS}
  if not FLicenseShown then
  begin
    ShowMessage( //
      'Thank you for trying the Sempare Template Engine.'#13#10#13#10 + //
      'To supress this message, set the conditional define SEMPARE_TEMPLATE_CONFIRM_LICENSE in the project options.'#13#10#13#10 + //
      'Please remember the library is dual licensed. You are free to use it under the GPL or you can support the project to keep it alive as per:'#13#10#13#10 + //
      'https://github.com/sempare/sempare-delphi-template-engine/blob/main/docs/commercial.license.md' //
      );
    FLicenseShown := true;
  end;
{$ENDIF}
{$ENDIF}
  LTemplateVisitor := TEvaluationTemplateVisitor.Create(AContext, AResolveContext, AValue, AStream);
  AcceptVisitor(ATemplate, LTemplateVisitor);
end;

class procedure Template.ExtractBlocks(const ATemplate: ITemplate; var ABlocks: TDictionary<string, ITemplate>);
var
  LStringStream: TStringStream;
  LContext: ITemplateContext;
  LEvalVisitor: IEvaluationTemplateVisitor;
  LVisitor: IBlockResolverVisitor;
  LName: string;
begin
  assert(ATemplate <> nil);
  assert(ABlocks <> nil);
  LStringStream := nil;
  try
    LStringStream := TStringStream.Create;
    LContext := Template.Context();
    LEvalVisitor := TEvaluationTemplateVisitor.Create(LContext, '', '', LStringStream);
    LVisitor := TBlockResolverVisitor.Create(LEvalVisitor, ATemplate);
    LVisitor.Discover;
    for LName in LVisitor.GetBlockNames do
    begin
      ABlocks.Add(LName, LVisitor.GetBlock(LName).Container);
    end;
  finally
    LStringStream.Free;
  end;
end;

class procedure Template.ExtractReferences(const ATemplate: ITemplate; out AVariables: TArray<string>; out AFunctions: TArray<string>);
var
  LVisitor: ITemplateVisitor;
  LExtractionVisitor: TTemplateReferenceExtractionVisitor;
begin
  LExtractionVisitor := TTemplateReferenceExtractionVisitor.Create();
  LVisitor := LExtractionVisitor;
  AcceptVisitor(ATemplate, LVisitor);
  AVariables := LExtractionVisitor.Variables;
  AFunctions := LExtractionVisitor.Functions;
end;

{$IFNDEF SEMPARE_TEMPLATE_CONFIRM_LICENSE}
{$IFDEF MSWINDOWS}

class procedure Template.Initialize;
begin
  FLicenseShown := false;
end;
{$ENDIF}
{$ENDIF}

class function Template.Eval(const AContext: ITemplateContext; const ATemplate: ITemplate): string;
begin
  exit(Eval(AContext, ATemplate, ''));
end;

class procedure Template.Eval(const AContext: ITemplateContext; const ATemplate: ITemplate; const AStream: TStream);
begin
  Eval(AContext, ATemplate, '', AStream);
end;

{ TEncodingHelper }

class function TEncodingHelper.GetUTF8WithoutBOM: TEncoding;
begin
  exit(GUTF8WithoutPreambleEncoding);
end;

initialization

{$IFNDEF SEMPARE_TEMPLATE_CONFIRM_LICENSE}
{$IFDEF MSWINDOWS}
  Template.Initialize;
{$ENDIF}
{$ENDIF}

finalization

end.
