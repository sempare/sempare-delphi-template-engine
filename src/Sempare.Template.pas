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
 * Copyright (c) 2020 Sempare Limited                                                               *
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

uses
  System.Classes,
  System.SysUtils,
  Sempare.Template.Common,
  Sempare.Template.Context,
  Sempare.Template.AST,
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
  // eoDebug = TTemplateEvaluationOption.eoDebug;
  eoPrettyPrint = TTemplateEvaluationOption.eoPrettyPrint;
  eoRaiseErrorWhenVariableNotFound = TTemplateEvaluationOption.eoRaiseErrorWhenVariableNotFound;
  eoReplaceNewline = TTemplateEvaluationOption.eoReplaceNewline;

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

  Template = class
  public
    class function Context(AOptions: TTemplateEvaluationOptions = []): ITemplateContext; inline; static;
    class function Parser(AContext: ITemplateContext): ITemplateParser; overload; inline; static;
    class function Parser(): ITemplateParser; overload; inline; static;
    class function PrettyPrint(ATemplate: ITemplate): string; inline; static;

    // EVAL output to stream

    class procedure Eval(const ATemplate: string; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval(ATemplate: ITemplate; const AStream: TStream; const AOptions: TTemplateEvaluationOptions = []); overload; static;
    class procedure Eval<T>(AContext: ITemplateContext; ATemplate: ITemplate; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval<T>(AContext: ITemplateContext; const ATemplate: string; const AValue: T; const AStream: TStream); overload; static;
    class procedure Eval(AContext: ITemplateContext; const ATemplate: string; const AStream: TStream); overload; static;
    class procedure Eval(AContext: ITemplateContext; ATemplate: ITemplate; const AStream: TStream); overload; static;

    // EVAL returning string

    class function Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval<T>(ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;
    class function Eval(ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions = []): string; overload; static;

    class function Eval<T>(AContext: ITemplateContext; ATemplate: ITemplate; const AValue: T): string; overload; static;
    class function Eval<T>(AContext: ITemplateContext; const ATemplate: string; const AValue: T): string; overload; static;
    class function Eval(AContext: ITemplateContext; const ATemplate: string): string; overload; static;
    class function Eval(AContext: ITemplateContext; ATemplate: ITemplate): string; overload; static;

    // PARSING

    // string operations
    class function Parse(const AString: string): ITemplate; overload; static;
    class function Parse(AContext: ITemplateContext; const AString: string): ITemplate; overload; static;

    // stream operations
    class function Parse(const AStream: TStream): ITemplate; overload; static;
    class function Parse(AContext: ITemplateContext; const AStream: TStream): ITemplate; overload; static;

    // file operations
    class function ParseFile(const AFile: string): ITemplate; overload; static;
    class function ParseFile(AContext: ITemplateContext; const AFile: string): ITemplate; overload; static;

  end;

  TEncodingHelper = class helper for TEncoding
    class function GetUTF8WithoutBOM: TEncoding; static;
    class property UTF8WithoutBOM: TEncoding read GetUTF8WithoutBOM;
  end;

implementation

uses
  Sempare.Template.Evaluate,
  Sempare.Template.PrettyPrint;

type
  TEmpty = TObject;

var
  GEmpty: TEmpty;

  { Template }

class function Template.Context(AOptions: TTemplateEvaluationOptions): ITemplateContext;
begin
  exit(Sempare.Template.Context.CreateTemplateContext(AOptions));
end;

class procedure Template.Eval<T>(ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(Context(AOptions), ATemplate, AValue, AStream);
end;

class procedure Template.Eval<T>(AContext: ITemplateContext; ATemplate: ITemplate; const AValue: T; const AStream: TStream);
var
  LValue: TTemplateValue;
begin
  LValue := TTemplateValue.From<T>(AValue);
  if typeinfo(T) = typeinfo(TTemplateValue) then
    LValue := LValue.AsType<TTemplateValue>();
  AcceptVisitor(ATemplate, TEvaluationTemplateVisitor.Create(AContext, LValue, AStream));
end;

class procedure Template.Eval(ATemplate: ITemplate; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(ATemplate, GEmpty, AStream, AOptions);
end;

class procedure Template.Eval(const ATemplate: string; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(ATemplate, GEmpty, AStream, AOptions);
end;

class procedure Template.Eval<T>(const ATemplate: string; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(Template.Parse(ATemplate), AValue, AStream, AOptions);
end;

class function Template.Parse(const AStream: TStream): ITemplate;
begin
  exit(Parser.Parse(AStream));
end;

class function Template.Parser(AContext: ITemplateContext): ITemplateParser;
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

class function Template.PrettyPrint(ATemplate: ITemplate): string;
var
  LVisitor: ITemplateVisitor;
begin
  LVisitor := TPrettyPrintTemplateVisitor.Create();
  AcceptVisitor(ATemplate, LVisitor);
  exit(TPrettyPrintTemplateVisitor(LVisitor).ToString);
end;

class procedure Template.Eval(AContext: ITemplateContext; const ATemplate: string; const AStream: TStream);
begin
  Eval(AContext, Template.Parse(AContext, ATemplate), GEmpty, AStream);
end;

class procedure Template.Eval<T>(AContext: ITemplateContext; const ATemplate: string; const AValue: T; const AStream: TStream);
begin
  Eval(AContext, Parse(ATemplate), AValue, AStream);
end;

class function Template.Parse(AContext: ITemplateContext; const AStream: TStream): ITemplate;
begin
  exit(Parser(AContext).Parse(AStream));
end;

class function Template.ParseFile(AContext: ITemplateContext; const AFile: string): ITemplate;
type
{$IFDEF SUPPORT_BUFFERED_STREAM}
  TFStream = TBufferedFileStream;
{$ELSE}
  TFStream = TFileStream;
{$ENDIF}
var
  LFileStream: TFStream;
begin
  LFileStream := TFStream.Create(AFile, fmOpenRead);
  exit(Parse(AContext, LFileStream));
end;

class function Template.ParseFile(const AFile: string): ITemplate;
begin
  exit(ParseFile(Context, AFile));
end;

class function Template.Parse(AContext: ITemplateContext; const AString: string): ITemplate;
begin
  exit(Parser(AContext).Parse(TStringStream.Create(AString, AContext.Encoding, false), true));
end;

class function Template.Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions): string;
begin
  exit(Eval(Context(AOptions), ATemplate));
end;

class function Template.Eval(ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions): string;
begin
  exit(Eval(Context(AOptions), ATemplate));
end;

class function Template.Eval(AContext: ITemplateContext; const ATemplate: string): string;
begin
  exit(Eval(AContext, Template.Parse(AContext, ATemplate)));
end;

class function Template.Eval<T>(ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions): string;
begin
  exit(Eval(Context(AOptions), ATemplate, AValue));
end;

class function Template.Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions): string;
begin
  exit(Eval(Context(AOptions), ATemplate, AValue));
end;

class function Template.Eval<T>(AContext: ITemplateContext; ATemplate: ITemplate; const AValue: T): string;
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

class function Template.Eval<T>(AContext: ITemplateContext; const ATemplate: string; const AValue: T): string;
begin
  exit(Eval(AContext, Template.Parse(AContext, ATemplate), AValue));
end;

class function Template.Eval(AContext: ITemplateContext; ATemplate: ITemplate): string;
begin
  exit(Eval(AContext, ATemplate, GEmpty));
end;

class procedure Template.Eval(AContext: ITemplateContext; ATemplate: ITemplate; const AStream: TStream);
begin
  Eval(AContext, ATemplate, GEmpty, AStream);
end;

{ TEncodingHelper }

class function TEncodingHelper.GetUTF8WithoutBOM: TEncoding;
begin
  exit(GUTF8WithoutPreambleEncoding);
end;

initialization

GEmpty := TObject.Create;

finalization

GEmpty.Free;

end.
