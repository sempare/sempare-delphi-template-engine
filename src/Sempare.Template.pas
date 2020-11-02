(*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                        Sempare Templating Engine                                                 *
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
  result := Sempare.Template.Context.CreateTemplateContext(AOptions);
end;

class procedure Template.Eval<T>(ATemplate: ITemplate; const AValue: T; const AStream: TStream; const AOptions: TTemplateEvaluationOptions);
begin
  Eval(Context(AOptions), ATemplate, AValue, AStream);
end;

class procedure Template.Eval<T>(AContext: ITemplateContext; ATemplate: ITemplate; const AValue: T; const AStream: TStream);
var
  v: TTemplateValue;
begin
  v := TTemplateValue.From<T>(AValue);
  if typeinfo(T) = typeinfo(TTemplateValue) then
    v := v.AsType<TTemplateValue>();
  AcceptVisitor(ATemplate, TEvaluationTemplateVisitor.Create(AContext, v, AStream));
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
  result := Parser.Parse(AStream);
end;

class function Template.Parser(AContext: ITemplateContext): ITemplateParser;
begin
  result := CreateTemplateParser(AContext);
end;

class function Template.Parse(const AString: string): ITemplate;
begin
  result := Parse(Context(), AString);
end;

class function Template.Parser: ITemplateParser;
begin
  result := CreateTemplateParser(Context);
end;

class function Template.PrettyPrint(ATemplate: ITemplate): string;
var
  v: ITemplateVisitor;
begin
  v := TPrettyPrintTemplateVisitor.Create();
  AcceptVisitor(ATemplate, v);
  result := TPrettyPrintTemplateVisitor(v).ToString;
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
  result := Parser(AContext).Parse(AStream);
end;

class function Template.ParseFile(AContext: ITemplateContext; const AFile: string): ITemplate;
type
{$IFDEF SUPPORT_BUFFERED_STREAM}
  TFStream = TBufferedFileStream;
{$ELSE}
  TFStream = TFileStream;
{$ENDIF}
var
  fs: TFStream;
begin
  fs := TFStream.Create(AFile, fmOpenRead);
  try
    result := Parse(AContext, fs);
  finally
    fs.Free;
  end;
end;

class function Template.ParseFile(const AFile: string): ITemplate;
begin
  result := ParseFile(Context, AFile);
end;

class function Template.Parse(AContext: ITemplateContext; const AString: string): ITemplate;
begin
  result := Parser(AContext).Parse(TStringStream.Create(AString, AContext.Encoding, false), true);
end;

class function Template.Eval(const ATemplate: string; const AOptions: TTemplateEvaluationOptions): string;
begin
  result := Eval(Context(AOptions), ATemplate);
end;

class function Template.Eval(ATemplate: ITemplate; const AOptions: TTemplateEvaluationOptions): string;
begin
  result := Eval(Context(AOptions), ATemplate);
end;

class function Template.Eval(AContext: ITemplateContext; const ATemplate: string): string;
begin
  result := Eval(AContext, Template.Parse(AContext, ATemplate));
end;

class function Template.Eval<T>(ATemplate: ITemplate; const AValue: T; const AOptions: TTemplateEvaluationOptions): string;
begin
  result := Eval(Context(AOptions), ATemplate, AValue);
end;

class function Template.Eval<T>(const ATemplate: string; const AValue: T; const AOptions: TTemplateEvaluationOptions): string;
begin
  result := Eval(Context(AOptions), ATemplate, AValue);
end;

class function Template.Eval<T>(AContext: ITemplateContext; ATemplate: ITemplate; const AValue: T): string;
var
  s: TStringStream;
begin
  s := TStringStream.Create('', AContext.Encoding, false);
  try
    Eval(AContext, ATemplate, AValue, s);
    result := s.DataString;
  finally
    s.Free;
  end;
end;

class function Template.Eval<T>(AContext: ITemplateContext; const ATemplate: string; const AValue: T): string;
begin
  result := Eval(AContext, Template.Parse(AContext, ATemplate), AValue);
end;

class function Template.Eval(AContext: ITemplateContext; ATemplate: ITemplate): string;
begin
  result := Eval(AContext, ATemplate, GEmpty);
end;

class procedure Template.Eval(AContext: ITemplateContext; ATemplate: ITemplate; const AStream: TStream);
begin
  Eval(AContext, ATemplate, GEmpty, AStream);
end;

{ TEncodingHelper }

class function TEncodingHelper.GetUTF8WithoutBOM: TEncoding;
begin
  result := UTF8WithoutPreambleEncoding;
end;

initialization

GEmpty := TObject.Create;

finalization

GEmpty.Free;

end.
