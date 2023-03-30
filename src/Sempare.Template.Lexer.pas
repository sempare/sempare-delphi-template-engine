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
unit Sempare.Template.Lexer;

interface

uses
  System.Classes,
  System.SysUtils,
  Sempare.Template.AST,
  Sempare.Template.Context;

type
  ITemplateValueSymbol = interface(ITemplateSymbol)
    ['{930E9892-38AA-4030-83CC-4069667B2E6E}']

    function GetValue: string;
    procedure SetValue(const AValue: string);
    property Value: string read GetValue write SetValue;
  end;

  ETemplateLexer = class(ETemplate);

  ITemplateLexer = interface
    ['{839FAB50-B21E-4C31-ACAA-2E50AEAA1456}']

    function GetToken: ITemplateSymbol;
  end;

function TemplateSymbolToString(const ASymbol: TTemplateSymbol): string;
function CreateTemplateLexer(const AContext: ITemplateContext; const AStream: TStream; const AFilename: string = ''; const AManageStream: Boolean = True): ITemplateLexer;

implementation

uses
  TypInfo,
  System.RegularExpressions,
  System.Generics.Collections,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Common;

var
  GKeywords: TDictionary<string, TTemplateSymbol>;
  GSymbolToKeyword: TDictionary<TTemplateSymbol, string>;

type
  TTemplateLexer = class(TInterfacedObject, ITemplateLexer)
  type
    TState = (SText, SScript);
{$WARN WIDECHAR_REDUCED OFF}
    TCharSet = set of char;
{$WARN WIDECHAR_REDUCED ON}

    TPair = record
      Input: char;
      Eof: Boolean;
      constructor Create(const AInput: char; const AEof: Boolean);
    end;
  private
    class var FIDRegex: TRegEx;
    class constructor Create; overload;
  private
    FReader: TStreamReader;
    FNextToken: ITemplateSymbol;
    FStream: TStream;
    FLine: integer;
    FPos: integer;
    FFilename: string;
    FLookahead: TPair;
    FCurrent: TPair;
    FManageStream: Boolean;
    FState: TState;
    FAccumulator: TStringBuilder;
    FPrevLineOffset: integer;
    FLineOffset: integer;
    FStartScript: string;
    FEndScript: string;
    FStartStripScript: string;
    FEndStripScript: string;
    FOptions: TTemplateEvaluationOptions;
    FContext: ITemplateContext;
    procedure GetInput;
    procedure SwallowInput; // a descriptive helper
    function Expecting(const Achar: char): Boolean; overload;
    function Expecting(const Achars: TCharSet): Boolean; overload;
    function GetTextToken: ITemplateSymbol;
    function GetScriptToken: ITemplateSymbol;
  public
    constructor Create(const AContext: ITemplateContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean = True); overload;
    destructor Destroy; override;
    function GetToken: ITemplateSymbol;
  end;

type
  TSimpleTemplateSymbol = class(TInterfacedObject, ITemplateSymbol)
  private
    FToken: TTemplateSymbol;
    FPosition: IPosition;
    FStripWS: Boolean;
    function GetPosition: IPosition;
  public
    constructor Create(const APosition: IPosition; const AToken: TTemplateSymbol; const AStripWS: Boolean = false);
    procedure SetToken(const AToken: TTemplateSymbol);
    function GetToken: TTemplateSymbol;
    function StripWS: Boolean;
  end;

  TTemplateValueSymbol = class(TSimpleTemplateSymbol, ITemplateValueSymbol)
  private
    FValue: string;
  public
    constructor Create(const APosition: IPosition; const AToken: TTemplateSymbol; const AString: string);
    procedure SetValue(const AValue: string);
    function GetValue: string;
  end;

function CreateTemplateLexer(const AContext: ITemplateContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean): ITemplateLexer;
begin
  exit(TTemplateLexer.Create(AContext, AStream, AFilename, AManageStream));
end;

function TemplateSymbolToString(const ASymbol: TTemplateSymbol): string;
begin
  if not GSymbolToKeyword.TryGetValue(ASymbol, Result) then
    exit(GetEnumName(TypeInfo(TTemplateSymbol), integer(ASymbol)));
end;

{ TTemplateLexer }

constructor TTemplateLexer.Create(const AContext: ITemplateContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean);
begin
  FContext := AContext;
  FReader := TStreamReader.Create(AStream, AContext.Encoding, false, 4096);
  FPrevLineOffset := -1;
  FLineOffset := 0;
  FOptions := AContext.Options;
  FStartScript := AContext.StartToken;
  FEndScript := AContext.EndToken;
  FStartStripScript := AContext.StartStripToken;
  FEndStripScript := AContext.EndStripToken;
  if length(FStartScript) <> 2 then
    raise ETemplateLexer.CreateRes(@SContextStartTokenMustBeTwoCharsLong);
  if length(FEndScript) <> 2 then
    raise ETemplateLexer.CreateRes(@SContextEndTokenMustBeTwoCharsLong);
  FStream := AStream;
  FManageStream := AManageStream;
  FFilename := AFilename;
  FLine := 1;
  FPos := 0;
  FState := SText;
  FLookahead.Input := #0;
  FLookahead.Eof := AStream.Size = 0;
  GetInput;
  FAccumulator := TStringBuilder.Create;
end;

class constructor TTemplateLexer.Create;
begin
  FIDRegex := TRegEx.Create('^[a-zA-Z_][a-zA-Z_0-9]*$');
end;

destructor TTemplateLexer.Destroy;
begin
  FAccumulator.Free;
  FReader.Free;
  if FManageStream then
    FStream.Free;
  inherited;
end;

function TTemplateLexer.Expecting(const Achar: char): Boolean;
begin
  exit(FLookahead.Input = Achar);
end;

function TTemplateLexer.Expecting(const Achars: TCharSet): Boolean;
begin
{$WARN WIDECHAR_REDUCED OFF}
  exit(not FLookahead.Eof and (FLookahead.Input in Achars));
{$WARN WIDECHAR_REDUCED ON}
end;

procedure TTemplateLexer.GetInput;
begin
  FCurrent := FLookahead;
  if FLookahead.Eof then
    exit;
  FLookahead.Eof := FReader.EndOfStream;
  if FLookahead.Eof then
    FLookahead.Input := #0
  else
  begin
    FLookahead.Input := char(FReader.Read());
    if FLookahead.Input = #10 then
    begin
      Inc(FLine);
      FPos := 0;
    end
    else
      Inc(FPos);
  end;
end;

function TTemplateLexer.GetScriptToken: ITemplateSymbol;

const
{$WARN WIDECHAR_REDUCED OFF}
  WHITESPACE: set of char = [#0, ' ', #9, #10, #13];
  VARIABLE_START: set of char = ['a' .. 'z', 'A' .. 'Z', '_'];
  VARIABLE_END: set of char = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
  NUMBER: set of char = ['0' .. '9'];
  ESCAPE = '\';
{$WARN WIDECHAR_REDUCED ON}
var
  LLine: integer;
  LPosition: integer;
  LLast: char;
  LEndExpect: char;
  LEndStripWS: Boolean;

  function MakePosition: IPosition;
  begin
    if eoNoPosition in FOptions then
      exit(nil)
    else
      exit(TPosition.Create(FFilename, LLine, LPosition));
  end;

  function SimpleToken(const ASymbol: TTemplateSymbol; const AStripWS: Boolean = false): ITemplateSymbol;
  var
    LPosition: IPosition;
  begin
    LPosition := MakePosition;
    Result := TSimpleTemplateSymbol.Create(LPosition, ASymbol, AStripWS);
    GetInput;
  end;

  function IsValidId(const AId: string): Boolean;
  begin
    exit(FIDRegex.IsMatch(AId));
  end;

  function ValueToken(const ASymbol: TTemplateSymbol): ITemplateSymbol;
  var
    LId: string;
    LPosition: IPosition;
  begin
    LId := FAccumulator.ToString;
    LPosition := MakePosition;
    if (ASymbol = vsID) and not IsValidId(LId) then
    begin
      RaiseErrorRes(LPosition, @SInvalidCharacterDetected);
    end;
    Result := TTemplateValueSymbol.Create(LPosition, ASymbol, LId);
    FAccumulator.Clear;
    GetInput;
  end;

  procedure Accumulate;
  begin
    FAccumulator.Append(FLookahead.Input);
    LLast := FLookahead.Input;
    GetInput;
  end;

  function ReturnString(const QuoteType: char): ITemplateSymbol;
  begin
    while not FLookahead.Eof and ((FLookahead.Input <> QuoteType) or (LLast = ESCAPE)) do
    begin
      if FLookahead.Input = QuoteType then
        FAccumulator.Remove(FAccumulator.length - 1, 1);
      Accumulate;
    end;
    SwallowInput;
    exit(ValueToken(vsString));
  end;

begin
  FAccumulator.Clear;
  LLine := FLine;
  LPosition := FPos;
  LLast := #0;
  while not FCurrent.Eof do
  begin
{$WARN WIDECHAR_REDUCED OFF}
    if FCurrent.Input in WHITESPACE then
{$WARN WIDECHAR_REDUCED ON}
    begin
      SwallowInput;
      continue;
    end
{$WARN WIDECHAR_REDUCED OFF}
    else if FCurrent.Input in VARIABLE_START then
{$WARN WIDECHAR_REDUCED ON}
    begin
      FAccumulator.Append(FCurrent.Input);
      while Expecting(VARIABLE_END) do
        Accumulate;
      exit(ValueToken(vsID));
    end
{$WARN WIDECHAR_REDUCED OFF}
    else if FCurrent.Input in NUMBER then
{$WARN WIDECHAR_REDUCED ON}
    begin
      FAccumulator.Append(FCurrent.Input);
      while Expecting(NUMBER) do
        Accumulate;
      if FLookahead.Input = FContext.DecimalSeparator then
      begin
        Accumulate;
        while Expecting(NUMBER) do
          Accumulate;
      end;
{$WARN WIDECHAR_REDUCED OFF}
      if FLookahead.Input in ['e', 'E'] then
{$WARN WIDECHAR_REDUCED ON}
      begin
        Accumulate;
{$WARN WIDECHAR_REDUCED OFF}
        if FLookahead.Input in ['-', '+'] then
{$WARN WIDECHAR_REDUCED ON}
        begin
          Accumulate;
        end;
        while Expecting(NUMBER) do
          Accumulate;
      end;
      exit(ValueToken(vsNumber));
    end
    else
      case FCurrent.Input of
        ';':
          exit(SimpleToken(vsSemiColon));
        ',':
          exit(SimpleToken(vsComma));
        '(':
          begin
            if not Expecting('*') then
              exit(SimpleToken(vsOpenRoundBracket));
            SwallowInput;
            while not FLookahead.Eof and not((FCurrent.Input = '*') and Expecting(')')) do
              SwallowInput;
            SwallowInput;
            exit(SimpleToken(vsComment));
          end;
        ')':
          exit(SimpleToken(vsCloseRoundBracket));
        '[':
          exit(SimpleToken(vsOpenSquareBracket));
        ']':
          exit(SimpleToken(vsCloseSquareBracket));
        '.':
          exit(SimpleToken(vsDOT));
        '?':
          exit(SimpleToken(vsQUESTION));
        '+':
          exit(SimpleToken(vsPLUS));
        '-':
          exit(SimpleToken(vsMinus));
        '*':
          exit(SimpleToken(vsMULT));
        '/':
          exit(SimpleToken(vsSLASH));
        '<':
          if Expecting('>') then
          begin
            SwallowInput;
            exit(SimpleToken(vsNotEQ))
          end
          else if Expecting('=') then
          begin
            SwallowInput;
            exit(SimpleToken(vsLTE))
          end
          else
            exit(SimpleToken(vsLT));
        '>':
          if Expecting('=') then
          begin
            SwallowInput;
            exit(SimpleToken(vsGTE))
          end
          else
            exit(SimpleToken(vsGT));
        '=':
          begin
            if FLookahead.Input = '=' then
              GetInput;
            exit(SimpleToken(vsEQ));
          end;
        '`':
          exit(ReturnString('`'));
        '‘':
          exit(ReturnString('’'));
        '''':
          exit(ReturnString(''''));
        '“':
          exit(ReturnString('”'));
        '"':
          exit(ReturnString('"'));
        ':':
          if Expecting('=') then
          begin
            SwallowInput;
            exit(SimpleToken(vsCOLONEQ));
          end
          else
            exit(SimpleToken(vsCOLON));
      else
        begin
          if CharInSet(FCurrent.Input, [FEndScript[1], FEndStripScript[1]]) then
          begin
            if FCurrent.Input = FEndScript[1] then
              LEndExpect := FEndScript[2]
            else
              LEndExpect := FEndStripScript[2];
            if Expecting(LEndExpect) then
            begin
              LEndStripWS := FCurrent.Input = FEndStripScript[1];
              GetInput;
              if FAccumulator.length > 0 then
              begin
                Result := ValueToken(vsText);
                FNextToken := SimpleToken(VsEndScript, LEndStripWS);
              end
              else
              begin
                Result := SimpleToken(VsEndScript, LEndStripWS);
              end;
              FState := SText;
              exit;
            end;
          end;
        end;
      end;
    FAccumulator.Append(FCurrent.Input);
    GetInput;
  end;

  if FAccumulator.length > 0 then
  begin
    Result := ValueToken(vsText);
    FNextToken := SimpleToken(vsEOF);
  end
  else
    exit(SimpleToken(vsEOF));
end;

function TTemplateLexer.GetTextToken: ITemplateSymbol;
var
  LLine: integer;
  LPosition: integer;
  LLastChar, LCurChar: char;
  LIsStartStripWSToken: Boolean;

  function MakePosition: IPosition;
  begin
    if eoNoPosition in FOptions then
      exit(nil)
    else
      exit(TPosition.Create(FFilename, LLine, LPosition));
  end;

  function SimpleToken(const ASymbol: TTemplateSymbol; const AStripWS: Boolean = false): ITemplateSymbol;
  var
    LPosition: IPosition;
  begin
    LPosition := MakePosition;
    Result := TSimpleTemplateSymbol.Create(LPosition, ASymbol, AStripWS);
    GetInput;
  end;

  function ValueToken(const ASymbol: TTemplateSymbol): ITemplateSymbol;
  var
    LPosition: IPosition;
  begin
    LPosition := MakePosition;
    Result := TTemplateValueSymbol.Create(LPosition, ASymbol, FAccumulator.ToString);
    FAccumulator.Clear;
    GetInput;
  end;

begin
  FAccumulator.Clear;
  LLine := FLine;
  LPosition := FPos;
  LLastChar := #0;
  if FCurrent.Input = #0 then
    GetInput;
  while not FCurrent.Eof do
  begin
    LIsStartStripWSToken := (FCurrent.Input = FStartStripScript[1]) and (FLookahead.Input = FStartStripScript[2]);
    if (FCurrent.Input = FStartScript[1]) and (FLookahead.Input = FStartScript[2]) or LIsStartStripWSToken then
    begin
      Result := ValueToken(vsText);
      FState := SScript;
      FNextToken := SimpleToken(VsStartScript, LIsStartStripWSToken);
      exit();
    end
    else
    begin
      LCurChar := FCurrent.Input;
      if (eoConvertTabsToSpaces in FOptions) and (LCurChar = #9) then
        LCurChar := ' ';
      if (eoStripRecurringSpaces in FOptions) and (LLastChar = ' ') and (LCurChar = ' ') then
        GetInput
      else
      begin
        FAccumulator.Append(LCurChar);
        LLastChar := LCurChar;
        GetInput;
      end;
    end;
  end;

  if FAccumulator.length > 0 then
  begin
    Result := ValueToken(vsText);
    FNextToken := SimpleToken(vsEOF);
  end
  else
    exit(SimpleToken(vsEOF));
end;

function TTemplateLexer.GetToken: ITemplateSymbol;
begin
  if FNextToken <> nil then
  begin
    Result := FNextToken;
    FNextToken := nil;
    exit;
  end;
  case FState of
    SText:
      exit(GetTextToken);
    SScript:
      exit(GetScriptToken);
  else
    raise ETemplateLexer.CreateRes(@SUnexpectedLexerState);
  end;
end;

procedure TTemplateLexer.SwallowInput;
begin
  GetInput;
end;

{ TSimpleMustacheToken }

constructor TSimpleTemplateSymbol.Create(const APosition: IPosition; const AToken: TTemplateSymbol; const AStripWS: Boolean);
begin
  FToken := AToken;
  FPosition := APosition;
  FStripWS := AStripWS;
end;

function TSimpleTemplateSymbol.GetPosition: IPosition;
begin
  exit(FPosition);
end;

function TSimpleTemplateSymbol.GetToken: TTemplateSymbol;
begin
  exit(FToken);
end;

procedure TSimpleTemplateSymbol.SetToken(const AToken: TTemplateSymbol);
begin
  FToken := AToken;
end;

function TSimpleTemplateSymbol.StripWS: Boolean;
begin
  exit(FStripWS);
end;

{ TStringMustacheToken }

constructor TTemplateValueSymbol.Create(const APosition: IPosition; const AToken: TTemplateSymbol; const AString: string);
begin
  inherited Create(APosition, AToken, false);
  SetValue(AString);
end;

function TTemplateValueSymbol.GetValue: string;
begin
  exit(FValue);
end;

procedure TTemplateValueSymbol.SetValue(const AValue: string);
var
  LSymbol: TTemplateSymbol;
begin
  FValue := AValue;
  if GetToken <> vsID then
    exit;
  if GKeywords.TryGetValue(AValue, LSymbol) then
  begin
    SetToken(LSymbol);
    exit;
  end;
end;

{ TTemplateLexer.TPair }

constructor TTemplateLexer.TPair.Create(const AInput: char; const AEof: Boolean);
begin
  Input := AInput;
  Eof := AEof;
end;

procedure AddHashedKeyword(const akeyword: string; const ASymbol: TTemplateSymbol);
begin
  GKeywords.add(akeyword, ASymbol);

  // true/false both map onto vsboolean
  if ASymbol <> vsBoolean then
    GSymbolToKeyword.add(ASymbol, akeyword);
end;

procedure AddSymKeyword(const ASym: string; const ASymbol: TTemplateSymbol);
begin
  GSymbolToKeyword.add(ASymbol, ASym);
end;

initialization

GSymbolToKeyword := TDictionary<TTemplateSymbol, string>.Create();
GKeywords := TDictionary<string, TTemplateSymbol>.Create;

AddHashedKeyword('require', vsRequire);
AddHashedKeyword('ignorenl', vsIgnoreNL);
AddHashedKeyword('if', vsIf);
AddHashedKeyword('elif', vsElIf);
AddHashedKeyword('else', vsElse);
AddHashedKeyword('while', vsWhile);
AddHashedKeyword('with', vsWith);
AddHashedKeyword('template', vsTemplate);
AddHashedKeyword('print', vsPrint);
AddHashedKeyword('for', vsFor);
AddHashedKeyword('cycle', vsCycle);
AddHashedKeyword('offset', vsOffset);
AddHashedKeyword('limit', vsLimit);
AddHashedKeyword('break', vsBreak);
AddHashedKeyword('continue', vsContinue);
AddHashedKeyword('in', vsIn);
AddHashedKeyword('of', vsOf);
AddHashedKeyword('end', vsEnd);
AddHashedKeyword('include', vsInclude);
AddHashedKeyword('to', vsTo);
AddHashedKeyword('downto', vsDownTo);
AddHashedKeyword('step', vsStep);
AddHashedKeyword('true', vsBoolean);
AddHashedKeyword('false', vsBoolean);
AddHashedKeyword('and', vsAnd);
AddHashedKeyword('or', vsOr);
AddHashedKeyword('not', vsNot);
AddHashedKeyword('mod', vsMod);
AddHashedKeyword('div', vsDiv);
AddHashedKeyword('onbegin', vsOnBegin);
AddHashedKeyword('onend', vsOnEnd);
AddHashedKeyword('onempty', vsOnEmpty);
AddHashedKeyword('betweenitems', vsBetweenItem);
AddHashedKeyword('extends', vsExtends);
AddHashedKeyword('block', vsBlock);

AddSymKeyword('ScriptStartToken', VsStartScript);
AddSymKeyword('ScriptEndToken', VsEndScript);
AddSymKeyword('(', vsOpenRoundBracket);
AddSymKeyword(')', vsCloseRoundBracket);
AddSymKeyword('(*   *) ', vsComment);
AddSymKeyword('Text', vsText);
AddSymKeyword(':=', vsCOLONEQ);
AddSymKeyword('id', vsID);
AddSymKeyword('.', vsDOT);
AddSymKeyword('[', vsOpenSquareBracket);
AddSymKeyword(']', vsCloseSquareBracket);

AddSymKeyword('number', vsNumber);
AddSymKeyword('boolean', vsBoolean);
AddSymKeyword('string', vsString);

AddSymKeyword('?', vsQUESTION);
AddSymKeyword(':', vsCOLON);

AddSymKeyword('=', vsEQ);
AddSymKeyword('<>', vsNotEQ);
AddSymKeyword('<', vsLT);
AddSymKeyword('<=', vsLTE);
AddSymKeyword('>', vsGT);
AddSymKeyword('>=', vsGTE);

AddSymKeyword('+', vsPLUS);
AddSymKeyword('-', vsMinus);
AddSymKeyword('*', vsMULT);
AddSymKeyword('/', vsSLASH);

AddSymKeyword(',', vsComma);
AddSymKeyword(';', vsSemiColon);

finalization

GKeywords.Free;
GSymbolToKeyword.Free;

end.
