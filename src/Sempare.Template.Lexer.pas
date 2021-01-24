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
    procedure SetValue(const Avalue: string);
    property Value: string read GetValue write SetValue;
  end;

  ETemplateLexer = class(ETemplate);

  ITemplateLexer = interface
    ['{839FAB50-B21E-4C31-ACAA-2E50AEAA1456}']

    function GetToken: ITemplateSymbol;
  end;

function TemplateSymbolToString(const ASymbol: TTemplateSymbol): string;
function CreateTemplateLexer(AContext: ITemplateContext; const AStream: TStream; const AFilename: string = ''; const AManageStream: Boolean = True): ITemplateLexer;

implementation

uses
  TypInfo,
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
      constructor Create(const Ainput: char; const Aeof: Boolean);
    end;

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
    procedure GetInput;
    procedure SwallowInput; // a descriptive helper
    function Expecting(const Achar: char): Boolean; overload;
    function Expecting(const Achars: TCharSet): Boolean; overload;
    function GetTextToken: ITemplateSymbol;
    function GetScriptToken: ITemplateSymbol;
  public
    constructor Create(AContext: ITemplateContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean = True);
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
    constructor Create(APosition: IPosition; const AToken: TTemplateSymbol; const AStripWS: Boolean = false);
    procedure SetToken(const AToken: TTemplateSymbol);
    function GetToken: TTemplateSymbol;
    function StripWS: Boolean;
  end;

  TTemplateValueSymbol = class(TSimpleTemplateSymbol, ITemplateValueSymbol)
  private
    FValue: string;
  public
    constructor Create(APosition: IPosition; const AToken: TTemplateSymbol; const AString: string);
    procedure SetValue(const Avalue: string);
    function GetValue: string;
  end;

function CreateTemplateLexer(AContext: ITemplateContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean): ITemplateLexer;
begin
  exit(TTemplateLexer.Create(AContext, AStream, AFilename, AManageStream));
end;

function TemplateSymbolToString(const ASymbol: TTemplateSymbol): string;
begin
  if not GSymbolToKeyword.TryGetValue(ASymbol, Result) then
    exit(GetEnumName(TypeInfo(TTemplateSymbol), integer(ASymbol)));
end;

{ TTemplateLexer }

constructor TTemplateLexer.Create(AContext: ITemplateContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean);
begin
  FReader := TStreamReader.Create(AStream, AContext.Encoding, false, 4096);
  FPrevLineOffset := -1;
  FLineOffset := 0;
  FNextToken := nil;
  FOptions := AContext.Options;
  FStartScript := AContext.StartToken;
  FEndScript := AContext.EndToken;
  FStartStripScript := AContext.StartStripToken;
  FEndStripScript := AContext.EndStripToken;
  if length(FStartScript) <> 2 then
    raise ETemplateLexer.Create(SContextStartTokenMustBeTwoCharsLong);
  if length(FEndScript) <> 2 then
    raise ETemplateLexer.Create(SContextEndTokenMustBeTwoCharsLong);
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

destructor TTemplateLexer.Destroy;
begin
  FNextToken := nil;
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
  begin
    Result := TSimpleTemplateSymbol.Create(MakePosition, ASymbol, AStripWS);
    GetInput;
  end;

  function ValueToken(const ASymbol: TTemplateSymbol): ITemplateSymbol;
  begin
    Result := TTemplateValueSymbol.Create(MakePosition, ASymbol, FAccumulator.ToString);
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
      exit(ValueToken(VsID));
    end
{$WARN WIDECHAR_REDUCED OFF}
    else if FCurrent.Input in NUMBER then
{$WARN WIDECHAR_REDUCED ON}
    begin
      FAccumulator.Append(FCurrent.Input);
      while Expecting(NUMBER) do
        Accumulate;
      if FLookahead.Input = '.' then
      begin
        Accumulate;
        while Expecting(NUMBER) do
          Accumulate;
      end;
      exit(ValueToken(VsNumber));
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
              exit(SimpleToken(VsOpenRoundBracket));
            SwallowInput;
            while not FLookahead.Eof and not((FCurrent.Input = '*') and Expecting(')')) do
              SwallowInput;
            SwallowInput;
            exit(SimpleToken(VsComment));
          end;
        ')':
          exit(SimpleToken(VsCloseRoundBracket));
        '[':
          exit(SimpleToken(VsOpenSquareBracket));
        ']':
          exit(SimpleToken(VsCloseSquareBracket));
        '.':
          exit(SimpleToken(VsDOT));
        '?':
          exit(SimpleToken(vsQUESTION));
        '+':
          exit(SimpleToken(VsPLUS));
        '-':
          exit(SimpleToken(VsMinus));
        '*':
          exit(SimpleToken(VsMULT));
        '/':
          exit(SimpleToken(VsSLASH));
        '<':
          if Expecting('>') then
          begin
            SwallowInput;
            exit(SimpleToken(VsNotEQ))
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
          exit(SimpleToken(VsEQ));
        '''':
          exit(ReturnString(''''));
        '"':
          exit(ReturnString('"'));
        ':':
          if Expecting('=') then
          begin
            SwallowInput;
            exit(SimpleToken(VsCOLONEQ));
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
                Result := ValueToken(VsText);
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
    Result := ValueToken(VsText);
    FNextToken := SimpleToken(VsEOF);
  end
  else
    exit(SimpleToken(VsEOF));
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
  begin
    Result := TSimpleTemplateSymbol.Create(MakePosition, ASymbol, AStripWS);
    GetInput;
  end;

  function ValueToken(const ASymbol: TTemplateSymbol): ITemplateSymbol;
  begin
    Result := TTemplateValueSymbol.Create(MakePosition, ASymbol, FAccumulator.ToString);
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
      Result := ValueToken(VsText);
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
    Result := ValueToken(VsText);
    FNextToken := SimpleToken(VsEOF);
  end
  else
    exit(SimpleToken(VsEOF));
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
    raise ETemplateLexer.Create(SUnexpectedLexerState);
  end;
end;

procedure TTemplateLexer.SwallowInput;
begin
  GetInput;
end;

{ TSimpleMustacheToken }

constructor TSimpleTemplateSymbol.Create(APosition: IPosition; const AToken: TTemplateSymbol; const AStripWS: Boolean);
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

constructor TTemplateValueSymbol.Create(APosition: IPosition; const AToken: TTemplateSymbol; const AString: string);
begin
  inherited Create(APosition, AToken, false);
  SetValue(AString);
end;

function TTemplateValueSymbol.GetValue: string;
begin
  exit(FValue);
end;

procedure TTemplateValueSymbol.SetValue(const Avalue: string);
var
  LSymbol: TTemplateSymbol;
begin
  FValue := Avalue;
  if GetToken <> VsID then
    exit;
  if GKeywords.TryGetValue(Avalue, LSymbol) then
  begin
    SetToken(LSymbol);
    exit;
  end;
end;

{ TTemplateLexer.TPair }

constructor TTemplateLexer.TPair.Create(const Ainput: char; const Aeof: Boolean);
begin
  Input := Ainput;
  Eof := Aeof;
end;

procedure AddHashedKeyword(const akeyword: string; const ASymbol: TTemplateSymbol);
begin
  GKeywords.Add(akeyword, ASymbol);

  // true/false both map onto vsboolean
  if ASymbol <> vsBoolean then
    GSymbolToKeyword.Add(ASymbol, akeyword);
end;

procedure AddSymKeyword(const ASym: string; const ASymbol: TTemplateSymbol);
begin
  GSymbolToKeyword.Add(ASymbol, ASym);
end;

initialization

GSymbolToKeyword := TDictionary<TTemplateSymbol, string>.Create();
GKeywords := TDictionary<string, TTemplateSymbol>.Create;

AddHashedKeyword('require', VsRequire);
AddHashedKeyword('ignorenl', VsIgnoreNL);
AddHashedKeyword('if', VsIF);
AddHashedKeyword('elif', VsELIF);
AddHashedKeyword('else', vsElse);
AddHashedKeyword('while', vsWhile);
AddHashedKeyword('with', vsWith);
AddHashedKeyword('template', vsTemplate);
AddHashedKeyword('print', VsPRINT);
AddHashedKeyword('for', VsFOR);
AddHashedKeyword('break', VsBREAK);
AddHashedKeyword('continue', VsCONTINUE);
AddHashedKeyword('in', VsIN);
AddHashedKeyword('end', VsEND);
AddHashedKeyword('include', VsINCLUDE);
AddHashedKeyword('to', vsTo);
AddHashedKeyword('downto', vsDownto);
AddHashedKeyword('true', vsBoolean);
AddHashedKeyword('false', vsBoolean);
AddHashedKeyword('and', VsAND);
AddHashedKeyword('or', VsOR);
AddHashedKeyword('not', VsNOT);
AddHashedKeyword('mod', VsMOD);
AddHashedKeyword('div', VsDIV);

AddSymKeyword('ScriptStartToken', VsStartScript);
AddSymKeyword('ScriptEndToken', VsEndScript);
AddSymKeyword('(', VsOpenRoundBracket);
AddSymKeyword(')', VsCloseRoundBracket);
AddSymKeyword('(*   *) ', VsComment);
AddSymKeyword('Text', VsText);
AddSymKeyword(':=', VsCOLONEQ);
AddSymKeyword('id', VsID);
AddSymKeyword('.', VsDOT);
AddSymKeyword('[', VsOpenSquareBracket);
AddSymKeyword(']', VsCloseSquareBracket);

AddSymKeyword('number', VsNumber);
AddSymKeyword('boolean', vsBoolean);
AddSymKeyword('string', vsString);

AddSymKeyword('?', vsQUESTION);
AddSymKeyword(':', vsCOLON);

AddSymKeyword('=', VsEQ);
AddSymKeyword('<>', VsNotEQ);
AddSymKeyword('<', vsLT);
AddSymKeyword('<=', vsLTE);
AddSymKeyword('>', vsGT);
AddSymKeyword('>=', vsGTE);

AddSymKeyword('+', VsPLUS);
AddSymKeyword('-', VsMinus);
AddSymKeyword('*', VsMULT);
AddSymKeyword('/', VsSLASH);

AddSymKeyword(',', vsComma);
AddSymKeyword(';', vsSemiColon);

finalization

GKeywords.Free;
GSymbolToKeyword.Free;

end.
