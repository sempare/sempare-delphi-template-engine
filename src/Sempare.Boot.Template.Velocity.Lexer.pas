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
unit Sempare.Boot.Template.Velocity.Lexer;

interface

uses
  System.Classes,
  System.SysUtils,
  Sempare.Boot.Template.Velocity.AST,
  Sempare.Boot.Template.Velocity.Context;

type
  IVelocityValueSymbol = interface(IVelocitySymbol)
    ['{930E9892-38AA-4030-83CC-4069667B2E6E}']

    function GetValue: string;
    procedure SetValue(const Avalue: string);
    property Value: string read GetValue write SetValue;
  end;

  EVelocityLexerException = class(Exception);

  IVelocityLexer = interface
    ['{839FAB50-B21E-4C31-ACAA-2E50AEAA1456}']

    function GetToken: IVelocitySymbol;
  end;

function VelocitySymbolToString(const ASymbol: TVelocitySymbol): string;
function CreateVelocityLexer(AContext: IVelocityContext; const AStream: TStream; const AFilename: string = ''; const AManageStream: Boolean = True): IVelocityLexer;

implementation

uses
  TypInfo,
  System.Generics.Collections,
  Sempare.Boot.Template.Velocity.Common;

var
  GKeywords: TDictionary<string, TVelocitySymbol>;
  GSymbolToKeyword: TDictionary<TVelocitySymbol, string>;

type
  TVelocityLexer = class(TInterfacedObject, IVelocityLexer)
  type
    TState = (SText, SScript);
{$WARN WIDECHAR_REDUCED OFF}
    TCharSet = set of Char;
{$WARN WIDECHAR_REDUCED ON}

    TPair = record
      Input: Char;
      Eof: Boolean;
      constructor Create(const Ainput: Char; const Aeof: Boolean);
    end;

  private
    FReader: TStreamReader;
    FNextToken: IVelocitySymbol;
    FStream: TStream;
    FLine: integer;
    Fpos: integer;
    Ffilename: string;
    FLookahead: TPair;
    Fcurrent: TPair;
    FManageStream: Boolean;
    FState: TState;
    FAccumulator: TStringBuilder;
    FPrevLineOffset: integer;
    FLineOffset: integer;
    FStartScript: string;
    FEndScript: string;
    FOptions: TVelocityEvaluationOptions;
    procedure GetInput;
    procedure SwallowInput; // a descriptive helper
    function Expecting(const Achar: Char): Boolean; overload;
    function Expecting(const Achars: TCharSet): Boolean; overload;
    function GetTextToken: IVelocitySymbol;
    function GetScriptToken: IVelocitySymbol;

  public
    constructor Create(AContext: IVelocityContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean = True);
    destructor Destroy; override;
    function GetToken: IVelocitySymbol;
  end;

type
  TSimpleVelocitySymbol = class(TInterfacedObject, IVelocitySymbol)
  private
    FToken: TVelocitySymbol;
    FPosition: IPosition;
    function GetPosition: IPosition;
  public
    constructor Create(APosition: IPosition; const AToken: TVelocitySymbol);
    procedure SetToken(const AToken: TVelocitySymbol);
    function GetToken: TVelocitySymbol;
  end;

  TVelocityValueSymbol = class(TSimpleVelocitySymbol, IVelocityValueSymbol)
  private
    FValue: string;
  public
    constructor Create(APosition: IPosition; const AToken: TVelocitySymbol; const AString: string);
    procedure SetValue(const Avalue: string);
    function GetValue: string;
  end;

function CreateVelocityLexer(AContext: IVelocityContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean): IVelocityLexer;
begin
  Result := TVelocityLexer.Create(AContext, AStream, AFilename, AManageStream);
end;

function VelocitySymbolToString(const ASymbol: TVelocitySymbol): string;
begin
  if not GSymbolToKeyword.TryGetValue(ASymbol, Result) then
    Result := GetEnumName(TypeInfo(TVelocitySymbol), integer(ASymbol));
end;

{ TVelocityLexer }

constructor TVelocityLexer.Create(AContext: IVelocityContext; const AStream: TStream; const AFilename: string; const AManageStream: Boolean);
begin
  FReader := TStreamReader.Create(AStream, AContext.Encoding, false, 4096);
  FPrevLineOffset := -1;
  FLineOffset := 0;
  FNextToken := nil;
  FOptions := AContext.Options;
  FStartScript := AContext.StartToken;
  FEndScript := AContext.EndToken;
  if length(FStartScript) <> 2 then
    raise Exception.Create('Context StartToken must be two characters long.');
  if length(FEndScript) <> 2 then
    raise Exception.Create('Context EndToken must be two characters long.');
  FStream := AStream;
  FManageStream := AManageStream;
  Ffilename := AFilename;
  FLine := 1;
  Fpos := 0;
  FState := SText;
  FLookahead.Input := #0;
  FLookahead.Eof := AStream.Size = 0;
  GetInput;
  FAccumulator := TStringBuilder.Create;
end;

destructor TVelocityLexer.Destroy;
begin
  FNextToken := nil;
  FAccumulator.Free;
  FReader.Free;
  if FManageStream then
    FStream.Free;
  inherited;
end;

function TVelocityLexer.Expecting(const Achar: Char): Boolean;
begin
  Result := FLookahead.Input = Achar;
end;

function TVelocityLexer.Expecting(const Achars: TCharSet): Boolean;

begin
{$WARN WIDECHAR_REDUCED OFF}
  Result := not FLookahead.Eof and (FLookahead.Input in Achars);
{$WARN WIDECHAR_REDUCED ON}
end;

procedure TVelocityLexer.GetInput;
begin
  Fcurrent := FLookahead;
  if FLookahead.Eof then
    Exit;
  FLookahead.Eof := FReader.EndOfStream;
  if FLookahead.Eof then
    FLookahead.Input := #0
  else
  begin
    FLookahead.Input := Char(FReader.Read());
    if FLookahead.Input = #10 then
    begin
      Inc(FLine);
      Fpos := 0;
    end
    else
      Inc(Fpos);
  end;
end;

function TVelocityLexer.GetScriptToken: IVelocitySymbol;
var
  Line: integer;
  Position: integer;

  function MakePosition: IPosition;
  begin
    if eoNoPosition in FOptions then
      Result := nil
    else
      Result := TPosition.Create(Ffilename, Line, Position);
  end;

  function SimpleToken(const ASymbol: TVelocitySymbol): IVelocitySymbol;
  begin
    Result := TSimpleVelocitySymbol.Create(MakePosition, ASymbol);
    GetInput;
  end;

  function ValueToken(const ASymbol: TVelocitySymbol): IVelocitySymbol;
  begin
    Result := TVelocityValueSymbol.Create(MakePosition, ASymbol, FAccumulator.ToString);
    FAccumulator.Clear;
    GetInput;
  end;

  procedure Accumulate;
  begin
    FAccumulator.Append(FLookahead.Input);
    GetInput;
  end;

const
{$WARN WIDECHAR_REDUCED OFF}
  WHITESPACE: set of Char = [#0, ' ', #9, #10, #13];
  VARIABLE_START: set of Char = ['a' .. 'z', 'A' .. 'Z', '_'];
  VARIABLE_END: set of Char = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
  NUMBER: set of Char = ['0' .. '9'];
{$WARN WIDECHAR_REDUCED ON}
begin
  FAccumulator.Clear;
  Line := FLine;
  Position := Fpos;
  while not Fcurrent.Eof do
  begin
{$WARN WIDECHAR_REDUCED OFF}
    if Fcurrent.Input in WHITESPACE then
{$WARN WIDECHAR_REDUCED ON}
    begin
      SwallowInput;
      continue;
    end
{$WARN WIDECHAR_REDUCED OFF}
    else if Fcurrent.Input in VARIABLE_START then
{$WARN WIDECHAR_REDUCED ON}
    begin
      FAccumulator.Append(Fcurrent.Input);
      while Expecting(VARIABLE_END) do
        Accumulate;
      Exit(ValueToken(VsID));
    end
{$WARN WIDECHAR_REDUCED OFF}
    else if Fcurrent.Input in NUMBER then
{$WARN WIDECHAR_REDUCED ON}
    begin
      FAccumulator.Append(Fcurrent.Input);
      while Expecting(NUMBER) do
        Accumulate;
      if FLookahead.Input = '.' then
      begin
        Accumulate;
        while Expecting(NUMBER) do
          Accumulate;
      end;
      Exit(ValueToken(VsNumber));
    end
    else
      case Fcurrent.Input of
        ';':
          Exit(SimpleToken(vsSemiColon));
        ',':
          Exit(SimpleToken(vsComma));
        '(':
          begin
            if not Expecting('*') then
              Exit(SimpleToken(VsOpenRoundBracket));
            SwallowInput;
            while not FLookahead.Eof and not((Fcurrent.Input = '*') and Expecting(')')) do
              SwallowInput;
            SwallowInput;
            Exit(SimpleToken(VsComment));
          end;
        ')':
          Exit(SimpleToken(VsCloseRoundBracket));
        '[':
          Exit(SimpleToken(VsOpenSquareBracket));
        ']':
          Exit(SimpleToken(VsCloseSquareBracket));
        '.':
          Exit(SimpleToken(VsDOT));
        '?':
          Exit(SimpleToken(vsQUESTION));
        '+':
          Exit(SimpleToken(VsPLUS));
        '-':
          Exit(SimpleToken(VsMinus));
        '*':
          Exit(SimpleToken(VsMULT));
        '/':
          Exit(SimpleToken(VsDIV));
        '<':
          if Expecting('=') then
          begin
            SwallowInput;
            Exit(SimpleToken(vsLTE))
          end
          else
            Exit(SimpleToken(vsLT));
        '>':
          if Expecting('=') then
          begin
            SwallowInput;
            Exit(SimpleToken(vsGTE))
          end
          else
            Exit(SimpleToken(vsGT));
        '=':
          Exit(SimpleToken(VsEQ));
        '''':
          begin
            while not FLookahead.Eof and (FLookahead.Input <> '''') do
              Accumulate;
            SwallowInput;
            Exit(ValueToken(vsString));
          end;
        ':':
          if Expecting('=') then
          begin
            SwallowInput;
            Exit(SimpleToken(VsCOLONEQ));
          end
          else
            Exit(SimpleToken(vsCOLON));
      else
        if Fcurrent.Input = FEndScript[1] then
        begin
          if Expecting(FEndScript[2]) then
          begin
            GetInput;
            if FAccumulator.length > 0 then
            begin
              Result := ValueToken(VsText);
              FNextToken := SimpleToken(VsEndScript);
            end
            else
            begin
              Result := SimpleToken(VsEndScript);
            end;
            FState := SText;
            Exit;
          end;
        end;
      end;
    FAccumulator.Append(Fcurrent.Input);
    GetInput;
  end;

  if FAccumulator.length > 0 then
  begin
    Result := ValueToken(VsText);
    FNextToken := SimpleToken(VsEOF);
  end
  else
  begin
    Result := SimpleToken(VsEOF);
  end;
end;

function TVelocityLexer.GetTextToken: IVelocitySymbol;
var
  Line: integer;
  Position: integer;
  last, cur: Char;

  function MakePosition: IPosition;
  begin
    if eoNoPosition in FOptions then
      Result := nil
    else
      Result := TPosition.Create(Ffilename, Line, Position);
  end;

  function SimpleToken(const ASymbol: TVelocitySymbol): IVelocitySymbol;
  begin
    Result := TSimpleVelocitySymbol.Create(MakePosition, ASymbol);
    GetInput;
  end;

  function ValueToken(const ASymbol: TVelocitySymbol): IVelocitySymbol;
  begin
    Result := TVelocityValueSymbol.Create(MakePosition, ASymbol, FAccumulator.ToString);
    FAccumulator.Clear;
    GetInput;
  end;

begin
  FAccumulator.Clear;
  Line := FLine;
  Position := Fpos;
  last := #0;
  if Fcurrent.Input = #0 then
    GetInput;
  while not Fcurrent.Eof do
  begin
    if (Fcurrent.Input = FStartScript[1]) and (FLookahead.Input = FStartScript[2]) then
    begin
      Result := ValueToken(VsText);
      FState := SScript;
      FNextToken := SimpleToken(VsStartScript);
      Exit();
    end
    else
    begin
      cur := Fcurrent.Input;
      if (eoConvertTabsToSpaces in FOptions) and (cur = #9) then
        cur := ' ';
      if (eoStripRecurringSpaces in FOptions) and (last = ' ') and (cur = ' ') then
        GetInput
      else
      begin
        FAccumulator.Append(cur);
        last := cur;
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
  begin
    Result := SimpleToken(VsEOF);
  end;
end;

function TVelocityLexer.GetToken: IVelocitySymbol;
begin
  if FNextToken <> nil then
  begin
    Result := FNextToken;
    FNextToken := nil;
    Exit;
  end;
  case FState of
    SText:
      Result := GetTextToken;
    SScript:
      Result := GetScriptToken;
  else
    raise EVelocityLexerException.Create('Unexpected lexer state');
  end;
end;

procedure TVelocityLexer.SwallowInput;
begin
  GetInput;
end;

{ TSimpleMustacheToken }

constructor TSimpleVelocitySymbol.Create(APosition: IPosition; const AToken: TVelocitySymbol);
begin
  FToken := AToken;
  FPosition := APosition;
end;

function TSimpleVelocitySymbol.GetPosition: IPosition;
begin
  Result := FPosition;
end;

function TSimpleVelocitySymbol.GetToken: TVelocitySymbol;
begin
  Result := FToken;
end;

procedure TSimpleVelocitySymbol.SetToken(const AToken: TVelocitySymbol);
begin
  FToken := AToken;
end;

{ TStringMustacheToken }

constructor TVelocityValueSymbol.Create(APosition: IPosition; const AToken: TVelocitySymbol; const AString: string);
begin
  inherited Create(APosition, AToken);
  SetValue(AString);
end;

function TVelocityValueSymbol.GetValue: string;
begin
  Result := FValue;
end;

procedure TVelocityValueSymbol.SetValue(const Avalue: string);
var
  token: TVelocitySymbol;
begin
  FValue := Avalue;
  if GetToken <> VsID then
    Exit;
  if GKeywords.TryGetValue(Avalue, token) then
  begin
    SetToken(token);
    Exit;
  end;
end;

{ TVelocityLexer.TPair }

constructor TVelocityLexer.TPair.Create(const Ainput: Char; const Aeof: Boolean);
begin
  Input := Ainput;
  Eof := Aeof;
end;

procedure AddHashedKeyword(const akeyword: string; const ASymbol: TVelocitySymbol);
begin
  GKeywords.Add(akeyword, ASymbol);

  // true/false both map onto vsboolean
  if ASymbol <> vsBoolean then
    GSymbolToKeyword.Add(ASymbol, akeyword);
end;

Procedure AddSymKeyword(const ASym: string; const ASymbol: TVelocitySymbol);
begin
  GSymbolToKeyword.Add(ASymbol, ASym);
end;

initialization

GSymbolToKeyword := TDictionary<TVelocitySymbol, string>.Create();
GKeywords := TDictionary<string, TVelocitySymbol>.Create;

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
AddSymKeyword('/', VsDIV);
AddSymKeyword('*', VsMULT);

AddSymKeyword(',', vsComma);
AddSymKeyword(';', vsSemiColon);

finalization

GKeywords.Free;
GSymbolToKeyword.Free;

end.
