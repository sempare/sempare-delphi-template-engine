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
unit Sempare.Template.TemplateRegistry;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.SyncObjs,
  Sempare.Template.Context,
  Sempare.Template.AST;

// NOTE: refresh is done simply by using a thread periodically. a more optimal approach could be to use
// file system events, but for development, this should be ok.

type
  ETemplateRefreshTooFrequent = class(Exception)
  public
    constructor Create;
  end;

  ETemplateNotResolved = class(Exception)
  public
    constructor Create(const ATemplateName: string);
  end;

  IRefreshableTemplate = interface(ITemplate)
    ['{AC4008EA-336F-4DCB-B6E6-E11034DF5ACF}']
    procedure Refresh;
  end;

  TAbstractProxyTemplate = class abstract(TInterfacedObject, ITemplate)
  strict protected
    FTemplate: ITemplate;
  public
    constructor Create(const ATemplate: ITemplate);
    function GetItem(const AOffset: integer): IStmt;
    function GetCount: integer;
    function GetLastItem: IStmt;
    procedure FlattenTemplate;
    procedure OptimiseTemplate(const AOptions: TParserOptions);
    procedure Accept(const AVisitor: ITemplateVisitor);
    function GetFilename: string;
    procedure SetFilename(const AFilename: string);
    function GetLine: integer;
    procedure SetLine(const Aline: integer);
    function GetPos: integer;
    procedure SetPos(const Apos: integer);
  end;

  TResourceTemplate = class(TAbstractProxyTemplate)
  public
    constructor Create(const AContext: ITemplateContext; const AName: string);
  end;

  TFileTemplate = class(TAbstractProxyTemplate, IRefreshableTemplate)
  strict private
    FContext: ITemplateContext;
    FModifiedAt: TDateTime;
    procedure Load(const AFilename: string; const ATime: TDateTime);
  public
    constructor Create(const AContext: ITemplateContext; const AFilename: string);
    procedure Refresh;
  end;

  TTemplateLoadStrategy = (tlsLoadResource, tlsLoadFile, tlsLoadCustom);
  TTemplateNameContextResolver = reference to function(const AName: string; const AContext: TTemplateValue): string;
  TTemplateNameResolver = reference to function(const AName: string): string;
  TTempateLogException = reference to procedure(const AException: Exception);
  TTempateLogMessage = reference to procedure(const AMessage: string; const args: array of const);

  TTemplateRegistry = class
  strict private
    class var FTemplateRegistry: TTemplateRegistry;
    class var FLock: TCriticalSection;
  private
    class procedure Initialize;
    class procedure Finalize;
    class function GetInstance: TTemplateRegistry; static;
  strict private
    FTemplates: TDictionary<string, ITemplate>;
    FContext: ITemplateContext;
    FLoadStrategy: TArray<TTemplateLoadStrategy>;
    FContextNameResolver: TTemplateNameContextResolver;
    FResourceNameResolver: TTemplateNameResolver;
    FFileNameResolver: TTemplateNameResolver;
    FTemplateRootFolder: string;
    FTemplateFileExt: string;
    FRefreshIntervalS: integer;
    FShutdown: TEvent;
    FThreadDone: TEvent;
    FThread: TThread;
    FAutomaticRefresh: boolean;
    FCustomTemplateLoader: TTemplateResolverWithContext;
    FExceptionLogger: TTempateLogException;
    FLogger: TTempateLogMessage;
    procedure Refresh;
    procedure SetRefreshIntervalS(const Value: integer);
    procedure SetAutomaticRefresh(const Value: boolean);
    procedure SetLoadStrategy(const Value: TArray<TTemplateLoadStrategy>);
    procedure SetTemplateFileExt(const Value: string);
    procedure LogException(const AException: Exception);
    procedure Log(const AMsg: string; const AArgs: array of const);
  public
    constructor Create();
    destructor Destroy; override;

    function GetTemplate(const ATemplateName: string): ITemplate; overload;
    function GetTemplate(const ATemplateName: string; const AContext: TTemplateValue): ITemplate; overload;
    function GetTemplate<T>(const ATemplateName: string; const AContext: T): ITemplate; overload; inline;
    procedure RemoveTemplate(const ATemplateName: string);

    procedure Eval<T>(const AOutputStream: TStream; const ATemplateName: string; const AData: T); overload;
    function Eval<T>(const ATemplateName: string; const AData: T): string; overload;

    procedure Eval(const AOutputStream: TStream; const ATemplateName: string); overload;
    function Eval(const ATemplateName: string): string; overload;

    // with context
    procedure Eval<T>(const AOutputStream: TStream; const ATemplateName: string; const AData: T; const AContext: TTemplateValue); overload;
    function Eval<T>(const ATemplateName: string; const AData: T; const AContext: TTemplateValue): string; overload;

    procedure EvalWithContext<T, TContext>(const AOutputStream: TStream; const ATemplateName: string; const AData: T; const AContext: TContext); overload;
    function EvalWithContext<T, TContext>(const ATemplateName: string; const AData: T; const AContext: TContext): string; overload;

    procedure EvalWithContext<TContext>(const AOutputStream: TStream; const ATemplateName: string; const AContext: TContext); overload;
    function EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string; overload;

    class property Instance: TTemplateRegistry read GetInstance;

    property Context: ITemplateContext read FContext;
    property ContextNameResolver: TTemplateNameContextResolver read FContextNameResolver write FContextNameResolver;
    property ResourceNameResolver: TTemplateNameResolver read FResourceNameResolver write FResourceNameResolver;
    property FileNameResolver: TTemplateNameResolver read FFileNameResolver write FFileNameResolver;
    property TemplateRootFolder: string read FTemplateRootFolder write FTemplateRootFolder;
    property TemplateFileExt: string read FTemplateFileExt write SetTemplateFileExt;
    property LoadStrategy: TArray<TTemplateLoadStrategy> read FLoadStrategy write SetLoadStrategy;
    property RefreshIntervalS: integer read FRefreshIntervalS write SetRefreshIntervalS;
    property AutomaticRefresh: boolean read FAutomaticRefresh write SetAutomaticRefresh;
    property CustomTemplateLoader: TTemplateResolverWithContext read FCustomTemplateLoader write FCustomTemplateLoader;
    property ExceptionLogger: TTempateLogException read FExceptionLogger write FExceptionLogger;
    property Logger: TTempateLogMessage read FLogger write FLogger;
  end;

implementation

uses
  Sempare.Template.ResourceStrings,
  Sempare.Template,
{$IFDEF DEBUG}
  Sempare.Template.Common, // for inline hint
  Sempare.Template.PrettyPrint,
{$ENDIF}
  System.IOUtils,
  System.DateUtils;

{ TTemplateRegistry }

constructor TTemplateRegistry.Create;
{$IFDEF DEBUG}
var
  LPath: string;
{$ENDIF}
begin
  FShutdown := TEvent.Create;
  FThreadDone := TEvent.Create;

  FTemplates := TDictionary<string, ITemplate>.Create;
  FContext := Template.Context([eoEmbedException]);

  TemplateFileExt := '.tpl';

  FContextNameResolver := function(const AName: string; const AContext: TTemplateValue): string
    begin
      exit(AName);
    end;

  FResourceNameResolver := function(const AName: string): string
    begin
      exit(AName.ToUpper.Replace('.', '_', [rfReplaceAll]));
    end;

  FFileNameResolver := function(const AName: string): string
    begin
      exit(TPath.Combine(TTemplateRegistry.Instance.TemplateRootFolder, AName));
    end;

  FTemplateRootFolder := TPath.Combine(TPath.GetDirectoryName(paramstr(0)), 'templates');
{$IFDEF DEBUG}
  if not TDirectory.Exists(FTemplateRootFolder) then
  begin
    LPath := TPath.GetFullPath(TPath.Combine(TPath.Combine(TPath.Combine(TPath.GetDirectoryName(paramstr(0)), '..'), '..'), 'templates'));
    if TDirectory.Exists(LPath) then
    begin
      FTemplateRootFolder := LPath;
    end;
  end;
{$ENDIF}
  FContext.Variable['CopyrightYear'] := YearOf(Now);

  FContext.TemplateResolverWithContext := function(const AContext: ITemplateContext; const AName: string; const AResolveContext: TTemplateValue): ITemplate
    begin
      exit(GetTemplate(AName, AResolveContext));
    end;

{$IFDEF DEBUG}
  setlength(FLoadStrategy, 2);
  FLoadStrategy[0] := tlsLoadFile;
  FLoadStrategy[1] := tlsLoadResource;
  FRefreshIntervalS := 5;
  AutomaticRefresh := true;
{$ELSE}
  FRefreshIntervalS := 10;
  setlength(FLoadStrategy, 1);
  FLoadStrategy[1] := tlsLoadResource;
  AutomaticRefresh := false;
{$ENDIF}
end;

destructor TTemplateRegistry.Destroy;
begin
  if FThread <> nil then
  begin
    FShutdown.SetEvent;
    FThreadDone.WaitFor();
  end;
  FThreadDone.Free;
  FShutdown.Free;
  FTemplates.Free;
  inherited;
end;

function TTemplateRegistry.Eval(const ATemplateName: string): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  exit(Template.Eval(FContext, LTemplate));
end;

procedure TTemplateRegistry.Eval(const AOutputStream: TStream; const ATemplateName: string);
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  Template.Eval(FContext, LTemplate, AOutputStream);
end;

procedure TTemplateRegistry.Eval<T>(const AOutputStream: TStream; const ATemplateName: string; const AData: T; const AContext: TTemplateValue);
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName, AContext);
  Template.Eval(FContext, LTemplate, AData, AOutputStream);
end;

function TTemplateRegistry.Eval<T>(const ATemplateName: string; const AData: T): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  exit(Template.Eval<T>(LTemplate, AData));
end;

procedure TTemplateRegistry.Eval<T>(const AOutputStream: TStream; const ATemplateName: string; const AData: T);
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  Template.Eval<T>(LTemplate, AData, AOutputStream);
end;

function TTemplateRegistry.Eval<T>(const ATemplateName: string; const AData: T; const AContext: TTemplateValue): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName, AContext);
  exit(Template.Eval(FContext, LTemplate, AData));
end;

procedure TTemplateRegistry.EvalWithContext<T, TContext>(const AOutputStream: TStream; const ATemplateName: string; const AData: T; const AContext: TContext);
begin
  Eval<T>(AOutputStream, ATemplateName, AData, TTemplateValue.From<TContext>(AContext));
end;

function TTemplateRegistry.EvalWithContext<T, TContext>(const ATemplateName: string; const AData: T; const AContext: TContext): string;
begin
  exit(Eval<T>(ATemplateName, AData, TTemplateValue.From<TContext>(AContext)));
end;

procedure TTemplateRegistry.EvalWithContext<TContext>(const AOutputStream: TStream; const ATemplateName: string; const AContext: TContext);
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName, AContext);
  Template.Eval(FContext, LTemplate, AOutputStream);
end;

function TTemplateRegistry.EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName, AContext);
  exit(Template.Eval(FContext, LTemplate));
end;

class procedure TTemplateRegistry.Finalize;
begin
  FTemplateRegistry.Free;
  FLock.Free;
end;

class function TTemplateRegistry.GetInstance: TTemplateRegistry;
begin
  if assigned(FTemplateRegistry) then
    exit(FTemplateRegistry);
  FLock.Acquire;
  try
    if not assigned(FTemplateRegistry) then
      FTemplateRegistry := TTemplateRegistry.Create;
  finally
    FLock.Release;
  end;
  exit(FTemplateRegistry);
end;

function TTemplateRegistry.GetTemplate(const ATemplateName: string): ITemplate;
begin
  exit(GetTemplate(ATemplateName, nil));
end;

function TTemplateRegistry.GetTemplate(const ATemplateName: string; const AContext: TTemplateValue): ITemplate;
var
  LExts: TArray<string>;
  LTemplateName: string;

  function LoadFromResource(out ATemplate: ITemplate): boolean;
  var
    LName: string;
    LExt: integer;
  begin
    ATemplate := nil;
    for LExt := low(LExts) to high(LExts) do
    begin
      LName := FResourceNameResolver(LTemplateName + LExts[LExt]);
      try
        ATemplate := TResourceTemplate.Create(FContext, LName);
        Log('Loaded template from resource: %s', [LName]);
        exit(true);
      except
        on e: Exception do
          if LExt = high(LExts) then
          begin
            LogException(e);
          end;
      end;
    end;
    exit(false);
  end;

  function LoadFromFile(out ATemplate: ITemplate): boolean;
  var
    LName: string;
    LExt: integer;
  begin
    result := false;
    ATemplate := nil;
    for LExt := low(LExts) to high(LExts) do
    begin
      LName := FFileNameResolver(LTemplateName + LExts[LExt]);
      if TFile.Exists(LName) then
      begin
        try
          ATemplate := TFileTemplate.Create(FContext, LName);
          Log('Loaded template from file: %s', [LName]);
          exit(true);
        except
          on e: Exception do
            if LExt = high(LExts) then
            begin
              LogException(e);
            end;
        end;
      end;
    end;
  end;

  function LoadFromCustom(out ATemplate: ITemplate): boolean;
  var
    LName: string;
    LExt: integer;
  begin
    result := false;
    ATemplate := nil;
    if not assigned(FCustomTemplateLoader) then
      exit;

    for LExt := low(LExts) to high(LExts) do
    begin
      try
        LName := FResourceNameResolver(LTemplateName + LExts[LExt]);
        ATemplate := FCustomTemplateLoader(FContext, LName, AContext);
        Log('Loaded template from custom loader: %s', [LName]);
        exit(true);
      except
        on e: Exception do
          if LExt = high(LExts) then
          begin
            LogException(e);
          end;
      end;
    end;
  end;

var
  LLoadStrategy: TTemplateLoadStrategy;
  LFound: boolean;
  LTemplateAttempts: array [0 .. 1] of string;
begin
  if assigned(FContextNameResolver) then
    LTemplateName := FContextNameResolver(ATemplateName, AContext)
  else
    LTemplateName := ATemplateName;

  LTemplateAttempts[0] := LTemplateName;
  LTemplateAttempts[1] := ATemplateName;
  LFound := false;

  for LTemplateName in LTemplateAttempts do
  begin
    if FContext.TryGetContextTemplate(LTemplateName, result, AContext) then
      exit;

    FLock.Acquire;
    try
      if FTemplates.TryGetValue(LTemplateName, result) then
        exit;
    finally
      FLock.Release;
    end;
    result := nil;
    setlength(LExts, 2);
    LExts[0] := FTemplateFileExt;
    LExts[1] := '';
    for LLoadStrategy in TTemplateRegistry.Instance.LoadStrategy do
    begin
      case LLoadStrategy of
        tlsLoadResource:
          begin
            LFound := LoadFromResource(result);
            if LFound then
            begin
              break;
            end;
          end;
        tlsLoadFile:
          begin
            LFound := LoadFromFile(result);
            if LFound then
            begin
              break;
            end;
          end;
        tlsLoadCustom:
          begin
            LFound := LoadFromCustom(result);
            if LFound then
            begin
              break;
            end;
          end;
      end;
    end;
    if LFound or (LTemplateAttempts[1] = LTemplateName) then
    begin
      break;
    end;
  end;
  if not assigned(result) then
    raise ETemplateNotResolved.Create(LTemplateName);
  FLock.Acquire;
  try
    FTemplates.Add(LTemplateName, result);
  finally
    FLock.Release;
  end;
end;

function TTemplateRegistry.GetTemplate<T>(const ATemplateName: string; const AContext: T): ITemplate;
begin
  exit(GetTemplate(ATemplateName, TTemplateValue.From<T>(AContext)));
end;

class procedure TTemplateRegistry.Initialize;
begin
  FLock := TCriticalSection.Create;
end;

procedure TTemplateRegistry.Log(const AMsg: string; const AArgs: array of const);
begin
  if assigned(FLogger) then
    FLogger(AMsg, AArgs);
end;

procedure TTemplateRegistry.LogException(const AException: Exception);
begin
  if assigned(FExceptionLogger) then
    FExceptionLogger(AException);
end;

procedure TTemplateRegistry.Refresh;
var
  LRefreshable: IRefreshableTemplate;
  LTemplate: ITemplate;
begin
  FLock.Acquire;
  try
    for LTemplate in FTemplates.Values do
    begin
      if supports(LTemplate, IRefreshableTemplate, LRefreshable) then
      begin
        LRefreshable.Refresh;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TTemplateRegistry.RemoveTemplate(const ATemplateName: string);
begin
  FLock.Acquire;
  try
    FContext.RemoveTemplate(ATemplateName);
    FTemplates.Remove(ATemplateName);
  finally
    FLock.Release;
  end;
end;

procedure TTemplateRegistry.SetAutomaticRefresh(const Value: boolean);
begin
  if FAutomaticRefresh = Value then
    exit;

  FAutomaticRefresh := Value;
  if Value then
  begin
    FThread := TThread.CreateAnonymousThread(
      procedure
      begin
        while true do
        begin
          case FShutdown.WaitFor(TTemplateRegistry.Instance.RefreshIntervalS * 1000) of
            wrSignaled:
              break;
            wrTimeout:
              ;
          end;
          Refresh;
        end;
        FThreadDone.SetEvent;
      end);
{$IFDEF DEBUG}
    TThread.NameThreadForDebugging('TTemplateRegistry.RefreshThread', FThread.ThreadID);
{$ENDIF}
    FThread.Start;
  end
  else if FThread <> nil then
  begin
    FShutdown.SetEvent;
    FThreadDone.WaitFor();
    FThread := nil;
  end;
end;

procedure TTemplateRegistry.SetLoadStrategy(const Value: TArray<TTemplateLoadStrategy>);
begin
  FLoadStrategy := Value;
  if (length(FLoadStrategy) = 1) and (FLoadStrategy[0] = tlsLoadResource) then
    AutomaticRefresh := false;
end;

procedure TTemplateRegistry.SetRefreshIntervalS(const Value: integer);
begin
  if Value < 5 then
    raise ETemplateRefreshTooFrequent.Create();
  FRefreshIntervalS := Value;
end;

procedure TTemplateRegistry.SetTemplateFileExt(const Value: string);
begin
  FTemplateFileExt := Value;
  if not FTemplateFileExt.StartsWith('.') then
    FTemplateFileExt := '.' + FTemplateFileExt;
end;

{ TAbstractProxyTemplate }

procedure TAbstractProxyTemplate.Accept(const AVisitor: ITemplateVisitor);
begin
  FTemplate.Accept(AVisitor);
end;

constructor TAbstractProxyTemplate.Create(const ATemplate: ITemplate);
begin
  FTemplate := ATemplate;
end;

function TAbstractProxyTemplate.GetCount: integer;
begin
  exit(FTemplate.Count);
end;

function TAbstractProxyTemplate.GetFilename: string;
begin
  exit(FTemplate.FileName);
end;

function TAbstractProxyTemplate.GetItem(const AOffset: integer): IStmt;
begin
  exit(FTemplate.GetItem(AOffset));
end;

function TAbstractProxyTemplate.GetLastItem: IStmt;
begin
  exit(FTemplate.GetLastItem);
end;

function TAbstractProxyTemplate.GetLine: integer;
begin
  exit(FTemplate.line);
end;

function TAbstractProxyTemplate.GetPos: integer;
begin
  exit(FTemplate.pos);
end;

procedure TAbstractProxyTemplate.FlattenTemplate;
begin
  FTemplate.FlattenTemplate;
end;

procedure TAbstractProxyTemplate.OptimiseTemplate(const AOptions: TParserOptions);
begin
  FTemplate.OptimiseTemplate(AOptions);
end;

procedure TAbstractProxyTemplate.SetFilename(const AFilename: string);
begin
  FTemplate.FileName := AFilename;
end;

procedure TAbstractProxyTemplate.SetLine(const Aline: integer);
begin
  FTemplate.line := Aline;
end;

procedure TAbstractProxyTemplate.SetPos(const Apos: integer);
begin
  FTemplate.pos := Apos;
end;

{ TResourceTemplate }

constructor TResourceTemplate.Create(const AContext: ITemplateContext; const AName: string);
var
  LStream: TStream;
  LTemplate: ITemplate;
begin
  LStream := TResourceStream.Create(HInstance, AName, RT_RCDATA);
  LTemplate := Template.Parse(AContext, LStream);
  inherited Create(LTemplate);
  LTemplate.FileName := '[Resource(' + AName + ')]';
end;

{ TFileTemplate }

constructor TFileTemplate.Create(const AContext: ITemplateContext; const AFilename: string);
begin
  FContext := AContext;
  inherited Create(nil);
  Load(AFilename, TFile.GetLastWriteTime(AFilename));
end;

procedure TFileTemplate.Load(const AFilename: string; const ATime: TDateTime);
var
  LStream: TStream;
  LTemplate: ITemplate;
begin
  if FModifiedAt = ATime then
    exit;
{$IFDEF SUPPORT_BUFFERED_STREAM}
  LStream := TBufferedFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
{$ELSE}
  LStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
{$ENDIF}
  LTemplate := Template.Parse(FContext, LStream);
  inherited Create(LTemplate);
  LTemplate.FileName := AFilename;
  FModifiedAt := ATime;
end;

procedure TFileTemplate.Refresh;
begin
  Load(FTemplate.FileName, TFile.GetLastWriteTime(FTemplate.FileName));
end;

{ ETemplateNotResolved }

constructor ETemplateNotResolved.Create(const ATemplateName: string);
begin
  inherited CreateResFmt(@STemplateNotFound, [ATemplateName]);
end;

{ ETemplateRefreshTooFrequent }

constructor ETemplateRefreshTooFrequent.Create;
begin
  inherited CreateRes(@SRefreshTooFrequent);
end;

initialization

TTemplateRegistry.Initialize;

finalization

TTemplateRegistry.Finalize;

end.
