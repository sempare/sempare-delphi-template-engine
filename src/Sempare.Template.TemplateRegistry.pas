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
  public
    class procedure Initialize;
    class procedure Finalize;
  private
    class function GetInstance: TTemplateRegistry; static;
    function GetResourceName(const AName, AExt: string): string;
    function GetFilename(const AName, AExt: string): string;
    function LoadResource(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;
    function LoadFile(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;
    function LoadCustom(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;
  private type
    TGetName = function(const AName, AExt: string): string of object;
    TLoadTemplate = function(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate of object;
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
    FLoadMethods: array [TTemplateLoadStrategy] of TLoadTemplate;
    FGetNames: array [TTemplateLoadStrategy] of TGetName;

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

    function GetTemplate(const ATemplateName: string): ITemplate; overload; inline;
    function GetTemplate(const ATemplateName: string; const AContext: TTemplateValue): ITemplate; overload;
    function GetTemplate<T>(const ATemplateName: string; const AContext: T): ITemplate; overload; inline;
    procedure RemoveTemplate(const ATemplateName: string);
    procedure ClearTemplates;

    procedure Eval<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream); overload;
    function Eval<T>(const ATemplateName: string; const AData: T): string; overload;

    procedure Eval(const ATemplateName: string; const AOutputStream: TStream); overload;
    function Eval(const ATemplateName: string): string; overload;

    // with context
    procedure Eval<T>(const ATemplateName: string; const AContext: TTemplateValue; const AData: T; const AOutputStream: TStream); overload;
    function Eval<T>(const ATemplateName: string; const AContext: TTemplateValue; const AData: T): string; overload;

    procedure EvalWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream); overload;
    function EvalWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T): string; overload;

    procedure EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream); overload;
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
  Sempare.Template.Common, // for inline hint
{$IFDEF DEBUG}
  Sempare.Template.PrettyPrint,
{$ENDIF}
  System.IOUtils,
  System.DateUtils;

const
  CLoadStrategy: array [TTemplateLoadStrategy] of string = ('resource', 'file', 'custom');

  { TTemplateRegistry }

procedure TTemplateRegistry.ClearTemplates;
begin
  FLock.Acquire;
  try
    FTemplates.Clear();
  finally
    FLock.Release;
  end;
end;

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

  FContext.TemplateResolverWithContext := function(const AContext: ITemplateContext; const AName: string; const AResolveContext: TTemplateValue; out ACacheInContext: boolean): ITemplate
    begin
      ACacheInContext := false;
      exit(GetTemplate(AName, AResolveContext));
    end;

  FLoadMethods[tlsLoadResource] := LoadResource;
  FLoadMethods[tlsLoadFile] := LoadFile;
  FLoadMethods[tlsLoadCustom] := LoadCustom;

  FGetNames[tlsLoadResource] := GetResourceName;
  FGetNames[tlsLoadFile] := GetFilename;
  FGetNames[tlsLoadCustom] := GetResourceName;

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
  AutomaticRefresh := false;
  FLock.Acquire;
  try
    FreeAndNil(FThreadDone);
    FreeAndNil(FShutdown);
    FreeAndNil(FTemplates);
  finally
    FLock.Release;
  end;
  inherited;
end;

function TTemplateRegistry.Eval(const ATemplateName: string): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  exit(Template.Eval(FContext, LTemplate));
end;

procedure TTemplateRegistry.Eval(const ATemplateName: string; const AOutputStream: TStream);
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  Template.Eval(FContext, LTemplate, AOutputStream);
end;

procedure TTemplateRegistry.Eval<T>(const ATemplateName: string; const AContext: TTemplateValue; const AData: T; const AOutputStream: TStream);
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

procedure TTemplateRegistry.Eval<T>(const ATemplateName: string; const AData: T; const AOutputStream: TStream);
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName);
  Template.Eval<T>(LTemplate, AData, AOutputStream);
end;

function TTemplateRegistry.Eval<T>(const ATemplateName: string; const AContext: TTemplateValue; const AData: T): string;
var
  LTemplate: ITemplate;
begin
  LTemplate := GetTemplate(ATemplateName, AContext);
  exit(Template.EvalWithContext(FContext, LTemplate, AContext, TTemplateValue.From<T>(AData)));
end;

procedure TTemplateRegistry.EvalWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T; const AOutputStream: TStream);
begin
  Eval<T>(ATemplateName, TTemplateValue.From<TContext>(AContext), AData, AOutputStream);
end;

function TTemplateRegistry.EvalWithContext<TContext, T>(const ATemplateName: string; const AContext: TContext; const AData: T): string;
begin
  exit(Eval<T>(ATemplateName, TTemplateValue.From<TContext>(AContext), AData));
end;

procedure TTemplateRegistry.EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext; const AOutputStream: TStream);
begin
  EvalWithContext(ATemplateName, AContext, '', AOutputStream);
end;

function TTemplateRegistry.EvalWithContext<TContext>(const ATemplateName: string; const AContext: TContext): string;
begin
  exit(EvalWithContext(ATemplateName, AContext, ''));
end;

class procedure TTemplateRegistry.Finalize;
begin
  if FLock = nil then
    exit;
  FreeAndNil(FTemplateRegistry);
  FreeAndNil(FLock);
end;

class function TTemplateRegistry.GetInstance: TTemplateRegistry;
var
  LTemplateRegistry: TTemplateRegistry;
begin
  LTemplateRegistry := FTemplateRegistry;
  if assigned(FTemplateRegistry) then
    exit(LTemplateRegistry);
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

function TTemplateRegistry.GetResourceName(const AName: string; const AExt: string): string;
begin
  exit(FResourceNameResolver(AName + AExt));
end;

function TTemplateRegistry.GetFilename(const AName, AExt: string): string;
begin
  exit(FFileNameResolver(AName + AExt));
end;

function TTemplateRegistry.LoadResource(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;
begin
  ACacheInContext := false;
  exit(TResourceTemplate.Create(FContext, AName));
end;

function TTemplateRegistry.LoadFile(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;
begin
  ACacheInContext := false;
  exit(TFileTemplate.Create(FContext, AName));
end;

function TTemplateRegistry.LoadCustom(const AName: string; const AContext: TTemplateValue; out ACacheInContext: boolean): ITemplate;
begin
  exit(FCustomTemplateLoader(FContext, AName, AContext, ACacheInContext));
end;

function TTemplateRegistry.GetTemplate(const ATemplateName: string; const AContext: TTemplateValue): ITemplate;

type
  TTwoStrings = array [0 .. 1] of string;

var
  LExts: TTwoStrings;
  LTemplateName: string;

  function LoadTemplate(const ALoadStrategy: TTemplateLoadStrategy; out ATemplate: ITemplate): boolean;
  var
    LName: string;
    LExt: integer;
    LCacheInContext: boolean;
  begin
    ATemplate := nil;
    for LExt := low(LExts) to high(LExts) do
    begin
      LName := FGetNames[ALoadStrategy](ATemplateName, LExts[LExt]);
      try
        ATemplate := FLoadMethods[ALoadStrategy](LName, AContext, LCacheInContext);
        Log('Loaded template from %s: %s', [CLoadStrategy[ALoadStrategy], LName]);
        exit(true);
      except
        on e: ETemplateEvaluationError do
        begin
          LogException(e);
          exit(true);
        end;
        on e: Exception do
        begin
          if LExt = high(LExts) then
          begin
            LogException(e);
          end;
        end;
      end;
    end;
    exit(false);
  end;

var
  LLoadStrategy: TTemplateLoadStrategy;
  LFound: boolean;
  LTemplateAttempts: TTwoStrings;
  LAttempts: integer;
  i: integer;
begin
  if assigned(FContextNameResolver) then
    LTemplateName := FContextNameResolver(ATemplateName, AContext)
  else
    LTemplateName := ATemplateName;

  LTemplateAttempts[0] := LTemplateName;
  LTemplateAttempts[1] := ATemplateName;

  if LTemplateName = ATemplateName then
    LAttempts := 1
  else
    LAttempts := 2;

  LExts[0] := FTemplateFileExt;
  LExts[1] := '';
  LFound := false;

  for LLoadStrategy in TTemplateRegistry.Instance.LoadStrategy do
  begin
    for i := 0 to LAttempts - 1 do
    begin
      LTemplateName := LTemplateAttempts[i];
      if FContext.TryGetContextTemplate(LTemplateName, result, AContext) then
      begin
        exit;
      end;

      FLock.Acquire;
      try
        if FTemplates.TryGetValue(LTemplateName, result) then
          exit;
      finally
        FLock.Release;
      end;

      LFound := LoadTemplate(LLoadStrategy, result);
      if LFound then
      begin
        break;
      end;

      if LFound then
      begin
        break;
      end;
    end;
    if LFound then
    begin
      break;
    end;
  end;
  LTemplateName := LTemplateAttempts[0];
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
  if not assigned(FLock) then
    exit;
  FLock.Acquire;
  try
    for LTemplate in FTemplates.values do
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
  inherited Create(nil);
  FContext := AContext;
  Load(AFilename, TFile.GetLastWriteTime(AFilename));
end;

procedure TFileTemplate.Load(const AFilename: string; const ATime: TDateTime);
var
  LStream: TStream;
begin
  if FModifiedAt = ATime then
    exit;
{$IFDEF SUPPORT_BUFFERED_STREAM}
  LStream := TBufferedFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
{$ELSE}
  LStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
{$ENDIF}
  FTemplate := Template.Parse(FContext, LStream);
  FTemplate.FileName := AFilename;
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
