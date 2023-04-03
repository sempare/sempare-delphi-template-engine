program Sempare.Template.RCGenerator;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Sempare.Template,
  System.Generics.Collections,
  System.Classes,
  System.IOUtils,
  System.SysUtils;

type
  THelperClass = class
    class function RCName(const AName: string): string; static;
    class function EscapeBackslash(const AName: string): string; static;
  end;

procedure ScanDirectory(const ADirectory: string; const AExt: TList<string>; const AFiles: TList<string>);
begin
  TDirectory.GetFiles(ADirectory,
    function(const APath: string; const SearchRec: TSearchRec): Boolean
    var
      LExt: string;
    begin
      for LExt in AExt do
      begin
        if string(SearchRec.Name).ToLower.EndsWith(LExt.ToLower) then
        begin
          AFiles.Add(TPath.Combine(APath, SearchRec.Name));
          break;
        end;
      end;
      exit(false);
    end);

  TDirectory.GetDirectories(ADirectory,
    function(const APath: string; const SearchRec: TSearchRec): Boolean
    begin
      ScanDirectory(TPath.Combine(APath, SearchRec.Name), AExt, AFiles);
      exit(false);
    end);
end;

procedure NormaliseInput(var ATempalteRelPath: string; const AFiles: TList<string>);
var
  i: integer;
begin
  if not ATempalteRelPath.EndsWith(TPath.DirectorySeparatorChar) then
    ATempalteRelPath := ATempalteRelPath + TPath.DirectorySeparatorChar;
  for i := 0 to AFiles.Count - 1 do
  begin
    AFiles[i] := ExtractRelativePath(ATempalteRelPath, AFiles[i]);
  end;
end;

procedure WriteRC(const AFilename: string; const AFiles: TList<string>);
type
  TData = record
    Files: TList<string>;
  end;
var
  LStream: TBufferedFileStream;
  LData: TData;
begin
  LData.Files := AFiles;
  LStream := TBufferedFileStream.Create(AFilename, fmCreate);
  try
    TTemplateRegistry.Instance.Eval(LStream, 'sempare_template_rcgenerator_tpl', LData)
  finally
    LStream.Free;
  end;
end;

const
  DEFAULT_EXTS: TArray<string> //
    = ['.ico', '.png', '.jpg', '.jpeg', '.webp', '.tpl', '.bmp', '.gif', '.wbmp'];

procedure main;
var
  LRCFilename: string;
  LTemplatePath: string;
  LTempalteRelPath: string;
  LExt: TList<string>;
  LFiles: TList<string>;
  i: integer;
begin
  if ParamCount < 2 then
  begin
    writeln(TPath.GetFilename(ParamStr(0)) + ' <rcfilename> <templatepath> [<ext>+]');
    writeln;
    writeln('<rcfilename> is the path to the filename that will be generated');
    writeln('<templatepath> is the path to the directory listing all the templates');
    writeln('<ext>+ is one one or more extensions to be included. By default: ' + string.Join(', ', DEFAULT_EXTS));
    writeln('');
    exit;
  end;
  LRCFilename := TPath.GetFullPath(ParamStr(1));
  LTemplatePath := TPath.GetFullPath(ParamStr(2));
  LTempalteRelPath := TPath.GetDirectoryName(LRCFilename);
  LFiles := nil;
  LExt := nil;

  TTemplateRegistry.Instance.LoadStrategy := tlsLoadResource;
  TTemplateRegistry.Instance.Context.Functions.AddFunctions(THelperClass);

  try
    LFiles := TList<string>.Create;
    LExt := TList<string>.Create;

    LExt.AddRange(DEFAULT_EXTS);

    if ParamCount > 2 then
    begin
      LExt.Clear;
      for i := 3 to ParamCount do
      begin
        LExt.Add(ParamStr(i));
      end;
    end;
    ScanDirectory(LTemplatePath, LExt, LFiles);
    NormaliseInput(LTempalteRelPath, LFiles);
    WriteRC(LRCFilename, LFiles);
  finally
    LExt.Free;
    LFiles.Free;
  end;
end;

{ THelperClass }

class function THelperClass.EscapeBackslash(const AName: string): string;
begin
  exit(AName.Replace(TPath.DirectorySeparatorChar, TPath.DirectorySeparatorChar + TPath.DirectorySeparatorChar, [rfReplaceAll]));
end;

class function THelperClass.RCName(const AName: string): string;
var
  LParts: TArray<string>;
begin
  result := AName;
  LParts := result.Split([TPath.DirectorySeparatorChar]);
  delete(LParts, 0, 1);
  result := string.Join(TPath.DirectorySeparatorChar, LParts);
  result := result.ToLower;
  result := result.Replace('.', '_', [rfReplaceAll]);
end;

begin
  try
    main;
  except
    on E: Exception do
      writeln(E.ClassName, ': ', E.Message);
  end;

end.