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
unit Sempare.Template.PlaygroundForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  System.Generics.Collections,
  Sempare.Template,
  Sempare.Template.AST,
  Sempare.Template.PrettyPrint,
  Sempare.Template.Common,
  Vcl.StdCtrls,
  Vcl.OleCtrls,
  SHDocVw,
  Vcl.Grids,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

type
  TFormTemplateEnginePlayground = class(TForm)
    memoOutput: TMemo;
    memoTemplate: TMemo;
    cbStripRecurringSpaces: TCheckBox;
    cbConvertTabsToSpaces: TCheckBox;
    memoPrettyPrint: TMemo;
    cbEvalEarly: TCheckBox;
    cbEvalVarsEarly: TCheckBox;
    cbStripRecurringNewlines: TCheckBox;
    cbTrimLines: TCheckBox;
    cbRaiseErrorWhenVariableNotFound: TCheckBox;
    cbHtml: TCheckBox;
    cbSetEncoding: TCheckBox;
    cbUseHtmlBR: TCheckBox;
    WebBrowser1: TWebBrowser;
    butClear: TButton;
    butSave: TButton;
    butOpen: TButton;
    OpenDialog1: TOpenDialog;
    cmbEncoding: TComboBox;
    butSaveAs: TButton;
    SaveDialog1: TSaveDialog;
    pcTemplate: TPageControl;
    tsTemplate: TTabSheet;
    tsPrettyPrint: TTabSheet;
    Panel1: TPanel;
    Splitter1: TSplitter;
    pcOutput: TPageControl;
    tsOutput: TTabSheet;
    tsWebBrowser: TTabSheet;
    gbOptions: TGroupBox;
    Image1: TImage;
    lblTitle: TLabel;
    properties: TStringGrid;
    GroupBox1: TGroupBox;
    butEval: TButton;
    cbAutoEvaluate: TCheckBox;
    cmbCustomScriptTags: TComboBox;
    cbOptimiseTemplate: TCheckBox;
    cbUseCustomScriptTags: TCheckBox;
    cbFlattenTemplate: TCheckBox;
    cbShowWhitespace: TCheckBox;
    lblPosition: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure cbConvertTabsToSpacesClick(Sender: TObject);
    procedure cbStripRecurringSpacesClick(Sender: TObject);
    procedure cbTrimLinesClick(Sender: TObject);
    procedure cbStripRecurringNewlinesClick(Sender: TObject);
    procedure cbRaiseErrorWhenVariableNotFoundClick(Sender: TObject);
    procedure cbHtmlClick(Sender: TObject);
    procedure cbUseHtmlBRClick(Sender: TObject);
    procedure cbSetEncodingClick(Sender: TObject);
    procedure cbEvalEarlyClick(Sender: TObject);
    procedure cbEvalVarsEarlyClick(Sender: TObject);
    procedure butClearClick(Sender: TObject);
    procedure butSaveClick(Sender: TObject);
    procedure butOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memoTemplateChange(Sender: TObject);
    procedure propertiesGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure propertiesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure butSaveAsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure butEvalClick(Sender: TObject);
    procedure cbUseCustomScriptTagsClick(Sender: TObject);
    procedure cbOptimiseTemplateClick(Sender: TObject);
    procedure cbFlattenTemplateClick(Sender: TObject);
    procedure cmbCustomScriptTagsChange(Sender: TObject);
    procedure cbShowWhitespaceClick(Sender: TObject);
    procedure memoTemplateMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoTemplateKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FEncoding: TEncoding;
    FContext: ITemplateContext;
    FTemplate: ITemplate;
    FFilename: string;
    Finit: boolean;
    procedure Process;
    procedure GridPropsToContext;
    procedure WriteTmpHtml;
    procedure SetOption(const AEnable: boolean; const AOption: TTemplateEvaluationOption);
    procedure SetScriptTags(const AIdx: Integer);
    procedure Eval;
  public
    { Public declarations }
    procedure OnException(Sender: TObject; E: Exception);
  end;

var
  FormTemplateEnginePlayground: TFormTemplateEnginePlayground;

implementation

uses
  System.IoUtils,
  Sempare.Template.Context;

{$R *.dfm}

procedure TFormTemplateEnginePlayground.butClearClick(Sender: TObject);
var
  LIdx: Integer;
begin
  memoTemplate.Lines.Text := '';
  FFilename := '';
  butSave.Enabled := false;
  for LIdx := 1 to properties.RowCount do
  begin
    properties.Cells[0, LIdx] := '';
    properties.Cells[1, LIdx] := '';
  end;
  Eval;
end;

procedure TFormTemplateEnginePlayground.butEvalClick(Sender: TObject);
begin
  Eval;
end;

procedure TFormTemplateEnginePlayground.butOpenClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt := '.template';
  OpenDialog1.Filter := '*.template';
  if OpenDialog1.Execute then
  begin
    FFilename := OpenDialog1.FileName;

    memoTemplate.Lines.LoadFromFile(FFilename, FEncoding);
    butSave.Enabled := false;
    pcTemplate.ActivePageIndex := 0;
    pcOutput.ActivePageIndex := 0;
  end;
end;

procedure TFormTemplateEnginePlayground.butSaveAsClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt := '.template';
  SaveDialog1.Filter := '*.template';
  if FFilename = '' then
    SaveDialog1.FileName := 'output.template'
  else
    SaveDialog1.FileName := FFilename;
  if SaveDialog1.Execute then
  begin

    FFilename := SaveDialog1.FileName;
    memoTemplate.Lines.SaveToFile(FFilename, FEncoding);
  end;
end;

procedure TFormTemplateEnginePlayground.butSaveClick(Sender: TObject);
begin
  if FFilename = '' then
    exit;
  memoTemplate.Lines.SaveToFile(FFilename, FEncoding);
  butSave.Enabled := false;
end;

procedure TFormTemplateEnginePlayground.cbConvertTabsToSpacesClick(Sender: TObject);
begin
  SetOption(cbConvertTabsToSpaces.Checked, eoConvertTabsToSpaces);
end;

procedure TFormTemplateEnginePlayground.cbEvalEarlyClick(Sender: TObject);
begin
  SetOption(cbEvalEarly.Checked, eoEvalEarly);
end;

procedure TFormTemplateEnginePlayground.cbEvalVarsEarlyClick(Sender: TObject);
begin
  SetOption(cbEvalVarsEarly.Checked, eoEvalVarsEarly);
end;

procedure TFormTemplateEnginePlayground.cbFlattenTemplateClick(Sender: TObject);
begin
  SetOption(cbFlattenTemplate.Checked, eoFlattenTemplate);
end;

function DefaultEncoder(const AValue: string): string;
begin
  exit(AValue);
end;

procedure TFormTemplateEnginePlayground.cbHtmlClick(Sender: TObject);
begin
  if cbHtml.Checked then
    FContext.UseHtmlVariableEncoder
  else
    FContext.VariableEncoder := DefaultEncoder;
  Eval;
end;

procedure TFormTemplateEnginePlayground.cbOptimiseTemplateClick(Sender: TObject);
begin
  SetOption(cbOptimiseTemplate.Checked, eoOptimiseTemplate);
  if cbOptimiseTemplate.Checked then
    cbFlattenTemplate.Checked := true;
end;

procedure TFormTemplateEnginePlayground.cbRaiseErrorWhenVariableNotFoundClick(Sender: TObject);
begin
  SetOption(cbRaiseErrorWhenVariableNotFound.Checked, eoRaiseErrorWhenVariableNotFound);
end;

procedure TFormTemplateEnginePlayground.cbStripRecurringNewlinesClick(Sender: TObject);
begin
  SetOption(cbStripRecurringNewlines.Checked, eoStripRecurringNewlines);
end;

procedure TFormTemplateEnginePlayground.cbStripRecurringSpacesClick(Sender: TObject);
begin
  SetOption(cbStripRecurringSpaces.Checked, eoStripRecurringSpaces);
end;

procedure TFormTemplateEnginePlayground.cbTrimLinesClick(Sender: TObject);
begin
  SetOption(cbTrimLines.Checked, eoTrimLines);
end;

procedure TFormTemplateEnginePlayground.cbUseCustomScriptTagsClick(Sender: TObject);
begin
  if cbUseCustomScriptTags.Checked then
    SetScriptTags(cmbCustomScriptTags.ItemIndex)
  else
    SetScriptTags(0);
end;

procedure TFormTemplateEnginePlayground.cbUseHtmlBRClick(Sender: TObject);
begin
  if cbUseHtmlBR.Checked then
    FContext.NewLine := '<br>'#13#10
  else
    FContext.NewLine := #13#10;
end;

procedure TFormTemplateEnginePlayground.cmbCustomScriptTagsChange(Sender: TObject);
begin
  cbUseCustomScriptTags.Checked := true;
  SetScriptTags(cmbCustomScriptTags.ItemIndex);
  Eval;
end;

procedure TFormTemplateEnginePlayground.Eval;
var
  LActivePage: TTabSheet;
begin
  try
    if FFilename <> '' then
      butSave.Enabled := true;
    FTemplate := Template.Parse(FContext, memoTemplate.Lines.Text);
    Process;
    // this is a hack so that app does not throw an exception
    // during shutdown. it seems that the webbrowser must be visible
    // or else it does not shutdown properly
    LActivePage := pcOutput.ActivePage;
    pcOutput.ActivePage := tsWebBrowser;
    if not cbHtml.Checked then
      pcOutput.ActivePage := LActivePage;
  except
    on E: Exception do
      memoOutput.Lines.Text := E.Message;
  end;
end;

procedure TFormTemplateEnginePlayground.cbSetEncodingClick(Sender: TObject);
begin
  if cbSetEncoding.Checked then
  begin
    case cmbEncoding.ItemIndex of
      0:
        FEncoding := TEncoding.ASCII;
      1:
        FEncoding := TEncoding.UTF8;
    else
      FEncoding := TEncoding.UTF8WithoutBOM;
    end;
  end
  else
    FEncoding := TEncoding.UTF8WithoutBOM;
  Eval;
end;

procedure TFormTemplateEnginePlayground.cbShowWhitespaceClick(Sender: TObject);
begin
  if cbShowWhitespace.Checked then
    FContext.WhitespaceChar := #183
  else
    FContext.WhitespaceChar := ' ';
  Eval;
end;

procedure TFormTemplateEnginePlayground.FormCreate(Sender: TObject);
begin
  FContext := Template.Context();
  FContext.Variable['name'] := 'world';
  properties.Cells[0, 1] := 'name';
  properties.Cells[1, 1] := 'world';
  FEncoding := TEncoding.UTF8WithoutBOM;
  FTemplate := Template.Parse(FContext, '');
  properties.Cells[0, 0] := 'Variable';
  properties.Cells[1, 0] := 'Value';
  memoOutput.Lines.Text := '';
  memoTemplate.Lines.Text := '';
  memoPrettyPrint.Lines.Text := '';
  WebBrowser1.Enabled := true;
{$IF defined(RELEASE)}
  FContext.MaxRunTimeMs := 5000;
{$ENDIF}
  cbHtml.Checked := true;
  cbUseHtmlBR.Checked := true;
  cbFlattenTemplate.Checked := true;
  cbOptimiseTemplate.Checked := true;
  WebBrowser1.Enabled := true;
  pcTemplate.ActivePageIndex := 0;
  pcOutput.ActivePageIndex := 0;

  memoTemplate.Text := '<% template("local_template") %> Hello <% name %><br> <% end %> ' + #13#10 + //
    '  ' + #13#10 + //
    ' Welcome to the <i>Sempare Template Engine</i> <b><% SEMPARE_TEMPLATE_ENGINE_VERSION %></b> playpen project. ' + #13#10 + //
    '  ' + #13#10 + //
    ' You can prototype and test templates here.<p> ' + #13#10 + //
    '  ' + #13#10 + //
    '  ' + #13#10 + //
    ' For HTML output, preview using the brower tab to the right.<p> ' + #13#10 + //
    '  ' + #13#10 + //
    '  ' + #13#10 + //
    ' Press the "Evaluate" button to process this template or enable the "auto evaluate" option to process on every keypress.<p> ' + #13#10 + //
    '  ' + #13#10 + //
    '  ' + #13#10 + //
    ' <% include("local_template") %> ' + #13#10 + //
    '  ' + #13#10 + //
    ' This project is available on <a href="https://github.com/sempare/sempare-delphi-template-engine">https://github.com/sempare/sempare-delphi-template-engine</a><p> ' + #13#10 + //
    '  ' + #13#10 + //
    ' <% include("local_template") %> ' + #13#10 + //
    '  ' + #13#10 + //
    '  ' + #13#10 + //
    ' Templates can work nicely on almost any Delphi construct.<p> ' + #13#10 + //
    '  ' + #13#10 + //
    '  You can have loops:<br> ' + #13#10 + //
    '  ' + #13#10 + //

    ' <% for i := 1 to 10 %> ' + #13#10 + //
    '    <% i %><br> ' + #13#10 + //
    ' <% end %> ' + #13#10 + //
    '  <p>' + #13#10 + //
    '  You can define local variables and have conditional blocks:<br> ' + #13#10 + //
    '  ' + #13#10 + //

    ' <% val := 42 %> <-- try change this ' + #13#10 + //
    ' <% if val = 42 %> ' + #13#10 + //
    '    the value is 42 ' + #13#10 + //
    ' <% else %> ' + #13#10 + //
    ' the value is <b><% val %></b>! ' + #13#10 + //
    ' <% end %> ' + #13#10 + //
    '  <p>' + #13#10 + //
    ' Review the documentation and tests to explore all the features. ' + #13#10 + //
    ' <p> ' + #13#10 + //
    ' Otherwise, please raise an issue on github or email <a href="mailto:support@sempare.ltd">support@sempare.ltd</a> for support. ' + #13#10 + //
    '  ' + #13#10 + //
    ' If you like this project, please consider supporting enhancements via a commercial license which also entitles you to priority support.<p> ' + #13#10;

  Finit := true;
end;

procedure TFormTemplateEnginePlayground.FormResize(Sender: TObject);
begin
  if width < 1000 then
    width := 1000;
end;

procedure TFormTemplateEnginePlayground.GridPropsToContext;
var
  LIdx: Integer;
  LKey, LValue: string;
begin
  FContext.Variables.Clear;
  for LIdx := 1 to properties.RowCount do
  begin
    LKey := trim(properties.Cells[0, LIdx]);
    if LKey = '' then
      continue;
    LValue := trim(properties.Cells[1, LIdx]);
    FContext.Variables[LKey] := LValue;
  end;
end;

procedure TFormTemplateEnginePlayground.memoTemplateChange(Sender: TObject);
begin
  if not cbAutoEvaluate.Checked then
    exit;
  butEvalClick(Sender);
end;

function GetRowCol(const AMemo: TMemo): string;
begin
  exit(format('(Line: %d, Position: %d)   ', [AMemo.CaretPos.Y + 1, AMemo.CaretPos.X + 1]));
end;

procedure TFormTemplateEnginePlayground.memoTemplateKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  lblPosition.Caption := GetRowCol(memoTemplate);
end;

procedure TFormTemplateEnginePlayground.memoTemplateMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lblPosition.Caption := GetRowCol(memoTemplate);
end;

procedure TFormTemplateEnginePlayground.OnException(Sender: TObject; E: Exception);
begin
end;

procedure TFormTemplateEnginePlayground.Process;
var
  LPrettyOk: boolean;
begin
  if not Finit then
    exit;
  GridPropsToContext;
  LPrettyOk := false;
  try
    memoPrettyPrint.Lines.Text := Sempare.Template.Template.PrettyPrint(FTemplate);
    LPrettyOk := true;
    memoOutput.Lines.Text := Template.Eval(FContext, FTemplate);
  except
    on E: Exception do
    begin
      memoOutput.Lines.Text := E.Message;
      if not LPrettyOk then
        memoPrettyPrint.Lines.Text := '';
    end;
  end;
  WriteTmpHtml;
end;

procedure TFormTemplateEnginePlayground.propertiesGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  if cbAutoEvaluate.Checked then
    Eval;
end;

procedure TFormTemplateEnginePlayground.propertiesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if cbAutoEvaluate.Checked then
    Eval;
end;

procedure TFormTemplateEnginePlayground.SetOption(const AEnable: boolean; const AOption: TTemplateEvaluationOption);
begin
  if AEnable then
    FContext.Options := FContext.Options + [AOption]
  else
    FContext.Options := FContext.Options - [AOption];
  Eval;
end;

procedure TFormTemplateEnginePlayground.SetScriptTags(const AIdx: Integer);
begin
  case AIdx of
    1:
      begin
        FContext.StartToken := '{{';
        FContext.EndToken := '}}';
      end;
    2:
      begin
        FContext.StartToken := '<+';
        FContext.EndToken := '+>';
      end;
    3:
      begin
        FContext.StartToken := '{+';
        FContext.EndToken := '+}';
      end;
    4:
      begin
        FContext.StartToken := '{%';
        FContext.EndToken := '%}';
      end;
    5:
      begin
        FContext.StartToken := '<<';
        FContext.EndToken := '>>';
      end;
  else
    begin
      FContext.StartToken := '<%';
      FContext.EndToken := '%>';
    end;
  end;
end;

procedure TFormTemplateEnginePlayground.WriteTmpHtml;
var
  url: string;
begin
  if not WebBrowser1.Enabled or WebBrowser1.Busy then
    exit;
  try
    memoOutput.Lines.SaveToFile('out.htm', FEncoding);
    url := 'file://' + ExpandUNCFileName(GetCurrentDir).Replace('\', '/', [rfReplaceAll]) + '/out.htm';
    WebBrowser1.navigate(url);
  except
    on E: Exception do
    begin

    end;
  end;
end;

end.
