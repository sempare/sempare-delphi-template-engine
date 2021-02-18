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
unit Sempare.Template.DemoForm;

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
  TFormRealTime = class(TForm)
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
  public
    { Public declarations }
    procedure OnException(Sender: TObject; E: Exception);
  end;

var
  FormRealTime: TFormRealTime;

implementation

uses
  System.IoUtils,
  Sempare.Template.Context;

{$R *.dfm}

procedure TFormRealTime.butClearClick(Sender: TObject);
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
  Process;
end;

procedure TFormRealTime.butEvalClick(Sender: TObject);
var
  LActivePage: TTabSheet;
begin
  try
    if FFilename <> '' then
      butSave.Enabled := true;
    FTemplate := Template.Parse(memoTemplate.Lines.Text);
    // Template.TemplateText := memoTemplate.Lines.Text;
    Process;
    // this is a hack so that app does not throw an exception
    // during shutdown. it seems that the webbrowser must be visible
    // or else it does not shutdown properly
    LActivePage := pcOutput.ActivePage;
    pcOutput.ActivePage := tsWebBrowser;
    pcOutput.ActivePage := LActivePage;
  except
    on E: Exception do
      memoOutput.Lines.Text := E.Message;
  end;
end;

procedure TFormRealTime.butOpenClick(Sender: TObject);
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

procedure TFormRealTime.butSaveAsClick(Sender: TObject);
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

procedure TFormRealTime.butSaveClick(Sender: TObject);
begin
  if FFilename = '' then
    exit;
  memoTemplate.Lines.SaveToFile(FFilename, FEncoding);
  butSave.Enabled := false;
end;

procedure TFormRealTime.cbConvertTabsToSpacesClick(Sender: TObject);
begin
  SetOption(cbConvertTabsToSpaces.Checked, eoConvertTabsToSpaces);
end;

procedure TFormRealTime.cbEvalEarlyClick(Sender: TObject);
begin
  SetOption(cbEvalEarly.Checked, eoEvalEarly);
end;

procedure TFormRealTime.cbEvalVarsEarlyClick(Sender: TObject);
begin
  SetOption(cbEvalVarsEarly.Checked, eoEvalVarsEarly);
end;

function DefaultEncoder(const AValue: string): string;
begin
  exit(AValue);
end;

procedure TFormRealTime.cbHtmlClick(Sender: TObject);
begin
  if cbHtml.Checked then
    FContext.UseHtmlVariableEncoder
  else
    FContext.VariableEncoder := DefaultEncoder;
  Process;
end;

procedure TFormRealTime.cbRaiseErrorWhenVariableNotFoundClick(Sender: TObject);
begin
  SetOption(cbRaiseErrorWhenVariableNotFound.Checked, eoRaiseErrorWhenVariableNotFound);
end;

procedure TFormRealTime.cbStripRecurringNewlinesClick(Sender: TObject);
begin
  SetOption(cbStripRecurringNewlines.Checked, eoStripRecurringNewlines);
end;

procedure TFormRealTime.cbStripRecurringSpacesClick(Sender: TObject);
begin
  SetOption(cbStripRecurringSpaces.Checked, eoStripRecurringSpaces);
end;

procedure TFormRealTime.cbTrimLinesClick(Sender: TObject);
begin
  SetOption(cbTrimLines.Checked, eoTrimLines);
end;

procedure TFormRealTime.cbUseHtmlBRClick(Sender: TObject);
begin
  if cbUseHtmlBR.Checked then
    FContext.NewLine := '<br>'#13#10
  else
    FContext.NewLine := #13#10;
  SetOption(cbUseHtmlBR.Checked, eoReplaceNewline);
end;

procedure TFormRealTime.cbSetEncodingClick(Sender: TObject);
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
  Process;
end;

procedure TFormRealTime.FormCreate(Sender: TObject);
begin
  FContext := Template.Context();
  FContext.Variable['name'] := 'world';
  properties.Cells[0, 1] := 'name';
  properties.Cells[1, 1] := 'world';
  FEncoding := TEncoding.UTF8WithoutBOM;
  FTemplate := Template.Parse('');
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
  WebBrowser1.Enabled := true;
  pcTemplate.ActivePageIndex := 0;
  pcOutput.ActivePageIndex := 0;

  memoTemplate.Text := '<% template("local_template") %> Hello <% name %><br> <% end %> ' + #13#10 + //
    '  ' + #13#10 + //
    ' Welcome to the Sempare Template Engine demo project. ' + #13#10 + //
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

procedure TFormRealTime.FormResize(Sender: TObject);
begin
  if width < 1000 then
    width := 1000;
end;

procedure TFormRealTime.GridPropsToContext;
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

procedure TFormRealTime.memoTemplateChange(Sender: TObject);
begin
  if not cbAutoEvaluate.Checked then
    exit;
  butEvalClick(Sender);
end;

procedure TFormRealTime.OnException(Sender: TObject; E: Exception);
begin
end;

procedure TFormRealTime.Process;
begin
  if not Finit then
    exit;
  GridPropsToContext;
  try
    // evalEngine.Enabled := true;
    memoOutput.Lines.Text := Template.Eval(FContext, FTemplate);
    memoPrettyPrint.Lines.Text := Sempare.Template.Template.PrettyPrint(FTemplate);
  except
    on E: Exception do
    begin
      memoOutput.Lines.Text := E.Message;
      memoPrettyPrint.Lines.Text := '';
    end;
  end;
  WriteTmpHtml;
end;

procedure TFormRealTime.propertiesGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  if cbAutoEvaluate.Checked then
    Process;
end;

procedure TFormRealTime.propertiesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if cbAutoEvaluate.Checked then
    Process;
end;

procedure TFormRealTime.SetOption(const AEnable: boolean; const AOption: TTemplateEvaluationOption);
begin
  if AEnable then
    FContext.Options := FContext.Options + [AOption]
  else
    FContext.Options := FContext.Options - [AOption];
  Process;
end;

procedure TFormRealTime.WriteTmpHtml;
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
