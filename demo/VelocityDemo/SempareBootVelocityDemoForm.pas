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
unit SempareBootVelocityDemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections,
  Sempare.Boot.Template.Velocity.Component.Engine,
  Sempare.Boot.Template.Velocity.Component.Template,
  Sempare.Boot.Template.Velocity.Component.Context, Vcl.StdCtrls, Vcl.OleCtrls,
  SHDocVw, Vcl.Grids,
  Sempare.Boot.Template.Velocity, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Sempare.Boot.Template.Velocity.Common;

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
    Context: TSempareBootVelocityContext;
    Template: TSempareBootVelocityTemplate;
    evalEngine: TSempareBootVelocityEngine;
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
  private
    { Private declarations }
    FFilename: string;
    Finit: boolean;
    procedure Process;
    procedure GridPropsToContext;
    procedure WriteTmpHtml;
    procedure SetOption(const AEnable: boolean; const AOption: TVelocityEvaluationOption);
  public
    { Public declarations }
  end;

var
  FormRealTime: TFormRealTime;

implementation

uses
  System.IoUtils,
  Sempare.Boot.Template.Velocity.Context;

{$R *.dfm}

procedure TFormRealTime.butClearClick(Sender: TObject);
var
  i: Integer;
begin
  memoTemplate.Lines.Text := '';
  FFilename := '';
  butSave.Enabled := false;
  for i := 1 to properties.RowCount do
  begin
    properties.Cells[0, i] := '';
    properties.Cells[1, i] := '';
  end;
  Process;
end;

procedure TFormRealTime.butOpenClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt := '.velocity';
  OpenDialog1.Filter := '*.velocity';
  if OpenDialog1.Execute then
  begin
    FFilename := OpenDialog1.FileName;
    memoTemplate.Lines.LoadFromFile(FFilename, Context.Encoding);
    butSave.Enabled := false;
    pcTemplate.ActivePageIndex := 0;
    pcOutput.ActivePageIndex := 0;
  end;
end;

procedure TFormRealTime.butSaveAsClick(Sender: TObject);
begin
  SaveDialog1.DefaultExt := '.velocity';
  SaveDialog1.Filter := '*.velocity';
  if FFilename = '' then
    SaveDialog1.FileName := 'output.velocity'
  else
    SaveDialog1.FileName := FFilename;
  if SaveDialog1.Execute then
  begin

    FFilename := SaveDialog1.FileName;
    memoTemplate.Lines.SaveToFile(FFilename, Context.Encoding);
  end;
end;

procedure TFormRealTime.butSaveClick(Sender: TObject);
begin
  if FFilename = '' then
    exit;
  memoTemplate.Lines.SaveToFile(FFilename, Context.Encoding);
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
  result := AValue;
end;

procedure TFormRealTime.cbHtmlClick(Sender: TObject);
begin
  if cbHtml.Checked then
    Context.Context.UseHtmlVariableEncoder
  else
    Context.Context.VariableEncoder := DefaultEncoder;
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
    Context.Context.NewLine := '<br>'#13#10
  else
    Context.Context.NewLine := #13#10;
  SetOption(cbUseHtmlBR.Checked, eoReplaceNewline);
end;

procedure TFormRealTime.cbSetEncodingClick(Sender: TObject);
begin
  if cbSetEncoding.Checked then
  begin
    case cmbEncoding.ItemIndex of
      0:
        Context.Encoding := TEncoding.ASCII;
      1:
        Context.Encoding := TEncoding.UTF8;
    else
      Context.Encoding := TEncoding.UTF8WithoutBOM;
    end;
  end
  else
    Context.Encoding := TEncoding.UTF8WithoutBOM;
  Process;
end;

procedure TFormRealTime.FormCreate(Sender: TObject);
begin
  Template.TemplateText := memoTemplate.Lines.Text;
  properties.Cells[0, 0] := 'Variable';
  properties.Cells[1, 0] := 'Value';
  memoOutput.Lines.Text := '';
  memoTemplate.Lines.Text := '';
  memoPrettyPrint.Lines.Text := '';
  WebBrowser1.Enabled := true;
  context.MaxRunTimeMs := 5000;
  cbHtml.Checked := true;
  cbUseHtmlBR.Checked := true;
  WebBrowser1.Enabled := true;
  pcTemplate.ActivePageIndex := 0;
  pcOutput.ActivePageIndex := 0;
  Finit := true;
end;

procedure TFormRealTime.FormResize(Sender: TObject);
begin
  if width < 1000 then
    width := 1000;
end;

procedure TFormRealTime.GridPropsToContext;
var
  i: Integer;
  k, v: string;
begin
  Context.variables.Clear;
  for i := 1 to properties.RowCount do
  begin
    k := trim(properties.Cells[0, i]);
    if k = '' then
      continue;
    v := trim(properties.Cells[1, i]);
    Context.variables[k] := v;
  end;
end;

procedure TFormRealTime.memoTemplateChange(Sender: TObject);
begin
  try
    if FFilename <> '' then
      butSave.Enabled := true;
    Template.TemplateText := memoTemplate.Lines.Text;
    Process;
  except
    on e: exception do
      memoOutput.Lines.Text := e.Message;
  end;
end;

procedure TFormRealTime.Process;
begin
  if not Finit then
    exit;
  GridPropsToContext;
  try
    evalEngine.Enabled := true;
    memoOutput.Lines.Text := evalEngine.Text;
    memoPrettyPrint.Lines.Text := Velocity.PrettyPrint(Template.Template);
  except
    on e: exception do
    begin
      memoOutput.Lines.Text := e.Message;
      memoPrettyPrint.Lines.Text := '';
    end;
  end;
  WriteTmpHtml;
end;

procedure TFormRealTime.propertiesGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  Process;
end;

procedure TFormRealTime.propertiesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  Process;
end;

procedure TFormRealTime.SetOption(const AEnable: boolean; const AOption: TVelocityEvaluationOption);
begin
  if AEnable then
    Context.Options := Context.Options + [AOption]
  else
    Context.Options := Context.Options - [AOption];
  Process;
end;

procedure TFormRealTime.WriteTmpHtml;
var
  url: string;
begin
  if not WebBrowser1.Enabled or WebBrowser1.Busy then
    exit;
  try
    memoOutput.Lines.SaveToFile('out.htm', Context.Encoding);
    url := 'file://' + ExpandUNCFileName(GetCurrentDir).Replace('\', '/', [rfReplaceAll]) + '/out.htm';
    WebBrowser1.navigate(url);
  except
    on e: exception do
    begin

    end;
  end;
end;

end.
