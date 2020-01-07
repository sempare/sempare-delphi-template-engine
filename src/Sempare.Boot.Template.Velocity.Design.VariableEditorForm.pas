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
unit Sempare.Boot.Template.Velocity.Design.VariableEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin,
  Sempare.Boot.Template.Velocity,
  Sempare.Boot.Template.Velocity.Common;

type
  TFormVariableEditor = class(TForm)
    gridProperties: TStringGrid;
    butCancel: TButton;
    butOk: TButton;
    ToolBar1: TToolBar;
    butAdd: TToolButton;
    butDelete: TToolButton;
    images: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure init;
    procedure butAddClick(Sender: TObject);
    procedure butDeleteClick(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure gridPropertiesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure populateGrid(const AVariables: TVelocityVariables);
    procedure populateDict(const AVariables: TVelocityVariables);
  end;

implementation

uses
  System.Generics.Collections;

{$R *.dfm}

procedure TFormVariableEditor.butAddClick(Sender: TObject);
begin
  gridProperties.RowCount := gridProperties.RowCount + 1;
end;

procedure TFormVariableEditor.butCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormVariableEditor.butDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if gridProperties.Row = 0 then
    exit;
  for i := gridProperties.Row to gridProperties.RowCount - 1 do
  begin
    gridProperties.Cells[0, i] := gridProperties.Cells[0, i + 1];
    gridProperties.Cells[1, i] := gridProperties.Cells[1, i + 1];
  end;
  gridProperties.RowCount := gridProperties.RowCount - 1;
end;

procedure TFormVariableEditor.butOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormVariableEditor.FormCreate(Sender: TObject);
begin
  gridProperties.RowCount := 2;
  init;
end;

procedure TFormVariableEditor.FormResize(Sender: TObject);
begin
  gridProperties.DefaultColWidth := (width - 30) div 2;
end;

procedure TFormVariableEditor.gridPropertiesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  if ARow = 0 then
    init;
end;

procedure TFormVariableEditor.init;
begin
  gridProperties.Cells[0, 0] := 'Variable';
  gridProperties.Cells[1, 0] := 'Value';
end;

procedure TFormVariableEditor.populateDict(const AVariables: TVelocityVariables);
var
  i: Integer;
  k, v: string;
begin
  AVariables.Clear;
  for i := 1 to gridProperties.RowCount do
  begin
    k := trim(gridProperties.Cells[0, i]);
    if k = '' then
      continue;
    v := trim(gridProperties.Cells[1, i]);
    AVariables.SetItem(k, v);
  end;
end;

procedure TFormVariableEditor.populateGrid(const AVariables: TVelocityVariables);
var
  p: TPair<string, TVelocityValue>;
  i: Integer;
begin
  init;
  gridProperties.RowCount := AVariables.GetCount + 1;
  i := 1;
  for p in AVariables do
  begin
    gridProperties.Cells[0, i] := p.Key;
    gridProperties.Cells[1, i] := p.Value.AsString;
    inc(i);
  end;
end;

end.
