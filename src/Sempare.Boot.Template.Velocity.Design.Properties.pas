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
unit Sempare.Boot.Template.Velocity.Design.Properties;

interface

uses
  DesignEditors,
  DesignIntf;

type
  TVariablesProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TTemplateProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses
  System.UITypes,
  Sempare.Boot.Template.Velocity,
  Sempare.Boot.Template.Velocity.Common,
  Sempare.Boot.Template.Velocity.Design.TemplateEditorForm,
  Sempare.Boot.Template.Velocity.Design.VariableEditorForm;

{ TVariablesProperty }

procedure TVariablesProperty.Edit;
var
  FormVariableEditor: TFormVariableEditor;
  Dict: TVelocityVariables;
begin
  FormVariableEditor := TFormVariableEditor.Create(nil);
  try
    Dict := TVelocityVariables(getordvalue);
    FormVariableEditor.populateGrid(Dict);
    if FormVariableEditor.ShowModal = mrOk then
    begin
      FormVariableEditor.populateDict(Dict);
    end;
  finally
    FormVariableEditor.Free;
  end;
end;

function TVariablesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TTemplateProperty }

procedure TTemplateProperty.Edit;
var
  FormTemplateEditor: TFormTemplateEditor;
begin
  FormTemplateEditor := TFormTemplateEditor.Create(nil);
  try
    FormTemplateEditor.memo.Text := GetStrValue;
    if FormTemplateEditor.ShowModal = mrOk then
    begin
      SetStrValue(FormTemplateEditor.memo.Text);
    end;
  finally
    FormTemplateEditor.Free;
  end;
end;

function TTemplateProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
