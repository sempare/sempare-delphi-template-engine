(*%*********************************************************************************
 *                 ___                                                             *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                     *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                    *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                    *
 *                                    |_|                                          *
 ***********************************************************************************
 *                                                                                 *
 *                        Sempare Templating Engine                                *
 *                                                                                 *
 *                                                                                 *
 *         https://github.com/sempare/sempare-delphi-template-engine               *
 ***********************************************************************************
 *                                                                                 *
 * Copyright (c) 2020 Sempare Limited                                              *
 *                                                                                 *
 * Contact: info@sempare.ltd                                                       *
 *                                                                                 *
 * Licensed under the GPL Version 3.0 or the Sempare Commercial License            *
 * You may not use this file except in compliance with one of these Licenses.      *
 * You may obtain a copy of the Licenses at                                        *
 *                                                                                 *
 * https://www.gnu.org/licenses/gpl-3.0.en.html                                    *
 * https://github.com/sempare/sempare.boot.velocity.oss/docs/commercial.license.md *
 *                                                                                 *
 * Unless required by applicable law or agreed to in writing, software             *
 * distributed under the Licenses is distributed on an "AS IS" BASIS,              *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.        *
 * See the License for the specific language governing permissions and             *
 * limitations under the License.                                                  *
 *                                                                                 *
 ********************************************************************************%*)
unit Sempare.Template.Design.Properties;

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
  Sempare.Template,
  Sempare.Template.Common,
  Sempare.Template.Design.TemplateEditorForm,
  Sempare.Template.Design.VariableEditorForm;

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
