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
unit Sempare.Template.Design.Register;

interface

procedure Register;

implementation

uses
  DesignIntf,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  Sempare.Template,
  Sempare.Template.Common,
  Sempare.Template.Design.Properties,
  Sempare.Template.Components;

procedure Register;

begin
  RegisterPropertyEditor(TypeInfo(string), TSempareBootVelocityTemplate, 'TemplateText', TTemplateProperty);
  RegisterPropertyEditor(TypeInfo(TVelocityVariables), TSempareBootVelocityContext, 'Variables', TVariablesProperty);
  RegisterPropertyEditor(TypeInfo(TVelocityVariables), TSempareBootVelocityEngine, 'Variables', TVariablesProperty);
  RegisterComponents('Sempare Boot', [TSempareBootVelocityContext, TSempareBootVelocityTemplate, TSempareBootVelocityEngine]);
end;

end.
