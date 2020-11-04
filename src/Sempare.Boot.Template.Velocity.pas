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
unit Sempare.Boot.Template.Velocity;

interface

uses
  Sempare.Template;

const
  eoStripRecurringSpaces = Sempare.Template.TTemplateEvaluationOption.eoStripRecurringSpaces;
  eoConvertTabsToSpaces = Sempare.Template.TTemplateEvaluationOption.eoConvertTabsToSpaces;
  eoNoDefaultFunctions = Sempare.Template.TTemplateEvaluationOption.eoNoDefaultFunctions;
  eoNoPosition = Sempare.Template.TTemplateEvaluationOption.eoNoPosition;
  eoEvalEarly = Sempare.Template.TTemplateEvaluationOption.eoEvalEarly;
  eoEvalVarsEarly = Sempare.Template.TTemplateEvaluationOption.eoEvalVarsEarly;
  eoStripRecurringNewlines = Sempare.Template.TTemplateEvaluationOption.eoStripRecurringNewlines;
  eoTrimLines = Sempare.Template.TTemplateEvaluationOption.eoTrimLines;
  // eoDebug = TVelocityEvaluationOption.eoDebug;
  eoPrettyPrint = Sempare.Template.TTemplateEvaluationOption.eoPrettyPrint;
  eoRaiseErrorWhenVariableNotFound = Sempare.Template.TTemplateEvaluationOption.eoRaiseErrorWhenVariableNotFound;
  eoReplaceNewline = Sempare.Template.TTemplateEvaluationOption.eoReplaceNewline;

type
  TVelocityEvaluationOptions = Sempare.Template.TTemplateEvaluationOptions;
  TVelocityEvaluationOption = Sempare.Template.TTemplateEvaluationOption;
  TVelocityValue = Sempare.Template.TTemplateValue;
  IVelocityContext = Sempare.Template.ITemplateContext;
  IVelocityTemplate = Sempare.Template.ITemplate;
  IVelocityFunctions = Sempare.Template.ITemplateFunctions;
  TVelocityTemplateResolver = Sempare.Template.TTemplateResolver;
  TVelocityEncodeFunction = Sempare.Template.TTemplateEncodeFunction;
  IVelocityVariables = Sempare.Template.ITemplateVariables;
  TUTF8WithoutPreambleEncoding = Sempare.Template.TUTF8WithoutPreambleEncoding;

  Velocity = Sempare.Template.Template;

implementation

end.
