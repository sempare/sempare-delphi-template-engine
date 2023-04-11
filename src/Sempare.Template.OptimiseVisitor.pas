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
unit Sempare.Template.OptimiseVisitor;

interface

uses
  System.Generics.Collections,
  Sempare.Template.AST,
  Sempare.Template.Common,
  Sempare.Template.Visitor;

type
  IOptimiseVisitor = interface(ITemplateVisitor)
    ['{98EB0C4D-6AE5-41C4-82AE-FA30DFBE11F5}']
  end;

  TOptimiseVisitor = class(TNoExprTemplateVisitor, IOptimiseVisitor)
  private
    FFlatten, FOptimise: boolean;
  public
    constructor Create(const AFlatten, AOptimise: boolean);

    procedure Visit(const ATemplate: ITemplate); overload; override;
  end;

implementation

{ TOptimiseVisitor }

constructor TOptimiseVisitor.Create(const AFlatten, AOptimise: boolean);
begin
  FFlatten := AFlatten or AOptimise;
  FOptimise := AOptimise;
end;

procedure TOptimiseVisitor.Visit(const ATemplate: ITemplate);
begin
  inherited;
  if FFlatten then
    ATemplate.FlattenTemplate;
  if FOptimise then
    ATemplate.OptimiseTemplate;
end;

end.
