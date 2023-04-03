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
unit Sempare.Template.JSON;

interface

{$I 'Sempare.Template.Compiler.inc'}

uses
{$IFDEF SUPPORT_JSON_DBX}
  Data.DBXJSON
{$ELSE}
    System.JSON
{$ENDIF}
    ;

type
{$IFDEF SUPPORT_JSON_DBX}
  TJsonValue = Data.DBXJSON.TJsonValue;
  TJSONString = Data.DBXJSON.TJSONString;
  TJSONNumber = Data.DBXJSON.TJSONNumber;
  TJsonObject = Data.DBXJSON.TJsonObject;
  TJSONNull = Data.DBXJSON.TJSONNull;
  TJSONPair = Data.DBXJSON.TJSONPair;
  TJSONTrue = Data.DBXJSON.TJSONTrue;
  TJSONFalse = Data.DBXJSON.TJSONFalse;
{$ELSE}
  TJsonValue = System.JSON.TJsonValue;
  //TJSONBool = System.JSON.TJSONBool; // not in XE6
  TJSONString = System.JSON.TJSONString;
  TJSONNumber = System.JSON.TJSONNumber;
  TJsonObject = System.JSON.TJsonObject;
  TJSONNull = System.JSON.TJSONNull;
  TJSONPair = System.JSON.TJSONPair;
  TJSONTrue = System.JSON.TJSONTrue;
  TJSONFalse = System.JSON.TJSONFalse;
{$ENDIF}

implementation

end.
