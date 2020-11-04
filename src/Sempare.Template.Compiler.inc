(*%*************************************************************************************************
 *                 ___                                                                              *
 *                / __|  ___   _ __    _ __   __ _   _ _   ___                                      *
 *                \__ \ / -_) | '  \  | '_ \ / _` | | '_| / -_)                                     *
 *                |___/ \___| |_|_|_| | .__/ \__,_| |_|   \___|                                     *
 *                                    |_|                                                           *
 ****************************************************************************************************
 *                                                                                                  *
 *                        Sempare Templating Engine                                                 *
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
// -----------------------------------------------
{$IF CompilerVersion >= 25} // from XE4 onwards
{$DEFINE SUPPORT_JSON_DBX true}
{$DEFINE BROKEN_ARRAY_BOUNDS true}
{$ENDIF}
// -----------------------------------------------
{$IF CompilerVersion >= 26} // from XE5 onwards
{$UNDEF SUPPORT_JSON_DBX}
{$UNDEF BROKEN_ARRAY_BOUNDS}
{$DEFINE SUPPORT_JSON true}
{$ENDIF}
// -----------------------------------------------
{$IF CompilerVersion >= 28}  // from XE7 onwards
{$DEFINE SUPPORT_NET_ENCODING true}
{$ENDIF}
// -----------------------------------------------
{$IF CompilerVersion >= 29}  // from XE8 onwards
{$DEFINE SUPPORT_AS_VARREC true}
{$ENDIF}
// -----------------------------------------------
{$IF CompilerVersion >= 30} // from Delphi 10.0 Seatle onwards
{$DEFINE SUPPORT_JSON_BOOL true}
{$ENDIF}
// -----------------------------------------------
{$IF CompilerVersion >= 31}  // from Delphi 10.1 Berlin onwards
{$DEFINE SUPPORT_BUFFERED_STREAM true}
{$ENDIF}
// -----------------------------------------------

// Uncomment the following to test TDataSet using FIREDAC TFDMemTable
// {$DEFINE SEMPARE_TEMPLATE_FIREDAC}

// Uncomment the following if Indy is not present
// {$DEFINE SEMPARE_TEMPLATE_NO_INDY}

{$IFNDEF SEMPARE_TEMPLATE_NO_INDY}
{$DEFINE SEMPARE_TEMPLATE_INDY true}
{$ENDIF}
// -----------------------------------------------

// the following uses the defines above to snure the correct defines
// are in place
{$IF defined(SEMPARE_TEMPLATE_INDY) or defined(SUPPORT_NET_ENCODING)}
{$IFDEF SUPPORT_NET_ENCODING}
{$UNDEF SEMPARE_TEMPLATE_INDY}
{$ENDIF}
{$DEFINE SEMPARE_TEMPLATE_HAS_HTML_ENCODER true}
{$ENDIF}
{$IFNDEF SEMPARE_TEMPLATE_HAS_HTML_ENCODER}
{$MESSAGE 'No default html encoder present.'}
{$ENDIF}
