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
unit Sempare.Template.ResourceStrings;

interface

resourcestring
  SStatementNotSupportedInVisitor = 'Statement not supported in visitor';
  SExpressionNotSupportedInVisitor = 'Expression not supported in visitor';
  SIndexOutOfBounds = 'Index out of bounds';
  SContextStartTokenMustBeTwoCharsLong = 'Context StartToken must be two characters long.';
  SContextEndTokenMustBeTwoCharsLong = 'Context EndToken must be two characters long.';
  SUnexpectedLexerState = 'Unexpected lexer state.';
  STypeNotSupported = 'Type not supported.';
  STypesAreNotOfTheSameType = 'Types are not of the same type';
  STemplateNotFound = 'Template not found: %s';
  SStackFrameCanOnlyBeDefinedOnAClassOrRecord = 'StackFrame must be defined on a class or record.';
  SGetEnumeratorNotFoundOnObject = 'GetEnumerator not found on object. Type %s referenced.';
  SValueIsNotEnumerable = 'Value is not enumerable';
  SOnlyOneDimensionalArraysAreSupported = 'Only one dimensional arrays are supported.';
  SCannotDereferenceValueOnObject = 'Cannot dereference ''%s'' in %s';
  SDictionary = 'dictionary';
  SDataset = 'dataset';
  SCannotDereferenceValiable = 'Cannot dereference variable';
  SCannotFindValiable = 'Cannot find variable ''%s''';
  SBooleanTypeExpected = 'Boolean type(s) expected';
  SNumericTypeExpected = 'Numeric type(s) expected';
  SStringTypeExpected = 'String type expected';
  SEnumerableTypeExpected = 'Enumerable type expected';
  SForOpNotSupported = 'Forop not supported: %s';
  SContinueShouldBeInALoop = 'Continue should be in a for/while Stmt';
  SElIfExpected = 'ElIF expected';
  SEndNotExpected = 'End not expected';
  SUnexpectedToken = 'UnexpectedToken';
  SFunctionNotRegisteredInContext = 'Function %s not registered in context.';
  SParsingErrorExpecting = 'Parsing error. Expecting: %s';
  SStringOrNumericTypesExpected = 'String or numeric types expected';
  SBinOpNotSupported = 'Binop not supported';
  SUnaryOpNotSupported = 'Unary op not supported';
  SMaxRuntimeOfMsHasBeenExceeded = 'Max runtime of %dms has been exceeded.';
  SNumberOfArgsMismatch = 'Number of arguments mismatch';
  SInputOfRequiredTypeNotFound = 'Input of required type not found';
  SCycleStatementMustBeInALoop = 'Cycle statement must be in a loop';
  SDecimalSeparatorMustBeACommaOrFullStop = 'Decimal separator must be a comma or a full stop';
  STooManyParameters = 'Too many parameters';
  SInvalidCharacterDetected = 'Invalid character detected';
  SRefreshTooFrequent = 'Template refresh too frequent';

implementation

end.
