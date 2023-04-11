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
program Sempare.Template.Tester;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  Sempare.Template.TestIf in 'tests\Sempare.Template.TestIf.pas',
  Sempare.Template.TestContext in 'tests\Sempare.Template.TestContext.pas',
  Sempare.Template.TestNewLineOption in 'tests\Sempare.Template.TestNewLineOption.pas',
  Sempare.Template.TestCall in 'tests\Sempare.Template.TestCall.pas',
  Sempare.Template.TestArr in 'tests\Sempare.Template.TestArr.pas',
  Sempare.Template.TestDictionary in 'tests\Sempare.Template.TestDictionary.pas',
  Sempare.Template.TestJson in 'tests\Sempare.Template.TestJson.pas',
  Sempare.Template.TestAssign in 'tests\Sempare.Template.TestAssign.pas',
  Sempare.Template.TestExpr in 'tests\Sempare.Template.TestExpr.pas',
  Sempare.Template.TestInclude in 'tests\Sempare.Template.TestInclude.pas',
  Sempare.Template.TestDeref in 'tests\Sempare.Template.TestDeref.pas',
  Sempare.Template.TestFor in 'tests\Sempare.Template.TestFor.pas',
  Sempare.Template.Test in 'tests\Sempare.Template.Test.pas',
  Sempare.Template.TestStackFrame in 'tests\Sempare.Template.TestStackFrame.pas',
  Sempare.Template.TestLexer in 'tests\Sempare.Template.TestLexer.pas',
  Sempare.Template.TestFunctions in 'tests\Sempare.Template.TestFunctions.pas';

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;

begin
  ReportMemoryLeaksOnShutdown := true;
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
