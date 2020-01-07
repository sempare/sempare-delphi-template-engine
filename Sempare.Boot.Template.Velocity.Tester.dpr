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
program Sempare.Boot.Template.Velocity.Tester;

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
  Sempare.Boot.Template.Velocity.Functions.Test in 'src\Sempare.Boot.Template.Velocity.Functions.Test.pas',
  Sempare.Boot.Template.Velocity.Lexer.Test in 'src\Sempare.Boot.Template.Velocity.Lexer.Test.pas',
  Sempare.Boot.Template.Velocity.StackFrame.Test in 'src\Sempare.Boot.Template.Velocity.StackFrame.Test.pas',
  Sempare.Boot.Template.Velocity.Test in 'src\Sempare.Boot.Template.Velocity.Test.pas',
  Sempare.Boot.Template.Velocity.TestIf in 'src\Sempare.Boot.Template.Velocity.TestIf.pas',
  Sempare.Boot.Template.Velocity.TestFor in 'src\Sempare.Boot.Template.Velocity.TestFor.pas',
  Sempare.Boot.Template.Velocity.TestDeref in 'src\Sempare.Boot.Template.Velocity.TestDeref.pas',
  Sempare.Boot.Template.Velocity.TestInclude in 'src\Sempare.Boot.Template.Velocity.TestInclude.pas',
  Sempare.Boot.Template.Velocity.TestExpr in 'src\Sempare.Boot.Template.Velocity.TestExpr.pas',
  Sempare.Boot.Template.Velocity.TestAssign in 'src\Sempare.Boot.Template.Velocity.TestAssign.pas',
  Sempare.Boot.Template.Velocity.TestJson in 'src\Sempare.Boot.Template.Velocity.TestJson.pas',
  Sempare.Boot.Template.Velocity.TestDictionary in 'src\Sempare.Boot.Template.Velocity.TestDictionary.pas',
  Sempare.Boot.Template.Velocity.Test.Arr in 'src\Sempare.Boot.Template.Velocity.Test.Arr.pas',
  Sempare.Boot.Template.Velocity.TestCall in 'src\Sempare.Boot.Template.Velocity.TestCall.pas',
  Sempare.Boot.Template.Velocity.NewLineOption.Test in 'src\Sempare.Boot.Template.Velocity.NewLineOption.Test.pas',
  Sempare.Boot.Template.Velocity.Context.Test in 'src\Sempare.Boot.Template.Velocity.Context.Test.pas',
  Sempare.Boot.Template.Velocity.Components.Test in 'src\Sempare.Boot.Template.Velocity.Components.Test.pas';

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;

begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    // tell the runner how we will log things
    // Log to the console window
    logger := TDUnitXConsoleLogger.Create(True);
    runner.AddLogger(logger);
    // Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; // When true, Assertions must be made during tests;

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

{$IFNDEF CI}
    // We don't want this happening when running under CI.
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
