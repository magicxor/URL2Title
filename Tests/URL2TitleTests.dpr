program URL2TitleTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  TestuWebScraper in 'TestuWebScraper.pas',
  uWebScraper in '..\Source\uWebScraper.pas',
  dorPunyCode in '..\Source\dorPunyCode.pas',
  uUserAgent in '..\Source\uUserAgent.pas',
  ParameterizedTester in '..\..\!misc\ParameterizedTester\Source\ParameterizedTester.pas',
  PtTypes in '..\..\!misc\ParameterizedTester\Source\PtTypes.pas',
  PtMethodInvoker in '..\..\!misc\ParameterizedTester\Source\PtMethodInvoker.pas',
  PtWorker in '..\..\!misc\ParameterizedTester\Source\PtWorker.pas',
  Delphi.Mocks.Helpers in '..\..\!misc\ParameterizedTester\Source\Delphi.Mocks.Helpers.pas',
  PtGenericWorker in '..\..\!misc\ParameterizedTester\Source\PtGenericWorker.pas',
  PtObjectClone in '..\..\!misc\ParameterizedTester\Source\PtObjectClone.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

