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
  TestuWebScraper in 'TestuWebScraper.pas' {$R *.RES},
  uWebScraper in '..\uWebScraper.pas',
  uUserAgent in '..\uUserAgent.pas',
  DUnitTestRunner,
  Delphi.Mocks.Helpers in 'Delphi.Mocks.Helpers.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := true;
  DUnitTestRunner.RunRegisteredTests;

end.
