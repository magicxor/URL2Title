program URL2Title;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  uMain in 'uMain.pas' {FormMain},
  uWebScraper in 'uWebScraper.pas',
  uUserAgent in 'uUserAgent.pas',
  {$IFDEF MSWINDOWS}
  dorPunyCode in 'dorPunyCode.pas'
  {$ELSE}
  dorPunyCodeCap in 'dorPunyCodeCap.pas'
  {$ENDIF};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ELSE}
{$ENDIF}
  FMX.Types.GlobalUseDirect2D := False; // avoid bug with AMD Dual Graphics
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
