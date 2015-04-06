program URL2Title;

uses
  System.StartUpCopy,
  FMX.Forms, FMX.Types,
  uMain in 'uMain.pas' {FormMain};

{$R *.res}

begin
  FMX.Types.GlobalUseDirect2D := False; // avoid bug with AMD Dual Graphics
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
