/// <summary>
/// The main window
/// </summary>
unit uMain;

interface

uses
  // System
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  // FMX
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Memo, FMX.ScrollBox, FMX.Controls.Presentation;

type
  /// <summary>
  /// The main form
  /// </summary>
  TFormMain = class(TForm)
    MemoURLs: TMemo;
    MemoResult: TMemo;
    ButtonGetTitles: TButton;
    SplitterTB: TSplitter;
    PanelInput: TPanel;
    PanelOutput: TPanel;
    procedure ButtonGetTitlesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  /// <summary>
  /// The main form
  /// </summary>
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses uWebScraper;

procedure TFormMain.ButtonGetTitlesClick(Sender: TObject);
begin
  MemoResult.Text := '';
  if MemoURLs.Lines.Count > 0 then
  begin
    ButtonGetTitles.Text := 'Please wait...';
    Application.ProcessMessages;
    try
      TWebScraper.GetTitlesFromURLs(MemoURLs.Lines, MemoResult.Lines);
    finally
      ButtonGetTitles.Text := 'Get titles';
    end;
  end;
end;

end.
