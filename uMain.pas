unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Threading,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Memo, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, Generics.Collections, System.Generics.Defaults, System.RegularExpressions,
  System.NetEncoding, System.Math, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
  IdSSLOpenSSL, FMX.ScrollBox, FMX.Controls.Presentation {, UrlMon};

type
  TTitle = record
    FIndex: Integer;
    FURL: string;
    FTitle: string;
  end;

  TFormMain = class(TForm)
    MemoURLs: TMemo;
    MemoResult: TMemo;
    ButtonGetTitles: TButton;
    SplitterTB: TSplitter;
    PanelInput: TPanel;
    PanelOutput: TPanel;
    procedure ButtonGetTitlesClick(Sender: TObject);
    procedure IdHTTP1Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer;
      var Handled: Boolean; var VMethod: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.IdHTTP1Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer;
  var Handled: Boolean; var VMethod: string);
// handling HTTP(S) redirects (302 and so on)
begin
  Handled := True;
end;

(*
  function SimpleIsValidURL(const URL: string): Boolean;
  {
  How about using the PathIsURL function in the Windows API?
  Update: This is already wrapped in the Delphi RTL in the ShLwApi unit.
  }
  begin
  if IsValidURL(nil, PWideChar(URL), 0) = S_OK then
  Result := True
  else
  Result := False;
  end;
*)

function SimpleIsValidURL(const URL: string): Boolean;
const
  URL_RegEx = '^http(s)?:\/\/.{3,}$';
begin
  Result := TRegEx.IsMatch(URL, URL_RegEx);
end;

procedure TFormMain.ButtonGetTitlesClick(Sender: TObject);
var
  ThreadListOfTitle: TThreadList<TTitle>; // Thread-safe generic list of TTitle
  FinalListOfTitle: TList<TTitle>; // NOT thread-safe generic list of TTitle
  Comparison: TComparison<TTitle>; // Custom generic comparator of TTitle
  aTitle: TTitle; // Title (temp var)
  CRLFconst: string;
const
  timeout   = 5000; // 5 seconds
  useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64; rv:36.0) Gecko/20100101 Firefox/36.0'; // FF 36
begin
  MemoResult.Text := '';
  if MemoURLs.Lines.Count > 0 then
  begin
    ButtonGetTitles.Text := 'Please wait...';
    Application.ProcessMessages;

    ThreadListOfTitle := TThreadList<TTitle>.Create;
    try
      ThreadListOfTitle.Duplicates := dupAccept; // for higer perfomance
      // get all titles to list
      // parallel for: 1 thread per processor core
      TParallel.For(0, MemoURLs.Lines.Count - 1,
        procedure(I: Integer)
        var
          IdHTTP1: TIdHTTP; // HTTP
          IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL; // HTTPS
          Title: TTitle;
          PageContent: string;
          SuccessFlag: Boolean;
          procedure AddToThreadList(aTitle: TTitle);
          // add item to thread-safe list
          begin
            ThreadListOfTitle.LockList;
            try
              ThreadListOfTitle.Add(aTitle);
              SuccessFlag := True;
            finally
              ThreadListOfTitle.UnlockList;
            end;
          end;

        begin
          if SimpleIsValidURL(MemoURLs.Lines[I]) then
          begin
            SuccessFlag := False;
            try
              IdHTTP1 := TIdHTTP.Create(Self);
              try
                IdHTTP1.ConnectTimeout := timeout;
                IdHTTP1.ReadTimeout := timeout;
                IdHTTP1.Request.useragent := useragent; // TODO: add other request params
                // ...
                IdHTTP1.AllowCookies := True;
                IdHTTP1.HandleRedirects := True;
                IdHTTP1.RedirectMaximum := 10;
                IdHTTP1.OnRedirect := IdHTTP1Redirect;
                IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(Self);
                try
                  IdHTTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;
                  PageContent := IdHTTP1.Get(IdHTTP1.URL.URLEncode(MemoURLs.Lines[I]));
                  // Auto charset
                  Title.FIndex := I;
                  Title.FURL := MemoURLs.Lines[I];
                  // search for title tag
                  Title.FTitle := TRegEx.Match(PageContent, '<title\b[^>]*>(.*?)</title>',
                    [roIgnoreCase, roMultiLine]).Value;
                  // remove html tags and comments
                  Title.FTitle := TRegEx.Replace(Title.FTitle,
                    '</?[a-z][a-z0-9]*[^<>]*>|<!--.*?-->', '', [roIgnoreCase, roMultiLine]);
                  // decode html entities
                  Title.FTitle := TNetEncoding.HTML.Decode(Title.FTitle);
                  AddToThreadList(Title);
                finally
                  FreeAndNil(IdSSLIOHandlerSocketOpenSSL1);
                end;
              finally
                IdHTTP1.Disconnect;
                FreeAndNil(IdHTTP1);
              end;
            except
              on E: Exception do
                if not SuccessFlag then
                begin
                  CRLFconst := sLineBreak;
                  //
                  Title.FIndex := I;
                  Title.FURL := MemoURLs.Lines[I];
                  Title.FTitle := TRegEx.Replace(E.ClassName + ': ' + E.Message, sLineBreak, ' ');
                  // for Windows
                  Title.FTitle := TRegEx.Replace(E.ClassName + ': ' + E.Message,
                    CRLFconst.Substring(CRLFconst.Length - 1), ' ');
                  // for OS X and Android
                  AddToThreadList(Title);
                end;
            end;
          end
          else
          begin
            Title.FIndex := I;
            Title.FURL := MemoURLs.Lines[I];
            Title.FTitle := 'Is not valid URL';
            AddToThreadList(Title);
          end;
        end);
      // show list in memo
      FinalListOfTitle := ThreadListOfTitle.LockList;
      try
        Comparison := function(const Left, Right: TTitle): Integer
          begin
            // Result := CompareValue(Item1.FIndex, Item2.FIndex);  // but... I am a Hipster!
            Result := TComparer<Integer>.Default.Compare(Left.FIndex, Right.FIndex);
          end;
        FinalListOfTitle.Sort(TComparer<TTitle>.Construct(Comparison));
        for aTitle in FinalListOfTitle do
          MemoResult.Lines.Add(aTitle.FTitle);
      finally
        ThreadListOfTitle.UnlockList;
      end;
    finally
      FreeAndNil(ThreadListOfTitle);
      ButtonGetTitles.Text := 'Get titles';
    end;
  end;
end;

end.
