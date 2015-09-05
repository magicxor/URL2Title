/// <summary>
/// This unit can be used for getting the list of page titles by the list of
/// URLs
/// </summary>
/// <seealso cref="IdHTTP">
/// Indy HTTP component
/// </seealso>
unit uWebScraper;

interface

uses
  // Indy
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdURI, IdGlobal,
  // System
  Generics.Collections, System.Generics.Defaults, System.RegularExpressions, System.NetEncoding,
  System.Math, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Threading, System.Net.URLClient;

{$IFDEF DEBUG}
{$RTTI EXPLICIT METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])} // + RTTI for private ...
{$ELSE}
{$ENDIF}

type
  /// <summary>
  /// Represents a single web page element.
  /// </summary>
  /// <remarks>
  /// If exception will be raised, it will be stored in FTitle field
  /// </remarks>
  /// <example>
  /// FIndex = 0; FURL = 'https://www.google.com'; FTitle = 'Google';
  /// </example>
  TTitle = record
    /// <summary>
    /// Index of current page URL in list
    /// </summary>
    FIndex: Integer;
    /// <summary>
    /// Page URL
    /// </summary>
    FURL: string;
    /// <summary>
    /// Page title
    /// </summary>
    FTitle: string;
  end;

  /// <summary>
  /// This class can get a list of page titles from list of URLs
  /// </summary>
  TWebScraper = class
  private
    /// <summary>
    /// Check if text is a valid URL.
    /// </summary>
    /// <param name="URL">
    /// Page URL
    /// </param>
    class function IsValidURL_RegEx(const URL: string): Boolean;
    /// <summary>
    /// Milliseconds to wait for successful completion of a connection
    /// attempt / a read operation.
    /// </summary>
    class procedure IdHTTP1Redirect(Sender: TObject; var dest: string; var NumRedirect: Integer;
      var Handled: Boolean; var VMethod: string);
    /// <summary>
    /// Get page title using IdHTTP component.
    /// </summary>
    class function GetTitleByIdHTTP(const AURL: string; const AIndex: Integer;
      const ALineBreak: string): TTitle;

  const
    /// <summary>
    /// ConnectTimeout is a public Integer property that indicates the
    /// maximum number of milliseconds to wait for successful completion of a
    /// connection attempt on the client.
    /// </summary>
    CIdTimeout = 5000; // 5 seconds
    /// <summary>
    /// Maximum allowed number of redirects.
    /// </summary>
    CIdRedirectMaximum = 10;

  public
    /// <summary>
    /// Get page titles from list of URLs.
    /// </summary>
    class procedure GetTitlesFromURLs(URLStrings, ResultStrings: TStrings);
  end;

implementation

uses uUserAgent, {$IFDEF MSWINDOWS}dorPunyCode{$ELSE}dorPunyCodeCap{$ENDIF};

class procedure TWebScraper.IdHTTP1Redirect(Sender: TObject; var dest: string;
  var NumRedirect: Integer; var Handled: Boolean; var VMethod: string);
// handling HTTP(S) redirects (302 and so on)
begin
  Handled := True;
end;

class function TWebScraper.IsValidURL_RegEx(const URL: string): Boolean;
const
  URL_RegEx = '^http(s)?:\/\/.{3,}$';
begin
  Result := TRegEx.IsMatch(URL, URL_RegEx);
end;

class function TWebScraper.GetTitleByIdHTTP(const AURL: string; const AIndex: Integer;
  const ALineBreak: string): TTitle;
var
  IdHTTP1: TIdHTTP; // HTTP
  IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL; // HTTPS
  PageContent, Prepared_URL: string;
  URI: TURI;
begin
  Result.FIndex := -1;
  Result.FURL := 'Error';
  Result.FTitle := 'Error';

  IdHTTP1 := TIdHTTP.Create(nil);
  try
    IdHTTP1.ConnectTimeout := CIdTimeout;
    IdHTTP1.ReadTimeout := CIdTimeout;
    IdHTTP1.Request.UserAgent := TUserAgent.GetRandomUA;
    IdHTTP1.AllowCookies := True;
    IdHTTP1.HandleRedirects := True;
    IdHTTP1.RedirectMaximum := CIdRedirectMaximum;
    IdHTTP1.OnRedirect := TWebScraper.IdHTTP1Redirect;
    IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    try
      IdHTTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;

      if AURL.Contains('[') then // TURI.Create broken in Delphi 10 Seattle (RSP-12131)
        Prepared_URL := AURL
      else
      begin
        URI := TURI.Create(AURL);
        URI.Host := PunycodeEncodeDomain(URI.Host); // TURI.UnicodeToIDNA broken in Delphi 10 Seattle (RSP-12099)
        Prepared_URL := URI.ToString;
      end;

      PageContent := IdHTTP1.Get(Prepared_URL);
      // Auto charset
      Result.FIndex := AIndex;
      Result.FURL := AURL;
      // search for title tag
      Result.FTitle := TRegEx.Match(PageContent, '<title\b[^>]*>(.*?)</title>',
        [roIgnoreCase, roMultiLine]).Value;
      // remove html tags and comments
      Result.FTitle := TRegEx.Replace(Result.FTitle, '</?[a-z][a-z0-9]*[^<>]*>|<!--.*?-->', '',
        [roIgnoreCase, roMultiLine]);
      // decode html entities
      Result.FTitle := TNetEncoding.HTML.Decode(Result.FTitle);
      // remove line breaks (CR+LF or LF)
      Result.FTitle := Result.FTitle.Replace(ALineBreak, ' ');
    finally
      FreeAndNil(IdSSLIOHandlerSocketOpenSSL1);
    end;
  finally
    IdHTTP1.Disconnect;
    FreeAndNil(IdHTTP1);
  end;
end;

class procedure TWebScraper.GetTitlesFromURLs(URLStrings, ResultStrings: TStrings);
var
  /// Thread-safe generic list of TTitle
  ThreadListOfTitle: TThreadList<TTitle>;
  /// NOT thread-safe generic list of TTitle
  FinalListOfTitle: TList<TTitle>;
  /// Declares a reference to a generic callback function used to compare
  /// two TTitle records.
  Comparison: TComparison<TTitle>;
  /// Title (temp var)
  TMPTitle: TTitle;
begin
  ThreadListOfTitle := TThreadList<TTitle>.Create;
  try
    ThreadListOfTitle.Duplicates := dupAccept; // for higer perfomance
    // get all titles to list
    // parallel for: 1 thread per processor core
    TParallel.For(0, URLStrings.Count - 1,

      procedure(I: Integer)
      var
        Title: TTitle;
        SuccessFlag: Boolean;

        procedure AddToThreadList(ATitle: TTitle);
        // add item to thread-safe list
        begin
          ThreadListOfTitle.LockList;
          try
            ThreadListOfTitle.Add(ATitle);
            SuccessFlag := True;
          finally
            ThreadListOfTitle.UnlockList;
          end;
        end;

      begin
        if IsValidURL_RegEx(URLStrings[I]) then
        begin
          SuccessFlag := False;
          try
            Title := GetTitleByIdHTTP(URLStrings[I], I, ResultStrings.LineBreak);
            AddToThreadList(Title);
          except
            on E: Exception do
              if not SuccessFlag then
              begin
                Title.FIndex := I;
                Title.FURL := URLStrings[I];
                Title.FTitle := E.ClassName + ': ' + E.Message;
                // remove line breaks (CR+LF or LF)
                Title.FTitle := Title.FTitle.Replace(ResultStrings.LineBreak, ' ');
                AddToThreadList(Title);
              end;
          end;
        end
        else
        begin
          Title.FIndex := I;
          Title.FURL := URLStrings[I];
          Title.FTitle := 'Is not valid URL';
          AddToThreadList(Title);
        end;
      end);
    // show list in memo
    FinalListOfTitle := ThreadListOfTitle.LockList;
    try
      Comparison := function(const Left, Right: TTitle): Integer
        begin
          Result := TComparer<Integer>.Default.Compare(Left.FIndex, Right.FIndex);
        end;
      FinalListOfTitle.Sort(TComparer<TTitle>.Construct(Comparison));
      for TMPTitle in FinalListOfTitle do
        ResultStrings.Add(TMPTitle.FTitle);
    finally
      ThreadListOfTitle.UnlockList;
    end;
  finally
    FreeAndNil(ThreadListOfTitle);
  end;
end;

end.
