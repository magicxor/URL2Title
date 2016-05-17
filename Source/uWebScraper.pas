/// <summary>
/// This unit can be used for getting the list of page titles by the list of
/// URLs
/// </summary>
unit uWebScraper;

interface

uses
  // System
  Generics.Collections, System.Generics.Defaults, System.RegularExpressions,
  System.NetEncoding,
  System.Math, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  System.Threading, System.Net.URLClient, System.Net.HttpClientComponent;

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
    /// Get page title
    /// </summary>
    class function GetPageTitle(const AURL: string; const AIndex: Integer;
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

uses uUserAgent;

class function TWebScraper.IsValidURL_RegEx(const URL: string): Boolean;
const
  URL_RegEx = '^http(s)?:\/\/.{3,}$';
begin
  Result := TRegEx.IsMatch(URL, URL_RegEx);
end;

class function TWebScraper.GetPageTitle(const AURL: string; const AIndex: Integer;
  const ALineBreak: string): TTitle;
var
  HttpClient: TNetHTTPClient;
  PageContent: string;
  URI: TURI;
begin
  Result.FIndex := -1;
  Result.FURL := 'Error';
  Result.FTitle := 'Error';

  HttpClient := TNetHTTPClient.Create(nil);
  try
    HttpClient.ConnectionTimeout := CIdTimeout;
    HttpClient.ResponseTimeout := CIdTimeout;
    HttpClient.UserAgent := TUserAgent.GetRandomUA;
    HttpClient.AllowCookies := True;
    HttpClient.HandleRedirects := True;
    HttpClient.MaxRedirects := CIdRedirectMaximum;

    URI := TURI.Create(AURL);

    PageContent := HttpClient.Get(URI.ToString).ContentAsString;
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
    FreeAndNil(HttpClient);
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
            Title := GetPageTitle(URLStrings[I], I, ResultStrings.LineBreak);
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
