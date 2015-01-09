// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StToHTML.pas 4.04                           *}
{*********************************************************}
{* SysTools: HTML Text Formatter                         *}
{*********************************************************}

{$I StDefine.inc}

unit StToHTML;

interface

uses
  SysUtils, Windows,
  Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StStrms, StBase;

type
  TStOnProgressEvent = procedure(Sender : TObject; Percent : Word) of object;

  TStStreamToHTML = class(TObject)
  protected {private}
    { Private declarations }
    FCaseSensitive   : Boolean;
    FCommentMarkers  : TStringList;
    FEmbeddedHTML    : TStringList;
    FInFileSize      : Cardinal;
    FInFixedLineLen  : integer;
    FInLineTermChar  : Char;
    FInLineTerminator: TStLineTerminator;
    FInputStream     : TStream;
    FInSize          : Cardinal;
    FInTextStream    : TStAnsiTextStream;
    FIsCaseSensitive : Boolean;
    FKeywords        : TStringList;
    FOnProgress      : TStOnProgressEvent;
    FOutputStream    : TStream;
    FOutTextStream   : TStAnsiTextStream;
    FPageFooter      : TStringList;
    FPageHeader      : TStringList;
    FStringMarkers   : TStringList;
    FWordDelims      : String;
  protected
    { Protected declarations }

    {internal methods}
    function ParseBuffer : Boolean;

    procedure SetCommentMarkers(Value : TStringList);
    procedure SetEmbeddedHTML(Value : TStringList);
    procedure SetKeywords(Value : TStringList);
    procedure SetPageFooter(Value : TStringList);
    procedure SetPageHeader(Value : TStringList);
    procedure SetStringMarkers(Value : TStringList);

  public
    { Public declarations }

    property CaseSensitive : Boolean
      read FCaseSensitive
      write FCaseSensitive;

    property CommentMarkers : TStringList
      read FCommentMarkers
      write SetCommentMarkers;

    property EmbeddedHTML : TStringList
      read FEmbeddedHTML
      write SetEmbeddedHTML;

    property InFixedLineLength : integer
      read FInFixedLineLen
      write FInFixedLineLen;

    property InLineTermChar : Char
      read FInLineTermChar
      write FInLineTermChar;

    property InLineTerminator : TStLineTerminator
      read FInLineTerminator
      write FInLineTerminator;

    property InputStream : TStream
      read FInputStream
      write FInputStream;

    property Keywords : TStringList
      read FKeywords
      write SetKeywords;

    property OnProgress : TStOnProgressEvent
      read FOnProgress
      write FOnProgress;

    property OutputStream : TStream
      read FOutputStream
      write FOutputStream;

    property PageFooter : TStringList
      read FPageFooter
      write SetPageFooter;

    property PageHeader : TStringList
      read FPageHeader
      write SetPageHeader;

    property StringMarkers : TStringList
      read FStringMarkers
      write SetStringMarkers;

    property WordDelimiters : String
      read FWordDelims
      write FWordDelims;


    constructor Create;
    destructor Destroy; override;

    procedure GenerateHTML;
  end;


  TStFileToHTML = class(TStComponent)
  protected {private}
    { Private declarations }

    FCaseSensitive      : Boolean;
    FCommentMarkers     : TStringList;
    FEmbeddedHTML       : TStringList;
    FInFile             : TFileStream;
    FInFileName         : String;
    FInLineLength       : integer;
    FInLineTermChar     : Char;
    FInLineTerminator   : TStLineTerminator;
    FKeywords           : TStringList;
    FOnProgress         : TStOnProgressEvent;
    FOutFile            : TFileStream;
    FOutFileName        : String;
    FPageFooter         : TStringList;
    FPageHeader         : TStringList;
    FStream             : TStStreamToHTML;
    FStringMarkers      : TStringList;
    FWordDelims         : String;

  protected

    procedure SetCommentMarkers(Value : TStringList);
    procedure SetEmbeddedHTML(Value : TStringList);
    procedure SetKeywords(Value : TStringList);
    procedure SetPageFooter(Value : TStringList);
    procedure SetPageHeader(Value : TStringList);
    procedure SetStringMarkers(Value : TStringList);

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Execute;

  published
    property CaseSensitive : Boolean
      read FCaseSensitive
      write FCaseSensitive default False;

    property CommentMarkers : TStringList
      read FCommentMarkers
      write SetCommentMarkers;

    property EmbeddedHTML : TStringList
      read FEmbeddedHTML
      write SetEmbeddedHTML;

    property InFileName : String
      read FInFileName
      write FInFileName;

    property InFixedLineLength : integer
      read FInLineLength
      write FInLineLength default 80;

    property InLineTermChar : Char
      read FInLineTermChar
      write FInLineTermChar default #10;

    property InLineTerminator : TStLineTerminator
      read FInLineTerminator
      write FInLineTerminator default ltCRLF;

    property Keywords : TStringList
      read FKeywords
      write SetKeywords;

    property OnProgress : TStOnProgressEvent
      read FOnProgress
      write FOnProgress;

    property OutFileName : String
      read FOutFileName
      write FOutFileName;

    property PageFooter : TStringList
      read FPageFooter
      write SetPageFooter;

    property PageHeader : TStringList
      read FPageHeader
      write SetPageHeader;

    property StringMarkers : TStringList
      read FStringMarkers
      write SetStringMarkers;

    property WordDelimiters : String
      read FWordDelims
      write FWordDelims;
  end;

implementation


uses
  StConst,
  StDict;


(*****************************************************************************)
(*                         TStStreamToHTML Implementation                    *)
(*****************************************************************************)

constructor TStStreamToHTML.Create;
begin
  inherited Create;

  FCommentMarkers := TStringList.Create;
  FEmbeddedHTML   := TStringList.Create;
  FKeywords       := TStringList.Create;
  FPageFooter     := TStringList.Create;
  FPageHeader     := TStringList.Create;
  FStringMarkers  := TStringList.Create;

  FInputStream := nil;
  FOutputStream := nil;

  FInFileSize := 0;
  FWordDelims := ',; .()';

  FInLineTerminator := ltCRLF;  {normal Windows text file terminator}
  FInLineTermChar   := #10;
  FInFixedLineLen   := 80;

  with FEmbeddedHTML do begin
    Add('"=&quot;');
    Add('&=&amp;');
    Add('<=&lt;');
    Add('>=&gt;');
    Add('¡=&iexcl;');
    Add('¢=&cent;');
    Add('£=&pound;');
    Add('©=&copy;');
    Add('®=&reg;');
    Add('±=&plusmn;');
    Add('¼=&frac14;');
    Add('½=&frac12;');
    Add('¾=&frac34;');
    Add('÷=&divide;');
  end;
end;


destructor TStStreamToHTML.Destroy;
begin
  FCommentMarkers.Free;
  FCommentMarkers := nil;

  FEmbeddedHTML.Free;
  FEmbeddedHTML := nil;

  FKeywords.Free;
  FKeywords := nil;

  FPageFooter.Free;
  FPageFooter := nil;

  FPageHeader.Free;
  FPageHeader := nil;

  FStringMarkers.Free;
  FStringMarkers := nil;

  FInTextStream.Free;
  FInTextStream := nil;

  FOutTextStream.Free;
  FOutTextStream := nil;

  inherited Destroy;
end;


procedure TStStreamToHTML.GenerateHTML;
begin
  if not ((Assigned(FInputStream) and (Assigned(FOutputStream)))) then
    RaiseStError(EStToHTMLError, stscBadStream)
  else
    ParseBuffer;
end;


procedure DisposeString(Data : Pointer); far;
begin
  Dispose(PString(Data));
end;


function TStStreamToHTML.ParseBuffer : Boolean;
var
  I, J,
  P1,
  P2,
  BRead,
  PC          : Integer;
  CloseStr,
  SStr,
  EStr,
  S,
  VS,
  AStr,
  TmpStr       : String;
  P            : Pointer;
  PS           : PString;
  CommentDict  : TStDictionary;
  HTMLDict     : TStDictionary;
  KeywordsDict : TStDictionary;
  StringDict   : TStDictionary;
  CommentPend  : Boolean;

      function ConvertEmbeddedHTML(const Str2 : String) : String;
      var
        L,
        J  : Integer;
        PH : Pointer;
      begin
        Result := '';
        {avoid memory reallocations}
        SetLength(Result, 1024);
        J := 1;
        for L := 1 to Length(Str2) do begin
          if (not HTMLDict.Exists(Str2[L], PH)) then begin
            Result[J] := Str2[L];
            Inc(J);
          end else begin
            Move(String(PH^)[1], Result[J], Length(String(PH^)) * SizeOf(Char));
            Inc(J, Length(String(PH^)));
          end;
        end;
        Dec(J);
        SetLength(Result, J);
      end;

      procedure CheckSubString(const Str1 : String);
      var
        S2 : String;
      begin
        if (KeywordsDict.Exists(Str1, P)) then begin
          VS := String(P^);
          S2 := Copy(VS, 1, pos(';', VS)-1)
              + ConvertEmbeddedHTML(Str1)
              + Copy(VS, pos(';', VS)+1, Length(VS));
          if (P1 >= Length(Str1)) and (P1 <= Length(TmpStr)) then
            S2 := S2 + ConvertEmbeddedHTML(TmpStr[P1]);
        end else begin
          S2 := ConvertEmbeddedHTML(Str1);
          if (P1 >= Length(Str1)) and (P1 <= Length(TmpStr)) then
            S2 := S2 + ConvertEmbeddedHTML(TmpStr[P1]);
        end;
        S := S + S2;
      end;

begin
  if (Length(FWordDelims) = 0) then
    RaiseStError(EStToHTMLError, stscWordDelimiters);

  {create Dictionaries for lookups}
  CommentDict  := TStDictionary.Create(FCommentMarkers.Count+1);
  KeywordsDict := TStDictionary.Create(FKeywords.Count+1);
  HTMLDict     := TStDictionary.Create(FEmbeddedHTML.Count+1);
  StringDict   := TStDictionary.Create(FStringMarkers.Count+1);

  CommentDict.DisposeData  := DisposeString;
  KeywordsDict.DisposeData := DisposeString;
  HTMLDict.DisposeData     := DisposeString;
  StringDict.DisposeData   := DisposeString;

  FInTextStream := TStAnsiTextStream.Create(FInputStream);
  FInTextStream.LineTermChar := AnsiChar(FInLineTermChar);
  FInTextStream.LineTerminator := FInLineTerminator;
  FInTextStream.FixedLineLength := FInFixedLineLen;
  FInFileSize := FInTextStream.Size;

  FOutTextStream := TStAnsiTextStream.Create(FOutputStream);
  FOutTextStream.LineTermChar := #10;
  FOutTextStream.LineTerminator := ltCRLF;
  FOutTextStream.FixedLineLength := 80;

  FInLineTerminator := ltCRLF;  {normal Windows text file terminator}
  FInLineTermChar   := #10;
  FInFixedLineLen   := 80;

  try
    if (FCaseSensitive) then begin
      CommentDict.Hash  := AnsiHashStr;
      CommentDict.Equal := AnsiCompareStr;
      HTMLDict.Hash     := AnsiHashStr;
      HTMLDict.Equal    := AnsiCompareStr;
      KeywordsDict.Hash := AnsiHashStr;
      KeywordsDict.Equal:= AnsiCompareStr;
      StringDict.Hash   := AnsiHashStr;
      StringDict.Equal  := AnsiCompareStr;
    end else begin
      CommentDict.Hash  := AnsiHashText;
      CommentDict.Equal := AnsiCompareText;
      HTMLDict.Hash     := AnsiHashText;
      HTMLDict.Equal    := AnsiCompareText;
      KeywordsDict.Hash := AnsiHashText;
      KeywordsDict.Equal:= AnsiCompareText;
      StringDict.Hash   := AnsiHashText;
      StringDict.Equal  := AnsiCompareText;
    end;

    {Add items from string lists to dictionaries}
    for I := 0 to pred(FKeywords.Count) do begin
      if (Length(FKeywords[I]) = 0) then
        continue;
      if (pos('=', FKeywords[I]) > 0) then begin
        New(PS);
        S := FKeywords.Names[I];
        PS^ := FKeywords.Values[S];
        if (not KeywordsDict.Exists(S, P)) then
          KeywordsDict.Add(S, PS)
        else
          Dispose(PS);
      end else
        RaiseStError(EStToHTMLError, stscInvalidSLEntry);
    end;

    for I := 0 to pred(FStringMarkers.Count) do begin
      if (Length(FStringMarkers[I]) = 0) then
        continue;
      if (pos('=', FStringMarkers[I]) > 0) then begin
        New(PS);
        S := FStringMarkers.Names[I];
        PS^ := FStringMarkers.Values[S];
        if (not StringDict.Exists(S, P)) then
          StringDict.Add(S, PS)
        else
          Dispose(PS);
      end else
        RaiseStError(EStToHTMLError, stscInvalidSLEntry);
    end;

    for I := 0 to pred(FCommentMarkers.Count) do begin
      if (Length(FCommentMarkers[I]) = 0) then
        continue;
      if (pos('=', FCommentMarkers[I]) > 0) then begin
        New(PS);
        S := FCommentMarkers.Names[I];
        if (Length(S) = 1) then
          PS^ := FCommentMarkers.Values[S]
        else begin
          PS^ := ':1' + S[2] + ';' + FCommentMarkers.Values[S];
          S := S[1];
        end;
        if (not CommentDict.Exists(S, P)) then
          CommentDict.Add(S, PS)
        else begin
          AStr := String(P^);
          AStr := AStr + PS^;
          String(P^) := AStr;
          CommentDict.Update(S, P);
          Dispose(PS);
        end;
      end else
        RaiseStError(EStToHTMLError, stscInvalidSLEntry);
    end;

    for I := 0 to pred(FEmbeddedHTML.Count) do begin
      if (pos('=', FEmbeddedHTML[I]) > 0) then begin
        New(PS);
        S := FEmbeddedHTML.Names[I];
        PS^ := FEmbeddedHTML.Values[S];
        if (not HTMLDict.Exists(S, P)) then
          HTMLDict.Add(S, PS)
        else
          Dispose(PS);
      end else
        RaiseStError(EStToHTMLError, stscInvalidSLEntry);
    end;

    BRead := 0;
    if (FPageHeader.Count > 0) then begin
      for I := 0 to pred(FPageHeader.Count) do
        FOutTextStream.WriteLine(FPageHeader[I]);
    end;
    FOutTextStream.WriteLine('<pre>');
    CommentPend := False;
    AStr := '';
    SStr := '';
    EStr := '';

    {make sure buffer is at the start}
    FInTextStream.Position := 0;
    while not FInTextStream.AtEndOfStream do begin
      TmpStr := string(FInTextStream.ReadLine);
      Inc(BRead, Length(TmpStr) + Length(FInTextStream.LineTermChar));
      if (FInFileSize > 0) then begin
        PC := Round((BRead / FInFileSize * 100));
        if (Assigned(FOnProgress)) then
          FOnProgress(Self, PC);
      end;

      if (TmpStr = '') then begin
        if (CommentPend) then
          FOutTextStream.WriteLine(EStr)
        else
          FOutTextStream.WriteLine(' ');
        continue;
      end;

      if (CommentPend) then
        S := SStr
      else
        S := '';

      P1 := 1;
      repeat
        if (not CommentPend) and (CommentDict.Exists(TmpStr[P1], P)) then begin
          VS := String(P^);
          if (Copy(VS, 1 , 2) = ':1') then begin
            while (Copy(VS, 1 , 2) = ':1') do begin
              System.Delete(VS, 1, 2);
              if (TmpStr[P1+1] = VS[1]) then begin
                System.Delete(VS, 1, 2);
                CloseStr := Copy(VS, 1, pos(';', VS)-1);
                System.Delete(VS, 1, pos(';', VS));
                SStr := Copy(VS, 1, pos(';', VS)-1);
                System.Delete(VS, 1, pos(';', VS));
                J := pos(':1', VS);
                if (J = 0) then
                  EStr := Copy(VS, pos(';', VS)+1, Length(VS))
                else begin
                  EStr := Copy(VS, 1, J-1);
                  System.Delete(VS, 1, J+2);
                end;

                if (CloseStr = '') then begin
                  S := S + SStr;
                  AStr := Copy(TmpStr, P1, Length(TmpStr));
                  CheckSubString(AStr);
                  S := S + EStr;
                  CloseStr := '';
                  SStr := '';
                  EStr := '';
                  TmpStr := '';
                  continue;
                end else begin
                  I := pos(CloseStr, TmpStr);
                  if (I = 0) then begin
                    CommentPend := True;
                    S := SStr + S;
                  end else begin
                    S := S + SStr;
                    AStr := Copy(TmpStr, P1, I-P1+Length(CloseStr));
                    CheckSubstring(AStr);
                    S := S + EStr;
                    System.Delete(TmpStr, P1, I-P1+Length(CloseStr));
                  end;
                end;
              end else begin
                J := pos(':1', VS);
                if (J > 0) then
                  System.Delete(VS, 1, J-1);
              end;
            end;
          end else begin
            {is it really the beginning of a comment?}
            CloseStr := Copy(VS, 1, pos(';', VS)-1);
            System.Delete(VS, 1, pos(';', VS));
            SStr := Copy(VS, 1, pos(';', VS)-1);
            EStr := Copy(VS, pos(';', VS)+1, Length(VS));
            I := pos(CloseStr, TmpStr);
            if (I > 0) and (I > P1) then begin
              {ending marker found}
              CommentPend := False;
              S := S + SStr;
              AStr := Copy(TmpStr, P1, I-P1+Length(CloseStr));
              CheckSubstring(AStr);
              S := S + EStr;
              System.Delete(TmpStr, P1, I-P1+Length(CloseStr));
              P1 := 1;
              CloseStr := '';
              SStr := '';
              EStr := '';
              if (TmpStr = '') then
                continue;
            end else begin  {1}
              CommentPend := True;
              S := S + SStr;
              if (Length(TmpStr) > 1) then begin
                AStr := Copy(TmpStr, P1, Length(TmpStr));
                CheckSubstring(AStr);
              end else
                S := S + TmpStr;
              S := S + EStr;
              TmpStr := '';
              continue;
            end;
          end;
        end;

        if (CommentPend) then begin
          I := pos(CloseStr, TmpStr);
          if (I < 1) then begin
            AStr := Copy(TmpStr, P1, Length(TmpStr));
            CheckSubstring(AStr);
            S := S + EStr;
            TmpStr := '';
            continue;
          end else begin {2}
            CommentPend := False;
            if (Length(TmpStr) > 1) then begin
              AStr := Copy(TmpStr, P1, I-P1+Length(CloseStr));
              CheckSubstring(AStr);
            end else
              S := S + TmpStr;
            S := S + EStr;
            System.Delete(TmpStr, P1, I-P1+Length(CloseStr));
            CloseStr := '';
            SStr := '';
            EStr := '';
            if (TmpStr = '') then
              continue
            else
              P1 := 1;
          end;
        end else begin
          CloseStr := '';
          SStr := '';
          EStr := '';
        end;

        if (TmpStr = '') then
          continue;

        P := nil;
        while (P1 <= Length(TmpStr)) and (pos(TmpStr[P1], FWordDelims) = 0) and
              (not StringDict.Exists(TmpStr[P1], P)) do
          Inc(P1);
        if (Assigned(P)) then begin
          P2 := P1+1;
          VS := String(P^);
          CloseStr := Copy(VS, 1, pos(';', VS)-1);
          System.Delete(VS, 1, pos(';', VS));
          SStr := Copy(VS, 1, pos(';', VS)-1);
          System.Delete(VS, 1, pos(';', VS));
          EStr := Copy(VS, pos(';', VS)+1, Length(VS));

          while (TmpStr[P2] <> CloseStr) and (P2 <= Length(TmpStr)) do
            Inc(P2);
          S := S + SStr;
          AStr := Copy(TmpStr, P1, P2-P1+1);
          CheckSubString(AStr);
          S := S + EStr;

          System.Delete(TmpStr, P1, P2);
          if (TmpStr = '') then
            continue
          else
            P1 := 1;
          P := nil;
        end else if (P1 <= Length(TmpStr)) and (pos(TmpStr[P1],  FWordDelims) > 0) then begin
          if (P1 = 1) then begin
            S := S + ConvertEmbeddedHTML(TmpStr[1]);
            System.Delete(TmpStr, 1, 1);
            P1 := 1;
          end else begin
            AStr := Copy(TmpStr, 1, P1-1);
            if (Length(AStr) > 0) then
              CheckSubstring(AStr);
            System.Delete(TmpStr, 1, P1);
            P1 := 1;
          end;
        end else begin
          AStr := TmpStr;
          CheckSubString(AStr);
          TmpStr := '';
        end;
      until (Length(TmpStr) = 0);
      FOutTextStream.WriteLine(S);
    end;
    if (Assigned(FOnProgress)) then
      FOnProgress(Self, 0);

    Result := True;
    FOutTextStream.WriteLine('</pre>');
    if (FPageFooter.Count > 0) then begin
      for I := 0 to pred(FPageFooter.Count) do
        FOutTextStream.WriteLine(FPageFooter[I]);
    end;
  finally
    CommentDict.Free;
    HTMLDict.Free;
    KeywordsDict.Free;
    StringDict.Free;

    FInTextStream.Free;
    FInTextStream := nil;

    FOutTextStream.Free;
    FOutTextStream := nil;
  end;
end;


procedure TStStreamToHTML.SetCommentMarkers(Value : TStringList);
begin
  FCommentMarkers.Assign(Value);
end;


procedure TStStreamToHTML.SetEmbeddedHTML(Value : TStringList);
begin
  FEmbeddedHTML.Assign(Value);
end;


procedure TStStreamToHTML.SetKeywords(Value : TStringList);
begin
  FKeywords.Assign(Value);
end;


procedure TStStreamToHTML.SetPageFooter(Value : TStringList);
begin
  FPageFooter.Assign(Value);
end;


procedure TStStreamToHTML.SetPageHeader(Value : TStringList);
begin
  FPageHeader.Assign(Value);
end;


procedure TStStreamToHTML.SetStringMarkers(Value : TStringList);
begin
  FStringMarkers.Assign(Value);
end;



(*****************************************************************************)
(*                         TStFileToHTML Implementation                      *)
(*****************************************************************************)


constructor TStFileToHTML.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FCommentMarkers := TStringList.Create;
  FEmbeddedHTML   := TStringList.Create;
  FKeywords       := TStringList.Create;
  FPageFooter     := TStringList.Create;
  FPageHeader     := TStringList.Create;
  FStringMarkers  := TStringList.Create;

  FWordDelims := ',; .()';

  FInLineTerminator := ltCRLF;
  FInLineTermChar   := #10;
  FInLineLength     := 80;

  with FEmbeddedHTML do begin
    Add('"=&quot;');
    Add('&=&amp;');
    Add('<=&lt;');
    Add('>=&gt;');
    Add('¡=&iexcl;');
    Add('¢=&cent;');
    Add('£=&pound;');
    Add('©=&copy;');
    Add('®=&reg;');
    Add('±=&plusmn;');
    Add('¼=&frac14;');
    Add('½=&frac12;');
    Add('¾=&frac34;');
    Add('÷=&divide;');
  end;
end;


destructor TStFileToHTML.Destroy;
begin
  FCommentMarkers.Free;
  FCommentMarkers := nil;

  FEmbeddedHTML.Free;
  FEmbeddedHTML := nil;

  FKeywords.Free;
  FKeywords := nil;

  FPageFooter.Free;
  FPageFooter := nil;

  FPageHeader.Free;
  FPageHeader := nil;

  FStringMarkers.Free;
  FStringMarkers := nil;

  FInFile.Free;
  FInFile := nil;

  FOutFile.Free;
  FOutFile := nil;

  FStream.Free;
  FStream := nil;

  inherited Destroy;
end;


procedure TStFileToHTML.Execute;
begin
  FStream := TStStreamToHTML.Create;
  try
    if (FInFileName = '') then
      RaiseStError(EStToHTMLError, stscNoInputFile)
    else if (FOutFileName = '') then
      RaiseStError(EStToHTMLError, stscNoOutputFile)
    else begin
      if (Assigned(FInFile)) then
        FInFile.Free;
      try
        FInFile := TFileStream.Create(FInFileName, fmOpenRead or fmShareDenyWrite);
      except
        RaiseStError(EStToHTMLError, stscInFileError);
        Exit;
      end;

      if (Assigned(FOutFile)) then
        FOutFile.Free;
      try
        FOutFile := TFileStream.Create(FOutFileName, fmCreate);
      except
        RaiseStError(EStToHTMLError, stscOutFileError);
        Exit;
      end;

      try
        FStream.InputStream       := FInFile;
        FStream.OutputStream      := FOutFile;
        FStream.CaseSensitive     := CaseSensitive;
        FStream.CommentMarkers    := CommentMarkers;
        FStream.EmbeddedHTML      := EmbeddedHTML;
        FStream.InFixedLineLength := InFixedLineLength;
        FStream.InLineTermChar    := InLineTermChar;
        FStream.InLineTerminator  := InLineTerminator;
        FStream.Keywords          := Keywords;
        FStream.OnProgress        := OnProgress;
        FStream.PageFooter        := PageFooter;
        FStream.PageHeader        := PageHeader;
        FStream.StringMarkers     := StringMarkers;
        FStream.WordDelimiters    := WordDelimiters;

        FStream.GenerateHTML;
      finally
        FInFile.Free;
        FInFile := nil;
        FOutFile.Free;
        FOutFile := nil;
      end;
    end;
  finally
    FStream.Free;
    FStream := nil;
  end;
end;


procedure TStFileToHTML.SetCommentMarkers(Value : TStringList);
begin
  FCommentMarkers.Assign(Value);
end;


procedure TStFileToHTML.SetEmbeddedHTML(Value : TStringList);
begin
  FEmbeddedHTML.Assign(Value);
end;



procedure TStFileToHTML.SetKeywords(Value : TStringList);
begin
  FKeywords.Assign(Value);
end;


procedure TStFileToHTML.SetPageFooter(Value : TStringList);
begin
  FPageFooter.Assign(Value);
end;


procedure TStFileToHTML.SetPageHeader(Value : TStringList);
begin
  FPageHeader.Assign(Value);
end;


procedure TStFileToHTML.SetStringMarkers(Value : TStringList);
begin
  FStringMarkers.Assign(Value);
end;


end.
