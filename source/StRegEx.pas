// TODO-UNICODE

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
{* SysTools: StRegEx.pas 4.04                            *}
{*********************************************************}
{* SysTools: SysTools Regular Expression Engine          *}
{*********************************************************}

{$I StDefine.inc}

unit StRegEx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  StConst,
  StBase,
  StStrms;

const
  StWordDelimString : string[31] = #9#32'!"&()*+,-./:;<=>?@[\]^`{|}~';
  StHexDigitString  : string[19] = '0123456789ABCDEF';

type
  TMatchPosition = packed record
    StartPos : Cardinal;
    EndPos   : Cardinal;
    Length   : Cardinal;
    LineNum  : Cardinal;
  end;

  TStOutputOption = (ooUnselected, ooModified, ooCountOnly);
  TStOutputOptions = set of TStOutputOption;

  TStTokens = (tknNil, tknLitChar, tknCharClass, tknNegCharClass,
               tknClosure, tknMaybeOne, tknAnyChar, tknBegOfLine,
               tknEndOfLine, tknGroup, tknBegTag, tknEndTag, tknDitto);

  PStPatRecord = ^TStPatRecord;
  TStPatRecord = packed record
    StrPtr        : ^ShortString;
    NestedPattern : PStPatRecord;
    NextPattern   : PStPatRecord;
    Token         : TStTokens;
    OneChar       : AnsiChar;
    NextOK        : Boolean;
  end;

  TStTagLevel = -1..9;
  TStFlag     = array[0..1023] of TStTagLevel;

  TStOnRegExProgEvent = procedure(Sender : TObject; Percent : Word) of object;
  TStOnMatchEvent = procedure(Sender     : TObject;
                              REPosition : TMatchPosition) of object;


  TStNodeHeap = class
  private
    FFreeList : PStPatRecord;

  protected
    procedure nhClearHeap;
    function nhDeepCloneNode(aNode : PStPatRecord) : PStPatRecord;

  public
    constructor Create;
    destructor Destroy; override;

    function AllocNode : PStPatRecord;
    procedure FreeNode(aNode : PStPatRecord);

    function CloneNode(aNode : PStPatRecord) : PStPatRecord;
  end;


  TStStreamRegEx = class(TObject)
  protected {private}
    { Private declarations }
    FAvoid            : Boolean;
    FIgnoreCase       : Boolean;
    FInTextStream     : TStAnsiTextStream;
    FInFileSize       : Cardinal;
    FInputStream      : TStream;

    FInLineBuf        : PAnsiChar;
    FInLineCount      : Cardinal;
    FInLineNum        : Cardinal;
    FInLineTermChar   : AnsiChar;
    FInLineTerminator : TStLineTerminator;
    FInLineLength     : integer;
    FLineNumbers      : Boolean;
    FLinesPerSec      : Cardinal;

    FMatchCount       : Cardinal;

    FMatchPatSL       : TStringList;
    FMatchPatStr      : PAnsiChar;
    FMatchPatPtr      : PStPatRecord;

    FMaxLineLength    : Cardinal;

    FNodes            : TStNodeHeap;

    FOnMatch          : TStOnMatchEvent;
    FOutLineLength    : integer;
    FOutLineTermChar  : AnsiChar;
    FOutLineTerminator: TStLineTerminator;

    FReplaceCount     : Cardinal;
    FReplacePatSL     : TStringList;
    FReplacePatStr    : PAnsiChar;
    FReplacePatPtr    : PStPatRecord;

    FOnProgress       : TStOnRegExProgEvent;
    FOutputStream     : TStream;
    FOutTextStream    : TStAnsiTextStream;
    FOutLineBuf       : PAnsiChar;

    FOutputOptions    : TStOutputOptions;

    FSelAvoidPatSL    : TStringList;
    FSelAvoidPatStr   : PAnsiChar;
    FSelAvoidPatPtr   : PStPatRecord;

    FSelectCount      : Cardinal;

  protected
    { Protected declarations }

    procedure AddTokenToPattern(var PatRec : PStPatRecord;
                                LastPatRec : PStPatRecord;
                                     Token : TStTokens;
                                         S : ShortString);
    procedure AddTokenToReplace(var PatRec : PStPatRecord;
                                LastPatRec : PStPatRecord;
                                     Token : TStTokens;
                                const S    : ShortString);             {!!.02}
    function  AppendS(Dest, S1, S2 : PAnsiChar; Count : Cardinal) : PAnsiChar;
    function  BuildAllPatterns : boolean;
    function  BuildPatternStr(var PStr  : PAnsiChar;
                              var Len   : Integer;
                                  SL    : TStringList) : Boolean;
    function  ConvertMaskToRegEx(const S : AnsiString) : AnsiString;
    procedure DisposeItems(var Data : PStPatRecord);

    procedure InsertLineNumber(Dest : PAnsiChar;
                               const S : PAnsiChar; LineNum : Integer);
    function  GetPattern(var Pattern : PAnsiChar;
                         var PatList : PStPatRecord) : Boolean;
    function  GetReplace(Pattern     : PAnsiChar;
                         var PatList : PStPatRecord) : Boolean;
    function  MakePattern(var Pattern : PAnsiChar;
                              Start   : Integer;
                              Delim   : AnsiChar;
                          var TagOn   : Boolean;
                          var PatList : PStPatRecord) : Integer;
    function  MakeReplacePattern(Pattern     : PAnsiChar;
                                 Start       : Integer;
                                 Delim       : AnsiChar;
                                 var PatList : PStPatRecord) : Integer;
    function  FindMatch(var Buf        : PAnsiChar;
                            PatPtr     : PStPatRecord;
                        var REPosition : TMatchPosition) : Boolean;
    function  MatchOnePatternElement(var Buf    : PAnsiChar;
                                     var I      : Integer;
                                     var TagOn  : Boolean;
                                     var TagNum : Integer;
                                       PatPtr   : PStPatRecord) : Boolean;
    function  ProcessLine(Buf           : PAnsiChar;
                          Len           : integer;
                          LineNum       : integer;
                          CheckOnly     : Boolean;
                          var REPosition: TMatchPosition) : Boolean;
    function  SearchMatchPattern(var Buf    : PAnsiChar;
                                     OffSet : Integer;
                                 var TagOn  : Boolean;
                                 var TagNum : Integer;
                                     PatPtr : PStPatRecord) : Integer;
    procedure SetMatchPatSL(Value : TStringList);
    procedure SetOptions(Value : TStOutputOptions);
    procedure SetReplacePatSL(Value : TStringList);
    procedure SetSelAvoidPatSL(Value : TStringList);
    procedure SubLine(Buf : PAnsiChar);
    function  SubLineFindTag(Buf         : PAnsiChar;
                             I           : Integer;
                             IEnd        : Integer;
                             TagNum      : Integer;
                             var Flags   : TStFlag;
                             var IStart  : Integer;
                             var IStop   : Integer) : Boolean;
    function  SubLineMatchOne(Buf        : PAnsiChar;
                              var Flags  : TStFlag;
                              var TagOn  : Boolean;
                              var I      : Integer;
                              var TagNum : Integer;
                              PatPtr     : PStPatRecord) : Boolean;
    function  SubLineMatchPattern(Buf        : PAnsiChar;
                                  var Flags  : TStFlag;
                                  var TagOn  : Boolean;
                                  var TagNum : Integer;
                                  OffSet     : Integer;
                                  PatPtr     : PStPatRecord) : Integer;
    procedure SubLineWrite(Buf       : PAnsiChar;
                           S         : PAnsiChar;
                           RepRec    : PStPatRecord;
                           I,
                           IEnd      : Integer;
                           var Flags : TStFlag);

  public
    { Public declarations }

    property InputStream : TStream
      read FInputStream
      write FInputStream;

    property OutputStream : TStream
      read FOutputStream
      write FOutputStream;

    constructor Create;
    destructor Destroy; override;

    function CheckString(const S : AnsiString;
                         var REPosition : TMatchPosition) : Boolean;
    function FileMasksToRegEx(Masks : AnsiString) : Boolean;
    function Execute : Boolean;
    function ReplaceString(var S : AnsiString;
                           var REPosition : TMatchPosition) : Boolean;

    property Avoid : Boolean
      read FAvoid
      write FAvoid;

    property IgnoreCase : Boolean
      read FIgnoreCase
      write FIgnoreCase;

    property InFixedLineLength : integer
      read FInLineLength
      write FInLineLength;

    property InLineTermChar : AnsiChar
      read FInLineTermChar
      write FInLineTermChar;

    property InLineTerminator : TStLineTerminator
      read FInLineTerminator
      write FInLineTerminator;

    property LineCount : Cardinal
      read FInLineCount;

    property LineNumbers : Boolean
      read FLineNumbers
      write FLineNumbers;

    property LinesMatched : Cardinal
      read FMatchCount;

    property LinesPerSecond : Cardinal
      read FLinesPerSec;

    property LinesReplaced : Cardinal
      read FReplaceCount;

    property LinesSelected : Cardinal
      read FSelectCount;

    property MatchPattern : TStringList
      read FMatchPatSL
      write SetMatchPatSL;

    property MaxLineLength : Cardinal
      read FMaxLineLength
      write FMaxLineLength;

    property OnMatch : TStOnMatchEvent
      read FOnMatch
      write FOnMatch;

    property OnProgress : TStOnRegExProgEvent
      read FOnProgress
      write FOnProgress;

    property OutFixedLineLength : integer
      read FOutLineLength
      write FOutLineLength;

    property OutLineTermChar : AnsiChar
      read FOutLineTermChar
      write FOutLineTermChar;

    property OutLineTerminator : TStLineTerminator
      read FOutLineTerminator
      write FOutLineTerminator;

    property OutputOptions : TStOutputOptions
      read FOutputOptions
      write SetOptions;

    property ReplacePattern : TStringList
      read FReplacePatSL
      write SetReplacePatSL;

    property SelAvoidPattern : TStringList
      read FSelAvoidPatSL
      write SetSelAvoidPatSL;
  end;


  TStRegEx = class(TStComponent)
  protected {private}
    FAvoid            : Boolean;
    FIgnoreCase       : Boolean;
    FInFileSize       : Cardinal;
    FInFileStream     : TFileStream;
    FInLineCount      : Cardinal;

    FInLineTermChar   : AnsiChar;
    FInLineTerminator : TStLineTerminator;
    FInFixedLineLength: integer;
    FInputFile        : AnsiString;

    FLineNumbers      : Boolean;
    FLinesPerSec      : Cardinal;

    FMatchCount       : Cardinal;

    FMatchPatSL       : TStringList;
    FMatchPatStr      : PAnsiChar;
    FMatchPatPtr      : PStPatRecord;

    FMaxLineLength    : Cardinal;

    FNodes            : TStNodeHeap;

    FOnProgress       : TStOnRegExProgEvent;
    FOnMatch          : TStOnMatchEvent;

    FOutFileStream    : TFileStream;
    FOutTextStream    : TStAnsiTextStream;
    FOutLineBuf       : PAnsiChar;

    FOutFixedLineLength : integer;
    FOutLineTermChar  : AnsiChar;
    FOutLineTerminator: TStLineTerminator;

    FOutputFile       : AnsiString;
    FOutputOptions    : TStOutputOptions;

    FReplaceCount     : Cardinal;
    FReplacePatSL     : TStringList;
    FReplacePatStr    : PAnsiChar;
    FReplacePatPtr    : PStPatRecord;

    FSelAvoidPatSL    : TStringList;
    FSelAvoidPatStr   : PAnsiChar;
    FSelAvoidPatPtr   : PStPatRecord;

    FSelectCount      : Cardinal;

    FStream           : TStStreamRegEx;

  protected
    procedure SetMatchPatSL(Value : TStringList);
    procedure SetOptions(Value : TStOutputOptions);
    procedure SetReplacePatSL(Value : TStringList);
    procedure SetSelAvoidPatSL(Value : TStringList);
    procedure SetStreamProperties;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function CheckString(const S : AnsiString;
                         var REPosition : TMatchPosition) : Boolean;
    function FileMasksToRegEx(const Masks : AnsiString) : Boolean;     {!!.02}
    function Execute : Boolean;
    function ReplaceString(var S : AnsiString;
                           var REPosition : TMatchPosition) : Boolean;

    property LineCount : Cardinal
      read FInLineCount;

    property LinesMatched : Cardinal
      read FMatchCount;

    property LinesPerSecond : Cardinal
      read FLinesPerSec;

    property LinesReplaced : Cardinal
      read FReplaceCount;

    property LinesSelected : Cardinal
      read FSelectCount;

    property MaxLineLength : Cardinal
      read FMaxLineLength
      write FMaxLineLength;

  published
    property Avoid : Boolean
      read FAvoid
      write FAvoid default False;

    property IgnoreCase : Boolean
      read FIgnoreCase
      write FIgnoreCase default False;

    property InFixedLineLength : Integer
      read FInFixedLineLength
      write FInFixedLineLength default 80;

    property InLineTermChar : AnsiChar
      read FInLineTermChar
      write FInLineTermChar default #10;

    property InLineTerminator : TStLineTerminator
      read FInLineTerminator
      write FInLineTerminator default ltCRLF;

    property InputFile : AnsiString
      read FInputFile
      write FInputFile;

    property LineNumbers : Boolean
      read FLineNumbers
      write FLineNumbers default False;

    property MatchPattern : TStringList
      read FMatchPatSL
      write SetMatchPatSL;

    property OnMatch : TStOnMatchEvent
      read FOnMatch
      write FOnMatch;

    property OnProgress : TStOnRegExProgEvent
      read FOnProgress
      write FOnProgress;

    property OutFixedLineLength : Integer
      read FOutFixedLineLength
      write FOutFixedLineLength default 80;

    property OutLineTermChar : AnsiChar
      read FOutLineTermChar
      write FOutLineTermChar default #10;

    property OutLineTerminator : TStLineTerminator
      read FOutLineTerminator
      write FOutLineTerminator default ltCRLF;

    property OutputFile : AnsiString
      read FOutputFile
      write FOutputFile;

    property OutputOptions : TStOutputOptions
      read FOutputOptions
      write SetOptions;

    property ReplacePattern : TStringList
      read FReplacePatSL
      write SetReplacePatSL;

    property SelAvoidPattern : TStringList
      read FSelAvoidPatSL
      write SetSelAvoidPatSL;
  end;


implementation

uses
  AnsiStrings,
  StStrL,
  StStrS;


const
  Null           = #0;
  EndStr         = #0;
  NewLine        = #13#10;
  Dash           = '-';
  Esc            = '\';
  Any            = '.';  {was '?'}
  Closure        = '*';
  ClosurePlus    = '+';
  MaybeOne       = '?';  {was '!'}
  Bol            = '^';
  Eol            = '$';
  Ccl            = '[';
  Negate         = '^';
  CclEnd         = ']';
  BTag           = '{';
  ETag           = '}';
  BGroup         = '(';
  EGroup         = ')';
  Alter          = '|';  {was #}
  Ditto          = '&';
  lSpace         = 's';
  lNewline       = 'n';
  lTab           = 't';
  lBackSpace     = 'b';
  lReturn        = 'r';
  lFeed          = 'l';
  lHex           = 'h';
  lWordDelim     = 'w';
  lNil           = 'z';


function CleanUpCase(S : String) : String;
{-convert string to uppercase and remove duplicates}
var
  I  : Integer;
  K  : Integer;
  C  : Char;
begin
  Result := '';
  S := AnsiUpperCase(S);
  for I := 1 to Length(S) do begin
    C := S[I];
    if not StrChPosL(Result, C, K) then
      Result := Result + C;
  end;
end;


procedure AppendChar(C : AnsiChar; var S : ShortString);
 {-append a character C onto string S}
begin
  S := S + C;
end;


function IsAlphaNum(C : AnsiChar) : Boolean;
begin
  Result := IsCharAlphaNumericA(C); //Ansi!
end;


procedure ExpandDash(Delim       : AnsiChar;
                     var Pattern : PAnsiChar ;
                     var I       : Integer;
                     var S       : ShortString);
{-expand the innards of the character class, including dashes}
{stop when endc is found}
{return a string S with the expansion}
var
  C,
  CLeft,
  CNext  : AnsiChar;
  K      : Integer;

begin
  while (Pattern[I] <> Delim) and (Pattern[I] <> EndStr) do begin
    C := Pattern[I];
    if (C = Esc) then begin
      if (Pattern[Succ(I)] <> EndStr) then begin
        I := Succ(I);
        C := Pattern[I];
        case C of
          lSpace      : AppendChar(#32, S);
          lTab        : AppendChar(#9,  S);
          lBackSpace  : AppendChar(#8,  S);
          lReturn     : AppendChar(#13, S);
          lFeed       : AppendChar(#10, S);
        else
          AppendChar(C, S);
        end;
      end else
        {escape must be the character}
        AppendChar(Esc, S);
    end else if (C <> Dash) then
      {literal character}
      AppendChar(C, S)
    else if ((Length(S) = 0) or (Pattern[Succ(I)] = Delim)) then
      {literal dash at begin or end of class}
      AppendChar(Dash, S)
    else begin
      {dash in middle of class}
      CLeft := Pattern[Pred(I)];
      CNext := Pattern[Succ(I)];
      if IsAlphaNum(CLeft) and IsAlphaNum(CNext) and (CLeft <= CNext) then begin
        {legal dash to be expanded}
        for K := (Ord(CLeft)+1) to Ord(CNext) do
          AppendChar(AnsiChar(K), S);
        {move over the end of dash character}
        I := Succ(I);
      end else
        {dash must be a literal}
        AppendChar(Dash, S);
    end;
    I := Succ(I);
  end;
end;


function GetCharacterClass(var Pattern : PAnsiChar;
                           var I       : Integer;
                           var S       : ShortString;
                           var AToken  : TStTokens) : Boolean;
{-expand a character class starting at position I of Pattern into a string S}
{return a token type (tknCharClass or tknNegCharClass)}
{return I pointing at the end of class character}
{return true if successful}

begin
{skip over start of class character}
  I := Succ(I);
  if (Pattern[I] = Negate) then begin
    AToken := tknNegCharClass;
    I := Succ(I);
  end else
    AToken := tknCharClass;
  {expand the character class}
  S := '';
  ExpandDash(CclEnd, Pattern, I, S);
  Result := (Pattern[I] = CclEnd);
end;





{******************************************************************************}
{                           TStNodeHeap Implementation                         }
{******************************************************************************}

constructor TStNodeHeap.Create;
begin
  inherited Create;

  New(FFreeList);
  FillChar(FFreeList^, sizeof(TStPatRecord), 0);
end;


destructor TStNodeHeap.Destroy;
begin
  nhClearHeap;
  Dispose(FFreeList);

  inherited Destroy;
end;


function TStNodeHeap.AllocNode : PStPatRecord;
begin
  if (FFreeList^.NextPattern = nil) then
    New(Result)
  else begin
    Result := FFreeList^.NextPattern;
    FFreeList^.NextPattern := Result^.NextPattern;
  end;
  FillChar(Result^, sizeof(TStPatRecord), 0);
end;


function TStNodeHeap.CloneNode(aNode : PStPatRecord) : PStPatRecord;
begin
  {allocate a new node}
  Result := AllocNode;

  {copy fields}
  Result^.Token         := aNode^.Token;
  Result^.OneChar       := aNode^.OneChar;
  Result^.NextOK        := aNode^.NextOK;
  if (aNode^.StrPtr <> nil) then begin                                 
    New(Result^.StrPtr);                                               
    Result^.StrPtr^     := aNode^.StrPtr^;                             
  end else                                                             
    Result^.StrPtr      := nil;                                        

  {deep clone the nested node}
  if (aNode^.NestedPattern <> nil) then
    Result^.NestedPattern := nhDeepCloneNode(aNode^.NestedPattern);
end;


procedure TStNodeHeap.FreeNode(aNode : PStPatRecord);
begin
  if (aNode <> nil) then begin
    aNode^.NextPattern := FFreeList^.NextPattern;
    FFreeList^.NextPattern := aNode;
  end;
end;


procedure TStNodeHeap.nhClearHeap;
var
  Walker,
  Temp    : PStPatRecord;
begin
  Walker := FFreeList^.NextPattern;
  FFreeList^.NextPattern := nil;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.NextPattern;
    Dispose(Temp);
  end;
end;


function TStNodeHeap.nhDeepCloneNode(aNode : PStPatRecord) : PStPatRecord;
begin
  {allocate a new node}
  Result := AllocNode;

  {copy fields}
  Result^.Token         := aNode^.Token;
  Result^.OneChar       := aNode^.OneChar;
  Result^.NextOK        := aNode^.NextOK;
  if (aNode^.StrPtr <> nil) then begin                                 
    New(Result^.StrPtr);                                               
    Result^.StrPtr^     := aNode^.StrPtr^;                             
  end else                                                             
    Result^.StrPtr      := nil;

  {recursively deepclone the next and nested nodes}
  if (aNode^.NextPattern <> nil) then
    Result^.NextPattern := nhDeepCloneNode(aNode^.NextPattern);
  if (aNode^.NestedPattern <> nil) then
    Result^.NestedPattern := nhDeepCloneNode(aNode^.NestedPattern);
end;


{******************************************************************************}
{                           TStStreamRegEx Implementation                      }
{******************************************************************************}


constructor TStStreamRegEx.Create;
begin
  inherited Create;

  FAvoid          := False;
  FIgnoreCase     := False;
  FLineNumbers    := False;
  FOutputOptions  := [];

  FInLineTerminator := ltCRLF;
  FInLineTermChar   := #10;
  FInLineLength     := 80;

  FOutLineTerminator := ltCRLF;
  FOutLineTermChar   := #10;
  FOutLineLength     := 80;

  FMaxLineLength := 1024;

  FMatchPatSL    := TStringList.Create;
  FMatchPatPtr   := nil;
  FSelAvoidPatSL := TStringList.Create;
  FSelAvoidPatPtr:= nil;
  FReplacePatSL  := TStringList.Create;
  FReplacePatPtr := nil;

  FInputStream      := nil;
  FInTextStream     := nil;
  FOutputStream     := nil;
  FOutTextStream    := nil;

  FNodes    := TStNodeHeap.Create;
end;


procedure TStStreamRegEx.DisposeItems(var Data : PStPatRecord);
var
  Walker, Temp : PStPatRecord;
begin
  if (Data <> nil) then begin
    Walker := Data;
    while (Walker <> nil) do begin
      Temp := Walker;

      if (Assigned(Walker^.StrPtr)) then
        Dispose(Walker^.StrPtr);

      if (Assigned(Walker^.NestedPattern)) then
        DisposeItems(Walker^.NestedPattern);

      Walker := Walker^.NextPattern;
      FNodes.FreeNode(Temp);
    end;
    Data := nil;
  end;
end;


destructor TStStreamRegEx.Destroy;
begin
  DisposeItems(FMatchPatPtr);
  DisposeItems(FSelAvoidPatPtr);
  DisposeItems(FReplacePatPtr);

  FNodes.Free;
  FNodes := nil;

  if (Assigned(FMatchPatStr)) then begin
    FreeMem(FMatchPatStr, AnsiStrings.StrLen(FMatchPatStr) + 1);
    FMatchPatStr := nil;
  end;

  if (Assigned(FReplacePatStr)) then
    FreeMem(FReplacePatStr, AnsiStrings.StrLen(FReplacePatStr) + 1);
  FReplacePatStr := nil;

  if (Assigned(FSelAvoidPatStr)) then
    FreeMem(FSelAvoidPatStr, AnsiStrings.StrLen(FSelAvoidPatStr) + 1);
  FSelAvoidPatStr := nil;

  FMatchPatSL.Free;
  FMatchPatSL := nil;

  FReplacePatSL.Free;
  FReplacePatSL := nil;

  FSelAvoidPatSL.Free;
  FSelAvoidPatSL := nil;

  inherited Destroy;
end;


function TStStreamRegEx.AppendS(Dest, S1, S2 : PAnsiChar;
                                Count : Cardinal) : PAnsiChar;
var
  Remaining : Cardinal;
  I         : Cardinal;
begin
  Result := Dest;
  I := AnsiStrings.StrLen(S1);
  Remaining := MaxLineLength - I;
  if (Remaining < AnsiStrings.StrLen(S2)) then
    Count := Remaining;
  Move(S1[0], Dest[0], I);
  Move(S2[0], Dest[I], Count);
  I := I + Count;
  Dest[I] := #0;
end;


function TStStreamRegEx.BuildAllPatterns : Boolean;
var
  Len : Integer;
begin
  if (FMatchPatSL.Count > 0) then begin
    DisposeItems(FMatchPatPtr);

    if (BuildPatternStr(FMatchPatStr, Len, FMatchPatSL)) then begin
      if (Len > 0) then
        GetPattern(FMatchPatStr, FMatchPatPtr)
      else
        DisposeItems(FMatchPatPtr);
      Result := True;
    end else begin
      DisposeItems(FMatchPatPtr);
      Result := False;
    end;
  end else begin
    DisposeItems(FMatchPatPtr);
    Result := True;
  end;

  if Result then begin
    if (FSelAvoidPatSL.Count > 0) then begin
      DisposeItems(FSelAvoidPatPtr);
      if (BuildPatternStr(FSelAvoidPatStr, Len, FSelAvoidPatSL)) then begin
        if (Len > 0) then
          GetPattern(FSelAvoidPatStr, FSelAvoidPatPtr)
        else
          DisposeItems(FSelAvoidPatPtr);
        Result := True;
      end else begin
        DisposeItems(FSelAvoidPatPtr);
        Result := False;
      end;
    end else begin
      DisposeItems(FSelAvoidPatPtr);
      Result := True;
    end;
  end;

  if Result then begin
    if (FReplacePatSL.Count > 0) then begin
      DisposeItems(FReplacePatPtr);
      if (BuildPatternStr(FReplacePatStr, Len, FReplacePatSL)) then begin
        if (Len > 0) then
          GetReplace(FReplacePatStr, FReplacePatPtr)
        else
          DisposeItems(FReplacePatPtr);
        Result := True;
      end else begin
        DisposeItems(FReplacePatPtr);
        Result := False;
      end;
    end else begin
      DisposeItems(FReplacePatPtr);
      Result := True;
    end;
  end;
end;



function TStStreamRegEx.BuildPatternStr(var PStr  : PAnsiChar;
                                  var Len   : Integer;
                                      SL    : TStringList) : Boolean;
var
  I,
  J   : integer;
  CurLen : Integer;                                                  {!!.01}
begin
  Len := 0;
  for I := 0 to pred(SL.Count) do
    Len := Len + Length(TrimL(SL[I]));
  if (Len = 0) then
    Result := True
  else begin
    if Assigned(PStr) then
      FreeMem(PStr, AnsiStrings.StrLen(PStr)+1);
    GetMem(PStr, Len+1);
    PStr[Len] := EndStr;
    J := 0;
    for I := 0 to pred(SL.Count) do begin
      CurLen := Length(TrimL(SL[I]));                                {!!.01}
      if CurLen > 0 then begin                                       {!!.01}
        Move(SL[I][1], PStr[J], CurLen);                             {!!.01}
        Inc(J, CurLen);                                              {!!.01}
      end;                                                           {!!.01}
    end;
    Result := True;
  end;
end;


function TStStreamRegEx.CheckString(const S : AnsiString;
                                    var REPosition : TMatchPosition) : Boolean;
var
  Tmp : PAnsiChar;
  I   : integer;
  Len : integer;
  OK  : Boolean;
begin
  I := Length(S);
  GetMem(Tmp, I+3);
  try
    if I > 0 then                                                     {!!.01}
      Move(S[1], Tmp[0], I);

    Tmp[I]   := #13;
    Tmp[I+1] := #10;
    Tmp[I+2] := EndStr;

    if (FMatchPatSL.Count > 0) then begin
      OK := BuildPatternStr(FMatchPatStr, Len, FMatchPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FMatchPatStr, FMatchPatPtr)
        else
          DisposeItems(FMatchPatPtr);
      end else
        DisposeItems(FMatchPatPtr);
    end else
      DisposeItems(FMatchPatPtr);

    if (FSelAvoidPatSL.Count > 0) then begin
      OK := BuildPatternStr(FSelAvoidPatStr, Len, FSelAvoidPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FSelAvoidPatStr, FSelAvoidPatPtr)
        else
          DisposeItems(FSelAvoidPatPtr);
      end;
    end else
      DisposeItems(FSelAvoidPatPtr);

    FMatchCount    := 0;
    FSelectCount   := 0;
    FReplaceCount  := 0;
    FInLineCount   := 0;
    FLinesPerSec   := 0;

    REPosition.LineNum := 1;
    if ((FSelAvoidPatPtr <> nil) or (FMatchPatPtr <> nil)) then
      Result := ProcessLine(Tmp, I, 1, True, REPosition)
    else begin
      Result := False;
      RaiseStError(EStRegExError, stscNoPatterns);
    end;
  finally
    FreeMem(Tmp, I+3);
  end;
end;


function TStStreamRegEx.ReplaceString(var S : AnsiString;
                                      var REPosition : TMatchPosition) : Boolean;
var
  Tmp : PAnsiChar;
  I   : integer;
  Len : integer;
  OK  : Boolean;

      function ProcessString(var S          : AnsiString;
                                 Len        : integer;
                                 LineNum    : integer;
                             var REPosition : TMatchPosition) : Boolean;
      var
        TmpBuf : PAnsiChar;
        ABuf   : PAnsiChar;
        L      : Integer;
      begin
        L := Length(S)+1;
        GetMem(TmpBuf, MaxLineLength+1);
        GetMem(ABuf, L);
        try
          AnsiStrings.StrPCopy(ABuf, S);
          if (FSelAvoidPatPtr <> nil) then begin
            Result := False;
            if (not Avoid) then
              Result := FindMatch(ABuf, FSelAvoidPatPtr, REPosition)
            else
              Result := not(FindMatch(ABuf, FSelAvoidPatPtr, REPosition));
          end else
            Result := True;

          if Result then begin
            {met select criterion, perhaps by default}
            FSelectCount := Succ(FSelectCount);
            if (FReplacePatPtr <> nil) then begin
              Result := FindMatch(ABuf, FMatchPatPtr, REPosition);
              if Result then begin
                TmpBuf[0] := #0;
                SubLine(ABuf);
                S := AnsiStrings.StrPas(FOutLineBuf);
              end;
            end;
          end;
        finally
          FreeMem(TmpBuf, MaxLineLength+1);
          FreeMem(ABuf, L);
        end;
      end;


begin
  I := Length(S);
  GetMem(Tmp, I+3);
  try
    if I > 0 then                                                  {!!.01}
      Move(S[1], Tmp[0], I);
    Tmp[I]   := #13;
    Tmp[I+1] := #10;
    Tmp[I+2] := EndStr;

    if (FMatchPatSL.Count > 0) then begin
      OK := BuildPatternStr(FMatchPatStr, Len, FMatchPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FMatchPatStr, FMatchPatPtr)
        else
          DisposeItems(FMatchPatPtr);
      end else
        DisposeItems(FMatchPatPtr);
    end else
      DisposeItems(FMatchPatPtr);

    if (FSelAvoidPatSL.Count > 0) then begin
      OK := BuildPatternStr(FSelAvoidPatStr, Len, FSelAvoidPatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FSelAvoidPatStr, FSelAvoidPatPtr)
        else
          DisposeItems(FSelAvoidPatPtr);
      end;
    end else
      DisposeItems(FSelAvoidPatPtr);

    if (FReplacePatSL.Count > 0) then begin
      OK := BuildPatternStr(FReplacePatStr, Len, FReplacePatSL);
      if (OK) then begin
        if (Len > 0) then
          GetPattern(FReplacePatStr, FReplacePatPtr)
        else
          DisposeItems(FReplacePatPtr);
      end else
        DisposeItems(FReplacePatPtr);
    end else
      DisposeItems(FReplacePatPtr);

    FMatchCount    := 0;
    FSelectCount   := 0;
    FReplaceCount  := 0;
    FInLineCount   := 0;
    FLinesPerSec   := 0;

    GetMem(FInLineBuf, MaxLineLength+3);
    GetMem(FOutLineBuf, MaxLineLength+3);
    try
      REPosition.LineNum := 1;
      if ((FSelAvoidPatPtr <> nil) or (FMatchPatPtr <> nil)) and
          (Assigned(FReplacePatPtr))then begin
        Result := ProcessString(S, I, 1, REPosition);
      end else begin
        Result := False;
        RaiseStError(EStRegExError, stscNoPatterns);
      end;
    finally
      FreeMem(FInLineBuf, MaxLineLength+3);
      FreeMem(FOutLineBuf, MaxLineLength+3);
    end;
  finally
    FreeMem(Tmp, I+3);
  end;
end;


function TStStreamRegEx.ConvertMaskToRegEx(const S : AnsiString) : AnsiString;
var
  I      : integer;
  TS     : AnsiString;
begin
  I := 1;
  while (I <= Length(S)) do begin
    if (I = 1) then begin
      if not (S[1] in ['*', '?']) then begin
        TS := '((^[' ;
        TS := TS + S[1] + '])';
        Inc(I);
      end else
        TS := '(';
    end;

    if not (S[I] in ['*', '?', '.', '\']) then
      TS := TS + S[I]
    else begin
      if (S[I] = '*') then
        TS := TS + '.*'
      else if (S[I] = '?') then begin
        if (I = 1) then
          TS := TS + '(^.)'
        else
          TS := TS + '.?';
      end else begin
        TS := TS + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
  Result := TS + '\n)';
end;


function TStStreamRegEx.FileMasksToRegEx(Masks : AnsiString) : Boolean;
var
  SL : TStringList;
  S  : AnsiString;
  K  : Integer;
  Len: Integer;
begin
  SL := TStringList.Create;
  try
    if StrChPosL(Masks, ';', K) then begin
      while (K > 0) do begin
        S := Copy(Masks, 1, K-1);
        if (Length(S) > 0) then begin
          if (SL.Count = 0) then
            SL.Add(ConvertMaskToRegEx(S))
          else
            SL.Add('|' + ConvertMaskToRegEx(S));
        end;
        Delete(Masks, 1, K);
        if not (StrChPosL(Masks, ';', K)) then
          break;
      end;
      if (Length(Masks) > 0) then
        SL.Add('|' + ConvertMaskToRegEx(Masks));
    end else begin
      if (Length(Masks) > 0) then
        SL.Add(ConvertMaskToRegEx(Masks));
    end;

    if (SL.Count > 0) then begin
      FMatchPatSL.Clear;
      FMatchPatSL.Assign(SL);
      DisposeItems(FMatchPatPtr);
      FMatchPatPtr := nil;
      if (BuildPatternStr(FMatchPatStr, Len, FMatchPatSL)) then begin
        if (Len > 0) then
          GetPattern(FMatchPatStr, FMatchPatPtr)
        else begin
          DisposeItems(FMatchPatPtr);
          FMatchPatPtr := nil;
        end;
        Result := True;
      end else begin
        DisposeItems(FMatchPatPtr);
        FMatchPatPtr := nil;
        Result := False;
      end;
      Result := True;
    end else
      Result := False;
  finally
    SL.Free;
  end;
end;



function TStStreamRegEx.Execute : Boolean;
var
  Len       : TStMemSize;
  LineNum   : Integer;
  ATime     : TDateTime;
  PC        : Cardinal;
  LPC       : Cardinal;
  BytesRead : Cardinal;
  REPosition: TMatchPosition;
  Found     : Boolean;

  Src : PAnsiChar; {!!!}
  FFoundText : AnsiString; {!!!}
begin
  if (FMatchPatSL.Count = 0) and
     (FReplacePatSL.Count = 0) and (FSelAvoidPatSL.Count = 0) then
    RaiseStError(EStRegExError, stscNoPatterns);

  if (not (BuildAllPatterns)) then
    RaiseStError(EStRegExError, stscPatternError);

  if (FMatchPatPtr = nil) and (FSelAvoidPatPtr = nil) and (FReplacePatPtr = nil) then
    RaiseStError(EStRegExError, stscNoPatterns);

  if (not (Assigned(FInputStream))) or
     ((not (Assigned(FOutputStream)) and (not (ooCountOnly in OutputOptions)))) then
    RaiseStError(EStRegExError, stscStreamsNil);

  FInTextStream := nil;
  FOutTextStream := nil;
  try
    FInTextStream := TStAnsiTextStream.Create(FInputStream);
    FInTextStream.LineTermChar := FInLineTermChar;
    FInTextStream.LineTerminator := FInLineTerminator;
    FInTextStream.FixedLineLength := FInLineLength;
    FInFileSize := FInTextStream.Size;

    if not (ooCountOnly in OutputOptions) then begin
      FOutTextStream := TStAnsiTextStream.Create(FOutputStream);
      FOutTextStream.LineTermChar := FOutLineTermChar;
      FOutTextStream.LineTerminator := FOutLineTerminator;
      FOutTextStream.FixedLineLength := FInLineLength;
    end;

    FMatchCount    := 0;
    FSelectCount   := 0;
    FReplaceCount  := 0;
    FInLineCount   := 0;
    FLinesPerSec   := 0;
    BytesRead      := 0;
    LPC            := 0;

    FInTextStream.Position := 0;
    FInLineBuf := nil;
    FOutLineBuf := nil;
    try
      GetMem(FInLineBuf, MaxLineLength+3);
      GetMem(FOutLineBuf, MaxLineLength+3);

      LineNum := 1;
      ATime := Now;
      while not FInTextStream.AtEndOfStream do begin
        Len := FInTextStream.ReadLineArray(FInLineBuf, MaxLineLength);
        Inc(BytesRead, Len);

        FInLineBuf[Len]   := #13;
        FInLineBuf[Len+1] := #10;
        FInLineBuf[Len+2] := EndStr;
{!!.02 - added }
        REPosition.StartPos := 0;
        REPosition.EndPos   := 0;
        REPosition.Length   := 0;
{!!.02 - added end }
        REPosition.LineNum := LineNum;
        Found := ProcessLine(FInLineBuf, Len, LineNum, False, REPosition);

{!!!}
        SetLength(FFoundText, REPosition.Length);
        Src := FInLineBuf;
        Inc(Src, REPosition.StartPos);
        AnsiStrings.StrMove(PAnsiChar(FFoundText), Src, REPosition.Length);
{!!!}

        if (FInFileSize > 0) then begin
          PC := Round(BytesRead / FInFileSize * 100);
          {avoid calling with every line - when OnProgress is assigned}
          {performance is considerably reduced anyway, don't add to it}
          if (PC > LPC) then begin
            LPC := PC;
            if (Assigned(FOnProgress)) then
              FOnProgress(Self, PC);
          end;
        end;
        if (Assigned(FOnMatch)) and (Found) then
          FOnMatch(Self, REPosition);

        Inc(LineNum);
      end;
      ATime := (Now - ATime) * 86400;
      FInLineCount := LineNum-1;
      if (ATime > 0) then
        FLinesPerSec := Trunc(FInLineCount / ATime)
      else
        FLinesPerSec := 0;
      if (Assigned(FOnProgress)) then
        FOnProgress(Self, 100);
      Result := (FMatchCount > 0) or (FSelectCount > 0);
    finally
      FreeMem(FInLineBuf, MaxLineLength+3);
      FreeMem(FOutLineBuf, MaxLineLength+3);
    end;
  finally
    FInTextStream.Free;
    FInTextStream := nil;
    FOutTextStream.Free;
    FOutTextStream := nil;
  end;
end;


procedure TStStreamRegEx.AddTokenToPattern(var PatRec : PStPatRecord;
                                           LastPatRec : PStPatRecord;
                                                Token : TStTokens;
                                                    S : ShortString);
{-add a token record to the pattern list}
{-S contains a literal character or an expanded character class}


begin
  PatRec := FNodes.AllocNode;
  PatRec^.Token := Token;        {save token type}
  PatRec^.NextOK := False;       {default to non-alternation}

  LastPatRec^.NextPattern := PatRec;  {hook up the previous token}
  case Token of
    tknNil, tknAnyChar, tknBegOfLine, tknEndOfLine, tknGroup, tknBegTag, tknEndTag :
      begin
        PatRec^.OneChar := Null;
        PatRec^.StrPtr := nil;
      end;
    tknLitChar :
      begin
        if IgnoreCase then
          PatRec^.OneChar := AnsiChar(AnsiUpperCase(S[1])[1])
        else
          PatRec^.OneChar := S[1];
        PatRec^.StrPtr := nil;
      end;
    tknCharClass, tknNegCharClass :
      begin
        PatRec^.OneChar := Null;
        if FIgnoreCase then
          S := AnsiString(CleanUpCase(S));
        New(PatRec^.StrPtr);
        PatRec^.StrPtr^ := S;
      end;
  else
    RaiseStError(EStRegExError, stscUnknownError);
  end;
end;


function TStStreamRegEx.MakePattern(var Pattern : PAnsiChar;
                                        Start   : Integer;
                                        Delim   : AnsiChar;
                                    var TagOn   : Boolean;
                                    var PatList : PStPatRecord) : Integer;
var
  I              : Integer;
  NextLastPatRec,
  LastPatRec,
  TempPatRec,
  PatRec         : PStPatRecord;
  Done           : Boolean;
  AChar          : AnsiChar;
  TmpStr         : ShortString;
  AToken         : TStTokens;
  GroupStartPos,
  GroupEndPos    : integer;

begin
  PatList := FNodes.AllocNode;
  PatList^.Token  := tknNil;    {put a nil token at the beginning}
  PatList^.NextOK := False;
  LastPatRec := PatList;
  NextLastPatRec := nil;

  I := Start;                 {start point of pattern string}
  Done := False;
  while not(Done) and (Pattern[I] <> Delim) and (Pattern[I] <> EndStr) do begin
    AChar := Pattern[I];
    if (AChar = Any) then
      AddTokenToPattern(PatRec, LastPatRec, tknAnyChar, AChar)
    else if (AChar = Bol) then
      AddTokenToPattern(PatRec, LastPatRec, tknBegOfLine, '')
    else if (AChar = Eol) then
      AddTokenToPattern(PatRec, LastPatRec, tknEndOfLine, '')
    else if (AChar = Ccl) then begin
      Done := (GetCharacterClass(Pattern, I, TmpStr, AToken) = False);
      if Done then
        RaiseStError(EStRegExError, stscExpandingClass);
      AddTokenToPattern(PatRec, LastPatRec, AToken, TmpStr);
    end else if (AChar = Alter) then begin
      if (NextLastPatRec = nil) or
         ((NextLastPatRec^.Token <> tknClosure) and
          (NextLastPatRec^.Token <> tknMaybeOne)) then begin
        {flag the current token as non-critical, i.e., "next is OK"}
        LastPatRec^.NextOK := True;
      end else begin
        {alternation immediately after a closure is probably not desired}
        {e.g., [a-z]*|[0-9] would internally produce ([a-z]|[0-9])*}
        Done := True;
        RaiseStError(EStRegExError, stscAlternationFollowsClosure);
      end;
    end else if (AChar = BGroup) then begin
      GroupStartPos := I+1;
      AddTokenToPattern(PatRec, LastPatRec, tknGroup, '');
      {recursive branch off the list}
      I := MakePattern(Pattern, Succ(I), EGroup, TagOn, TempPatRec);
      if (I > 0) then begin
        GroupEndPos := I-1;
        if (Pattern[I+1] <> EndStr) then begin
          if (Pattern[I+1] in [Closure, ClosurePlus]) then begin
            if  ((((GroupEndPos - GroupStartPos) = 1) or
                (((GroupEndPos - GroupStartPos) = 2) and (Pattern[GroupStartPos] = Esc))) and
                (Pattern[GroupEndPos] in [Closure, MaybeOne])) then begin
              Done := True;
              RaiseStError(EStRegExError, stscClosureMaybeEmpty);
            end else
              PatRec^.NestedPattern := TempPatRec;
          end else
            PatRec^.NestedPattern := TempPatRec;
        end else
          PatRec^.NestedPattern := TempPatRec;
      end else begin
        {didn't find egroup}
        Done := True;
        RaiseStError(EStRegExError, stscUnbalancedParens);
      end;
    end else if ((AChar = BTag) and (not(TagOn))) then begin
      AddTokenToPattern(PatRec, LastPatRec, tknBegTag, '');
      TagOn := True;
    end else if ((AChar = ETag) and (TagOn)) then begin
      AddTokenToPattern(PatRec, LastPatRec, tknEndTag,  '');
      TagOn := False;
    end else if (((AChar = Closure) or (AChar = ClosurePlus) or
                  (AChar = MaybeOne)) and (I > Start)) then begin
      if ((LastPatRec^.Token in [tknBegOfLine, tknEndOfLine, tknMaybeOne, tknClosure]) or
          (NextLastPatRec^.Token = tknClosure)) then begin
        {error, can't have closure after any of these}
        Done := True;
        RaiseStError(EStRegExError, stscFollowingClosure);
      end else begin
        if (AChar = ClosurePlus) then begin
          {insert an extra copy of the last token before the closure}
          TempPatRec := FNodes.CloneNode(LastPatRec);
          NextLastPatRec^.NextPattern := TempPatRec;
          TempPatRec^.NextPattern := LastPatRec;
          NextLastPatRec := TempPatRec;
        end;
        {insert the closure between next to last and last token}
        TempPatRec := FNodes.AllocNode;
        NextLastPatRec^.NextPattern := TempPatRec;
        if (AChar = MaybeOne) then
          TempPatRec^.Token := tknMaybeOne
        else
          TempPatRec^.Token := tknClosure;
        TempPatRec^.OneChar := Null;

        TempPatRec^.NextPattern := LastPatRec;
        TempPatRec^.NextOK := False;
        {set j and lastj back into sequence}
        PatRec := LastPatRec;
        LastPatRec := TempPatRec;
      end;
    end else begin
      if (AChar = Esc) then begin
        {skip over escape character}
        I := Succ(I);
        AChar := Pattern[I];
        case AChar of
          lSpace     : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #32);
          lNewline   :
            begin
              AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #13);
              LastPatRec  := PatRec;
              AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #10);
            end;
          lTab       : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #9);
          lBackSpace : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #8);
          lReturn    : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #13);
          lFeed      : AddTokenToPattern(PatRec, LastPatRec, tknLitChar, #10);
          lWordDelim : AddTokenToPattern(PatRec, LastPatRec, tknCharClass, StWordDelimString);
          lHex       : AddTokenToPattern(PatRec, LastPatRec, tknCharClass, StHexDigitString);
        else
          AddTokenToPattern(PatRec, LastPatRec, tknLitChar,AChar);
        end;
      end else
        AddTokenToPattern(PatRec, LastPatRec, tknLitChar, AChar);
    end;
    NextLastPatRec := LastPatRec;
    LastPatRec  := PatRec;
    if not(Done) then
      I := Succ(I);
  end; {of looking through pattern string}

  if ((Done) or (Pattern[I] <> Delim)) then begin
    Result := 0;
    RaiseStError(EStRegExError, stscPatternError);
  end else
    Result := I;
end;


function TStStreamRegEx.GetPattern(var Pattern : PAnsiChar;
                                   var PatList : PStPatRecord) : Boolean;
{-convert a Pattern PAnsiChar into a pattern list, pointed to by patlist}
{-return true if successful}
var
  TagOn          : Boolean;
begin
  TagOn := False;
  Result := (MakePattern(Pattern, 0, EndStr, TagOn, PatList) > 0);
  if TagOn then begin
    GetPattern := False;
    RaiseStError(EStRegExError, stscUnbalancedTag);
  end;
end;


procedure TStStreamRegEx.AddTokenToReplace(var PatRec : PStPatRecord;
                                           LastPatRec : PStPatRecord;
                                                Token : TStTokens;
                                          const S     : ShortString);  {!!.02}
{-add a token record to the pattern list}
{S contains a literal character or an expanded character class}
begin
  PatRec := FNodes.AllocNode;
  PatRec^.Token := Token;                    {save token type}
  PatRec^.NextOK  := False;                  {default to non-alternation}
  LastPatRec^.NextPattern := PatRec;         {hook up the previous token}
  if (Token = tknLitChar) or (Token = tknDitto) then begin
    PatRec^.OneChar := S[1];
    PatRec^.StrPtr := nil;
  end else
    RaiseStError(EStRegExError, stscUnknownError);
end;


function TStStreamRegEx.MakeReplacePattern(Pattern     : PAnsiChar;
                                           Start       : Integer;
                                           Delim       : AnsiChar;
                                           var PatList : PStPatRecord) : Integer;
{-make a pattern list from arg[i], starting at start, ending at delim}
{return 0 is error, last char position in arg if OK}
var
  I          : Integer;
  PatRec,
  LastPatRec : PStPatRecord;
  Done       : Boolean;
  AChar      : AnsiChar;

begin
  PatList := FNodes.AllocNode;
  PatList^.Token     := tknNil;    {put a nil token at the beginning}
  PatList^.NextOK    := False;
  LastPatRec := PatList;
  I := Start;                    {start point of pattern string}
  Done := False;
  while not(Done) and (Pattern[I] <> Delim) and (Pattern[I] <> EndStr) do begin
    AChar := Pattern[I];
    if (AChar = Ditto) then
      AddTokenToReplace(PatRec, LastPatRec, tknDitto, '0')
    else begin
      if (AChar = Esc) then begin
        {skip over escape character}
        I := Succ(I);
        AChar := Pattern[I];
        if (AChar >= '1') and (AChar <= '9') then
          {a tagged ditto}
          AddTokenToReplace(PatRec, LastPatRec, tknDitto, AChar)
        else case AChar of
          lSpace       : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #32);
          lNewline     :
            begin
              AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #13);
              LastPatRec := PatRec;
              AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #10);
            end;
          lTab         : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #9);
          lBackSpace   : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #8);
          lReturn      : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #13);
          lFeed        : AddTokenToReplace(PatRec, LastPatRec, tknLitChar, #10);
          lNil         : ;
        else
          AddTokenToReplace(PatRec, LastPatRec, tknLitChar, AChar);
        end;
      end else
        AddTokenToReplace(PatRec, LastPatRec, tknLitChar, AChar);
    end;
    LastPatRec := PatRec;
    if not(Done) then
      Inc(I);
  end; {of looking through pattern string}

  if Done or (Pattern[I] <> Delim) then begin
    Result := 0;
    RaiseStError(EStRegExError, stscPatternError);
  end else
    Result := I;
end;


function TStStreamRegEx.GetReplace(Pattern     : PAnsiChar;
                                   var PatList : PStPatRecord) : Boolean;
begin
  Result := (MakeReplacePattern(Pattern, 0, EndStr, PatList) > 0);
end;


function TStStreamRegEx.MatchOnePatternElement(var Buf    : PAnsiChar;
                                               var I      : Integer;
                                               var TagOn  : Boolean;
                                               var TagNum : Integer;
                                               PatPtr   : PStPatRecord) : Boolean;
{-match one pattern element at pattern pointed to by PatPtr, Buf[I]}
var
  Advance  : -1..255;
  AToken   : TStTokens;
  PatPos   : Integer;
  K        : Integer;
  C        : AnsiChar;
begin
  Advance := -1;
  AToken := PatPtr^.Token;
  if FIgnoreCase then
    C := AnsiChar(AnsiUpperCase(Buf[I])[1])
  else
    C := Buf[I];

  if (C <> EndStr) then begin
    if (AToken = tknLitChar) then begin
      if (C = PatPtr^.OneChar) then
        Advance := 1;
    end else if (AToken = tknCharClass) then begin
      if (StrChPosS(PatPtr^.StrPtr^, C, K)) then
        Advance := 1;
    end else if (AToken = tknNegCharClass) then begin
      if (not (C in [#13, #10])) then begin
        if not (StrChPosS(PatPtr^.StrPtr^, C, K)) then
          Advance := 1;
      end;
    end else if (AToken = tknAnyChar) then begin
      if not (C in [#13, #10]) then
        Advance := 1;
    end else if (AToken = tknBegOfLine) then begin
      if (I = 0) then
        Advance := 0;
    end else if (AToken = tknEndOfLine) then begin
      if (C = #13) and (Buf[Succ(I)] = #10) then
        Advance := 0;
    end else if (AToken = tknNil) then begin
      Advance := 0;
    end else if (AToken = tknBegTag) then begin
      Advance := 0;
      if not(TagOn) then begin
        TagNum := Succ(TagNum);
        TagOn := True;
      end;
    end else if (AToken = tknEndTag) then begin
      Advance := 0;
      TagOn := False;
    end else if (AToken = tknGroup) then begin
      {we treat a group as a "character", but allow advance of multiple chars}
      {recursive call to SearchMatchPattern}
      PatPos := SearchMatchPattern(Buf, I, TagOn, TagNum, PatPtr^.NestedPattern);
      if (PatPos >= I) then begin
        I := PatPos;
        Advance := 0;
      end;
    end;
  end else begin
    {at end of line}
    {end tag marks match}
    if (AToken = tknEndTag) then
      Advance := 0;
  end;

  if (Advance >= 0) then begin
    {ignore tag words here, since they are not used}
    Result := True;
    Inc(I, Advance);
  end else
    Result := False;
end;


function TStStreamRegEx.SearchMatchPattern(var Buf    : PAnsiChar;
                                               OffSet : Integer;
                                           var TagOn  : Boolean;
                                           var TagNum : Integer;
                                           PatPtr : PStPatRecord) : Integer;
{-look for match of pattern list starting at PatPtr with Buf[offset...]}
{-return the last position that matched}
var
  I      : Integer;
  K      : Integer;
  PatRec : PStPatRecord;
  Done   : Boolean;
  AToken : TStTokens;

begin
  Done := False;
  PatRec    := PatPtr;
  while not(Done) and (PatRec <> nil) do begin
    AToken := PatRec^.Token;
    if (AToken = tknClosure) then begin
      {a closure}
      PatRec := PatRec^.NextPattern; {step past the closure in the pattern list}
      I := OffSet;                   {leave the current line position unchanged}
      {match as many as possible}
      while not(Done) and (Buf[I] <> EndStr) do begin
       if not(MatchOnePatternElement(Buf, I, TagOn, TagNum, PatRec)) then
          Done := True;
      end;
      {I points to the location that caused a non-match}
      {match rest of pattern against rest of input}
      {shrink closure by one after each failure}
      Done := False;
      K := -1;
      while not(Done) and (I >= OffSet) do begin
        K := SearchMatchPattern(Buf, I, TagOn, TagNum, PatRec^.NextPattern);
        if (K > -1) then
          Done := True
        else
          Dec(I);
      end;
      OffSet := K;   {if k=-1 then failure else success}
      Done := True;
    end else if (AToken = tknMaybeOne) then begin
      {a 0 or 1 closure}
      PatRec := PatRec^.NextPattern;   {step past the closure marker}
      {match or no match is ok, but advance lin cursor if matched}
      MatchOnePatternElement(Buf, OffSet, TagOn, TagNum, PatRec);
      {advance to the next pattern token}
      PatRec := PatRec^.NextPattern;
    end else if not(MatchOnePatternElement(Buf, OffSet,
                                           TagOn, TagNum, PatRec)) then begin
      if PatRec^.NextOK then begin
        {we get another chance because of alternation}
        PatRec := PatRec^.NextPattern;
      end else begin
        OffSet := -1;
        Done := True;
      end;
    end else begin 
      {skip over alternates if we matched already}
      while (PatRec^.NextOK) and (PatRec^.NextPattern <> nil) do
        PatRec := PatRec^.NextPattern;
      {move to the next non-alternate}
      PatRec := PatRec^.NextPattern;
    end;
  end;
  Result := OffSet;
end;


function TStStreamRegEx.FindMatch(var Buf        : PAnsiChar;
                                      PatPtr     : PStPatRecord;
                                  var REPosition : TMatchPosition) : Boolean;
var
  I,
  LPos,
  TagNum : Integer;
  TagOn  : Boolean;

begin
  LPos   := -1;
  I      := 0;
  TagNum := 0;
  TagOn := False;
  Result := False;
  REPosition.Length := 0;
  while (Buf[I] <> EndStr) and (LPos = -1) do begin
    LPos := SearchMatchPattern(Buf, I, TagOn, TagNum, PatPtr);
    Result := (LPos > -1);
    if (Result) then begin
      REPosition.StartPos := I+1;
      RePosition.EndPos   := LPos;
      RePosition.Length   := REPosition.EndPos - REPosition.StartPos + 1;
    end;
    Inc(I);
  end;
end;



procedure TStStreamRegEx.InsertLineNumber(Dest    : PAnsiChar;
                                    const S : PAnsiChar;
                                    LineNum : Integer);
var
  Count : Cardinal;
  SI    : string[8];
begin
  Dest[0] := #0;
  Count := AnsiStrings.StrLen(S);
  if (Count > MaxLineLength - 8) then
    Count := MaxLineLength - 8;
  SI := AnsiString(LeftPadS(IntToStr(LineNum), 6) + '  ');
  Move(SI[1], Dest[0], 8);
  Move(S^, Dest[8], Count);
  Dest[Count+8] := #0;
end;



function TStStreamRegEx.ProcessLine(    Buf       : PAnsiChar;
                                        Len       : integer;
                                        LineNum   : integer;
                                        CheckOnly : Boolean;
                                    var REPosition: TMatchPosition) : Boolean;
var
  Tmp : PAnsiChar;
begin
  GetMem(Tmp, MaxLineLength+1);
  try
    if (FSelAvoidPatPtr <> nil) then begin
      if (not Avoid) then
        Result := FindMatch(Buf, FSelAvoidPatPtr, REPosition)
      else if (Avoid) then
        Result := not(FindMatch(Buf, FSelAvoidPatPtr, REPosition))
      else
        Result := True;
    end else
      Result := True;

    if Result then begin
      {met select criterion, perhaps by default}
      FSelectCount := Succ(FSelectCount);
      if ((FReplacePatPtr <> nil) and (not CheckOnly)) then begin
        if (ooModified in FOutputOptions) then begin
          {we only want to replace and output lines that have a match}
          Result := FindMatch(Buf, FMatchPatPtr, REPosition);
        end;
        if Result then begin
          Tmp[0] := #0;
          SubLine(Buf);
          if (not (ooCountOnly in FOutputOptions)) then begin
            if (LineNumbers) then
              InsertLineNumber(Tmp, FOutlineBuf, LineNum)
            else
              AnsiStrings.StrCopy(Tmp, FOutlineBuf);
            Tmp[AnsiStrings.StrLen(Tmp)-2] := #0;
            FOutTextStream.WriteLineZ(Tmp);
          end;
          {subline keeps a count of matched lines and replaced patterns}
        end;
      end else if (FMatchPatPtr <> nil) then begin
        Result := FindMatch(Buf, FMatchPatPtr, REPosition);
        {met match criterion}
        if Result then begin
          FMatchCount := Succ(FMatchCount);
          if (not CheckOnly) then begin
            if (not (ooCountOnly in FOutputOptions)) then begin
              Buf[Len] := #0;
              if (LineNumbers) then
                InsertLineNumber(Tmp, Buf, LineNum)
              else
                AnsiStrings.StrCopy(Tmp, Buf);
              Tmp[AnsiStrings.StrLen(Tmp)] := #0;
              FOutTextStream.WriteLineZ(Tmp);
            end;
          end;
        end;
      end else begin
        {we are neither matching nor replacing, just selecting}
        {output the selected line}
        if (not CheckOnly) then begin
          if (not (ooCountOnly in FOutputOptions)) then begin
            Buf[Len] := #0;
            if (LineNumbers) then
              InsertLineNumber(Tmp, Buf, LineNum)
            else
              AnsiStrings.StrCopy(Tmp, Buf);
            Tmp[AnsiStrings.StrLen(Tmp)] := #0;
            FOutTextStream.WriteLineZ(Tmp);
          end;
        end;
      end;
    end else begin
      {non-selected line, do we write it?}
      if (ooUnselected in FOutputOptions) and
         (not (ooCountOnly in FOutputOptions)) then begin
        Buf[Len] := #0;
        if (LineNumbers) then
          InsertLineNumber(Tmp, Buf, LineNum)
        else
          AnsiStrings.StrCopy(Tmp, Buf);
        Tmp[AnsiStrings.StrLen(Tmp)] := #0;
        FOutTextStream.WriteLineZ(Tmp);
      end;
    end;
  finally
    FreeMem(Tmp, MaxLineLength+1);
  end;
end;



procedure TStStreamRegEx.SetMatchPatSL(Value : TStringList);
begin
  FMatchPatSL.Assign(Value);
  DisposeItems(FMatchPatPtr);
end;



procedure TStStreamRegEx.SetOptions(Value : TStOutputOptions);
begin
  if (Value <> FOutputOptions) then begin
    FOutputOptions := Value;
    if (ooCountOnly in FOutputOptions) then
      FOutputOptions := [ooCountOnly];
  end;
end;



procedure TStStreamRegEx.SetReplacePatSL(Value : TStringList);
begin
  FReplacePatSL.Assign(Value);
  DisposeItems(FReplacePatPtr);
end;



procedure TStStreamRegEx.SetSelAvoidPatSL(Value : TStringList);
begin
  FSelAvoidPatSL.Assign(Value);
  DisposeItems(FSelAvoidPatPtr);
end;


function TStStreamRegEx.SubLineMatchOne(Buf        : PAnsiChar;
                                        var Flags  : TStFlag;
                                        var TagOn  : Boolean;
                                        var I      : Integer;
                                        var TagNum : Integer;
                                        PatPtr     : PStPatRecord) : Boolean;
var
  Advance  : -1..255;
  lToken   : TStTokens;
  PatPos   : Integer;
  K        : Integer;
  C        : AnsiChar;
begin                       
  Advance := -1;
  lToken := PatPtr^.Token;
  if FIgnoreCase then
    C := AnsiChar(AnsiUpperCase(Buf[I])[1])
  else
    C := Buf[I];

  if (C <> EndStr) then begin
    if (lToken = tknLitChar) then begin
      if (C = PatPtr^.OneChar) then
        Advance := 1;
    end else if (lToken = tknCharClass) then begin
      if (StrChPosS(PatPtr^.StrPtr^, C, K)) then
        Advance := 1;
    end else if (lToken = tknNegCharClass) then begin
      if (pos(C, NewLine) = 0) then begin
        if not (StrChPosS(PatPtr^.StrPtr^, C, K)) then
          Advance := 1;
      end;
    end else if (lToken = tknAnyChar) then begin
      if (not (C in [#13, #10])) then
        Advance := 1;
    end else if (lToken = tknBegOfLine) then begin
      if (I = 0) then
        Advance := 0;
    end else if (lToken = tknEndOfLine) then begin
      if (C = #13) and (Buf[Succ(I)] = #10) then begin
        Advance := 0;
      end;
    end else if (lToken = tknNil) then begin
      Advance := 0;
    end else if (lToken = tknBegTag) then begin
      Advance := 0;
      if not(TagOn) then begin
        Inc(TagNum);
        TagOn := True;
      end;
    end else if (lToken = tknEndTag) then begin
      Advance := 0;
      TagOn := False;
    end else if (lToken = tknGroup) then begin
      {we treat a group as a "character", but allow advance of multiple chars}

      PatPos := SubLineMatchPattern(Buf, Flags, TagOn, TagNum,
                                    I, PatPtr^.NestedPattern);
      if (PatPos >= I) then begin
        I := PatPos;
        Advance := 0;
      end;
    end;
  end else begin
    {at end of line}
    {end tag marks match}
    if (lToken = tknEndTag) then
      Advance := 0;
  end;

  if (Advance > 0) then begin
    {we had a match at this (these) character position(s)}
    {set the match flags}
    if (TagOn) then
      Flags[I] := TagNum
    else
      Flags[I] := 0;
    Inc(I, Advance);
    Result := True;
  end else if (Advance = 0) then begin
    Result := True;
  end else begin
    {this character didn't match}
    Result := False;
    Flags[I] := -1;
  end;
end;



function TStStreamRegEx.SubLineMatchPattern(Buf        : PAnsiChar;
                                            var Flags  : TStFlag;
                                            var TagOn  : Boolean;
                                            var TagNum : Integer;
                                            OffSet     : Integer;
                                            PatPtr     : PStPatRecord) : Integer;
{-look for match of pattern list starting at PatPtr with Buf[offset...]}
{return the last position that matched}
var
  I,
  LocTag   : Integer;
  PatPos   : Integer;
  PatRec   : PStPatRecord;
  Done     : Boolean;
  AToken   : TStTokens;
  OldTagOn : boolean;                                          
  OldTagNum: integer;
begin
  Done := False;
  PatRec := PatPtr;
  while not(Done) and (PatRec <> nil) do begin
    AToken := PatRec^.Token;
    if (AToken = tknClosure) then begin
      {a closure}
      PatRec := PatRec^.NextPattern; {step past the closure in the pattern list}
      I := OffSet;                   {leave the current line position unchanged}
      LocTag := TagNum;
      {match as many as possible}
      while not(Done) and (Buf[I] <> EndStr) do begin
        if not(SubLineMatchOne(Buf, Flags, TagOn,                      
                               I, LocTag, PatRec)) then
          Done := True;
      end;
      {i points to the location that caused a non-match}
      {match rest of pattern against rest of input}
      {shrink closure by one after each failure}
      Done := False;
      PatPos := -1;
      while not(Done) and (I >= OffSet) do begin
        OldTagOn := TagOn;                                     
        OldTagNum := LocTag;
        PatPos := SubLineMatchPattern(Buf, Flags, TagOn,
                                      LocTag, I, PatRec^.NextPattern);
        if (PatPos > -1) then
          Done := True
        else begin
          I := Pred(I);
          TagOn := OldTagOn;                                   
          LocTag := OldTagNum;
        end;
      end;
      OffSet := PatPos;            {if k=-1 then failure else success}
      TagNum := LocTag;
      Done   := True;
    end else if (AToken = tknMaybeOne) then begin
      {a 0 or 1 closure}
      PatRec := PatRec^.NextPattern;    {step past the closure marker}
      {match or no match is ok, but advance lin cursor if matched}
      SubLineMatchOne(Buf, Flags, TagOn, OffSet, TagNum, PatRec);
      {advance to the next pattern token}
      PatRec := PatRec^.NextPattern;
    end else if not(SubLineMatchOne(Buf, Flags, TagOn,
                                    OffSet, TagNum, PatRec)) then begin
      if PatRec^.NextOK then begin
        {we get another chance because of alternation}
        PatRec := PatRec^.NextPattern;
      end else begin
        OffSet := -1;
        Done := True;
      end;
    end else begin
      {skip over alternates if we matched already}
      while PatRec^.NextOK and (PatRec^.NextPattern <> nil) do
        PatRec := PatRec^.NextPattern;
      {move to the next non-alternate}
      PatRec := PatRec^.NextPattern;
    end;
  end;
  Result := OffSet;
end;


function TStStreamRegEx.SubLineFindTag(Buf         : PAnsiChar;
                                       I           : Integer;
                                       IEnd        : Integer;
                                       TagNum      : Integer;
                                       var Flags   : TStFlag;
                                       var IStart  : Integer;
                                       var IStop   : Integer) : Boolean;
{-find the tagged match region}
{return true if it is found}
begin
  IStart := I;
  while (Buf[IStart] <> EndStr) and (Flags[IStart] <> TagNum) do
    Inc(IStart);
  if (Flags[IStart] = TagNum) then begin
    Result := True;
    IStop := IStart;
    while (Flags[IStop] = TagNum) and (IStop < IEnd) do
      Inc(IStop);
  end else
    Result := False;
end;  {findtag}



procedure TStStreamRegEx.SubLineWrite(Buf       : PAnsiChar;
                                      S         : PAnsiChar;
                                      RepRec    : PStPatRecord;
                                      I,
                                      IEnd      : Integer;
                                      var Flags : TStFlag);
{-Write the output line with replacements}
var
  TagNum,
  IStart,
  IStop     : Integer;
  PatRec    : PStPatRecord;
  Token     : TStTokens;
begin  {writesub}
  {scan the replacement list}
  S[0] := #0;
  PatRec := RepRec;
  while (PatRec <> nil) do begin
    Token := PatRec^.Token;
    if (Token = tknDitto) then begin
      TagNum := Ord(PatRec^.OneChar)-Ord('0');
      if (TagNum = 0) then begin
        {untagged ditto}
        {add the entire matched region}
        AppendS(S, S, @Buf[I], IEnd-I);
      end else begin
        {tagged ditto}
        {find the tagged region}

        if SubLineFindTag(Buf, I, IEnd, TagNum, Flags, IStart, IStop) then begin
          {add the tagged region}
          AppendS(S, S, @Buf[IStart], IStop-IStart);
        end else begin
           {else couldn't find tagged word, don't append anything}
        end;
      end;
    end else if (Token = tknLitChar) then
      AppendS(S, S, @PatRec^.OneChar, 1);
    PatRec := PatRec^.NextPattern;
  end;
end;



procedure TStStreamRegEx.SubLine(Buf : PAnsiChar);
var
  I,
  M,
  NumToAdd,
  TagNum,
  Lastm      : Integer;

  Flags      : TStFlag;
  TagOn,
  DidReplace : Boolean;
  ALine      : PAnsiChar;
begin
  DidReplace := False;
  LastM  := -1;
  I := 0;

  GetMem(ALine, MaxLineLength+1);
  try
    FOutLineBuf[0] := #0;
    FillChar(ALine^, MaxLineLength+1, #0);
    while (Buf[I] <> EndStr) do begin
      TagNum := 0;
      TagOn := False;

      M := SubLineMatchPattern(Buf, Flags, TagOn, TagNum, I, FMatchPatPtr);
      if (M > -1) and (M <> I) and (LastM <> M) then begin
        {keep track of count}
        DidReplace := True;
        Inc(FReplaceCount);
        {replace matched text}

        SubLineWrite(Buf, ALine, FReplacePatPtr, I, M, Flags);
        LastM := M;
        AppendS(FOutLineBuf, FOutLineBuf, ALine, AnsiStrings.StrLen(ALine));
      end;

      if (M = -1) or (M = I) then begin
        {no match or null match, append the character}
          if (Buf[I] = #13) then
            NumToAdd := 2
          else
            NumToAdd := 1;
        AppendS(FOutLineBuf, FOutLineBuf, @Buf[I], NumToAdd);
        I := I + NumToAdd;
      end else                    {skip matched text}
        I := M;

    end;
    if DidReplace then
      Inc(FMatchCount);
  finally
    FreeMem(ALine, MaxLineLength+1)
  end;
end;


{******************************************************************************}
{                           TStRegEx Implementation                            }
{******************************************************************************}

constructor TStRegEx.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FAvoid          := False;
  FIgnoreCase     := False;
  FLineNumbers    := False;
  FOutputOptions  := [];

  FInLineTerminator := ltCRLF;
  FInLineTermChar   := #10;
  FInFixedLineLength:= 80;

  FOutLineTerminator  := ltCRLF;
  FOutLineTermChar    := #10;
  FOutFixedLineLength := 80;      {not used straight away}

  FMaxLineLength := 1024;

  FMatchPatSL    := TStringList.Create;
  FMatchPatPtr   := nil;
  FSelAvoidPatSL := TStringList.Create;
  FSelAvoidPatPtr:= nil;
  FReplacePatSL  := TStringList.Create;
  FReplacePatPtr := nil;

  FInFileStream  := nil;
  FOutFileStream := nil;

  FStream := TStStreamRegEx.Create;
end;


destructor TStRegEx.Destroy;
begin
  FMatchPatSL.Free;
  FMatchPatSL := nil;

  FReplacePatSL.Free;
  FReplacePatSL := nil;

  FSelAvoidPatSL.Free;
  FSelAvoidPatSL := nil;

  FStream.Free;
  FStream := nil;

  inherited Destroy;
end;


function TStRegEx.CheckString(const S : AnsiString;
                              var REPosition : TMatchPosition) : Boolean;
begin
  if (Assigned(FStream)) then begin
    SetStreamProperties;
    Result := FStream.CheckString(S, REPosition);
  end else
    Result := False;
end;


function TStRegEx.ReplaceString(var S : AnsiString;
                                var REPosition : TMatchPosition) : Boolean;
begin
  if (Assigned(FStream)) then begin
    SetStreamProperties;
    Result := FStream.ReplaceString(S, REPosition);
  end else
    Result := False;
end;


function TStRegEx.FileMasksToRegEx(const Masks : AnsiString) : Boolean;{!!.02}
begin
  if (Assigned(FStream)) then begin
    SetStreamProperties;
    Result := FStream.FileMasksToRegEx(Masks);
    if (Result) then
      FMatchPatSL.Assign(FStream.FMatchPatSL);
  end else
    Result := False;
end;


function TStRegEx.Execute : Boolean;
begin
  Result := False;
  try
    if (not FileExists(FInputFile)) then
      RaiseStError(EStRegExError, stscInFileNotFound);

    try
      FInFileStream := TFileStream.Create(FInputFile,
                                          fmOpenRead or fmShareDenyWrite);
      FStream.InputStream := FInFileStream
    except
      RaiseStError(EStRegExError, stscREInFileError);
      Exit;
    end;

    if not (ooCountOnly in OutputOptions) then begin
      if (FileExists(FOutputFile)) then
        try
          SysUtils.DeleteFile(FOutputFile);
        except
          RaiseStError(EStRegExError, stscOutFileDelete);
          Exit;
        end;

      FOutFileStream := nil;
      FStream.OutputStream := nil;
      try
        FOutFileStream := TFileStream.Create(FOutputFile, fmCreate);
        FStream.OutputStream := FOutFileStream
      except
        RaiseStError(EStRegExError, stscOutFileCreate);
        Exit;
      end;
    end;

    SetStreamProperties;
    Result := FStream.Execute;

    FMatchCount    := FStream.FMatchCount;
    FSelectCount   := FStream.FSelectCount;
    FReplaceCount  := FStream.FReplaceCount;
    FInLineCount   := FStream.FInLineCount;
    FLinesPerSec   := FStream.FLinesPerSec;
  finally
    FInFileStream.Free;
    FInFileStream := nil;

    FOutFileStream.Free;
    FOutFileStream := nil;
  end;
end;



procedure TStRegEx.SetMatchPatSL(Value : TStringList);
begin
  FMatchPatSL.Assign(Value);
end;



procedure TStRegEx.SetOptions(Value : TStOutputOptions);
begin
  if (Value <> FOutputOptions) then begin
    FOutputOptions := Value;
    if (ooCountOnly in FOutputOptions) then
      FOutputOptions := [ooCountOnly];
  end;
end;



procedure TStRegEx.SetReplacePatSL(Value : TStringList);
begin
  FReplacePatSL.Assign(Value);
end;



procedure TStRegEx.SetSelAvoidPatSL(Value : TStringList);
begin
  FSelAvoidPatSL.Assign(Value);
end;



procedure TStRegEx.SetStreamProperties;
begin
  if (not Assigned(FStream)) then Exit;

  FStream.InLineTermChar    := FInLineTermChar;
  FStream.InLineTerminator  := FInLineTerminator;
  FStream.InFixedLineLength := FInFixedLineLength;
{!!.02 - Changed }
//  FStream.InLineTermChar    := FOutLineTermChar;
//  FStream.InLineTerminator  := FOutLineTerminator;
//  FStream.InFixedLineLength := FOutFixedLineLength;
  FStream.OutLineTermChar    := FOutLineTermChar;
  FStream.OutLineTerminator  := FOutLineTerminator;
  FStream.OutFixedLineLength := FOutFixedLineLength;
{!!.02 - Changed end }

  FStream.Avoid          := FAvoid;
  FStream.IgnoreCase     := FIgnoreCase;
  FStream.LineNumbers    := FLineNumbers;
  FStream.MatchPattern   := FMatchPatSL;
  FStream.OnMatch        := FOnMatch;
  FStream.OnProgress     := FOnProgress;
  FStream.OutputOptions  := FOutputOptions;
  FStream.ReplacePattern := FReplacePatSL;
  FStream.SelAvoidPattern:= FSelAvoidPatSL;

  FStream.FMatchCount    := 0;
  FStream.FSelectCount   := 0;
  FStream.FReplaceCount  := 0;
  FStream.FInLineCount   := 0;
  FStream.FLinesPerSec   := 0;
end;


end.

