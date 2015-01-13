// Upgraded to Delphi 2009: Sebastian Zierer
// TODO-UNICODE - Upgrade to Unicode (BM asm code is a blocking issue)

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
{* SysTools: StOStr.pas 4.04                             *}
{*********************************************************}
{* SysTools: String class                                *}
{*********************************************************}

{$I StDefine.inc}

unit StOStr;

interface

uses
  Windows, SysUtils, Classes,
  StConst, StBase, StStrL;

const
{.Z+}
  DefAllocSize = 8;
  DefDelimiters = ' ';
  DefQuote = '''';
  DefRepeatValue = 1;
  DefResetRepeat = True;
  DefTabSize = 8;
  DefWrap = 80;
{.Z-}

type

  TStString = class(TPersistent)
  private
{.Z+}
  protected {private}
    FAlloc : Cardinal;
    FBM : BTable;
    FBMString : PChar;
    FCursor : PChar;
    FDelimiters : PChar;
    FEnableCursor : Boolean;
    FItems : TStringList;
    FOneBased : Boolean;
    FRepeatValue : Cardinal;
    FResetRepeat : Boolean;
    FQuote : Char;
    FString : PChar;
    FTabSize : Byte;
    FTemp : PChar;
    FTempAlloc : Cardinal;
    FWrap : Cardinal;
    FLineTermChar: Char;
    FLineTerminator: TStLineTerminator;
  protected
    procedure SetLineTerm(const Value: TStLineTerminator);
    procedure SetLineTermChar(const Value: Char);
    procedure AddIntToList(Num : Integer);
    procedure AllocTemp(Size : Cardinal);
    procedure BMMakeTable(S : PChar);
    procedure CheckAlloc(Size : Cardinal);
    function  DesiredCursor : PChar;
    procedure FixCursor(Pos, Size : Cardinal; Delete : Boolean);
    function  Get(Index : Cardinal) : Char;
    function  GetAsciiCount : Cardinal;
    function  GetAsShortStr : string;
    function  GetCursorPos : Cardinal;
    function  GetDelimiters : string;
    function  GetLength : Cardinal;
    function  GetRelativePos(Pos : Cardinal) : Cardinal;
    function  GetSoundex : string;
    function  GetWordCount : Cardinal;
    procedure Put(Index : Cardinal; Item : Char);
    procedure SetAllocLength(Value : Cardinal);
    procedure SetAsShortStr(const Value : string);
    procedure SetCursorPos(Value : Cardinal);
    procedure SetDelimiters(Value : string);
    procedure SetItems(Value : TStringList);
    function  SuggestSize(Size : Cardinal) : Cardinal;
    procedure TempToString;
    procedure UpdateCursor(Pos : Cardinal);
    function  GetAsLongStr : string;
    procedure SetAsLongStr(Value : string);
    function  GetAsVariant : Variant;
    procedure SetAsVariant(Value : Variant);
    function MakeTerminator(var Terminator : PChar) : Integer;       {!!.01}

{.Z-}
  public
    constructor Create;
    constructor CreateAlloc(Size : Cardinal); virtual;
    constructor CreateS(const S : string); virtual;
    constructor CreateZ(const S : PChar); virtual;
    constructor CreateV(const S : Variant); virtual;
    destructor Destroy; override;
    procedure AppendPChar(S : PChar);
    procedure AppendString(S : string);
    function  AsciiPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
    function  BMSearch(const S : string; var Pos : Cardinal) : Boolean;
    function  BMSearchUC(const S : string; var Pos : Cardinal) : Boolean;
    procedure Center(Size : Cardinal);
    procedure CenterCh(const C : Char; Size : Cardinal);
    function  CharCount(const C : Char) : Cardinal;
    function  CharExists(const C : Char) : boolean;
    procedure CharStr(const C : Char; Size : Cardinal);
    procedure ClearItems;
    procedure CursorNextWord;
    procedure CursorNextWordPrim;
    procedure CursorPrevWord;
    procedure CursorPrevWordPrim;
    procedure CursorToEnd;
    procedure DeleteAsciiAtCursor;
    procedure DeleteAtCursor(Length : Cardinal);
    procedure DeleteWordAtCursor;
    procedure Detab;
    procedure Entab;
    function  ExtractAscii(N : Cardinal) : string;
    function  ExtractWord(N : Cardinal) : string;
    procedure Filter(const Filters : PChar);
    function  GetAsciiAtCursor : string;
    function  GetAsciiAtCursorZ(Dest : PChar) : PChar;
    function  GetAsPChar(Dest : PChar) : PChar;
    function  GetWordAtCursor : string;
    function  GetWordAtCursorZ(Dest : PChar) : PChar;
    procedure InsertLineTerminatorAtCursor;
    procedure InsertLineTerminator(Pos : Cardinal);
    procedure InsertPCharAtCursor(S : PChar);
    procedure InsertStringAtCursor(S : string);
    procedure ItemsToString;
    procedure LeftPad(Size : Cardinal);
    procedure LeftPadCh(const C : Char; Size : Cardinal);
    function  MakeLetterSet : Integer;
    procedure MoveCursor(Delta : Integer);
    procedure Pack;
    procedure Pad(Size : Cardinal);
    procedure PadCh(const C : Char; Size : Cardinal);
    procedure ResetCursor;
    procedure Scramble(const Key : string);
    procedure SetAsPChar(S : PChar);
    function  SizeAsciiAtCursor(InclTrailers : Boolean) : Cardinal;
    function  SizeWordAtCursor(InclTrailers : Boolean) : Cardinal;
    procedure StrChDelete(Pos : Cardinal);
    procedure StrChInsert(const C : Char; Pos : Cardinal);
    function  StrChPos(const C : Char; var Pos : Integer) : Boolean;
    procedure StringToItems;
    procedure StripLineTerminators;
    procedure StrStDelete(const Pos, Length : Cardinal);
    procedure StrStInsert(const S : string; Pos : Cardinal);
    function  StrStPos(const S : string; var Pos : Cardinal) : Boolean;
    procedure Substitute(FromStr, ToStr : PChar);
    procedure Trim;
    procedure TrimLead;
    procedure TrimSpaces;
    procedure TrimTrail;
    function  WordPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
    procedure WrapToItems;

    property AllocLength : Cardinal
      read FAlloc write SetAllocLength;
    property AsciiCount : Cardinal
      read GetAsciiCount;
    property AsLongStr : string
      read GetAsLongStr write SetAsLongStr;
    property AsVariant : Variant
      read GetAsVariant write SetAsVariant;
    property AsShortStr : string
      read GetAsShortStr write SetAsShortStr;
    property AtIndex[Index: Cardinal]: Char
      read Get write Put; default;
    property CursorPos : Cardinal
      read GetCursorPos write SetCursorPos;
    property Delimiters : string
      read GetDelimiters write SetDelimiters;
    property EnableCursor : Boolean
      read FEnableCursor write FEnableCursor;
    property Length : Cardinal
      read GetLength;
    property LineTermChar : Char
      read FLineTermChar write SetLineTermChar default #10;
    property LineTerminator : TStLineTerminator
      read FLineTerminator write SetLineTerm default ltCRLF;
    property Items : TStringList
      read FItems write SetItems;
    property OneBased : Boolean
      read FOneBased write FOneBased;
    property RepeatValue : Cardinal
      read FRepeatValue write FRepeatValue;
    property ResetRepeat : Boolean
      read FResetRepeat write FResetRepeat;
    property Soundex : string
      read GetSoundex;
    property Quote : Char
      read FQuote write FQuote;
    property TabSize : Byte
      read FTabSize write FTabSize;
    property WordCount : Cardinal
      read GetWordCount;
    property WrapColumn : Cardinal
      read FWrap write FWrap;
  end;

implementation

uses
  StrUtils;

constructor TStString.Create;
{- Create nil string object. }
begin
  inherited Create;
  SetDelimiters(DefDelimiters);
  FItems := TStringList.Create;
  FTabSize := DefTabSize;
  FQuote := DefQuote;
  FRepeatValue := DefRepeatValue;
  FResetRepeat := DefResetRepeat;
  FWrap := DefWrap;

  FLineTerminator := ltCRLF;
  FLineTermChar   := #10;


end;

constructor TStString.CreateAlloc(Size : Cardinal);
{- Create string object allocated to given size. }
var
  AllocSize : Cardinal;
begin
  Create;
  AllocSize := SuggestSize(Size);
  FString := StrAlloc(AllocSize);
  FString[0] := #0;
  FAlloc := AllocSize;
  ResetCursor;
end;

constructor TStString.CreateV(const S : Variant);
{- Create string object and copy variant into it. }
var
  Len : Cardinal;
  Temp : string;
begin
  Create;
  Temp := string(S);
  Len := System.Length(Temp);
  FString := StrAlloc(SuggestSize(Len));
  if Assigned(FString) then begin
    FAlloc := SuggestSize(Len);
    StrCopy(FString, PChar(Temp));
  end;
  ResetCursor;
end;

constructor TStString.CreateS(const S : string);
{- Create string object and copy string into it. }
begin
  Create;
  FString := StrAlloc(SuggestSize(System.Length(S)));
  if Assigned(FString) then begin
    FAlloc := SuggestSize(System.Length(S));
    StrPCopy(FString, S);
  end;
  ResetCursor;
end;

constructor TStString.CreateZ(const S : PChar);
{- Create string object and copy PChar into it. }
begin
  Create;
  FString := StrAlloc(SuggestSize(StrLen(S)));
  if Assigned(FString) then begin
    StrCopy(FString, S);
    FAlloc := SuggestSize(StrLen(S));
  end;
  ResetCursor;
end;

destructor TStString.Destroy;
{- Dispose string object. }
begin
  FItems.Free;
  StrDispose(FBMString);
  StrDispose(FDelimiters);
  StrDispose(FString);
  inherited Destroy;
end;

procedure TStString.AppendPChar(S : PChar);
{- Appends PChar to end of string. }
var
  Temp : PChar;
begin
  CheckAlloc(StrLen(S) + GetLength);
  Temp := StrEnd(FString);
  StrCopy(Temp, S);
end;

procedure TStString.AppendString(S : string);
{- Appends string to end of string. }
var
  Temp : PChar;
begin
  CheckAlloc(System.Length(S) + Integer(GetLength));
  Temp := StrEnd(FString);
  StrPCopy(Temp, S);
end;

function TStString.AsciiPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
{- Returns the Pos of the Nth word using ASCII rules. }
var
  I, Num : Cardinal;
begin
  Result := False;
  Num := N;
  ClearItems;
  for I := 1 to FRepeatValue do begin
    if AsciiPositionL(Num, DesiredCursor, FDelimiters, FQuote, Pos) then begin
      if Result = False then Inc(Num);
      Pos := GetRelativePos(Pos);
      Result := True;
      UpdateCursor(Pos);
      if FOneBased then Inc(Pos);
      AddIntToList(Pos);
    end
      else Break;                                                      
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

function TStString.BMSearch(const S : string; var Pos : Cardinal) : Boolean;
{- Performs case sensitive BM search on string. }
var
  I : Cardinal;
  Temp : PChar;
begin
  Result := False;
  ClearItems;
  Temp := StrAlloc(Succ(System.Length(S)));
  try
    StrPCopy(Temp, S);
    BMMakeTable(Temp);
    for I := 1 to FRepeatValue do begin
      if BMSearchL(DesiredCursor^, StrLen(DesiredCursor), FBM, Temp, Pos) then begin
        Result := True;
        Pos := GetRelativePos(Pos);
        UpdateCursor(Pos);
        if FOneBased then Inc(Pos);
        AddIntToList(Pos);
        Inc(FCursor);
      end else Break;                                                  
    end;
    if Result then Dec(FCursor);                                       
  finally
    StrDispose(Temp);
  end;
end;

function TStString.BMSearchUC(const S : string; var Pos : Cardinal) : Boolean;
{- Performs case insensitive BM search on string. }
var
  I : Cardinal;
  Temp : PChar;
begin
  Result := False;
  ClearItems;
  Temp := StrAlloc(Succ(System.Length(S)));
  try
    StrPCopy(Temp, S);
    StrUpper(Temp);
    BMMakeTable(Temp);
    for I := 1 to FRepeatValue do begin
      if BMSearchUCL(DesiredCursor^, StrLen(DesiredCursor), FBM, Temp, Pos) then begin
        Result := True;
        Pos := GetRelativePos(Pos);
        UpdateCursor(Pos);
        if FOneBased then Inc(Pos);
        AddIntToList(Pos);
        Inc(FCursor);
      end else Break;                                                  
    end;
    if Result then Dec(FCursor);                                       
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.Center(Size : Cardinal);
{- Centers string to Size. }
begin
  CheckAlloc(Size);
  CenterL(FString, Size);
  ResetCursor;
end;

procedure TStString.CenterCh(const C : Char; Size : Cardinal);
{- Centers string with 'Ch' to Size. }
begin
  CheckAlloc(Size);
  CenterChL(FString, C, Size);
  ResetCursor;
end;

function TStString.CharExists(const C : Char) : boolean;
{- Determines whether C exists in string. }
begin
  Result := CharExistsL(DesiredCursor, C);
end;

function TStString.CharCount(const C : Char) : Cardinal;
{- Counts C in string. }
begin
  Result := CharCountL(DesiredCursor, C);
end;

procedure TStString.CharStr(const C : Char; Size : Cardinal);
{- Fills string to Size with C. }
begin
  CheckAlloc(Size);
  FString := PChar(CharStrL(C, Size));
  ResetCursor;
end;

procedure TStString.AddIntToList(Num : Integer);
{- Adds integer value to Items -- as both numeric value and numeric string. }
begin
  FItems.AddObject(IntToStr(Num), TObject(Num));
end;

procedure TStString.AllocTemp(Size : Cardinal);
{- Allocates FTemp to Size. }
begin
  FTemp := StrAlloc(Size);
  FTempAlloc := Size;
end;

procedure TStString.BMMakeTable(S : PChar);
{- Checks whether table needs to be made -- and makes it. }
begin
  if Assigned(FBMString) then
    if StrComp(S, FBMString) = 0 then Exit;
  StrDispose(FBMString);
  FBMString := StrNew(S);
  BMMakeTableL(FBMString, FBM);
end;

procedure TStString.CheckAlloc(Size : Cardinal);
{- Sets allocated length for string if needed size is > current size. }
begin
  if FAlloc = 0 then begin
    FString := StrAlloc(SuggestSize(Size));
    FAlloc := SuggestSize(Size);
    FString[0] := #0;
    ResetCursor;
  end else if Succ(Size) > FAlloc then
    SetAllocLength(Succ(Size));
end;

procedure TStString.ClearItems;
{- Clears Items list. }
begin
  FItems.Clear;
end;

procedure TStString.CursorNextWord;
{- Moves cursor to the beginning of the next word, or terminating null. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    CursorNextWordPrim;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.CursorNextWordPrim;
{- Moves cursor to the beginning of the next word, or terminating null. }
var
  Ch : Char;
begin
  Ch := FCursor^;
  while (Ch <> #0) and (not CharExistsL(FDelimiters, Ch)) do begin
    Inc(FCursor);
    Ch := FCursor^;    
  end;
  while (Ch <> #0) and (CharExistsL(FDelimiters, Ch)) do begin
    Inc(FCursor);
    Ch := FCursor^;
  end;
end;

procedure TStString.CursorPrevWord;
{- Move Cursor to beginning of prev word, or first word in string. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    CursorPrevWordPrim;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;


procedure TStString.CursorPrevWordPrim;
{- Move Cursor to beginning of prev word, or first word in string. }
var
  Ch : Char;
  i  : integer;
begin
  Ch := FCursor^;
  {go around twice to get to the previous word, not the current word}
  for i := 1 to 2 do begin
    while (FCursor <> FString) and (CharExistsL(FDelimiters, Ch)) do begin
      Dec(FCursor);
      Ch := FCursor^;
    end;
    while (FCursor <> FString) and (not CharExistsL(FDelimiters, Ch)) do begin
      Dec(FCursor);
      Ch := FCursor^;
    end;
  end;
  {now get off the delimiter }
  if (FCursor <> FString) then
    Inc(FCursor);
end;

procedure TStString.CursorToEnd;
{- Set cursor to null terminator at the end of string. }
begin
  FCursor := StrEnd(FString);
end;

procedure TStString.DeleteAsciiAtCursor;
{- Deletes word (and any trailing delimiters) at cursor following ASCII rules. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    StrStDeleteL(FCursor, 0, SizeAsciiAtCursor(True));
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.DeleteAtCursor(Length : Cardinal);
{- Deletes Length number of characters at cursor. }
begin
  StrStDeleteL(FCursor, 0, Length);
end;

procedure TStString.DeleteWordAtCursor;
{- Deletes word (and any trailing delimiters) at cursor. }
var
  I : Cardinal;
begin
  for I := 1 to FRepeatValue do begin
    StrStDeleteL(FCursor, 0, SizeWordAtCursor(True));
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;

procedure TStString.Detab;
{- Detabs string. }
begin
  AllocTemp(SuggestSize(FAlloc + (CharCountL(FString, #9)*FTabSize)));
  if Assigned(FTemp) then begin
    FTemp := PChar(DetabL(FString, FTabSize));
    TempToString;
  end;
  ResetCursor;
end;

procedure TStString.Entab;
{- Entabs string. }
begin
  AllocTemp(FAlloc);
  if Assigned(FTemp) then begin
    FTemp := PChar(EntabL(FString, FTabSize));
    TempToString;
  end;
  ResetCursor;
end;

function TStString.ExtractAscii(N : Cardinal) : string;
{- Extracts the N'th word in string. }
var
  OldCursor : PChar;
  Position : Cardinal;
begin
  Result := '';
  OldCursor := FCursor;
  if AsciiPositionL(N, FString, FDelimiters, FQuote, Position) then
    FCursor := FString + Position
  else Exit;
  Result := GetAsciiAtCursor;
  FCursor := OldCursor;
end;

function TStString.ExtractWord(N : Cardinal) : string;
{- Extracts the N'th word in string. }
var
  OldCursor : PChar;
  Position : Cardinal;
begin
  Result := '';
  OldCursor := FCursor;
  if WordPositionL(N, FString, FDelimiters, Position) then
    FCursor := FString + Position
  else Exit;
  Result := GetWordAtCursor;
  FCursor := OldCursor;
end;

procedure TStString.Filter(const Filters : PChar);
{- Filters characters from string. }
begin
  AllocTemp(FAlloc);
  FTemp := PChar(FilterL(DesiredCursor, Filters));
  TempToString;
  ResetCursor;
end;

procedure TStString.FixCursor(Pos, Size : Cardinal; Delete : Boolean);
{- Fixes cursor position following an insertion or deletion. }
begin
  if (FCursor - FString) < Integer(Pos) then
    Exit;
  if Delete then begin
    FCursor := FCursor - Size;
    if (FCursor - FString) < Integer(Pos) then
      FCursor := FString + Pos;
  end else begin
    if (FCursor - FString) = Integer(Pos) then
      Exit;
    FCursor := FCursor + Size;
  end;
end;

function TStString.MakeLetterSet : Integer;
{- Performs MakeLetterSetZ on the word at the Cursor. }
var
  Temp : PChar;
begin
  Temp := StrAlloc(Succ(SizeWordAtCursor(False)));
  try
    GetWordAtCursorZ(Temp);
    Result := MakeLetterSetL(Temp);
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.MoveCursor(Delta : Integer);
{- Moves Cursor by Delta characters. }
begin
  Inc(FCursor, Delta);
  if FCursor < FString then
    FCursor := FString;
  if FCursor > (FString + Succ(GetLength)) then
    CursorToEnd;
end;

function TStString.GetAsciiAtCursor : string;
{- Gets the word that the Cursor is pointing to -- returns string. }
var
  I, Size : Integer;
begin
  Size := SizeAsciiAtCursor(False);
  SetLength(Result, Size);
  for I := 0 to Pred(Size) do
    Result[Succ(I)] := FCursor[I];
end;

function TStString.GetAsciiAtCursorZ(Dest : PChar) : PChar;
{- Gets the word that the Cursor is pointing to -- returns PChar. }
var
  I, Size : Integer;
begin
  Size := SizeAsciiAtCursor(False);
  Result := Dest;
  for I := 0 to Pred(Size) do
    Dest[I] := FCursor[I];
  Dest[Size] := #0;
end;

function TStString.GetAsPChar(Dest : PChar) : PChar;
{- Exports string as a null-terminated string. }
begin
  Result := Dest;
  StrCopy(Result, FString);
end;

function TStString.GetWordAtCursor : string;
{- Gets the word that the Cursor is pointing to -- returns string. }
var
  I, Size : Integer;
begin
  Size := SizeWordAtCursor(False);
  SetLength(Result, Size);
  for I := 0 to Pred(Size) do
    Result[Succ(I)] := FCursor[I];
end;

function TStString.GetWordAtCursorZ(Dest : PChar) : PChar;
{- Gets the word that the Cursor is pointing to -- returns PChar. }
var
  I, Size : Integer;
begin
  Size := SizeWordAtCursor(False);
  Result := Dest;
  for I := 0 to Pred(Size) do
    Dest[I] := FCursor[I];
  Dest[Size] := #0;
end;

procedure TStString.Pack;
{- Sets string allocation to minimum size. }
var
  StrLen : Cardinal;
begin
  StrLen := GetLength;
  if SuggestSize(StrLen) < FAlloc then
    SetAllocLength(StrLen);
end;

procedure TStString.Pad(Size : Cardinal);
{- Pads string. }
begin
  CheckAlloc(Size);
  FString := PChar(PadL(FString, Size));
  ResetCursor;
end;


function TStString.MakeTerminator(var Terminator : PChar) : Integer; {!!.01}
begin
  Result := 0;
  case self.LineTerminator of
     ltNone :;
     ltCR, ltLF, ltOther :begin
       Result := 2;
       GetMem(Terminator, Result * SizeOf(Char));
       case LineTerminator of
         ltCR    : StrCopy(Terminator, #13);
         ltLF    : StrCopy(Terminator, #10);
         ltOther : begin
           Terminator[0] := FLineTermChar;
           Terminator[1] := #0;
         end;
       end;
     end;
     ltCRLF : begin
       Result := 3;
       GetMem(Terminator, Result * SizeOf(Char));
       StrCopy(Terminator, #13#10);
     end;
  end;
end;

procedure TStString.InsertLineTerminatorAtCursor;
{- Inserts line termintor at cursor position. }
var
  Pos : Cardinal;
  Terminator : PChar;
  TermSiz : Integer;
begin
  Terminator := nil;
  TermSiz := MakeTerminator(Terminator);
  CheckAlloc(GetLength + 2);
  Pos := FCursor - FString;
  FString := PChar(StrStInsertL(FString, Terminator, Pos));
  FreeMem(Terminator, TermSiz * SizeOf(Char));
end;

procedure TStString.InsertLineTerminator(Pos : Cardinal);
{- Inserts line terminator at given position. }
var
  AdjPos : Cardinal;
  Terminator : PChar;
  TermSiz : Integer;
begin
  Terminator := nil;
  TermSiz := MakeTerminator(Terminator);
  CheckAlloc(GetLength + 2);
  AdjPos := Pos;
  if FOneBased then Dec(AdjPos);
  FString := PChar(StrStInsertL(FString, Terminator, AdjPos));
  FreeMem(Terminator, TermSiz * SizeOf(Char));
end;


procedure TStString.InsertPCharAtCursor(S : PChar);
{- Inserts null-terminated string at cursor position. }
var
  Len, Pos : Cardinal;
begin
  Len := StrLen(S);
  Pos := FCursor - FString;
  CheckAlloc(GetLength + Len);
  FString := PChar(StrStInsertL(FString, S, Pos));
end;

procedure TStString.InsertStringAtCursor(S : string);
{- Inserts string at cursor position. }
var
  Pos, Len : Cardinal;
  Temp : PChar;
begin
  Pos := FCursor - FString;
  Len := System.Length(S);
  Temp := StrAlloc(Succ(Len));
  try
    StrPCopy(Temp, S);
    CheckAlloc(GetLength + Len);
    FString := PChar(StrStInsertL(FString, Temp, Pos));
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.ItemsToString;
{- Copies items strings to string. }
var                                                                    
  Temp : string;
begin
  Temp := FItems.Text;
  SetAsPChar(PChar(string(Temp)));
end;

procedure TStString.LeftPad(Size : Cardinal);
{- Left pad string. }
begin
  CheckAlloc(Size);
  FString := PChar(LeftPadL(FString, Size));
  ResetCursor;
end;

procedure TStString.LeftPadCh(const C : Char; Size : Cardinal);
{- Left pad string with C. }
begin
  CheckAlloc(Size);
  FString := PChar(LeftPadChL(FString, C, Size));
  ResetCursor;
end;

procedure TStString.PadCh(const C : Char; Size : Cardinal);
{- Pad string with C. }
begin
  CheckAlloc(Size);
  FString := PChar(PadChL(FString, C, Size));
  ResetCursor;
end;

procedure TStString.ResetCursor;
{- Resets Cursor to beginning of string. }
begin
  FCursor := FString;
end;

procedure TStString.Scramble(const Key : string);
{- Encrypts / Decrypts string. }
var
  Temp : PChar;
begin
  Temp := StrAlloc(Succ(System.Length(Key)));
  try
    StrPCopy(Temp, Key);
    FString := PChar(ScrambleL(FString, Temp));
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.SetAsPChar(S : PChar);
{- Sets string to PChar. }
begin
  CheckAlloc(StrLen(S));
  StrCopy(FString, S);
  ResetCursor;
end;

function TStString.SizeAsciiAtCursor(InclTrailers : Boolean) : Cardinal;
{- Get the size of the word that the Cursor on (follows ASCII rules). }
var
  TempPtr : PChar;
  Ch : Char;
  InQuote : Boolean;
begin
  InQuote := False;
  TempPtr := FCursor;
  Ch := TempPtr^;
  while (Ch <> #0) and ((InQuote) or (not CharExistsL(FDelimiters, Ch))) do begin
    if Ch = FQuote then
      InQuote := not InQuote;
    Inc(TempPtr);
    Ch := TempPtr^;
  end;
  if InclTrailers then begin
    while (Ch <> #0) and CharExistsL(FDelimiters, Ch) do begin
      Inc(TempPtr);
      Ch := TempPtr^;
    end;
  end;
  Result := TempPtr - FCursor;
end;

function TStString.SizeWordAtCursor(InclTrailers : Boolean) : Cardinal;
{- Get the size of the word that the Cursor is pointing to. }
var
  TempPtr : PChar;
  Ch : Char;
begin
  TempPtr := FCursor;
  Ch := TempPtr^;
  while (Ch <> #0) and (not CharExistsL(FDelimiters, Ch)) do begin
    Inc(TempPtr);
    Ch := TempPtr^;
  end;
  if InclTrailers then begin
    while (Ch <> #0) and CharExistsL(FDelimiters, Ch) do begin
      Inc(TempPtr);
      Ch := TempPtr^;
    end;
  end;
  Result := TempPtr - FCursor;
end;

procedure TStString.StrChDelete(Pos : Cardinal);
{- Delete character at Pos. }
var
  AdjPos : Cardinal;
begin
  AdjPos := Pos;
  if FOneBased then Dec(AdjPos);
  FString := PChar(StrChDeleteL(FString, AdjPos));
  FixCursor(AdjPos, 1, True);
end;

procedure TStString.StrChInsert(const C : Char; Pos : Cardinal);
{- Insert character at Pos. }
var
  AdjPos : Cardinal;
begin
  AdjPos := Pos;
  CheckAlloc(Succ(GetLength));
  if FOneBased then Dec(AdjPos);
  FString := PChar(StrChInsertL(FString, C, AdjPos));
  FixCursor(AdjPos, 1, False);
end;

function TStString.StrChPos(const C : Char; var Pos : Integer) : Boolean;
{- Search string for character. }
var
  I : Cardinal;
begin
  Result := False;
  ClearItems;
  for I := 1 to FRepeatValue do begin
    if StrChPosL(DesiredCursor, C, Pos) then begin
      Result := True;
      Pos := GetRelativePos(Pos);
      UpdateCursor(Pos);
      if FOneBased then Inc(Pos);
      AddIntToList(Pos);
      Inc(FCursor);
    end else Break;                                                    
  end;
  if Result then Dec(FCursor);                                         
end;


procedure TStString.StripLineTerminators;
{- Strip all line terminators from string, replacing them with a space. }
var
  Terminator : PChar;
  TermSiz : Integer;
begin
  Terminator := nil;
  if self.FLineTerminator = ltCRLF then begin
    Filter(#13);
    Substitute(#10, ' ');
  end
  else begin
    TermSiz := MakeTerminator(Terminator);
    Substitute(Terminator, ' ');
    FreeMem(Terminator, TermSiz * SizeOf(Char));
  end;
  ResetCursor;
end;


procedure TStString.StrStDelete(const Pos, Length : Cardinal);
{- Delete substring from string at Pos. }
var
  AdjPos : Cardinal;
begin
  AdjPos := Pos;
  if FOneBased then Dec(AdjPos);
  FString := PChar(StrStDeleteL(FString, AdjPos, Length));
  FixCursor(AdjPos, Length, True);
end;

procedure TStString.StrStInsert(const S : string; Pos : Cardinal);
{- Insert string at Pos. }
var
  AdjPos, Len : Cardinal;
  Temp : PChar;
begin
  AdjPos := Pos;
  Len := System.Length(S);
  Temp := StrAlloc(Succ(Len));
  try
    StrPCopy(Temp, S);
    if FOneBased then Dec(AdjPos);
    CheckAlloc(GetLength + Len);
    FString := PChar(StrStInsertL(FString, Temp, AdjPos));
  finally
    StrDispose(Temp);
  end;
  FixCursor(AdjPos, Len, False);
end;

function TStString.StrStPos(const S : string; var Pos : Cardinal) : Boolean;
{- Search for substring in string. }
var
  I : Cardinal;
  Temp : PChar;
begin
  Result := False;
  ClearItems;
  Temp := StrAlloc(Succ(System.Length(S)));
  try
    StrPCopy(Temp, S);
    for I := 1 to FRepeatValue do begin
      if StrStPosL(DesiredCursor, Temp, Pos) then begin
        Result := True;
        Pos := GetRelativePos(Pos);
        UpdateCursor(Pos);
        if FOneBased then inc(Pos);
        AddIntToList(Pos);
        Inc(FCursor);
      end else Break;                                                  
    end;
    if Result then Dec(FCursor);
  finally
    StrDispose(Temp);
  end;
end;

procedure TStString.Substitute(FromStr, ToStr : PChar);
{- Substitute characters in string. }
var
  CursorDelta : Cardinal;
begin
  AllocTemp(FAlloc);
  CursorDelta := FCursor - FString;
  if Assigned(FTemp) then begin
    FTemp := PChar(SubstituteL(FString, FromStr, ToStr));
    TempToString;
  end;
  FCursor := FString + CursorDelta;
end;

procedure TStString.Trim;
{- Trim string. }
begin
  FString := PChar(TrimL(FString));
  ResetCursor;
end;

procedure TStString.TrimLead;
{- Trim leading whitespace from string. }
begin
  FString := PChar(TrimLeadL(FString));
  ResetCursor;
end;

procedure TStString.TrimSpaces;
{- Trim spaces from string. }
begin
  FString := PChar(TrimSpacesL(FString));
  ResetCursor;
end;

procedure TStString.TrimTrail;
{- Trim trailing whitespace from string. }
begin
  FString := PChar(TrimTrailL(FString));
  ResetCursor;
end;

function TStString.WordPosition(N : Cardinal; var Pos : Cardinal) : Boolean;
{- Return the position of the N'th word. }
var
  I, Temp, Num : Cardinal;
begin
  Result := False;
  Num := N;
  ClearItems;
  for I := 1 to FRepeatValue do begin
    if WordPositionL(Num, DesiredCursor, FDelimiters, Temp) then begin
      if Result = False then Inc(Num);
      Pos := GetRelativePos(Temp);
      Result := True;
      UpdateCursor(Pos);
      if FOneBased then Inc(Pos);
      AddIntToList(Pos);
    end else                                                           
      Break;
  end;
  if FResetRepeat then FRepeatValue := DefRepeatValue;
end;


procedure TStString.WrapToItems;
{- Copy string to items with word wrap. }
var
  I, J : Cardinal;
  Anchor, Cur, EndTemp : PChar;
  InWord, EndFound : Boolean;

  Terminator, TermPlusSpace : PChar;
  TermSiz : Integer;
begin
  Terminator := nil;
  TermSiz := MakeTerminator(Terminator);
  GetMem(TermPlusSpace, (TermSiz + 1) * SizeOf(Char));
  StrCopy(TermPlusSpace, Terminator);
  StrCat(TermPlusSpace, ' ');

  if GetLength > FWrap then begin
    EndFound := False;
    AllocTemp(SuggestSize(GetLength + (GetLength div FWrap * 2)));
    FTemp^ := #0;
    Anchor := FString;
    Cur := FString;
    repeat
      I := 0;
      J := 0;
      InWord := False;
      while (Cur^ <> #0) and (I < Succ(FWrap)) do begin
//        if CharExistsL(' '#13#10, Cur^) then begin
        if CharExistsL(TermPlusSpace, Cur^) then begin
          if InWord then begin
            InWord := False;
            J := I;
          end;
          if Cur^ <> ' ' then Break;
        end else begin
          InWord := True;
        end;
        Inc(I);
        Inc(Cur);
      end;
      if Cur^ = #0 then begin
        EndFound := True;
        J := I;
      end;
      EndTemp := StrEnd(FTemp);
      if InWord and (J = 0) then
        J := FWrap;
      StrLCopy(EndTemp, Anchor, J);
      if not EndFound then begin
//        StrCat(FTemp, #13#10);
        StrCat(FTemp, Terminator);
        Anchor := Anchor + J;
        while Anchor^ = ' ' do
          Inc(Anchor);
        if FLineTerminator = ltCRLF then begin
          if Anchor^ = #13 then Inc(Anchor);
          if Anchor^ = #10 then Inc(Anchor);
        end else begin
          if Anchor^ = Terminator[0] then Inc(Anchor);
        end;
        Cur := Anchor;
      end;
    until EndFound;
    FItems.Text := string(string(FTemp));
    StrDispose(FTemp);
  end else begin
    StringToItems;
  end;
  FreeMem(Terminator, TermSiz * SizeOf(Char));
  FreeMem(TermPlusSpace, (TermSiz + 1) *  SizeOf(Char));
end;


function TStString.DesiredCursor : PChar;
{- Returns FString or FCursor. }
begin
  if FEnableCursor then
    Result := FCursor
  else
    Result := FString;
end;

function TStString.Get(Index : Cardinal) : Char;
{- Get character from position Index within string. }
begin
  if FOneBased then begin
    if (Index = 0) or (Index > GetLength) then
      RaiseStError(EStStringError, stscOutOfBounds);
    Result := FString[Index - 1]
  end else begin
    if Index > (GetLength-1) then
      RaiseStError(EStStringError, stscOutOfBounds);
    Result := FString[Index];
  end;
end;

function TStString.GetAsciiCount : Cardinal;
{- Count words following ASCII rules. }
begin
  Result := AsciiCountL(FString, FDelimiters, FQuote);
end;

function TStString.GetAsShortStr : string;
{- Provide short string output. }
begin
  Result := StrPas(FString);
end;

function TStString.GetCursorPos : Cardinal;
{- Return the position of the Cursor relative to the beginning of the string. }
begin
  Result := FCursor - FString;
  if FOneBased then Inc(Result);
end;

function TStString.GetDelimiters : string;
{- Return string with current delimiters. }
begin
  Result := StrPas(FDelimiters);
end;

function TStString.GetLength : Cardinal;
{- Return the length of the string. }
begin
  if Assigned(FString) then
    Result := StrLen(FString)
  else
    Result := 0;
end;

function TStString.GetRelativePos(Pos : Cardinal) : Cardinal;
{- Return position relative to FString. }
begin
  if FEnableCursor then
    Result := Pos + FCursor - FString
  else
    Result := Pos;
end;

function TStString.GetSoundex : string;
{- Return Soundex for word at Cursor. }
var
  I : Integer;
  Temp, Dest : PChar;
begin
  ClearItems;
  Dest := StrAlloc(5);
  try
    for I := 1 to FRepeatValue do begin
      if FCursor^ = #0 then Exit;
      Temp := StrAlloc(Succ(SizeWordAtCursor(False)));
      try
        GetWordAtCursorZ(Temp);
        Result := StrUtils.Soundex(Temp);
        FItems.Add(string(Result));
      finally
        StrDispose(Temp);
      end;
      if FRepeatValue > 1 then CursorNextWordPrim;
    end;
  finally
    StrDispose(Dest);
  end;
end;

function TStString.GetWordCount : Cardinal;
{- Count words in string. }
begin
  Result := WordCountL(FString, FDelimiters);
end;

procedure TStString.Put(Index : Cardinal; Item : Char);
{- Put character at position Index within string. }
begin
  if FOneBased then begin
    if (Index = 0) or (Index > GetLength) then
      RaiseStError(EStStringError, stscOutOfBounds);
    FString[Index - 1] := Item;
  end else begin
    if Index > (GetLength-1) then
      RaiseStError(EStStringError, stscOutOfBounds);
    FString[Index] := Item;
  end;
end;

procedure TStString.SetAllocLength(Value : Cardinal);
{- Sets allocated length for string - including the terminating null. }
begin
  if Value <> FAlloc then begin
    AllocTemp(SuggestSize(Value));
    if Assigned(FTemp) then begin
      if Assigned(FString) then begin
        StrLCopy(FTemp, FString, Value);
      end;
      TempToString;
    end;
  end;
end;

procedure TStString.SetAsShortStr(const Value : string);
{- Copy short string into string object. }
begin
  FString := PChar(Value);
  ResetCursor;
end;

procedure TStString.SetCursorPos(Value : Cardinal);
{- Sets the position of the cursor. }
begin
  FCursor := FString + Value;
  if FOneBased then Dec(FCursor);
end;

procedure TStString.SetDelimiters(Value : string);
{- Set the delimiters. }
begin
  StrDispose(FDelimiters);
  FDelimiters := StrAlloc(Succ(System.Length(Value)));
  if Assigned(FDelimiters) then
    StrPCopy(FDelimiters, Value);
end;

procedure TStString.SetItems(Value: TStringList);
{- Sets Items. }
begin
  FItems.Assign(Value);
end;

procedure TStString.StringToItems;
{- Copies string into items -- respects line terminators. }
begin
  FItems.Text := string(string(FString));
end;

function TStString.SuggestSize(Size : Cardinal) : Cardinal;
{- Internal method -- returns recommended size for allocation. }
var
  AdjSize, Delta : Cardinal;
begin
  AdjSize := Succ(Size);
  Delta := AdjSize mod DefAllocSize;
  Result := AdjSize - Delta + DefAllocSize;
end;

procedure TStString.TempToString;
{- Internal method -- copys temp to string. }
begin
  FAlloc := FTempAlloc;
  FCursor := (FCursor - FString) + FTemp;
  StrDispose(FString);
  FString := FTemp;
  FTemp := nil;
end;

procedure TStString.UpdateCursor(Pos : Cardinal);
{- Internal method -- updates cursor position if necessary. }
begin
  if EnableCursor then
    FCursor := FString + Pos;
end;

function TStString.GetAsLongStr : string;
{- Provide output as long string. }
begin
  Result := FString;
end;

procedure TStString.SetAsLongStr(Value : string);
{- Copy long string into string object. }
begin
  CheckAlloc(System.Length(Value));
  StrCopy(FString, PChar(Value));
  ResetCursor;
end;

function TStString.GetAsVariant : Variant;
{- Provide output as variant. }
begin
  Result := StrPas(FString);
end;

procedure TStString.SetAsVariant(Value : Variant);
{- Copy variant into string object. }
var
  Temp : string;
begin
  Temp := string(Value);
  CheckAlloc(System.Length(Temp));
  StrCopy(FString, PChar(Temp));
  ResetCursor;
end;


procedure TStString.SetLineTerm(const Value: TStLineTerminator);
begin
  FLineTerminator := Value;
end;

procedure TStString.SetLineTermChar(const Value: Char);
begin
  FLineTermChar := Value;
end;


end.
