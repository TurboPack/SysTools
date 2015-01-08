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
{* SysTools: StStrW.pas 4.04                             *}
{*********************************************************}
{* SysTools: Wide (Unicode) string routines              *}
{*********************************************************}

{$I StDefine.inc}

unit StStrW;

interface

uses
  Windows,
  Classes,
  SysUtils,
  StConst,
  StBase;


{.Z+}
type
  WStrRec = packed record
    Length    : Longint;
  end;

const
  StrOffset = SizeOf(WStrRec);
{.Z-}

  {-------- Numeric conversion -----------}

function HexBW(B : Byte) : WideString;
  {-Return the hex string for a byte.}

function HexWW(W : Word) : WideString;
  {-Return the hex string for a word.}

function HexLW(L : LongInt) : WideString;
  {-Return the hex string for a long integer.}

function HexPtrW(P : Pointer) : WideString;
  {-Return the hex string for a pointer.}

function BinaryBW(B : Byte) : WideString;
  {-Return a binary string for a byte.}

function BinaryWW(W : Word) : WideString;
  {-Return the binary string for a word.}

function BinaryLW(L : LongInt) : WideString;
  {-Return the binary string for a long integer.}

function OctalBW(B : Byte) : WideString;
  {-Return an octal string for a byte.}

function OctalWW(W : Word) : WideString;
  {-Return an octal string for a word.}

function OctalLW(L : LongInt) : WideString;
  {-Return an octal string for a long integer.}

function Str2Int16W(const S : WideString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

function Str2WordW(const S : WideString; var I : Word) : Boolean;
  {-Convert a string to a word.}

function Str2LongW(const S : WideString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

function Str2RealW(const S : WideString; var R : Double) : Boolean;
  {-Convert a string to a real.}

function Str2ExtW(const S : WideString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}

function Long2StrW(L : LongInt) : WideString;
  {-Convert an integer type to a string.}

function Real2StrW(R : Double; Width : Byte; Places : ShortInt) : WideString;
  {-Convert a real to a string.}

function Ext2StrW(R : Extended; Width : Byte; Places : ShortInt) : WideString;
  {-Convert an extended to a string.}

function ValPrepW(const S : WideString) : WideString;
  {-Prepares a string for calling Val.}

  {-------- General purpose string manipulation --------}

function CharStrW(C : WideChar; Len : Cardinal) : WideString;
  {-Return a string filled with the specified character.}

function PadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the right with a specified character.}

function PadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the right with spaces.}

function LeftPadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left with a specified character.}

function LeftPadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left with spaces.}

function TrimLeadW(const S : WideString) : WideString;
  {-Return a string with leading white space removed.}

function TrimTrailW(const S : WideString) : WideString;
  {-Return a string with trailing white space removed.}

function TrimW(const S : WideString) : WideString;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesW(const S : WideString) : WideString;
  {-Return a string with leading and trailing spaces removed.}

function CenterChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with a specified character.}

function CenterW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with spaces.}


function EntabW(const S : WideString; TabSize : Byte) : WideString;
  {-Convert blanks in a string to tabs.}

function DetabW(const S : WideString; TabSize : Byte) : WideString;
  {-Expand tabs in a string to blanks.}


function ScrambleW(const S, Key : WideString) : WideString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteW(const S, FromStr, ToStr : WideString) : WideString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterW(const S, Filters : WideString) : WideString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}

  {--------------- Word / Char manipulation -------------------------}

function CharExistsW(const S : WideString; C : WideChar) : Boolean;
  {-Determine whether a given character exists in a string. }

function CharCountW(const S : WideString; C : WideChar) : Cardinal;
  {-Count the number of a given character in a string. }

function WordCountW(const S, WordDelims : WideString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}

function WordPositionW(N : Cardinal; const S, WordDelims : WideString;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordW(N : Cardinal; const S, WordDelims : WideString) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountW(const S, WordDelims : WideString; Quote : WideChar) : Cardinal;
  {-Return the number of words in a string.}

function AsciiPositionW(N : Cardinal; const S, WordDelims : WideString;
                       Quote : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}

function ExtractAsciiW(N : Cardinal; const S, WordDelims : WideString;
                       Quote : WideChar) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapW(const InSt : WideString; var OutSt, Overlap : WideString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

  {--------------- String comparison and searching -----------------}
function CompStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings.}

function CompUCStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionW(const Name, Ext : WideString) : WideString;
  {-Return a file name with a default extension attached.}

function ForceExtensionW(const Name, Ext : WideString) : WideString;
  {-Force the specified extension onto the file name.}

function JustFilenameW(const PathName : WideString) : WideString;
  {-Return just the filename and extension of a pathname.}

function JustNameW(const PathName : WideString) : WideString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionW(const Name : WideString) : WideString;
  {-Return just the extension of a pathname.}

function JustPathnameW(const PathName : WideString) : WideString;
  {-Return just the drive and directory portion of a pathname.}

function AddBackSlashW(const DirName : WideString) : WideString;
  {-Add a default backslash to a directory name.}

function CleanPathNameW(const PathName : WideString) : WideString;
  {-Return a pathname cleaned up as DOS does it.}

function HasExtensionW(const Name : WideString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

  {------------------ Formatting routines --------------------}

function CommaizeW(L : LongInt) : WideString;
  {-Convert a long integer to a string with commas.}

function CommaizeChW(L : Longint; Ch : WideChar) : WideString;
  {-Convert a long integer to a string with Ch in comma positions.}

function FloatFormW(const Mask : WideString ; R : TstFloat ; const LtCurr,
                    RtCurr : WideString ; Sep, DecPt : WideChar) : WideString;
  {-Return a formatted string with digits from R merged into mask.}

function LongIntFormW(const Mask : WideString ; L : LongInt ; const LtCurr,
                      RtCurr : WideString ; Sep : WideChar) : WideString;
  {-Return a formatted string with digits from L merged into mask.}

function StrChPosW(const P : WideString; C : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}

function StrStPosW(const P, S : WideString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Copy characters at a specified position in a string.}

function StrChInsertW(const S : WideString; C : WideChar; Pos : Cardinal) : WideString;
  {-Insert a character into a string at a specified position.}

function StrStInsertW(const S1, S2 : WideString; Pos : Cardinal) : WideString;
  {-Insert a string into another string at a specified position.}

function StrChDeleteW(const S : WideString; Pos : Cardinal) : WideString;
  {-Delete the character at a specified position in a string.}

function StrStDeleteW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Delete characters at a specified position in a string.}

{--------------------------  New Functions -----------------------------------}

function ContainsOnlyW(const S, Chars : WideString;
                       var BadPos : Cardinal) : Boolean;

function ContainsOtherThanW(const S, Chars : WideString;
                            var BadPos : Cardinal) : Boolean;

function CopyFromNthWordW(const S, WordDelims : WideString;
                         AWord : WideString; N : Cardinal;
                         var SubString : WideString) : Boolean;

function CopyFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                         N1, N2 : Cardinal;
                         var SubString : WideString) : Boolean;

function CopyLeftW(const S : WideString; Len : Cardinal) : WideString;
  {-Return the left Len characters of a string}

function CopyMidW(const S : WideString; First, Len : Cardinal) : WideString;
  {-Return the mid part of a string}

function CopyRightW(const S : WideString; First : Cardinal) : WideString;
  {-Return the right Len characters of a string}

function CopyRightAbsW(const S : WideString; NumChars : Cardinal) : WideString;
  {-Return NumChar characters starting from end}

function CopyWithinW(const S, Delimiter : WideString;
                     Strip : Boolean) : WideString;

function DeleteFromNthWordW(const S, WordDelims : WideString;
                            AWord : WideString; N : Cardinal;
                            var SubString : WideString) : Boolean;


function DeleteFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                           N1, N2 : Cardinal;
                           var SubString : WideString) : Boolean;

function DeleteWithinW(const S, Delimiter : WideString) : WideString;

function ExtractTokensW(const S, Delims  : WideString;
                              QuoteChar  : WideChar;
                              AllowNulls : Boolean;
                              Tokens     : TStrings) : Cardinal;

function IsChAlphaW(C : WideChar) : Boolean;
 {-Returns true if Ch is an alpha}

function IsChNumericW(C : WideChar; Numbers : WideString) : Boolean;
 {-Returns true if Ch in numeric set}

function IsChAlphaNumericW(C : WideChar; Numbers : WideString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}

function IsStrAlphaW(const S : WideString) : Boolean;
  {-Returns true if all characters in string are an alpha}

function IsStrNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are in numeric set}

function IsStrAlphaNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}

function KeepCharsW(const S, Chars : WideString) : WideString;

function LastStringW(const S, AString : WideString;
                     var Position : Cardinal) : Boolean;

function LastWordW(const S, WordDelims, AWord : WideString;
                   var Position : Cardinal) : Boolean;

function LastWordAbsW(const S, WordDelims : WideString;
                        var Position : Cardinal) : Boolean;

function LeftTrimCharsW(const S, Chars : WideString) : WideString;

function ReplaceWordW(const S, WordDelims, OldWord, NewWord : WideString;
                      N : Cardinal;
                      var Replacements : Cardinal) : WideString;

function ReplaceWordAllW(const S, WordDelims, OldWord, NewWord : WideString;
                         var Replacements : Cardinal) : WideString;

function ReplaceStringW(const S, OldString, NewString : WideString;
                        N : Cardinal;
                        var Replacements : Cardinal) : WideString;

function ReplaceStringAllW(const S, OldString, NewString : WideString;
                           var Replacements : Cardinal) : WideString;


function RepeatStringW(const RepeatString : WideString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : WideString;

function RightTrimCharsW(const S, Chars : WideString) : WideString;

function StrWithinW(const S, SearchStr : WideString;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
  {-finds the position of a substring within a string starting at a given point}

function TrimCharsW(const S, Chars : WideString) : WideString;

function WordPosW(const S, WordDelims, AWord : WideString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a word within a string}


implementation

uses
  StUtils;

  {-------- Numeric conversion -----------}

function HexBW(B : Byte) : WideString;
  {-Return the hex string for a byte.}
begin
  SetLength(Result, 2);
  Result[1] := WideChar(StHexDigits[B shr 4]);
  Result[2] := WideChar(StHexDigits[B and $F]);
end;

function HexWW(W : Word) : WideString;
  {-Return the hex string for a word.}
begin
  SetLength(Result, 4);
  Result[1] := WideChar(StHexDigits[hi(W) shr 4]);
  Result[2] := WideChar(StHexDigits[hi(W) and $F]);
  Result[3] := WideChar(StHexDigits[lo(W) shr 4]);
  Result[4] := WideChar(StHexDigits[lo(W) and $F]);
end;

function HexLW(L : LongInt) : WideString;
  {-Return the hex string for a long integer.}
begin
  SetLength(Result, 8);
  Result := HexWW(HiWord(DWORD(L))) + HexWW(LoWord(DWORD(L)));         {!!.02}
end;

function HexPtrW(P : Pointer) : WideString;
  {-Return the hex string for a pointer.}
begin
  SetLength(Result, 9);
  Result := ':' + HexLW(LongInt(P));
end;

function BinaryBW(B : Byte) : WideString;
  {-Return a binary string for a byte.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 8);
  for I := 7 downto 0 do begin
    Result[N] := WideChar(StHexDigits[Ord(B and (1 shl I) <> 0)]); {0 or 1}
    Inc(N);
  end;
end;

function BinaryWW(W : Word) : WideString;
  {-Return the binary string for a word.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 16);
  for I := 15 downto 0 do begin
    Result[N] := WideChar(StHexDigits[Ord(W and (1 shl I) <> 0)]); {0 or 1}
    Inc(N);
  end;
end;

function BinaryLW(L : LongInt) : WideString;
  {-Return the binary string for a long integer.}
var
  I : Longint;
  N : Byte;
begin
  N := 1;
  SetLength(Result, 32);
  for I := 31 downto 0 do begin
    Result[N] := WideChar(StHexDigits[Ord(L and LongInt(1 shl I) <> 0)]); {0 or 1}
    Inc(N);
  end;
end;

function OctalBW(B : Byte) : WideString;
  {-Return an octal string for a byte.}
var
  I : Word;
begin
  SetLength(Result, 3);
  for I := 0 to 2 do begin
    Result[3-I] := WideChar(StHexDigits[B and 7]);
    B := B shr 3;
  end;
end;

function OctalWW(W : Word) : WideString;
  {-Return an octal string for a word.}
var
  I : Word;
begin
  SetLength(Result, 6);
  for I := 0 to 5 do begin
    Result[6-I] := WideChar(StHexDigits[W and 7]);
    W := W shr 3;
  end;
end;

function OctalLW(L : LongInt) : WideString;
  {-Return an octal string for a long integer.}
var
  I : Word;
begin
  SetLength(Result, 12);
  for I := 0 to 11 do begin
    Result[12-I] := WideChar(StHexDigits[L and 7]);
    L := L shr 3;
  end;
end;

function Str2Int16W(const S : WideString; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

var
  ec : Integer;
begin
  if (length(S) > 255) then begin
    Result := false;
    I := 256;
  end
  else begin
    {note the automatic string conversion}
    ValSmallint(S, I, ec);
    if (ec = 0) then
      Result := true
    else begin
      Result := false;
      if (ec < 0) then
        I := succ(length(S))
      else
        I := ec;
    end;
  end;
end;

function Str2WordW(const S : WideString; var I : Word) : Boolean;
  {-Convert a string to a word.}

var
  ec : Integer;
begin
  if (length(S) > 255) then begin
    Result := false;
    I := 256;
  end
  else begin
    {note the automatic string conversion}
    ValWord(S, I, ec);
    if (ec = 0) then
      Result := true
    else begin
      Result := false;
      if (ec < 0) then
        I := succ(length(S))
      else
        I := ec;
    end;
  end;
end;

function Str2LongW(const S : WideString; var I : LongInt) : Boolean;
  {-Convert a string to a long integer.}

var
  ec : Integer;
begin
  if (length(S) > 255) then begin
    Result := false;
    I := 256;
  end
  else begin
    {note the automatic string conversion}
    ValLongint(S, I, ec);
    if (ec = 0) then
      Result := true
    else begin
      Result := false;
      if (ec < 0) then
        I := succ(length(S))
      else
        I := ec;
    end;
  end;
end;

function Str2RealW(const S : WideString; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : AnsiString;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailW(S);
  if St = '' then Exit;
  Val(ValPrepW(St), R, Code);
  if Code <> 0 then begin
    R := Code;
  end else
    Result := True;
end;

function Str2ExtW(const S : WideString; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P    : WideString;
begin
  Result := False;
  if S = '' then Exit;
  P := TrimTrailW(S);
  if P = '' then Exit;
  Val(ValPrepW(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
  end else
    Result := True;
end;

function Long2StrW(L : LongInt) : WideString;
  {-Convert an integer type to a string.}
begin
  Str(L, Result);
end;

function Real2StrW(R : Double; Width : Byte; Places : ShortInt) : WideString;
  {-Convert a real to a string.}
begin
  Str(R:Width:Places, Result);
end;

function Ext2StrW(R : Extended; Width : Byte; Places : ShortInt) : WideString;
  {-Convert an extended to a string.}
begin
  Str(R:Width:Places, Result);
end;

function ValPrepW(const S : WideString) : WideString;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
  C : Longint;
begin
  Result := TrimSpacesW(S);
  if Result <> '' then begin
    if StrChPosW(Result, WideChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator), P) then begin
      C := P;
      Result[C] := '.';
      if C = Length(Result) then
        SetLength(Result, Pred(C));
    end;
  end else
    Result := '0';
end;

  {-------- General purpose string manipulation --------}

function CharStrW(C : WideChar; Len : Cardinal) : WideString;
  {-Return a string filled with the specified character.}
var
  I : Longint;
begin
  SetLength(Result, Len);
  if Len <> 0 then begin
    {FillChar does not work for widestring}
    for I := 1 to Len do
      Result[I] := C;
  end;
end;

function PadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the right with a specified character.}
var
  J,
  R  : Longint;
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else begin
    SetLength(Result, Len);

    { copy current contents (if any) of S to Result }
    if (Length(S) > 0) and (Length(Result) > 0) then                     {!!.01}
      Move(S[1], Result[1], Length(S)*SizeOf(WideChar));                 {!!.01}

    R := longint(Len) - Length(S);
    J := Succ(Length(S));
    while (R > 0) do begin
      Result[J] := C;
      Inc(J);
      Dec(R);
    end;
  end;
end;

function PadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChW(S, ' ', Len);
end;

function LeftPadChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left with a specified character.}
var
  J,
  R  : Longint;
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    if (Length(S) > 0) and (Length(Result) > 0) then                     {!!.01}
      Move(S[1], Result[Succ(Word(Len))-Length(S)],                      {!!.01}
        Length(S)*SizeOf(WideChar));                                     {!!.01}
    R := longint(Len) - Length(S);
    J := 1;
    while (R > 0) do begin
      Result[J] := C;
      Inc(J);
      Dec(R);
    end;
  end;
end;

function LeftPadW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChW(S, ' ', Len);
end;

function TrimLeadW(const S : WideString) : WideString;
  {-Return a string with leading white space removed}
var
  I : Longint;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] <= ' ') do
    Inc(I);
  SetLength(Result, Length(S)-Pred(I));
  if Length(Result) > 0 then                                             {!!.01}
    Move(S[I], Result[1], Length(S)-Pred(I)*SizeOf(WideChar));           {!!.01}
end;

function TrimTrailW(const S : WideString) : WideString;
  {-Return a string with trailing white space removed.}
var
  L : Longint;
begin
  Result := S;
  L := Length(Result);
  while (L > 0) and (Result[L] <= ' ') do
    Dec(L);
  SetLength(Result, L);
end;

function TrimW(const S : WideString) : WideString;
  {-Return a string with leading and trailing white space removed.}
var
  I : Longint;
begin
  Result := S;
  I := Length(Result);
  while (I > 0) and (Result[I] <= ' ') do
    Dec(I);
  SetLength(Result, I);

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if (I > 0) then
    System.Delete(Result, 1, I);
end;

function TrimSpacesW(const S : WideString) : WideString;
  {-Return a string with leading and trailing spaces removed.}
var
  I : Longint;
begin
  Result := S;
  I := Length(Result);
  while (I > 0) and (Result[I] = ' ') do
    Dec(I);
  SetLength(Result, I);

  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if (I > 0) then
    System.Delete(Result, 1, I);
end;

function CenterChW(const S : WideString; C : WideChar; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= LongInt(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
    SetLength(Result, Len);
    Result := CharStrW(C, Len);
    if Length(S) > 0 then                                                {!!.01}
      Move(S[1], Result[Succ((LongInt(Len)-Length(S)) shr 1)], Length(S));
  end;
end;

function CenterW(const S : WideString; Len : Cardinal) : WideString;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChW(S, ' ', Len);
end;


function EntabW(const S : WideString; TabSize : Byte) : WideString;
  {-Convert blanks in a string to tabs.}
const
  WSpace = WideChar(#32);
  {$IFNDEF VERSION4}
  WTab   = string(WideChar(#9));
  {$ELSE}
  WTab   = WideChar(#9);
  {$ENDIF}
var
  Col,
  CP,
  OP,
  Spaces  : Longint;
begin
  if (pos(' ', S) = 0) then begin
    Result := S;
    Exit;
  end;
  Result := '';
  Col := 1;
  repeat
     CP := Col;
     while ((S[CP] <> WSpace) and (CP <= Length(S))) do
       Inc(CP);
     if (CP <> Col) then begin
       OP := Length(Result) + 1;
       SetLength(Result, Length(Result) + (CP-Col));
       Move(S[Col], Result[OP], ((CP-Col) * SizeOf(WideChar)));
       Col := CP;
     end;

     while (S[CP] = WSpace) do begin
       Inc(CP);
       if ((CP mod TabSize) = 1) then begin
         Result := Result + WTab;
         Col := CP;
       end;
     end;
     Spaces := 0;
     while (Col < CP) do begin
       Inc(Spaces);
       Inc(Col);
     end;
     if (Spaces > 0) then
       Result := Result + PadW(WSpace, Spaces);
  until (Col > Length(S));
end;


function DetabW(const S : WideString; TabSize : Byte) : WideString;
  {-Expand tabs in a string to blanks.}
var
  Col,
  CP,
  OP,
  Spaces  : Longint;
begin
  if S = '' then begin
    Result := '';
    Exit;
  end else if (TabSize = 0) then begin
    Result := S;
    Exit;
  end;
  if (CharCountW(S, WideChar(#9)) = 0) then begin
    Result := S;
    Exit;
  end;
  Result := '';

  Col := 1;
  while (Col <= Length(S)) do begin
    if (S[Col] = WideChar(#9)) then begin
      Spaces := 0;
      repeat
        Inc(Spaces);
      until (((Col + Spaces) mod TabSize) = 1);
      Inc(Col);
      Result := PadW(Result, Length(Result) + Spaces);
    end else begin
      CP := Col;
      repeat
        Inc(Col);
      until (Col > Length(S)) or (S[Col] = WideChar(#9));
      OP := Length(Result) + 1;
      SetLength(Result, Length(Result) + (Col - CP));
      Move(S[CP], Result[OP], (Col-CP)*SizeOf(WideChar));
    end;
  end;
end;


function ScrambleW(const S, Key : WideString) : WideString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
var
  I, J, LKey, LStr : Cardinal;
begin
  Result := S;
  if Key = '' then Exit;
  if S = '' then Exit;
  LKey := Length(Key);
  LStr := Length(S);
  I := 1;
  J := LKey;
  while I <= LStr do begin
    if J = 0 then
      J := LKey;
    if (S[I] <> Key[J]) then
      Result[I] := WideChar(Word(S[I]) xor Word(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

function SubstituteW(const S, FromStr, ToStr : WideString) : WideString;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  I : Cardinal;
  P : Cardinal;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      if StrChPosW(FromStr, S[I], P) then
        Result[I] := ToStr[P];
    end;
end;

function FilterW(const S, Filters : WideString) : WideString;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if not CharExistsW(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  SetLength(Result, Len);
end;

  {--------------- Word / Char manipulation -------------------------}

function CharExistsW(const S : WideString; C : WideChar) : Boolean;
  {-see if character exists at least once in a string}
var
  i : integer;
begin
  Result := true;
  for i := 1 to length(S) do
    if (S[i] = C) then
      Exit;
  Result := false;
end;

function CharCountW(const S : WideString; C : WideChar) : Cardinal;
  {-Count the number of a given character in a string. }
var
  i : integer;
begin
  Result := 0;
  for i := 1 to length(S) do
    if (S[i] = C) then
      inc(Result);
end;

function WordCountW(const S, WordDelims : WideString) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}
var
  I    : Cardinal;
  SLen : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);

  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and CharExistsW(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsW(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionW(N : Cardinal; const S, WordDelims : WideString;
                       var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  Count : Longint;
  I     : Longint;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and CharExistsW(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and not CharExistsW(WordDelims, S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordW(N : Cardinal; const S, WordDelims : WideString) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  C   : Cardinal;
  I,
  J   : Longint;
begin
  Result := '';
  if WordPositionW(N, S, WordDelims, C) then begin
    I := C;
    {find the end of the current word}
    J := I;
    while (I <= Length(S)) and not
           CharExistsW(WordDelims, S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], (I-J) * SizeOf(WideChar));
  end;
end;

function AsciiCountW(const S, WordDelims : WideString; Quote : WideChar) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Longint;
  InQuote : Boolean;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= Length(S) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote)
      and CharExistsW(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Result);
    {find the end of the current word}
    while (I <= Length(S)) and
          (InQuote or not CharExistsW(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionW(N : Cardinal; const S, WordDelims : WideString;
                        Quote : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  I,
  Count    : Longint;
  InQuote  : Boolean;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= Length(S)) and (Count <> LongInt(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote) and
          CharExistsW(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> LongInt(N) then
      while (I <= Length(S)) and
            (InQuote or not CharExistsW(WordDelims, S[I])) do begin
        if S[I] = Quote then
          InQuote := not InQuote;
        Inc(I);
      end
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractAsciiW(N : Cardinal; const S, WordDelims : WideString;
                       Quote : WideChar) : WideString;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  C       : Cardinal;
  I, J    : Longint;
  InQuote : Boolean;
begin
  InQuote := False;
  if AsciiPositionW(N, S, WordDelims, Quote, C) then begin
    I := C;
    J := I;
    {find the end of the current word}
    while (I <= Length(S)) and ((InQuote)
      or not CharExistsW(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not(InQuote);
      Inc(I);
    end;
    SetLength(Result, I-J);
    Move(S[J], Result[1], (I-J) * SizeOf(WideChar));
  end;
end;

procedure WordWrapW(const InSt : WideString; var OutSt, Overlap : WideString;
                   Margin : Cardinal; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  InStLen  : Cardinal;
  EOS,
  BOS      : Cardinal;
  ASpace   : WideChar;
begin
  InStLen := Length(InSt);

{!!.02 - Added }
  { handle empty string on input }         
  if InStLen = 0 then begin                
    OutSt := '';                           
    Overlap := '';                         
    Exit;                                  
  end;                                     
{!!.02 - Added }

  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (InSt[EOS] = ' ') and (EOS > 0) do
      Dec(EOS);

    if EOS > Margin then begin
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);

      {if EOS = 0 then we can't wrap it}
      if EOS = 0 then
        EOS := Margin
      else
        {trim trailing blanks}
        while (InSt[EOS] = ' ') and (EOS > 0) do
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  SetLength(OutSt, EOS);
  Move(InSt[1], OutSt[1], Length(OutSt) * SizeOf(WideChar));

  {find the start of the next word in the line}
  BOS := Succ(EOS);
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    SetLength(OverLap, 0)
  else begin
    {copy from the start of the next word to the end of the line}

    SetLength(OverLap, InStLen);
    Move(InSt[BOS], Overlap[1], Succ(InStLen-BOS) * SizeOf(WideChar));
    SetLength(OverLap, Succ(InStLen-BOS));
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (Length(OutSt) < LongInt(Margin)) then begin
    SetLength(OutSt, Margin);
    ASpace := ' ';
    StUtils.FillWord(OutSt[Succ(Length(OutSt))],
                     LongInt(Margin)-Length(OutSt), Word(ASpace));
  end;
end;

  {--------------- String comparison and searching -----------------}

function CompStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings.}
begin
  Result := CompareStr(S1, S2);
end;

function CompUCStringW(const S1, S2 : WideString) : Integer;
  {-Compare two strings. This compare is not case sensitive.}
begin
  Result := CompareText(S1, S2);
end;

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionW(const Name, Ext : WideString) : WideString;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionW(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionW(const Name, Ext : WideString) : WideString;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionW(Name, DotPos) then
    Result := System.Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameW(const PathName : WideString) : WideString;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Word(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (pos(PathName[I], DosDelimSetW) > 0)                  {!!.01}
    or (PathName[I] = #0);                                               {!!.01}
  Result := System.Copy(PathName, Succ(I), StMaxFileLen);
end;

function JustNameW(const PathName : WideString) : WideString;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
  S      : WideString;
begin
  S := JustFileNameW(PathName);
  if HasExtensionW(S, DotPos) then
    S := System.Copy(S, 1, DotPos-1);
  Result := S;
end;

function JustExtensionW(const Name : WideString) : WideString;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionW(Name, DotPos) then
    Result := System.Copy(Name, Succ(DotPos), StMaxFileLen)
  else
    Result := '';
end;

function JustPathnameW(const PathName : WideString) : WideString;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  if PathName = '' then Exit;

  I := Succ(Word(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or (pos(PathName[I], DosDelimSetW) > 0)                  {!!.01}
    or (PathName[I] = #0);                                               {!!.01}

  if I = 0 then
    {Had no drive or directory name}
    SetLength(Result, 0)
  else if I = 1 then
    {Either the root directory of default drive or invalid pathname}
    Result := PathName[1]
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Result := System.Copy(PathName, 1, I)
    else
      {Subdirectory, remove the trailing backslash}
      Result := System.Copy(PathName, 1, Pred(I));
  end else
    {Either the default directory of a drive or invalid pathname}
    Result := System.Copy(PathName, 1, I);
end;


function AddBackSlashW(const DirName : WideString) : WideString;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then
    Exit;
  if ((Length(Result) = 2) and (Result[2] = ':')) or
     ((Length(Result) > 2) and (Result[Length(Result)] <> '\')) then
    Result := Result + '\';
end;

function CleanFileNameW(const FileName : WideString) : WideString;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos : Cardinal;
  NameLen : Word;
begin
  if HasExtensionW(FileName, DotPos) then begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := Pred(DotPos);
    if NameLen > 8 then
      NameLen := 8;
    Result := System.Copy(FileName, 1, NameLen)+System.Copy(FileName, DotPos, 4);
  end else
    {Take the first 8 chars of name}
    Result := System.Copy(FileName, 1, 8);
end;

function CleanPathNameW(const PathName : WideString) : WideString;
  {-Return a pathname cleaned up as DOS does it.}
var
  I : Cardinal;
  S : WideString;
begin
  SetLength(Result, 0);
  S := PathName;

  I := Succ(Word(Length(S)));
  repeat
    dec(I);
    if I > 2 then
      if (S[I] = '\') and (S[I-1] = '\') then
        if (S[I-2] <> ':') then
          System.Delete(S, I, 1);
  until I <= 0;

  I := Succ(Word(Length(S)));
  repeat
    {Get the next directory or drive portion of pathname}
    repeat
      Dec(I);
    until (I = 0) or CharInSet(S[I], DosDelimSet);                            {!!.02}

    {Clean it up and prepend it to output string}
    Result := CleanFileNameW(System.Copy(S, Succ(I), StMaxFileLen)) + Result;
    if I > 0 then begin
      Result := S[I] + Result;
      System.Delete(S, I, 255);
    end;
  until I <= 0;

end;

function HasExtensionW(const Name : WideString; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}
var
  I : Cardinal;
begin
  DotPos := 0;
  for I := Length(Name) downto 1 do
    if (Name[I] = '.') and (DotPos = 0) then
      DotPos := I;
  Result := (DotPos > 0)
    and not CharExistsW(System.Copy(Name, Succ(DotPos), StMaxFileLen), '\');
end;

  {------------------ Formatting routines --------------------}


function CommaizeChW(L : Longint; Ch : WideChar) : WideString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp       : WideString;
  I,
  Len,
  NumCommas  : Cardinal;
  Neg        : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if (L < 0) then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrW(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeW(L : LongInt) : WideString;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChW(L, ',');
end;

function FormPrimW(const Mask     : WideString;
                         R        : TstFloat;
                   const LtCurr,
                         RtCurr   : WideString;
                         Sep,
                         DecPt    : WideChar;
                         AssumeDP : Boolean) : WideString;
  {-Returns a formatted string with digits from R merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
{$IFOPT N+}
  MaxPlaces = 18;
{$ELSE}
  MaxPlaces = 11;
{$ENDIF}
  FormChars  : string[8] = '#@*$-+,.';
  PlusArray  : array[Boolean] of WideChar = ('+', '-');
  MinusArray : array[Boolean] of WideChar = (' ', '-');
  FillArray  : array[Blank..Zero] of WideChar = (' ', '*', '0');
var
  S            : WideString; {temporary string}
  Filler       : Integer;    {char for unused digit slots: ' ', '*', '0'}
  WontFit,                   {true if number won't fit in the mask}
  AddMinus,                  {true if minus sign needs to be added}
  Dollar,                    {true if floating dollar sign is desired}
  Negative     : Boolean;    {true if B is negative}
  StartF,                    {starting point of the numeric field}
  EndF         : Longint;    {end of numeric field}
  RtChars,                   {# of chars to add to right}
  LtChars,                   {# of chars to add to left}
  DotPos,                    {position of '.' in Mask}
  Digits,                    {total # of digits}
  Blanks,                    {# of blanks returned by Str}
  Places,                    {# of digits after the '.'}
  FirstDigit,                {pos. of first digit returned by Str}
  Extras,                    {# of extra digits needed for special cases}
  DigitPtr     : Byte;       {pointer into temporary string of digits}
  I            : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if (not AssumeDP) and (not CharExistsW(Result, '.')) then
    AssumeDP := true;
  if AssumeDP and (Result <> '') then begin
    SetLength(Result, Succ(Length(Result)));
    Result[Length(Result)] := '.';
  end;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Result) = 0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 1;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  for I := Length(Result) downto 1 do begin
    if Result[I] = 'C' then begin
      Inc(RtChars);
      System.Delete(Result, I, 1);
    end else if Result[I] = 'c' then begin
      Inc(LtChars);
      System.Delete(Result, I, 1);
    end;
  end;

  {find the starting point for the field}
  while (StartF <= Length(Result))
    {and (System.Pos(Result[StartF], FormChars) = 0) do}
    and not CharExistsW(FormChars, Result[StartF]) do
    Inc(StartF);
  if StartF > Length(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do begin
    EndF := I;
    case Result[EndF] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := EndF;
    else
      goto EndFound;
    end;
    {Inc(EndF);}
  end;

  {if we get here at all, the last char was part of the field}
  Inc(EndF);

EndFound:
  {if we jumped to here instead, it wasn't}
  Dec(EndF);

  {disallow Dollar if Filler is Zero}
  if Filler = Zero then
    Dollar := False;

  {we need an extra slot if Dollar is True}
  Extras := Ord(Dollar);

  {get total # of digits and # after the decimal point}
  for I := StartF to EndF do
    case Result[I] of
      '#', '@',
      '*', '$' :
        begin
          Inc(Digits);
          if (I > DotPos) and (DotPos <> 0) then
            Inc(Places);
        end;
    end;

  {need one more 'digit' if Places > 0}
  Inc(Digits, Ord(Places > 0));

  {also need an extra blank if (1) Negative is true, and (2) Filler is Blank,
   and (3) AddMinus is true}
  if Negative and AddMinus and (Filler = Blank) then
    Inc(Extras)
  else
    AddMinus := False;

  {translate the real to a string}
  Str(R:Digits:Places, S);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    I := Length(S);
    SetLength(S, LongInt(I) + (Places-MaxPlaces));
    StUtils.FillWord(S[Succ(I)], Places-MaxPlaces, Word(WideChar('0')));
    while (Length(S) > Digits) and (S[1] = ' ') do
      System.Delete(S, 1, 1);
  end;

  {count number of initial blanks}
  Blanks := 1;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;
  Dec(Blanks);

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (Length(S) > Digits) or (Blanks < Extras);

  {if it won't fit, fill decimal slots with '*'}
  if WontFit then begin
    for I := StartF to EndF do
      case Result[I] of
        '#', '@', '*', '$' : Result[I] := '*';
        '+' : Result[I] := PlusArray[Negative];
        '-' : Result[I] := MinusArray[Negative];
      end;
    goto Done;
  end;

  {fill initial blanks in S with Filler; insert floating dollar sign}
  if Blanks > 0 then begin
    FillWord(S[1], Blanks, Word(FillArray[Filler]));

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Blanks] := LtCurr[1];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Blanks] := '-';
  end;

  {put in the digits / signs}
  DigitPtr := Length(S);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if DigitPtr <> 0 then begin
          Result[I] := S[DigitPtr];
          Dec(DigitPtr);
          if (DigitPtr <> 0) and (S[DigitPtr] = '.') then                {!!.01}
            Dec(DigitPtr);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (DigitPtr < FirstDigit) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '+' : Result[I] := PlusArray[Negative];
      '-' : Result[I] := MinusArray[Negative];
    end;
  end;

Done:
  if AssumeDP then
    SetLength(Result, Pred(Length(Result)));
  if RtChars > 0 then begin
    S := RtCurr;
    if Length(S) > RtChars then
      SetLength(S, RtChars)
    else
      S := LeftPadW(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Length(S) > LtChars then
      SetLength(S, LtChars)
    else
      S := PadW(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormW(const Mask     : WideString;
                          R        : TstFloat ;
                    const LtCurr,
                          RtCurr   : WideString;
                          Sep,
                          DecPt    : WideChar) : WideString;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimW(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormW(const Mask    : WideString;
                            L       : Longint;
                      const LtCurr,
                            RtCurr  : WideString;
                            Sep     : WideChar) : WideString;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimW(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosW(const P : WideString; C : WideChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified character within a string.}
var
  i : integer;
begin
  Result := true;
  for i := 1 to length(P) do
    if (P[i] = C) then begin
      Pos := i;
      Exit;
    end;
  Result := false;
  Pos := 0;
end;

function StrStPosW(const P, S : WideString; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertW(const S : WideString; C : WideChar; Pos : Cardinal) : WideString;
  {-Insert a character into a string at a specified position.}
var
  Temp : WideString;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertW(const S1, S2 : WideString; Pos : Cardinal) : WideString;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteW(const S : WideString; Pos : Cardinal) : WideString;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteW(const S : WideString; Pos, Count : Cardinal) : WideString;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;




function CopyLeftW(const S : WideString; Len : Cardinal) : WideString;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;



function CopyMidW(const S : WideString; First, Len : Cardinal) : WideString;
  {-Return the mid part of a string}
begin
  if (LongInt(First) > Length(S)) or (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;



function CopyRightW(const S : WideString; First : Cardinal) : WideString;
  {-Return the right Len characters of a string}
begin
  if (LongInt(First) > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;


function CopyRightAbsW(const S : WideString; NumChars : Cardinal) : WideString;
  {-Return NumChar characters starting from end}
begin
  if (Cardinal(Length(S)) > NumChars) then
    Result := Copy(S, (Cardinal(Length(S)) - NumChars)+1, NumChars)
  else
    Result := S;
end;


function WordPosW(const S, WordDelims, AWord : WideString;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr : WideString;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S = '') or (AWord = '') or (pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  TmpStr := S;
  I      := 0;
  Len    := Length(AWord);
  P1     := pos(AWord, TmpStr);

  while (P1 > 0) and (Length(TmpStr) > 0) do begin
    P2 := P1 + pred(Len);
    if (P1 = 1) then begin
      if (pos(TmpStr[P2+1], WordDelims) > 0) then begin
        Inc(I);
      end else
        System.Delete(TmpStr, 1, P2);
    end else if (pos(TmpStr[P1-1], WordDelims) > 0) and
                ((pos(TmpStr[P2+1], WordDelims) > 0) or
                 (LongInt(P2+1) = Length(TmpStr))) then begin
      Inc(I);
    end else if ((LongInt(P1) + LongInt(pred(Len))) = Length(TmpStr)) then begin
      if (P1 > 1) and (pos(TmpStr[P1-1], WordDelims) > 0) then
        Inc(I);
    end;

    if (I = N) then begin
      Result := True;
      Position := Position + P1;
      Exit;
    end;
    System.Delete(TmpStr, 1, P2);
    Position := Position + P2;
    P1 := pos(AWord, TmpStr);
  end;
end;




function CopyFromNthWordW(const S, WordDelims : WideString;
                          AWord : WideString; N : Cardinal;
                          var SubString : WideString) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosW(S, WordDelims, AWord, N, P)) then begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end else begin
    SubString := '';
    Result := False;
  end;
end;



function DeleteFromNthWordW(const S, WordDelims : WideString;
                            AWord : WideString; N : Cardinal;
                            var SubString : WideString) : Boolean;
var
  P : Cardinal;
begin
  SubString := S;
  if (WordPosW(S, WordDelims, AWord, N, P)) then begin
    Result := True;
    SubString := Copy(S, 1, P-1);
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function CopyFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                         N1, N2 : Cardinal;
                         var SubString : WideString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosW(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosW(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        SubString := Copy(S, P1, P2-P1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function DeleteFromToWordW(const S, WordDelims, Word1, Word2 : WideString;
                           N1, N2 : Cardinal;
                           var SubString : WideString) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  SubString := S;
  if (WordPosW(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosW(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        System.Delete(SubString, P1, P2-P1+1);
      end else begin
        Result := False;
        SubString := '';
      end;
    end else begin
      Result := False;
      SubString := '';
    end;
  end else begin
    Result := False;
    SubString := '';
  end;
end;



function CopyWithinW(const S, Delimiter : WideString;
                     Strip : Boolean) : WideString;
var
  P1,
  P2     : Cardinal;
  TmpStr : WideString;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosW(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if StrStPosW(TmpStr, Delimiter, P2) then begin
        Result := Copy(TmpStr, 1, P2-1);
        if (not Strip) then
          Result := Delimiter + Result + Delimiter;
      end else begin
        Result := TmpStr;
        if (not Strip) then
          Result := Delimiter + Result;
      end;
    end;
  end;
end;



function DeleteWithinW(const S, Delimiter : WideString) : WideString;
var
  P1,
  P2     : Cardinal;
  TmpStr : WideString;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosW(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, LongInt(P1) + Length(Delimiter), Length(S));
      if (pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1-1)
      else begin
        if (StrStPosW(TmpStr, Delimiter, P2)) then begin
          P2 := LongInt(P2) + (2*Length(Delimiter));
          Result := S;
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;



function ReplaceWordW(const S, WordDelims, OldWord, NewWord : WideString;
                      N : Cardinal;
                      var Replacements : Cardinal) : WideString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (pos(OldWord, S) = 0) then begin
    Result := '';
    Replacements := 0;
    Exit;
  end;

  if (WordPosW(S, WordDelims, OldWord, N, P1)) then begin
    Result := S;
    System.Delete(Result, P1, Length(OldWord));

    C := 0;
    for I := 1 to Replacements do begin
      if (((Length(NewWord)) + Length(Result)) < MaxLongInt) then begin
        Inc(C);
        System.Insert(NewWord, Result, P1);
        Inc(P1, Length(NewWord) + 1);
      end else begin
        Replacements := C;
        Exit;
      end;
    end;
  end else begin
    Result := S;
    Replacements := 0;
  end;
end;


function ReplaceWordAllW(const S, WordDelims, OldWord, NewWord : WideString;
                         var Replacements : Cardinal) : WideString;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (Pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
  end else begin
    Result := S;
    C := 0;
    while (WordPosW(Result, WordDelims, OldWord, 1, P1)) do begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do begin
        if ((Length(NewWord) + Length(Result)) <= 255) then begin
          Inc(C);
          System.Insert(NewWord, Result, P1);
        end else begin
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
  end;
end;


function ReplaceStringW(const S, OldString, NewString : WideString;
                        N : Cardinal;
                        var Replacements : Cardinal) : WideString;
var
  I,
  C,
  P1 : Cardinal;
  TmpStr : WideString;
begin
  if (S = '') or (OldString = '') or (pos(OldString, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;
  TmpStr := S;

  I  := 1;
  P1 := pos(OldString, TmpStr);
  C  := P1;
  while (I < N) and (LongInt(C) < Length(TmpStr)) do begin
    Inc(I);
    System.Delete(TmpStr, 1, LongInt(P1) + Length(OldString));
    Inc(C, LongInt(P1) + Length(OldString));
  end;
  Result := S;
  System.Delete(Result, C, Length(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if (((Length(NewString)) + Length(Result)) < MaxLongInt) then begin
      Inc(C);
      System.Insert(NewString, Result, P1);
      Inc(P1, Length(NewString) + 1);
    end else begin
      Replacements := C;
      Exit;
    end;
  end;
end;


function ReplaceStringAllW(const S, OldString, NewString : WideString;
                           var Replacements : Cardinal) : WideString;
var
  I,
  C  : Cardinal;
  P1 : longint;
  Tmp: WideString;
begin
  Result := S;
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
    Replacements := 0
  else begin
    Tmp := S;
    P1 := AnsiPos(OldString, S);
    if (P1 > 0) then begin
      Result := Copy(Tmp, 1, P1-1);
      C := 0;
      while (P1 > 0) do begin
        for I := 1 to Replacements do begin
          Inc(C);
          Result := Result + NewString;
        end;
        Tmp := Copy(Tmp, P1+Length(OldString), MaxLongInt);
        P1 := AnsiPos(OldString, Tmp);
        if (P1 > 0) then begin
          Result := Result + Copy(Tmp, 1, P1-1);
        end else
          Result := Result + Tmp;
      end;
      Replacements := C;
    end else begin
      Result := S;
      Replacements := 0;
    end;
  end;
end;


function LastWordW(const S, WordDelims, AWord : WideString;
                   var Position : Cardinal) : Boolean;
var
  TmpStr : WideString;
  I      : Cardinal;
begin
  if (S = '') or (WordDelims = '') or
     (AWord = '') or (pos(AWord, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  I := Length(TmpStr);
  while (pos(TmpStr[I], WordDelims) > 0) do begin
    System.Delete(TmpStr, I, 1);
    I := Length(TmpStr);
  end;

  Position := Length(TmpStr);
  repeat
    while (pos(TmpStr[Position], WordDelims) = 0) and (Position > 1) do
      Dec(Position);
    if (Copy(TmpStr, Position + 1, Length(AWord)) = AWord) then begin
      Inc(Position);
      Result := True;
      Exit;
    end;
    System.Delete(TmpStr, Position, Length(TmpStr));
    Position := Length(TmpStr);
  until (Length(TmpStr) = 0);
  Result := False;
  Position := 0;
end;



function LastWordAbsW(const S, WordDelims : WideString;
                      var Position : Cardinal) : Boolean;
begin
  if (S = '') or (WordDelims = '') then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not a "one-word wonder"}
  Position := Length(S);
  while (Position > 0) and (pos(S[Position], WordDelims) > 0) do
    Dec(Position);

  if (Position = 0) then begin
    Result := True;
    Position := 1;
    Exit;
  end;

{find next delimiter character}
  while (Position > 0) and (pos(S[Position], WordDelims) = 0) do
    Dec(Position);
  Inc(Position);
  Result := True;
end;



function LastStringW(const S, AString : WideString;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : WideString;
  I, C   : Cardinal;
begin
  if (S = '') or (AString = '') or (pos(AString, S) = 0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  TmpStr := S;
  C := 0;
  I := pos(AString, TmpStr);
  while (I > 0) do begin
    Inc(C, LongInt(I) + Length(AString));
    System.Delete(TmpStr, 1, LongInt(I) + Length(AString));
    I := pos(AString, TmpStr);
  end;
{Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;



function KeepCharsW(const S, Chars : WideString) : WideString;
var
  P1,
  P2  : Cardinal;
begin
  if (S = '') or (Chars = '') then begin
    Result := '';
    Exit;
  end;

  Result := '';
  P1 := 1;
  P2 := 1;
  repeat
    while (pos(S[P2], Chars) > 0) and (LongInt(P2) <= Length(S)) do
      Inc(P2);
    Result := Result + Copy(S, P1, P2-P1);
    P1 := P2+1;
    P2 := P1;
    while (pos(S[P2], Chars) = 0) and (LongInt(P2) <= Length(S)) do
      Inc(P2);
    P1 := P2;
  until (LongInt(P1) > Length(S));
end;



function RepeatStringW(const RepeatString : WideString;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : WideString;
var
  J,
  Len : Cardinal;
begin
  Len := Length(RepeatString);
  Repetitions := MaxLen div Len;
  SetLength(Result, Repetitions * Len);
  for J := 0 to pred(Repetitions) do
    Move(RepeatString[1], Result[J * Len + 1], Len*SizeOf(WideChar));
end;



function TrimCharsW(const S, Chars : WideString) : WideString;
begin
  Result := RightTrimCharsW(S, Chars);
  Result := LeftTrimCharsW(Result, Chars);
end;



function RightTrimCharsW(const S, Chars : WideString) : WideString;
begin
  Result := S;
  while (pos(Result[Length(Result)], Chars) > 0) do
    System.Delete(Result, Length(Result), 1);
end;



function LeftTrimCharsW(const S, Chars : WideString) : WideString;
begin
  Result := S;
  while (pos(Result[1], Chars) > 0) do
    System.Delete(Result, 1, 1);
end;



function ExtractTokensW(const S, Delims  : WideString;
                              QuoteChar  : WideChar;
                              AllowNulls : Boolean;
                              Tokens     : TStrings) : Cardinal;
var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : WideChar;
  TokenStart : integer;
  Inx        : integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping

    If the quote character is #0, it's taken to mean that the routine
    should not check for quoted substrings.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S = '') or
     (Delims = '') or
     CharExistsW(Delims, QuoteChar) then
    Exit;

  {start off in the normal scanning state}
  State := ScanStart;

  {the first token starts at position 1}
  TokenStart := 1;

  {read through the entire string}
  for Inx := 1 to length(S) do begin

    {get the current character}
    CurChar := S[Inx];

    {process the character according to the current state}
    case State of
      ScanStart :
        begin
          {if the current char is the quote character, switch states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanQuotedToken

          {if the current char is a delimiter, output a null token}
          else if CharExistsW(Delims, CurChar) then begin

            {if allowed to, output a null token}
            if AllowNulls then begin
              Tokens.Add('');
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);
          end

          {otherwise, the current char is starting a normal token, so
           switch states}
          else
            State := ScanNormalToken
        end;

      ScanQuotedToken :
        begin
          {if the current char is the quote character, switch states}
          if (CurChar = QuoteChar) then
            State := ScanQuotedTokenEnd
        end;

      ScanQuotedTokenEnd :
        begin
          {if the current char is the quote character, we have a token
           consisting of two (or more) quoted substrings, so switch
           states}
          if (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token
           without the quotes}
          else if CharExistsW(Delims, CurChar) then begin

            {if the token is empty without the quotes, output a null
             token only if allowed to}
            if ((Inx - TokenStart) = 2) then begin
              if AllowNulls then begin
                Tokens.Add('');
                inc(Result);
              end
            end

            {else output the token without the quotes}
            else begin
              Tokens.Add(Copy(S, succ(TokenStart), Inx - TokenStart - 2));
              inc(Result);
            end;

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end

          {otherwise it's a (complex) normal token, so switch states}
          else
            State := ScanNormalToken
        end;

      ScanNormalToken :
        begin
          {if the current char is the quote character, we have a
           complex token with at least one quoted substring, so switch
           states}
          if (QuoteChar <> #0) and (CurChar = QuoteChar) then
            State := ScanNormalTokenWithQuote

          {if the current char is a delimiter, output the token}
          else if CharExistsW(Delims, CurChar) then begin
            Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
            inc(Result);

            {set the start of the next token to be one character after
             this delimiter}
            TokenStart := succ(Inx);

            {switch states back to the start state}
            State := ScanStart;
          end;
        end;

      ScanNormalTokenWithQuote :
        begin
          {if the current char is the quote character, switch states
           back to scanning a normal token}
          if (CurChar = QuoteChar) then
            State := ScanNormalToken;
        end;

    end;
  end;

  {we need to process the (possible) final token: first assume that
   the current character index is just beyond the end of the string}
  Inx := succ(length(S));

  {if we are in the scanning quoted token state, we've read an opening
   quote, but no closing one; increment the token start value}
  if (State = ScanQuotedToken) then
    inc(TokenStart)

  {if we've finished scanning a quoted token, we've read both quotes;
   increment the token start value, and decrement the current index}
  else if (State = ScanQuotedTokenEnd) then begin
    inc(TokenStart);
    dec(Inx);
  end;

  {if the final token is not empty, output the token}
  if (TokenStart < Inx) then begin
    Tokens.Add(Copy(S, TokenStart, Inx - TokenStart));
    inc(Result);
  end
  {otherwise the final token is empty, so output a null token if
   allowed to}
  else if AllowNulls then begin
    Tokens.Add('');
    inc(Result);
  end;
end;



function ContainsOnlyW(const S, Chars : WideString;
                       var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (not CharExistsW(Chars, S[I])) then begin
        BadPos := I;
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    BadPos := 0;
  end;
end;



function ContainsOtherThanW(const S, Chars : WideString;
                            var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (CharExistsW(Chars, S[I])) then begin
        BadPos := I;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
    BadPos := 0;
  end;
end;



function IsChAlphaW(C : WideChar) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := Windows.IsCharAlphaW(C);
end;



function IsChNumericW(C : WideChar; Numbers : WideString) : Boolean;
 {-Returns true if Ch in numeric set}
begin
  Result := pos(C, Numbers) > 0;
end;



function IsChAlphaNumericW(C : WideChar; Numbers : WideString) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := (Windows.IsCharAlphaW(C)) or (pos(C, Numbers) > 0);
end;



function IsStrAlphaW(const S : WideString) : Boolean;
  {-Returns true if all characters in string are an alpha}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := Windows.IsCharAlphaW(S[I]);
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;



function IsStrNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are in numeric set}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := pos(S[I], Numbers) > 0;
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;


function IsStrAlphaNumericW(const S, Numbers : WideString) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
var
  I : Cardinal;
begin
  if (S = '') then
    Result := False
  else begin
    for I := 1 to Length(S) do begin
      Result := (Windows.IsCharAlphaW(S[I])) or (pos(S[I], Numbers) > 0);
      if not Result then
        Exit;
    end;
    Result := True;
  end;
end;

function StrWithinW(const S, SearchStr : WideString;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
var
  TmpStr : WideString;
begin
  TmpStr := S;
  if (Start > 1) then
    System.Delete(TmpStr, 1, Start-1);
  Position := pos(SearchStr, TmpStr);
  if (Position > 0) then begin
    Position := Position + Start - 1;
    Result := True;
  end else
    Result := False;
end;

end.
