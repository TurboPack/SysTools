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
{* SysTools: StStrL.pas 4.04                             *}
{*********************************************************}
{* SysTools: Long string routines                        *}
{*********************************************************}

{$I StDefine.inc}

unit StStrL;

interface

uses
  Windows,
  Classes,
  SysUtils,
  StConst,
  StBase;

{.Z+}
type
  LStrRec = record
    AllocSize : Integer;
    RefCount  : Integer;
    Length    : Integer;
  end;

const
  StrOffset = SizeOf(LStrRec);
{.Z-}

  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}

function HexLL(L : Integer) : AnsiString;
  {-Return the hex string for a long integer.}

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}

function BinaryLL(L : Integer) : AnsiString;
  {-Return the binary string for a long integer.}

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}

function OctalLL(L : Integer) : AnsiString;
  {-Return an octal string for a long integer.}

function Str2Int16L(const S : string; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

function Str2WordL(const S : string; var I : Word) : Boolean;
  {-Convert a string to a word.}

function Str2LongL(const S : string; var I : Integer) : Boolean;
  {-Convert a string to a long integer.}

function Str2RealL(const S : string; var R : Double) : Boolean;

function Str2ExtL(const S : string; var R : Extended) : Boolean;
  {-Convert a string to an extended.}

function Long2StrL(L : Integer) : String; inline;
  {-Convert an integer type to a string.}

function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : String;
  {-Convert a real to a string.}

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : String;
  {-Convert an extended to a string.}

function ValPrepL(const S : String) : String;
  {-Prepares a string for calling Val.}

  {-------- General purpose string manipulation --------}

function CharStrL(C : Char; Len : Cardinal) : String;
  {-Return a string filled with the specified character.}

function PadChL(const S : String; C : Char; Len : Integer) : String;
  {-Pad a string on the right with a specified character.}

function PadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the right with spaces.}

function LeftPadChL(const S : String; C : Char; Len : Integer) : String;
  {-Pad a string on the left with a specified character.}

function LeftPadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left with spaces.}

function TrimLeadL(const S : String) : String;
  {-Return a string with leading white space removed.}

function TrimTrailL(const S : String) : String;
  {-Return a string with trailing white space removed.}

function TrimL(const S : String) : String;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesL(const S : String) : String;
  {-Return a string with leading and trailing spaces removed.}

function CenterChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the left and right with a specified character.}

function CenterL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left and right with spaces.}

function EntabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Convert blanks in a string to tabs.}

function DetabL(const S : AnsiString; TabSize : Byte) : AnsiString;
  {-Expand tabs in a string to blanks.}

function ScrambleL(const S, Key : AnsiString) : AnsiString;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteL(const S, FromStr, ToStr : String) : String;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterL(const S, Filters : String) : String;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}

  {--------------- Word / Char manipulation -------------------------}

function CharExistsL(const S : String; C : Char) : Boolean;
  {-Determine whether a given character exists in a string. }

function CharCountL(const S : String; C : Char) : Cardinal;
  {-Count the number of a given character in a string. }

function WordCountL(const S, WordDelims : String) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}

function WordPositionL(N : Cardinal; const S, WordDelims : String;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordL(N : Cardinal; const S, WordDelims : String) : String;
  {-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountL(const S, WordDelims : String; Quote : Char) : Cardinal;
  {-Return the number of words in a string.}

function AsciiPositionL(N : Cardinal; const S, WordDelims : String;
                       Quote : Char; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}

function ExtractAsciiL(N : Cardinal; const S, WordDelims : String;
                       Quote : Char) : String;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapL(const InSt : String; var OutSt, Overlap : String;
                   Margin : Integer; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

  {--------------- String comparison and searching -----------------}
function CompStringL(const S1, S2 : String) : Integer;
  {-Compare two strings.}

function CompUCStringL(const S1, S2 : String) : Integer;
  {-Compare two strings. This compare is not case sensitive.}

function SoundexL(const S : AnsiString) : AnsiString;
  {-Return 4 character soundex of an input string.}

function MakeLetterSetL(const S : AnsiString) : Integer;
  {-Return a bit-mapped long storing the individual letters contained in S.}

{$IFDEF UNICODE}
procedure BMMakeTableL(const MatchString : UnicodeString; var BT : BTable); overload;
{$ELSE}
procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable); overload;
{$ENDIF}
  {-Build a Boyer-Moore link table}

{$IFDEF UNICODE}
function BMSearchL(var Buffer; BufLength: Cardinal; var BT: BTable;
                  const MatchString : String; out Pos : Cardinal) : Boolean; overload;
{$ELSE}
function BMSearchL(var Buffer; BufLength : Cardinal; var BT : BTable;
                  const MatchString : AnsiString; var Pos : Cardinal) : Boolean; overload;
{$ENDIF}
  {-Use the Boyer-Moore search method to search a buffer for a string.}

{$IFDEF UNICODE}
function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
                    const MatchString : String ; var Pos : Cardinal) : Boolean;
{$ELSE}
function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
                    const MatchString : AnsiString ; var Pos : Cardinal) : Boolean;
{$ENDIF}
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionL(const Name, Ext : String) : String;
  {-Return a file name with a default extension attached.}

function ForceExtensionL(const Name, Ext : String) : String;
  {-Force the specified extension onto the file name.}

function JustFilenameL(const PathName : String) : String;
  {-Return just the filename and extension of a pathname.}

function JustNameL(const PathName : String) : String;
  {-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionL(const Name : String) : String;
  {-Return just the extension of a pathname.}

function JustPathnameL(const PathName : String) : String;
  {-Return just the drive and directory portion of a pathname.}

function AddBackSlashL(const DirName : String) : String;
  {-Add a default backslash to a directory name.}

function CleanPathNameL(const PathName : String) : String;
  {-Return a pathname cleaned up as DOS does it.}

function HasExtensionL(const Name : String; var DotPos : Cardinal) : Boolean;
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

  {------------------ Formatting routines --------------------}

function CommaizeL(L : Integer) : String;
  {-Convert a long integer to a string with commas.}

function CommaizeChL(L : Integer; Ch : Char) : String;
  {-Convert a long integer to a string with Ch in comma positions.}

function FloatFormL(const Mask : String ; R : TstFloat ; const LtCurr,
                    RtCurr : String ; Sep, DecPt : Char) : String;
  {-Return a formatted string with digits from R merged into mask.}

function LongIntFormL(const Mask : String ; L : Integer ; const LtCurr,
                      RtCurr : String ; Sep : Char) : String;
  {-Return a formatted string with digits from L merged into mask.}

function StrChPosL(const P : String; C : Char; var Pos : Integer) : Boolean;
  {-Return the position of a specified character within a string.}

function StrStPosL(const P, S : String; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyL(const S : String; Pos, Count : Cardinal) : String;
  {-Copy characters at a specified position in a string.}

function StrChInsertL(const S : String; C : Char; Pos : Cardinal) : String;
  {-Insert a character into a string at a specified position.}

function StrStInsertL(const S1, S2 : String; Pos : Cardinal) : String;
  {-Insert a string into another string at a specified position.}

function StrChDeleteL(const S : String; Pos : Cardinal) : String;
  {-Delete the character at a specified position in a string.}

function StrStDeleteL(const S : String; Pos, Count : Cardinal) : String;
  {-Delete characters at a specified position in a string.}


{--------------------------  New Functions -----------------------------------}

function ContainsOnlyL(const S, Chars : String;
                       var BadPos : Cardinal) : Boolean;

function ContainsOtherThanL(const S, Chars : String;
                            var BadPos : Cardinal) : Boolean;

function CopyLeftL(const S : String; Len : Cardinal) : String;
  {-Return the left Len characters of a string}

function CopyMidL(const S : String; First, Len : Cardinal) : String;
  {-Return the mid part of a string}

function CopyRightL(const S : String; First : Cardinal) : String;
  {-Return the right Len characters of a string}

function CopyRightAbsL(const S : String; NumChars : Cardinal) : String;
  {-Return NumChar characters starting from end}

function CopyFromNthWordL(const S, WordDelims : String;
                         const AWord : String; N : Cardinal;       {!!.02}
                         var SubString : String) : Boolean;

function CopyFromToWordL(const S, WordDelims, Word1, Word2 : String;
                         N1, N2 : Cardinal;
                         var SubString : String) : Boolean;

function CopyWithinL(const S, Delimiter : String;
                    Strip : Boolean) : String;

function DeleteFromNthWordL(const S, WordDelims : String;
                            const AWord : String; N : Cardinal;    {!!.02}
                            var SubString : String) : Boolean;

function DeleteFromToWordL(const S, WordDelims, Word1, Word2 : String;
                           N1, N2 : Cardinal;
                           var SubString : String) : Boolean;

function DeleteWithinL(const S, Delimiter : String) : String;

function ExtractTokensL(const S, Delims: String;
                        QuoteChar  : Char;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;

function IsChAlphaL(C : Char) : Boolean;
 {-Returns true if Ch is an alpha}

function IsChNumericL(C : Char; const Numbers : String) : Boolean; {!!.02}
 {-Returns true if Ch in numeric set}

function IsChAlphaNumericL(C : Char; const Numbers : String) : Boolean; {!!.02}
  {-Returns true if Ch is an alpha or numeric}

function IsStrAlphaL(const S : String) : Boolean;
  {-Returns true if all characters in string are an alpha}

function IsStrNumericL(const S, Numbers : String) : Boolean;
  {-Returns true if all characters in string are in numeric set}

function IsStrAlphaNumericL(const S, Numbers : String) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}

function KeepCharsL(const S, Chars : String) : String;

function LastWordL(const S, WordDelims, AWord : String;
                   var Position : Cardinal) : Boolean;

function LastWordAbsL(const S, WordDelims : String;
                        var Position : Cardinal) : Boolean;

function LastStringL(const S, AString : String;
                     var Position : Cardinal) : Boolean;

function LeftTrimCharsL(const S, Chars : String) : String;

function ReplaceWordL(const S, WordDelims, OldWord, NewWord : String;
                      N : Cardinal;
                      var Replacements : Cardinal) : String;

function ReplaceWordAllL(const S, WordDelims, OldWord, NewWord : String;
                         var Replacements : Cardinal) : String;

function ReplaceStringL(const S, OldString, NewString : String;
                        N : Cardinal;
                        var Replacements : Cardinal) : String;

function ReplaceStringAllL(const S, OldString, NewString : String;
                           var Replacements : Cardinal) : String;

function RepeatStringL(const RepeatString : String;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : String;

function RightTrimCharsL(const S, Chars : String) : String;

function StrWithinL(const S, SearchStr : string;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
  {-finds the position of a substring within a string starting at a given point}

function TrimCharsL(const S, Chars : String) : String;

function WordPosL(const S, WordDelims, AWord : String;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a word within a string}


implementation

  {-------- Numeric conversion -----------}

function HexBL(B : Byte) : AnsiString;
  {-Return the hex string for a byte.}
begin
  SetLength(Result, 2);
  Result[1] := StHexDigits[B shr 4];
  Result[2] := StHexDigits[B and $F];
end;

function HexWL(W : Word) : AnsiString;
  {-Return the hex string for a word.}
begin
  SetLength(Result, 4);
  Result[1] := StHexDigits[hi(W) shr 4];
  Result[2] := StHexDigits[hi(W) and $F];
  Result[3] := StHexDigits[lo(W) shr 4];
  Result[4] := StHexDigits[lo(W) and $F];
end;

function HexLL(L : Integer) : AnsiString;
  {-Return the hex string for a long integer.}
begin
  SetLength(Result, 8);
  Result := HexWL(HiWord(DWORD(L))) + HexWL(LoWord(DWORD(L)));         {!!.02}
end;

function HexPtrL(P : Pointer) : AnsiString;
  {-Return the hex string for a pointer.}
begin
  SetLength(Result, 9);
  Result := ':' + HexLL(Integer(P));
end;

function BinaryBL(B : Byte) : AnsiString;
  {-Return a binary string for a byte.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 8);
  for I := 7 downto 0 do begin
    Result[N] := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryWL(W : Word) : AnsiString;
  {-Return the binary string for a word.}
var
  I, N : Word;
begin
  N := 1;
  SetLength(Result, 16);
  for I := 15 downto 0 do begin
    Result[N] := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function BinaryLL(L : Integer) : AnsiString;
  {-Return the binary string for a long integer.}
var
  I : Integer;
  N : Byte;
begin
  N := 1;
  SetLength(Result, 32);
  for I := 31 downto 0 do begin
    Result[N] := StHexDigits[Ord(L and Integer(1 shl I) <> 0)]; {0 or 1}
    Inc(N);
  end;
end;

function OctalBL(B : Byte) : AnsiString;
  {-Return an octal string for a byte.}
var
  I : Word;
begin
  SetLength(Result, 3);
  for I := 0 to 2 do begin
    Result[3-I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
end;

function OctalWL(W : Word) : AnsiString;
  {-Return an octal string for a word.}
var
  I : Word;
begin
  SetLength(Result, 6);
  for I := 0 to 5 do begin
    Result[6-I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
end;

function OctalLL(L : Integer) : AnsiString;
  {-Return an octal string for a long integer.}
var
  I : Word;
begin
  SetLength(Result, 12);
  for I := 0 to 11 do begin
    Result[12-I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
end;

function Str2Int16L(const S : string; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

var
  ec : Integer;
begin
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

function Str2WordL(const S : string; var I : Word) : Boolean;
  {-Convert a string to a word.}

var
  ec : Integer;
begin
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

function Str2LongL(const S : string; var I : Integer) : Boolean;
  {-Convert a string to a long integer.}

var
  ec : Integer;
begin
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

function Str2RealL(const S : string; var R : Double) : Boolean;
  {-Convert a string to a real.}
var
  Code : Integer;
  St   : string;
begin
  Result := False;
  if S = '' then Exit;
  St := TrimTrailL(S);
  if St = '' then Exit;
  Val(ValPrepL(St), R, Code);
  if Code <> 0 then begin
    R := Code;
  end else
    Result := True;
end;

function Str2ExtL(const S : string; var R : Extended) : Boolean;
  {-Convert a string to an extended.}
var
  Code : Integer;
  P : string;
begin
  Result := False;
  if S = '' then Exit;
  P := TrimTrailL(S);
  if P = '' then Exit;
  Val(ValPrepL(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
  end else
    Result := True;
end;

function Long2StrL(L : Integer) : String;
  {-Convert an integer type to a string.}
begin
  Result := L.ToString;
end;

function Real2StrL(R : Double; Width : Byte; Places : ShortInt) : String;
  {-Convert a real to a string.}
var
  sBuffer: ShortString;
begin
  Str(R:Width:Places, sBuffer);
  Result := string(sBuffer);
end;

function Ext2StrL(R : Extended; Width : Byte; Places : ShortInt) : String;
  {-Convert an extended to a string.}
var
  sBuffer: ShortString;
begin
  Str(R:Width:Places, sBuffer);
  Result := string(sBuffer);
end;

function ValPrepL(const S : String) : String;
  {-Prepares a string for calling Val.}
var
  P : Integer;
  C : Integer;
begin
  Result := TrimSpacesL(S);
  if Result <> '' then begin
    if StrChPosL(Result, FormatSettings.DecimalSeparator, P) then begin
      C := P;
      Result[C] := '.';
      if C = Length(Result) then
        SetLength(Result, Pred(C));
    end;
  end else
    Result := '0';
end;

  {-------- General purpose string manipulation --------}

function CharStrL(C : Char; Len : Cardinal) : String;
  {-Return a string filled with the specified character.}
begin
  Result := StringOfChar(C, Len)
end;

function PadChL(const S : String; C : Char; Len : Integer) : String;
  {-Pad a string on the right with a specified character.}
begin
  Result := S;
  if Length(Result) < Len then
    Result := Result + StringOfChar(C, Len - Length(Result));
end;

function PadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the right with spaces.}
begin
  Result := PadChL(S, ' ', Len);
end;

function LeftPadChL(const S : String; C : Char; Len : Integer) : String;
  {-Pad a string on the left with a specified character.}
begin
  if Length(S) > Len then
    Result := S
  else
    Result := StringOfChar(C, Len - Length(S)) + S;
end;

function LeftPadL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left with spaces.}
begin
  Result := LeftPadChL(S, ' ', Len);
end;

function TrimLeadL(const S : String) : String;
  {-Return a string with leading white space removed}
begin
  Result := TrimLeft(S);
end;

function TrimTrailL(const S : String) : String;
  {-Return a string with trailing white space removed.}
begin
  Result := TrimRight(S);
end;

function TrimL(const S : String) : String;
  {-Return a string with leading and trailing white space removed.}
var
  I : Integer;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    SetLength(Result, Pred(Length(Result)));

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function TrimSpacesL(const S : String) : String;
  {-Return a string with leading and trailing spaces removed.}
var
  I : Integer;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] = ' ') do
    SetLength(Result, Pred(Length(Result)));
  I := 1;
  while (I <= Length(Result)) and (S[I] = ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function CenterChL(const S : String; C : Char; Len : Cardinal) : String;
  {-Pad a string on the left and right with a specified character.}
begin
  if Length(S) >= Integer(Len) then
    Result := S
  else if Length(S) < MaxLongInt then begin
//    SetLength(Result, Len);
//    FillChar(Result[1], Len, C);
    Result := StringOfChar(C, Len);
    if Length(S) > 0 then                                                {!!.01}
      Move(S[1], Result[Succ((Integer(Len)-Length(S)) shr 1)], Length(S)*SizeOf(Char));
  end;
end;

function CenterL(const S : String; Len : Cardinal) : String;
  {-Pad a string on the left and right with spaces.}
begin
  Result := CenterChL(S, ' ', Len);
end;

function EntabL(const S : AnsiString; TabSize : Byte) : AnsiString;   //TODO-UNICODE
  {-Convert blanks in a string to tabs.}
var
  InLen, OutLen : Cardinal;
begin
  if S = '' then Exit;
  InLen := Length(S);
  OutLen := 0;
  SetLength(Result, InLen);
asm
  push   ebx                   { Save registers }
  push   edi
  push   esi

  mov    edi, [Result]
  mov    edi, [edi]
  xor    ecx, ecx
  add    cl, TabSize
  jz     @@Done

  mov    esi, S
  xor    ebx, ebx              { Zero EBX and EDX }
  xor    edx, edx
  inc    edx                   { Set output length to 1 }

@@Next:
  or     ebx, ebx
  je     @@NoTab               { Jump to NoTab if spacecount is zero }
  mov    eax, edx              { IPos to EAX }
  push   edx
  xor    edx, edx
  div    ecx
  cmp    edx, 1                { Is mod = 1? }
  pop    edx
  jne    @@NoTab               { If not, no tab }

  sub    edi, ebx
  sub    OutLen, ebx
  inc    OutLen
  xor    ebx, ebx              { Reset spacecount }
  mov    byte ptr [edi], 9h    { Store a tab }
  inc    edi

@@NoTab:
  mov    al, [esi]             { Get next input character }
  inc    esi
  cmp    edx, InLen            { End of string? }
  jg     @@Done                { Yes, done }
  inc    ebx                   { Increment SpaceCount }
  cmp    al, 20h               { Is character a space? }
  jz     @@Store               { Yes, store it for now }
  xor    ebx, ebx              { Reset SpaceCount }
  cmp    al, 27h               { Is it a quote? }
  jz     @@Quotes              { Yep, enter quote loop }
  cmp    al, 22h               { Is it a doublequote? }
  jnz    @@Store               { Nope, store it }

@@Quotes:
  mov    ah, al                { Save quote start }

@@NextQ:
  mov    [edi], al             { Store quoted character }
  inc    edi
  inc    OutLen
  mov    al, [esi]             { Get next character }
  inc    esi
  inc    edx                   { Increment Ipos }

  cmp    edx, ecx              { At end of line? }
  jae    @@Store               { If so, exit quote loop }

  cmp    al, ah                { Matching end quote? }
  jnz    @@NextQ               { Nope, stay in quote loop }

  cmp    al, 27h               { Single quote? }
  jz     @@Store               { Exit quote loop }

  cmp    byte ptr [esi-2],'\'  { Previous character an escape? }
  jz     @@NextQ               { Stay in if so }

@@Store:
  mov    [edi], al             { Store last character }
  inc    edi
  inc    OutLen
  inc    edx                   { Increment input position }
  jmp    @@Next                { Repeat while characters left }

@@Done:
  mov    byte ptr [edi], 0h
  pop    esi
  pop    edi
  pop    ebx
end;
  SetLength(Result, OutLen);
end;

function DetabL(const S : AnsiString; TabSize : Byte) : AnsiString;   //TODO-UNICODE
  {-Expand tabs in a string to blanks.}
var
  NumTabs : Integer;
begin
  Result := '';
  if S = '' then Exit;
  if TabSize = 0 then Exit;
  Result := S;
  NumTabs := CharCountL(string(S), #9);
  if NumTabs = 0 then Exit;
  SetLength(Result, Length(Result)+NumTabs*(Pred(TabSize)));
asm
  push   ebx                { Save registers since we'll be changing them. }
  push   edi
  push   esi

  mov    edi, Result        { EDI => output string. }
  mov    esi, S             { ESI => input string. }
  xor    ebx, ebx
  mov    bl, TabSize
  mov    edi, [edi]
  xor    ecx, ecx           { Default input length = 0. }
  xor    edx, edx           { Zero EDX for output length }
  xor    eax, eax           { Zero EAX }
  mov    ecx, [esi-StrOffset].LStrRec.Length  { Get input length. }
  or     ebx, ebx           { TabSize = 0? }
  jnz    @@DefLength
  mov    ecx, edx           { Return zero length string if TabSize = 0. }

@@DefLength:
  mov    [edi-StrOffset].LStrRec.Length, ecx  { Store default output length. }
  or     ecx, ecx
  jz     @@Done             { Done if empty input string. }

@@Next:
  mov    al, [esi]          { Next input character. }
  inc    esi
  cmp    al, 09h            { Is it a tab? }
  jz     @@Tab              { Yes, compute next tab stop. }
  mov    [edi], al          { No, store to output. }
  inc    edi
  inc    edx                { Increment output length. }
  dec    ecx                { Decrement input length. }
  jnz    @@Next
  jmp    @@StoreLen         { Loop termination. }

@@Tab:
  push   ecx                { Save input length. }
  push   edx                { Save output length. }
  mov    eax, edx           { Get current output length in EDX:EAX. }
  xor    edx, edx
  div    ebx                { Output length MOD TabSize in DX. }
  mov    ecx, ebx           { Calc number of spaces to insert... }
  sub    ecx, edx           {  = TabSize - Mod value. }
  pop    edx
  add    edx, ecx           { Add count of spaces into current output length. }

  mov    eax,$2020          { Blank in AH, Blank in AL. }
  shr    ecx, 1             { Store blanks. }
  rep    stosw
  adc    ecx, ecx
  rep    stosb
  pop    ecx                { Restore input length. }
  dec    ecx
  jnz    @@Next
  {jmp    @@Next}           { Back for next input. }

@@StoreLen:
  xor    ebx, ebx
  mov    [edi], bl          { Store terminating null }
  mov    eax, edx
  sub    edi, eax
  mov    [edi-StrOffset].LStrRec.Length, edx  { Store final length. }

@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;
end;

function ScrambleL(const S, Key : AnsiString) : AnsiString;
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
      Result[I] := AnsiChar(Byte(S[I]) xor Byte(Key[J]));
    Inc(I);
    Dec(J);
  end;
end;

function SubstituteL(const S, FromStr, ToStr : String) : String;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}
var
  I : Integer;
  P : Integer;
begin
  Result := S;
  if Length(FromStr) = Length(ToStr) then
    for I := 1 to Length(Result) do begin
      {P := System.Pos(S[I], FromStr);}
      {if P <> 0 then}
      if StrChPosL(FromStr, S[I], P) then
        Result[I] := ToStr[P];
    end;
end;

function FilterL(const S, Filters : String) : String;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}
var
  I : Cardinal;
  Len : Cardinal;
begin
  Len := 0;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
    if not CharExistsL(Filters, S[I]) then begin
      Inc(Len);
      Result[Len] := S[I];
    end;
  SetLength(Result, Len);
end;

  {--------------- Word / Char manipulation -------------------------}

function CharExistsL(const S : String; C : Char) : Boolean; register;
  {-Count the number of a given character in a string. }
{$IFDEF UNICODE}
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
  begin
    if S[I] = C then
    begin
      Result := True;
      Break;
    end;
  end;
end;
{$ELSE}
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx
  jmp   @@Done

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx
  jmp   @@Done

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx
  jmp   @@Done

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx
  jmp   @@Done

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;
{$ENDIF}

function CharCountL(const S : String; C : Char) : Cardinal; register;
  {-Count the number of a given character in a string. }
{$IFDEF UNICODE}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = C then
      Inc(Result);
end;
{$ELSE}
asm
  push  ebx
  xor   ecx, ecx
  or    eax, eax
  jz    @@Done
  mov   ebx, [eax-StrOffset].LStrRec.Length
  or    ebx, ebx
  jz    @@Done
  jmp   @@5

@@Loop:
  cmp   dl, [eax+3]
  jne   @@1
  inc   ecx

@@1:
  cmp   dl, [eax+2]
  jne   @@2
  inc   ecx

@@2:
  cmp   dl, [eax+1]
  jne   @@3
  inc   ecx

@@3:
  cmp   dl, [eax+0]
  jne   @@4
  inc   ecx

@@4:
  add   eax, 4
  sub   ebx, 4

@@5:
  cmp   ebx, 4
  jge   @@Loop

  cmp   ebx, 3
  je    @@1

  cmp   ebx, 2
  je    @@2

  cmp   ebx, 1
  je    @@3

@@Done:
  mov   eax, ecx
  pop   ebx
end;
{$ENDIF}

function WordCountL(const S, WordDelims : String) : Cardinal;
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
    while (I <= SLen) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Result);

    {find the end of the current word}
    while (I <= SLen) and not CharExistsL(WordDelims, S[I]) do
      Inc(I);
  end;
end;

function WordPositionL(N : Cardinal; const S, WordDelims : String;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}
var
  Count : Integer;
  I     : Integer;
begin
  Count := 0;
  I := 1;
  Result := False;

  while (I <= Length(S)) and (Count <> Integer(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and CharExistsL(WordDelims, S[I]) do
      Inc(I);

    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> Integer(N) then
      while (I <= Length(S)) and not CharExistsL(WordDelims, S[I]) do
        Inc(I)
    else begin
      Pos := I;
      Result := True;
    end;
  end;
end;

function ExtractWordL(N : Cardinal; const S, WordDelims : String) : String;
  {-Given an array of word delimiters, return the N'th word in a string.}
var
  C : Cardinal;
  I, J   : Integer;
begin
  Result := '';
  if WordPositionL(N, S, WordDelims, C) then begin
    I := C;
    {find the end of the current word}
    J := I;
    while (I <= Length(S)) and not
           CharExistsL(WordDelims, S[I]) do
      Inc(I);
    SetLength(Result, I-J);
    Move(S[J], Result[1], (I-J) * SizeOf(Char));
  end;
end;


function AsciiCountL(const S, WordDelims : String; Quote : Char) : Cardinal;
  {-Return the number of words in a string.}
var
  I       : Integer;
  InQuote : Boolean;
begin
  Result := 0;
  I := 1;
  InQuote := False;
  while I <= Length(S) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote)               
      and CharExistsL(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Result);
    {find the end of the current word}
    while (I <= Length(S)) and
          (InQuote or not CharExistsL(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not InQuote;
      Inc(I);
    end;
  end;
end;

function AsciiPositionL(N : Cardinal; const S, WordDelims : String;
                        Quote : Char; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}
var
  Count, I : Integer;
  InQuote  : Boolean;
begin
  Count := 0;
  InQuote := False;
  Result := False;
  I := 1;
  while (I <= Length(S)) and (Count <> Integer(N)) do begin
    {skip over delimiters}
    while (I <= Length(S)) and (S[I] <> Quote) and
          CharExistsL(WordDelims, S[I]) do
      Inc(I);
    {if we're not beyond end of S, we're at the start of a word}
    if I <= Length(S) then
      Inc(Count);
    {if not finished, find the end of the current word}
    if Count <> Integer(N) then
      while (I <= Length(S)) and (InQuote or not
             CharExistsL(WordDelims, S[I])) do begin
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

function ExtractAsciiL(N : Cardinal; const S, WordDelims : String;
                       Quote : Char) : String;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}
var
  C       : Cardinal;                                                  
  I, J    : Integer;
  InQuote : Boolean;
begin
  InQuote := False;
  if AsciiPositionL(N, S, WordDelims, Quote, C) then begin
    I := C;                                                            
    J := I;
    {find the end of the current word}
    while (I <= Length(S)) and ((InQuote)
      or not CharExistsL(WordDelims, S[I])) do begin
      if S[I] = Quote then
        InQuote := not(InQuote);
      Inc(I);
    end;
    SetLength(Result, I-J);
    Move(S[J], Result[1], I-J);
  end;
end;

procedure WordWrapL(const InSt : String; var OutSt, Overlap : String;
                   Margin : Integer; PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}
var
  InStLen  : Integer;
  EOS, BOS : Integer;
  Len : Integer;                                                       {!!.02}
begin
  InStLen := Length(InSt);

{!!.02 - Added }
  { handle empty string on input }
  if InStLen = 0 then begin
    OutSt := '';
    Overlap := '';
    Exit;
  end;
{!!.02 - End Added }

  {find the end of the output string}
  if InStLen > Margin then begin
    {find the end of the word at the margin, if any}
    EOS := Margin;
    while (EOS <= InStLen) and (InSt[EOS] <> ' ') do
      Inc(EOS);
    if EOS > InStLen then
      EOS := InStLen;

    {trim trailing blanks}
    while (EOS > 0) and (InSt[EOS] = ' ') do                           {!!.04}
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
        while (EOS > 0) and (InSt[EOS] = ' ') do                       {!!.04}
          Dec(EOS);
    end;
  end else
    EOS := InStLen;

  {copy the unwrapped portion of the line}
  if EOS > 0 then begin                                                {!!.04}
    SetLength(OutSt, EOS);
    Move(InSt[1], OutSt[1], Length(OutSt) * SizeOf(Char));
  end;                                                                 {!!.04}

  {find the start of the next word in the line}
  BOS := Succ(EOS);
  while (BOS <= InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS > InStLen then
    SetLength(OverLap, 0)
  else begin
    {copy from the start of the next word to the end of the line}

    SetLength(OverLap, InStLen);
    Move(InSt[BOS], Overlap[1], Succ(InStLen-BOS) * SizeOf(Char));
    SetLength(OverLap, Succ(InStLen-BOS));
  end;

  {pad the end of the output string if requested}
{!!.02 - Rewritten}
  Len := Length(OutSt);
  if PadToMargin and (Len < Integer(Margin)) then begin
//    SetLength(OutSt, Margin);
//    FillChar(OutSt[Succ(Len)], Integer(Margin)-Length(OutSt), ' ');
    OutSt := OutSt + StringOfChar(' ', Margin - Length(OutSt));
  end;
{!!.02 - End Rewritten}
end;

  {--------------- String comparison and searching -----------------}
function CompStringL(const S1, S2 : String) : Integer; register;
  {-Compare two strings.}
{$IFDEF UNICODE}
begin
  Result := AnsiCompareStr(S1, S2);
end;
{$ELSE}
asm
  push   edi
  mov    edi, edx           { EDI points to S2 }
  push   esi
  mov    esi, eax           { ESI points to S1 }

  xor    edx, edx
  xor    ecx, ecx

  or     edi, edi
  jz     @@1
  mov    edx, [edi-StrOffset].LStrRec.Length

@@1:
  or     esi, esi
  jz     @@2
  mov    ecx, [esi-StrOffset].LStrRec.Length

@@2:
  or     eax, -1            { EAX holds temporary result }

  cmp    ecx, edx           { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    eax                { S1 longer than S2 }
  mov    ecx, edx           { Length(S2) in CL }

@@EqLen:
  inc    eax                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if either is empty }

  repe   cmpsb              { Compare until no match or ECX = 0 }
  je     @@Done             { If Equal, result ready based on length }

  mov    eax, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     eax, -1            { Else S1 Less, Return -1 }

@@Done:
  pop    esi
  pop    edi
end;
{$ENDIF}

function CompUCStringL(const S1, S2 : String) : Integer; register;
  {-Compare two strings. This compare is not case sensitive.}
{$IFDEF UNICODE}
begin
  Result := AnsiCompareText(S1, S2);
end;
{$ELSE}
asm
  push   ebx                { Save registers }
  push   edi
  push   esi

  mov    edi, edx           { EDI points to S2 }
  mov    esi, eax           { ESI points to S1 }

  xor    eax, eax
  xor    ecx, ecx
  xor    edx, edx           { DL chars from S2 }
  or     ebx, -1

  or     edi, edi
  jz     @@1
  mov    eax, [edi-StrOffset].LStrRec.Length

@@1:
  or     esi, esi
  jz     @@2
  mov    ecx, [esi-StrOffset].LStrRec.Length

@@2:
  cmp    ecx, eax           { Compare lengths }
  je     @@EqLen            { Lengths equal? }
  jb     @@Comp             { Jump if S1 shorter than S1 }

  inc    ebx                { S1 longer than S2 }
  mov    ecx, eax           { Shorter length in ECX }

@@EqLen:
  inc    ebx                { Equal or greater }

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if lesser string is empty }

@@Start:
  xor    eax, eax           { EAX holds chars from S1 }
  mov    al, [esi]          { S1[?] into AL }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  mov    dl, [edi]          { S2[?] into DL }
  inc    edi                { Point EDI to next char in S2 }
  mov    dh, al
  mov    al, dl
  mov    dl, dh

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  cmp    dl, al             { Compare until no match }
  jne    @@Output
  dec    ecx
  jnz    @@Start

  je     @@Done             { If Equal, result ready based on length }

@@Output:
  mov    ebx, 1
  ja     @@Done             { S1 Greater? Return 1 }
  or     ebx, -1            { Else S1 Less, Return -1 }

@@Done:
  mov    eax, ebx           { Result into EAX }
  pop    esi                { Restore Registers }
  pop    edi
  pop    ebx
end;
{$ENDIF}

function SoundexL(const S : AnsiString) : AnsiString;
  {-Return 4 character soundex of an input string}
const
  SoundexTable : array[0..255] of Char =
    (#0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0,
    { A   B    C    D    E   F    G    H   I   J    K    L    M  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { N    O   P    Q    R    S    T    U   V    W   X    Y   X  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0,
    { a   b    c    d    e   f    g    h   i   j    k    l    m  }
     #0, '1', '2', '3', #0, '1', '2', #0, #0, '2', '2', '4', '5',
    { n    o   p    q    r    s    t    u   v    w   x    y   x  }
     '5', #0, '1', '2', '6', '2', '3', #0, '1', #0, '2', #0, '2',
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0, #0, #0, #0, #0, #0, #0, #0,
     #0, #0, #0);
begin
  if S = '' then Exit;
  SetLength(Result, 4);
asm
  push  edi
  mov   edi, [Result]            { EDI => output string. }
  mov   edi, [edi]
  push  ebx
  push  esi

  mov   esi, S                   { ESI => input string. }
  mov   dword ptr [edi], '0000'  { Initialize output string to '0000'. }
  xor   eax, eax
  mov   [edi+4], al              { Set null at end of string. }

  mov   ecx, [esi-StrOffset].LStrRec.Length
  or    ecx, ecx                 { Exit if null string. }
  jz    @@Done

  mov   al, [esi]                { Get first character of input string. }
  inc   esi

  push  ecx                      { Save ECX across call to CharUpper. }
  push  eax                      { Push Char onto stack for CharUpper. }
  call  CharUpper                { Uppercase AL. }
  pop   ecx                      { Restore saved register. }

  mov   [edi], al                { Store first output character. }
  inc   edi

  dec   ecx                      { One input character used. }
  jz    @@Done                   { Was input string one char long?. }

  mov   bh, 03h                  { Output max 3 chars beyond first. }
  mov   edx, offset SoundexTable { EDX => Soundex table. }
  xor   eax, eax                 { Prepare for address calc. }
  xor   bl, bl                   { BL will be used to store 'previous char'. }

@@Next:
  mov   al, [esi]                { Get next char in AL. }
  inc   esi
  mov   al, [edx+eax]            { Get soundex code into AL. }
  or    al, al                   { Is AL zero? }
  jz    @@NoStore                { If yes, skip this char. }
  cmp   bl, al                   { Is it the same as the previous stored char? }
  je    @@NoStore                { If yes, skip this char. }
  mov   [edi], al                { Store char to Dest. }
  inc   edi
  dec   bh                       { Decrement output counter. }
  jz    @@Done                   { If zero, we're done. }
  mov   bl, al                   { New previous character. }

@@NoStore:
  dec   ecx                      { Decrement input counter. }
  jnz   @@Next

@@Done:
  pop   esi
  pop   ebx
  pop   edi
end;
end;

function MakeLetterSetL(const S : AnsiString) : Integer; register;
  {-Return a bit-mapped long storing the individual letters contained in S.}
asm
  push   ebx                { Save registers }
  push   esi

  mov    esi, eax           { ESI => string }
  xor    ecx, ecx           { Zero ECX }
  xor    edx, edx           { Zero EDX }
  {or     edx, edx}
  or     eax, eax
  jz     @@Exit
  xor    eax, eax           { Zero EAX }
  add    ecx, [esi-StrOffset].LStrRec.Length
  jz     @@Exit             { Done if ECX is 0 }

@@Next:
  mov    al, [esi]          { EAX has next char in S }
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx

  sub    eax, 'A'           { Convert to bit number }
  cmp    eax, 'Z'-'A'       { Was char in range 'A'..'Z'? }
  ja     @@Skip             { Skip it if not }

  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx
  ror    edx, cl
  or     edx, 01h               { Set appropriate bit }
  rol    edx, cl
  mov    ebx, eax               { Exchange EAX and ECX }
  mov    eax, ecx
  mov    ecx, ebx

@@Skip:
  dec    ecx
  jnz    @@Next             { Get next character }

@@Exit:
  mov    eax, edx           { Move EDX to result }
  pop    esi                { Restore registers }
  pop    ebx
end;

{$IFDEF UNICODE}
procedure BMMakeTableL(const MatchString : UnicodeString; var BT : BTable);
begin
  // Do nothing until BMSearchL is fixed
{var
  I: Integer;
  Len: Byte;
begin
  Len := Length(MatchString);
  if Len > 255 then
    Len := 255;

  FillChar(BT, SizeOf(BT), Len);
  for I := 1 to Length(MatchString) - 1 do
    BT[Word(MatchString[I])] := Len - I; }
end;
{$ELSE}
procedure BMMakeTableL(const MatchString : AnsiString; var BT : BTable); register;
  {-Build a Boyer-Moore link table}
asm
  push  edi                { Save registers because they will be changed }
  push  esi
  mov   esi, eax           { Move EAX to ESI }
  push  ebx

  or    eax, eax
  jz    @@MTDone

  xor   eax, eax           { Zero EAX }
  mov   ecx, [esi-StrOffset].LStrRec.Length
  cmp   ecx, 0FFh          { If ECX > 255, force to 255 }
  jbe   @@1
  mov   ecx, 0FFh

@@1:
  mov   ch, cl             { Duplicate CL in CH }
  mov   eax, ecx           { Fill each byte in EAX with length }
  shl   eax, 16
  mov   ax, cx
  mov   edi, edx           { Point to the table }
  mov   ecx, 64            { Fill table bytes with length }
  rep   stosd
  cmp   al, 1              { If length <= 1, we're done }
  jbe   @@MTDone
  mov   edi, edx           { Reset EDI to beginning of table }
  xor   ebx, ebx           { Zero EBX }
  mov   cl, al             { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   al, [esi]          { Load table with positions of letters }
  mov   bl, al             { that exist in the search string }
  inc   esi
  mov   [edi+ebx], cl
  dec   cl
  jnz   @@MTNext

@@MTDone:
  pop   ebx                { Restore registers }
  pop   esi
  pop   edi
end;
{$ENDIF}

{$IFDEF UNICODE}
function BMSearchL(var Buffer; BufLength: Cardinal; var BT: BTable; // TODO-UNICODE
  const MatchString : String; out Pos : Cardinal) : Boolean;
var
  BufPtr: PChar;
//  s: string;
//  Len: Integer;
//  I,J,K: Integer;
begin
  // the commented code doesn't work correctly, so use a simple Pos for now
  BufPtr := PChar(@Buffer);
  Pos := System.Pos(MatchString, BufPtr);
  Exit(Pos <> 0);


(*  BufPtr := PChar(@Buffer);
  s := bufptr;
  Len := Length(MatchString);
//  if Len > 255 then
//    Len := 255;

  // Check to see if we have a trivial case
  if Len = 0 then
  begin
    Pos := 0;
    Exit(False);
  end
  else if Len = 1 then
  begin
    Pos := System.Pos(MatchString, BufPtr);
    Exit(Pos <> 0);
  end
  else
  begin
    I := Len;
    while I < BufLength do
    begin
      J := 0; // Matched letter count
      while J < Len do
      begin
        if s[I - J - 1] = MatchString[Len - J] then
          Inc(J)
        else
          Break;
      end;
      if J = Len then
      begin
        Pos := J;
        Exit(True);
      end;
      Inc(I, BT[Word(s[I])]);
    end;
  end;    *)

end;
{$ELSE}
function BMSearchL(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : AnsiString; var Pos : Cardinal) : Boolean; register;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  or    esi, esi
  jz    @@BMSNotFound
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  xor   eax, eax            { Zero EAX }

  mov   edx, [esi-StrOffset].LStrRec.Length
  cmp   edx, 0FFh          { If EDX > 255, force to 255 }
  jbe   @@1
  mov   edx, 0FFh

@@1:
  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  mov   esi, Pos            { Set position in Pos }
  {dec   edi}               { Found, calculate position }              
  sub   edi, ebx
  mov   eax, 1              { Set result to True }
  mov   [esi], edi
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  repe  cmpsb               { Compare MatchString to buffer }
  je    @@BMSFound          { If equal, string is found }

  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;
{$ENDIF}

{$IFDEF UNICODE}
function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;  // TODO-UNICODE
  const MatchString : String ; var Pos : Cardinal) : Boolean; register;
var
  BufPtr: PChar;
begin
  BufPtr := PChar(@Buffer);
  Pos := System.Pos(AnsiUpperCase(MatchString), AnsiUpperCase(BufPtr));
  Exit(Pos <> 0);
end;
{$ELSE}

function BMSearchUCL(var Buffer; BufLength : Cardinal; var BT : BTable;
  const MatchString : AnsiString ; var Pos : Cardinal) : Boolean; register;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  or    esi, esi
  jz    @@BMSNotFound
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  xor   eax, eax            { Zero EAX }

  mov   edx, [esi-StrOffset].LStrRec.Length
  cmp   edx, 0FFh           { If EDX > 255, force to 255 }
  jbe   @@1
  mov   edx, 0FFh

@@1:
  or    dl, dl              { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  push  eax                 { Push Char onto stack for CharUpper }
  call  CharUpper
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  jecxz @@BMSFound          { If ECX is zero, string is found }

@@StringComp:
  xor   eax, eax
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  call  CharUpper
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  mov   al, [esi]           { Get MatchString char }
  dec   esi
  cmp   ah, al              { Compare }
  loope @@StringComp        { OK?  Get next character }
  je    @@BMSFound          { Matched! }

  xor   ah, ah              { Zero AH }
  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
end;
{$ENDIF}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionL(const Name, Ext : String) : String;
  {-Return a file name with a default extension attached.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := Name
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function ForceExtensionL(const Name, Ext : String) : String;
  {-Force the specified extension onto the file name.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := System.Copy(Name, 1, DotPos) + Ext
  else if Name = '' then
    Result := ''
  else
    Result := Name + '.' + Ext;
end;

function JustFilenameL(const PathName : String) : String;
  {-Return just the filename and extension of a pathname.}
var
  I : Cardinal;
begin
  Result := '';
  if PathName = '' then Exit;
  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or CharInSet(PathName[I], DosDelimSet);                         {!!.01}
  Result := System.Copy(PathName, Succ(I), StMaxFileLen);
end;

function JustNameL(const PathName : String) : String;
  {-Return just the filename (no extension, path, or drive) of a pathname.}
var
  DotPos : Cardinal;
  S      : string;
begin
  S := JustFileNameL(PathName);
  if HasExtensionL(S, DotPos) then
    S := System.Copy(S, 1, DotPos-1);
  Result := S;
end;

function JustExtensionL(const Name : String) : String;
  {-Return just the extension of a pathname.}
var
  DotPos : Cardinal;
begin
  if HasExtensionL(Name, DotPos) then
    Result := System.Copy(Name, Succ(DotPos), StMaxFileLen)
  else
    Result := '';
end;

function JustPathnameL(const PathName : String) : String;
  {-Return just the drive and directory portion of a pathname.}
var
  I : Cardinal;
begin
  if PathName = '' then Exit;

  I := Succ(Cardinal(Length(PathName)));
  repeat
    Dec(I);
  until (I = 0) or CharInSet(PathName[I], DosDelimSet);                         {!!.01}

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

function AddBackSlashL(const DirName : String) : String;
  {-Add a default backslash to a directory name}
begin
  Result := DirName;
  if (Length(Result) = 0) then
    Exit;
  if ((Length(Result) = 2) and (Result[2] = ':')) or
     ((Length(Result) > 2) and (Result[Length(Result)] <> '\')) then
    Result := Result + '\';
end;

function CleanFileNameL(const FileName : string) : string;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos : Cardinal;
  NameLen : Word;
begin
  if HasExtensionL(FileName, DotPos) then begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := Pred(DotPos);
    if NameLen > 8 then
      NameLen := 8;
    Result := System.Copy(FileName, 1, NameLen)+System.Copy(FileName, DotPos, 4);
  end else
    {Take the first 8 chars of name}
    Result := System.Copy(FileName, 1, 8);
end;

function CleanPathNameL(const PathName : String) : String;
  {-Return a pathname cleaned up as DOS does it.}
var
  I : Cardinal;
  S : String;
begin
  SetLength(Result, 0);
  S := PathName;

  I := Succ(Cardinal(Length(S)));
  repeat
    dec(I);
    if I > 2 then
      if (S[I] = '\') and (S[I-1] = '\') then
        if (S[I-2] <> ':') then
          System.Delete(S, I, 1);
  until I <= 0;

  I := Succ(Cardinal(Length(S)));
  repeat
    {Get the next directory or drive portion of pathname}
    repeat
      Dec(I);
    until (I = 0) or CharInSet(S[I], DosDelimSet);                            {!!.02}

    {Clean it up and prepend it to output string}
    Result := CleanFileNameL(System.Copy(S, Succ(I), StMaxFileLen)) + Result;
    if I > 0 then begin
      Result := S[I] + Result;
      System.Delete(S, I, 255);
    end;
  until I <= 0;

end;

function HasExtensionL(const Name : String; var DotPos : Cardinal) : Boolean;
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
    and not CharExistsL(System.Copy(Name, Succ(DotPos), StMaxFileLen), '\');
end;

  {------------------ Formatting routines --------------------}


function CommaizeChL(L : Integer; Ch : Char) : String;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp : string;
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := Long2StrL(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;

function CommaizeL(L : Integer) : String;
  {-Convert a long integer to a string with commas}
begin
  Result := CommaizeChL(L, ',');
end;

function FormPrimL(const Mask : String; R : TstFloat; const LtCurr, RtCurr : String;
                  Sep, DecPt : Char; AssumeDP : Boolean) : String;
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
  FormChars : string = '#@*$-+,.';
  PlusArray : array[Boolean] of Char = ('+', '-');
  MinusArray : array[Boolean] of Char = (' ', '-');
  FillArray : array[Blank..Zero] of Char = (' ', '*', '0');
var
  sBuffer: ShortString;
  S : string;              {temporary string}
  Filler : Integer;        {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative : Boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF : Integer;          {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Blanks,                  {# of blanks returned by Str}
  Places,                  {# of digits after the '.'}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr : Byte;         {pointer into temporary string of digits}
  I : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Mask;
  if (not AssumeDP) and (not CharExistsL(Result, '.')) then
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
    and not CharExistsL(FormChars, Result[StartF]) do
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
  Str(R:Digits:Places, sBuffer);
  S := string(sBuffer);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
//    I := Length(S);
//    SetLength(S, Integer(I) + (Places-MaxPlaces));
//    FillChar(S[Succ(I)], Places-MaxPlaces, '0');
    S := StringOfChar('0', Places-MaxPlaces) + S;
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
    //FillChar(S[1], Blanks, FillArray[Filler]);
    Delete(S, 1, Blanks);
    S := StringOfChar(FillArray[Filler], Blanks) + S;

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
      S := LeftPadL(S, RtChars);
    Result := Result + S;
  end;
  if LtChars > 0 then begin
    S := LtCurr;
    if Length(S) > LtChars then
      SetLength(S, LtChars)
    else
      S := PadL(S, LtChars);
    Result := S + Result;
  end;
end;

function FloatFormL(const Mask : String ; R : TstFloat ; const LtCurr,
                    RtCurr : String ; Sep, DecPt : Char) : String;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimL(Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormL(const Mask : String ; L : Integer ; const LtCurr,
                      RtCurr : String ; Sep : Char) : String;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimL(Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosL(const P : String; C : Char; var Pos : Integer) : Boolean;
  {-Return the position of a specified character within a string.}
{$IFDEF UNICODE}
begin
  Pos := System.Pos(C, P);
  Result := Pos <> 0;
end;
{$ELSE}
asm
  push  ebx             { Save registers }
  push  edi

  or    eax, eax        { Protect against null string }
  jz    @@NotFound

  xor   edi, edi        { Zero counter }
  mov   ebx, [eax-StrOffset].LStrRec.Length  { Get input length }

@@Loop:
  inc   edi             { Increment counter }
  cmp   [eax], dl       { Did we find it? }
  jz    @@Found
  inc   eax             { Increment pointer }

  cmp   edi, ebx        { End of string? }
  jnz   @@Loop          { If not, loop }

@@NotFound:
  xor   eax, eax        { Not found, zero EAX for False }
  mov   [ecx], eax
  jmp   @@Done

@@Found:
  mov   [ecx], edi      { Set Pos }
  mov   eax, 1          { Set EAX to True }

@@Done:
  pop   edi             { Restore registers }
  pop   ebx
end;
{$ENDIF}

function StrStPosL(const P, S : String; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}
begin
  Pos := System.Pos(S, P);
  Result := Pos <> 0;
end;

function StrStCopyL(const S : String; Pos, Count : Cardinal) : String;
  {-Copy characters at a specified position in a string.}
begin
  Result := System.Copy(S, Pos, Count);
end;

function StrChInsertL(const S : String; C : Char; Pos : Cardinal) : String;
var
  Temp : string;
begin
  SetLength(Temp, 1);
  Temp[1] := C;
  Result := S;
  System.Insert(Temp, Result, Pos);
end;

function StrStInsertL(const S1, S2 : String; Pos : Cardinal) : String;
  {-Insert a string into another string at a specified position.}
begin
  Result := S1;
  System.Insert(S2, Result, Pos);
end;

function StrChDeleteL(const S : String; Pos : Cardinal) : String;
  {-Delete the character at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, 1);
end;

function StrStDeleteL(const S : String; Pos, Count : Cardinal) : String;
  {-Delete characters at a specified position in a string.}
begin
  Result := S;
  System.Delete(Result, Pos, Count);
end;


{----------------------------------------------------------------------------}

function CopyLeftL(const S : String; Len : Cardinal) : String;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, 1, Len);
end;

{----------------------------------------------------------------------------}

function CopyMidL(const S : String; First, Len : Cardinal) : String;
  {-Return the mid part of a string}
begin
  if (Integer(First) > Length(S)) or (Integer(Len) < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Len);
end;

{----------------------------------------------------------------------------}

function CopyRightL(const S : String; First : Cardinal) : String;
  {-Return the right Len characters of a string}
begin
  if (Integer(First) > Length(S)) or (First < 1) or (S = '') then
    Result := ''
  else
    Result := Copy(S, First, Length(S));
end;

{----------------------------------------------------------------------------}

function CopyRightAbsL(const S : String; NumChars : Cardinal) : String;
  {-Return NumChar characters starting from end}
begin
  if (Cardinal(Length(S)) > NumChars) then
    Result := Copy(S, (Cardinal(Length(S)) - NumChars)+1, NumChars)
  else
    Result := S;
end;

{----------------------------------------------------------------------------}

function WordPosL(const S, WordDelims, AWord : String;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Nth instance of a given word within a string}
var
  TmpStr : String;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S = '') or (AWord = '') or (pos(AWord, S) = 0) or (N < 1) then begin
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
//                ((pos(TmpStr[P2+1], WordDelims) > 0) or              {!!.02}
//                 (Integer(P2+1) = Length(TmpStr))) then begin        {!!.02}
                ((Integer(P2+1) >= Length(TmpStr)) or                  {!!.02}
                (pos(TmpStr[P2+1], WordDelims) > 0)) then begin        {!!.02}
      Inc(I);
    end else if ((Integer(P1 + pred(Len))) = Length(TmpStr)) then begin
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


{----------------------------------------------------------------------------}

function CopyFromNthWordL(const S, WordDelims : String;
                          const AWord : String; N : Cardinal;      {!!.02}
                          var SubString : String) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosL(S, WordDelims, AWord, N, P)) then begin
    SubString := Copy(S, P, Length(S));
    Result := True;
  end else begin
    SubString := '';
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromNthWordL(const S, WordDelims : String;
                            const AWord : String; N : Cardinal;    {!!.02}
                            var SubString : String) : Boolean;
var
  P : Cardinal;
begin
  SubString := S;
  if (WordPosL(S, WordDelims, AWord, N, P)) then begin
    Result := True;
    SubString := Copy(S, 1, P-1);
  end else begin
    Result := False;
    SubString := '';
  end;
end;

{----------------------------------------------------------------------------}

function CopyFromToWordL(const S, WordDelims, Word1, Word2 : String;
                         N1, N2 : Cardinal;
                         var SubString : String) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosL(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosL(S, WordDelims, Word2, N2, P2)) then begin
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

{----------------------------------------------------------------------------}

function DeleteFromToWordL(const S, WordDelims, Word1, Word2 : String;
                           N1, N2 : Cardinal;
                           var SubString : String) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  SubString := S;
  if (WordPosL(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosL(S, WordDelims, Word2, N2, P2)) then begin
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

{----------------------------------------------------------------------------}

function CopyWithinL(const S, Delimiter : String;
                     Strip : Boolean) : String;
var
  P1,
  P2     : Cardinal;
  TmpStr : String;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosL(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, Integer(P1) + Length(Delimiter), Length(S));
      if StrStPosL(TmpStr, Delimiter, P2) then begin
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

{----------------------------------------------------------------------------}

function DeleteWithinL(const S, Delimiter : String) : String;
var
  P1,
  P2     : Cardinal;
  TmpStr : String;
begin
  if (S = '') or (Delimiter = '') or (pos(Delimiter, S) = 0) then
    Result := ''
  else begin
    if (StrStPosL(S, Delimiter, P1)) then begin
      TmpStr := Copy(S, Integer(P1) + Length(Delimiter), Length(S));
      if (pos(Delimiter, TmpStr) = 0) then
        Result := Copy(S, 1, P1-1)
      else begin
        if (StrStPosL(TmpStr, Delimiter, P2)) then begin
          Result := S;
          P2 := Integer(P2) + (2*Length(Delimiter));
          System.Delete(Result, P1, P2);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function ReplaceWordL(const S, WordDelims, OldWord, NewWord : String;
                      N : Cardinal;
                      var Replacements : Cardinal) : String;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S = '') or (WordDelims = '') or (OldWord = '') or
     (pos(OldWord, S) = 0) then begin
    Result := S;
    Replacements := 0;
    Exit;
  end;

  if (WordPosL(S, WordDelims, OldWord, N, P1)) then begin
    Result := S;
    System.Delete(Result, P1, Length(OldWord));

    C := 0;
    for I := 1 to Replacements do begin
      if ((Length(NewWord)) + Length(Result)) < MaxLongInt then begin
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


function ReplaceWordAllL(const S, WordDelims, OldWord, NewWord : String;
                         var Replacements : Cardinal) : String;
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
    while (WordPosL(Result, WordDelims, OldWord, 1, P1)) do begin
      System.Delete(Result, P1, Length(OldWord));
      for I := 1 to Replacements do begin
        if ((Length(NewWord) + Length(Result)) < MaxLongInt) then begin
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


{----------------------------------------------------------------------------}

function ReplaceStringL(const S, OldString, NewString : String;
                        N : Cardinal;
                        var Replacements : Cardinal) : String;
var
  I,
  C,
  P1 : Cardinal;
  TmpStr : String;
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
  while (I < N) and (Integer(C) < Length(TmpStr)) do begin
    Inc(I);
    System.Delete(TmpStr, 1, Integer(P1) + Length(OldString));
    Inc(C, Integer(P1) + Length(OldString));
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


function ReplaceStringAllL(const S, OldString, NewString : String;
                           var Replacements : Cardinal) : String;
var
  I,
  C  : Cardinal;
  P1 : Integer;
  Tmp: String;
begin
  if (S = '') or (OldString = '') or (Pos(OldString, S) = 0) then
  begin
    Result := S;
    Replacements := 0;
  end
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


function LastWordL(const S, WordDelims, AWord : String;
                   var Position : Cardinal) : Boolean;
var
  TmpStr : String;
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



function LastWordAbsL(const S, WordDelims : String;
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



function LastStringL(const S, AString : String;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : String;
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
    Inc(C, Integer(I) + Length(AString));
    System.Delete(TmpStr, 1, Integer(I) + Length(AString));
    I := pos(AString, TmpStr);
  end;
{Go back the length of AString since the while loop deletes the last instance}
  Dec(C, Length(AString));
  Position := C;
  Result := True;
end;



function KeepCharsL(const S, Chars : String) : String;
var
  FromInx : Cardinal;
  ToInx   : Cardinal;
begin
  {if either the input string or the list of acceptable chars is empty
   the destination string will also be empty}
  if (S = '') or (Chars = '') then begin
    Result := '';
    Exit;
  end;

  {set the maximum length of the result string (it could be less than
   this, of course}
  SetLength(Result, length(S));

  {start off the to index}
  ToInx := 0;

  {in a loop, copy over the chars that match the list}
  for FromInx := 1 to length(S) do
    if CharExistsL(Chars, S[FromInx]) then begin
      inc(ToInx);
      Result[ToInx] := S[FromInx];
    end;

  {make sure that the length of the result string is correct}
  SetLength(Result, ToInx);
end;



function RepeatStringL(const RepeatString : String;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : String;
var
  i    : Cardinal;
  Len  : Cardinal;
  ActualReps : Cardinal;
begin
  Result := '';
  if (MaxLen <> 0) and
     (Repetitions <> 0) and
     (RepeatString <> '') then begin
    Len := length(RepeatString);
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then begin
      SetLength(Result, ActualReps * Len);
      for i := 0 to pred(ActualReps) do
        Move(RepeatString[1], Result[i * Len + 1], Len * SizeOf(Char));
    end;
  end;
end;



function TrimCharsL(const S, Chars : String) : String;
begin
  Result := RightTrimCharsL(S, Chars);
  Result := LeftTrimCharsL(Result, Chars);
end;



function RightTrimCharsL(const S, Chars : String) : String;
var
  CutOff : integer;
begin
  CutOff := length(S);
  while (CutOff > 0) do begin
    if not CharExistsL(Chars, S[CutOff]) then
      Break;
    dec(CutOff);
  end;
  if (CutOff = 0) then
    Result := ''
  else
    Result := Copy(S, 1, CutOff);
end;



function LeftTrimCharsL(const S, Chars : String) : String;
var
  CutOff : integer;
  LenS   : integer;
begin
  LenS := length(S);
  CutOff := 1;
  while (CutOff <= LenS) do begin
    if not CharExistsL(Chars, S[CutOff]) then
      Break;
    inc(CutOff);
  end;
  if (CutOff > LenS) then
    Result := ''
  else
    Result := Copy(S, CutOff, LenS - CutOff + 1);
end;



function ExtractTokensL(const S, Delims: String;
                        QuoteChar  : Char;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal; overload;
var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : Char;
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
     CharExistsL(Delims, QuoteChar) then
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
          else if CharExistsL(Delims, CurChar) then begin

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
          else if CharExistsL(Delims, CurChar) then begin

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
          else if CharExistsL(Delims, CurChar) then begin
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

function ContainsOnlyL(const S, Chars : String;
                       var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (not CharExistsL(Chars, S[I])) then begin
        BadPos := I;
        Result := False;
        Exit;
      end;
    end;
    Result := True;
    BadPos := 0;
  end;
end;



function ContainsOtherThanL(const S, Chars : String;
                            var BadPos : Cardinal) : Boolean;
var
  I : Cardinal;
begin
  if (S = '') then begin
    Result := False;
    BadPos := 0;
  end else begin
    for I := 1 to Length(S) do begin
      if (CharExistsL(Chars, S[I])) then begin
        BadPos := I;
        Result := True;
        Exit;
      end;
    end;
    Result := False;
    BadPos := 0;
  end;
end;



function IsChAlphaL(C : Char) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := Windows.IsCharAlpha(C);
end;



function IsChNumericL(C : Char; const Numbers : String) : Boolean; {!!.02}
 {-Returns true if Ch in numeric set}
begin
  Result := CharExistsL(Numbers, C);
end;



function IsChAlphaNumericL(C : Char; const Numbers : String) : Boolean; {!!.02}
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := Windows.IsCharAlpha(C) or CharExistsL(Numbers, C);
end;



function IsStrAlphaL(const S : String) : Boolean;
  {-Returns true if all characters in string are an alpha}
var
  I : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for I := 1 to Length(S) do
      if not Windows.IsCharAlpha(S[I]) then
        Exit;
    Result := true;
  end;
end;



function IsStrNumericL(const S, Numbers : String) : Boolean;
  {-Returns true if all characters in string are in numeric set}
var
  i : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for i := 1 to Length(S) do
      if not CharExistsL(Numbers, S[i]) then
        Exit;
    Result := true;
  end;
end;



function IsStrAlphaNumericL(const S, Numbers : String) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
var
  i : Cardinal;
begin
  Result := false;
  if (length(S) > 0) then begin
    for I := 1 to Length(S) do
      if (not Windows.IsCharAlpha(S[i])) and
         (not CharExistsL(Numbers, S[i])) then
        Exit;
    Result := true;
  end;
end;


function StrWithinL(const S, SearchStr : string;
                    Start : Cardinal;
                    var Position : Cardinal) : boolean;
var
  TmpStr : string;
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
