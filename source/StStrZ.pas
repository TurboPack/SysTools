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
{* SysTools: StStrZ.pas 4.04                             *}
{*********************************************************}
{* SysTools: Null terminated string routines             *}
{*********************************************************}

{$I StDefine.inc}

unit StStrZ;

interface

uses
  Windows,
  Classes, SysUtils, StConst, StBase;

  {-------- Numeric conversion -----------}

function HexBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar; overload;
function HexBZ(Dest : PWideChar; B : Byte) : PWideChar; overload;
  {-Return the hex string for a byte.}

function HexWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return the hex string for a word.}

function HexLZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Return the hex string for a long integer.}

function HexPtrZ(Dest : PAnsiChar; P : Pointer) : PAnsiChar;
  {-Return the hex string for a pointer.}

function BinaryBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return a binary string for a byte.}

function BinaryWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return the binary string for a word.}

function BinaryLZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Return the binary string for a long integer.}

function OctalBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return an octal string for a byte.}

function OctalWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return an octal string for a word.}

function OctalLZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Return an octal string for a long integer.}

function Str2Int16Z(S : PAnsiChar; var I : SmallInt) : Boolean;
  {-Convert a string to an SmallInt.}

function Str2WordZ(S : PAnsiChar; var I : Word) : Boolean;
  {-Convert a string to a word.}

function Str2LongZ(S : PAnsiChar; var I : Integer) : Boolean;
  {-Convert a string to a long integer.}

{$IFDEF VER93}
function Str2RealZ(S : PAnsiChar; var R : Double) : Boolean;
{$ELSE}
function Str2RealZ(S : PChar; var R : Real) : Boolean;
{$ENDIF}
  {-Convert a string to a real.}

function Str2ExtZ(S : PAnsiChar; var R : Extended) : Boolean;
  {-Convert a string to an extended.}

function Long2StrZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Convert an integer type to a string.}

function Real2StrZ(Dest : PAnsiChar; R : Double; Width : Byte;
                  Places : ShortInt) : PAnsiChar;
  {-Convert a real to a string.}

function Ext2StrZ(Dest : PAnsiChar; R : Extended; Width : Byte;
                 Places : ShortInt) : PAnsiChar;
  {-Convert an extended to a string.}

function ValPrepZ(S : PAnsiChar) : PAnsiChar; overload;
{$IFDEF UNICODE}
function ValPrepZ(S : PWideChar) : PWideChar; overload;
{$ENDIF}
  {-Prepares a string for calling Val.}


  {-------- General purpose string manipulation --------}

function CharStrZ(Dest : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string filled with the specified character.}

function PadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with a specified character.}

function PadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with spaces.}

function LeftPadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with a specified character.}

function LeftPadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with spaces.}

function PadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with a specified character.
    This primitive version modifies the source string directly.}

function PadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the right with spaces. This primitive version modifies the
    source string directly.}

function LeftPadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with a specified character. This primitive
    version modifies the source string directly.}

function LeftPadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left with spaces. This primitive version modifies the
    source string directly.}

function TrimLeadZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading white space removed.}

function TrimTrailZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed.}

function TrimZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed.}

function TrimSpacesZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed.}

function TrimLeadPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading white space removed. This primitive version
    modifies the source string directly.}

function TrimTrailPrimZ(S : PAnsiChar) : PAnsiChar; overload;
function TrimTrailPrimZ(S : PWideChar) : PWideChar; overload;
  {-Return a string with trailing white space removed. This primitive version
    modifies the source string directly.}

function TrimPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed. This
    primitive version modifies the source string directly.}

function TrimSpacesPrimZ(S : PAnsiChar) : PAnsiChar; overload;
{$IFDEF UNICODE}
function TrimSpacesPrimZ(S : PWideChar) : PWideChar; overload;
{$ENDIF}
  {-Return a string with leading and trailing spaces removed. This primitive
    version modifies the source string directly.}

function CenterChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with a specified character. This
    primitive version modifies the source string directly.}

function CenterPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with spaces. This primitive version
    modifies the source string directly.}

function CenterChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with a specified character.}

function CenterZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Pad a string on the left and right with spaces.}

function EntabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;
  {-Convert blanks in a string to tabs.}

function DetabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;
  {-Expand tabs in a string to blanks.}

function ScramblePrimZ(S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption. This
    primitive version modifies the source string directly.}

function ScrambleZ(Dest, S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}

function SubstituteZ(Dest, Src, FromStr, ToStr : PAnsiChar) : PAnsiChar;
  {-Map the characters found in FromStr to the corresponding ones in ToStr.}

function FilterZ(Dest, Src, Filters : PAnsiChar) : PAnsiChar;
  {-Remove characters from a string. The characters to remove are specified in
    ChSet.}

  {--------------- Word / Char manipulation -------------------------}

function CharExistsZ(S : PAnsiChar; C : AnsiChar) : Boolean; overload;
function CharExistsZ(S : PWideChar; C : Char) : Boolean;     overload;
  {-Determine whether the given character exists in a string. }

function CharCountZ(S : PAnsiChar; C : AnsiChar) : Cardinal;
  {-Count the number of a given character in a string. }

function WordCountZ(S : PAnsiChar; WordDelims : PAnsiChar) : Cardinal;
  {-Given an array of word delimiters, return the number of words in a string.}

function WordPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                      var Pos : Cardinal) : Boolean;
  {-Given an array of word delimiters, set Pos to the start position of the
    N'th word in a string.  Result indicates success/failure.}

function ExtractWordZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                     WordDelims : PAnsiChar) : PAnsiChar;
  {-Given an array of word delimiters, return the N'th word in a string.}

function AsciiCountZ(S : PAnsiChar; WordDelims : PAnsiChar; Quote : AnsiChar) : Cardinal;
  {-Return the number of words in a string.}

function AsciiPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of the N'th word in a string.}

function ExtractAsciiZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                      WordDelims : PAnsiChar; Quote : AnsiChar) : PAnsiChar;
  {-Given an array of word delimiters, return the N'th word in a string. Any
    text within Quote characters is counted as one word.}

procedure WordWrapZ(Dest : PAnsiChar; InSt, Overlap : PAnsiChar;
                   Margin : Cardinal;
                   PadToMargin : Boolean);
  {-Wrap a text string at a specified margin.}

  {--------------- String comparison and searching -----------------}
function CompStringZ(S1, S2 : PAnsiChar) : Integer;
  {-Compare two strings.}

function CompUCStringZ(S1, S2 : PAnsiChar) : Integer;
  {-Compare two strings. This compare is not case sensitive.}

function SoundexZ(Dest : PAnsiChar; S : PAnsiChar) : PAnsiChar;
  {-Return 4 character soundex of an input string}

function MakeLetterSetZ(S : PAnsiChar) : Integer;
  {-Return a bit-mapped long storing the individual letters contained in S.}

procedure BMMakeTableZ(MatchString : PAnsiChar; var BT : BTable);
  {-Build a Boyer-Moore link table}

function BMSearchZ(var Buffer; BufLength : Cardinal; var BT : BTable;
                  MatchString : PAnsiChar ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string.}

function BMSearchUCZ(var Buffer; BufLength : Cardinal; var BT : BTable;
                    MatchString : PAnsiChar ; var Pos : Cardinal) : Boolean;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}

  {--------------- DOS pathname parsing -----------------}

function DefaultExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Return a file name with a default extension attached.}

function ForceExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Force the specified extension onto the file name.}

function JustFilenameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar; overload;
{$IFDEF UNICODE}
function JustFilenameZ(Dest : PWideChar; PathName : PWideChar) : PWideChar; overload;
{$ENDIF}
  {-Return just the filename and extension of a pathname.}

function JustNameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the filename (no extension, path, or drive) of a pathname.}

function JustExtensionZ(Dest : PAnsiChar; Name : PAnsiChar) : PAnsiChar;
  {-Return just the extension of a pathname.}

function JustPathnameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the drive and directory portion of a pathname.}

function AddBackSlashZ(Dest : PAnsiChar; DirName : PAnsiChar) : PAnsiChar;
  {-Add a default backslash to a directory name.}

function CleanPathNameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return a pathname cleaned up as DOS does it.}

function HasExtensionZ(Name : PAnsiChar; var DotPos : Cardinal) : Boolean; overload;
{$IFDEF UNICODE}
function HasExtensionZ(Name : PWideChar; var DotPos : Cardinal) : Boolean; overload;
{$ENDIF}
  {-Determine if a pathname contains an extension and, if so, return the
    position of the dot in front of the extension.}

  {------------------ Formatting routines --------------------}

function CommaizeZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Convert a long integer to a string with commas.}

function CommaizeChZ(Dest : PAnsiChar; L : Integer; Ch : AnsiChar) : PAnsiChar;
  {-Convert a long integer to a string with Ch in comma positions.}

function FloatFormZ(Dest, Mask : PAnsiChar ; R : TstFloat ; LtCurr,
                    RtCurr : PAnsiChar ; Sep, DecPt : AnsiChar) : PAnsiChar;
  {-Return a formatted string with digits from R merged into mask.}

function LongIntFormZ(Dest, Mask : PAnsiChar ; L : Integer ; LtCurr,
                     RtCurr : PAnsiChar ; Sep : AnsiChar) : PAnsiChar;
  {-Return a formatted string with digits from L merged into mask.}

function StrChPosZ(P : PAnsiChar; C : AnsiChar; var Pos : Cardinal) : Boolean; overload;
{$IFDEF UNICODE}
function StrChPosZ(P : PWideChar; C : Char; var Pos : Cardinal) : Boolean; overload;
{$ENDIF}
  {-Return the position of a specified character within a string.}

function StrStPosZ(P, S : PAnsiChar; var Pos : Cardinal) : Boolean;
  {-Return the position of a specified substring within a string.}

function StrStCopyZ(Dest, S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar; overload;
{$IFDEF UNICODE}
function StrStCopyZ(Dest, S : PWideChar; Pos, Count : Cardinal) : PWideChar; overload;
{$ENDIF}
  {-Copy characters at a specified position in a string.}

function StrChInsertZ(Dest, S : PAnsiChar; C : AnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a character into a string at a specified position.}

function StrStInsertZ(Dest, S1, S2 : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a string into another string at a specified position.}

function StrChDeleteZ(Dest, S : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Delete the character at a specified position in a string.}

function StrStDeleteZ(Dest, S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
  {-Delete characters at a specified position in a string.}

function StrChInsertPrimZ(Dest : PAnsiChar; C : AnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a character into a string at a specified position. This primitive
    version modifies the source string directly.}

function StrStInsertPrimZ(Dest, S : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Insert a string into another string at a specified position. This
    primitive version modifies the source string directly.}

function StrChDeletePrimZ(P : PAnsiChar; Pos : Cardinal) : PAnsiChar;
  {-Delete the character at a specified position in a string. This primitive
    version modifies the source string directly.}

function StrStDeletePrimZ(P : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar; overload;
function StrStDeletePrimZ(P : PWideChar; Pos, Count : Cardinal) : PWideChar; overload;
  {-Delete characters at a specified position in a string. This primitive
    version modifies the source string directly.}


{--------------------------  New Functions -----------------------------------}

function ContainsOnlyZ(const S, Chars : PAnsiChar;
                       var BadPos : Cardinal) : Boolean;

function ContainsOtherThanZ(const S, Chars : PAnsiChar;
                            var BadPos : Cardinal) : Boolean;

function CopyFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                          N : Cardinal) : Boolean;

function CopyFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                         N1, N2 : Cardinal) : Boolean;

function CopyLeftZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return the left Len characters of a string}

function CopyMidZ(Dest, S : PAnsiChar; First, Len : Cardinal) : PAnsiChar;
  {-Return the mid part of a string}

function CopyRightZ(Dest, S : PAnsiChar; First : Cardinal) : PAnsiChar;
  {-Return the right Len characters of a string}

function CopyRightAbsZ(Dest, S : PAnsiChar; NumChars : Cardinal) : PAnsiChar;
  {-Return the right Len characters of a string}


function CopyWithinZ(Dest, S, Delimiter : PAnsiChar;
                     Strip : Boolean) : PAnsiChar;

function DeleteFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                            N : Cardinal) : Boolean;

function DeleteFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                           N1, N2 : Cardinal) : Boolean;

function DeleteWithinZ(Dest, S, Delimiter : PAnsiChar) : PAnsiChar;

function ExtractTokensZ(S, Delims   : PAnsiChar;
                        QuoteChar   : AnsiChar;
                        AllowNulls  : Boolean;
                        Tokens      : TStrings) : Cardinal;

function IsChAlphaZ(C : Char) : Boolean;
 {-Returns true if Ch is an alpha}

function IsChNumericZ(C : AnsiChar; Numbers : PAnsiChar) : Boolean;
 {-Returns true if Ch in numeric set}

function IsChAlphaNumericZ(C : Char; Numbers : PChar) : Boolean;
  {-Returns true if Ch is an alpha or numeric}

function IsStrAlphaZ(S : PChar) : Boolean;
  {-Returns true if all characters in string are an alpha}

function IsStrNumericZ(S, Numbers : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are in numeric set}

function IsStrAlphaNumericZ(S, Numbers : PChar) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}

function KeepCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

function LastStringZ(S, AString : PAnsiChar;
                     var Position : Cardinal) : Boolean;

function LastWordZ(S, WordDelims, AWord : PAnsiChar;
                   var Position : Cardinal) : Boolean;


function LastWordAbsZ(S, WordDelims : PAnsiChar;
                      var Position : Cardinal) : Boolean;

function LeftTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

function RepeatStringZ(Dest, RepeatString : PAnsiChar;
                       var Repetitions : Cardinal;
                       MaxLen : Cardinal) : PAnsiChar;

function ReplaceWordZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                      N : Cardinal;
                      var Replacements : Cardinal) : PAnsiChar;

function ReplaceWordAllZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                         var Replacements : Cardinal) : PAnsiChar;

function ReplaceStringZ(Dest, S, OldString, NewString : PAnsiChar;
                        N : Cardinal;
                        var Replacements : Cardinal) : PAnsiChar;

function ReplaceStringAllZ(Dest, S, OldString, NewString : PAnsiChar;
                           var Replacements : Cardinal) : PAnsiChar;

function RightTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

function StrWithinZ(S, SearchStr : PAnsiChar;
                    Start        : Cardinal;
                    var Position : Cardinal) : Boolean;

function TrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;


function WordPosZ(S, WordDelims, AWord : PAnsiChar;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a word within a string}



implementation

uses
  AnsiStrings;

function HexBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
    {-Return hex string for byte}
begin
  Result := Dest;
  Dest^ := StHexDigits[B shr 4];
  Inc(Dest);
  Dest^ := StHexDigits[B and $F];
  Inc(Dest);
  Dest^ := #0;
end;

function HexBZ(Dest : PWideChar; B : Byte) : PWideChar;
    {-Return hex string for byte}
begin
  Result := Dest;
  Dest^ := WideChar(StHexDigits[B shr 4]);
  Inc(Dest);
  Dest^ := WideChar(StHexDigits[B and $F]);
  Inc(Dest);
  Dest^ := #0;
end;

function HexWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return hex string for word}
begin
  Result := Dest;
  Dest^ := StHexDigits[hi(W) shr 4];
  Inc(Dest);
  Dest^ := StHexDigits[hi(W) and $F];
  Inc(Dest);
  Dest^ := StHexDigits[lo(W) shr 4];
  Inc(Dest);
  Dest^ := StHexDigits[lo(W) and $F];
  Inc(Dest);
  Dest^ := #0;
end;

function HexLZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Return hex string for Integer}
//type                                                                 {!!.02}
//  LH = record L, H : word; end;                                      {!!.02}
var
  T2 : Array[0..4] of AnsiChar;
begin
//  Result := AnsiStrings.StrCat(HexWZ(Dest, LH(L).H), HexWZ(T2, LH(L).L));        {!!.02}
  Result := AnsiStrings.StrCat(HexWZ(Dest, HiWord(DWORD(L))),                      {!!.02}
    HexWZ(T2, LoWord(DWORD(L))));                                      {!!.02}
end;

function HexPtrZ(Dest : PAnsiChar; P : Pointer) : PAnsiChar;
  {-Return hex string for pointer}
var
  T2 : array[0..8] of AnsiChar;
begin
  AnsiStrings.StrCopy(Dest, ':');
  Result := AnsiStrings.StrCat(Dest, HexLZ(T2, Integer(P)));
end;

function BinaryBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return binary string for byte}
var
  I : Word;
begin
  Result := Dest;
  for I := 7 downto 0 do begin
    Dest^ := StHexDigits[Ord(B and (1 shl I) <> 0)]; {0 or 1}
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function BinaryWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return binary string for word}
var
  I : Word;
begin
  Result := Dest;
  for I := 15 downto 0 do begin
    Dest^ := StHexDigits[Ord(W and (1 shl I) <> 0)]; {0 or 1}
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function BinaryLZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Return binary string for Integer}
var
  I : Integer;
begin
  Result := Dest;
  for I := 31 downto 0 do begin
    Dest^ := StHexDigits[Ord(L and Integer(1 shl I) <> 0)]; {0 or 1}
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function OctalBZ(Dest : PAnsiChar; B : Byte) : PAnsiChar;
  {-Return octal string for byte}
var
  I : Word;
begin
  Result := Dest;
  for I := 0 to 2 do begin
    Dest[2-I] := StHexDigits[B and 7];
    B := B shr 3;
  end;
  Dest[3] := #0;
end;

function OctalWZ(Dest : PAnsiChar; W : Word) : PAnsiChar;
  {-Return octal string for word}
var
  I : Word;
begin
  Result := Dest;
  for I := 0 to 5 do begin
    Dest[5-I] := StHexDigits[W and 7];
    W := W shr 3;
  end;
  Dest[6] := #0;
end;

function OctalLZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Return octal string for word}
var
  I : Word;
begin
  Result := Dest;
  for I := 0 to 11 do begin
    Dest[11-I] := StHexDigits[L and 7];
    L := L shr 3;
  end;
  Dest[12] := #0;
end;

function CharStrZ(Dest : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
register;
asm
  push    edi            { Save EDI-about to change it }
  push    eax            { Save Dest pointer for return }
  mov     edi, eax       { Point EDI to Dest }

  mov     dh, dl         { Dup character 4 times }
  mov     eax, edx
  shl     eax, $10
  mov     ax, dx

  mov     edx, ecx       { Save Len }

  shr     ecx, 2         { Store dword char chunks first }
  rep     stosd
  mov     ecx, edx       { Store remaining characters }
  and     ecx, 3
  rep     stosb

  xor     al,al          { Add null terminator }
  mov     [edi], al

  pop     eax            { Return Dest pointer }
  pop     edi            { Restore orig value of EDI }
end;

function PadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
register;
asm
  push   eax
  push   ebx
  push   edi

  mov    edi, eax
  mov    ebx, ecx
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  dec    edi
  mov    eax, ebx
  sub    eax, ecx
  jbe    @@ExitPoint

  mov    ecx, eax
  mov    eax, edx
  rep    stosb

@@ExitPoint:
  xor    eax, eax
  mov    [edi], al

  pop    edi
  pop    ebx
  pop    eax
end;

function PadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string right-padded to length len with blanks}
begin
  Result := PadChPrimZ(S, ' ', Len);
end;

function LeftPadChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with C}
register;
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, edi
  mov    ebx, ecx

  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    eax, ebx
  mov    edi, esi
  add    edi, ebx
  mov    ebx, esi
  sub    eax, ecx
  jbe    @@ExitPoint

  add    esi, ecx
  inc    ecx
  std
  rep    movsb
  mov    ecx, eax
  mov    eax, edx
  rep    stosb

@@ExitPoint:
  cld
  mov    eax, ebx
  pop    esi
  pop    edi
  pop    ebx
end;

function LeftPadPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with blanks}
begin
  Result := LeftPadChPrimZ(S, ' ', Len);
end;

function PadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a PChar right-padded to length Len with C}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := PadChPrimZ(Dest, C, Len);
end;

function PadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string right-padded to length len with blanks}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := PadPrimZ(Dest, Len);
end;

function LeftPadChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with C}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := LeftPadChPrimZ(Dest, C, Len);
end;

function LeftPadZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string left-padded to length len with blanks}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := LeftPadPrimZ(Dest, Len);
end;

function TrimLeadPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading white space removed}
register;
asm
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, eax
  mov    edx, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    edi, edx
  jz     @@CopyRest

@@Lo:
  cmp    byte ptr [esi], ' '
  ja     @@CopyRest
  inc    esi
  dec    ecx
  jnz    @@Lo

@@CopyRest:
  inc    ecx
  rep    movsb
  mov    eax, edx

  pop    esi
  pop    edi
end;

function TrimLeadZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading white space removed}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := TrimLeadPrimZ(Dest);
end;

function TrimTrailPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed}
register;
asm
  push   edi

  mov    edi, eax
  mov    edx, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  jz     @@ExitPoint
  dec    edi
  dec    edi

@@Lo:
  cmp    BYTE PTR [edi], ' '
  ja     @@AllDone
  dec    edi
  dec    ecx
  jnz    @@Lo

@@AllDone:
  inc    edi
  mov    byte ptr [edi], 0h

@@ExitPoint:
  mov    eax, edx
  pop    edi
end;

function TrimTrailPrimZ(S : PWideChar) : PWideChar;     //SZ
  {-Return a string with trailing white space removed}
register;
asm
  push   edi

  mov    edi, eax
  mov    edx, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasw
  not    ecx
  dec    ecx
  dec    ecx
  jz     @@ExitPoint
  dec    edi
  dec    edi
  dec    edi
  dec    edi

@@Lo:
  cmp    WORD PTR [edi], ' '
  ja     @@AllDone
  dec    edi
  dec    edi
  dec    ecx
  dec    ecx
  jnz    @@Lo

@@AllDone:
  inc    edi
  inc    edi
  mov    word ptr [edi], 0h

@@ExitPoint:
  mov    eax, edx
  pop    edi
end;


function TrimTrailZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := TrimTrailPrimZ(Dest);
end;

function TrimPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed}
begin
  Result := TrimTrailPrimZ(TrimLeadPrimZ(S));
end;

function TrimZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing white space removed}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := TrimPrimZ(Dest);
end;

function TrimSpacesPrimZ(S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed}
var
  I, SLen : Cardinal;
begin
  Result := S;
  SLen := AnsiStrings.StrLen(S);
  while (SLen > 0) and (S[SLen-1] = ' ') do
    Dec(SLen);
  S[SLen] := #0;
  I := 0;
  while (I < SLen) and (S[I] = ' ') do
    Inc(I);
  if I > 0 then
    StrStDeletePrimZ(S, 0, I);
end;

{$IFDEF UNICODE}
function TrimSpacesPrimZ(S : PWideChar) : PWideChar;
  {-Return a string with leading and trailing spaces removed}
var
  I, SLen : Cardinal;
begin
  Result := S;
  SLen := StrLen(S);
  while (SLen > 0) and (S[SLen-1] = ' ') do
    Dec(SLen);
  S[SLen] := #0;
  I := 0;
  while (I < SLen) and (S[I] = ' ') do
    Inc(I);
  if I > 0 then
    StrStDeletePrimZ(S, 0, I);
end;
{$ENDIF}

function TrimSpacesZ(Dest, S : PAnsiChar) : PAnsiChar;
  {-Return a string with leading and trailing spaces removed}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := TrimSpacesPrimZ(Dest);
end;

function CenterChPrimZ(S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a string of C with specified width}
register;
asm
  push   eax                 { save registers }
  push   ebx
  push   edi
  push   esi

  mov    edi, eax            { set EDI and ESI to S }
  mov    esi, eax
  mov    ebx, ecx            { store Len in EBX }
  xor    eax, eax
  or     ecx, -1
  repne  scasb               { Find null terminator in S }
  not    ecx
  dec    ecx                 { ECX has length of S }
  jz     @@SpecialCase       { if zero, jump to special case }

  cmp    ecx, ebx
  jae    @@ExitPoint         { if Len >= Length(S), we're done }

  mov    eax, ebx            { copy Len to EAX }
  sub    ebx, ecx            { EBX = number of pad characters }
  inc    ebx
  shr    ebx, 1              { EBX = number of pad characters on one side }
  sub    eax, ebx
  sub    eax, ecx
  push   eax
  add    esi, ecx            { set ESI to end of text in S }
  mov    edi, esi
  add    edi, ebx            { set EDI to end of destination }
  dec    esi
  push   edi
  dec    edi
  std                        { Backward string ops }
  rep    movsb               { move string }
  mov    eax, edx            { copy pad character to EAX }
  mov    ecx, ebx
  rep    stosb               { pad to left of text }
  pop    edi
  pop    ecx
  cld                        { forward string ops }
  rep    stosb               { pad to right of text }
  jmp    @@AddNull           { add null terminator }

@@SpecialCase:
  mov    ecx, ebx            { fill string with C }
  mov    eax, edx
  mov    edi, esi
  rep    stosb

@@AddNull:
  mov    byte ptr [edi], 0h  { add null at end of string }

@@ExitPoint:
  pop    esi                 { restore registers }
  pop    edi
  pop    ebx
  pop    eax
end;

function CenterChZ(Dest, S : PAnsiChar; C : AnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a string of C with specified width}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := CenterChPrimZ(Dest, C, Len);
end;

function CenterPrimZ(S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a blank string of specified width}
begin
  Result := CenterChPrimZ(S, ' ', Len);
end;

function CenterZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return a string centered in a blank string of specified width}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := CenterPrimZ(Dest, Len);
end;

function ScramblePrimZ(S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption. This
    primitive version modifies the source string directly.}
var
  SPtr, KPtr, EndPtr : PAnsiChar;
begin
  Result := S;
  if Key^ = #0 then Exit;
  if S^ = #0 then Exit;
  SPtr := S;
  EndPtr := AnsiStrings.StrEnd(Key);
  Dec(EndPtr);
  KPtr := EndPtr;
  while SPtr^ <> #0 do begin
    if KPtr < Key then
      KPtr := EndPtr;
    if (SPtr^ <> KPtr^) then
      SPtr^ := AnsiChar(Byte(SPtr^) xor Byte(KPtr^));
    Inc(SPtr);
    Dec(KPtr);
  end;
end;

function ScrambleZ(Dest, S, Key : PAnsiChar) : PAnsiChar;
  {-Encrypt / Decrypt string with enhanced XOR encryption.}
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := ScramblePrimZ(Dest, Key);
end;

function SubstituteZ(Dest, Src, FromStr, ToStr : PAnsiChar) : PAnsiChar;
  {-Return string S after mapping characters found in FromStr to the
    corresponding ones in ToStr}
var
  I : Cardinal;
  P : Cardinal;
  L : Cardinal;
begin
  AnsiStrings.StrCopy(Dest, Src);
  if AnsiStrings.StrLen(FromStr) = AnsiStrings.StrLen(ToStr) then begin
    L := AnsiStrings.StrLen(Dest);
    if L > 0 then
      for I := 0 to L-1 do begin
        if StrChPosZ(FromStr, Dest[I], P) then
          Dest[I] := ToStr[P];
      end;
  end;
  Result := Dest;
end;

function FilterZ(Dest, Src, Filters : PAnsiChar) : PAnsiChar;
  {-Return string S after removing all characters in Filters from it}
var
  I : Cardinal;
  Len : Cardinal;
  L : Cardinal;
begin
  Result := Dest;
  AnsiStrings.StrCopy(Dest, Src);
  Len := 0;
  L := AnsiStrings.StrLen(Dest);
  if L > 0 then
    for I := 0 to L-1 do
      if not CharExistsZ(Filters, Dest[I]) then begin
        Result[Len] := Dest[I];
        inc(Len);
      end;
  Result[Len] := #0;
end;

function EntabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;
  {-Convert blanks in a string to tabs on spacing TabSize}
register;
asm
  push   eax                { Save registers }
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  and    ecx, 0FFh          { zero all but low byte of ECX }
  jz     @@Done
  mov    esi, edx
  xor    ebx, ebx           { Zero EBX and EDX }
  xor    edx, edx
  inc    edx                { Set EDX to 1 }

@@Next:
  or     ebx, ebx
  je     @@NoTab            { Jump to NoTab if spacecount is zero }
  mov    eax, edx           { IPos to EAX }
  push   edx
  xor    edx, edx
  div    ecx
  cmp    edx, 1             { Is mod = 1? }
  pop    edx
  jne    @@NoTab            { If not, no tab }

  sub    edi, ebx
  mov    byte ptr [edi], 9h { Store a tab }
  inc    edi
  xor    ebx, ebx           { Reset spacecount }

@@NoTab:
  mov    al, [esi]          { Get next input character }
  inc    esi
  or     al, al             { End of string? }
  jz     @@Done             { Yes, done }
  inc    ebx                { Increment SpaceCount }
  cmp    al, 20h            { Is character a space? }
  jz     @@Store            { Yes, store it for now }
  xor    ebx, ebx           { Reset SpaceCount }
  cmp    al, 27h            { Is it a quote? }
  jz     @@Quotes           { Yep, enter quote loop }
  cmp    al, 22h            { Is it a doublequote? }
  jnz    @@Store            { Nope, store it }

@@Quotes:
  mov    ah, al             { Save quote start }

@@NextQ:
  mov    [edi], al          { Store quoted character }
  inc    edi
  mov    al, [esi]          { Get next character }
  inc    esi
  inc    edx                { Increment Ipos }
  cmp    edx, ecx           { At end of line? }
  jae    @@Store            { If so, exit quote loop }

  cmp    al, ah             { Matching end quote? }
  jnz    @@NextQ            { Nope, stay in quote loop }
  cmp    al, 27h            { Single quote? }
  jz     @@Store            { Exit quote loop }
  cmp    byte ptr [esi-2],'\'  { Previous character an escape? }
  jz     @@NextQ            { Stay in if so }

@@Store:
  mov    [edi], al          { Store last character }
  inc    edi
  inc    edx                { Increment input position }
  jmp    @@Next             { Repeat while characters left }

@@Done:
  mov    byte ptr [edi], 0h
  pop    esi
  pop    edi
  pop    ebx
  pop    eax
end;

function DetabZ(Dest, Src : PAnsiChar; TabSize : Byte) : PAnsiChar;
  { -Expand tabs in a string to blanks on spacing TabSize- }
register;
asm
  push    eax           { Save Dest for return value }
  push    edi           { Save EDI, ESI and EBX, we'll be changing them }
  push    esi
  push    ebx

  mov     esi, edx      { ESI -> Src }
  mov     edi, eax      { EDI -> Dest }
  xor     ebx, ebx      { Get TabSize in EBX }
  add     bl, cl
  jz      @@Done        { Exit if TabSize is zero }

  xor     edx, edx      { Set output length to zero }

@@Next:
  mov     al, [esi]
  inc     esi           { Get next input character }
  or      al, al        { Is it a null? }
  jz      @@Done        { Yes-all done }
  cmp     al, 09        { Is it a tab? }
  je      @@Tab         { Yes, compute next tab stop }
  mov     [edi], al     { No, store to output }
  inc     edi
  inc     edx           { Increment output length }
  jmp     @@Next        { Next character }

@@Tab:
  push    edx           { Save output length }
  mov     eax, edx      { Get current output length in EDX:EAX }
  xor     edx, edx
  div     ebx           { Output length MOD TabSize in DX }
  mov     ecx, ebx      { Calc number of spaces to insert... }
  sub     ecx, edx      { = TabSize - Mod value }
  pop     edx
  add     edx, ecx      { Add count of spaces into current output length }

  mov     eax,$2020     { Blank in AH, Blank in AL }
  shr     ecx, 1        { Store blanks }
  rep     stosw
  adc     ecx, ecx
  rep     stosb
  jmp     @@Next        { Back for next input }

@@Done:
  mov     byte ptr [edi], 0h { Store final null terminator }

  pop     ebx           { Restore caller's EBX, ESI and EDI }
  pop     esi
  pop     edi
  pop     eax           { Return Dest }
end;

function HasExtensionZ(Name : PAnsiChar; var DotPos : Cardinal) : Boolean;
  {-Return whether and position of extension separator dot in a pathname}
var
  I, L : Integer;
  Pos : Cardinal;
  P : TSmallArrayA;
begin
  I := -1;
  DotPos := Cardinal(I);
  Result := False;
  L := AnsiStrings.StrLen(Name);
  if L = 0 then
    Exit;
  for I := L-1 downto 0 do
    if (Name[I] = '.') and (DotPos = Cardinal(-1)) then
      DotPos := I;
  Result := (DotPos <> Cardinal(-1)) and not
    StrChPosZ(StrStCopyZ(P, Name, Succ(DotPos), StMaxFileLen), '\', Pos);
end;

{$IFDEF UNICODE}
function HasExtensionZ(Name : PWideChar; var DotPos : Cardinal) : Boolean;
  {-Return whether and position of extension separator dot in a pathname}
var
  I, L : Integer;
  Pos : Cardinal;
  P : TSmallArray;
begin
  I := -1;
  DotPos := Cardinal(I);
  Result := False;
  L := StrLen(Name);
  if L = 0 then
    Exit;
  for I := L-1 downto 0 do
    if (Name[I] = '.') and (DotPos = Cardinal(-1)) then
      DotPos := I;
  Result := (DotPos <> Cardinal(-1)) and not
    StrChPosZ(StrStCopyZ(P, Name, Succ(DotPos), StMaxFileLen), '\', Pos);
end;
{$ENDIF}


function DefaultExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Return a pathname with the specified extension attached}
var
  DotPos : Cardinal;
begin
  if HasExtensionZ(Name, DotPos) then
    AnsiStrings.StrCopy(Dest, Name)
  else if AnsiStrings.StrLen(Name) = 0 then
    Dest[0] := #0
  else begin
    AnsiStrings.StrCopy(Dest, Name);
    AnsiStrings.StrCat(Dest, '.');
    AnsiStrings.StrCat(Dest, Ext);
  end;
  Result := Dest;
end;

function ForceExtensionZ(Dest : PAnsiChar; Name, Ext : PAnsiChar) : PAnsiChar;
  {-Return a pathname with the specified extension attached}
var
  DotPos : Cardinal;
begin
  if HasExtensionZ(Name, DotPos) then
    Dest := AnsiStrings.StrCat(StrStCopyZ(Dest, Name, 0, Succ(DotPos)), Ext)
  else if AnsiStrings.StrLen(Name) = 0 then
    Dest[0] := #0
  else begin
    Dest := AnsiStrings.StrCopy(Dest, Name);
    Dest := AnsiStrings.StrCat(Dest, '.');
    Dest := AnsiStrings.StrCat(Dest, Ext);
  end;
  Result := Dest;
end;

function JustExtensionZ(Dest : PAnsiChar; Name : PAnsiChar) : PAnsiChar;
  {-Return just the extension of a pathname}
var
  DotPos : Cardinal;
begin
  if HasExtensionZ(Name, DotPos) then
    Dest := StrStCopyZ(Dest, Name, Succ(DotPos), StMaxFileLen)
  else
    Dest[0] := #0;
  Result := Dest;
end;

function JustFilenameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the filename of a pathname}
var
  I : Integer;
begin
  I := AnsiStrings.StrLen(PathName);
  while (I > 0) and (not (PathName[I-1] in DosDelimSet)) do
    Dec(I);
  Dest := StrStCopyZ(Dest, PathName, I, StMaxFileLen);
  Result := Dest;
end;

{$IFDEF UNICODE}
function JustFilenameZ(Dest : PWideChar; PathName : PWideChar) : PWideChar;
  {-Return just the filename of a pathname}
var
  I : Integer;
begin
  I := StrLen(PathName);
  while (I > 0) and (not CharInSet(PathName[I-1], DosDelimSet)) do
    Dec(I);
  Dest := StrStCopyZ(Dest, PathName, I, StMaxFileLen);
  Result := Dest;
end;
{$ENDIF}


function JustNameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the name (no extension, no path) of a pathname}
var
  DotPos : Cardinal;
  T : TSmallArrayA;
begin
  JustFileNameZ(T, PathName);
  if HasExtensionZ(T, DotPos) then
    Dest := StrStCopyZ(Dest, T, 0, DotPos)
  else
    AnsiStrings.StrCopy(Dest, T);
  Result := Dest;
end;

function JustPathnameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return just the drive:directory portion of a pathname}
var
  I : Integer;
begin
  I := AnsiStrings.StrLen(PathName);
  repeat
    Dec(I);
  until (I = -1) or (PathName[I] in DosDelimSet);

  if I = -1 then
    {Had no drive or directory name}
    Dest[0] := #0
  else if I = 0 then begin
    {Either the root directory of default drive or invalid pathname}
    Dest[0] := PathName[0];
    Dest[1] := #0;
  end
  else if (PathName[I] = '\') then begin
    if PathName[Pred(I)] = ':' then
      {Root directory of a drive, leave trailing backslash}
      Dest := StrStCopyZ(Dest, PathName, 0, Succ(I))
    else
      {Subdirectory, remove the trailing backslash}
      Dest := StrStCopyZ(Dest, PathName, 0, I);
  end else
    {Either the default directory of a drive or invalid pathname}
    Dest:= StrStCopyZ(Dest, PathName, 0, Succ(I));
  Result := Dest;
end;

function AddBackSlashZ(Dest : PAnsiChar; DirName : PAnsiChar) : PAnsiChar;
  {-Add a default backslash to a directory name}
var
  L : Integer;
begin
  Result := Dest;
  AnsiStrings.StrCopy(Dest, DirName);
  L := AnsiStrings.StrLen(DirName);
  if (L > 0) then begin
    if ((L = 2) and (Dest[1] = ':')) or
       ((L > 2) and (Dest[L-1] <> '\')) then begin
      Dest[L] := '\';
      Dest[L+1] := #0;
    end;
  end;
end;

function CleanFileNameZ(Dest, FileName : PAnsiChar) : PAnsiChar;
  {-Return filename with at most 8 chars of name and 3 of extension}
var
  DotPos : Cardinal;
  NameLen : Integer;
  P2 : TSmallArrayA;
begin
  if HasExtensionZ(FileName, DotPos) then begin
    {Take the first 8 chars of name and first 3 chars of extension}
    NameLen := DotPos;
    if NameLen > 8 then
      NameLen := 8;
    StrStCopyZ(Dest, FileName, 0, NameLen);
    AnsiStrings.StrCat(Dest, StrStCopyZ(P2, FileName, DotPos, 4));
  end else
    {Take the first 8 chars of name}
    StrStCopyZ(Dest, FileName, 0, 8);
  Result := Dest;
end;

function CleanPathNameZ(Dest : PAnsiChar; PathName : PAnsiChar) : PAnsiChar;
  {-Return a pathname cleaned up as DOS will do it}
var
  I : Word;
  S1, S, OName : TSmallArrayA;
begin
  Result := Dest;
  AnsiStrings.StrCopy(Dest, PathName);
  I := AnsiStrings.StrLen(PathName);
  OName[0] := #0;
  while I > 0 do begin
    Dec(I);
    if I > 1 then
      if (Dest[I] = '\') and (Dest[I-1] = '\') then
        if (Dest[I-2] <> ':') then
          StrChDeletePrimZ(Dest, I);
  end;
  I := AnsiStrings.StrLen(Dest);
  while I > 0 do begin
    Dec(I);
    {Get the next directory or drive portion of pathname}
    while ((I > 0) and not (Dest[I] in DosDelimSet)) do                {!!.02}
      Dec(I);
    {Clean it up and prepend it to output string}
    StrStCopyZ(S1, Dest, I + 1, StMaxFileLen);
    AnsiStrings.StrCopy(S, OName);
    CleanFileNameZ(OName, S1);
    AnsiStrings.StrCat(OName, S);
    {if I >= 0 then begin}
      AnsiStrings.StrCopy(S, OName);
      StrStCopyZ(OName, Dest, I, 1);
      AnsiStrings.StrCat(OName, S);
      StrStDeletePrimZ(Dest, I, 255);
    {end;}
  end;
  AnsiStrings.StrCopy(Dest, OName);
end;

function ConvertToShortString(S : PAnsiChar; var SS : ShortString) : integer;

var
  LenS : integer;
begin
  {returns 0 if the string was converted successfully
           1 if the string is nil
           2 if the string length is greater than 255}
  if (S = nil) then begin
    Result := 1;
  end
  else begin
    LenS := AnsiStrings.StrLen(S);
    if (LenS > 255) then begin
      Result := 2;
    end
    else begin
      {we can't use StrPas in 32-bit since it assumes a long string
       and that would incur too much overhead, so convert to a short
       string from first principles}
      Move(S^, SS[1], LenS);
      SetLength(SS, LenS);
      Result := 0;
    end;
  end;
end;

function Str2Int16Z(S : PAnsiChar; var I : SmallInt) : Boolean;
  {-Convert a string to an integer, returning true if successful}

var
  ec : integer;
  SS : ShortString;
begin
  case ConvertToShortString(S, SS) of
    0 : begin {success}
          ValSmallint(string(SS), I, ec);
          if (ec = 0) then
            Result := true
          else begin
            Result := false;
            if (ec < 0) then
              I := AnsiStrings.StrLen(S)
            else
              I := pred(ec); {null terminated strings are zero-based}
          end;
        end;
    1 : begin {S is nil}
          Result := false;
          I := 0;
        end;
    2 : begin {S is more than 255 characters long}
          Result := false;
          I := 256;
        end;
  else
    Result := false;
  end;
end;

function Str2WordZ(S : PAnsiChar; var I : Word) : Boolean;
  {-Convert a string to a word, returning true if successful}

var
  ec : integer;
  SS : ShortString;
begin
  case ConvertToShortString(S, SS) of
    0 : begin {success}
          ValWord(string(SS), I, ec);
          if (ec = 0) then
            Result := true
          else begin
            Result := false;
            if (ec < 0) then
              I := AnsiStrings.StrLen(S)
            else
              I := pred(ec); {null terminated strings are zero-based}
          end;
        end;
    1 : begin {S is nil}
          Result := false;
          I := 0;
        end;
    2 : begin {S is more than 255 characters long}
          Result := false;
          I := 256;
        end;
  else
    Result := false;
  end;
end;

function Str2LongZ(S : PAnsiChar; var I : Integer) : Boolean;
  {-Convert a string to a Integer, returning true if successful}

var
  ec : integer;
  SS : ShortString;
begin
  case ConvertToShortString(S, SS) of
    0 : begin {success}
          ValLongint(string(SS), I, ec);
          if (ec = 0) then
            Result := true
          else begin
            Result := false;
            if (ec < 0) then
              I := AnsiStrings.StrLen(S)
            else
              I := pred(ec); {null terminated strings are zero-based}
          end;
        end;
    1 : begin {S is nil}
          Result := false;
          I := 0;
        end;
    2 : begin {S is more than 255 characters long}
          Result := false;
          I := 256;
        end;
  else
    Result := false;
  end;
end;

{$IFDEF VER93}
function Str2RealZ(S : PAnsiChar; var R : Double) : Boolean;
{$ELSE}
function Str2RealZ(S : PChar; var R : Real) : Boolean;
{$ENDIF}
  {-Convert a string to a real, returning true if successful}
var
  Code : Integer;
  P : TSmallArray;
begin
  if StrLen(S)+1 > SizeOf(P) then begin
    Result := False;
    R := -1;
    Exit;
  end;
  StrCopy(P, S);
  TrimTrailPrimZ(P);
  Val(ValPrepZ(P), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
    Result := False;
  end else
    Result := True;
end;

function Str2ExtZ(S : PAnsiChar; var R : Extended) : Boolean;
  {-Convert a string to an extended, returning true if successful}
var
  Code : Integer;
  P : TSmallArrayA;
begin
  if AnsiStrings.StrLen(S)+1 > SizeOf(P) then begin
    Result := False;
    R := -1;
    Exit;
  end;
  AnsiStrings.StrCopy(P, S);
  TrimTrailPrimZ(P);
  Val(string(ValPrepZ(P)), R, Code);
  if Code <> 0 then begin
    R := Code - 1;
    Result := False;
  end else
    Result := True;
end;

function Long2StrZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
  {-Convert a long/word/integer/byte/shortint to a string}
type
  PCharArray = ^TCharArray;
  TCharArray = array[0..99] of AnsiChar;
begin
  Str(L, PCharArray(Dest)^);
  Result := Dest;
end;

function Real2StrZ(Dest : PAnsiChar; R : Double; Width : Byte;
                  Places : ShortInt) : PAnsiChar;
  {-Convert a real to a string}
type
  PCharArray = ^TCharArray;
  TCharArray = array[0..99] of AnsiChar;
begin
  Str(R:Width:Places, PCharArray(Dest)^);
  Result := Dest;
end;

function Ext2StrZ(Dest : PAnsiChar; R : Extended; Width : Byte;
                 Places : ShortInt) : PAnsiChar;
  {-Convert an extended to a string}
type
  PCharArray = ^TCharArray;
  TCharArray = array[0..99] of AnsiChar;
begin
  Str(R:Width:Places, PCharArray(Dest)^);
  Result := Dest;
end;

function ValPrepZ(S : PAnsiChar) : PAnsiChar;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
begin
  Result := TrimSpacesPrimZ(S);
  if AnsiStrings.StrLen(Result) <> 0 then begin
    if StrChPosZ(Result, AnsiChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator), P) then begin
      Result[P] := '.';
      if Succ(P) = AnsiStrings.StrLen(Result) then
        Result[P] := #0;
    end;
  end else begin
    Result := '0';
  end;
end;

{$IFDEF UNICODE}
function ValPrepZ(S : PWideChar) : PWideChar;
  {-Prepares a string for calling Val.}
var
  P : Cardinal;
begin
  Result := TrimSpacesPrimZ(S);
  if StrLen(Result) <> 0 then begin
    if StrChPosZ(Result, {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator, P) then begin
      Result[P] := '.';
      if Succ(P) = StrLen(Result) then
        Result[P] := #0;
    end;
  end else begin
    Result := '0';
  end;
end;
{$ENDIF}

function CharExistsZ(S : PAnsiChar; C : AnsiChar) : Boolean;
  {-Determine whether the given character exists in a string. }
register;
asm
  xor   dh, dh
  xor   ecx, ecx
@@Loop:
  cmp   dh, [eax+0]
  je    @@Done
  cmp   dl, [eax+0]
  jne   @@1
  inc   ecx
  jmp   @@Done
@@1:
  cmp   dh, [eax+1]
  je    @@Done
  cmp   dl, [eax+1]
  jne   @@2
  inc   ecx
  jmp   @@Done
@@2:
  cmp   dh, [eax+2]
  je    @@Done
  cmp   dl, [eax+2]
  jne   @@3
  inc   ecx
  jmp   @@Done
@@3:
  cmp   dh, [eax+3]
  je    @@Done
  cmp   dl, [eax+3]
  jne   @@4
  inc   ecx
  jmp   @@Done
@@4:
  add   eax, 4
  jmp   @@Loop
@@Done:
  mov   eax, ecx
end;

function CharExistsZ(S : PWideChar; C : Char) : Boolean; //SZ
  {-Determine whether the given character exists in a string. }
register;
asm
  xor   dx, dx
  xor   ecx, ecx
@@Loop:
  cmp   dx, [eax+0]
  je    @@Done
  cmp   dx, [eax+0]
  jne   @@1
  inc   ecx
  inc   ecx
  jmp   @@Done
@@1:
  cmp   dx, [eax+2]
  je    @@Done
  cmp   dx, [eax+2]
  jne   @@2
  inc   ecx
  inc   ecx
  jmp   @@Done
@@2:
  cmp   dx, [eax+4]
  je    @@Done
  cmp   dx, [eax+4]
  jne   @@3
  inc   ecx
  inc   ecx
  jmp   @@Done
@@3:
  cmp   dx, [eax+6]
  je    @@Done
  cmp   dx, [eax+6]
  jne   @@4
  inc   ecx
  inc   ecx
  jmp   @@Done
@@4:
  add   eax, 8
  jmp   @@Loop
@@Done:
  mov   eax, ecx
end;


function CharCountZ(S : PAnsiChar; C : AnsiChar) : Cardinal;
  {-Count the number of a given character in a string. }
register;
asm
  xor   dh, dh
  xor   ecx, ecx
@@Loop:
  cmp   dh, [eax+0]
  je    @@Done
  cmp   dl, [eax+0]
  jne   @@1
  inc   ecx
@@1:
  cmp   dh, [eax+1]
  je    @@Done
  cmp   dl, [eax+1]
  jne   @@2
  inc   ecx
@@2:
  cmp   dh, [eax+2]
  je    @@Done
  cmp   dl, [eax+2]
  jne   @@3
  inc   ecx
@@3:
  cmp   dh, [eax+3]
  je    @@Done
  cmp   dl, [eax+3]
  jne   @@4
  inc   ecx
@@4:
  add   eax, 4
  jmp   @@Loop
@@Done:
  mov   eax, ecx
end;

function WordCountZ(S : PAnsiChar; WordDelims : PAnsiChar) : Cardinal;
  {-Given a set of word delimiters, return number of words in S}
var
  Count : Cardinal;
  I     : Cardinal;
  SLen  : Cardinal;

begin
  Count := 0;
  I := 0;
  SLen := AnsiStrings.StrLen(S);
  while I < SLen do begin
    {skip over delimiters}
    while (I < SLen) and (CharExistsZ(WordDelims, S^)) do begin
      Inc(I);
      Inc(S);
    end;
    {if we're not beyond end of S, we're at the start of a word}
    if I < SLen then
      Inc(Count);

   {find the end of the current word}
    while (I < SLen) and (not CharExistsZ(WordDelims, S^)) do begin
      Inc(I);
      Inc(S);
    end;
  end;

  Result := Count;
end;

function WordPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                      var Pos : Cardinal) : Boolean;
  {-Given a set of word delimiters, return start position of N'th word in S}
var
  Count : Cardinal;
  SLen  : Cardinal;
begin
  Count := 0;
  Pos := 0;
  Result := False;
  SLen := AnsiStrings.StrLen(S);

  while (Pos < SLen) and (Count <> N) do begin
    {skip over delimiters}
    while (Pos < SLen) and (CharExistsZ(WordDelims, S^)) do begin
      Inc(Pos);
      Inc(S);
    end;
    {if we're not beyond end of S, we're at the start of a word}
    if Pos < SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then begin
      while (Pos < SLen) and (not CharExistsZ(WordDelims, S^)) do begin
        Inc(Pos);
        Inc(S);
      end;
    end
    else
      Result := True;
  end;
end;

function ExtractWordZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                     WordDelims : PAnsiChar) : PAnsiChar;
  {-Given a set of word delimiters, return in Dest the N'th word in Src}
var
  I : Cardinal;
  SLen : Cardinal;
begin
  Result := Dest;
  SLen := AnsiStrings.StrLen(Src);
  if WordPositionZ(N, Src, WordDelims, I) then begin
    Inc(Src, I);
    {find the end of the current word}
    while (I <= SLen) and (not CharExistsZ(WordDelims, Src^)) do begin
      {add the I'th character to result}
      Dest^ := Src^;
      Inc(Dest);
      Inc(Src);
      Inc(I);
    end;
  end;
  Dest^ := #0;
end;

function AsciiCountZ(S : PAnsiChar; WordDelims : PAnsiChar; Quote : AnsiChar) : Cardinal;
  {-Given a set of word delimiters, return number of words in S}
var
  Count : Cardinal;
  I     : Cardinal;
  SLen  : Cardinal;
  InQuote : Boolean;
begin
  Count := 0;
  I := 1;
  InQuote := False;
  SLen := AnsiStrings.StrLen(S);
  while I <= SLen do begin
    {skip over delimiters}
    while (I <= SLen) and (S^ <> Quote) and CharExistsZ(WordDelims, S^) do begin
      Inc(I);
      Inc(S);
    end;
    {if we're not beyond end of S, we're at the start of a word}
    if I <= SLen then
      Inc(Count);
    {find the end of the current word}
    while (I <= SLen) and ((InQuote) or (not CharExistsZ(WordDelims, S^))) do begin
      if S^ = Quote then
        InQuote := not(InQuote);
      Inc(I);
      Inc(S);
    end;
  end;

  Result := Count;
end;

function AsciiPositionZ(N : Cardinal; S : PAnsiChar; WordDelims : PAnsiChar;
                       Quote : AnsiChar; var Pos : Cardinal) : Boolean;
  {-Given a set of word delimiters, return start position of N'th word in S}
var
  Count : Cardinal;
  SLen  : Cardinal;
  InQuote : Boolean;
begin
  Count := 0;
  Pos := 0;
  InQuote := False;
  Result := False;
  SLen := AnsiStrings.StrLen(S);
  while (Pos < SLen) and (Count <= N) do begin
   {skip over delimiters}
    while (Pos < SLen) and (S^ <> Quote) and CharExistsZ(WordDelims, S^) do begin
      Inc(Pos);
      Inc(S);
    end;

    {if we're not beyond end of S, we're at the start of a word}
    if Pos < SLen then
      Inc(Count);

    {if not finished, find the end of the current word}
    if Count <> N then
      while (Pos < SLen) and ((InQuote) or (not CharExistsZ(WordDelims, S^))) do begin
        if S^ = Quote then
          InQuote := not(InQuote);
        Inc(Pos);
        Inc(S);
      end
    else begin
      Result := True;
      Exit;
    end;
  end;
end;

function ExtractAsciiZ(Dest : PAnsiChar; N : Cardinal; Src : PAnsiChar;
                      WordDelims : PAnsiChar; Quote : AnsiChar) : PAnsiChar;
  {-Given a set of word delimiters, return in Dest the N'th word in Src}
var
  I : Cardinal;
  Len : Cardinal;
  SLen : Cardinal;
  InQuote : Boolean;
begin
  Len := 0;
  InQuote := False;
  Dest[0] := #0;
  Result := Dest;
  SLen := AnsiStrings.StrLen(Src);
  if AsciiPositionZ(N, Src, WordDelims, Quote, I) then
    {find the end of the current word}
    while (I < SLen) and ((InQuote) or (not CharExistsZ(WordDelims, Src[I]))) do begin
      {add the I'th character to result}
      if Src[I] = Quote then
        InQuote := Not(InQuote);
      Dest[Len] := Src[I];
      Inc(Len);
      Inc(I);
    end;
  Dest[Len] := #0;
end;

procedure WordWrapZ(Dest : PAnsiChar; InSt, Overlap : PAnsiChar;
                   Margin : Cardinal;
                   PadToMargin : Boolean);
  {-Wrap InSt at Margin, storing the result in Dest and the remainder
    in Overlap}
var
  InStLen : Cardinal;
  OutStLen : Cardinal;
  OvrLen : Cardinal;
  EOS, BOS : Cardinal;
begin
  OutStLen := 0;
  InStLen := AnsiStrings.StrLen(InSt);

{!!.02 - Added }
  { handle empty string on input }                  
  if InStLen = 0 then begin                         
    if Assigned(Dest) then                          
      Dest[0] := #0;                                
    if Assigned(Overlap) then                       
      Overlap[0] := #0;                             
    Exit;                                           
  end;
{!!.02 - End Added }

  {find the end of the new output string}
  if InStLen > Margin then begin
    {assume this is a good break point}
    EOS := Margin-1;

    {is this the position of the last character of a word}
    if InSt[EOS+1] <> ' ' then begin  {check next char}
      {look for the space before the current word}
      while (EOS > 0) and (InSt[EOS] <> ' ') do
        Dec(EOS);
      {when done, EOS points to a space char or is zero}

      {if EOS = 0 then - can't wrap it properly}
      if EOS = 0 then
        EOS := Margin-1  {set up to break line at margin}
      else
        while (InSt[EOS] = ' ') and (EOS > 0) do  {trim trailing blanks}
          Dec(EOS);
    end else
      while (EOS > 0) and (InSt[EOS] = ' ') do  {trim trailing blanks}
        Dec(EOS);
  end
  else
    EOS := InStLen-1;

  {at this point EOS points to the break point, the end of the line,
   or is zero}

  {copy the unwrapped portion of the line}
  if (EOS = 0) and (InSt[EOS] = ' ') then
    Dest[0] := #0
  else begin
    OutStLen := EOS+1;
    Move(InSt^, Dest^, OutStLen);
    Dest[OutStLen] := #0;
  end;

  {find the start of the next word in the line}
  BOS := EOS+1;
  while (BOS < InStLen) and (InSt[BOS] = ' ') do
    Inc(BOS);

  if BOS >= InStLen then begin
    OverLap[0] := #0;
  end else begin
    {copy from the start of the next word to the end of the line}
    OvrLen := InStLen-BOS;
    Move(InSt[BOS], Overlap^, OvrLen);
    Overlap[OvrLen] := #0;
  end;

  {pad the end of the output string if requested}
  if PadToMargin and (OutStLen < Margin) then begin
    FillChar(Dest[OutStLen], Margin-OutStLen, ' ');
    Dest[Margin] := #0;
  end;
end;

function CompStringZ(S1, S2 : PAnsiChar) : Integer;
  {-Return -1, 0, 1 if S1<S2, S1=S2, or S1>S2}
register;
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx

  mov    edi, edx
  mov    ebx, edx
  mov    edx, ecx
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    edi, ebx
  or     ebx, -1
  cmp    edx, ecx
  je     @@EqLen
  jb     @@Comp

  inc    ebx
  mov    ecx, edx

@@EqLen:
  inc    ebx

@@Comp:
  or     ecx, ecx
  jz     @@Done

  repe   cmpsb
  je     @@Done

  mov    ebx, 1
  ja     @@Done
  or     ebx, -1

@@Done:
  mov    eax, ebx
  pop    esi
  pop    edi
  pop    ebx
end;


function CompUCStringZ(S1, S2 : PAnsiChar) : Integer;
  {-Return -1, 0, 1 if s1<s2, s1=s2, or s1>s2. Comparison is done in
    uppercase}
register;
asm
  push   ebx
  push   edi
  push   esi

  mov    edi, eax
  mov    esi, eax
  xor    eax, eax
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx

  mov    edi, edx
  mov    ebx, edx
  mov    edx, ecx
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx
  mov    edi, ebx
  or     ebx, -1
  cmp    edx, ecx
  je     @@EqLen
  jb     @@Comp

  inc    ebx
  mov    ecx, edx

@@EqLen:
  inc    ebx

@@Comp:
  or     ecx, ecx
  jz     @@Done             { Done if either is empty }

@@Start:
  mov    al, [esi]
  inc    esi
  push   ebx                { Save registers }
  push   ecx
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx
  pop    ebx

  mov    edx, eax
  mov    al, [edi]
  inc    edi

  push   ebx                { Save registers }
  push   ecx
  push   edx
  push   eax                { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                { Restore registers }
  pop    ecx
  pop    ebx

  cmp    edx, eax
  jne    @@Output
  dec    ecx
  jnz    @@Start
  je     @@Done

@@Output:
  mov    ebx, 1
  ja     @@Done
  or     ebx, -1

@@Done:
  mov    eax, ebx
  pop    esi
  pop    edi
  pop    ebx
end;

function SoundexZ(Dest : PAnsiChar; S : PAnsiChar) : PAnsiChar; assembler;
  {-Return 4 character soundex of input string}
register;
const
  SoundexTable : array[0..255] of AnsiChar =
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
asm
  push   eax                      { Save registers }
  push   ebx
  push   edi
  push   esi
  mov    edi, edx
  mov    ebx, eax
  mov    esi, edx

  mov    dword ptr [ebx], '0000'  { Initialize output string to '0000'. }
  xor    eax, eax
  mov    [ebx+4], al              { Set null at end of string. }

  or     ecx, -1                  { Set ECX to $FFFFFFFF }
  repne  scasb
  not    ecx
  dec    ecx                      { ECX has length of S }
  jz     @@Done                   { Exit if null string. }

  mov    edi, ebx
  mov    al, [esi]                { Get first character of input string. }
  inc    esi

  push   ecx                      { Save ECX across call to CharUpper. }
  push   eax                      { Push Char onto stack for CharUpper. }
  call   CharUpper                { Uppercase AL. }
  pop    ecx                      { Restore saved register. }

  mov    [edi], al                { Store first output character. }
  inc    edi

  dec    ecx                      { One input character used. }
  jz     @@Done                   { Was input string one char long?. }

  mov    bh, 03h                  { Output max 3 chars beyond first. }
  mov    edx, offset SoundexTable { EDX => Soundex table. }
  xor    eax, eax                 { Prepare for address calc. }
  xor    bl, bl                   { BL will be used to store 'previous char'. }

@@Next:
  mov    al, [esi]                { Get next char in AL. }
  inc    esi
  mov    al, [edx+eax]            { Get soundex code into AL. }
  or     al, al                   { Is AL zero? }
  jz     @@NoStore                { If yes, skip this char. }
  cmp    bl, al                   { Is it the same as the previous stored char? }
  je     @@NoStore                { If yes, skip this char. }
  mov    [edi], al                { Store char to Dest. }
  inc    edi
  dec    bh                       { Decrement output counter. }
  jz     @@Done                   { If zero, we're done. }
  mov    bl, al                   { New previous character. }

@@NoStore:
  dec    ecx                      { Decrement input counter. }
  jnz    @@Next

@@Done:                           { Restore registers }
  pop    esi
  pop    edi
  pop    ebx
  pop    eax
end;

function MakeLetterSetZ(S : PAnsiChar) : Integer;
  {-Return a bit-mapped long storing the individual letters contained in S.}
register;
asm
  push   ebx                    { Save registers }
  push   edi
  push   esi
  mov    esi, eax
  mov    edi, eax
  xor    edx, edx
  xor    eax, eax               { Measure S }
  or     ecx, -1
  repne  scasb
  not    ecx
  dec    ecx                    { ECX has length of S }
  jz     @@Exit

@@Next:
  mov    al, [esi]              { EAX has next char in S }
  inc    esi

  push   ecx                    { Save registers }
  push   edx
  push   eax                    { Push Char onto stack for CharUpper }
  call   CharUpper
  pop    edx                    { Restore registers }
  pop    ecx

  sub    eax, 'A'               { Convert to bit number }
  cmp    eax, 'Z'-'A'           { Was char in range 'A'..'Z'? }
  ja     @@Skip                 { Skip it if not }

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
  jnz    @@Next                 { Get next character }

@@Exit:
  mov    eax, edx               { Move EDX to result }
  pop    esi
  pop    edi                    { Restore registers }
  pop    ebx
end;

procedure BMMakeTableZ(MatchString : PAnsiChar; var BT : BTable);
  {Build Boyer-Moore link table}
register;
asm
  push  esi             { Save registers because they will be changed }
  push  edi
  push  ebx

  mov   edi, eax        { Move EAX to ESI & EDI }
  mov   esi, eax
  xor   eax, eax        { Zero EAX }
  or    ecx, -1
  repne scasb           { Search for null terminator }
  not   ecx
  dec   ecx             { ECX is length of search string }
  cmp   ecx, 0FFh       { If ECX > 255, force to 255 }
  jbe   @@1
  mov   ecx, 0FFh

@@1:
  mov   ch, cl          { Duplicate CL in CH }
  mov   eax, ecx        { Fill each byte in EAX with length }
  shl   eax, 16
  mov   ax, cx
  mov   edi, edx        { Point to the table }
  mov   ecx, 64         { Fill table bytes with length }
  rep   stosd
  cmp   al, 1           { If length <= 1, we're done }
  jbe   @@MTDone
  mov   edi, edx        { Reset EDI to beginning of table }
  xor   ebx, ebx        { Zero EBX }
  mov   cl, al          { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   bl, [esi]       { Load table with positions of letters }
  inc   esi             { That exist in the search string }
  mov   [edi+ebx], cl
  dec   ecx
  jnz   @@MTNext

@@MTDone:
  pop   ebx             { Restore registers }
  pop   edi
  pop   esi
end;

function BMSearchZ(var Buffer; BufLength : Cardinal; var BT : BTable;
  MatchString : PAnsiChar; var Pos : Cardinal) : Boolean; assembler;
register;
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx
  push  edx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }

  xor   eax, eax            { Zero out EAX so we can search for null }
  mov   edi, MatchString    { Set EDI to beginning of MatchString }
  or    ecx, -1             { We will be counting down }
  repne scasb               { Find null }
  not   ecx                 { ECX = length of MatchString + null }
  dec   ecx                 { ECX = length of MatchString }
  mov   edx, ecx            { Copy length of MatchString to EDX }

  pop   ecx                 { Pop length of buffer into ECX }
  mov   edi, esi            { Set EDI to beginning of search buffer }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }

  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  dec   edi                 { Found, calculate position }
  sub   edi, ebx
  mov   esi, Pos            { Set position in Pos }
  mov   [esi], edi
  mov   eax, 1              { Set result to True }
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  std                       { Backward string ops }
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
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  mov   esi, Pos
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

function BMSearchUCZ(var Buffer; BufLength : Cardinal; var BT : BTable;
  MatchString : PAnsiChar; var Pos : Cardinal) : Boolean; assembler;
  {- Case-insensitive search of Buffer for MatchString. Return indicates
     success or failure.  Assumes MatchString is already raised to
     uppercase (PRIOR to creating the table) -}
register;
var
  BufPtr : Pointer;
asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx
  push  edx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   esi, eax
  mov   ebx, ecx            { Copy BufLength to EBX }

  xor   eax, eax            { Zero out EAX so we can search for null }
  mov   edi, MatchString    { Set EDI to beginning of MatchString }
  or    ecx, -1             { We will be counting down }
  repne scasb               { Find null }
  not   ecx                 { ECX = length of MatchString + null }
  dec   ecx                 { ECX = length of MatchString }
  mov   edx, ecx            { Copy length of MatchString to EDX }

  pop   ecx                 { Pop length of buffer into ECX }
  mov   edi, esi            { Set EDI to beginning of search buffer }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }

  or    dl, dl              { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  std                       { Backward string ops }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
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
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  lodsb                     { Get MatchString char }
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
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  mov   esi, Pos
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

  {------------------ Formatting routines --------------------}

function CommaizeChZ(Dest : PAnsiChar; L : Integer; Ch : AnsiChar) : PAnsiChar;
var
  NumCommas, Len, I : Cardinal;
begin
  Result := Dest;
  Long2StrZ(Dest, L);
  Len := AnsiStrings.StrLen(Dest);
  NumCommas := (Len - 1) div 3;
  for I := 1 to NumCommas do
    StrChInsertPrimZ(Dest, Ch, Len - (I * 3));
end;

function CommaizeZ(Dest : PAnsiChar; L : Integer) : PAnsiChar;
begin
  Result := CommaizeChZ(Dest, L, ',');
end;

function FormPrimZ(Dest, Mask : PAnsiChar; R : TstFloat; LtCurr,
                   RtCurr : PAnsiChar; Sep, DecPt : AnsiChar;
                   AssumeDP : Boolean) : PAnsiChar;
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
  FormChars : array[0..8] of AnsiChar = '#@*$-+,.';
  PlusArray : array[Boolean] of AnsiChar = ('+', '-');
  MinusArray : array[Boolean] of AnsiChar = (' ', '-');
  FillArray : array[Blank..Zero] of AnsiChar = (' ', '*', '0');
var
  Temp : PAnsiChar;
  S : array[0..20] of AnsiChar; {temporary string}
  Filler : integer;{char for unused digit slots: ' ', '*', '0'}
  WontFit,                      {true if number won't fit in the mask}
  AddMinus,                     {true if minus sign needs to be added}
  Dollar,                       {true if floating dollar sign is desired}
  Negative : Boolean;           {true if B is negative}
  StartF,                       {starting point of the numeric field}
  EndF : Cardinal;              {end of numeric field}
  RtChars,                      {# of chars to add to right}
  LtChars,                      {# of chars to add to left}
  DotPos,                       {position of '.' in Mask}
  Digits,                       {total # of digits}
  Places,                       {# of digits after the '.'}
  Blanks,                       {# of blanks returned by Str}
  FirstDigit,                   {pos. of first digit returned by Str}
  Extras,                       {# of extra digits needed for special cases}
  I : Cardinal;
label
  EndFound,
  RedoCase,
  Done;
begin
  {assume decimal point at end?}
  Result := Dest;
  AnsiStrings.StrCopy(Result, Mask);
  if (not AssumeDP) and (not CharExistsZ(Result, '.')) then
    AssumeDP := true;
  if AssumeDP and (Result^ <> #0) then
    AnsiStrings.StrCat(Result, '.');

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Result^ = #0 then
    goto Done;

  {initialize variables}
  Filler := Blank;
  DotPos := 0;
  Places := 0;
  Digits := 0;
  Dollar := False;
  AddMinus := True;
  StartF := 0;

  {store the sign of the real and make it positive}
  Negative := (R < 0);
  R := Abs(R);

  {strip and count c's}
  Temp := AnsiStrings.StrEnd(Result);
  Dec(Temp);
  while Temp >= Result do begin
    if Temp^ = 'C' then begin
      Inc(RtChars);
      StrChDeletePrimZ(Result, Temp - Result);
    end else if Temp^ = 'c' then begin
      Inc(LtChars);
      StrChDeletePrimZ(Result, Temp - Result);
    end;
    Dec(Temp);
  end;

  {find the starting point for the field}
  Temp := Result;
  while (Temp^ <> #0) and not CharExistsZ(FormChars, Temp^) do begin
    Inc(StartF);
    Inc(Temp);
  end;
  if Succ(StartF) > AnsiStrings.StrLen(Result) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  while (Temp^ <> #0) do begin
    case Temp^ of
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
    Inc(Temp);
    Inc(EndF);
  end;

EndFound:
  {correct the off-by-one nature of the loop}
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
  Real2StrZ(S, R, Digits, Places);

  {add zeros that Str may have left out}
  if Places > MaxPlaces then begin
    Temp := AnsiStrings.StrEnd(S);
    CharStrZ(Temp, '0', Places-MaxPlaces);
    while (AnsiStrings.StrLen(S) > Digits) and (S[0] = ' ') do
      StrChDeletePrimZ(S, 0);
  end;

  {count number of initial blanks}
  Blanks := 0;
  while S[Blanks] = ' ' do
    Inc(Blanks);
  FirstDigit := Blanks;

  {the number won't fit if (a) S is longer than Digits or (b) the number of
   initial blanks is less than Extras}
  WontFit := (AnsiStrings.StrLen(S) > Digits) or (Blanks < Extras);

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
    FillChar(S[0], Blanks, FillArray[Filler]);

    {put floating dollar sign in last blank slot if necessary}
    if Dollar then begin
      S[Pred(Blanks)] := LtCurr[0];
      Dec(Blanks);
    end;

    {insert a minus sign if necessary}
    if AddMinus then
      S[Pred(Blanks)] := '-';
  end;

  {put in the digits / signs}
  Temp := AnsiStrings.StrEnd(S);
  Dec(Temp);
  for I := EndF downto StartF do begin
RedoCase:
    case Result[I] of
      '#', '@', '*', '$' :
        if Temp >= S then begin
          Result[I] := Temp^;
          Dec(Temp);
          if (Temp^ = '.') and (Temp >= S) then
            Dec(Temp);
        end
        else
          Result[I] := FillArray[Filler];
      ',' :
        begin
          Result[I] := Sep;
          if (I < DotPos) and (Temp < (S + FirstDigit)) then begin
            Result[I] := '#';
            goto RedoCase;
          end;
        end;
      '.' :
        begin
          Result[I] := DecPt;
          if (I < DotPos) and (Temp < (S + FirstDigit)) then begin
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
    Result[Pred(AnsiStrings.StrLen(Result))] := #0;
  if RtChars > 0 then begin
    AnsiStrings.StrLCopy(S, RtCurr, RtChars);
    LeftPadPrimZ(S, RtChars);
    AnsiStrings.StrCat(Result, S);
  end;
  if LtChars > 0 then begin
    AnsiStrings.StrLCopy(S, LtCurr, LtChars);
    PadPrimZ(S, LtChars);
    StrStInsertPrimZ(Result, S, 0);
  end;
end;

function FloatFormZ(Dest, Mask : PAnsiChar; R : TstFloat; LtCurr,
                    RtCurr : PAnsiChar; Sep, DecPt : AnsiChar) : PAnsiChar;
  {-Return a formatted string with digits from R merged into mask.}
begin
  Result := FormPrimZ(Dest, Mask, R, LtCurr, RtCurr, Sep, DecPt, False);
end;

function LongIntFormZ(Dest, Mask : PAnsiChar; L : Integer; LtCurr,
                      RtCurr : PAnsiChar; Sep : AnsiChar) : PAnsiChar;
  {-Return a formatted string with digits from L merged into mask.}
begin
  Result := FormPrimZ(Dest, Mask, L, LtCurr, RtCurr, Sep, '.', True);
end;

function StrChPosZ(P : PAnsiChar; C : AnsiChar; var Pos : Cardinal): Boolean;
  {-Sets Pos to position of character C within string P returns True if found}
var
  Temp : PAnsiChar;
begin
  Result := False;
  Temp := AnsiStrings.StrScan(P, C);
  if Temp <> nil then begin
    Pos := Temp - P;
    Result := True;
  end;
end;

{$IFDEF UNICODE}
function StrChPosZ(P : PWideChar; C : Char; var Pos : Cardinal): Boolean;
  {-Sets Pos to position of character C within string P returns True if found}
var
  Temp : PChar;
begin
  Result := False;
  Temp := StrScan(P, C);
  if Temp <> nil then begin
    Pos := Temp - P;
    Result := True;
  end;
end;
{$ENDIF}

function StrStPosZ(P, S : PAnsiChar; var Pos : Cardinal) : boolean;
  {-Sets Pos to position of string S within string P returns True if found}
var
  Temp : PAnsiChar;
begin
  Result := False;
  Temp := AnsiStrings.StrPos(P, S);
  if Temp <> nil then begin
    Pos := Temp - P;
    Result := True;
  end;
end;

function StrChInsertPrimZ(Dest : PAnsiChar; C : AnsiChar;
                          Pos : Cardinal) : PAnsiChar;
register;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi

  mov    esi, eax        {copy Dest to ESI and EDI}
  mov    edi, eax
  mov    ah, dl
  mov    edx, ecx        {move POS to edx}

  xor    al, al          {zero}
  or     ecx, -1         {set ECX to $FFFFFFFF}
  repne  scasb           {find null terminator}

  not    ecx             {calc length (including null)}
  std                    {backwards string ops}
  add    esi, ecx
  dec    esi             {point to end of source string}
  sub    ecx, edx        {calculate number to do}
  jae    @@1             {set ECX to 1 if Pos greater than AnsiStrings.StrLen + 1}
  mov    ecx, 1

@@1:
  rep    movsb           {adjust tail of string}
  mov    [edi], ah       {insert the new character}

@@ExitPoint:
  cld                    {be a good neighbor}

  pop    esi             {restore registers}
  pop    edi
  pop    eax
end;

function StrStInsertPrimZ(Dest : PAnsiChar; S : PAnsiChar;
                         Pos : Cardinal) : PAnsiChar;
register;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi
  push   ebx

  mov    ebx, ecx        {move POS to ebx}
  mov    esi, eax        {copy Dest to ESI, S to EDI}
  mov    edi, edx

  xor    al, al          {zero}
  or     ecx, -1         {set ECX to $FFFFFFFF}
  repne  scasb           {find null terminator}
  not    ecx             {calc length of source string (including null)}
  dec    ecx             {length without null}
  jz     @@ExitPoint     {if source length = 0, exit}
  push   ecx             {save length for later}

  mov    edi, esi        {reset EDI to Dest}
  or     ecx, -1
  repne  scasb           {find null}
  not    ecx             {length of dest string (including null)}

  cmp    ebx, ecx
  jb     @@1
  mov    ebx, ecx
  dec    ebx

@@1:
  std                    {backwards string ops}
  pop    eax             {restore length of S from stack}
  add    edi, eax        {set EDI S beyond end of Dest}
  dec    edi             {back up one for null}

  add    esi, ecx        {set ESI to end of Dest}
  dec    esi             {back up one for null}
  sub    ecx, ebx        {# of chars in Dest that are past Pos}
  rep    movsb           {adjust tail of string}

  mov    esi, edx        {set ESI to S}
  add    esi, eax        {set ESI to end of S}
  dec    esi             {back up one for null}
  mov    ecx, eax        {# of chars in S}
  rep    movsb           {copy S into Dest}

  cld                    {be a good neighbor}

@@ExitPoint:

  pop    ebx             {restore registers}
  pop    esi
  pop    edi
  pop    eax
end;

function StrStCopyZ(Dest : PAnsiChar; S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
var
  Len : Cardinal;
begin
  Len := AnsiStrings.StrLen(S);
  if Pos < Len then begin
    if (Len-Pos) < Count then
      Count := Len-Pos;
    Move(S[Pos], Dest^, Count);
    Dest[Count] := #0;
  end else
    Dest[0] := #0;
  Result := Dest;
end;

{$IFDEF UNICODE}
function StrStCopyZ(Dest : PWideChar; S : PWideChar; Pos, Count : Cardinal) : PWideChar;
var
  Len : Cardinal;
begin
  Len := StrLen(S);
  if Pos < Len then begin
    if (Len-Pos) < Count then
      Count := Len-Pos;
    Move(S[Pos], Dest^, Count * SizeOf(WideChar));
    Dest[Count] := #0;
  end else
    Dest[0] := #0;
  Result := Dest;
end;
{$ENDIF}

function StrChDeletePrimZ(P : PAnsiChar; Pos : Cardinal) : PAnsiChar;
register;
asm
  push   edi             { Save because we will be changing them }
  push   esi
  push   ebx

  mov    ebx, eax        { Save P to EDI & EBX }
  mov    edi, eax

  xor    al, al          { Zero }
  or     ecx, -1         { Set ECX to $FFFFFFFF }
  repne  scasb           { Find null terminator }
  not    ecx
  dec    ecx
  or     ecx, ecx
  jz     @@ExitPoint
  sub    ecx, edx        { Calc number to move }
  jb     @@ExitPoint     { Exit if Pos > AnsiStrings.StrLen }

  mov    edi, ebx
  add    edi, edx        { Point to position to adjust }
  mov    esi, edi
  inc    esi             { Offset for source string }
  inc    ecx             { One more to include null terminator }
  rep    movsb           { Adjust the string }

@@ExitPoint:
  mov    eax, ebx
  pop    ebx             { restore registers }
  pop    esi
  pop    edi
end;

function StrStDeletePrimZ(P : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
register;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi
  push   ebx

  mov    ebx, ecx        {move Count to BX}
  mov    esi, eax        {move P to ESI and EDI}
  mov    edi, eax

  xor    eax, eax        {null}
  or     ecx, -1
  repne  scasb           {find null terminator}
  not    ecx             {calc length}
  or     ecx, ecx
  jz     @@ExitPoint

  sub    ecx, ebx        {subtract Count}
  sub    ecx, edx        {subtract Pos}
  jns    @@L1

  mov    edi,esi         {delete everything after Pos}
  add    edi,edx
  mov    [edi], al
  jmp    @@ExitPoint

@@L1:
  mov    edi,esi
  add    edi,edx         {point to position to adjust}
  mov    esi,edi
  add    esi,ebx         {point past string to delete in src}
  inc    ecx             {one more to include null terminator}
  rep    movsb           {adjust the string}

@@ExitPoint:

  pop    ebx            {restore registers}
  pop    esi
  pop    edi
  pop    eax
end;

function StrStDeletePrimZ(P : PWideChar; Pos, Count : Cardinal) : PWideChar; //SZ
register;
asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi
  push   ebx

  mov    ebx, ecx        {move Count to BX}
  mov    esi, eax        {move P to ESI and EDI}
  mov    edi, eax

  xor    eax, eax        {null}
  or     ecx, -1
  repne  scasw           {find null terminator}
  not    ecx             {calc length}
  or     ecx, ecx
  jz     @@ExitPoint

  sub    ecx, ebx        {subtract Count}
  sub    ecx, ebx        {subtract Count}
  sub    ecx, edx        {subtract Pos}
  sub    ecx, edx        {subtract Pos}
  jns    @@L1

  mov    edi,esi         {delete everything after Pos}
  add    edi,edx
  mov    [edi], ax
  jmp    @@ExitPoint

@@L1:
  mov    edi,esi
  add    edi,edx         {point to position to adjust}
  add    edi,edx         {point to position to adjust}
  mov    esi,edi
  add    esi,ebx         {point past string to delete in src}
  add    esi,ebx         {point past string to delete in src}
  inc    ecx             {one more to include null terminator}
  inc    ecx
  rep    movsw           {adjust the string}

@@ExitPoint:

  pop    ebx            {restore registers}
  pop    esi
  pop    edi
  pop    eax
end;

function StrChDeleteZ(Dest, S : PAnsiChar; Pos : Cardinal) : PAnsiChar;
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := StrChDeletePrimZ(Dest, Pos);
end;

function StrStDeleteZ(Dest, S : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar;
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := StrStDeletePrimZ(Dest, Pos, Count);
end;

function StrChInsertZ(Dest, S : PAnsiChar; C : AnsiChar; Pos : Cardinal) : PAnsiChar;
begin
  AnsiStrings.StrCopy(Dest, S);
  Result := StrChInsertPrimZ(Dest, C, Pos);
end;

function StrStInsertZ(Dest : PAnsiChar; S1, S2 : PAnsiChar; Pos : Cardinal) : PAnsiChar;
begin
  AnsiStrings.StrCopy(Dest, S1);
  Result := StrStInsertPrimZ(Dest, S2, Pos);
end;


{----------------------------------------------------------------------------}

function CopyLeftZ(Dest, S : PAnsiChar; Len : Cardinal) : PAnsiChar;
  {-Return the left Len characters of a string}
begin
  if (Len < 1) or (S[0] = #0) then
    Dest[0] := #0
  else
    Dest := StrStCopyZ(Dest, S, 0, Len);
  Result := Dest;
end;

{----------------------------------------------------------------------------}

function CopyMidZ(Dest, S : PAnsiChar; First, Len : Cardinal) : PAnsiChar;
  {-Return the mid part of a string}
begin
  if (First >= AnsiStrings.StrLen(S)) or (Integer(Len) < 1) or (S[0] = #0) then
    Dest[0] := #0
  else
    Dest := StrStCopyZ(Dest, S, First, Len);
  Result := Dest;
end;

{----------------------------------------------------------------------------}

function CopyRightZ(Dest, S : PAnsiChar; First : Cardinal) : PAnsiChar;
  {-Return the right characters of a string}
begin
  if (First >= AnsiStrings.StrLen(Dest)) or (S[0] = #0) then
    Dest[0] := #0
  else
    Dest := StrStCopyZ(Dest, S, First, AnsiStrings.StrLen(S)-First+1);
  Result := Dest;
end;

{----------------------------------------------------------------------------}
function CopyRightAbsZ(Dest, S : PAnsiChar; NumChars : Cardinal) : PAnsiChar;
  {-Return the right Len characters of a string}
var
  I : Cardinal;
begin
  if (AnsiStrings.StrLen(S) > NumChars) then begin
    I := AnsiStrings.StrLen(S) - NumChars;
    Dest := StrStCopyZ(Dest, S, I, NumChars)
  end else
    Dest := S;
  Result := Dest;
end;

{----------------------------------------------------------------------------}

function WordPosZ(S, WordDelims, AWord : PAnsiChar;
                  N : Cardinal; var Position : Cardinal) : Boolean;
  {-returns the Occurrence instance of a given word within a string}
var
  P,
  TmpStr  : PAnsiChar;
  Len,
  I,
  P1,
  P2      : Cardinal;
begin
  if (S[0] = #0) or (AWord[0] = #0) or
     (AnsiStrings.StrPos(S, AWord) = nil) or (N < 1) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  Result := False;
  Position := 0;

  GetMem(TmpStr, AnsiStrings.StrLen(S)+1);
  try
    AnsiStrings.StrCopy(TmpStr, S);
    I      := 0;
    Len    := AnsiStrings.StrLen(AWord);
    P  := AnsiStrings.StrPos(TmpStr, AWord);
    P1 := P - TmpStr;

    while (AnsiStrings.StrLen(TmpStr) > 0) do begin
      P2 := P1 + pred(Len);
      if (P1 = 0) then begin
        if (CharExistsZ(WordDelims, TmpStr[P2+1])) then begin
          Inc(I);
        end else
          StrStDeleteZ(TmpStr, TmpStr, 0, P2);
      end else if (CharExistsZ(WordDelims, TmpStr[P1-1])) and
                  ((CharExistsZ(WordDelims, TmpStr[P2+1])) or (P2+1 = AnsiStrings.StrLen(TmpStr))) then begin
        Inc(I);
      end else if ((P1 + pred(Len)) = AnsiStrings.StrLen(TmpStr)) then begin
        if (CharExistsZ(WordDelims, TmpStr[P1-1])) then
          Inc(I);
      end;

      if (I = N) then begin
        Result := True;
        Position := Position + P1;
        Exit;
      end;
      StrStDeletePrimZ(TmpStr, 0, P2+1);
      Position := Position + P2+1;
      P := AnsiStrings.StrPos(TmpStr, AWord);
      if (P <> nil) then
        P1 := P - TmpStr
      else
        break;
    end;
  finally
    FreeMem(TmpStr, AnsiStrings.StrLen(S)+1);
  end;
end;


{----------------------------------------------------------------------------}

function CopyFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                          N : Cardinal) : Boolean;
var
  P      : Cardinal;
begin
  if (WordPosZ(S, WordDelims, AWord, N, P)) then begin
    StrStCopyZ(Dest, S, P, AnsiStrings.StrLen(S)-P+1);
    Result := True;
  end else begin
    Dest[0] := #0;
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}


function DeleteFromNthWordZ(Dest, S, WordDelims, AWord : PAnsiChar;
                            N : Cardinal) : Boolean;
var
  P : Cardinal;
begin
  if (WordPosZ(S, WordDelims, AWord, N, P)) then begin
    StrStDeleteZ(Dest, S, P, AnsiStrings.StrLen(S)-P+1);
    Result := False;
  end else begin
    Dest[0] := #0;
    Result := False;
  end;
end;

{----------------------------------------------------------------------------}

function CopyFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                         N1, N2 : Cardinal) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosZ(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosZ(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        StrStCopyZ(Dest, S, P1, P2-P1);
      end else begin
        Result := False;
        Dest[0] := #0;
      end;
    end else begin
      Result := False;
      Dest[0] := #0;
    end;
  end else begin
    Result := False;
    Dest[0] := #0;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteFromToWordZ(Dest, S, WordDelims, Word1, Word2 : PAnsiChar;
                           N1, N2 : Cardinal) : Boolean;
var
  P1,
  P2  : Cardinal;
begin
  if (WordPosZ(S, WordDelims, Word1, N1, P1)) then begin
    if (WordPosZ(S, WordDelims, Word2, N2, P2)) then begin
      Dec(P2);
      if (P2 > P1) then begin
        Result := True;
        StrStDeleteZ(Dest, S, P1, P2-P1+1);
      end else begin
        Result := False;
        Dest[0] := #0;
      end;
    end else begin
      Result := False;
      Dest[0] := #0;
    end;
  end else begin
    Result := False;
    Dest[0] := #0;
  end;
end;

{----------------------------------------------------------------------------}

function CopyWithinZ(Dest, S, Delimiter : PAnsiChar; Strip : Boolean) : PAnsiChar;
var
  P1,
  P2     : Cardinal;
  L      : Cardinal;
  TmpStr : PAnsiChar;
begin
  if (S[0] = #0) or (Delimiter[0] = #0) or
     (AnsiStrings.StrPos(S, Delimiter) = nil) then begin
    Dest[0] := #0;
    Result := Dest;
  end else begin
    if (StrStPosZ(S, Delimiter, P1)) then begin
      L := AnsiStrings.StrLen(S) - (P1 + AnsiStrings.StrLen(Delimiter)) + 1;
      GetMem(TmpStr, L);
      try
        StrStCopyZ(TmpStr, S, P1 + AnsiStrings.StrLen(Delimiter), AnsiStrings.StrLen(S));
        if (StrStPosZ(TmpStr, Delimiter, P2)) then begin
          StrStCopyZ(Dest, TmpStr, 0, P2);
          if (not Strip) then
            AnsiStrings.StrCat(StrStInsertZ(Dest, Dest, Delimiter, 0), Delimiter)
        end else begin
          AnsiStrings.StrCopy(Dest, TmpStr);
          if (not Strip) then
            StrStInsertZ(Dest, Dest, Delimiter, 0);
        end;
      finally
        FreeMem(TmpStr, L);
      end;
    end;
    Result := Dest;
  end;
end;

{----------------------------------------------------------------------------}

function DeleteWithinZ(Dest, S, Delimiter : PAnsiChar) : PAnsiChar;
var
  P1,
  P2     : Cardinal;
  L      : Cardinal;
  TmpStr : PAnsiChar;
begin
  if (S[0] = #0) or (Delimiter[0] = #0) or
     (AnsiStrings.StrPos(S, Delimiter) = nil) then begin
    Dest[0] := #0;
    Result := Dest;
  end else begin
    if (StrStPosZ(S, Delimiter, P1)) then begin
      L := AnsiStrings.StrLen(S) - (P1 + AnsiStrings.StrLen(Delimiter)) + 1;
      GetMem(TmpStr, L);
      try
        StrStCopyZ(TmpStr, S, P1 + AnsiStrings.StrLen(Delimiter), AnsiStrings.StrLen(S));
        if not (StrStPosZ(TmpStr, Delimiter, P2)) then
          StrStCopyZ(Dest, S, 0, P1)
        else begin
          P2 := P2 + (2*AnsiStrings.StrLen(Delimiter));
          StrStDeleteZ(Dest, S, P1, P2);
        end;
      finally
        FreeMem(TmpStr, L);
      end;
    end;
    Result := Dest;
  end;
end;

{----------------------------------------------------------------------------}

function ReplaceWordZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                      N : Cardinal;
                      var Replacements : Cardinal) : PAnsiChar;
var
  I,
  C,
  P1  : Cardinal;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) or (OldWord[0] = #0) or
     (AnsiStrings.StrPos(S, OldWord) = nil) then begin
    AnsiStrings.StrCopy(Dest, S);
    Replacements := 0;
    Result := Dest;
  end else begin
    if (WordPosZ(S, WordDelims, OldWord, N, P1)) then begin
      AnsiStrings.StrCopy(Dest, S);
      StrStDeleteZ(Dest, Dest, P1, AnsiStrings.StrLen(OldWord));

      C := 0;
      for I := 1 to Replacements do begin
        if (AnsiStrings.StrLen(NewWord) + 1 + AnsiStrings.StrLen(Dest)) < High(Cardinal) then begin
          Inc(C);
          StrStInsertZ(Dest, Dest, NewWord, P1);
          Inc(P1, AnsiStrings.StrLen(NewWord) + 1);
        end else begin
          Replacements := C;
          Result := Dest;
          Exit;
        end;
      end;
      Result := Dest;
    end else begin
      Replacements := 0;
      Result := Dest;
    end;
  end;
end;


function ReplaceWordAllZ(Dest, S, WordDelims, OldWord, NewWord : PAnsiChar;
                         var Replacements : Cardinal) : PAnsiChar;
var
  I,
  C,
  P1  : Cardinal;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) or (OldWord[0] = #0) or
     (AnsiStrings.StrPos(S, OldWord) = nil) then begin
    Replacements := 0;
    AnsiStrings.StrCopy(Dest, S);
    Result := Dest;
  end else begin
    AnsiStrings.StrCopy(Dest, S);
    C := 0;
    while (WordPosZ(Dest, WordDelims, OldWord, 1, P1)) do begin
      StrStDeleteZ(Dest, Dest, P1, AnsiStrings.StrLen(OldWord));
      for I := 1 to Replacements do begin
        if ((AnsiStrings.StrLen(NewWord) + 1 + AnsiStrings.StrLen(Dest)) < High(Cardinal)) then begin
          Inc(C);
          StrStInsertZ(Dest, Dest, NewWord, P1);
        end else begin
          Result := Dest;
          Replacements := C;
          Exit;
        end;
      end;
    end;
    Replacements := C;
    Result := Dest;
  end;
end;


function ReplaceStringZ(Dest, S, OldString, NewString : PAnsiChar;
                        N : Cardinal;
                        var Replacements : Cardinal) : PAnsiChar;
var
  I,
  L,
  C,
  P1 : Cardinal;
  TmpStr : PAnsiChar;
begin
  if (S[0] = #0) or (OldString[0] = #0) or
     (AnsiStrings.StrPos(S, OldString) = nil) then begin
    AnsiStrings.StrCopy(Dest, S);
    Replacements := 0;
    Result := Dest;
    Exit;
  end;

  L := AnsiStrings.StrLen(S) + 1;
  GetMem(TmpStr, L);
  try
    AnsiStrings.StrCopy(TmpStr, S);

    I  := 1;
    StrStPosZ(TmpStr, OldString, P1);
    C  := P1;
    while (I < N) and (C < AnsiStrings.StrLen(TmpStr)) do begin
      Inc(I);
      StrStDeleteZ(TmpStr, TmpStr, P1, P1 + AnsiStrings.StrLen(OldString));
      Inc(C, P1 + AnsiStrings.StrLen(OldString));
    end;
  finally
    FreeMem(TmpStr, L);
  end;
  AnsiStrings.StrCopy(Dest, S);
  StrStDeleteZ(Dest, Dest, C, AnsiStrings.StrLen(OldString));

  C := 0;
  for I := 1 to Replacements do begin
    if ((AnsiStrings.StrLen(NewString) + 1 + AnsiStrings.StrLen(Dest)) < High(Cardinal)) then begin
      Inc(C);
      StrStInsertZ(Dest, Dest, NewString, P1);
      Inc(P1, AnsiStrings.StrLen(NewString) + 1);
    end else begin
      Replacements := C;
      Result := Dest;
      Exit;
    end;
  end;
  Result := Dest;
end;


function ReplaceStringAllZ(Dest, S, OldString, NewString : PAnsiChar;
                           var Replacements : Cardinal) : PAnsiChar;
var
  I,
  C,
  P1 : Cardinal;
begin
  if (S[0] = #0) or (OldString[0] = #0) or (AnsiStrings.StrPos(S, OldString) = nil) then begin
    AnsiStrings.StrCopy(Dest, S);
    Result := Dest;
    Replacements := 0;
  end else begin
    AnsiStrings.StrCopy(Dest, S);
    C  := 0;
    while StrStPosZ(Dest, OldString, P1) do begin
      StrStDeleteZ(Dest, Dest, P1, AnsiStrings.StrLen(OldString));
      for I := 1 to Replacements do begin
        if (Integer((AnsiStrings.StrLen(NewString) + 1 + AnsiStrings.StrLen(Dest))) < MaxLongInt) then begin
          Inc(C);
          StrStInsertZ(Dest, Dest, NewString, P1);
        end else begin
          Replacements := C;
          Result := Dest;
          Exit;
        end;
      end;
    end;
    Replacements := C;
    Result := Dest;
  end;
end;



function LastWordZ(S, WordDelims, AWord : PAnsiChar;
                   var Position : Cardinal) : Boolean;
var
  TmpStr1,
  TmpStr2 : PAnsiChar;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) or
     (AWord[0] = #0) or (AnsiStrings.StrPos(S, AWord) = nil) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  GetMem(TmpStr1, AnsiStrings.StrLen(S) + 1);
  GetMem(TmpStr2, AnsiStrings.StrLen(AWord) + 1);
  try
    AnsiStrings.StrCopy(TmpStr1, S);
    Position := AnsiStrings.StrLen(TmpStr1)-1;
    while (CharExistsZ(WordDelims, TmpStr1[Position])) do
      Dec(Position);

    Position := AnsiStrings.StrLen(TmpStr1)-1;
    repeat
      while (not CharExistsZ(WordDelims, TmpStr1[Position])) and (Position > 0) do
        Dec(Position);
      if (CompStringZ(
            StrStCopyZ(TmpStr2, TmpStr1, Position+1, AnsiStrings.StrLen(AWord)),
            AWord) = 0) then begin
        Inc(Position);
        Result := True;
        Exit;
      end;
      StrStDeleteZ(TmpStr1, TmpStr1, Position, AnsiStrings.StrLen(TmpStr1));
      Position := AnsiStrings.StrLen(TmpStr1)-1;
    until (Position = 0);
    Result := False;
    Position := 0;
  finally
    FreeMem(TmpStr1, AnsiStrings.StrLen(S)+1);
    FreeMem(TmpStr2, AnsiStrings.StrLen(AWord)+1);
  end;
end;

{----------------------------------------------------------------------------}

function LastWordAbsZ(S, WordDelims : PAnsiChar;
                      var Position : Cardinal) : Boolean;
begin
  if (S[0] = #0) or (WordDelims[0] = #0) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

{find first non-delimiter character, if any. If not, it's a "one word wonder"}
  Position := AnsiStrings.StrLen(S)-1;
  while (Position > 0) and (CharExistsZ(WordDelims, S[Position])) do
    Dec(Position);

  if (Position = 0) then begin
    if (CharExistsZ(WordDelims, S[Position])) then begin
      Result := True;
      Position := 1;
      Exit;
    end else begin
      Result := True;
      Exit;
    end;
  end;

{find next non-delimiter character}
  Result := True;
  while (Position > 0) and (not CharExistsZ(WordDelims, S[Position])) do
    Dec(Position);
  if (Position = 0) then begin
    if (CharExistsZ(WordDelims, S[Position])) then begin
      Position := 1;
    end else begin
      Position := 0;
    end;
  end else begin
    Inc(Position);
  end;
end;

{----------------------------------------------------------------------------}

function LastStringZ(S, AString : PAnsiChar;
                     var Position : Cardinal) : Boolean;
var
  TmpStr : PAnsiChar;
  I, C   : Cardinal;
begin
  if (S[0] = #0) or (AnsiStrings.StrPos(S, AString) = nil) then begin
    Result := False;
    Position := 0;
    Exit;
  end;

  GetMem(TmpStr, AnsiStrings.StrLen(S)+1);
  try
    AnsiStrings.StrCopy(TmpStr, S);
    C := 0;
    while (StrStPosZ(TmpStr, AString, I)) do begin
      Inc(C, I + AnsiStrings.StrLen(AString));
      StrStDeleteZ(TmpStr, TmpStr, 0, I + AnsiStrings.StrLen(AString));
    end;

  {Go back the length of AString since the while loop deletes the last instance}
    Dec(C, AnsiStrings.StrLen(AString));
    Position := C;
    Result := True;
  finally
    FreeMem(TmpStr, AnsiStrings.StrLen(S)+1);
  end;
end;


{----------------------------------------------------------------------------}

function KeepCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

begin
  Result := Dest;
  while (S^ <> #0) do begin
    if CharExistsZ(Chars, S^) then begin
      Dest^ := S^;
      inc(Dest);
    end;
    inc(S);
  end;
  Dest^ := #0;
end;

{----------------------------------------------------------------------------}

function RepeatStringZ(Dest, RepeatString : PAnsiChar;
                   var Repetitions        : Cardinal;
                       MaxLen             : Cardinal) : PAnsiChar;

var
  i    : Cardinal;
  Len  : Cardinal;
  ActualReps : Cardinal;
begin
  Result := Dest;
  Result^ := #0;
  Len := AnsiStrings.StrLen(RepeatString);
  if (MaxLen <> 0) and
     (Repetitions <> 0) and
     (Len <> 0) then begin
    ActualReps := MaxLen div Len;
    if (ActualReps > Repetitions) then
      ActualReps := Repetitions
    else
      Repetitions := ActualReps;
    if (ActualReps > 0) then begin
      for i := 0 to pred(ActualReps) do begin
        Move(RepeatString[0], Dest[0], Len);
        inc(Dest, Len);
      end;
      Dest^ := #0;
    end;
  end;
end;

{----------------------------------------------------------------------------}

function TrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

begin
  Result := LeftTrimCharsZ(Dest, RightTrimCharsZ(Dest, S, Chars), Chars);
end;

{----------------------------------------------------------------------------}

function RightTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

var
  EndS : PAnsiChar;
begin
  Result := Dest;
  EndS := AnsiStrings.StrEnd(S);
  while (EndS <> S) do begin
    dec(EndS);
    if not CharExistsZ(Chars, EndS^) then begin
      if (Dest <> S) then
        AnsiStrings.StrLCopy(Dest, S, succ(EndS - S))
      else begin
        inc(EndS);
        EndS^ := #0;
      end;
      Exit;
    end;
  end;
  Result^ := #0;
end;

{----------------------------------------------------------------------------}

function LeftTrimCharsZ(Dest, S, Chars : PAnsiChar) : PAnsiChar;

begin
  Result := Dest;
  while (S^ <> #0) and CharExistsZ(Chars, S^) do
    inc(S);
  if (S^ <> #0) then
    AnsiStrings.StrCopy(Result, S)
  else
    Result^ := #0;
end;

{----------------------------------------------------------------------------}

function ExtractTokensZ(S, Delims  : PAnsiChar;
                        QuoteChar  : AnsiChar;
                        AllowNulls : Boolean;
                        Tokens     : TStrings) : Cardinal;

var
  State : (ScanStart,
           ScanQuotedToken,
           ScanQuotedTokenEnd,
           ScanNormalToken,
           ScanNormalTokenWithQuote);
  CurChar    : PAnsiChar;
  TokenStart : PAnsiChar;
  TempStr    : PAnsiChar;
  SLen       : integer;
begin
  {Notes: this routine implements the following state machine
    start ----> ScanStart
    ScanStart-----quote----->ScanQuotedToken (4)
    ScanStart-----delim----->ScanStart (1)
    ScanStart-----other----->ScanNormalToken
    ScanQuotedToken-----quote----->ScanQuotedTokenEnd
    ScanQuotedToken-----other----->ScanQuotedToken
    ScanQuotedTokenEnd-----quote----->ScanNormalTokenWithQuote
    ScanQuotedTokenEnd-----delim----->ScanStart (2)
    ScanQuotedTokenEnd-----other----->ScanNormalToken
    ScanNormalToken-----quote----->ScanNormalTokenWithQuote (4)
    ScanNormalToken-----delim----->ScanStart (3)
    ScanNormalToken-----other----->ScanNormalToken
    ScanNormalTokenWithQuote-----quote----->ScanNormalTokenWithQuote
    ScanNormalTokenWithQuote-----other----->ScanNormalToken

    (1) output a null token if allowed
    (2) output a token, stripping quotes (if the dequoted token is
        empty, output a null token if allowed)
    (3) output a token; no quote stripping
    (4) if the quote character is #0, it's taken to mean that the
        routine should not check for quoted substrings. These marked
        transitions are the only places this is checked.}

  {clear the tokens string list, set the return value to zero}
  Tokens.Clear;
  Result := 0;

  {if the input string is empty or the delimiter list is empty or
   the quote character is found in the delimiter list, return zero
   tokens found}
  if (S[0] = #0) or
     (Delims[0] = #0) or
     CharExistsZ(Delims, QuoteChar) then
    Exit;

  {allocate ourselves some scratch space for temporary tokens}
  SLen := AnsiStrings.StrLen(S);
  GetMem(TempStr, SLen+1);
  try

    {start off in the normal scanning state}
    State := ScanStart;

    {the first token starts at the beginning of the string}
    TokenStart := S;

    {read through the entire string}
    CurChar := S;
    while (CurChar^ <> #0) do begin

      {process the character according to the current state}
      case State of
        ScanStart :
          begin
            {if the current char is the quote character, switch
             states}
            if (QuoteChar <> #0) and (CurChar^ = QuoteChar) then
              State := ScanQuotedToken

            {if the current char is a delimiter, output a null token}
            else if CharExistsZ(Delims, CurChar^) then begin

              {if allowed to, output a null token}
              if AllowNulls then begin
                Tokens.Add('');
                inc(Result);
              end;

              {set the start of the next token to be one character
               after this delimiter}
              TokenStart := CurChar + 1;
            end

            {otherwise, the current char is starting a normal token,
             so switch states}
            else
              State := ScanNormalToken
          end;

        ScanQuotedToken :
          begin
            {if the current char is the quote character, switch
             states}
            if (CurChar^ = QuoteChar) then
              State := ScanQuotedTokenEnd
          end;

        ScanQuotedTokenEnd :
          begin
            {if the current char is the quote character, we have a
             token consisting of two (or more) quoted substrings, so
             switch states}
            if (CurChar^ = QuoteChar) then
              State := ScanNormalTokenWithQuote

            {if the current char is a delimiter, output the token
             without the quotes}
            else if CharExistsZ(Delims, CurChar^) then begin

              {if the token is empty without the quotes, output a null
               token only if allowed to}
              if ((CurChar - TokenStart) = 2) then begin
                if AllowNulls then begin
                  Tokens.Add('');
                  inc(Result);
                end
              end

              {else output the token without the quotes}
              else begin
                inc(TokenStart);
                AnsiStrings.StrLCopy(TempStr, TokenStart, CurChar - TokenStart - 1);
                Tokens.Add(string(AnsiStrings.StrPas(TempStr)));
                inc(Result);
              end;

              {set the start of the next token to be one character
               after this delimiter}
              TokenStart := CurChar + 1;

              {switch states back to the start state}
              State := ScanStart;
            end

            {otherwise it's a (complex) normal token, so switch
             states}
            else
              State := ScanNormalToken
          end;

        ScanNormalToken :
          begin
            {if the current char is the quote character, we have a
             complex token with at least one quoted substring, so
             switch states}
            if (QuoteChar <> #0) and (CurChar^ = QuoteChar) then
              State := ScanNormalTokenWithQuote

            {if the current char is a delimiter, output the token}
            else if CharExistsZ(Delims, CurChar^) then begin
              AnsiStrings.StrLCopy(TempStr, TokenStart, CurChar - TokenStart);
              Tokens.Add(string(AnsiStrings.StrPas(TempStr)));
              inc(Result);

              {set the start of the next token to be one character
               after this delimiter}
              TokenStart := CurChar + 1;

              {switch states back to the start state}
              State := ScanStart;
            end;
          end;

        ScanNormalTokenWithQuote :
          begin
            {if the current char is the quote character, switch states
             back to scanning a normal token}
            if (CurChar^ = QuoteChar) then
              State := ScanNormalToken;
          end;

      end;

      inc(CurChar);
    end;

    {we need to process the (possible) final token}

    {if we are in the scanning quoted token state, we've read an
     opening quote, but no closing one; increment the token start
     value}
    if (State = ScanQuotedToken) then
      inc(TokenStart)

    {if we've finished scanning a quoted token, we've read both
     quotes; increment the token start value, and decrement the
     current index}
    else if (State = ScanQuotedTokenEnd) then begin
      inc(TokenStart);
      dec(CurChar);
    end;

    {if the final token is not empty, output the token}
    if (TokenStart < CurChar) then begin
      AnsiStrings.StrLCopy(TempStr, TokenStart, CurChar - TokenStart);
      Tokens.Add(string(AnsiStrings.StrPas(TempStr)));
      inc(Result);
    end
    {otherwise the final token is empty, so output a null token if
     allowed to}
    else if AllowNulls then begin
      Tokens.Add('');
      inc(Result);
    end;

  finally
    FreeMem(TempStr, SLen+1);
  end;
end;

{----------------------------------------------------------------------------}

function ContainsOnlyZ(const S, Chars : PAnsiChar;
                       var BadPos : Cardinal) : Boolean;
var
  Walker : PAnsiChar;
begin
  {if the input string is empty, exit}
  if (S^ = #0) then begin
    Result := false;
    BadPos := 0;
    Exit;
  end;
  {otherwise walk through the string until we reach the end or we find
   the first char not in our list}
  Walker := S;
  while (Walker^ <> #0) do begin
    if not CharExistsZ(Chars, Walker^) then begin
      BadPos := Walker - S;
      Result := false;
      Exit;
    end;
    inc(Walker);
  end;
  {if we reach here, all chars are in the list}
  Result := true;
  BadPos := 0;
end;

{----------------------------------------------------------------------------}

function ContainsOtherThanZ(const S, Chars : PAnsiChar;
                            var BadPos : Cardinal) : Boolean;
var
  Walker : PAnsiChar;
begin
  {if the input string is empty, exit}
  if (S^ = #0) then begin
    Result := false;
    BadPos := 0;
    Exit;
  end;
  {otherwise walk through the string until we reach the end or we find
   the first char not in our list}
  Walker := S;
  while (Walker^ <> #0) do begin
    if not CharExistsZ(Chars, Walker^) then begin
      BadPos := Walker - S;
      Result := true;
      Exit;
    end;
    inc(Walker);
  end;
  {if we reach here, all chars are in the list}
  Result := false;
  BadPos := 0;
end;

{----------------------------------------------------------------------------}

function IsChAlphaZ(C : Char) : Boolean;
 {-Returns true if Ch is an alpha}
begin
  Result := Windows.IsCharAlpha(C);
end;

{----------------------------------------------------------------------------}

function IsChNumericZ(C : AnsiChar; Numbers : PAnsiChar) : Boolean;
 {-Returns true if Ch in numeric set}
begin
  Result := CharExistsZ(Numbers, C);
end;

{----------------------------------------------------------------------------}

function IsChAlphaNumericZ(C : Char; Numbers : PChar) : Boolean;
  {-Returns true if Ch is an alpha or numeric}
begin
  Result := Windows.IsCharAlpha(C) or CharExistsZ(Numbers, C);
end;

{----------------------------------------------------------------------------}

function IsStrAlphaZ(S : PChar) : Boolean;
  {-Returns true if all characters in string are an alpha}
begin
  Result := false;
  if (S^ <> #0) then begin
    while (S^ <> #0) do begin
      if not Windows.IsCharAlpha(S^) then
        Exit;
      inc(S);
    end;
    Result := true;
  end;
end;

{----------------------------------------------------------------------------}

function IsStrNumericZ(S, Numbers : PAnsiChar) : Boolean;
  {-Returns true if all characters in string are in numeric set}
begin
  Result := false;
  if (S^ <> #0) then begin
    while (S^ <> #0) do begin
      if not CharExistsZ(Numbers, S^) then
        Exit;
      inc(S);
    end;
    Result := true;
  end;
end;

{----------------------------------------------------------------------------}

function IsStrAlphaNumericZ(S, Numbers : PChar) : Boolean;
  {-Returns true if all characters in string are alpha or numeric}
begin
  Result := false;
  if (S^ <> #0) then begin
    while (S^ <> #0) do begin
      if (not Windows.IsCharAlpha(S^)) and
         (not CharExistsZ(Numbers, S^)) then
        Exit;
      inc(S);
    end;
    Result := true;
  end;
end;


function StrWithinZ(S, SearchStr : PAnsiChar;
                    Start        : Cardinal;
                    var Position : Cardinal) : Boolean;
var
  TmpStr : PAnsiChar;
begin
  GetMem(TmpStr, AnsiStrings.StrLen(S) + 1);
  try
    AnsiStrings.StrCopy(TmpStr, S);
    if (Start > 0) then
      StrStDeleteZ(TmpStr, TmpStr, 0, Start);
    Result := StrStPosZ(TmpStr, SearchStr, Position);
    if (Result) then
      Position := Position + Start;
  finally
    FreeMem(TmpStr, AnsiStrings.StrLen(S) + 1);
  end;
end;


end.
