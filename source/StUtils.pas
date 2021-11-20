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
{* SysTools: StUtils.pas 4.04                            *}
{*********************************************************}
{* SysTools: Assorted utility routines                   *}
{*********************************************************}

{$I StDefine.inc}

{$IFDEF FPC}
  {$MODE Delphi}
  {$ASMMODE intel}
{$ENDIF}

unit StUtils;

interface

uses
  Windows, SysUtils, Classes,

  StConst, StBase, StDate,
  StStrL; { long string routines }

function SignL(L : Integer) : Integer;
  {-return sign of Integer value}
function SignF(F : Extended) : Integer;
  {-return sign of floating point value}

function MinWord(A, B : Word) : Word;
  {-Return the smaller of A and B}
function MidWord(W1, W2, W3 : Word) : Word;
  {-return the middle of three Word values}
function MaxWord(A, B : Word) : Word;
  {-Return the greater of A and B}

function MinLong(A, B : Integer) : Integer;
  {-Return the smaller of A and B}
function MidLong(L1, L2, L3 : Integer) : Integer;
  {-return the middle of three Integer values}
function MaxLong(A, B : Integer) : Integer;
  {-Return the greater of A and B}

function MinFloat(F1, F2 : Extended) : Extended;
  {-return the lesser of two floating point values}
function MidFloat(F1, F2, F3 : Extended) : Extended;
  {-return the middle of three floating point values}
function MaxFloat(F1, F2 : Extended) : Extended;
  {-return the greater of two floating point values}

{-Assorted utility routines. }

function MakeInteger16(H, L : Byte): SmallInt;
  {-Construct an integer from two bytes}

function MakeWord(H, L : Byte) : Word;
  {-Construct a word from two bytes}

function SwapNibble(B : Byte) : Byte;
  {-Swap the high and low nibbles of a byte}

function SwapWord(L : Integer) : Integer;
  {-Swap the low- and high-order words of a long integer}

procedure SetFlag(var Flags : Word; FlagMask : Word);
  {-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}

procedure ClearFlag(var Flags : Word; FlagMask : Word);
  {-Clear bit(s) in the parameter Flags. The bits to clear are specified in Flagmask}

function FlagIsSet(Flags, FlagMask : Word) : Boolean;
  {-Return True if the bit specified by FlagMask is set in Flags}

procedure SetByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}

procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte);
  {-Clear bit(s) in the parameter Flags. The bits to clear are specified in FlagMask}

function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean;
  {-Return True if the bit specified by FlagMask is set in the Flags parameter}

procedure SetLongFlag(var Flags : Integer; FlagMask : Integer);
  {-Set bit(s) in the parameter Flags. The bits to set are specified in FlagMask}


procedure ClearLongFlag(var Flags : Integer; FlagMask : Integer);
  {-Clear bit(s) in the parameter Flags. The bits to clear are specified in FlagMask}


function LongFlagIsSet(Flags, FlagMask : Integer) : Boolean;
  {-Return True if the bit specified by FlagMask is set in Flags}

procedure ExchangeBytes(var I, J : Byte);
  {-Exchange the values in two bytes}

procedure ExchangeLongInts(var I, J : Integer);
  {-Exchange the values in two long integers}

procedure ExchangeStructs(var I, J; Size : Integer);
  {-Exchange the values in two structures}

procedure FillWord(var ADest; ACount: Integer; AFiller : Word);
  {-Fill memory with a word-sized filler}

function AddWordToPtr(P : Pointer; W : Word) : Pointer;
  {-Add a word to a pointer.}

implementation

const
  ecOutOfMemory = 8;

function MakeInteger16(H, L : Byte): SmallInt;
begin
  Word(Result) := (H shl 8) or L;  {!!.02}
end;

function SwapNibble(B : Byte) : Byte;
begin
  Result := (B shr 4) or (B shl 4);
end;

function SwapWord(L : Integer) : Integer; register;
asm
  ror eax,16;
end;

procedure SetFlag(var Flags : Word; FlagMask : Word);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearFlag(var Flags : Word; FlagMask : Word);
begin
  Flags := Flags and (not FlagMask);
end;


function FlagIsSet(Flags, FlagMask : Word) : Boolean;
begin
  Result := (FlagMask AND Flags <> 0);
end;

procedure SetByteFlag(var Flags : Byte; FlagMask : Byte);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearByteFlag(var Flags : Byte; FlagMask : Byte);
begin
  Flags := Flags and (not FlagMask);
end;

function ByteFlagIsSet(Flags, FlagMask : Byte) : Boolean;
begin
  Result := (FlagMask AND Flags <> 0);
end;

procedure SetLongFlag(var Flags : Integer; FlagMask : Integer);
begin
  Flags := Flags or FlagMask;
end;

procedure ClearLongFlag(var Flags : Integer; FlagMask : Integer);
begin
  Flags := Flags and (not FlagMask);
end;

function LongFlagIsSet(Flags, FlagMask : Integer) : Boolean;
begin
  Result := FlagMask = (Flags and FlagMask);
end;

procedure ExchangeBytes(var I, J : Byte);
register;
asm
  mov  cl, [eax]
  mov  ch, [edx]
  mov  [edx], cl
  mov  [eax], ch
end;

procedure ExchangeLongInts(var I, J : Integer);
var
  iTemp: Integer;
begin
  iTemp := I;
  I := J;
  J := iTemp;
end;

procedure ExchangeStructs(var I, J; Size : Integer);
var
  pTemp: TBytes;
begin
  if Size <= 0 then
    Exit;

  SetLength(pTemp, Size);
  Move(I, pTemp[0], Length(pTemp));
  Move(J, I, Length(pTemp));
  Move(pTemp[0], J, Length(pTemp));
end;

procedure FillWord(var ADest; ACount: Integer; AFiller : Word);
var
  pDest: Pointer;
  iCount: Integer;
begin
  pDest := @ADest;
  for iCount := 0 to ACount - 1 do
  begin
    Move(AFiller, pDest^, SizeOf(AFiller));
    Inc(NativeInt(pDest), SizeOf(AFiller));
  end;
end;

function AddWordToPtr(P : Pointer; W : Word) : Pointer;
begin
  Result := Pointer(Integer(P)+W);
end;

function MakeWord(H, L : Byte) : Word;
begin
  Result := (Word(H) shl 8) or L;
end;

function MinWord(A, B : Word) : Word;
begin
  if A < B then
     Result := A
  else
     Result := B;
end;

function MaxWord(A, B : Word) : Word;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MinLong(A, B : Integer) : Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxLong(A, B : Integer) : Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function SignL(L : Integer) : Integer;
  {-return sign of Integer value}
begin
  if L < 0 then
    Result := -1
  else if L = 0 then
    Result := 0
  else
    Result := 1;
end;

function SignF(F : Extended) : Integer;
  {-return sign of floating point value}
begin
  if F < 0 then
    Result := -1
  else if F = 0 then
    Result := 0
  else
    Result := 1;
end;

function MidWord(W1, W2, W3 : Word) : Word;
  {return the middle of three Word values}
begin
  Result := StUtils.MinWord(StUtils.MinWord(StUtils.MaxWord(W1, W2),
                            StUtils.MaxWord(W2, W3)), StUtils.MaxWord(W1, W3));
end;

function MidLong(L1, L2, L3 : Integer) : Integer;
  {return the middle of three Integer values}
begin
  Result := StUtils.MinLong(StUtils.MinLong(StUtils.MaxLong(L1, L2),
                            StUtils.MaxLong(L2, L3)), StUtils.MaxLong(L1, L3));
end;

function MidFloat(F1, F2, F3 : Extended) : Extended;
  {return the middle of three floating point values}
begin
  Result := MinFloat(MinFloat(MaxFloat(F1, F2), MaxFloat(F2, F3)), MaxFloat(F1, F3));
end;

function MinFloat(F1, F2 : Extended) : Extended;
  {-return the lesser of two floating point values}
begin
  if F1 <= F2 then
    Result := F1
  else
    Result := F2;
end;

function MaxFloat(F1, F2 : Extended) : Extended;
  {-return the greater of two floating point values}
begin
  if F1 > F2 then
    Result := F1
  else
    Result := F2;
end;


end.



