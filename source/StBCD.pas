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
{* SysTools: StBCD.pas 4.04                              *}
{*********************************************************}
{* SysTools: BCD arithmetic functions                    *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  The BCD format matches that defined by Turbo Pascal 3.0. It is as follows:

     LSB                      MSB (most significant byte at end)
      |<------ Mantissa ------>|
    1  2  3  4  5  6  7  8  9 10  <- Byte #
   sE ML ML ML ML ML ML ML ML ML
    ^                         ^^--- Less significant digit
    |                         |---- More significant digit
    |
    v
    7 6 5 4 3 2 1 0 <-- Bit # (in Byte 1)
    s E E E E E E E
    ^ <--exponent->
    |       |
    |       |--- exponent has offset of $3F (eg, $41 means 10^2 = 100)
    |----------- sign bit (0 = positive, 1 = negative)

   Unpacked BCD format
   -------------------
   Many of the routines that follow work with these reals in an unpacked
   format. That is, before an arithmetic operation is performed, the mantissas
   are expanded (unpacked) so that there is one digit per byte. After unpacking,
   the reals look like this:

     LSB                                                MSB
      |<------------------ mantissa --------------------->|
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
   sE 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 0d 00
                                                         ^^
                                                         ||---- Digit
                                                         |----- 0
   Byte 1 is unchanged.
   Bytes 2-19 contain the digits in the mantissa, LSB first. The high
     nibble of each byte is 0, and the low nibble contains the digit.
   Byte 20, sometimes used to keep track of overflow, is set to 0.

   The constant BcdSize determines the size and accuracy of the Bcd
   routines. It can be any value in the range 4-20 bytes. The default
   value of 10 gives 18 digits of accuracy. A size of 20 gives 38 digits
   of accuracy.

   The BCD routines are thread-aware; all temporary variables are local.

   STBCD uses the DecimalSeparator global variable from the SYSUTILS unit
   wherever it needs a decimal point. As such the formatting of BCD
   strings is aware of international differences.

   The transcendental routines (Sqrt, Ln, Exp, Pow) are accurate for
   all but 1 or 2 of the available digits of storage. For BcdSize =
   10, this means 16-17 accurate digits; for BcdSize = 20, this means
   36-37 accurate digits. The last digit or two is lost to roundoff
   errors during the calculations.

   Algorithms used for transcendental routines (depending on BcdSize):
     Sqrt:
       Herron's iterative approximation
     Exp:
       <= 10 bytes, Chebyshev polynomials per Cody and Waite
       > 10 bytes, traditional series expansion
     Ln:
       <= 10 bytes, Chebyshev polynomials of rational approximation
         per Cody and Waite
       > 10 bytes, Carlson's iterative approximation
     Pow:
       straight multiplication for integer powers
       use of Exp and Ln for non-integer powers

   Computation of Exp and Ln for BcdSize > 10 bytes is quite slow. Exp
   takes up to 30 terms to fill in all the digits when BcdSize = 20.
   Ln takes 9 iterations for BcdSize = 20, but each iteration is complicated
   and involves a sqrt, a divide, and other simpler operations.

   FormatBcd mimics the FormatFloat routine from the SYSUTILS unit.
   StrGeneralBcd mimics the FloatToStrF routine with the ffGeneral option.
   See the documentation for those routines for more information.
}


unit StBCD;

interface

uses
  Windows,
  SysUtils,
  StConst,
  StBase,
  StStrL;

const
  BcdSize = 10;                   {bytes in BCD, valid range 4-20}
  {.Z+}
  MantissaDigits = 2*(BcdSize-1); {digits in mantissa}
  OverflowChar = '*';             {character used to fill an overflow string}
  {.Z-}

type
  TBcd = array[0..BcdSize-1] of Byte;

var
  {these values are set up by the initialization block}
  ZeroBcd : TBcd;
  MinBcd  : TBcd;
  MaxBcd  : TBcd;
  BadBcd  : TBcd;
  PiBcd   : TBcd;
  eBcd    : TBcd;
  Ln10Bcd : TBcd;

{$IFNDEF CBuilder}
function AddBcd(const B1, B2 : TBcd) : TBcd;
  {-Return B1+B2}
function SubBcd(const B1, B2 : TBcd) : TBcd;
  {-Return B1-B2}
function MulBcd(const B1, B2 : TBcd) : TBcd;
  {-Return B1*B2}
function DivBcd(const B1, B2 : TBcd) : TBcd;
  {-Return B1/B2}
function ModBcd(const B1, B2 : TBcd) : TBcd;
  {-Return B1 mod B2}
function NegBcd(const B : TBcd) : TBcd;
  {-Return the negative of B}
function AbsBcd(const B : TBcd) : TBcd;
  {-Return the absolute value of B}
function FracBcd(const B : TBcd) : TBcd;
  {-Return the fractional part of B}
function IntBcd(const B : TBcd) : TBcd;
  {-Return the integer part of B, as a BCD real}
function RoundDigitsBcd(const B : TBcd; Digits : Cardinal) : TBcd;
  {-Return B rounded to specified total digits of accuracy}
function RoundPlacesBcd(const B : TBcd; Places : Cardinal) : TBcd;
  {-Return B rounded to specified decimal places of accuracy}
function ValBcd(const S : string) : TBcd;
  {-Convert a string to a BCD}
function LongBcd(L : LongInt) : TBcd;
  {-Convert a long integer to a BCD}
function ExtBcd(E : Extended) : TBcd;
  {-Convert an extended real to a BCD}
function ExpBcd(const B : TBcd) : TBcd;
  {-Return e**B}
function LnBcd(const B : TBcd) : TBcd;
  {-Return natural log of B}
function IntPowBcd(const B : TBcd; E : LongInt) : TBcd;
  {-Return B**E, where E is an integer}
function PowBcd(const B, E : TBcd) : TBcd;
  {-Return B**E}
function SqrtBcd(const B : TBcd) : TBcd;
  {-Return the square root of B}
{$ENDIF}

function CmpBcd(const B1, B2 : TBcd) : Integer;
  {-Return <0 if B1<B2, =0 if B1=B2, >0 if B1>B2}
function EqDigitsBcd(const B1, B2 : TBcd; Digits : Cardinal) : Boolean;
  {-Return True if B1 and B2 are equal after rounding to specified digits}
function EqPlacesBcd(const B1, B2 : TBcd; Digits : Cardinal) : Boolean;
  {-Return True if B1 and B2 are equal after rounding to specified decimal places}
function IsIntBcd(const B : TBcd) : Boolean;
  {-Return True if B has no fractional part (may still not fit into a LongInt)}
function TruncBcd(const B : TBcd) : LongInt;
  {-Return B after discarding its fractional part}
function BcdExt(const B : TBcd) : Extended;
  {-Convert B to an extended real}
function RoundBcd(const B : TBcd) : LongInt;
  {-Round B rounded to the nearest integer}
function StrBcd(const B : TBcd; Width, Places : Cardinal) : string;
  {-Convert BCD to a string in floating point format}
function StrExpBcd(const B : TBcd; Width : Cardinal) : string;
  {-Convert BCD to a string in scientific format}
function FormatBcd(const Format: string; const B : TBcd): string;
  {-Format a BCD like FormatFloat does for Extended}
function StrGeneralBcd(const B : TBcd) : string;
  {-Format a BCD like FloatToStrF does with ffGeneral format, MantissaDigits
    for Precision, and zero for Digits}
function FloatFormBcd(const Mask : string; B : TBCD;
                      const LtCurr, RtCurr : string;
                      Sep, DecPt : Char) : string;
  {-Returns a formatted string with digits from B merged into the Mask}
procedure ConvertBcd(const SrcB; SrcSize : Byte; var DestB; DestSize : Byte);
  {-Convert a BCD of one size to another size}

{the following routines are provided to support C++Builder}
{$IFDEF CBuilder}
procedure AddBcd_C(const B1, B2 : TBcd; var Res : TBcd);
procedure SubBcd_C(const B1, B2 : TBcd; var Res : TBcd);
procedure MulBcd_C(const B1, B2 : TBcd; var Res : TBcd);
procedure DivBcd_C(const B1, B2 : TBcd; var Res : TBcd);
procedure ModBcd_C(const B1, B2 : TBcd; var Res : TBcd);
procedure NegBcd_C(const B : TBcd; var Res : TBcd);
procedure AbsBcd_C(const B : TBcd; var Res : TBcd);
procedure FracBcd_C(const B : TBcd; var Res : TBcd);
procedure IntBcd_C(const B : TBcd; var Res : TBcd);
procedure RoundDigitsBcd_C(const B : TBcd; Digits : Cardinal; var Res : TBcd);
procedure RoundPlacesBcd_C(const B : TBcd; Places : Cardinal; var Res : TBcd);
procedure ValBcd_C(const S : string; var Res : TBcd);
procedure LongBcd_C(L : LongInt; var Res : TBcd);
procedure ExtBcd_C(E : Extended; var Res : TBcd);
procedure ExpBcd_C(const B : TBcd; var Res : TBcd);
procedure LnBcd_C(const B : TBcd; var Res : TBcd);
procedure IntPowBcd_C(const B : TBcd; E : LongInt; var Res : TBcd);
procedure PowBcd_C(const B, E : TBcd; var Res : TBcd);
procedure SqrtBcd_C(const B : TBcd; var Res : TBcd);
{$ENDIF}

{the following function is interfaced to avoid hints from the compiler}
{for its non use when the BcdSize constant is set a value less than 11}
{$IFNDEF CBuilder}
function LnBcd20(const B : TBcd) : TBcd;
{$ENDIF}

{=========================================================}

implementation

{Define to use assembly language in primitive routines below}
{$DEFINE UseAsm}

const
  NoSignBit = $7F;                {mask to get just the exponent}
  SignBit   = $80;                {mask to get just the sign}
  ExpBias   = $3F;                {bias added to actual exponent value}
  SigDigits = MantissaDigits+1;   {counts overflow digit}

type
  TUnpBcd = array[0..SigDigits] of Byte;   {unpacked BCD}
  PUnpBcd = ^TUnpBcd;
  TIntBcd = array[0..4*BcdSize-1] of Byte; {double size buffer for mult/div}

{$IFDEF CBuilder}
function AddBcd(const B1, B2 : TBcd) : TBcd; forward;
function SubBcd(const B1, B2 : TBcd) : TBcd; forward;
function MulBcd(const B1, B2 : TBcd) : TBcd; forward;
function DivBcd(const B1, B2 : TBcd) : TBcd; forward;
function ModBcd(const B1, B2 : TBcd) : TBcd; forward;
function NegBcd(const B : TBcd) : TBcd; forward;
function AbsBcd(const B : TBcd) : TBcd; forward;
function FracBcd(const B : TBcd) : TBcd; forward;
function IntBcd(const B : TBcd) : TBcd; forward;
function RoundDigitsBcd(const B : TBcd; Digits : Cardinal) : TBcd; forward;
function RoundPlacesBcd(const B : TBcd; Places : Cardinal) : TBcd; forward;
function ValBcd(const S : string) : TBcd; forward;
function LongBcd(L : LongInt) : TBcd; forward;
function ExtBcd(E : Extended) : TBcd; forward;
function ExpBcd(const B : TBcd) : TBcd; forward;
function LnBcd(const B : TBcd) : TBcd; forward;
function IntPowBcd(const B : TBcd; E : LongInt) : TBcd; forward;
function PowBcd(const B, E : TBcd) : TBcd; forward;
function SqrtBcd(const B : TBcd) : TBcd; forward;
{$ENDIF}

function FastValPrep(S : String) : String;
var
  I : LongInt;
begin
  I := Pos('.', S);
  if I > 0 then
    S[I] := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator;
  Result := S;
end;

procedure RaiseBcdError(Code : LongInt);
var
  E : EStBCDError;
begin
  E := EStBCDError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure AddMantissas(const UB1 : TUnpBcd; var UB2 : TUnpBcd);
{$IFDEF UseAsm}
  asm
    push esi
    push edi
    mov esi,UB1
    mov edi,UB2
    {inc esi}
    {inc edi}
    mov ecx,SigDigits
    clc
@1: mov al,[esi]  {UB1}
    inc esi
    adc al,[edi]  {UB1+UB2+CF}
    aaa
    mov [edi],al  {update UB2}
    inc edi
    dec ecx
    jnz @1
    jnc @2
    inc byte ptr [edi]
@2: pop edi
    pop esi
  end;
{$ELSE}
var
  I : Integer;
  T, C : Byte;
begin
  C := 0;
  for I := 0 to MantissaDigits do begin
    T := UB2[I]+UB1[I]+C;
    if T > 9 then begin
      C := 1;
      dec(T, 10);
    end else
      C := 0;
    UB2[I] := T;
  end;
  UB2[SigDigits] := C;
end;
{$ENDIF}

function IsZeroMantissa(const UB : TUnpBcd) : Boolean;
{$IFDEF UseAsm}
 asm
   push edi
   mov edi,UB
   {inc edi}
   xor al,al
   mov ecx,SigDigits
   repe scasb
   jne @1
   inc al
@1:pop edi
 end;
{$ELSE}
var
  I : Integer;
begin
  for I := 0 to MantissaDigits do
    if UB[I] <> 0 then begin
      Result := False;
      Exit;
    end;
  Result := True;
end;
{$ENDIF}

procedure NegMantissa(var UB : TUnpBcd);
{$IFDEF UseAsm}
  asm
    push edi
    mov edi,UB
    {inc edi}
    mov ecx,SigDigits
    xor dh,dh
    clc
@1: mov al,dh
    sbb al,[edi]
    aas
    mov [edi],al
    inc edi
    dec ecx
    jnz @1
    pop edi
  end;
{$ELSE}
var
  I : Integer;
  C : Byte;
begin
  C := 1;
  for I := 0 to MantissaDigits do begin
    UB[I] := 9+C-UB[I];
    if UB[I] > 9 then begin
      dec(UB[I], 10);
      C := 1;
    end else
      C := 0;
  end;
end;
{$ENDIF}

procedure NormalizeMantissa(var UB : TunpBcd; var E : Integer);
var
  I, Shift : Integer;
begin
  {find most significant non-zero digit}
  for I := MantissaDigits downto 0 do
    if UB[I] <> 0 then begin
      Shift := MantissaDigits-I;
      if Shift >= E then begin
        {number disappears}
        E := 0;
        FillChar(UB[0], SigDigits, 0);
      end else if Shift <> 0 then begin
        dec(E, Shift);
        move(UB[0], UB[Shift], SigDigits-Shift);
        FillChar(UB[0], Shift, 0);
      end;
      Exit;
    end;
  {mantissa is all zeros}
  E := 0;
end;

procedure SetZero(var B : TBcd);
begin
  FillChar(B, SizeOf(TBcd), 0);
end;

procedure Pack(const UB : TUnpBcd; Exponent : Integer; Sign : Byte;
               var B : TBcd);
{$IFNDEF UseAsm}
var
  I : Integer;
{$ENDIF}
begin
  if Exponent <= 0 then
    SetZero(B)

  else begin
    B[0] := Sign or Exponent;
    {repack digits}
{$IFDEF UseAsm}
    asm
      push esi
      push edi
      mov esi,UB
      mov edi,B
      inc esi
      inc edi
      mov ecx,BcdSize-1
@1:   mov ax,[esi]
      inc esi
      inc esi
      shl ah,4
      or  al,ah
      mov [edi],al
      inc edi
      dec ecx
      jnz @1
      pop edi
      pop esi
    end;
{$ELSE}
    for I := 1 to BcdSize-1 do
      B[I] := UB[2*I-1] or (UB[2*I] shl 4);
    {overflow digit ignored}
{$ENDIF}
  end;
end;

procedure RoundMantissa(var UB : TUnpBcd; Start : Integer);
var
{$IFNDEF UseAsm}
  I : Integer;
{$ENDIF}
  C : Byte;
begin
  if Start > MantissaDigits then begin
    Start := SigDigits;
    C := 0;
  end else
    C := UB[Start];
  FillChar(UB[1], Start, 0);
  if C < 5 then
    Exit;
{$IFDEF UseAsm}
  asm
    push edi
    mov edi,UB
    mov eax,Start
    add edi,eax
    inc edi
    mov ecx,MantissaDigits
    sub ecx,eax
    jle  @2
    stc
@1: mov al,[edi]
    adc al,0
    aaa
    mov [edi],al
    inc edi
    jnc @3
    dec ecx
    jnz @1
@2: inc byte ptr [edi]
@3: pop edi
  end;
{$ELSE}
  C := 1;
  for I := Start+1 to MantissaDigits do begin
    inc(UB[I], C);
    if UB[I] > 9 then begin
      dec(UB[I], 10);
      C := 1;
    end else
      {done rounding}
      Exit;
  end;
  {set overflow digit if we get here}
  inc(UB[SigDigits]);
{$ENDIF}
end;

procedure ShiftMantissaDown(var UB : TUnpBcd; Shift : Integer);
begin
  if Shift > MantissaDigits then
    {UB disappears when shifted}
    FillChar(UB[0], SigDigits+1, 0)

  else if Shift > 0 then begin
    Move(UB[Shift], UB[0], SigDigits+1-Shift);
    FillChar(UB[SigDigits+1-Shift], Shift, 0);
  end;
end;

procedure SubMantissas(const UB1 : TUnpBcd; var UB2 : TUnpBcd);
{$IFDEF UseAsm}
  asm
    push esi
    push edi
    mov esi,UB1
    mov edi,UB2
    {inc esi}
    {inc edi}
    mov ecx,SigDigits
    clc
@1: mov al,[edi]  {UB2}
    sbb al,[esi]  {UB2-UB1-CF}
    aas
    mov [edi],al  {update UB2}
    inc edi
    inc esi
    dec ecx
    jnz @1
    jnc @2
    inc byte ptr [edi]
@2: pop edi
    pop esi
  end;
{$ELSE}
var
  I : Integer;
  T, C : ShortInt;
begin
  C := 0;
  for I := 0 to MantissaDigits do begin
    T := UB2[I]-UB1[I]-C;
    if T < 0 then begin
      C := 1;
      inc(T, 10);
    end else
      C := 0;
    UB2[I] := T;
  end;
  UB2[SigDigits] := C;
end;
{$ENDIF}

procedure Unpack(const B : TBcd; var UB : TUnpBcd;
                 var Exponent : Integer; var Sign : Byte);
{$IFNDEF UseAsm}
var
  I : Integer;
{$ENDIF}
begin
{$IFDEF UseAsm}
  asm
    {$IFDEF VER140}
    push ecx  { get round a compiler bug in D6 }
    {$ENDIF}
    push esi
    push edi
    mov esi,B
    mov edi,UB
    inc esi
    inc edi
    mov ecx,BcdSize-1
@1: mov al,[esi]
    inc esi
    mov ah,al
    and al,$0F
    shr ah,4
    mov [edi],ax
    inc edi
    inc edi
    dec ecx
    jnz @1
    xor al,al
    mov [edi],al
    pop edi
    pop esi
    {$IFDEF VER140}
    pop ecx  { get round a compiler bug in D6 }
    {$ENDIF}
  end;
{$ELSE}
  {unpack digits}
  for I := 1 to BcdSize-1 do begin
    UB[2*I-1] := B[I] and $F;
    UB[2*I] := B[I] shr 4;
  end;
  {set last overflow digit to zero}
  UB[2*BcdSize-1] := 0;
{$ENDIF}

  {copy sign/exponent}
  UB[0] := 0;
  Exponent := B[0] and NoSignBit;
  Sign := B[0] and SignBit;
end;

{----------------------------------------------------------------------}

function AbsBcd(const B : TBcd) : TBcd;
begin
  Result := B;
  Result[0] := B[0] and noSignBit;
end;

function AddBcd(const B1, B2 : TBcd) : TBcd;
var
  E1, E2 : Integer;
  S1, S2 : Byte;
  UB1, UB2 : TUnpBcd;
begin
  if B1[0] = 0 then
    Result :=  B2

  else if B2[0] = 0 then
    Result := B1

  else begin
    Unpack(B1, UB1, E1, S1);
    Unpack(B2, UB2, E2, S2);

    If E1 < E2 then begin
      {shift UB1's mantissa to account for smaller exponent}
      RoundMantissa(UB1, E2-E1-1);
      ShiftMantissaDown(UB1, E2-E1);
    end else if E1 > E2 then begin
      {shift UB2's mantissa to account for smaller exponent}
      RoundMantissa(UB2, E1-E2-1);
      ShiftMantissaDown(UB2, E1-E2);
      E2 := E1;
    end;

    if S1 <> S2 then begin
      {differing signs}
      SubMantissas(UB1, UB2);
      if UB2[SigDigits] <> 0 then begin
        {negative result}
        S2 := S2 xor SignBit;
        UB2[SigDigits] := 0;
        NegMantissa(UB2);
      end;
      {shift to get rid of any leading zeros}
      NormalizeMantissa(UB2, E2);
    end else begin
      {same signs}
      AddMantissas(UB1, UB2);
      if UB2[SigDigits] = 0 then
        RoundMantissa(UB2, 0);
      if UB2[SigDigits] <> 0 then begin
        {temporary overflow}
        RoundMantissa(UB2, 1);
        ShiftMantissaDown(UB2, 1);
        inc(E2);
        if E2 > NoSignBit then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
      end;
    end;

    {set sign and exponent}
    if E2 = 0 then
      UB2[0] := 0
    else
      UB2[0] := S2 or E2;

    Pack(UB2, E2, S2, Result);
  end;
end;

function BcdExt(const B : TBcd) : Extended;
var
  Code : Integer;
  S : string[59];
begin
  S := StrExpBcd(B, 0);
  if ({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator <> '.') then begin
    while (pos({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator, S) > 0) do
      S[pos({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator, S)] := '.';
  end;
  Val(S, Result, Code);
end;

procedure ConvertBcd(const SrcB; SrcSize : Byte; var DestB; DestSize : Byte);
label
  Repack;
type
  TBA = array[0..40] of Byte; {largest BCD size times 2}
  PBA = ^TBA;
var
  I, O, Exponent : Integer;
  PS : PBA;
  C : Byte;
begin
  if (SrcSize = 0) or (DestSize = 0) then
    exit;

  Exponent := TBA(SrcB)[0] and NoSignBit;

  {transfer mantissa}
  if SrcSize <= DestSize then begin
    {dest is at least as big as src}
    FillChar(TBA(DestB)[1], DestSize-SrcSize, 0);
    Move(TBA(SrcB)[1], TBA(DestB)[DestSize-SrcSize+1], SrcSize-1);

  end else begin
    {need to round src before copying to dest}
    GetMem(PS, 2*SrcSize);

    {unpack digits}
    for I := 1 to SrcSize-1 do begin
      PS^[2*I-1] := TBA(SrcB)[I] and $F;
      PS^[2*I] := TBA(SrcB)[I] shr 4;
    end;
    {set last overflow digit to zero}
    PS^[2*SrcSize-1] := 0;
    {O is a shift used when rounding causes an overflow}
    O := 0;

    {round src starting at most significant lost digit}
    if PS^[SrcSize-DestSize] >= 5 then begin
      {rounding has an effect}
      C := 1;
      for I := SrcSize-DestSize+1 to 2*(SrcSize-1) do begin
        inc(PS^[I], C);
        if PS^[I] > 9 then begin
          dec(PS^[I], 10);
          C := 1;
        end else
          {done rounding}
          goto Repack;
      end;
      {set overflow digit if we get here}
      PS^[2*SrcSize-1] := 1;
      inc(Exponent);
      O := 1;
    end;

Repack:
    {repack into same buffer taking account of overflow offset}
    for I := 1 to SrcSize-1 do
      PS^[I] := PS^[2*I-1+O] or (PS^[2*I+O] shl 4);

    {copy rounded src into dest}
    Move(PS^[SrcSize-DestSize+1], TBA(DestB)[1], DestSize-1);

    FreeMem(PS, 2*SrcSize);
  end;

  {copy sign/exponent}
  TBA(DestB)[0] := Exponent or (TBA(SrcB)[0] and SignBit);
end;

function EqDigitsBcd(const B1, B2 : TBcd; Digits : Cardinal) : Boolean;
begin
  Result := (CmpBcd(RoundDigitsBcd(B1, Digits), RoundDigitsBcd(B2, Digits)) = 0);
end;

function EqPlacesBcd(const B1, B2 : TBcd; Digits : Cardinal) : Boolean;
begin
  Result := (CmpBcd(RoundPlacesBcd(B1, Digits), RoundPlacesBcd(B2, Digits)) = 0);
end;

function CmpBcd(const B1, B2 : TBcd) : Integer;
var
{$IFNDEF UseAsm}
  I : Integer;
{$ENDIF}
  E1, E2 : Integer;
  S1, S2 : Byte;
  UB1, UB2 : TUnpBcd;
begin
  Unpack(B1, UB1, E1, S1);
  Unpack(B2, UB2, E2, S2);

  if S1 <> S2 then
    {signs differ}
    Result := Integer(S2)-S1

  else begin
    {signs the same}
    if E1 <> E2 then
      {exponents differ}
      Result := E1-E2

    else if E1 = 0 then
      {both numbers are zero}
      Result := 0

    else begin
      {exponents the same, compare the mantissas}
{$IFDEF UseAsm}
      asm
        push esi
        push edi
        lea esi,UB1+MantissaDigits
        lea edi,UB2+MantissaDigits
        mov ecx,MantissaDigits
@1:     mov al,[esi]
        sub al,[edi]
        jnz @2
        dec esi
        dec edi
        dec ecx
        jnz @1
@2:     movsx eax,al
        mov Result,eax
        pop edi
        pop esi
      end;
{$ELSE}
      for I := MantissaDigits downto 1 do begin
        Result := Integer(UB1[I])-UB2[I];
        if Result <> 0 then
          break;
      end;
{$ENDIF}
    end;

    if S1 <> 0 then
      {both numbers negative, reverse the result}
      Result := -Result;
  end;
end;

function ModBcd(const B1, B2 : TBcd) : TBcd;
  {-Return B1 mod B2}
begin
  Result := IntBcd(DivBcd(B1, B2));
end;

function DivBcd(const B1, B2 : TBcd) : TBcd;
{$IFNDEF UseAsm}
label
  StoreDigit;
{$ENDIF}
var
{$IFNDEF UseAsm}
  DivIntoCount, I, R : Integer;
  T, C : ShortInt;
  DDigit, NDigit : Byte;
{$ENDIF}
  E1, E2, DivDigits, N : Integer;
  S1, S2 : Byte;
  UB1, UB2 : TUnpBcd;
  TB : TIntBcd;
begin
  if B2[0] = 0 then
    {divide by zero}
    RaiseBcdError(stscBcdDivByZero);

  if B1[0] = 0 then
    {numerator is zero, return zero}
    SetZero(Result)

  else begin
    Unpack(B1, UB1, E1, S1);
    Unpack(B2, UB2, E2, S2);

    {TB is the extended numerator}
    FillChar(TB, 2*BcdSize, 0);
    Move(UB1[1], TB[2*BcdSize], SigDigits);

    {UB1 is now used to store the result}

    {count significant mantissa digits in divisor}
{$IFDEF UseAsm}
  asm
    push edi
    lea edi,UB2+1
    mov ecx,SigDigits
    xor al,al
    repe scasb
    mov DivDigits,ecx
    pop edi
  end;
{$ELSE}
    DivDigits := 0;
    for I := 1 to MantissaDigits do
      if UB2[I] <> 0 then begin
        DivDigits := SigDigits-I;
        break;
      end;
{$ENDIF}

    if DivDigits = 0 then
      {divide by zero, shouldn't have gotten here, but just in case...}
      RaiseBcdError(stscBcdDivByZero);

{$IFDEF UseAsm}
  asm
    push ebx
    push esi
    push edi
    mov ecx,SigDigits             {number of digits in result}
    lea edi,UB1+SigDigits         {edi points to MSD of result}
    lea esi,TB+2*MantissaDigits+1 {esi points to MSD of numerator}
    mov dh,byte ptr DivDigits     {keep DivDigits in dh}

@1: push ecx                      {save result counter}
    push edi                      {save result position}
    mov ebx,esi                   {save numerator position}
    xor dl,dl                     {dl = number of times divisor fits into numerator}

@2: cmp byte ptr [esi+1],0        {check for remainder in numerator}
    jnz @4                        {divisor guaranteed to fit again}
    xor ecx,ecx
    mov cl,dh                     {ecx = number of divisor digits}
    lea edi,UB2+MantissaDigits    {last digit of divisor}

@3: mov al,[esi]                  {al = numerator digit}
    dec esi
    mov ah,[edi]                  {ah = divisor digit}
    dec edi
    cmp al,ah
    ja @4                         {divisor fits if numerator digit > divisor}
    jb @7                         {doesn't fit if numerator digit < divisor}
    dec ecx
    jnz @3

@4: inc dl                        {increment number of times divisor fits}
    mov edi,ebx                   {restore numerator position to edi}
    xor ecx,ecx
    mov cl,dh                     {ecx = number of divisor digits}
    lea esi,UB2+MantissaDigits    {esi points to MSD of divisor}
    dec ecx
    sub esi,ecx                   {first significant digit of divisor}
    sub edi,ecx                   {first active digit of numerator}
    inc ecx
    clc                           {no carry to start}

@5: mov al,[edi]                  {al = digit from numerator}
    sbb al,[esi]                  {subtract divisor from numerator}
    aas
    mov [edi],al                  {store back to numerator}
    inc esi
    inc edi
    dec ecx
    jnz @5
    jnc @6
    dec byte ptr [edi]            {reduce last digit for borrow}

@6: mov esi,ebx                   {restore numerator position to esi}
    jmp @2                        {see if divisor fits in numerator again}

@7: mov esi,ebx                   {restore numerator position to esi}
    pop edi                       {restore result position}
    pop ecx                       {restore result counter}
    mov [edi],dl                  {store times divisor went into numerator}
    dec edi                       {next result digit}
    dec esi                       {next numerator digit}
    dec ecx
    jnz @1                        {compute next result digit}

    pop edi
    pop esi
    pop ebx
  end;
{$ELSE}
    {start with most significant digit of numerator}
    N := 2*MantissaDigits+1;

    {iterate until the result mantissa is filled}
    for R := SigDigits downto 1 do begin
      DivIntoCount := 0;

      repeat
        {subtract divisor from current numerator position as many times as possible}
        if TB[N+1] = 0 then begin
          {no overflow digit in this position of numerator}
          for I := 0 to DivDigits-1 do begin
            DDigit := UB2[MantissaDigits-I];
            NDigit := TB[N-I];
            if DDigit < NDigit then
              {divisor still fits}
              break
            else if DDigit > NDigit then
              {divisor doesn't fit}
              goto StoreDigit;
          end;
        end;
        inc(DivIntoCount);

        {subtract divisor once from numerator}
        C := 0;
        for I := DivDigits-1 downto 0 do begin
          T := TB[N-I]-UB2[MantissaDigits-I]-C;
          if T < 0 then begin
            C := 1;
            inc(T, 10);
          end else
            C := 0;
          TB[N-I] := T;
        end;
        {reduce last digit for borrow}
        dec(TB[N+1], C);
      until False;

StoreDigit:
      {store this digit of result}
      UB1[R] := DivIntoCount;
      {next numerator digit}
      dec(N);
    end;
{$ENDIF}

    if UB1[SigDigits] <> 0 then begin
      {round away the temporary digit}
      RoundMantissa(UB1, 1);
      ShiftMantissaDown(UB1, 1);
      inc(E1);
    end;

    {compute exponent}
    N := E1-E2+ExpBias;
    if N > NoSignBit then
      {numeric overflow}
      RaiseBcdError(stscBcdOverflow);
    Pack(UB1, N, S1 xor S2, Result);
  end;
end;

function FastVal(const S : string) : TBcd;
  {-Internal routine to quickly convert a string constant to a Bcd}
  {Assumes no leading spaces,
   no leading '+',
   no leading '.',
   always contains decimal point defined by international DecimalSeparator,
   no invalid characters,
   no exponent,
   < MantissaDigits before decimal point}
var
  I, O, Digits, Exponent : Integer;
  Sign : Byte;
  Rounded : Boolean;
  UB : TUnpBcd;

  procedure AddDigit(Ch : Char);
  begin
    if O > 0 then begin
      UB[O] := Byte(Ch)-Byte('0');
      dec(O);
    end else if not Rounded then begin
      {got more significant digits than will fit, must round}
      Rounded := True;
      UB[0] := Byte(Ch)-Byte('0');
      RoundMantissa(UB, 0);
      if UB[SigDigits] <> 0 then begin
        ShiftMantissaDown(UB, 1);
        inc(Digits);
      end;
    end;
  end;

begin
  FillChar(UB, SizeOf(TUnpBcd), 0);

  O := MantissaDigits;
  Rounded := False;
  Digits := 0;

  {get sign if any}
  if S[1] = '-' then begin
    Sign := SignBit;
    I := 2;
  end else begin
    Sign := 0;
    I := 1;
  end;

  {skip leading zeros}
  while S[I] = '0' do
    inc(I);

  {add significant digits}
  while S[I] <> '.' do begin
    AddDigit(S[I]);
    inc(I);
    inc(Digits);
  end;

  {handle dot}
  inc(I);
  if Digits = 0 then
    {no digits before dot, skip zeros after dot}
    while (I <= length(S)) and (S[I] = '0') do begin
      inc(I);
      dec(Digits);
    end;

  {add significant digits}
  while I <= Length(S) do begin
    AddDigit(S[I]);
    if Rounded then
      break;
    inc(I);
  end;

  {compute final exponent}
  Exponent := Digits+ExpBias;

  if (Exponent <= 0) or IsZeroMantissa(UB) then
    {return zero}
    Exponent := 0;

  {Return packed result}
  Pack(UB, Exponent, Sign, Result);
end;

function ExpBcd(const B : TBcd) : TBcd;
var
  MI, Exponent : LongInt;
  B1, B2, B3, B4, B5 : TBcd;
begin
  if CmpBcd(B, FastVal('147.36')) > 0 then
    {numeric overflow}
    RaiseBcdError(stscBcdOverflow);

  if CmpBcd(B, FastVal('-145.06')) < 0 then begin
    {return zero}
    SetZero(Result);
    Exit;
  end;

  if B[0] = 0 then begin
    {return one}
    Result := FastVal('1.0');
    Exit;
  end;

{If BcdSize > 10, Delphi 2.0 generates a hint (if hints on) about B3 during compile}
{this can be ignored or you can suppress warnings in STDEFINE.INC}
{or suppress hints and warning for the IF..THEN block}

  if BcdSize <= 10 then begin
    {Burns (Cody-Waite) approximation}
    Exponent := RoundBcd(MulBcd(B, FastVal('0.868588963806503655')));
    MI := Exponent; {prevent D32 from generating a hint}
    B5 := LongBcd(MI);

    B3 := AddBcd(B, MulBcd(B5, FastVal('-1.151')));
    B1 := AddBcd(B3, MulBcd(B5, FastVal('-0.000292546497022842009')));
    B2 := MulBcd(B1, B1);

    B3 := MulBcd(B2, FastVal('42.0414268137450315'));
    B3 := MulBcd(B2, AddBcd(B3, FastVal('10097.4148724273918')));
    B4 := MulBcd(B1, AddBcd(B3, FastVal('333267.029226801611')));

    B3 := MulBcd(B2, AddBcd(B2, FastVal('841.243584514154545')));
    B3 := MulBcd(B2, AddBcd(B3, FastVal('75739.3346159883444')));
    B3 := AddBcd(B3, FastVal('666534.058453603223'));
    B3 := DivBcd(B4, SubBcd(B3, B4));
    Result := MulBcd(AddBcd(B3, FastVal('0.5')), FastVal('2.0'));

    if Odd(MI) then begin
      if MI < 0 then
        Result := DivBcd(Result, FastVal('3.16227766016837933'))
      else
        Result := MulBcd(Result, FastVal('3.16227766016837933'));
    end;

    inc(ShortInt(Result[0]), MI div 2);

  end else begin
    {series approximation}
    {compute B2, a number whose exp is close to 1.0}
    {and MI, a number whose exp is a power of 10}
    B2 := DivBcd(B, Ln10Bcd);
    if B[0] and SignBit <> 0 then
      B2 := SubBcd(B2, FastVal('0.5'))
    else
      B2 := AddBcd(B2, FastVal('0.5'));
    MI := TruncBcd(B2);
    B2 := SubBcd(B, MulBcd(IntBcd(B2), Ln10Bcd));

    {compute exp(B2)}
    B1 := FastVal('1.0');
    B4 := B1;
    Result := B1;
    B5 := B2;
    while B5[0] and NoSignBit > ExpBias-MantissaDigits-1 do begin
      Result := AddBcd(Result, B5);
      B4 := AddBcd(B4, B1);
      B5 := DivBcd(MulBcd(B5, B2), B4);
    end;

    {correct exponent for 10**MI}
    Exponent := Result[0] and NoSignBit;
    inc(Exponent, MI);
    if Exponent > NoSignBit then
      {numeric overflow}
      RaiseBcdError(stscBcdOverflow);
    if Exponent <= 0 then
      {underflow}
      SetZero(Result);
    Result[0] := Exponent;
  end;
end;

function ExtBcd(E : Extended) : TBcd;
var
  S : string;
begin
  Str(e:0:MantissaDigits, S);
  Result := ValBcd(FastValPrep(S));
end;

function StrGeneralBcd(const B : TBcd) : string;
var
  I, EndI, Exponent : Integer;

  procedure RemoveTrailingZeros(StartI, EndI : Integer);
  var
    I : Integer;
  begin
    I := StartI;
    while (I > 0) and (Result[I] = '0') and (Result[I] <> {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator) do
      dec(I);
    if Result[I] = {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator then
      dec(I);
    Delete(Result, I+1, EndI-I);
  end;

begin
  Exponent := B[0] and NoSignBit;

  if (Exponent = 0) or
  ((Exponent <= MantissaDigits+ExpBias) and (Exponent >= ExpBias-4)) then begin
    {use fixed point format for zero, digits to left of decimal point greater
     than or equal to MantissaDigits, or value greater than 0.00001}
    Result := StrBcd(B, 0, MantissaDigits);
    RemoveTrailingZeros(Length(Result), Length(Result));

  end else begin
    {otherwise use scientific format}
    Result := StrExpBcd(B, 0);
    if Result[1] = ' ' then
      Delete(Result, 1, 1);
    I := Length(Result)-1;
    EndI := I-3;
    while (I <= Length(Result)) and (Result[I] = '0') do
      Delete(Result, I, 1);
    if I > Length(Result) then begin
      {exponent was all zero}
      Delete(Result, Length(Result)-1, 2);
      I := Length(Result);
    end else
      {skip back over "e+"}
      I := EndI;
    RemoveTrailingZeros(I, EndI);
  end;
end;

function FormatBcd(const Format: string; const B : TBcd): string;
label
  Restart;
var
  SectNum, SectOfs, I, ExpDigits, ActPlaces : Integer;
  DigitCount, DecimalIndex, FirstDigit, LastDigit : Integer;
  DigitPlace, DigitDelta, Exponent : Integer;
  BufOfs, UBOfs : Integer;
  ThousandSep, Scientific : Boolean;
  Ch : Char;
  Sign : Byte;
  UB : TUnpBcd;
  SExponent : string;//[4];
  Buffer : array[0..255] of Char;

  function FindSection(SectNum : Integer) : Integer;
    {-Return the offset into Format for the given section number}
  var
    Ch : Char;
  begin
    if SectNum > 0 then begin
      Result := 1;
      while Result <= Length(Format) do begin
        Ch := Format[Result];
        case Ch of
          {labels in ASCII order so 32-bit compiler generates better code}
          '"', '''' : {skip literal}
            begin
              inc(Result);
              while (Result <= Length(Format)) and (Format[Result] <> Ch) do
                inc(Result);
              if Result > Length(Format) then
                break;
            end;
          ';' : {end of section}
            begin
              dec(SectNum);
              if SectNum = 0 then begin
                inc(Result);
                if (Result > Length(Format)) or (Format[Result] = ';') then
                  {empty section}
                  break
                else
                  {found the section, return its offset}
                  exit;
              end;
            end;
        end;
        inc(Result);
      end;
    end;

    {arrive here if desired section is empty, not found, or ill-formed}
    if (Length(Format) = 0) or (Format[1] = ';') then
      {first section is empty, use general format}
      Result := 0
    else
      {use first section}
      Result := 1;
  end;

  procedure ScanSection(SectOfs : Integer);
    {-Initialize DigitCount, DecimalIndex, ThousandSep,
      Scientific, FirstDigit, LastDigit}
  var
    FirstZero, LastZero : Integer;
    Ch : Char;
  begin
    FirstZero := 32767;
    LastZero := 0;
    DigitCount := 0;
    DecimalIndex := -1;
    ThousandSep := False;
    Scientific := False;

    repeat
      Ch := Format[SectOfs];
      case Ch of
        {labels in ASCII order so 32-bit compiler generates better code}
        '"' :
          begin
            inc(SectOfs);
            while (SectOfs <= Length(Format)) and (Format[SectOfs] <> Ch) do
              inc(SectOfs);
            if SectOfs > Length(Format) then
              break;
          end;

        '#' :
          inc(DigitCount);

        '''' :
          begin
            inc(SectOfs);
            while (SectOfs <= Length(Format)) and (Format[SectOfs] <> Ch) do
              inc(SectOfs);
            if SectOfs > Length(Format) then
              break;
          end;

        '0' :
          begin
            if DigitCount < FirstZero then
              FirstZero := DigitCount;
            inc(DigitCount);
            LastZero := DigitCount;
          end;

        ';' :
          break;

        'E', 'e' :
          if SectOfs < Length(Format) then begin
            inc(SectOfs);
            case Format[SectOfs] of
              '-', '+' :
                begin
                  Scientific := True;
                  repeat
                    inc(SectOfs);
                  until (SectOfs > Length(Format)) or (Format[SectOfs] <> '0');
                end;
            else
              {back up and look at character after 'e' again}
              dec(SectOfs);
            end;
          end;
      else
        if Ch = {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}ThousandSeparator then
          ThousandSep := True;

        if Ch = {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator  then
          if DecimalIndex = -1 then
            DecimalIndex := DigitCount;
      end;

      inc(SectOfs);
      if SectOfs > Length(Format) then
        break;
    until False;

    if DecimalIndex = -1 then
      DecimalIndex := DigitCount;
    LastDigit := DecimalIndex-LastZero;
    if LastDigit > 0 then
      LastDigit := 0;
    FirstDigit := DecimalIndex-FirstZero;
    if FirstDigit < 0 then
      FirstDigit := 0;
  end;

  procedure StoreChar(Ch : Char);
  begin
    if BufOfs >= Length(Buffer) then
      {buffer overrun}
      RaiseBcdError(stscBcdBufOverflow);
    Buffer[BufOfs] := Ch;
    inc(BufOfs);
  end;

  procedure StoreDigitReally(ReadUB : Boolean);
  var
    BVal : Byte;
  begin
    if ReadUB then begin
      if UBOfs >= 0 then begin
        BVal := UB[UBOfs];
        dec(UBOfs);
      end else if DigitPlace <= LastDigit then begin
        dec(DigitPlace);
        Exit;
      end else
        BVal := 0;
    end else
      BVal := 0;

    if DigitPlace = 0 then begin
      StoreChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
      StoreChar(Char(BVal+Byte('0')));
    end else begin
      StoreChar(Char(BVal+Byte('0')));
      if ThousandSep then
        if DigitPlace > 1 then
          if DigitPlace mod 3 = 1 then
            StoreChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}ThousandSeparator);
    end;

    dec(DigitPlace);
  end;

  procedure StoreDigit;
  begin
    if DigitDelta = 0 then
      StoreDigitReally(True)
    else if DigitDelta < 0 then begin
      inc(DigitDelta);
      if DigitPlace <= FirstDigit then
        StoreDigitReally(False)
      else
        dec(DigitPlace);
    end else begin
      repeat
        StoreDigitReally(True);
        dec(DigitDelta);
      until DigitDelta = 0;
      StoreDigitReally(True);
    end;
  end;

begin
  Unpack(B, UB, Exponent, Sign);

Restart:
  if Exponent = 0 then
    {zero}
    SectNum := 2
  else if Sign <> 0 then
    {negative}
    SectNum := 1
  else
    {positive}
    SectNum := 0;
  SectOfs := FindSection(SectNum);

  if SectOfs = 0 then
    {general floating point format}
    Result := StrGeneralBcd(B)

  else begin
    {scan the section once to determine critical format properties}
    ScanSection(SectOfs);

    if Exponent <> 0 then begin
      {round based on number of displayed digits}
      ActPlaces := Integer(MantissaDigits)-Exponent+ExpBias;
      if DigitCount-DecimalIndex < ActPlaces then begin
        RoundMantissa(UB, ActPlaces-(DigitCount-DecimalIndex));
        if UB[SigDigits] <> 0 then begin
          ShiftMantissaDown(UB, 1);
          inc(Exponent);
        end else if IsZeroMantissa(UB) then begin
          {rounded to zero, possibly use a different mask}
          Exponent := 0;
          goto Restart;
        end;
      end;
    end;

    {apply formatting}
    if Scientific then begin
      DigitPlace := DecimalIndex;
      DigitDelta := 0;
      if Exponent = 0 then
        {for input = 0, display E+00}
        Exponent := ExpBias+1
    end else begin
      if Exponent = 0 then
        {special case for input = 0}
        Exponent := ExpBias
      else if Exponent-ExpBias > MantissaDigits then begin
        {all digits are integer part}
        Result := StrGeneralBcd(B);
        Exit;
      end;
      DigitPlace := Exponent-ExpBias;
      DigitDelta := DigitPlace-DecimalIndex;
      if DigitPlace < DecimalIndex then
        DigitPlace := DecimalIndex;
    end;

    BufOfs := 0;
    UBOfs := MantissaDigits;

    if Sign <> 0 then
      if SectOfs = 1 then
        StoreChar('-');

    repeat
      Ch := Format[SectOfs];
      case Ch of
        {labels in ASCII order so 32-bit compiler generates better code}
        '"' :
          begin
            inc(SectOfs);
            while (SectOfs <= Length(Format)) and (Format[SectOfs] <> Ch) do begin
              StoreChar(Format[SectOfs]);
              inc(SectOfs);
            end;
            if SectOfs > Length(Format) then
              break;
          end;
        '#' :
          StoreDigit;

        '''' :
          begin
            inc(SectOfs);
            while (SectOfs <= Length(Format)) and (Format[SectOfs] <> Ch) do begin
              StoreChar(Format[SectOfs]);
              inc(SectOfs);
            end;
            if SectOfs > Length(Format) then
              break;
          end;

        '0' :
          StoreDigit;

        ';' :
          break;

        'E', 'e' :
          if SectOfs < Length(Format) then begin
            inc(SectOfs);
            case Format[SectOfs] of
              '-', '+' :
                begin
                  StoreChar(Ch);
                  Ch := Format[SectOfs];
                  ExpDigits := -1;
                  repeat
                    inc(ExpDigits);
                    inc(SectOfs);
                  until (SectOfs > Length(Format)) or (Format[SectOfs] <> '0');
                  if ExpDigits > 4 then
                    ExpDigits := 4;
                  dec(Exponent, ExpBias+DecimalIndex);
                  if (Exponent >= 0) and (Ch = '+') then
                    StoreChar('+');
                  if Exponent < 0 then begin
                    StoreChar('-');
                    Exponent := Abs(Exponent);
                  end;
                  Str(Exponent:ExpDigits, SExponent);
                  for I := 1 to ExpDigits do
                    if SExponent[I] = ' ' then
                      StoreChar('0')
                    else
                      StoreChar(SExponent[I]);
                end;
            else
              StoreChar(Ch);
              StoreChar(Format[SectOfs]);
            end;
          end else
            StoreChar(Ch);
      else
        {these characters are automatically inserted in StoreDigit};
        if not CharInSet(Ch, [FormatSettings.ThousandSeparator, FormatSettings.DecimalSeparator]) then
          StoreChar(Ch);
      end;

      inc(SectOfs);
      if SectOfs > Length(Format) then
        break;
    until False;

    SetLength(Result, BufOfs);
    move(Buffer[0], Result[1], BufOfs * SizeOf(Char));
  end;
end;

function FracBcd(const B : TBcd) : TBcd;
begin
  Result := SubBcd(B, IntBcd(B));
end;

function IsIntBcd(const B : TBcd) : Boolean;
var
{$IFNDEF UseAsm}
  I : Integer;
{$ENDIF}
  Exponent : Integer;
  Sign : Byte;
  UB : TUnpBcd;
begin
  Unpack(B, UB, Exponent, Sign);

  if Exponent = 0 then
    {0.0 has no fractional part}
    Result := True

  else if Exponent <= ExpBias then
    {value is less than one, but non-zero}
    Result := False

  else if Exponent-ExpBias >= MantissaDigits then
    {entire mantissa is non-fractional}
    Result := True

  else begin
    {see if any non-zero digits to left of decimal point}
{$IFDEF UseAsm}
      asm
        push edi
        lea edi,UB+1
        mov ecx,MantissaDigits+ExpBias
        sub ecx,Exponent
        xor al,al
        cld
        repe scasb
        jne @1
        inc al
@1:     mov Result,al
        pop edi
      end;
{$ELSE}
    for I := 1 to MantissaDigits-(Exponent-ExpBias) do
      if UB[I] <> 0 then begin
        Result := False;
        Exit;
      end;
    Result := True;
{$ENDIF}
  end;
end;

function IntBcd(const B : TBcd) : TBcd;
var
  Exponent : Integer;
  Sign : Byte;
  UB : TUnpBcd;
begin
  Unpack(B, UB, Exponent, Sign);

  if Exponent <= ExpBias then
    {value is less than one}
    SetZero(Result)

  else if Exponent-ExpBias >= MantissaDigits then
    {entire mantissa is integer part}
    Result := B

  else begin
    {clear fractional digits}
    FillChar(UB[1], MantissaDigits-(Exponent-ExpBias), 0);
    Pack(UB, Exponent, Sign, Result);
  end;
end;

function IntPowBcd(const B : TBcd; E : LongInt) : TBcd;
var
  I : LongInt;
  B1 : TBcd;
begin
  B1 := FastVal('1.0');
  Result := B1;
  for I := 1 to Abs(E) do
    Result := MulBcd(Result, B);
  if E < 0 then
    Result := DivBcd(B1, Result);
end;

function LnBcd20(const B : TBcd) : TBcd;
const
  Iterations = 9;
var
  Exponent, N, K : integer;
  BN, B025, B05, B1, AN, GN, Pow : TBcd;
  DN1, DN : array[0..Iterations] of TBcd;
begin
  {normalize input in range 0.10-0.99...}
  Exponent := B[0]-ExpBias;
  BN := B;
  BN[0] := ExpBias;

  {initialize some constants}
  B025 := FastVal('0.25');
  B05 := FastVal('0.5');
  B1 := FastVal('1.0');

  {compute initial terms of approximation}
  AN := MulBcd(B05, AddBcd(BN, B1));
  GN := SqrtBcd(BN);
  DN1[0] := AN;

  {converge on exact value}
  for N := 1 to Iterations do begin
    AN := MulBcd(B05, AddBcd(AN, GN));
    DN[0] := AN;
    Pow := B025;
    for K := 1 to N do begin
      DN[K] := DivBcd(SubBcd(DN[K-1], MulBcd(Pow, DN1[K-1])), SubBcd(B1, Pow));
      if K = N then
        break;
      Pow := MulBcd(Pow, B025);
    end;

    if N = Iterations then
      break;
    GN := SqrtBcd(MulBcd(AN, GN));
    DN1 := DN;
  end;
  Result := DivBcd(SubBcd(BN, B1), DN[Iterations]);

  {correct for normalization}
  Result := AddBcd(Result, MulBcd(LongBcd(Exponent), Ln10Bcd));
end;

function LnBcd10(const B : TBcd) : TBcd;
var
  Exponent : Integer;
  BN, B1, S, W, T, AW, BW : TBcd;
begin
  {normalize input in range 0.10-0.99...}
  Exponent := B[0]-ExpBias;
  BN := B;
  BN[0] := ExpBias;

  if CmpBcd(BN, FastVal('0.316227766016837933')) < 0 then begin
    {renormalize in range .316-3.16}
    dec(Exponent);
    inc(BN[0]);
  end;

  B1 := FastVal('1.0');
  S := DivBcd(SubBcd(BN, B1), AddBcd(BN, B1));
  W := MulBcd(S, S);

  T := MulBcd(W, FastVal('-0.741010784161919239'));
  T := MulBcd(W, AddBcd(T, FastVal('10.3338571514793865')));
  T := MulBcd(W, AddBcd(T, FastVal('-39.273741020315625')));
  T := MulBcd(W, AddBcd(T, FastVal('55.4085912041205931')));
  AW := AddBcd(T, FastVal('-26.0447002405557636'));

  T := MulBcd(W, AddBcd(W, FastVal('-19.3732345832854786')));
  T := MulBcd(W, AddBcd(T, FastVal('107.109789115668009')));
  T := MulBcd(W, AddBcd(T, FastVal('-244.303035341829542')));
  T := MulBcd(W, AddBcd(T, FastVal('245.347618868489348')));
  BW := AddBcd(T, FastVal('-89.9552077881033117'));

  T := MulBcd(W, DivBcd(AW, BW));
  T := MulBcd(S, AddBcd(T, FastVal('0.868588963806503655')));

  Result := MulBcd(AddBcd(T, LongBcd(Exponent)), Ln10Bcd);
end;

function LnBcd(const B : TBcd) : TBcd;
begin
  if (B[0] = 0) or (B[0] and SignBit <> 0) then
    {ln of zero or a negative number}
    RaiseBcdError(stscBcdBadInput);

  if BcdSize <= 10 then
    Result := LnBcd10(B)
  else
    Result := LnBcd20(B);
end;

function LongBcd(L : LongInt) : TBcd;
var
  S : string;
begin
  Str(L, S);
  Result := ValBcd(FastValPrep(S));
end;

function MulBcd(const B1, B2 : TBcd) : TBcd;
var
  E1, E2, Digits : Integer;
  S1, S2 : Byte;
{$IFNDEF UseAsm}
  I1, I2 : Integer;
  CP, CN : Byte;
  T, T1, T2 : Byte;
{$ENDIF}
  PB : PUnpBcd;
  UB1, UB2 : TUnpBcd;
  TB : TIntBcd;
begin
  if (B1[0] = 0) or (B2[0] = 0) then
    SetZero(Result)

  else begin
    Unpack(B1, UB1, E1, S1);
    Unpack(B2, UB2, E2, S2);

    FillChar(TB, SizeOf(TIntBcd), 0);

    {multiply and sum the mantissas}
{$IFDEF UseAsm}
    asm
      push ebx
      push esi
      push edi
      lea ebx,UB1  {multiplier}
      lea edi,TB   {result}
      mov ecx,MantissaDigits

@1:   inc ebx      {next multiplier digit}
      inc edi      {next output digit}
      mov al,[ebx] {get next multiplier digit}
      or al,al     {if zero, nothing to do}
      jz @3

      push ecx     {save digit counter}
      mov dl,al    {save multiplier}
      lea esi,UB2+1 {multiplicand}
      mov ecx,MantissaDigits
      xor dh,dh

@2:   mov al,[esi] {next multiplicand digit}
      inc esi
      mul dl       {multiply by multiplier, overflow in ah}
      aam
      add al,[edi] {add previous result}
      aaa
      add al,dh    {add previous overflow}
      aaa
      mov [edi],al {store temporary result}
      inc edi
      mov dh,ah    {save overflow for next time}
      dec ecx
      jnz @2
      mov [edi],dh {save last overflow in next digit}
      sub edi,MantissaDigits {reset output offset for next multiplier}
      pop ecx

@3:   dec ecx      {next multiplier digit}
      jnz @1
      pop edi
      pop esi
      pop ebx
    end;
{$ELSE}
    for I1 := 1 to MantissaDigits do begin
      T1 := UB1[I1];
      if T1 <> 0 then begin
        CP := 0;
        for I2 := 1 to MantissaDigits do begin
          T := T1*UB2[I2];
          T2 := T mod 10;
          CN := T div 10;
          inc(T2, TB[I1+I2-1]);
          if T2 > 9 then begin
            dec(T2, 10);
            inc(CN);
          end;
          inc(T2, CP);
          if T2 > 9 then begin
            dec(T2, 10);
            inc(CN);
          end;
          TB[I1+I2-1] := T2;
          CP := CN;
        end;
        {store last carry in next digit of buffer}
        TB[I1+MantissaDigits] := CP;
      end;
    end;
{$ENDIF}

    {normalize the product}
    if TB[2*MantissaDigits] <> 0 then begin
      PB := PUnpBcd(@TB[MantissaDigits]);
      Digits := 0;
    end else begin
      PB := PUnpBcd(@TB[MantissaDigits-1]);
      Digits := -1;
    end;
    RoundMantissa(PB^, 0);
    if PB^[SigDigits] <> 0 then begin
      inc(PByte(PB));
      inc(Digits);
    end;
    {copy back to UB2}
    UB2 := PB^;

    {set sign and exponent}
    inc(E2, E1+Digits-ExpBias);
    if E2 > NoSignBit then
      {numeric overflow}
      RaiseBcdError(stscBcdOverflow);

    Pack(UB2, E2, S1 xor S2, Result);
  end;
end;

function NegBcd(const B : TBcd) : TBcd;
begin
  Result := B;
  if B[0] <> 0 then
    Result[0] := B[0] xor SignBit;
end;

function PowBcd(const B, E : TBcd) : TBcd;
begin
  if E[0] = 0 then
    {anything raised to the zero power is 1.0}
    Result := FastVal('1.0')

  else if IsIntBcd(E) then
    {compute the power by simple multiplication}
    Result := IntPowBcd(B, TruncBcd(E))

  else begin
    if B[0] and SignBit <> 0 then
      {negative number raised to a non-integer power}
      RaiseBcdError(stscBcdBadInput);

    Result := ExpBcd(MulBcd(E, LnBcd(B)));
  end;
end;

function RoundBcd(const B : TBcd) : LongInt;
var
  Exponent, I : Integer;
  Sign : Byte;
  UB : TUnpBcd;
begin
  Unpack(B, UB, Exponent, Sign);

  Result := 0;
  if Exponent <> 0 then begin
    {Bcd is not zero}
    I := MantissaDigits;
    {add digits to left of decimal point}
    while (I >= 1) and (Exponent > ExpBias) do begin
      if Abs(Result) > MaxLongInt div 10 then
        {numeric overflow}
        RaiseBcdError(stscBcdOverflow);
      Result := 10*Result;
      if Sign <> 0 then begin
        if Result < -MaxLongInt-1+UB[I] then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
        dec(Result, UB[I]);
      end else begin
        if Result > MaxLongInt-UB[I] then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
        inc(Result, UB[I]);
      end;
      dec(I);
      dec(Exponent);
    end;

    {round last digit}
    if (I >= 1) and (Exponent = ExpBias) and (UB[I] >= 5) then begin
      if Sign <> 0 then begin
        if Result = -MaxLongInt-1 then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
        dec(Result);
      end else begin
        if Result = MaxLongInt then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
        inc(Result);
      end;
    end;

  end;
end;

function RoundDigitsBcd(const B : TBcd; Digits : Cardinal) : TBcd;
var
  Exponent : Integer;
  Sign : Byte;
  UB : TUnpBcd;
begin
  if B[0] = 0 then
    {input is zero}
    SetZero(Result)

  else if Digits >= MantissaDigits then
    {no actual rounding}
    Result := B

  else begin
    Unpack(B, UB, Exponent, Sign);

    {treat 0 digits same as 1}
    if Digits = 0 then
      Digits := 1;

    RoundMantissa(UB, MantissaDigits-Digits);
    if UB[SigDigits] <> 0 then begin
      ShiftMantissaDown(UB, 1);
      inc(Exponent);
    end else if IsZeroMantissa(UB) then
      Exponent := 0;

    Pack(UB, Exponent, Sign, Result);
  end;
end;

function RoundPlacesBcd(const B : TBcd; Places : Cardinal) : TBcd;
var
  Exponent, ActPlaces : Integer;
  Sign : Byte;
  UB : TUnpBcd;
begin
  if B[0] = 0 then
    {input is zero}
    SetZero(Result)

  else begin
    ActPlaces := Integer(MantissaDigits)-(B[0] and NoSignBit)+ExpBias;

    if LongInt(Places) >= ActPlaces then
      {no actual rounding}
      Result := B

    else begin
      Unpack(B, UB, Exponent, Sign);

      RoundMantissa(UB, ActPlaces-LongInt(Places));
      if UB[SigDigits] <> 0 then begin
        ShiftMantissaDown(UB, 1);
        inc(Exponent);
      end else if IsZeroMantissa(UB) then
        Exponent := 0;

      Pack(UB, Exponent, Sign, Result);
    end;
  end;
end;

function SqrtBcd(const B : TBcd) : TBcd;
var
  Exponent, I, Iterations : Integer;
  BN, B05 : TBcd;
begin
  if B[0] and SignBit <> 0 then
    {square root of a negative number}
    RaiseBcdError(stscBcdBadInput);

  if B[0] = 0 then begin
    {done for input of zero}
    SetZero(Result);
    Exit;
  end;

  {normalize input}
  Exponent := B[0]-ExpBias;
  BN := B;
  BN[0] := ExpBias;

  {create reused constant bcd}
  B05 := FastVal('0.5');

  {compute initial approximation of sqrt}
  Result := AddBcd(MulBcd(FastVal('0.894470'), BN),
                          FastVal('0.223607'));

  if BcdSize <= 10 then
    Iterations := 3
  else
    Iterations := 5;

  {iterate to accurate normalized sqrt, Result = 0.5*((BN/Result)+Result)}
  for I := 1 to Iterations do
    Result := MulBcd(AddBcd(DivBcd(BN, Result), Result), B05);

  {final correction Result = (0.5*(BN/Result-Result))+Result}
  Result := AddBcd(MulBcd(SubBcd(DivBcd(BN, Result), Result), B05), Result);

  if Odd(Exponent) then begin
    Result := MulBcd(Result,
      FastVal('0.31622776601683793319988935444327185337')); {Sqrt(0.1)}
    inc(Exponent);
  end;

  inc(Result[0], Exponent shr 1);
end;

function StrBcd(const B : TBcd; Width, Places : Cardinal) : string;
var
  I, O, Exponent, ActWidth, Digits, DecimalPos : Integer;
  Sign : Byte;
  UB : TUnpBcd;

  procedure AddChar(Ch : Char);
  begin
    Result[O] := Ch;
    inc(O);
  end;

begin
  Unpack(B, UB, Exponent, Sign);

  if Exponent = 0 then begin
    {ensure mantissa is set to zero}
    FillChar(UB[1], SigDigits, 0);
    {fool the rest of the function}
    Exponent := ExpBias+1;
  end;

  {ActWidth is the non-padded width}
  {it has at least one digit before decimal point}
  ActWidth := 1;
  if Exponent > ExpBias+1 then
    {add other digits before decimal point}
    inc(ActWidth, Exponent-ExpBias-1);

  {add digits after decimal point}
  inc(ActWidth, Places);

  {see how many digits from mantissa to use}
  if Exponent < ExpBias+1 then begin
    Digits := LongInt(Places)-(ExpBias-Exponent);
    if Digits < 0 then
      Digits := 0;
  end else
    Digits := ActWidth;

  if Places <> 0 then
    {add one for decimal point}
    inc(ActWidth);

  if Sign <> 0 then
    {add one for minus sign}
    inc(ActWidth);

  if Digits < MantissaDigits then begin
    {need to round}
    RoundMantissa(UB, MantissaDigits-Digits);
    if UB[SigDigits] <> 0 then begin
      ShiftMantissaDown(UB, 1);
      inc(Exponent);
      inc(Digits);
      if Exponent > ExpBias+1 then
        inc(ActWidth);
    end;
  end else
    {use all mantissa digits}
    Digits := MantissaDigits;

  {adjust and limit Width}
  if Width = 0 then
    Width := ActWidth;
{$IFDEF WStrings}
  if Width > 255 then
    Width := 255;
{$ENDIF}
  SetLength(Result, Width);

  if LongInt(Width) < ActWidth then begin
    {result won't fit in specified width}
    Result := StringOfChar(OverflowChar, Length(Result)); //FillChar(Result[1], Length(Result) * SizeOf(Char), OverflowChar);
    Exit;
  end;

  if LongInt(Width) > ActWidth then begin
    {store leading spaces}
     StrPCopy(PChar(Result), StringOfChar(' ', LongInt(Width)-ActWidth));    //FillChar(Result[1], LongInt(Width)-ActWidth, ' ');
    O := LongInt(Width)-ActWidth+1;
  end else
    O := 1;

  if Sign <> 0 then
    AddChar('-');

  if Exponent < ExpBias+1 then begin
    {number is less than 1}
    AddChar('0');
    if Exponent <> 0 then begin
      AddChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
      for I := 1 to ExpBias-Exponent do
        if O <= LongInt(Width) then
          AddChar('0');
    end;
  end;

  if Places = 0 then
    {no decimal point}
    DecimalPos := 0
  else
    DecimalPos := Width-Places;

  {add digits from the mantissa}
  if Digits <> 0 then begin
    I := SigDigits;
    if UB[I] = 0 then
      dec(I);
    while (Digits > 0) and (O <= LongInt(Width)) do begin
      if O = DecimalPos then
        AddChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
      AddChar(Char(UB[I]+Byte('0')));
      dec(I);
      dec(Digits);
    end;
  end;

  {add trailing zeros, if any}
  while O <= LongInt(Width) do begin
    if O = DecimalPos then
      AddChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
    if O <= LongInt(Width) then
      AddChar('0');
  end;
end;

function StrExpBcd(const B : TBcd; Width : Cardinal) : string;
const
  MinWidth = 8;
  MaxWidth = MantissaDigits+6;
var
  I, O, Exponent : Integer;
  Sign : Byte;
  UB : TUnpBcd;

  procedure AddChar(Ch : Char);
  begin
    Result[O] := Ch;
    inc(O);
  end;

begin
  Unpack(B, UB, Exponent, Sign);

  {validate and adjust Width}
  if Width = 0 then
    Width := MaxWidth
  else if Width < MinWidth then
    Width := MinWidth;
{$IFDEF WStrings}
  if Width > 255 then
    Width := 255;
{$ENDIF}
  SetLength(Result, Width);

  {store leading spaces}
  if Width > MaxWidth then begin
    StrPCopy(PChar(Result), StringOfChar(' ', Width-MaxWidth));  //FillChar(Result[1], Width-MaxWidth, ' ');
    O := Width-MaxWidth+1;
  end else
    O := 1;

  {store sign}
  if Sign <> 0 then
    AddChar('-')
  else
    AddChar(' ');

  if Exponent = 0 then begin
    {ensure mantissa is set to zero}
    FillChar(UB[1], SigDigits, 0);
    {force Exponent to display as 0}
    Exponent := ExpBias+1;

  end else if Width < MaxWidth then begin
    {need to round}
    RoundMantissa(UB, MaxWidth-Width);
    if UB[SigDigits] <> 0 then begin
      ShiftMantissaDown(UB, 1);
      inc(Exponent);
    end;
  end;

  {copy mantissa to string}
  I := MantissaDigits;
  AddChar(Char(UB[I]+Byte('0')));
  dec(I);
  AddChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
  while O < LongInt(Width-3) do begin
    AddChar(Char(UB[I]+Byte('0')));
    dec(I);
  end;

  {store exponent}
  AddChar('E');
  if Exponent < ExpBias+1 then begin
    AddChar('-');
    Exponent := ExpBias+1-Exponent;
  end else begin
    AddChar('+');
    dec(Exponent, ExpBias+1);
  end;
  AddChar(Char((Exponent div 10)+Byte('0')));
  AddChar(Char((Exponent mod 10)+Byte('0')));
end;

function SubBcd(const B1, B2 : TBcd) : TBcd;
begin
  Result := AddBcd(B1, NegBcd(B2));
end;

function TruncBcd(const B : TBcd) : LongInt;
var
  Exponent, I : Integer;
  Sign : Byte;
  UB : TUnpBcd;
begin
  Unpack(B, UB, Exponent, Sign);

  Result := 0;
  if Exponent <> 0 then begin
    {Bcd is not zero}
    I := MantissaDigits;
    {Add digits to left of decimal point}
    while (I >= 1) and (Exponent > ExpBias) do begin
      if Abs(Result) > MaxLongInt div 10 then
        {numeric overflow}
        RaiseBcdError(stscBcdOverflow);
      Result := 10*Result;
      if Sign <> 0 then begin
        if Result < -MaxLongInt-1+UB[I] then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
        dec(Result, UB[I]);
      end else begin
        if Result > MaxLongInt-UB[I] then
          {numeric overflow}
          RaiseBcdError(stscBcdOverflow);
        inc(Result, UB[I]);
      end;

      dec(I);
      dec(Exponent);
    end;
  end;
end;

function ValBcd(const S : string) : TBcd;
var
  I, O, Digits, Exponent : Integer;
  Sign : Byte;
  ExpSigned, Rounded : Boolean;
  UB : TUnpBcd;

  function SChar(I : Integer) : Char;
  begin
    if I > Length(S) then
      Result := #0
    else
      Result := S[I];
  end;

  function IsDigit(Ch : Char) : Boolean;
  begin
    Result := (Ch >= '0') and (Ch <= '9');
  end;

  procedure AddDigit(Ch : Char);
  begin
    if O > 0 then begin
      UB[O] := Byte(Ch)-Byte('0');
      dec(O);
    end else if not Rounded then begin
      {got more significant digits than will fit, must round}
      Rounded := True;
      UB[0] := Byte(Ch)-Byte('0');
      RoundMantissa(UB, 0);
      if UB[SigDigits] <> 0 then begin
        ShiftMantissaDown(UB, 1);
        inc(Digits);
      end;
    end;
  end;

begin
  FillChar(UB, SizeOf(TUnpBcd), 0);

  I := 1;  {input position}
  O := MantissaDigits; {output position}
  Exponent := 0;
  Sign := 0;
  Rounded := False;

  {digits before dot, or negative digits after dot in case of 0.0000n}
  Digits := 0;

  {skip leading spaces}
  while SChar(I) = ' ' do
    inc(I);

  {get sign if any}
  case SChar(I) of
    '+' :
      {skip +}
      inc(I);
    '-' :
      begin
        {negative number}
        Sign := SignBit;
        inc(I);
      end;
  end;

  {handle first digit}
  if SChar(I) <> {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator then begin
    if not IsDigit(SChar(I)) then
      RaiseBcdError(stscBcdBadFormat);

    {skip leading zeros}
    while SChar(I) = '0' do
      inc(I);

    {add significant digits}
    while IsDigit(SChar(I)) do begin
      AddDigit(SChar(I));
      inc(I);
      inc(Digits);
    end;
  end;

  {handle dot}
  if SChar(I) = {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator then begin
    inc(I);
    if Digits = 0 then begin
      {no digits before dot, skip zeros after dot}
      while SChar(I) = '0' do begin
        inc(I);
        dec(Digits);
      end;
    end;

    {add significant digits}
    while IsDigit(SChar(I)) do begin
      AddDigit(SChar(I));
      inc(I);
    end;
  end;

  {handle exponent}
  case SChar(I) of
    'e', 'E' :
      begin
        inc(I);
        ExpSigned := False;
        case SChar(I) of
          '+' :
            {skip +}
            inc(I);
          '-' :
            begin
              {negative exponent}
              ExpSigned := True;
              inc(I);
            end;
        end;
        if not IsDigit(SChar(I)) then
          {digit must follow 'e', invalid format}
          RaiseBcdError(stscBcdBadFormat);

        {collect exponent value}
        while IsDigit(SChar(I)) do begin
          Exponent := 10*Exponent+Byte(SChar(I))-Byte('0');
          inc(I);
        end;

        if ExpSigned then
          Exponent := -Exponent;
      end;
  end;

  if SChar(I) <> #0 then
    {should be end of string, otherwise invalid format}
    RaiseBcdError(stscBcdBadFormat);

  {compute final exponent}
  Inc(Exponent, Digits+ExpBias);

  if Exponent > NoSignBit then
    {numeric overflow}
    RaiseBcdError(stscBcdOverflow);

  if (Exponent <= 0) or IsZeroMantissa(UB) then
    {return zero}
    Exponent := 0;

  {Return packed result}
  Pack(UB, Exponent, Sign, Result);
end;

function FloatFormBcd(const Mask : string; B : TBCD;
                      const LtCurr, RtCurr : string;
                      Sep, DecPt : Char) : string;
  {-Returns a formatted string with digits from B merged into the Mask}
const
  Blank = 0;
  Asterisk = 1;
  Zero = 2;
const
  FormChars : string = '#@*$-+,.';
  PlusArray : array[Boolean] of Char = ('+', '-');
  MinusArray : array[Boolean] of Char = (' ', '-');
  FillArray : array[Blank..Zero] of Char = (' ', '*', '0');
var
  ExpB : Byte absolute B;  {B's sign/exponent byte}
  S : string;              {temporary string}
  Filler : integer;        {char for unused digit slots: ' ', '*', '0'}
  WontFit,                 {true if number won't fit in the mask}
  AddMinus,                {true if minus sign needs to be added}
  Dollar,                  {true if floating dollar sign is desired}
  Negative : Boolean;      {true if B is negative}
  StartF,                  {starting point of the numeric field}
  EndF : Word;             {end of numeric field}
  RtChars,                 {# of chars to add to right}
  LtChars,                 {# of chars to add to left}
  DotPos,                  {position of '.' in Mask}
  Digits,                  {total # of digits}
  Places,                  {# of digits after the '.'}
  Blanks,                  {# of blanks returned by StrBcd}
  FirstDigit,              {pos. of first digit returned by Str}
  Extras,                  {# of extra digits needed for special cases}
  DigitPtr : Byte;         {pointer into temporary string of digits}
  I : Word;
label
  EndFound,
  RedoCase,
  Done;
begin
  Result := Mask;

  RtChars := 0;
  LtChars := 0;

  {check for empty string}
  if Length(Mask) = 0 then
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
  Negative := (ExpB and $80) <> 0;
  ExpB := ExpB and $7F;

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
  while (StartF <= Length(Result)) and
    not CharExistsL(FormChars, Result[StartF]) do
    Inc(StartF);
  if StartF > Length(Mask) then
    goto Done;

  {find the end point for the field}
  EndF := StartF;
  for I := StartF to Length(Result) do
  begin
    case Result[I] of
      '*' : Filler := Asterisk;
      '@' : Filler := Zero;
      '$' : Dollar := True;
      '-',
      '+' : AddMinus := False;
      '#' : {ignore} ;
      ',',
      '.' : DotPos := I;
    else
      goto EndFound;
    end;
    Inc(EndF);
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
  if EndF > Length(Result) then                                        {!!.02}
    EndF := Length(Result);                                            {!!.02}

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

  {translate the BCD to a string}
  S := StrBCD(B, Digits, Places);


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
    StrPCopy(PChar(S), StringOfChar(FillArray[Filler], Blanks)); // FillChar(S[1], Blanks, FillArray[Filler]);

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
          if (DigitPtr <> 0) and (S[DigitPtr] = '.') then              {!!.02}
//          if (S[DigitPtr] = '.') and (DigitPtr <> 0) then
            Dec(DigitPtr);
        end
        else
          Result[I] := FillArray[Filler];
      ',' : begin
              Result[I] := Sep;
              if (I < DotPos) and (DigitPtr < FirstDigit) then begin
                Result[I] := '#';
                goto RedoCase;
              end;
            end;
      '.' : begin
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

{routines to support C++Builder}
{$IFDEF CBuilder}
procedure AddBcd_C(const B1, B2 : TBcd; var Res : TBcd);
begin
  Res := AddBcd(B1, B2);
end;

procedure SubBcd_C(const B1, B2 : TBcd; var Res : TBcd);
begin
  Res := SubBcd(B1, B2);
end;

procedure MulBcd_C(const B1, B2 : TBcd; var Res : TBcd);
begin
  Res := MulBcd(B1, B2);
end;

procedure DivBcd_C(const B1, B2 : TBcd; var Res : TBcd);
begin
  Res := DivBcd(B1, B2);
end;

procedure ModBcd_C(const B1, B2 : TBcd; var Res : TBcd);
begin
  Res := ModBcd(B1, B2);
end;

procedure NegBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := NegBcd(B);
end;

procedure AbsBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := AbsBcd(B);
end;

procedure FracBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := FracBcd(B);
end;

procedure IntBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := IntBcd(B);
end;

procedure RoundDigitsBcd_C(const B : TBcd; Digits : Cardinal; var Res : TBcd);
begin
  Res := RoundDigitsBcd(B, Digits);
end;

procedure RoundPlacesBcd_C(const B : TBcd; Places : Cardinal; var Res : TBcd);
begin
  Res := RoundPlacesBcd(B, Places);
end;

procedure ValBcd_C(const S : string; var Res : TBcd);
begin
  Res := ValBcd(S);
end;

procedure LongBcd_C(L : LongInt; var Res : TBcd);
begin
  Res := LongBcd(L);
end;

procedure ExtBcd_C(E : Extended; var Res : TBcd);
begin
  Res := ExtBcd(E);
end;

procedure ExpBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := ExpBcd(B);
end;

procedure LnBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := LnBcd(B);
end;

procedure IntPowBcd_C(const B : TBcd; E : LongInt; var Res : TBcd);
begin
  Res := IntPowBcd(B, E);
end;

procedure PowBcd_C(const B, E : TBcd; var Res : TBcd);
begin
  Res := PowBcd(B, E);
end;

procedure SqrtBcd_C(const B : TBcd; var Res : TBcd);
begin
  Res := SqrtBcd(B);
end;
{$ENDIF}

initialization
  ZeroBcd := FastVal('0.0');
  MinBcd  := ValBcd('-9'+{$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator+'9E+63');
  BadBcd  := MinBcd;
  MaxBcd  := ValBcd('9'+{$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator+'9E+63');
  PiBcd   := FastVal('3.1415926535897932384626433832795028841971');
  Ln10Bcd := FastVal('2.3025850929940456840179914546843642076011');
  eBcd    := FastVal('2.7182818284590452353602874713526624977572');
end.
