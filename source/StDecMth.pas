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
{* SysTools: StDecMth.pas 4.04                           *}
{*********************************************************}
{* SysTools: Class for doing decimal arithmetic          *}
{*********************************************************}

{$I StDefine.inc}

unit StDecMth;

interface

{Note: StDecMth declares and implements TStDecimal. This is a fixed-
       point value with a total of 38 significant digits of which
       16 are to the right of the decimal point.}

uses
  SysUtils;

type
  TStRoundMethod = ( {different rounding methods...}
    rmNormal,        {..normal (round away from zero if half way)}
    rmTrunc,         {..truncate (always round to zero)}
    rmBankers,       {..bankers (round to even digit if half way)}
    rmUp);           {..force round up (always round from zero)}

  TStInt128 = array [0..3] of longint; // must be longint, not DWORD

  TStDecimal = class
    private
      FInt : TStInt128;
    protected
      function dcGetAsStr : AnsiString;
      procedure dcSetFromStr(const aValue : AnsiString);               {!!.02}
    public
      constructor Create;
      destructor Destroy; override;

      function Compare(X : TStDecimal) : integer;
        {-returns <0 if Self < X, 0 is equal, >0 otherwise}
      function IsNegative : boolean;
        {-returns Self < 0.0}
      function IsOne : boolean;
        {-returns Self = 1.0}
      function IsPositive : boolean;
        {-returns Self > 0.0}
      function IsZero : boolean;
        {-returns Self = 0.0}
      procedure SetToOne;
        {-sets Self equal to 1.0}
      procedure SetToZero;
        {-sets Self equal to 0.0}

      procedure Assign(X : TStDecimal);
        {-sets Self equal to X}
      procedure AssignFromFloat(aValue : double);
        {-sets Self equal to aValue}
      procedure AssignFromInt(aValue : integer);
        {-sets Self equal to aValue}

      function AsFloat : double;
        {-returns Self as an floating point value}
      function AsInt(aRound : TStRoundMethod) : integer;
        {-returns Self as an integer, rounded}

      procedure Abs;
        {-calculates Self := Abs(Self)}
      procedure Add(X : TStDecimal);
        {-calculates Self := Self + X}
      procedure AddOne;
        {-calculates Self := Self + 1.0}
      procedure ChangeSign;
        {-calculates Self := ChgSign(Self)}
      procedure Divide(X : TStDecimal);
        {-calculates Self := Self div X}
      procedure Multiply(X : TStDecimal);
        {-calculates Self := Self * X}
      procedure RaiseToPower(N : integer);
        {-calculates Self := Self ^ N}
      procedure Round(aRound : TStRoundMethod; aDecPl : integer);
        {-calculates Self := Round(Self)}
      procedure Subtract(X : TStDecimal);
        {-calculates Self := Self - X}
      procedure SubtractOne;
        {-calculates Self := Self - 1}

      property AsString : AnsiString read dcGetAsStr write dcSetFromStr;
        {-returns Self as a string, sets Self from a string}
  end;

implementation

uses
  StConst,
  StBase;

type
  TStInt256 = array [0..7] of integer;
  TStInt192 = array [0..5] of integer;

const
  MaxDecPl = 16;

  Int128One_0 = longint($6FC10000);
  Int128One_1 = longint($002386F2);

  PowerOf10 : array [0..MaxDecPl div 2] of integer =
              (1, 10, 100, 1000, 10000, 100000, 1000000, 10000000,
               100000000);

{===Helper routines==================================================}
procedure Int256Div10E8(var X : TStInt256; var aRem : integer);
{Note: this routine assumes X is positive}
asm
  push ebx             // save ebx

  push edx             // save address of remainder variable

  mov ecx, 100000000   // we're dividing by 10^8
  mov ebx, eax         // ebx points to X

  xor edx, edx         // start off with high dividend digit zero
  mov eax, [ebx+28]    // get last 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+28], eax    // save highest quotient digit

  mov eax, [ebx+24]    // get next 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+24], eax    // save next quotient digit

  mov eax, [ebx+20]    // get next 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+20], eax    // save next quotient digit

  mov eax, [ebx+16]    // get next 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+16], eax    // save next quotient digit

  mov eax, [ebx+12]    // get next 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+12], eax    // save next quotient digit

  mov eax, [ebx+8]     // get next 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+8], eax     // save next quotient digit

  mov eax, [ebx+4]     // get next 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx+4], eax     // save next quotient digit

  mov eax, [ebx]       // get first 32-bit digit
  div ecx              // divide by 10: eax is quotient, edx remainder
  mov [ebx], eax       // save first quotient digit

  pop eax              // pop off the address of remainder variable
  mov [eax], edx       // store remainder

  pop ebx              // restore ebx
end;
{--------}
procedure Int192Times10E8(var X : TStInt192);
{Note: this routine assumes X is positive}
asm
  push ebx           // save ebx
  push ebp           // save ebp

  mov ecx, 100000000 // we're multiplying by 10^8
  mov ebx, eax       // ebx points to X

  mov eax, [ebx]     // get the first 32-bit digit
  mul ecx            // multiply it by 10^8 to give answer in edx:eax
  mov [ebx], eax     // save first digit of result
  mov ebp, edx       // save overflow

  mov eax, [ebx+4]   // get the second 32-bit digit
  mul ecx            // multiply it by 10^8 to give answer in edx:eax
  add eax, ebp       // add the overflow from the first digit
  adc edx, 0
  mov [ebx+4], eax   // save second digit of result
  mov ebp, edx       // save overflow

  mov eax, [ebx+8]   // get the third 32-bit digit
  mul ecx            // multiply it by 10^8 to give answer in edx:eax
  add eax, ebp       // add the overflow from the second digit
  adc edx, 0
  mov [ebx+8], eax   // save third digit of result
  mov ebp, edx       // save overflow

  mov eax, [ebx+12]  // get the fourth 32-bit digit
  mul ecx            // multiply it by 10^8 to give answer in edx:eax
  add eax, ebp       // add the overflow from the third digit
  adc edx, 0
  mov [ebx+12], eax  // save fourth digit of result
  mov ebp, edx       // save overflow

  mov eax, [ebx+16]  // get the fifth 32-bit digit
  mul ecx            // multiply it by 10^8 to give answer in edx:eax
  add eax, ebp       // add the overflow from the fourth digit
  adc edx, 0
  mov [ebx+16], eax  // save fifth digit of result
  mov ebp, edx       // save overflow

  mov eax, [ebx+20]  // get the sixth 32-bit digit
  mul ecx            // multiply it by 10^8 to give answer in edx:eax
  add eax, ebp       // add the overflow from the fifth digit
  mov [ebx+20], eax  // save sixth digit of result

  pop ebp            // restore ebp
  pop ebx            // restore ebx
end;
{--------}
function Int32MultPrim(X, Y : longint;
                   var P : longint; Carry : longint) : longint;
asm
  {Note: calculates X * Y + P + Carry
         returns answer in P, with overflow as result value}
  mul edx
  add eax, [ecx]
  adc edx, 0
  add eax, Carry
  adc edx, 0
  mov [ecx], eax
  mov eax, edx
end;
{--------}
procedure Int128Add(var X : TStInt128; const Y : TStInt128);
asm
  push ebx
  mov ecx, [edx]
  mov ebx, [edx+4]
  add [eax], ecx
  adc [eax+4], ebx
  mov ecx, [edx+8]
  mov ebx, [edx+12]
  adc [eax+8], ecx
  adc [eax+12], ebx
  pop ebx
end;
{--------}
procedure Int128AddInt(var X : TStInt128; aDigit : integer);
asm
  add [eax], edx
  adc dword ptr [eax+4], 0
  adc dword ptr [eax+8], 0
  adc dword ptr [eax+12], 0
end;
{--------}
procedure Int128ChgSign(var X : TStInt128);
asm
  mov ecx, [eax]
  mov edx, [eax+4]
  not ecx
  not edx
  add ecx, 1
  adc edx, 0
  mov [eax], ecx
  mov [eax+4], edx
  mov ecx, [eax+8]
  mov edx, [eax+12]
  not ecx
  not edx
  adc ecx, 0
  adc edx, 0
  mov [eax+8], ecx
  mov [eax+12], edx
end;
{--------}
function Int128Compare(const X, Y : TStInt128) : integer;
asm
  // Can be called from pascal
  // All registers are preserved, except eax, which returns the
  //   result of the comparison
  push ebx
  push ecx
  mov ecx, [eax+12]
  mov ebx, [edx+12]
  xor ecx, $80000000
  xor ebx, $80000000
  cmp ecx, ebx
  jb @@LessThan
  ja @@GreaterThan
  mov ecx, [eax+8]
  mov ebx, [edx+8]
  cmp ecx, ebx
  jb @@LessThan
  ja @@GreaterThan
  mov ecx, [eax+4]
  mov ebx, [edx+4]
  cmp ecx, ebx
  jb @@LessThan
  ja @@GreaterThan
  mov ecx, [eax]
  mov ebx, [edx]
  cmp ecx, ebx
  jb @@LessThan
  ja @@GreaterThan
  xor eax, eax
  jmp @@Exit
@@LessThan:
  mov eax, -1
  jmp @@Exit
@@GreaterThan:
  mov eax, 1
@@Exit:
  pop ecx
  pop ebx
end;
{--------}
procedure Int192SHL(var X : TStInt192);
asm
  // DO NOT CALL FROM PASCAL
  // IN:  eax -> 192-bit integer to shift left
  // OUT: eax -> 192-bit integer shifted left
  //      CF  =  most significant bit shifted out
  // All registers are preserved
  push ebx
  push ecx
  mov ebx, [eax]
  mov ecx, [eax+4]
  shl ebx, 1
  rcl ecx, 1
  mov [eax], ebx
  mov [eax+4], ecx
  mov ebx, [eax+8]
  mov ecx, [eax+12]
  rcl ebx, 1
  rcl ecx, 1
  mov [eax+8], ebx
  mov [eax+12], ecx
  mov ebx, [eax+16]
  mov ecx, [eax+20]
  rcl ebx, 1
  rcl ecx, 1
  mov [eax+16], ebx
  mov [eax+20], ecx
  pop ecx
  pop ebx
end;
{--------}
procedure Int128RCL(var X : TStInt128);
asm
  // DO NOT CALL FROM PASCAL
  // IN:  eax -> 128-bit integer to shift left
  //      CF  =  least significant bit to shift in
  // OUT: eax -> 128-bit integer shifted left
  //      CF  -> topmost bit shifted out
  // All registers are preserved
  push ebx
  push ecx
  mov ebx, [eax]
  mov ecx, [eax+4]
  rcl ebx, 1
  rcl ecx, 1
  mov [eax], ebx
  mov [eax+4], ecx
  mov ebx, [eax+8]
  mov ecx, [eax+12]
  rcl ebx, 1
  rcl ecx, 1
  mov [eax+8], ebx
  mov [eax+12], ecx
  pop ecx
  pop ebx
end;
{--------}
procedure Int128FastDivide(var X       : TStInt192;
                           var Y, aRem : TStInt128);
asm
  push ebp
  push ebx
  push edi
  push esi

  mov esi, eax         // esi -> dividend
  mov edi, edx         // edi -> divisor
  mov ebp, ecx         // ebp -> remainder

  mov ecx, 192         // we'll do the loop for all 192 bits in the
                       //   dividend

  xor eax, eax         // zero the remainder
  mov [ebp], eax
  mov [ebp+4], eax
  mov [ebp+8], eax
  mov [ebp+12], eax

@@GetNextBit:
  mov eax, esi         // shift the dividend left, and...
  call Int192SHL
  mov eax, ebp         // ...shift the topmost bit into the remainder
  call Int128RCL

  mov eax, ebp         // compare the remainder with the divisor
  mov edx, edi
  call Int128Compare

  cmp eax, 0           // if the remainder is smaller, we can't
  jl @@TooSmall        //   subtract the divisor

                       // essentially we've shown that the divisor
                       //   divides the remainder exactly once, so

  add dword ptr [esi], 1   // add one to the quotient

  mov eax, [ebp]       // subtract the divisor from the remainder
  mov ebx, [ebp+4]
  sub eax, [edi]
  sbb ebx, [edi+4]
  mov [ebp], eax
  mov [ebp+4], ebx
  mov eax, [ebp+8]
  mov ebx, [ebp+12]
  sbb eax, [edi+8]
  sbb ebx, [edi+12]
  mov [ebp+8], eax
  mov [ebp+12], ebx

@@TooSmall:
  dec ecx              // go get the next bit to work on
  jnz @@GetNextBit

  pop esi
  pop edi
  pop ebx
  pop ebp
end;
{--------}
function Int128DivInt(var X : TStInt128; aDivisor : integer) : integer;
{Note: this routine assumes X is positive}
asm
  push ebx            // save ebx

  mov ecx, edx        // ecx is now the divisor
  mov ebx, eax        // ebx points to X

  xor edx, edx        // start off with high dividend digit zero
  mov eax, [ebx+12]   // get last 32-bit digit
  div ecx             // divide by ecx: eax is quotient, edx remainder
  mov [ebx+12], eax   // save highest quotient digit

  mov eax, [ebx+8]    // get next 32-bit digit
  div ecx             // divide by ecx: eax is quotient, edx remainder
  mov [ebx+8], eax    // save next quotient digit

  mov eax, [ebx+4]    // get next 32-bit digit
  div ecx             // divide by ecx: eax is quotient, edx remainder
  mov [ebx+4], eax    // save next quotient digit

  mov eax, [ebx]      // get first 32-bit digit
  div ecx             // divide by ecx: eax is quotient, edx remainder
  mov [ebx], eax      // save first quotient digit

  mov eax, edx        // return remainder

  pop ebx             // restore ebx
end;
{--------}
procedure Int128Divide(var X, Y : TStInt128);
var
  XTemp : TStInt192;
  Rem   : TStInt128;
begin
  {note: the easy cases have been dealt with
         X and Y are both positive
         X will be set to the quotient X/Y and Y will be trashed}

  {we need to increase the number of decimal places to 32, so convert
   the 128 bit dividend to a 192 bit one and multiply by 10^16}
  XTemp[0] := X[0];
  XTemp[1] := X[1];
  XTemp[2] := X[2];
  XTemp[3] := X[3];
  XTemp[4] := 0;
  XTemp[5] := 0;
  Int192Times10E8(XTemp);
  Int192Times10E8(XTemp);

  {Note: this algorithm follows that described by Knuth in volume 2 of
         The Art of Computer Programming. Algorithm D of section 4.3
         as applied to binary numbers (radix=2)}

  {divide the 192-bit dividend by the 128-bit divisor}
  Int128FastDivide(XTemp, Y, Rem);

  {have we overflowed? ie, have we divided a very big number by one
   much less than zero}
  if (XTemp[3] < 0) or (XTemp[4] <> 0) or (XTemp[5] <> 0) then
    raise EStDecMathError.Create(stscDecMathDivOverflowS);

  {return the result of the computation}
  X[0] := XTemp[0];
  X[1] := XTemp[1];
  X[2] := XTemp[2];
  X[3] := XTemp[3];
end;
{--------}
procedure Int128Multiply(var X, Y : TStInt128);
var
  P : TStInt256;
  XIsNeg : boolean;
  YIsNeg : boolean;
  YInx   : integer;
  YDigit : integer;
  Carry  : integer;
  YTemp  : TStInt128;
begin
  {Note: calculates X * Y and puts the answer in X}

  {get rid of the easy cases where one of the operands is zero}
  if (X[0] = 0) and (X[1] = 0) and (X[2] = 0) and (X[3] = 0) then
    Exit;
  if (Y[0] = 0) and (Y[1] = 0) and (Y[2] = 0) and (Y[3] = 0) then begin
    X[0] := 0;
    X[1] := 0;
    X[2] := 0;
    X[3] := 0;
    Exit;
  end;

  {we might need to trash Y, so we use a local variable}
  YTemp[0] := Y[0];
  YTemp[1] := Y[1];
  YTemp[2] := Y[2];
  YTemp[3] := Y[3];

  {convert both operands to positive values: we'll fix the sign later}
  XIsNeg := X[3] < 0;
  if XIsNeg then
    Int128ChgSign(X);
  YIsNeg := YTemp[3] < 0;
  if YIsNeg then
    Int128ChgSign(YTemp);

  {initialize the temporary product}
  P[0] := 0;
  P[1] := 0;
  P[2] := 0;
  P[3] := 0;
  P[4] := 0;
  P[5] := 0;
  P[6] := 0;
  P[7] := 0;

  {for every digit in Y we shall multiply by all the X digits and sum}
  for YInx := 0 to 3 do begin

    {get the Y digit}
    YDigit := YTemp[YInx];

    {there's only something to do if the Y digit is non-zero}
    if (YDigit <> 0) then begin

      {multiply this digit with all the X digits, storing the result
       in the temporary product}
      Carry := Int32MultPrim(X[0], YDigit, P[YInx], 0);
      Carry := Int32MultPrim(X[1], YDigit, P[YInx + 1], Carry);
      Carry := Int32MultPrim(X[2], YDigit, P[YInx + 2], Carry);
      P[YInx + 4] := Int32MultPrim(X[3], YDigit, P[YInx + 3], Carry);
    end;
  end;

  {the product has 32 decimal places, so divide by 10^8 twice to get
   the answer to the 16 decimal places we need}
  Int256Div10E8(P, Carry);
  Int256Div10E8(P, Carry);

  {note: if Carry <> 0 then we're losing precision}

  {check for multiplication overflow}
  if (P[3] < 0) or
     (P[4] <> 0) or (P[5] <> 0) or (P[6] <> 0) or (P[7] <> 0) then
    raise EStDecMathError.Create(stscDecMathMultOverflowS);

  {return the value in X, remembering to set the sign}
  X[0] := P[0];
  X[1] := P[1];
  X[2] := P[2];
  X[3] := P[3];

  (*
  {round if necessary}
  if (Carry >= 500000000) then
    Int128AddInt(X, 1);
  *)

  {set the sign}
  if (XIsNeg xor YIsNeg) then
    Int128ChgSign(X);
end;
{--------}
procedure Int128TimesInt(var X : TStInt128; aValue : integer);
{Note: this routine assumes X is positive}
asm
  push ebx             // save ebx
  push ebp             // save ebp

  mov ecx, edx         // we're multiplying by aValue
  mov ebx, eax         // ebx points to X

  mov eax, [ebx]       // get the first 32-bit digit
  mul ecx              // multiply it by 10 to give answer in edx:eax
  mov [ebx], eax       // save first digit of result
  mov ebp, edx         // save overflow

  mov eax, [ebx+4]     // get the second 32-bit digit
  mul ecx              // multiply it by 10 to give answer in edx:eax
  add eax, ebp         // add the overflow from the first digit
  adc edx, 0
  mov [ebx+4], eax     // save second digit of result
  mov ebp, edx         // save overflow

  mov eax, [ebx+8]     // get the third 32-bit digit
  mul ecx              // multiply it by 10 to give answer in edx:eax
  add eax, ebp         // add the overflow from the second digit
  adc edx, 0
  mov [ebx+8], eax     // save second digit of result
  mov ebp, edx         // save overflow

  mov eax, [ebx+12]     // get the third 32-bit digit
  mul ecx              // multiply it by 10 to give answer in edx:eax
  add eax, ebp         // add the overflow from the second digit
  mov [ebx+12], eax     // save third digit of result

  pop ebp              // restore ebp
  pop ebx              // restore ebx
end;
{--------}
procedure Int128Round(var X : TStInt128;
                          aRound : TStRoundMethod;
                          aDecPl : integer);
var
  Rem     : integer;
  HadRem  : boolean;
  AddOne  : boolean;
  Expnt   : integer;
  NeedInt : boolean;
begin
  {Assumptions: X is positive, 0 <= aDecPl <= MaxDecPl
                --the caller *must* ensure these}

  {if the number of decimal places is -1, it's a special signal to
   perform the rounding to an integer, but not to multiply the result
   by 10^16 at the end; the caller is AsInt, in other words}
  if (aDecPl >= 0) then
    NeedInt := false
  else begin
    NeedInt := true;
    aDecPl := 0;
  end;

  {if we're asked to round to the precision of the type, there's
   nothing to do}
  if (aDecPl = MaxDecPl) then
    Exit;

  {perform the required rounding}
  AddOne := false;   // keep the compiler happy
  case aRound of
    rmNormal :
      begin
        {to do normal rounding: divide by the required power of ten,
         if the most significant digit of the remainder was 5 or more,
         we'll add one to the result}
        Expnt := MaxDecPl - aDecPl - 1;
        if (Expnt > 0) then begin
          if (Expnt > 8) then begin
            Int128DivInt(X, PowerOf10[8]);
            dec(Expnt, 8);
          end;
          Int128DivInt(X, PowerOf10[Expnt]);
        end;
        AddOne := Int128DivInt(X, 10) >= 5;
      end;
    rmTrunc :
      begin
        {to truncate: just divide by the required power of ten}
        Expnt := MaxDecPl - aDecPl;
        if (Expnt > 8) then begin
          Int128DivInt(X, PowerOf10[8]);
          dec(Expnt, 8);
        end;
        Int128DivInt(X, PowerOf10[Expnt]);
        AddOne := false;
      end;
    rmBankers :
      begin
        {to do bankers rounding:
          - divide by the required power of ten, checking to see if
            there's a non-zero remainder
          - if the most significant digit of the remainder was greater
            than 5, we'll add one to the result
          - if the most significant digit of the remainder was 5 and
            there was at least one other digit in the remainder, we'll
            add one to the result
          - if the most significant digit of the remainder was 5 and
            there were no other digits in the remainder, determine if
            the result is odd; if it is, we'll add one to the result}
        HadRem := false;
        if ((MaxDecPl - aDecPl) > 1) then begin
          Expnt := MaxDecPl - aDecPl - 1;
          if (Expnt > 8) then begin
            if (Int128DivInt(X, PowerOf10[8]) <> 0) then
              HadRem := true;
            dec(Expnt, 8);
          end;
          if (Int128DivInt(X, PowerOf10[Expnt]) <> 0) then
            HadRem := true;
        end;
        Rem := Int128DivInt(X, 10);
        AddOne := (Rem > 5) or
                  ((Rem = 5) and HadRem) or
                  ((Rem = 5) and Odd(X[0]));
      end;
    rmUp :
      begin
        {to always round up: divide by the required power of ten,
         if there was a remainder, we'll add one to the result}
        AddOne := false;
        Expnt := MaxDecPl - aDecPl;
        if (Expnt > 8) then begin
          if (Int128DivInt(X, PowerOf10[8]) <> 0) then
            AddOne := true;
          dec(Expnt, 8);
        end;
        if (Int128DivInt(X, PowerOf10[Expnt]) <> 0) then
          AddOne := true;
      end;
  end;{case}

  {add one to the result, if required}
  if AddOne then
    Int128AddInt(X, 1);

  {finally, multiply by the required power of ten}
  if not NeedInt then begin
    Expnt := MaxDecPl - aDecPl;
    if (Expnt > 8) then begin
      Int128TimesInt(X, PowerOf10[8]);
      dec(Expnt, 8);
    end;
    Int128TimesInt(X, PowerOf10[Expnt]);
  end;
end;
{====================================================================}


{====================================================================}
constructor TStDecimal.Create;
begin
  {create the ancestor}
  inherited Create;
  {note: the internal number will be automatically zero}
end;
{--------}
destructor TStDecimal.Destroy;
begin
  {free the ancestor}
  inherited Destroy;
end;
{--------}
procedure TStDecimal.Abs;
begin
  if (FInt[3] < 0) then
    Int128ChgSign(FInt);
end;
{--------}
procedure TStDecimal.Add(X : TStDecimal);
begin
  if (X <> nil) then
    Int128Add(FInt, X.FInt);
end;
{--------}
procedure TStDecimal.AddOne;
var
  One : TStInt128;
begin
  One[0] := Int128One_0;
  One[1] := Int128One_1;
  One[2] := 0;
  One[3] := 0;
  Int128Add(FInt, One);
end;
{--------}
function TStDecimal.AsFloat : double;
begin
  Result := StrToFloat(string(AsString));
end;
{--------}
function TStDecimal.AsInt(aRound : TStRoundMethod) : integer;
var
  X : TStInt128;
  IsNeg : boolean;
begin
  {get the current value locally}
  X[0] := FInt[0];
  X[1] := FInt[1];
  X[2] := FInt[2];
  X[3] := FInt[3];

  {force it to be positive}
  IsNeg := X[3] < 0;
  if IsNeg then
    Int128ChgSign(X);

  {round it to an integer}
  Int128Round(X, aRound, -1);

  {check for errors (the least significant digit cannot be negative,
   and all the others must be zero)}
  if (X[0] < 0) or (X[1] <> 0) or (X[2] <> 0) or (X[3] <> 0) then
    raise EStDecMathError.Create(stscDecMathAsIntOverflowS);

  {return the result}
  if IsNeg then
    Result := -X[0]
  else
    Result := X[0];
end;
{--------}
procedure TStDecimal.Assign(X : TStDecimal);
begin
  if (X = nil) then
    SetToZero
  else begin
    FInt[0] := X.FInt[0];
    FInt[1] := X.FInt[1];
    FInt[2] := X.FInt[2];
    FInt[3] := X.FInt[3];
  end;
end;
{--------}
procedure TStDecimal.AssignFromFloat(aValue : double);
begin
  AsString := AnsiString(Format('%38.16f', [aValue]));
end;
{--------}
procedure TStDecimal.AssignFromInt(aValue : integer);
begin
  FInt[0] := System.Abs(aValue);
  FInt[1] := 0;
  FInt[2] := 0;
  FInt[3] := 0;
  Int128TimesInt(FInt, PowerOf10[8]);
  Int128TimesInt(FInt, PowerOf10[8]);
  if (aValue < 0) then
    Int128ChgSign(FInt);
end;
{--------}
procedure TStDecimal.ChangeSign;
begin
  Int128ChgSign(FInt);
end;
{--------}
function TStDecimal.Compare(X : TStDecimal) : integer;
begin
  Compare := Int128Compare(FInt, X.FInt);
end;
{--------}
function TStDecimal.dcGetAsStr : AnsiString;
var
  X      : TStInt128;
  i      : integer;
  Rem    : integer;
  IsNeg  : boolean;
  ChStack: array [0..47] of AnsiChar;
                          // this is ample for 38 digits + punctuation
  ChSP   : integer;
begin
  {initialize the stack}
  ChSP := 0;

  {since we're going to trash the value, store it locally}
  X[0] := FInt[0];
  X[1] := FInt[1];
  X[2] := FInt[2];
  X[3] := FInt[3];

  {make sure it's positive}
  IsNeg := X[3] < 0;
  if IsNeg then
    Int128ChgSign(X);

  {push the least significant digits (those that will appear after the
   radix point)}
  for i := 1 to MaxDecPl do begin
    Rem := Int128DivInt(X, 10);
    ChStack[ChSP] := AnsiChar(Rem + ord('0'));
    inc(ChSP);
  end;

  {push the radix point}
  ChStack[ChSP] := AnsiChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
  inc(ChSP);

  {repeat until the local value is zero}
  repeat
    Rem := Int128DivInt(X, 10);
    ChStack[ChSP] := AnsiChar(Rem + ord('0'));
    inc(ChSP);
  until (X[0] = 0) and (X[1] = 0) and (X[2] = 0) and (X[3] = 0);

  {if the value was negative, push a minus sign}
  if IsNeg then begin
    ChStack[ChSP] := '-';
    inc(ChSP);
  end;

  {construct the result value by popping off characters}
  SetLength(Result, ChSP);
  i := 1;
  while (ChSP <> 0) do begin
    dec(ChSP);
    Result[i] := ChStack[ChSP];
    inc(i);
  end;
end;
{--------}
procedure TStDecimal.dcSetFromStr(const aValue : AnsiString);          {!!.02}
var
  State : (ScanStart, ScanSign, ScanRadix, ScanBefore,
           ScanAfter, ScanEnd, GotError);
  i     : integer;
  Ch    : AnsiChar;
  IsNeg : boolean;
  DecPlCount : integer;
begin
  {Note: this implements the following DFA:

     ScanStart  --space--> ScanStart
     ScanStart  --plus---> ScanSign
     ScanStart  --minus--> ScanSign
     ScanStart  --digit--> ScanBefore
     ScanStart  --radix--> ScanRadix

     ScanSign   --radix--> ScanRadix
     ScanSign   --digit--> ScanBefore

     ScanRadix  --digit--> ScanAfter

     ScanBefore --radix--> ScanAfter
     ScanBefore --digit--> ScanBefore
     ScanBefore --space--> ScanEnd

     ScanAfter  --digit--> ScanAfter
     ScanAfter  --space--> ScanEnd

     ScanEnd    --space--> ScanEnd

   The terminating states are ScanBefore, ScanAfter and ScanEnd; in
   other words, a valid numeric string cannot end in a radix point.
  }

  {initialize}
  SetToZero;
  DecPlCount := 0;
  IsNeg := false;
  State := ScanStart;

  {read through the input string}
  for i := 1 to length(aValue) do begin

    {get the current character}
    Ch := aValue[i];

    case State of
      ScanStart :
        begin
          if ('0' <= Ch) and (Ch <= '9') then begin
            FInt[0] := ord(Ch) - ord('0');
            State := ScanBefore;
          end
          else if (Ch = '+') then begin
            State := ScanSign;
          end
          else if (Ch = '-') then begin
            IsNeg := true;
            State := ScanSign;
          end
          else if (Ch = AnsiChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator)) then begin
            State := ScanRadix;
          end
          else if (Ch <> ' ') then
            State := GotError;
        end;
      ScanSign :
        begin
          if ('0' <= Ch) and (Ch <= '9') then begin
            FInt[0] := ord(Ch) - ord('0');
            State := ScanBefore;
          end
          else if (Ch = AnsiChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator)) then begin
            State := ScanRadix;
          end
          else
            State := GotError;
        end;
      ScanRadix :
        begin
          if ('0' <= Ch) and (Ch <= '9') then begin
            inc(DecPlCount);
            Int128TimesInt(FInt, 10);
            Int128AddInt(FInt, ord(Ch) - ord('0'));
            State := ScanAfter;
          end
          else
            State := GotError;
        end;
      ScanBefore :
        begin
          if ('0' <= Ch) and (Ch <= '9') then begin
            Int128TimesInt(FInt, 10);
            Int128AddInt(FInt, ord(Ch) - ord('0'));
          end
          else if (Ch = AnsiChar({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator)) then begin
            State := ScanAfter;
          end
          else if (Ch = ' ') then
            State := ScanEnd
          else
            State := GotError;
        end;
      ScanAfter :
        begin
          if ('0' <= Ch) and (Ch <= '9') then begin
            inc(DecPlCount);
            if (DecPlCount <= MaxDecPl) then begin
              Int128TimesInt(FInt, 10);
              Int128AddInt(FInt, ord(Ch) - ord('0'));
            end;
          end
          else if (Ch = ' ') then
            State := ScanEnd
          else
            State := GotError;
        end;
      ScanEnd :
        begin
          if (Ch <> ' ') then
            State := GotError;
        end;
      GotError :
        begin
          Break;
        end;
    end;
  end;

  if (State <> ScanBefore) and
     (State <> ScanAfter) and
     (State <> ScanEnd) then
    raise EStDecMathError.Create(stscDecMathConversionS);

  {make sure we have the correct number of decimal places}
  if (MaxDecPl > DecPlCount) then begin
    DecPlCount := MaxDecPl - DecPlCount;
    if (DecPlCount > 8) then begin
      Int128TimesInt(FInt, Powerof10[8]);
      dec(DecPlCount, 8);
    end;
    Int128TimesInt(FInt, Powerof10[DecPlCount]);
  end;

  {force negative, if required}
  if IsNeg then
    Int128ChgSign(FInt);
end;
{--------}
procedure TStDecimal.Divide(X : TStDecimal);
var
  TempX  : TStInt128;
  IsNeg  : boolean;
  XIsNeg : boolean;
begin
  {easy case: X is nil or zero}
  if (X = nil) or X.IsZero then
    raise EStDecMathError.Create(stscDecMathDivByZeroS);

  {easy case: Self is zero}
  if IsZero then
    Exit;

  {we might have to change X, so make it local}
  TempX[0] := X.FInt[0];
  TempX[1] := X.FInt[1];
  TempX[2] := X.FInt[2];
  TempX[3] := X.FInt[3];

  {force the divisor and dividend positive}
  IsNeg := FInt[3] < 0;
  if IsNeg then
    Int128ChgSign(FInt);
  XIsNeg := TempX[3] < 0;
  if XIsNeg then
    Int128ChgSign(TempX);

  {easy case: X is 1.0: set the correct sign}
  if (TempX[0] = Int128One_0) and (TempX[1] = Int128One_1) and
     (TempX[2] = 0) and (TempX[3] = 0) then begin
    if (IsNeg xor XIsNeg) then
      Int128ChgSign(FInt);
    Exit;
  end;

  {easy case: compare the dividend and divisor: if they're equal,
   set ourselves to 1.0 with the correct sign}
  if (Int128Compare(FInt, TempX) = 0) then begin
    FInt[0] := Int128One_0;
    FInt[1] := Int128One_1;
    FInt[2] := 0;
    FInt[3] := 0;
    if (IsNeg xor XIsNeg) then
      Int128ChgSign(FInt);
    Exit;
  end;

  {no more easy cases: just do the division}
  Int128Divide(FInt, TempX);

  {set the sign}
  if (IsNeg xor XIsNeg) then
    Int128ChgSign(FInt);
end;
{--------}
function TStDecimal.IsNegative : boolean;
begin
  {if the most significant longint is negative, so is the value}
  Result := FInt[3] < 0;
end;
{--------}
function TStDecimal.IsOne : boolean;
begin
  Result := (FInt[0] = Int128One_0) and (FInt[1] = Int128One_1) and
            (FInt[2] = 0) and (FInt[3] = 0);
end;
{--------}
function TStDecimal.IsPositive : boolean;
begin
  {if the most significant longint is positive, so is the value; if it
   is zero, one of the other longints must be non-zero for the value
   to be positive}
  Result := (FInt[3] > 0) or
            ((FInt[3] = 0) and
             ((FInt[2] <> 0) or (FInt[1] <> 0) or (FInt[0] <> 0)));
end;
{--------}
function TStDecimal.IsZero : boolean;
begin
  Result := (FInt[0] = 0) and (FInt[1] = 0) and
            (FInt[2] = 0) and (FInt[3] = 0);
end;
{--------}
procedure TStDecimal.Multiply(X : TStDecimal);
begin
  if (X = nil) then
    SetToZero
  else
    Int128Multiply(FInt, X.FInt);
end;
{--------}
procedure TStDecimal.RaiseToPower(N : integer);
var
  Accum : TStInt128;
  Mask  : longint;
  IsNeg : boolean;
begin
  {take care of some easy cases}
  if (N < 0) then
    raise EStDecMathError.Create(stscDecMathNegExpS);
  if (N = 0) then begin
    SetToOne;
    Exit;
  end;
  if (N = 1) then
    Exit;

  {force the value positive}
  IsNeg := FInt[3] < 0;
  if IsNeg then
    Int128ChgSign(FInt);

  {initialize the accumulator to 1.0}
  Accum[0] := Int128One_0;
  Accum[1] := Int128One_1;
  Accum[2] := 0;
  Accum[3] := 0;

  {set the bit mask}
  Mask := longint($80000000);

  {find the first set bit in the exponent}
  while ((N and Mask) = 0) do
    Mask := Mask shr 1;

  {calculate the power}
  while (Mask <> 0) do begin
    Int128Multiply(Accum, Accum);
    if ((N and Mask) <> 0) then
      Int128Multiply(Accum, FInt);
    Mask := Mask shr 1;
  end;

  {save the calculated value}
  FInt[0] := Accum[0];
  FInt[1] := Accum[1];
  FInt[2] := Accum[2];
  FInt[3] := Accum[3];

  {force the value negative if required}
  if IsNeg and Odd(N) then
    Int128ChgSign(FInt);
end;
{--------}
procedure TStDecimal.Round(aRound : TStRoundMethod; aDecPl : integer);
var
  IsNeg : boolean;
begin
  {check decimal places parameter to be in range}
  if not ((0 <= aDecPl) and (aDecPl <= MaxDecPl)) then
    raise EStDecMathError.Create(stscDecMathRoundPlacesS);

  {force the value positive}
  IsNeg := FInt[3] < 0;
  if IsNeg then
    Int128ChgSign(FInt);

  {perform the rounding}
  Int128Round(FInt, aRound, aDecPl);

  {force the value negative if it was negative}
  if IsNeg then
    Int128ChgSign(FInt);
end;
{--------}
procedure TStDecimal.SetToOne;
begin
  FInt[0] := Int128One_0;
  FInt[1] := Int128One_1;
  FInt[2] := 0;
  FInt[3] := 0;
end;
{--------}
procedure TStDecimal.SetToZero;
begin
  FInt[0] := 0;
  FInt[1] := 0;
  FInt[2] := 0;
  FInt[3] := 0;
end;
{--------}
procedure TStDecimal.Subtract(X : TStDecimal);
var
  MinusX : TStInt128;
begin
  if (X <> nil) then begin
    MinusX[0] := X.FInt[0];
    MinusX[1] := X.FInt[1];
    MinusX[2] := X.FInt[2];
    MinusX[3] := X.FInt[3];
    Int128ChgSign(MinusX);
    Int128Add(Fint, MinusX);
  end;
end;
{--------}
procedure TStDecimal.SubtractOne;
var
  MinusOne : TStInt128;
begin
  MinusOne[0] := Int128One_0;
  MinusOne[1] := Int128One_1;
  MinusOne[2] := 0;
  MinusOne[3] := 0;
  Int128ChgSign(MinusOne);
  Int128Add(FInt, MinusOne);
end;
{====================================================================}

end.
