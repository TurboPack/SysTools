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
{* SysTools: StRandom.pas 4.04                           *}
{*********************************************************}
{* SysTools: Classes for random number distributions     *}
{*********************************************************}

{$I StDefine.inc}

unit StRandom;

interface

uses
  Windows, SysUtils, Classes,
  StBase;

type
  TStRandomBase = class
    private
    protected
      function rbMarsagliaGamma(aShape   : double) : double;
      function rbMontyPythonNormal : double;
    public
      {uniform distributions}
      function AsFloat : double; virtual; abstract;
      function AsInt(aUpperLimit : integer) : integer;
      function AsIntInRange(aLowerLimit : integer;
                            aUpperLimit : integer) : integer;

      {continuous non-uniform distributions}
      function AsBeta(aShape1, aShape2 : double) : double;
      function AsCauchy : double;
      function AsChiSquared(aFreedom : integer) : double;
      function AsErlang(aMean  : double;
                        aOrder : integer) : double;
      function AsExponential(aMean : double) : double;
      function AsF(aFreedom1 : integer;
                   aFreedom2 : integer) : double;
      function AsGamma(aShape : double; aScale : double) : double;
      function AsLogNormal(aMean   : double;
                           aStdDev : double) : double;
      function AsNormal(aMean   : double;
                        aStdDev : double) : double;
      function AsT(aFreedom : integer) : double;
      function AsWeibull(aShape : double;
                         aScale : double) : double;
  end;

  TStRandomSystem = class(TStRandomBase)
    private
      FSeed : integer;
    protected
      procedure rsSetSeed(aValue : integer);
    public
      constructor Create(aSeed : integer);
      function AsFloat : double; override;
      property Seed : integer read FSeed write rsSetSeed;
  end;

  TStRandomCombined = class(TStRandomBase)
    private
      FSeed1 : integer;
      FSeed2 : integer;
    protected
      procedure rcSetSeed1(aValue : integer);
      procedure rcSetSeed2(aValue : integer);
    public
      constructor Create(aSeed1, aSeed2 : integer);
      function AsFloat : double; override;
      property Seed1 : integer read FSeed1 write rcSetSeed1;
      property Seed2 : integer read FSeed2 write rcSetSeed2;
  end;

{$IFDEF WIN32}
  TStRandomMother = class(TStRandomBase)
    private
      FNminus4 : integer;
      FNminus3 : integer;
      FNminus2 : integer;
      FNminus1 : integer;
      FC       : integer;
    protected
      procedure rsSetSeed(aValue : integer);
    public
      constructor Create(aSeed : integer);
      function AsFloat : double; override;
      property Seed : integer write rsSetSeed;
  end;
{$ENDIF}

implementation

uses
  StConst;

var
  Root2Pi    : double;
  InvRoot2Pi : double;
  RootLn4    : double;
  Ln2        : double;
  MPN_s      : double;
  Ln2MPN_s   : double;
  MPN_sPlus1 : double;

  Mum1       : integer;
  Mum2       : integer;
  Mum3       : integer;
  Mum4       : integer;

{===Helper routines==================================================}
function GetRandomSeed : integer;
var
  Hash : integer;
  SystemTime: TSystemTime;
  G : integer;
begin
  {start with the tick count}
  Hash := integer(GetTickCount);

  {get the current time}
  GetLocalTime(SystemTime);

  {hash in the milliseconds}
  Hash := (Hash shl 4) + SystemTime.wMilliseconds;
  G := Hash and Integer($F0000000);
  if (G <> 0) then
    Hash := (Hash xor (G shr 24)) xor G;

  {hash in the second}
  Hash := (Hash shl 4) + SystemTime.wSecond;
  G := Hash and Integer($F0000000);
  if (G <> 0) then
    Hash := (Hash xor (G shr 24)) xor G;

  {hash in the minute}
  Hash := (Hash shl 4) + SystemTime.wMinute;
  G := Hash and Integer($F0000000);
  if (G <> 0) then
    Hash := (Hash xor (G shr 24)) xor G;

  {hash in the hour}
  Hash := (Hash shl 3) + SystemTime.wHour;
  G := Hash and Integer($F0000000);
  if (G <> 0) then
    Hash := (Hash xor (G shr 24)) xor G;

  {return the hash}
  Result := Hash;
end;
{====================================================================}


{===TStRandomBase====================================================}
function TStRandomBase.AsBeta(aShape1, aShape2 : double) : double;
var
  R1, R2 : double;
begin
  if not ((aShape1 > 0.0) and (aShape2 > 0.0)) then
    raise EStPRNGError.Create(stscPRNGBetaShapeS);

  if (aShape2 = 1.0) then begin
    repeat
      R1 := AsFloat;
    until R1 <> 0.0;
    Result := exp(ln(R1) / aShape1);
  end
  else if (aShape1 = 1.0) then begin
    repeat
      R1 := AsFloat;
    until R1 <> 0.0;
    Result := 1.0 - exp(ln(R1) / aShape1);
  end
  else begin
    R1 := AsGamma(aShape1, 1.0);
    R2 := AsGamma(aShape2, 1.0);
    Result := R1 / (R1 + R2);
  end;
end;
{--------}
function TStRandomBase.AsCauchy : double;
var
  x : double;
  y : double;
begin
  repeat
    repeat
      x := AsFloat;
    until (x <> 0.0);
    y := (AsFloat * 2.0) - 1.0;
  until sqr(x) + sqr(y) < 1.0;
  Result := y / x;
end;
{--------}
function TStRandomBase.AsChiSquared(aFreedom : integer) : double;
begin
  if not (aFreedom > 0) then
    raise EStPRNGError.Create(stscPRNGDegFreedomS);

  Result := AsGamma(aFreedom * 0.5, 2.0)
end;
{--------}
function TStRandomBase.AsErlang(aMean  : double;
                                aOrder : integer) : double;
var
  Product : double;
  i       : integer;
begin
  if not (aMean > 0.0) then
    raise EStPRNGError.Create(stscPRNGMeanS);
  if not (aOrder > 0) then
    raise EStPRNGError.Create(stscPRNGErlangOrderS);

  if (aOrder < 10) then begin
    Product := 1.0;
    for i := 1 to aOrder do
      Product := Product * AsFloat;
    Result := -aMean * ln(Product) / aOrder;
  end
  else begin
    Result := AsGamma(aOrder, aMean);
  end;
end;
{--------}
function TStRandomBase.AsExponential(aMean : double) : double;
var
  R : double;
begin
  if not (aMean > 0.0) then
    raise EStPRNGError.Create(stscPRNGMeanS);

  repeat
    R := AsFloat;
  until (R <> 0.0);
  Result := -aMean * ln(R);
end;
{--------}
function TStRandomBase.AsF(aFreedom1 : integer;
                           aFreedom2 : integer) : double;
begin
  Result := (AsChiSquared(aFreedom1) * aFreedom1) /
            (AsChiSquared(aFreedom2) * aFreedom2);
end;
{--------}
function TStRandomBase.AsGamma(aShape : double; aScale : double) : double;
var
  R : double;
begin
  if not (aShape > 0.0) then
    raise EStPRNGError.Create(stscPRNGGammaShapeS);
  if not (aScale > 0.0) then
    raise EStPRNGError.Create(stscPRNGGammaScaleS);

  {there are three cases:
   ..0.0 < shape < 1.0, use Marsaglia's technique of
       Gamma(shape) = Gamma(shape+1) * uniform^(1/shape)}
  if (aShape < 1.0) then begin
    repeat
      R := AsFloat;
    until (R <> 0.0);
    Result := aScale * rbMarsagliaGamma(aShape + 1.0) *
              exp(ln(R) / aShape);
  end

  {..shape = 1.0: this is the same as exponential}
  else if (aShape = 1.0) then begin
    repeat
      R := AsFloat;
    until (R <> 0.0);
    Result := aScale * -ln(R);
  end

  {..shape > 1.0: use Marsaglia./Tsang algorithm}
  else begin
    Result := aScale * rbMarsagliaGamma(aShape);
  end;
end;
{--------}
function TStRandomBase.AsInt(aUpperLimit : integer) : integer;
begin
  if not (aUpperLimit > 0) then
    raise EStPRNGError.Create(stscPRNGLimitS);

  Result := Trunc(AsFloat * aUpperLimit);
end;
{--------}
function TStRandomBase.AsIntInRange(aLowerLimit : integer;
                                    aUpperLimit : integer) : integer;
begin
  if not (aLowerLimit < aUpperLimit) then
    raise EStPRNGError.Create(stscPRNGUpperLimitS);

  Result := Trunc(AsFloat * (aUpperLimit - aLowerLimit)) + ALowerLimit;
end;
{--------}
function TStRandomBase.AsLogNormal(aMean   : double;
                                   aStdDev : double) : double;
begin
  Result := exp(AsNormal(aMean, aStdDev));
end;
{--------}
function TStRandomBase.AsNormal(aMean   : double;
                                aStdDev : double) : double;
begin
  if not (aStdDev > 0.0) then
    raise EStPRNGError.Create(stscPRNGStdDevS);

  Result := (rbMontyPythonNormal * aStdDev) + aMean;

(*** alternative: The Box-Muller transformation
var
  R1, R2     : double;
  RadiusSqrd : double;
begin
  {get two random numbers that define a point in the unit circle}
  repeat
    R1 := (2.0 * aRandGen.AsFloat) - 1.0;
    R2 := (2.0 * aRandGen.AsFloat) - 1.0;
    RadiusSqrd := sqr(R1) + sqr(R2);
  until (RadiusSqrd < 1.0) and (RadiusSqrd > 0.0);

  {apply Box-Muller transformation}
  Result := (R1 * sqrt(-2.0 * ln(RadiusSqrd) / RadiusSqrd) * aStdDev)
            + aMean;
 ***)
end;
{--------}
function TStRandomBase.AsT(aFreedom : integer) : double;
begin
  if not (aFreedom > 0) then
    raise EStPRNGError.Create(stscPRNGDegFreedomS);

  Result := rbMontyPythonNormal / sqrt(AsChiSquared(aFreedom) / aFreedom);
end;
{--------}
function TStRandomBase.AsWeibull(aShape : double;
                   aScale : double) : double;
var
  R : double;
begin
  if not (aShape > 0) then
    raise EStPRNGError.Create(stscPRNGWeibullShapeS);
  if not (aScale > 0) then
    raise EStPRNGError.Create(stscPRNGWeibullScaleS);

  repeat
    R := AsFloat;
  until (R <> 0.0);
  Result := exp(ln(-ln(R)) / aShape) * aScale;
end;
{--------}

function TStRandomBase.rbMarsagliaGamma(aShape   : double) : double;
var
  d : double;
  c : double;
  x : double;
  v : double;
  u : double;
  Done : boolean;
begin
  {Notes: implements the Marsaglia/Tsang method of generating random
          numbers belonging to the gamma distribution:

          Marsaglia & Tsang, "A Simple Method for Generating Gamma
          Variables", ACM Transactions on Mathematical Software,
          Vol. 26, No. 3, September 2000, Pages 363-372

          It is pointless to try and work out what's going on in this
          routine without reading this paper :-)
          }

  d := aShape - (1.0 / 3.0);
  c := 1.0 / sqrt(9.0 * d);
  Done := false;
  v := 0.0;

  while not Done do begin
    repeat
      x := rbMontyPythonNormal;
      v := 1.0 + (c * x);
    until (v > 0.0);

    v := v * v * v;
    u := AsFloat;

    Done := u < (1.0 - 0.0331 * sqr(sqr(x)));

    if not Done then
      Done := ln(u) < (0.5 * sqr(x)) + d * (1.0 - v + ln(v))
  end;

  Result := d * v;
end;
{--------}
function TStRandomBase.rbMontyPythonNormal : double;
var
  x : double;
  y : double;
  v : double;
  NonZeroRandom : double;
begin
  {Notes: implements the Monty Python method of generating random
          numbers belonging to the Normal (Gaussian) distribution:

          Marsaglia & Tsang, "The Monty Python Method for Generating
          Random Variables", ACM Transactions on Mathematical
          Software, Vol. 24, No. 3, September 1998, Pages 341-350

          It is pointless to try and work out what's going on in this
          routine without reading this paper :-)

          Some constants:
          a = sqrt(ln(4))
          b = sqrt(2 * pi)
          s = a / (b - a)
          }

  {step 1: generate a random number x between +/- sqrt(2*Pi) and
           return it if its absolute value is less than sqrt(ln(4));
           note that this exit will happen about 47% of the time}
  x := ((AsFloat * 2.0) - 1.0) * Root2Pi;
  if (abs(x) < RootLn4) then begin
    Result := x;
    Exit;
  end;

  {step 2a: generate another random number y strictly between 0 and 1}
  repeat
    y := AsFloat;
  until (y <> 0.0);

  {step 2b: the first quadratic pretest avoids ln() calculation
            calculate v = 2.8658 - |x| * (2.0213 - 0.3605 * |x|)
            return x if y < v}
  v := 2.8658 - Abs(x) * (2.0213 - 0.3605 * Abs(x));
  if (y < v) then begin
    Result := x;
    Exit;
  end;

  {step 2c: the second quadratic pretest again avoids ln() calculation
            return s * (b - x) if y > v + 0.0506}
  if (y > v + 0.0506) then begin
    if (x > 0) then
      Result := MPN_s * (Root2Pi - x)
    else
      Result := -MPN_s * (Root2Pi + x);
    Exit;
  end;

  {step 2d: return x if y < f(x) or
              ln(y) < ln(2) - (0.5 * x * x) }
  if (ln(y) < (Ln2 - (0.5 * x * x))) then begin
    Result := x;
    Exit;
  end;

  {step 3: translate x to s * (b - x) and return it if y > g(x) or
             ln(1 + s - y) < ln(2 * s) - (0.5 * x * x) }
  if (x > 0) then
    x := MPN_s * (Root2Pi - x)
  else
    x := -MPN_s * (Root2Pi + x);
  if (ln(MPN_sPlus1 - y) < (Ln2MPN_s - (0.5 * x * x))) then begin
    Result := x;
    Exit;
  end;

  {step 4: the iterative process}
  repeat
    repeat
      NonZeroRandom := AsFloat;
    until (NonZeroRandom <> 0.0);
    x := -ln(NonZeroRandom) * InvRoot2Pi;
    repeat
      NonZeroRandom := AsFloat;
    until (NonZeroRandom <> 0.0);
    y := -ln(NonZeroRandom);
  until (y + y) > (x * x);
  if (NonZeroRandom < 0.5) then
    Result := -(Root2Pi + x)
  else
    Result := Root2Pi + x;
end;
{====================================================================}


{===TStRandomSystem==================================================}
constructor TStRandomSystem.Create(aSeed : integer);
begin
  inherited Create;
  Seed := aSeed;
end;
{--------}
function TStRandomSystem.AsFloat : double;
var
  SaveSeed : integer;
begin
  SaveSeed := RandSeed;
  RandSeed := FSeed;
  Result := System.Random;
  FSeed := RandSeed;
  RandSeed := SaveSeed;
end;
{--------}
procedure TStRandomSystem.rsSetSeed(aValue : integer);
begin
  if (aValue = 0) then
    FSeed := GetRandomSeed
  else
    FSeed := aValue;
end;
{====================================================================}


{===TStRandomCombined================================================}
const
  m1 = 2147483563;
  m2 = 2147483399;
{--------}
constructor TStRandomCombined.Create(aSeed1, aSeed2 : integer);
begin
  inherited Create;
  Seed1 := aSeed1;
  if (aSeed1 = 0) and (aSeed2 = 0) then
    Sleep(10); // a small delay to enable seed to change
  Seed2 := aSeed2;
end;
{--------}
function TStRandomCombined.AsFloat : double;
const
  a1 = 40014;
  q1 = 53668;  {equals m1 div a1}
  r1 = 12211;  {equals m1 mod a1}

  a2 = 40692;
  q2 = 52774;  {equals m2 div a2}
  r2 = 3791;   {equals m2 mod a2}

  OneOverM1 : double = 1.0 / m1;
var
  k : Integer;
  Z : Integer;
begin
  {advance first PRNG}
  k := FSeed1 div q1;
  FSeed1 := (a1 * (FSeed1 - (k * q1))) - (k * r1);
  if (FSeed1 < 0) then
    inc(FSeed1, m1);

  {advance second PRNG}
  k := FSeed2 div q2;
  FSeed2 := (a2 * (FSeed2 - (k * q2))) - (k * r2);
  if (FSeed2 < 0) then
    inc(FSeed2, m2);

  {combine the two seeds}
  Z := FSeed1 - FSeed2;
  if (Z <= 0) then
    Z := Z + m1 - 1;
  Result := Z * OneOverM1;
end;
{--------}
procedure TStRandomCombined.rcSetSeed1(aValue : integer);
begin
  if (aValue = 0) then
    FSeed1 := GetRandomSeed
  else
    FSeed1 := aValue;
end;
{--------}
procedure TStRandomCombined.rcSetSeed2(aValue : integer);
begin
  if (aValue = 0) then
    FSeed2 := GetRandomSeed
  else
    FSeed2 := aValue;
end;
{====================================================================}


{$IFDEF WIN32}
{===TStRandomMother==================================================}
constructor TStRandomMother.Create(aSeed : integer);
begin
  inherited Create;
  Seed := aSeed;
end;
{--------}
function TStRandomMother.AsFloat : double;
const
  TwoM31 : double = 1.0 / $7FFFFFFF;
begin
  asm
    push esi
    push edi
    push ebx

    {get around a compiler bug where it doesn't notice that edx is
     being changed in the asm code !!! D5 bug}
    push edx

    {set ebx to point to self}
    mov ebx, eax

    {multiply X(n-4) by 21111111}
    mov eax, [ebx].TStRandomMother.FNMinus4
    mul [Mum1]
    mov edi, eax
    mov esi, edx

    {multiply X(n-3) by 1492 (save it in X(n-4) before though)}
    mov eax, [ebx].TStRandomMother.FNMinus3
    mov [ebx].TStRandomMother.FNMinus4, eax
    mul [Mum2]
    add edi, eax
    adc esi, edx

    {multiply X(n-2) by 1776 (save it in X(n-3) before though)}
    mov eax, [ebx].TStRandomMother.FNMinus2
    mov [ebx].TStRandomMother.FNMinus3, eax
    mul [Mum3]
    add edi, eax
    adc esi, edx

    {multiply X(n-1) by 5115 (save it in X(n-2) before though)}
    mov eax, [ebx].TStRandomMother.FNMinus1
    mov [ebx].TStRandomMother.FNMinus2, eax
    mul [Mum4]
    add edi, eax
    adc esi, edx

    {add in the remainder}
    add edi, [ebx].TStRandomMother.FC
    adc esi, 0;

    {save the lower 32 bits in X(n-1), the upper into the remainder}
    mov [ebx].TStRandomMother.FNMinus1, edi
    mov [ebx].TStRandomMother.FC, esi

    {get around a compiler bug where it doesn't notice that edx was
     changed in the asm code !!! D5 bug}
    pop edx

    pop ebx
    pop edi
    pop esi
  end;
  Result := (FNMinus1 shr 1) * TwoM31;
end;
{--------}
{$IFOPT Q+}
{note: TStRandomMother.rsSetSeed expressly overflows integers (it's
       equivalent to calculating mod 2^32), so we have to force
       overflow checks off}
{$DEFINE SaveQPlus}
{$Q-}
{$ENDIF}
procedure TStRandomMother.rsSetSeed(aValue : integer);
begin
  if (aValue = 0) then
    aValue := GetRandomSeed;
  FNminus4 := aValue;
  {note: the following code uses the generator
            Xn := (69069 * Xn-1) mod 2^32
         from D.E.Knuth, The Art of Computer Programming, Vol. 2
         (second edition), Addison-Wesley, 1981, pp.102}
  FNminus3 := 69069 * FNminus4;
  FNminus2 := 69069 * FNminus3;
  FNminus1 := 69069 * FNminus2;
  FC := 69069 * FNminus1;
end;
{$IFDEF SaveQPlus}
{$Q+}
{$ENDIF}
{$ENDIF}
{====================================================================}


{====================================================================}
procedure CalcConstants;
begin
  {for the normal variates}
  Root2Pi := sqrt(2 * Pi);
  InvRoot2Pi := 1.0 / Root2Pi;
  RootLn4 := sqrt(ln(4.0));
  Ln2 := ln(2.0);
  MPN_s := RootLn4 / (Root2Pi - RootLn4);
  Ln2MPN_s := ln(2.0 * MPN_s);
  MPN_sPlus1 := MPN_s + 1.0;

  Mum1 := 2111111111;
  Mum2 := 1492;
  Mum3 := 1776;
  Mum4 := 5115;
end;
{====================================================================}


initialization
  CalcConstants;

end.
