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
{* SysTools: StJupsat.pas 4.04                           *}
{*********************************************************}
{* SysTools: Astronomical Routines                       *}
{*           (for the four "Gallilean" moons of Jupiter  *}
{*            Callisto, Europa, Ganymede, and Io)        *}
{*********************************************************}

{$I StDefine.inc}

{ ************************************************************** }
{ Sources:                                                       }
{   1. Astronomical Algorithms, Jean Meeus, Willmann-Bell, 1991. }
{                                                                }
{   2. Planetary and Lunar Coordinates (1984-2000), U.S. Govt,   }
{         1983.                                                  }
{                                                                }
{   3. Supplement to the American Ephemeris and Nautical Almanac,}
{        U.S. Govt, 1964.                                        }
{                                                                }
{   4. MPO96-98 source files, Brian D. Warner, 1995-98.          }
{                                                                }
{ ************************************************************** }

(* **************************************************************

The formulae in this unit are based on DYNAMICAL time, which is based
on the actual rotation of the Earth and is gradually slowing. Universal
Time is based on a fixed rotation rate. To directly compare results in
the astronomical literature (and Meeus' examples), you must use a Universal
Time that is adjusted by the value Delta T. This value is approximately 1 min
in the latter part of the 20th century and will be about 80 seconds in the
year 2020. As an example, to compare the High Precision positions for
1992 December 16 (Meeus' example), you must use a Universal Time of
1992 December 16 at 00:00:59 which equals 00:00:00 Dynamical Time


The Shadows parameter is used for high precision calculations only. If True,
the positions are calculated as seen from the SUN, not the Earth. For eclipses
of the satellites by Jupiter, in effect, the position is in reference to the
SHADOW of Jupiter in space, not the planet itself. For shadow transits, where
the shadow of the satellite is projected onto the surface of the planet, the
position is that of the satellite's shadow in reference to Jupiter and not
the satellite itself.

The purpose of the Shadows parameter is to complete the prediction for
satellite phenomenon. For example, using Shadow := False, the result may
indicate that a satellite goes behind Jupiter at a given instant but not
if the satellite is visible because it is in Jupiter's shadow. Setting
Shadow := True for the same time will indicate if the planet is in or out
of Jupiter's shadow.

(Shadow := FALSE) and (abs(satellite X-coordinate) = 1)
-------------------------------------------------------
If the X-value is negative and heading towards 0, the satellite is entering
the front of the planet.
If the X-value is negative and increasing, the satellite is coming from
behind the planet.
If the X-value is positive and heading towards 0, the satellite is going
behind the planet.
If the X-value is positive and increasing, the satellite is leaving
the front of the planet.

(Shadow := TRUE) and (abs(satellite X-coordinate) = 1)
-------------------------------------------------------
If the X-value is negative and heading towards 0, the satellite's shadow is
entering the planet's disc.
If the X-value is negative and increasing, the satellite is leaving the
planet's shadow.
If the X-value is positive and heading towards 0, the satellite entering
the planet's shadow.
If the X-value is positive and increasing, the satellite's shadow is
leaving the planet.

The X and Y coordinates are based on the equatorial radius of Jupiter. Because
the planet is considerably flattened by its rapid rotation, the polar diameter
is less than 1. To avoid dealing with an elliptical disc for Jupiter, multiply
the Y values only by 1.071374. This creates a "circular Jupiter" and so makes
determining if the satellite is above or below Jupiter easier
(abs(Y-coordinate) > 1).

****************************************************************** *)

unit StJupsat;

interface

type
  TStJupSatPos = packed record
    X : Double;
    Y : Double;
  end;

  TStJupSats = packed record
    Io       : TStJupSatPos;
    Europa   : TStJupSatPos;
    Ganymede : TStJupSatPos;
    Callisto : TStJupSatPos;
  end;

function GetJupSats(JD : TDateTime; HighPrecision, Shadows : Boolean) : TStJupSats;

implementation
uses
  StDate, StAstro, StAstroP, StJup, StMath;

type
  SunCoordsRec = packed record
    X, Y, Z   : Double;
    L, B, R   : Double;
  end;

  TranformRec = packed record
    A, B, C  : array[1..6] of Double;
  end;


function SunCoords(JD : Double) : SunCoordsRec;
var
  L, B, R,
  T0, TM,
  RS,
  OB, A         : Double;
begin
  T0 := (JD - StdDate) / 365250;
  TM := T0/100;
  RS := radcor * 3600;
  OB := 0.4090928042223
      - 4680.93/RS * TM
      -    1.55/RS * sqr(TM)
      + 1999.25/RS * sqr(TM) * TM
      -   51.38/RS * sqr(sqr(TM))
      -  249.67/RS * sqr(sqr(TM)) * TM
      -   39.05/RS * sqr(sqr(TM)) * sqr(TM)
      +    7.12/RS * sqr(sqr(TM)) * sqr(TM) * TM
      +   27.87/RS * sqr(sqr(sqr(TM)));

  L := 175347046
     +   3341656 * cos(4.6692568 +  6283.07585*T0)
     +     34894 * cos(4.6261000 + 12566.1517*T0)
     +      3497 * cos(2.7441000 +  5753.3849*T0)
     +      3418 * cos(2.8289000 +     3.5231*T0)
     +      3136 * cos(3.6277000 + 77713.7715*T0)
     +      2676 * cos(4.4181000 +  7860.4194*T0)
     +      2343 * cos(6.1352000 +  3930.2097*T0)
     +      1324 * cos(0.7425000 + 11506.7698*T0)
     +      1273 * cos(2.0371000 +   529.6910*T0)
     +      1199 * cos(1.1096000 +  1577.3435*T0)
     +       990 * cos(5.2330000 +  5884.9270*T0)
     +       902 * cos(2.0450000 +    26.1490*T0)
     +       857 * cos(3.5080000 +   398.149*T0)
     +       780 * cos(1.1790000 +  5223.694*T0)
     +       753 * cos(2.5330000 +  5507.553*T0)
     +       505 * cos(4.5830000 + 18849.228*T0)
     +       492 * cos(4.2050000 +   775.523*T0)
     +       357 * cos(2.9200000 +     0.067*T0)
     +       317 * cos(5.8490000 + 11790.626*T0)
     +       284 * cos(1.8990000 +   796.298*T0)
     +       271 * cos(0.3150000 + 10977.079*T0)
     +       243 * cos(0.3450000 +  5486.778*T0)
     +       206 * cos(4.8060000 +  2544.314*T0)
     +       205 * cos(1.8690000 +  5573.143*T0)
     +       202 * cos(2.4580000 +  6069.777*T0)
     +       156 * cos(0.8330000 +   213.299*T0)
     +       132 * cos(3.4110000 +  2942.463*T0)
     +       126 * cos(1.0830000 +    20.775*T0)
     +       115 * cos(0.6450000 +     0.980*T0)
     +       103 * cos(0.6360000 +  4694.003*T0)
     +       102 * cos(0.9760000 + 15720.839*T0)
     +       102 * cos(4.2670000 +     7.114*T0)
     +        99 * cos(6.2100000 +  2146.170*T0)
     +        98 * cos(0.6800000 +   155.420*T0)
     +        86 * cos(5.9800000 +161000.690*T0)
     +        85 * cos(1.3000000 +  6275.960*T0)
     +        85 * cos(3.6700000 + 71430.700*T0)
     +        80 * cos(1.8100000 + 17260.150*T0);

  A := 628331966747.0
     + 206059 * cos(2.678235 +  6283.07585*T0)
     +   4303 * cos(2.635100 + 12566.1517*T0)
     +    425 * cos(1.590000 +     3.523*T0)
     +    119 * cos(5.796000 +    26.298*T0)
     +    109 * cos(2.966000 +  1577.344*T0)
     +     93 * cos(2.590000 + 18849.23*T0)
     +     72 * cos(1.140000 +   529.69*T0)
     +     68 * cos(1.870000 +   398.15*T0)
     +     67 * cos(4.410000 +  5507.55*T0)
     +     59 * cos(2.890000 +  5223.69*T0)
     +     56 * cos(2.170000 +   155.42*T0)
     +     45 * cos(0.400000 +   796.30*T0)
     +     36 * cos(0.470000 +   775.52*T0)
     +     29 * cos(2.650000 +     7.11*T0)
     +     21 * cos(5.340000 +     0.98*T0)
     +     19 * cos(1.850000 +  5486.78*T0)
     +     19 * cos(4.970000 +   213.30*T0)
     +     17 * cos(2.990000 +  6275.96*T0)
     +     16 * cos(0.030000 +  2544.31*T0);
  L := L + (A * T0);

  A := 52919
     + 8720 * cos(1.0721 +  6283.0758*T0)
     +  309 * cos(0.867  + 12566.152*T0)
     +   27 * cos(0.050  +     3.52*T0)
     +   16 * cos(5.190  +    26.30*T0)
     +   16 * cos(3.68   +   155.42*T0)
     +   10 * cos(0.76   + 18849.23*T0)
     +    9 * cos(2.06   + 77713.77*T0)
     +    7 * cos(0.83   +   775.52*T0)
     +    5 * cos(4.66   +  1577.34*T0);
  L := L + (A * sqr(T0));

  A := 289 * cos(5.844 +  6283.076*T0)
     +  35
     +  17 * cos(5.49  + 12566.15*T0)
     +   3 * cos(5.20  +   155.42*T0)
     +   1 * cos(4.72  +     3.52*T0);
  L := L + (A * sqr(T0) * T0);

  A := 114 * cos(3.142);
  L := L + (A * sqr(sqr(T0)));
  L := L / 1.0E+8;

{solar latitude}
  B := 280 * cos(3.199 + 84334.662*T0)
     + 102 * cos(5.422 +  5507.553*T0)
     +  80 * cos(3.88  +  5223.69*T0)
     +  44 * cos(3.70  +  2352.87*T0)
     +  32 * cos(4.00  +  1577.34*T0);

  A :=   9 * cos(3.90  + 5507.550*T0)
     +   6 * cos(1.73  + 5223.690*T0);
  B := B + (A * T0);
  B := B / 1.0E+8;


{solar radius vector (astronomical units)}
  R         := 100013989
             +   1670700 * cos(3.0984635 +  6283.07585*T0)
             +     13956 * cos(3.05525   + 12566.15170*T0)
             +      3084 * cos(5.1985    + 77713.7715*T0)
             +      1628 * cos(1.1739    +  5753.3849*T0)
             +      1576 * cos(2.8649    +  7860.4194*T0)
             +       925 * cos(5.453     + 11506.770*T0)
             +       542 * cos(4.564     +  3930.210*T0)
             +       472 * cos(3.661     +  5884.927*T0)
             +       346 * cos(0.964     +  5507.553*T0)
             +       329 * cos(5.900     +  5223.694*T0)
             +       307 * cos(0.299     +  5573.143*T0)
             +       243 * cos(4.273     + 11790.629*T0)
             +       212 * cos(5.847     +  1577.344*T0)
             +       186 * cos(5.022     + 10977.079*T0)
             +       175 * cos(3.012     + 18849.228*T0)
             +       110 * cos(5.055     +  5486.778*T0)
             +        98 * cos(0.89      +  6069.78*T0)
             +        86 * cos(5.69      + 15720.84*T0)
             +        86 * cos(1.27      +161000.69*T0)
             +        65 * cos(0.27      + 17260.15*T0)
             +        63 * cos(0.92      +   529.69*T0)
             +        57 * cos(2.01      + 83996.85*T0)
             +        56 * cos(5.24      + 71430.70*T0)
             +        49 * cos(3.25      +  2544.31*T0)
             +        47 * cos(2.58      +   775.52*T0)
             +        45 * cos(5.54      +  9437.76*T0)
             +        43 * cos(6.01      +  6275.96*T0)
             +        39 * cos(5.36      +  4694.00*T0)
             +        38 * cos(2.39      +  8827.39*T0)
             +        37 * cos(0.83      + 19651.05*T0)
             +        37 * cos(4.90      + 12139.55*T0)
             +        36 * cos(1.67      + 12036.46*T0)
             +        35 * cos(1.84      +  2942.46*T0)
             +        33 * cos(0.24      +  7084.90*T0)
             +        32 * cos(0.18      +  5088.63*T0)
             +        32 * cos(1.78      +   398.15*T0)
             +        28 * cos(1.21      +  6286.60*T0)
             +        28 * cos(1.90      +  6279.55*T0)
             +        26 * cos(4.59      + 10447.39*T0);
  R := R / 1.0E+8;

  A := 103019 * cos(1.107490 +  6283.075850*T0)
     +   1721 * cos(1.0644   + 12566.1517*T0)
     +    702 * cos(3.142)
     +     32 * cos(1.02     + 18849.23*T0)
     +     31 * cos(2.84     +  5507.55*T0)
     +     25 * cos(1.32     +  5223.69*T0)
     +     18 * cos(1.42     +  1577.34*T0)
     +     10 * cos(5.91     + 10977.08*T0)
     +      9 * cos(1.42     +  6275.96*T0)
     +      9 * cos(0.27     +  5486.78*T0);
  R := R + (A * T0 / 1.0E+8);

  A := 4359 * cos(5.7846 +  6283.0758*T0)
     +  124 * cos(5.579  + 12566.152*T0)
     +   12 * cos(3.14)
     +    9 * cos(3.63   + 77713.77*T0)
     +    6 * cos(1.87   +  5573.14*T0)
     +    3 * cos(5.47   + 18849.23*T0);
  R := R + (A * sqr(T0) / 1.0E+8);

  L := (L + PI);
  L := Frac(L / 2.0 / PI) * 2.0 * Pi;
  if L < 0 then
    L := L + (2.0*PI);
  B := -B;

  Result.L := L;
  Result.B := B;
  Result.R := R;
  Result.X := R * cos(B) * cos(L);
  Result.Y := R * (cos(B) * sin(L) * cos(OB) - sin(B) * sin(OB));
  Result.Z := R * (cos(B) * sin(L) * sin(OB) + sin(B) * cos(OB));
end;


{-------------------------------------------------------------------------}

function JupSatsLo(AJD : Double) : TStJupSats;
var
  DateDif,   {d}
  ArgJup,    {V}
  AnomE,     {M}
  AnomJ,     {N}
  DeltaLong, {J}
  ECenterE,  {A}
  ECenterJ,  {B}
  K,
  RVE,       {R}
  RVJ,       {r}
  EJDist,    {Delta}
  Phase,     {Psi}
  Lambda,    {Lambda}
  DS, DE,    {DS, DE}
  Mu1, Mu2,
  Mu3, Mu4,  {Mu1 - Mu4}
  G, H,      {G, H}
  TmpDbl1,
  TmpDbl2,
  R1, R2,
  R3, R4     {R1 - R4}
             : Double;

begin
  AJD         := DateTimeToAJD(AJD);
  DateDif     := AJD - 2451545.0;
  ArgJup      := 172.74 + (0.00111588 * DateDif);
  ArgJup := Frac(ArgJup/360.0) * 360.0;
  if (ArgJup < 0) then
    ArgJup := 360.0 + ArgJup;
  ArgJup      := ArgJup / radcor;

  AnomE       := 357.529 + (0.9856003 * DateDif);
  AnomE := Frac(AnomE/360.0) * 360.0;
  if (AnomE < 0) then
    AnomE := 360.0 + AnomE;
  AnomE       := AnomE / radcor;

  AnomJ       := 20.020 + (0.0830853 * DateDif + (0.329 * sin(ArgJup)));
  AnomJ := Frac(AnomJ/360.0) * 360.0;
  if (AnomJ < 0) then
    AnomJ := 360.0 + AnomJ;
  AnomJ       := AnomJ / radcor;

  DeltaLong   := 66.115 + (0.9025179 * DateDif - (0.329 * sin(ArgJup)));
  DeltaLong   := Frac(DeltaLong/360.0) * 360.0;
  if (DeltaLong < 0) then
    DeltaLong := 360.0 + DeltaLong;
  DeltaLong   := DeltaLong / radcor;

  ECenterE    := 1.915 * sin(AnomE) + 0.020 * sin(2*AnomE);
  ECenterE    := ECenterE / radcor;

  ECenterJ    := 5.555 * sin(AnomJ) + 0.168 * sin(2*AnomJ);
  ECenterJ    := ECenterJ / radcor;

  K := (DeltaLong + ECenterE - ECenterJ);

  RVE := 1.00014 - (0.01671 * cos(AnomE)) - (0.00014 * cos(2*AnomE));
  RVJ := 5.20872 - (0.25208 * cos(AnomJ)) - (0.00611 * cos(2*AnomJ));

  EJDist := sqrt(sqr(RVJ) + sqr(RVE) - (2 * RVJ * RVE * cos(K)));

  Phase := RVE/EJDist * sin(K);
  Phase := StInvSin(Phase);

  if ((sin(K) < 0) and (Phase > 0)) or
     ((sin(K) > 0) and (Phase < 0)) then
    Phase := -Phase;

  Lambda := 34.35 + (0.083091 * DateDif) + (0.329 * sin(ArgJup));
  Lambda := Lambda / radcor + ECenterJ;

  DS := 3.12 * sin(Lambda + 42.8 / radcor);
  DE := DS - 2.22 * sin(Phase) * cos(Lambda + 22/radcor)
      - 1.30 * ((RVJ - EJDist) / EJDist) * sin(Lambda - 100.5/radcor);
  DE := DE / radcor;

  Mu1 := 163.8067 + 203.4058643 * (DateDif - (EJDist / 173));
  Mu1 := Frac(Mu1/360.0) * 360.0;
  if (Mu1 < 0) then
    Mu1 := 360.0 + Mu1;
  Mu1 := Mu1 / radcor + Phase - ECenterJ;

  Mu2 := 358.4108 + 101.2916334 * (DateDif - (EJDist / 173));
  Mu2 := Frac(Mu2/360.0) * 360.0;
  if (Mu2 < 0) then
    Mu2 := 360.0 + Mu2;
  Mu2 := Mu2 / radcor + Phase - ECenterJ;

  Mu3 :=   5.7129 +  50.2345179 * (DateDif - (EJDist / 173));
  Mu3 := Frac(Mu3/360.0) * 360.0;
  if (Mu3 < 0) then
    Mu3 := 360.0 + Mu3;
  Mu3 := Mu3 / radcor + Phase - ECenterJ;

  Mu4 := 224.8151 +  21.4879801 * (DateDif - (EJDist / 173));
  Mu4 := Frac(Mu4/360.0) * 360.0;
  if (Mu4 < 0) then
    Mu4 := 360.0 + Mu4;
  Mu4 := Mu4 / radcor + Phase - ECenterJ;

  G := 331.18 + 50.310482 * (DateDif - (EJDist / 173));
  G := Frac(G/360.0) * 360.0;
  if (G < 0) then
    G := 360.0 + G;
  G := G / radcor;
  H :=  87.40 + 21.569231 * (DateDif - (EJDist / 173));
  H := Frac(H/360.0) * 360.0;
  if (H < 0) then
    H := 360.0 + H;
  H := H / radcor;

  TmpDbl1 := 0.473 * sin(2 * (Mu1 - Mu2)) / radcor;
  TmpDbl2 := 1.065 * sin(2 * (Mu2 - Mu3)) / radcor;

  R1 :=  5.9073 - 0.0244 * cos(2 * (Mu1 - Mu2));
  R2 :=  9.3991 - 0.0882 * cos(2 * (Mu2 - Mu3));
  R3 := 14.9924 - 0.0216 * cos(G);
  R4 := 26.3699 - 0.1935 * cos(H);

  Mu1 := Mu1 + TmpDbl1;
  Mu2 := Mu2 + TmpDbl2;
  Mu3 := Mu3 + (0.165 * sin(G)) / radcor;
  Mu4 := Mu4 + (0.841 * sin(H)) / radcor;

  Result.Io.X := R1 * sin(Mu1);
  Result.Io.Y := -R1 * cos(Mu1) * sin(DE);

  Result.Europa.X := R2 * sin(Mu2);
  Result.Europa.Y := -R2 * cos(Mu2) * sin(DE);

  Result.Ganymede.X := R3 * sin(Mu3);
  Result.Ganymede.Y := -R3 * cos(Mu3) * sin(DE);

  Result.Callisto.X := R4 * sin(Mu4);
  Result.Callisto.Y := -R4 * cos(Mu4) * sin(DE);
end;

{-------------------------------------------------------------------------}

function JupSatsHi(AJD : Double; Shadows : Boolean) : TStJupSats;
var
  I      : Integer;
  SunPos : SunCoordsRec;
  STUT   : TStDateTimeRec;
  JupPos : TStEclipticalCord;

  SatX   : array[1..5] of Double;
  SatY   : array[1..5] of Double;
  SatZ   : array[1..5] of Double;

  TD1,
  TD2,
  Angle,        {Temporary Double values}
  LTime,        {Tau}
  AJDT,         {AJD adjusted for light time (Tau)}
  JupX,
  JupY,
  JupZ,         {Jupiter's geocentric rectangular coordinates}
  EJDist,       {Delta}
  Jup1,
  Jup2,         {/\, Alpha}
  DateDif,      {t}
  L1, L2,
  L3, L4,       {script L1-4}
  Pi1, Pi2,
  Pi3, Pi4,     {Pi1-4}
  W1, W2,
  W3, W4,       {Omega1-4}
  Inequality,   {upside down L}
  PhiLambda,
  NodeJup,      {Psi}
  AnomJup,      {G}
  AnomSat,      {G'}
  LongPerJ,
  S1, S2,
  S3, S4,       {Sum1-4}
  TL1, TL2,
  TL3, TL4,     {Capital L1-4}
  B1, B2,
  B3, B4,       {tangent of latitude}
  R1, R2,
  R3, R4,       {radius vector}
  T0,           {Julian Centuries}
  Precession,   {P}
  Inclination   {I}

                : Double;
  Transforms    : array[1..5] of TranformRec;

begin
  FillChar(Result, SizeOf(TStJupSats), #0);
  AJD := DateTimeToAJD(AJD);
  SunPos := SunCoords(AJD);

  if not Shadows then begin
    TD1 := 5;
    AJDT := AJD - 0.0057755183 * TD1;  {first guess}
    repeat
      JupPos := ComputeJupiter(AJDT);

      JupX := JupPos.R0 * cos(JupPos.B0) * cos(JupPos.L0)
            + SunPos.R * cos(SunPos.L);
      JupY := JupPos.R0 * cos(JupPos.B0) * sin(JupPos.L0)
            + SunPos.R * sin(SunPos.L);
      JupZ := JupPos.R0 * sin(JupPos.B0);

      EJDist := sqrt(sqr(JupX) + sqr(JupY) + sqr(JupZ));
      TD2 := abs(EJDist - TD1);
      if abs(TD2) > 0.0005 then begin
        AJDT := AJD - 0.0057755183 * ((EJDist + TD1) / 2);
        TD1 := EJDist;
      end;
    until (TD2 <= 0.0005);
  end else begin
    JupPos := ComputeJupiter(AJD);

    JupX := JupPos.R0 * cos(JupPos.B0) * cos(JupPos.L0);
    JupY := JupPos.R0 * cos(JupPos.B0) * sin(JupPos.L0);
    JupZ := JupPos.R0 * sin(JupPos.B0);
    EJDist := sqrt(sqr(JupX+SunPos.X) +
                   sqr(JupY+SunPos.Y) + sqr(JupZ+SunPos.Z));
  end;

  Jup1  := StInvTan2(JupX, JupY);
  Jup2  := ArcTan(JupZ / sqrt(sqr(JupX) + sqr(JupY)));

  DateDif := AJD - 2443000.5 - (0.0057755183 * EJDist);

  L1 := 106.07947 + 203.488955432 * DateDif;
  L1 := Frac(L1/360.0) * 360.0;
  if (L1 < 0) then
    L1 := 360.0 + L1;
  L1 := L1 / radcor;

  L2 := 175.72938 + 101.374724550 * DateDif;
  L2 := Frac(L2/360.0) * 360.0;
  if (L2 < 0) then
    L2 := 360.0 + L2;
  L2 := L2 / radcor;

  L3 := 120.55434 +  50.317609110 * DateDif;
  L3 := Frac(L3/360.0) * 360.0;
  if (L3 < 0) then
    L3 := 360.0 + L3;
  L3 := L3 / radcor;

  L4 :=  84.44868 +  21.571071314 * DateDif;
  L4 := Frac(L4/360.0) * 360.0;
  if (L4 < 0) then
    L4 := 360.0 + L4;
  L4 := L4 / radcor;

  Pi1 :=  58.3329 + 0.16103936 * DateDif;
  Pi1 := Frac(Pi1/360.0) * 360.0;
  if (Pi1 < 0) then
    Pi1 := 360.0 + Pi1;
  Pi1 := Pi1 / radcor;

  Pi2 := 132.8959 + 0.04647985 * DateDif;
  Pi2 := Frac(Pi2/360.0) * 360.0;
  if (Pi2 < 0) then
    Pi2 := 360.0 + Pi2;
  Pi2 := Pi2 / radcor;

  Pi3 := 187.2887 + 0.00712740 * DateDif;
  Pi3 := Frac(Pi3/360.0) * 360.0;
  if (Pi3 < 0) then
    Pi3 := 360.0 + Pi3;
  Pi3 := Pi3 / radcor;

  Pi4 := 335.3418 + 0.00183998 * DateDif;
  Pi4 := Frac(Pi4/360.0) * 360.0;
  if (Pi4 < 0) then
    Pi4 := 360.0 + Pi4;
  Pi4 := Pi4 / radcor;

  W1 := 311.0793 - 0.13279430 * DateDif;
  W1 := Frac(W1/360.0) * 360.0;
  if (W1 < 0) then
    W1 := 360.0 + W1;
  W1 := W1 / radcor;

  W2 := 100.5099 - 0.03263047 * DateDif;
  W2 := Frac(W2/360.0) * 360.0;
  if (W2 < 0) then
    W2 := 360.0 + W2;
  W2 := W2 / radcor;

  W3 := 119.1688 - 0.00717704 * DateDif;
  W3 := Frac(W3/360.0) * 360.0;
  if (W3 < 0) then
    W3 := 360.0 + W3;
  W3 := W3 / radcor;

  W4 := 322.5729 - 0.00175934 * DateDif;
  W4 := Frac(W4/360.0) * 360.0;
  if (W4 < 0) then
    W4 := 360.0 + W4;
  W4 := W4 / radcor;

  Inequality := 0.33033 * sin((163.679 + 0.0010512*DateDif) / radcor)
              + 0.03439 * sin((34.486 - 0.0161731*DateDif) / radcor);
  Inequality := Inequality / radcor;

  PhiLambda := 191.8132 + 0.17390023 * DateDif;
  PhiLambda := Frac(PhiLambda / 360.0) * 360.0;
  if (PhiLambda < 0) then
    PhiLambda := 360.0 + PhiLambda;
  PhiLambda := PhiLambda / radcor;

  NodeJup := 316.5182 - 0.00000208 * DateDif;
  NodeJup := Frac(NodeJup / 360.0) * 360.0;
  if (NodeJup < 0) then
    NodeJup := 360.0 + NodeJup;
  NodeJup := NodeJup / radcor;

  AnomJup := 30.23756 + 0.0830925701 * DateDif;
  AnomJup := Frac(AnomJup / 360.0) * 360.0;
  if (AnomJup < 0) then
    AnomJup := 360.0 + AnomJup;
  AnomJup := AnomJup / radcor + Inequality;

  AnomSat := 31.97853 + 0.0334597339 * DateDif;
  AnomSat := Frac(AnomSat / 360.0) * 360.0;
  if (AnomSat < 0) then
    AnomSat := 360.0 + AnomSat;
  AnomSat := AnomSat / radcor;

  LongPerJ := 13.469942 / radcor;

  S1 := 0.47259 * sin(2*(L1-L2))
      - 0.03480 * sin(Pi3-Pi4)
      - 0.01756 * sin(Pi1 + Pi3 - 2*LongPerJ - 2*AnomJup)
      + 0.01080 * sin(L2 - 2*L3 + Pi3)
      + 0.00757 * sin(PhiLambda)
      + 0.00663 * sin(L2 - 2*L3 + Pi4)
      + 0.00453 * sin(L1 - Pi3)
      + 0.00453 * sin(L2 - 2*L3 + Pi2)
      - 0.00354 * sin(L1-L2)
      - 0.00317 * sin(2*NodeJup - 2*LongPerJ)
      - 0.00269 * sin(L2 - 2*L3 + Pi1)
      + 0.00263 * sin(L1 - Pi4)
      + 0.00186 * sin(L1 - Pi1)
      - 0.00186 * sin(AnomJup)
      + 0.00167 * sin(Pi2 - Pi3)
      + 0.00158 * sin(4*(L1-L2))
      - 0.00155 * sin(L1 - L3)
      - 0.00142 * sin(NodeJup + W3 - 2*LongPerJ - 2*AnomJup)
      - 0.00115 * sin(2*(L1 - 2*L2 + W2))
      + 0.00089 * sin(Pi2 - Pi4)
      + 0.00084 * sin(W2 - W3)
      + 0.00084 * sin(L1 + Pi3 - 2*LongPerJ - 2*AnomJup)
      + 0.00053 * sin(NodeJup - W2);

  S2 := 1.06476 * sin(2*(L2-L3))
      + 0.04253 * sin(L1 - 2*L2 + Pi3)
      + 0.03579 * sin(L2 - Pi3)
      + 0.02383 * sin(L1 - 2*L2 + Pi4)
      + 0.01977 * sin(L2 - Pi4)
      - 0.01843 * sin(PhiLambda)
      + 0.01299 * sin(Pi3 - Pi4)
      - 0.01142 * sin(L2 - L3)
      + 0.01078 * sin(L2 - Pi2)
      - 0.01058 * sin(AnomJup)
      + 0.00870 * sin(L2 - 2*L3 + Pi2)
      - 0.00775 * sin(2*(NodeJup - LongPerJ))
      + 0.00524 * sin(2*(L1-L2))
      - 0.00460 * sin(L1-L3)
      + 0.00450 * sin(L2 - 2*L3 + Pi1)
      + 0.00327 * sin(NodeJup - 2*AnomJup + W3 - 2*LongPerJ)
      - 0.00296 * sin(Pi1 + Pi3 - 2*LongPerJ - 2*AnomJup)
      - 0.00151 * sin(2*AnomJup)
      + 0.00146 * sin(NodeJup - W3)
      + 0.00125 * sin(NodeJup - W4)
      - 0.00117 * sin(L1 -2*L3 + Pi3)
      - 0.00095 * sin(2*(L2-W2))
      + 0.00086 * sin(2*(L1-2*L2 +W2))
      - 0.00086 * sin(5*AnomSat - 2*AnomJup + 52.225/radcor)
      - 0.00078 * sin(L2-L4)
      - 0.00064 * sin(L1 - 2*L3 + Pi4)
      - 0.00063 * sin(3*L3 - 7*L4 + 4*Pi4)
      + 0.00061 * sin(Pi1 - Pi4)
      + 0.00058 * sin(2*(NodeJup - LongPerJ - AnomJup))
      + 0.00058 * sin(W3 - W4)
      + 0.00056 * sin(2*(L2-L4))
      + 0.00055 * sin(2*(L1-L3))
      + 0.00052 * sin(3*L3 - 7*L4 + Pi3 + 3*Pi4)
      - 0.00043 * sin(L1 - Pi3)
      + 0.00042 * sin(Pi3 - Pi2)
      + 0.00041 * sin(5*(L2-L3))
      + 0.00041 * sin(Pi4 - LongPerJ)
      + 0.00038 * sin(L2 - Pi1)
      + 0.00032 * sin(W2 - W3)
      + 0.00032 * sin(2*(L3 - AnomJup - LongPerJ))
      + 0.00029 * sin(Pi1 - Pi3);

  S3 := 0.16477 * sin(L3 - Pi3)
      + 0.09062 * sin(L3 - Pi4)
      - 0.06907 * sin(L2 - L3)
      + 0.03786 * sin(Pi3 - Pi4)
      + 0.01844 * sin(2*(L3-L4))
      - 0.01340 * sin(AnomJup)
      + 0.00703 * sin(L2 - 2*L3 + Pi3)
      - 0.00670 * sin(2*(NodeJup - LongPerJ))
      - 0.00540 * sin(L3-L4)
      + 0.00481 * sin(Pi1 + Pi3 -2*LongPerJ - 2*AnomJup)
      - 0.00409 * sin(L2 - 2*L3 + Pi2)
      + 0.00379 * sin(L2 - 2*L3 + Pi4)
      + 0.00235 * sin(NodeJup - W3)
      + 0.00198 * sin(NodeJup - W4)
      + 0.00180 * sin(PhiLambda)
      + 0.00129 * sin(3*(L3-L4))
      + 0.00124 * sin(L1-L3)
      - 0.00119 * sin(5*AnomSat - 2*AnomJup + 52.225/radcor)
      + 0.00109 * sin(L1-L2)
      - 0.00099 * sin(3*L3 - 7*L4 + 4*Pi4)
      + 0.00091 * sin(W3 - W4)
      + 0.00081 * sin(3*L3 - 7*L4 + Pi3 + 3*Pi4)
      - 0.00076 * sin(2*L2 - 3*L3 + Pi3)
      + 0.00069 * sin(Pi4 - LongPerJ)
      - 0.00058 * sin(2*L3 - 3*L4 + Pi4)
      + 0.00057 * sin(L3 + Pi3 - 2*LongPerJ - 2*AnomJup)
      - 0.00057 * sin(L3 - 2*L4 + Pi4)
      - 0.00052 * sin(Pi2 - Pi3)
      - 0.00052 * sin(L2 - 2*L3 + Pi1)
      + 0.00048 * sin(L3 - 2*L4 + Pi3)
      - 0.00045 * sin(2*L2 - 3*L3 + Pi4)
      - 0.00041 * sin(Pi2 - Pi4)
      - 0.00038 * sin(2*AnomJup)
      - 0.00033 * sin(Pi3 - Pi4 + W3 - W4)
      - 0.00032 * sin(3*L3 - 7*L4 + 2*Pi3 + 2*Pi4)
      + 0.00030 * sin(4*(L3-L4))
      - 0.00029 * sin(W3 + NodeJup - 2*LongPerJ - 2*AnomJup)
      + 0.00029 * sin(L3 + Pi4 - 2*LongPerJ - 2*AnomJup)
      + 0.00026 * sin(L3 - LongPerJ - AnomJup)
      + 0.00024 * sin(L2 - 3*L3 + 2*L4)
      + 0.00021 * sin(2*(L3 - LongPerJ - AnomJup))
      - 0.00021 * sin(L3 - Pi2)
      + 0.00017 * sin(2*(L3 - Pi3));

  S4 := 0.84109 * sin(L4 - Pi4)
      + 0.03429 * sin(Pi4 - Pi3)
      - 0.03305 * sin(2*(NodeJup - LongPerJ))
      - 0.03211 * sin(AnomJup)
      - 0.01860 * sin(L4 - Pi3)
      + 0.01182 * sin(NodeJup - W4)
      + 0.00622 * sin(L4 + Pi4 - 2*AnomJup - 2*LongPerJ)
      + 0.00385 * sin(2*(L4 - Pi4))
      - 0.00284 * sin(5*AnomSat - 2*AnomJup + 52.225/radcor)
      - 0.00233 * sin(2*(NodeJup - Pi4))
      - 0.00223 * sin(L3 - L4)
      - 0.00208 * sin(L4 - LongPerJ)
      + 0.00177 * sin(NodeJup + W4 - 2*Pi4)
      + 0.00134 * sin(Pi4 - LongPerJ)
      + 0.00125 * sin(2*(L4 - AnomJup - LongPerJ))
      - 0.00117 * sin(2*AnomJup)
      - 0.00112 * sin(2*(L3 - L4))
      + 0.00106 * sin(3*L3 - 7*L4 + 4*Pi4)
      + 0.00102 * sin(L4 - AnomJup - LongPerJ)
      + 0.00096 * sin(2*L4 - NodeJup - W4)
      + 0.00087 * sin(2*(NodeJup - W4))
      - 0.00087 * sin(3*L3 - 7*L4 + Pi3 + 3*Pi4)
      + 0.00085 * sin(L3 - 2*L4 + Pi4)
      - 0.00081 * sin(2*(L4 - NodeJup))
      + 0.00071 * sin(L4 + Pi4 -2*LongPerJ - 3*AnomJup)
      + 0.00060 * sin(L1 - L4)
      - 0.00056 * sin(NodeJup - W3)
      - 0.00055 * sin(L3 - 2*L4 + Pi3)
      + 0.00051 * sin(L2 - L4)
      + 0.00042 * sin(2*(NodeJup - AnomJup - LongPerJ))
      + 0.00039 * sin(2*(Pi4 - W4))
      + 0.00036 * sin(NodeJup + LongPerJ - Pi4 - W4)
      + 0.00035 * sin(2*AnomSat - AnomJup + 188.37/radcor)
      - 0.00035 * sin(L4 - Pi4 + 2*LongPerJ - 2*NodeJup)
      - 0.00032 * sin(L4 + Pi4 - 2*LongPerJ - AnomJup)
      + 0.00030 * sin(3*L3 - 7*L4 + 2*Pi3 + 2*Pi4)
      + 0.00030 * sin(2*AnomSat - 2*AnomJup + 149.15/radcor)
      + 0.00028 * sin(L4 - Pi4 + 2*NodeJup - 2*LongPerJ)
      - 0.00028 * sin(2*(L4 - W4))
      - 0.00027 * sin(Pi3 - Pi4 + W3 - W4)
      - 0.00026 * sin(5*AnomSat - 3*AnomJup + 188.37/radcor)
      + 0.00025 * sin(W4 - W3)
      - 0.00025 * sin(L2 - 3*L3 + 2*L4)
      - 0.00023 * sin(3*(L3 - L4))
      + 0.00021 * sin(2*L4 - 2*LongPerJ - 3*AnomJup)
      - 0.00021 * sin(2*L3 - 3*L4 + Pi4)
      + 0.00019 * sin(L4 - Pi4 - AnomJup)
      - 0.00019 * sin(2*L4 - Pi3 - Pi4)
      - 0.00018 * sin(L4 - Pi4 + AnomJup)
      - 0.00016 * sin(L4 + Pi3 -2*LongPerJ - 2*AnomJup);

  S1 := S1/radcor;
  S2 := S2/radcor;
  S3 := S3/radcor;
  S4 := S4/radcor;

  TL1 := L1 + S1;
  TL2 := L2 + S2;
  TL3 := L3 + S3;
  TL4 := L4 + S4;

  B1 := 0.0006502 * sin(TL1 - W1)
      + 0.0001835 * sin(TL1 - W2)
      + 0.0000329 * sin(TL1 - W3)
      - 0.0000311 * sin(TL1 - NodeJup)
      + 0.0000093 * sin(TL1 - W4)
      + 0.0000075 * sin(3*TL1 - 4*L2 - 1.9927/radcor * S1 + W2)
      + 0.0000046 * sin(TL1 + NodeJup - 2*LongPerJ - 2*AnomJup);
  B1 := ArcTan(B1);

  B2 := 0.0081275 * sin(TL2 - W2)
      + 0.0004512 * sin(TL2 - W3)
      - 0.0003286 * sin(TL2 - NodeJup)
      + 0.0001164 * sin(TL2 - W4)
      + 0.0000273 * sin(L1 - 2*L3 + 1.0146/radcor * S2 + W2)
      + 0.0000143 * sin(TL2 + NodeJup - 2*LongPerJ - 2*AnomJup)
      - 0.0000143 * sin(TL2 - W1)
      + 0.0000035 * sin(TL2 - NodeJup + AnomJup)
      - 0.0000028 * sin(L1 - 2*L3 + 1.0146/radcor * S2 + W3);
  B2 := ArcTan(B2);

  B3 := 0.0032364 * sin(TL3 - W3)
      - 0.0016911 * sin(TL3 - NodeJup)
      + 0.0006849 * sin(TL3 - W4)
      - 0.0002806 * sin(TL3 - W2)
      + 0.0000321 * sin(TL3 + NodeJup - 2*LongPerJ - 2*AnomJup)
      + 0.0000051 * sin(TL3 - NodeJup + AnomJup)
      - 0.0000045 * sin(TL3 - NodeJup - AnomJup)
      - 0.0000045 * sin(TL3 + NodeJup - 2*LongPerJ)
      + 0.0000037 * sin(TL3 + NodeJup - 2*LongPerJ - 3*AnomJup)
      + 0.0000030 * sin(2*L2 - 3*TL3 + 4.03/radcor * S3 + W2)
      - 0.0000021 * sin(2*L2 - 3*TL3 + 4.03/radcor * S3 + W3);
  B3 := ArcTan(B3);

  B4 := -0.0076579 * sin(TL4 - NodeJup)
      + 0.0044148 * sin(TL4 - W4)
      - 0.0005106 * sin(TL4 - W3)
      + 0.0000773 * sin(TL4 + NodeJup - 2*LongPerJ - 2*AnomJup)
      + 0.0000104 * sin(TL4 - NodeJup + AnomJup)
      - 0.0000102 * sin(TL4 - NodeJup - AnomJup)
      + 0.0000088 * sin(TL4 + NodeJup - 2*LongPerJ - 3*AnomJup)
      - 0.0000038 * sin(TL4 + NodeJup - 2*LongPerJ - AnomJup);
  B4 := ArcTan(B4);

  R1 := -0.0041339 * cos(2*(L1-L2))
      - 0.0000395 * cos(L1 - Pi3)
      - 0.0000214 * cos(L1 - Pi4)
      + 0.0000170 * cos(L1 - L2)
      - 0.0000162 * cos(L1 - Pi1)
      - 0.0000130 * cos(4*(L1-L2))
      + 0.0000106 * cos(L1 - L3)
      - 0.0000063 * cos(L1 + Pi3 - 2*LongPerJ - 2*AnomJup);

  R2 := 0.0093847 * cos(L1-L2)
      - 0.0003114 * cos(L2 - Pi3)
      - 0.0001738 * cos(L2 - Pi4)
      - 0.0000941 * cos(L2 - Pi2)
      + 0.0000553 * cos(L2 - L3)
      + 0.0000523 * cos(L1 - L3)
      - 0.0000290 * cos(2*(L1-L2))
      + 0.0000166 * cos(2*(L2-W2))
      + 0.0000107 * cos(L1 - 2*L3 + Pi3)
      - 0.0000102 * cos(L2 - Pi1)
      - 0.0000091 * cos(2*(L1-L3));

  R3 := -0.0014377 * cos(L3 - Pi3)
      - 0.0007904 * cos(L3 - Pi4)
      + 0.0006342 * cos(L2 - L3)
      - 0.0001758 * cos(2*(L3-L4))
      + 0.0000294 * cos(L3 - L4)
      - 0.0000156 * cos(3*(L3-L4))
      + 0.0000155 * cos(L1 - L3)
      - 0.0000153 * cos(L1 - L2)
      + 0.0000070 * cos(2*L2 - 3*L3 + Pi3)
      - 0.0000051 * cos(L3 + Pi3 - 2*LongPerJ - 2*AnomJup);

  R4 := -0.0073391 * cos(L4 - Pi4)
      + 0.0001620 * cos(L4 - Pi3)
      + 0.0000974 * cos(L3 - L4)
      - 0.0000541 * cos(L4 + Pi4 - 2*LongPerJ - 2*AnomJup)
      - 0.0000269 * cos(2*(L4-Pi4))
      + 0.0000182 * cos(L4- LongPerJ)
      + 0.0000177 * cos(2*(L3-L4))
      - 0.0000167 * cos(2*L4 - NodeJup - W4)
      + 0.0000167 * cos(NodeJup - W4)
      - 0.0000155 * cos(2*(L4-LongPerj-AnomJup))
      + 0.0000142 * cos(2*(L4-NodeJup))
      + 0.0000104 * cos(L1 - L4)
      + 0.0000092 * cos(L2 - L4)
      - 0.0000089 * cos(L4 - LongPerJ - AnomJup)
      - 0.0000062 * cos(L4 + Pi4 - 2*LongPerJ - 3*AnomJup)
      + 0.0000048 * cos(2*(L4-W4));

  R1 :=  5.90730 * (1 + R1);
  R2 :=  9.39912 * (1 + R2);
  R3 := 14.99240 * (1 + R3);
  R4 := 26.36990 * (1 + R4);

  T0 := (AJD - 2433282.423) / 36525;
  Precession := (1.3966626*T0 + 0.0003088*sqr(T0)) / radcor;

  TL1 := TL1 + Precession;
  TL2 := TL2 + Precession;
  TL3 := TL3 + Precession;
  TL4 := TL4 + Precession;
  NodeJup := NodeJup + Precession;

  T0 := (AJD - AstJulianDatePrim(1900, 1, 1, 0)) / 36525;
  Inclination := (3.120262 + 0.0006*T0) / radcor;

  SatX[1] := R1 * cos(TL1 - NodeJup) * cos(B1);
  SatY[1] := R1 * sin(TL1 - NodeJup) * cos(B1);
  SatZ[1] := R1 * sin(B1);

  SatX[2] := R2 * cos(TL2 - NodeJup) * cos(B2);
  SatY[2] := R2 * sin(TL2 - NodeJup) * cos(B2);
  SatZ[2] := R2 * sin(B2);

  SatX[3] := R3 * cos(TL3 - NodeJup) * cos(B3);
  SatY[3] := R3 * sin(TL3 - NodeJup) * cos(B3);
  SatZ[3] := R3 * sin(B3);

  SatX[4] := R4 * cos(TL4 - NodeJup) * cos(B4);
  SatY[4] := R4 * sin(TL4 - NodeJup) * cos(B4);
  SatZ[4] := R4 * sin(B4);

  SatX[5] := 0;
  SatY[5] := 0;
  SatZ[5] := 1;

  T0 := (AJD -  2451545.0) / 36525.0;
  TD1 := 100.464441
       +   1.0209550 * T0
       +   0.00040117 * sqr(T0)
       +   0.000000569 * sqr(T0) * T0;
  TD1 := TD1 / radcor;

  TD2 := 1.303270
       - 0.0054966 * T0
       + 0.00000465 * sqr(T0)
       - 0.000000004 * sqr(T0) * T0;
  TD2 := TD2 / radcor;

  for I := 1 to 5 do begin
    Transforms[I].A[1] := SatX[I];
    Transforms[I].B[1] := SatY[I] * cos(Inclination)
                        - SatZ[I] * sin(Inclination);
    Transforms[I].C[1] := SatY[I] * sin(Inclination)
                        + SatZ[I] * cos(Inclination);

    Transforms[I].A[2] := Transforms[I].A[1] * cos(NodeJup - TD1)
                        - Transforms[I].B[1] * sin(NodeJup - TD1);
    Transforms[I].B[2] := Transforms[I].A[1] * sin(NodeJup - TD1)
                        + Transforms[I].B[1] * cos(NodeJup - TD1);
    Transforms[I].C[2] := Transforms[I].C[1];

    Transforms[I].A[3] := Transforms[I].A[2];
    Transforms[I].B[3] := Transforms[I].B[2] * cos(TD2)
                        - Transforms[I].C[2] * sin(TD2);
    Transforms[I].C[3] := Transforms[I].B[2] * sin(TD2)
                        + Transforms[I].C[2] * cos(TD2);

    Transforms[I].A[4] := Transforms[I].A[3] * cos(TD1)
                        - Transforms[I].B[3] * sin(TD1);
    Transforms[I].B[4] := Transforms[I].A[3] * sin(TD1)
                        + Transforms[I].B[3] * cos(TD1);
    Transforms[I].C[4] := Transforms[I].C[3];

    Transforms[I].A[5] := Transforms[I].A[4] * sin(Jup1)
                        - Transforms[I].B[4] * cos(Jup1);
    Transforms[I].B[5] := Transforms[I].A[4] * cos(Jup1)
                        + Transforms[I].B[4] * sin(Jup1);
    Transforms[I].C[5] := Transforms[I].C[4];

    Transforms[I].A[6] := Transforms[I].A[5];
    Transforms[I].B[6] := Transforms[I].C[5] * sin(Jup2)
                        + Transforms[I].B[5] * cos(Jup2);
    Transforms[I].C[6] := Transforms[I].C[5] * cos(Jup2)
                        - Transforms[I].B[5] * sin(Jup2);
  end;

  Angle := StInvTan2(Transforms[5].C[6], Transforms[5].A[6]);

{Io calculations}
  Result.Io.X := Transforms[1].A[6] * cos(Angle)
               - Transforms[1].C[6] * sin(Angle);
  Result.Io.Y := Transforms[1].A[6] * sin(Angle)
               + Transforms[1].C[6] * cos(Angle);
  TD1 := Transforms[1].B[6];

  {correct for light time}
  TD2 := abs(TD1) / 17295 * sqrt(1 - sqr(Result.Io.X/R1));
  Result.Io.X := Result.Io.X + TD2;

  {correct for perspective}
  TD2 := EJDist / (EJDist + TD1/2095);
  Result.Io.X := Result.Io.X * TD2;
  Result.Io.Y := Result.Io.Y * TD2;

{Europa calculations}
  Result.Europa.X := Transforms[2].A[6] * cos(Angle)
                     - Transforms[2].C[6] * sin(Angle);
  Result.Europa.Y := Transforms[2].A[6] * sin(Angle)
                     + Transforms[2].C[6] * cos(Angle);
  TD1 := Transforms[2].B[6];

  {correct for light time}
  TD2 := abs(TD1) / 21819 * sqrt(1 - sqr(Result.Europa.X/R2));
  Result.Europa.X := Result.Europa.X + TD2;

  {correct for perspective}
  TD2 := EJDist / (EJDist + TD1/2095);
  Result.Europa.X := Result.Europa.X * TD2;
  Result.Europa.Y := Result.Europa.Y * TD2;

{Ganymede calculations}
  Result.Ganymede.X := Transforms[3].A[6] * cos(Angle)
                   - Transforms[3].C[6] * sin(Angle);
  Result.Ganymede.Y := Transforms[3].A[6] * sin(Angle)
                   + Transforms[3].C[6] * cos(Angle);
  TD1 := Transforms[3].B[6];

  {correct for light time}
  TD2 := abs(TD1) / 27558 * sqrt(1 - sqr(Result.Ganymede.X/R3));
  Result.Ganymede.X := Result.Ganymede.X + TD2;

  {correct for perspective}
  TD2 := EJDist / (EJDist + TD1/2095);
  Result.Ganymede.X := Result.Ganymede.X * TD2;
  Result.Ganymede.Y := Result.Ganymede.Y * TD2;

{Callisto calculations}
  Result.Callisto.X := Transforms[4].A[6] * cos(Angle)
                     - Transforms[4].C[6] * sin(Angle);
  Result.Callisto.Y := Transforms[4].A[6] * sin(Angle)
                     + Transforms[4].C[6] * cos(Angle);
  TD1 := Transforms[4].B[6];

  {correct for light time}
  TD2 := abs(TD1) / 36548 * sqrt(1 - sqr(Result.Callisto.X/R4));
  Result.Callisto.X := Result.Callisto.X + TD2;

  {correct for perspective}
  TD2 := EJDist / (EJDist + TD1/2095);
  Result.Callisto.X := Result.Callisto.X * TD2;
  Result.Callisto.Y := Result.Callisto.Y * TD2;
end;

{-------------------------------------------------------------------------}

function GetJupSats(JD : TDateTime; HighPrecision, Shadows : Boolean) : TStJupSats;
begin
  if not HighPrecision then
    Result := JupSatsLo(JD)
  else
    Result := JupSatsHi(JD, Shadows);
end;

end.
