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
{* SysTools: StAstroP.pas 4.04                           *}
{*********************************************************}
{* SysTools: Astronomical Routines (general Planetary)   *}
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
{   4. MPO96-MPO98 source files, Brian D. Warner, 1995-2000.     }
{                                                                }
{ ************************************************************** }

unit StAstroP;

interface

const
  StdDate = 2451545.0;            {Ast. Julian Date for J2000 Epoch}
  OB2000  = 0.409092804;          {J2000 obliquity of the ecliptic (radians)}

type
  TStEclipticalCord = packed record
    L0,
    B0,
    R0   : Double;
  end;

  TStRectangularCord = packed record
    X,
    Y,
    Z   : Double;
  end;

  TStPlanetsRec = packed record
    RA,
    DC,
    Elong  : Double;
  end;
  TStPlanetsArray = array[1..8] of TStPlanetsRec;


procedure PlanetsPos(JD : Double; var PA : TStPlanetsArray);


implementation

uses
  Windows,
  StDate, StMerc, StVenus, StMars, StJup, StSaturn, StUranus, StNeptun,
  StPluto, StMath;

var
  PlanEC   : TStEclipticalCord;
  PlanRC,
  SunRC    : TStRectangularCord;
  SunEQ    : TStPlanetsRec;


{--------------------------------------------------------------------------}

function RealAngle(Value2, Value1, Start : Double) : Double;
begin
  Result := Start;

  if (Value1 = 0) then begin
    if Value2 > 0 then
      Result := Pi / 2.0
    else
      Result := 3.0 * Pi / 2.0;
  end else begin
    if (Value2 > 0.0) then begin
      if (Value1 < 0.0) then
        Result := Start + Pi
      else
        Result := Start;
    end else begin
      if (Value2 = 0) then begin
        if Value1 > 0 then
          Result := 0
        else
          Result := Pi;
      end else begin
        if (Value2 < 0) then begin
          if (Value1 < 0) then
            Result := Start + Pi
          else
            Result := Start + (2.0 * Pi)
        end;
      end;
    end;
  end;
end;

{--------------------------------------------------------------------------}

function SunOfDate(JD : Double) : TStRectangularCord;
  {-compute J2000 XYZ coordinates of the Sun}
var
  T0,
  A,
  L,
  B,
  RV,
  TX,
  TY,
  TZ  : Double;

begin
  T0 := (JD - StdDate) / 365250;

{solar longitude}
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

  A := 628307584999.0
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

  A := 8722 * cos(1.0725 +  6283.0758*T0)
     +  991 * cos(3.1416)
     +  295 * cos(0.437  + 12566.1520*T0)
     +   27 * cos(0.050  +     3.52*T0)
     +   16 * cos(5.190  +    26.30*T0)
     +   16 * cos(3.69   +   155.42*T0)
     +    9 * cos(0.30   + 18849.23*T0)
     +    9 * cos(2.06   + 77713.77*T0);
  L := L + (A * sqr(T0));

  A := 289 * cos(5.842 +  6283.076*T0)
     +  21 * cos(6.05  + 12566.15*T0)
     +   3 * cos(5.20  +   155.42*T0)
     +   3 * cos(3.14);
  L := L + (A * sqr(T0) * T0);
  L := L / 1.0E+8;


{solar latitude}
  B := 280 * cos(3.199 + 84334.662*T0)
     + 102 * cos(5.422 +  5507.553*T0)
     +  80 * cos(3.88  +  5223.69*T0)
     +  44 * cos(3.70  +  2352.87*T0)
     +  32 * cos(4.00  +  1577.34*T0);
  B := B / 1.0E+8;

  A := 227778 * cos(3.413766 + 6283.07585*T0)
     +   3806 * cos(3.3706 + 12566.1517*T0)
     +   3620
     +     72 * cos(3.33 + 18849.23*T0)
     +      8 * cos(3.89 +  5507.55*T0)
     +      8 * cos(1.79 +  5223.69*T0)
     +      6 * cos(5.20 +  2352.87*T0);
  B := B + (A * T0 / 1.0E+8);

  A := 9721 * cos(5.1519 + 6283.07585*T0)
     +  233 * cos(3.1416)
     +  134 * cos(0.644  + 12566.152*T0)
     +    7 * cos(1.07   + 18849.23*T0);
  B := B + (A * sqr(T0) / 1.0E+8);

  A := 276 * cos(0.595 + 6283.076*T0)
     +  17 * cos(3.14)
     +   4 * cos(0.12  + 12566.15*T0);
  B := B + (A * sqr(T0) * T0 / 1.0E+8);


{solar radius vector (astronomical units)}
  RV := 100013989
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
  RV := RV / 1.0E+8;

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
  RV := RV + (A * T0 / 1.0E+8);

  A := 4359 * cos(5.7846 +  6283.0758*T0)
     +  124 * cos(5.579  + 12566.152*T0)
     +   12 * cos(3.14)
     +    9 * cos(3.63   + 77713.77*T0)
     +    6 * cos(1.87   +  5573.14*T0)
     +    3 * cos(5.47   + 18849.23*T0);
  RV := RV + (A * sqr(T0) / 1.0E+8);

  L := (L + PI);
  L := Frac(L / 2.0 / PI) * 2.0 * Pi;
  if L < 0 then
    L := L + (2.0*PI);
  B := -B;

  TX := RV * cos(B) * cos(L);
  TY := RV * cos(B) * sin(L);
  TZ := RV * sin(B);

  Result.X :=               TX +     4.40360E-7 * TY - 1.90919E-7 * TZ;
  Result.Y := -4.79966E-7 * TX + 0.917482137087 * TY - 0.397776982902 * TZ;
  Result.Z :=                    0.397776982902 * TY + 0.917482137087 * TZ;
end;

{--------------------------------------------------------------------------}

function EclipticToRectangular(Longitude, Latitude,
                               RadiusVector : Double) : TStRectangularCord;
var
  var1,
  var2,
  var3 : Double;
begin
  var1 := RadiusVector * cos(Longitude) * cos(Latitude);
  var2 := RadiusVector * sin(Longitude) * cos(Latitude);
  var3 := RadiusVector * sin(Latitude);

  Result.X := var1;
  Result.Y := var2 * cos(OB2000) - var3 * sin(OB2000);
  Result.Z := var2 * sin(OB2000) + var3 * cos(OB2000);
end;

{--------------------------------------------------------------------------}

function RADec(Planet, Sun : TStRectangularCord;
               ComputeElong : Boolean) : TStPlanetsRec;
var
  var1,
  var2,
  var3,
  var4,
  var5       : Double;
begin
  FillChar(Result, SizeOf(TStPlanetsRec), #0);

  var1 := Sun.X + Planet.X;
  var2 := Sun.Y + Planet.Y;
  var3 := Sun.Z + Planet.Z;

  var4 := arctan(var2/var1);
  var4 := RealAngle(var2, var1, var4) * radcor;

  var5 := sqrt(sqr(var1) + sqr(var2) + sqr(var3));
  var3 := StInvsin(var3/var5) * radcor;

  Result.RA := var4;
  Result.DC := var3;

  var4 := Result.RA / radcor;
  var3 := Result.DC / radcor;

  if (ComputeElong) then begin
    var1 := sin(SunEQ.DC/radcor) * sin(var3);
    var2 := cos(SunEQ.DC/radcor) * cos(var3) * cos(SunEQ.RA/radcor - var4);
    Result.Elong := StInvcos(var1+var2) * radcor;
  end;
end;

{--------------------------------------------------------------------------}

function MercuryPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeMercury(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function VenusPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeVenus(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function MarsPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeMars(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function JupiterPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeJupiter(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function SaturnPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeSaturn(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function UranusPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeUranus(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function NeptunePosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputeNeptune(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

function PlutoPosition(JD : Double) : TStPlanetsRec;
begin
  PlanEC := ComputePluto(JD);
  PlanRC := EclipticToRectangular(PlanEC.L0, PlanEC.B0, PlanEC.R0);
  Result := RADec(PlanRC, SunRC, True);
end;

{--------------------------------------------------------------------------}

procedure PlanetsPos(JD : Double; var PA : TStPlanetsArray);
var
  I   : Integer;
  Sun : TStRectangularCord;
begin
  {find Sun's Rectangular Coordinates}
  SunRC := SunofDate(JD);

  FillChar(SunEQ, SizeOf(TStPlanetsRec), #0);
  FillChar(Sun, SizeOf(TStRectangularCord), #0);

  {find Sun's RA/Dec}
  SunEQ := RADec(SunRC, Sun, False);
  PA[1] := PlutoPosition(JD);

  {find RA/Dec of each planet}
  for I := 1 to 8 do begin
    case I of
      1 : PA[I] := MercuryPosition(JD);
      2 : PA[I] := VenusPosition(JD);
      3 : PA[I] := MarsPosition(JD);
      4 : PA[I] := JupiterPosition(JD);
      5 : PA[I] := SaturnPosition(JD);
      6 : PA[I] := UranusPosition(JD);
      7 : PA[I] := NeptunePosition(JD);
      8 : PA[I] := PlutoPosition(JD);
    end;
  end;
end;


end.
