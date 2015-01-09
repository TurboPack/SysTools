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
{* SysTools: StAstro.pas 4.04                            *}
{*********************************************************}
{* SysTools: Astronomical Routines                       *}
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
{   4. MPO96 source files, Brian D. Warner, 1995.                }
{                                                                }
{ ************************************************************** }

unit StAstro;

interface

uses
  Windows, SysUtils,
  StConst, StBase, StDate, StStrS, StDateSt, StMath;

type
  TStTwilight = (ttCivil, ttNautical, ttAstronomical);

  TStRiseSetRec = record
    ORise : TStTime;
    OSet  : TStTime;
  end;

  TStPosRec = record
    RA,
    DC : Double;
  end;
  TStPosRecArray = array[1..3] of TStPosRec;

  TStSunXYZRec = record
    SunX,
    SunY,
    SunZ,
    RV,
    SLong,
    SLat   : Double;
  end;

  TStLunarRecord = record
    T : array[0..1] of TStDateTimeRec;
  end;

  TStPhaseRecord = packed record
    NMDate,
    FQDate,
    FMDate,
    LQDate  : Double;
  end;
  TStPhaseArray = array[0..13] of TStPhaseRecord;

  TStDLSDateRec = record
    Starts : TStDate;
    Ends   : TStDate;
  end;

  TStMoonPosRec = record
    RA,
    DC,
    Phase,
    Dia,
    Plx,
    Elong   : Double;
  end;


const
  radcor  = 57.29577951308232;    {number of degrees in a radian}
  StdDate = 2451545.0;            {Ast. Julian Date for J2000 Epoch}
  OB2000  = 0.409092804;          {J2000 obliquity of the ecliptic (radians)}


{Sun procedures/functions}
function AmountOfSunlight(LD : TStDate; Longitude, Latitude : Double) : TStTime;
  {-compute the hours, min, sec of sunlight on a given date}
function FixedRiseSet(LD : TStDate; RA, DC, Longitude, Latitude : Double) : TStRiseSetRec;
  {-compute the rise and set time for a fixed object, e.g., a star}
function SunPos(UT : TStDateTimeRec) : TStPosRec;
  {-compute the J2000 RA/Declination of the Sun}
function SunPosPrim(UT : TStDateTimeRec) : TStSunXYZRec;
  {-compute the J2000 geocentric rectangular & ecliptic coordinates of the sun}
function SunRiseSet(LD : TStDate; Longitude, Latitude : Double) : TStRiseSetRec;
  {-compute the Sun rise or set time}
function Twilight(LD : TStDate; Longitude, Latitude : Double; TwiType : TStTwilight) : TStRiseSetRec;
  {-compute the beginning and end of twilight (civil, nautical, or astron.)}

{Lunar procedures/functions}
function LunarPhase(UT : TStDateTimeRec) : Double;
  {-compute the phase of the moon}
function MoonPos(UT : TStDateTimeRec) : TStMoonPosRec;
  {-compute the J2000 RA/Declination of the moon}
function MoonRiseSet(LD : TStDate; Longitude, Latitude : Double) : TStRiseSetRec;
  {-compute the Moon rise and set time}
function FirstQuarter(D : TStDate) : TStLunarRecord;
  {-compute date/time of FirstQuarter(s)}
function FullMoon(D : TStDate) : TStLunarRecord;
  {-compute the date/time of FullMoon(s)}
function LastQuarter(D : TStDate) : TStLunarRecord;
  {-compute the date/time of LastQuarter(s)}
function NewMoon(D : TStDate) : TStLunarRecord;
  {-compute the date/time of NewMoon(s)}
function NextFirstQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest FirstQuarter}
function NextFullMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest FullMoon}
function NextLastQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest LastQuarter}
function NextNewMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest NewMoon}
function PrevFirstQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest FirstQuarter}
function PrevFullMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest FullMoon}
function PrevLastQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest LastQuarter}
function PrevNewMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest NewMoon}

{Calendar procedures/functions}
function SiderealTime(UT : TStDateTimeRec) : Double;
  {-compute Sidereal Time at Greenwich}
function Solstice(Y, Epoch : Integer; Summer : Boolean) : TStDateTimeRec;
  {-compute the date/time of the summer or winter solstice}
function Equinox(Y, Epoch : Integer; Vernal : Boolean) : TStDateTimeRec;
  {-compute the date/time of the vernal/autumnal equinox}
function Easter(Y, Epoch : Integer) : TStDate;
  {-compute the date of Easter (astronmical calendar)}

{Astronomical Julian Date Conversions}
function DateTimeToAJD(D : TDateTime) : Double;

{Conversion routines}
function HoursMin(RA : Double) : String;
  {-convert RA to hh:mm:ss string}
function DegsMin(DC : Double) : String;
  {-convert Declination to +/-dd:mm:ss string}
function AJDToDateTime(D : Double) : TDateTime;


implementation

var
  AJDOffset : Double;

function CheckDate(UT : TStDateTimeRec) : Boolean;
begin
  with UT do begin
    if (D < MinDate) or (D > MaxDate) or
       (T < 0) or (T > MaxTime) then
      Result := False
    else
      Result := True;
  end;
end;

function CheckYear(Y, Epoch : Integer) : Integer;
begin
  if Y < 100 then begin
    if Y >= (Epoch mod 100) then
      Result := ((Epoch div 100) * 100) + Y
    else
      Result := ((Epoch div 100) * 100) + 100 + Y;
  end else
    Result := Y;
end;

function SiderealTime(UT : TStDateTimeRec) : Double;
  {-compute Sidereal Time at Greenwich in degrees}
var
  T,
  JD : Double;
begin
  if not CheckDate(UT) then begin
    Result := -1;
    Exit;
  end;

  JD := AstJulianDate(UT.D) + UT.T/86400;

  T := (JD - 2451545.0) / 36525.0;

  Result := 280.46061837
          + 360.98564736629 * (JD - 2451545.0)
          + 0.000387933 * sqr(T)
          - (sqr(T) * T / 38710000);
  Result := Frac(Result/360.0) * 360.0;
  if Result < 0 then
    Result := 360 + Result;
end;

function SunPosPrim(UT : TStDateTimeRec) : TStSunXYZRec;
  {-compute J2000 XYZ coordinates of the Sun}
var
  JD,
  T0,
  A,
  L,
  B,
  X,Y,Z  : Double;

begin
  if not CheckDate(UT) then begin
    with Result do begin
      SunX := -99;
      SunY := -99;
      SunZ := -99;
      RV := -99;
      SLong := -99;
      SLat := -99;
    end;
    Exit;
  end;

  JD := AstJulianDate(UT.D) + UT.T/86400;
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
  Result.RV := 100013989
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
  Result.RV := Result.RV / 1.0E+8;

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
  Result.RV := Result.RV + (A * T0 / 1.0E+8);

  A := 4359 * cos(5.7846 +  6283.0758*T0)
     +  124 * cos(5.579  + 12566.152*T0)
     +   12 * cos(3.14)
     +    9 * cos(3.63   + 77713.77*T0)
     +    6 * cos(1.87   +  5573.14*T0)
     +    3 * cos(5.47   + 18849.23*T0);
  Result.RV := Result.RV + (A * sqr(T0) / 1.0E+8);

  L := (L + PI);
  L := Frac(L / 2.0 / PI) * 2.0 * Pi;
  if L < 0 then
    L := L + (2.0*PI);
  B := -B;

  Result.SLong := L * radcor;
  Result.SLat  := B * radcor;

  X := Result.RV * cos(B) * cos(L);
  Y := Result.RV * cos(B) * sin(L);
  Z := Result.RV * sin(B);

  Result.SunX :=               X +     4.40360E-7 * Y - 1.90919E-7 * Z;
  Result.SunY := -4.79966E-7 * X + 0.917482137087 * Y - 0.397776982902 * Z;
  Result.SunZ :=                   0.397776982902 * Y + 0.917482137087 * Z;
end;

function MoonPosPrim(UT : TStDateTimeRec) : TStMoonPosRec;
  {-computes J2000 coordinates of the moon}
var
  JD,
  TD,
  JCent,
  JC2, JC3, JC4,
  LP, D,
  M, MP,
  F, I,
  A1,A2,A3,
  MoonLong,
  MoonLat,
  MoonDst,
  S1,C1,
  SunRA,
  SunDC,
  EE,EES      : Double;

  SP : TStSunXYZRec;

begin
  JD := AstJulianDate(UT.D) + UT.T/86400;
  JCent := (JD - 2451545) / 36525;
  JC2 := sqr(JCent);
  JC3 := JC2 * JCent;
  JC4 := sqr(JC2);

  SP := SunPosPrim(UT);
  SunRA := StInvTan2(SP.SunX, SP.SunY) * radcor;
  SunDC := StInvSin(SP.SunZ / SP.RV) * radcor;


{Lunar mean longitude}
  LP := 218.3164591 + (481267.88134236 * JCent)
      - (1.3268E-3 * JC2) + (JC3 / 538841) - (JC4 / 65194000);
  LP := Frac(LP/360) * 360;
  if LP < 0 then
      LP := LP + 360;
  LP := LP/radcor;

{Lunar mean elongation}
  D  := 297.8502042 + (445267.1115168 * JCent)
        - (1.63E-3 * JC2) + (JC3 / 545868) - (JC4 / 113065000);
  D := Frac(D/360) * 360;
  if D < 0 then
      D := D + 360;
  D := D/radcor;

{Solar mean anomaly}
  M  := 357.5291092 + (35999.0502909 * JCent)
        - (1.536E-4 * JC2) + (JC3 / 24490000);
  M := Frac(M/360) * 360;
  if M < 0 then
      M := M + 360;
  M := M/radcor;

{Lunar mean anomaly}
  MP := 134.9634114 + (477198.8676313 * JCent)
        + (8.997E-3 * JC2) + (JC3 / 69699) - (JC4 / 14712000);
  MP := Frac(MP/360) * 360;
  if MP < 0 then
      MP := MP + 360;
  MP := MP/radcor;

{Lunar argument of latitude}
  F  := 93.2720993 + (483202.0175273 * JCent)
        - (3.4029E-3 * JC2) - (JC3 / 3526000) + (JC4 / 863310000);
  F := Frac(F/360) * 360;
  if F < 0 then
      F := F + 360;
  F := F/radcor;


{Other required arguments}
  A1 := 119.75 + (131.849 * JCent);
  A1 := Frac(A1/360) * 360;
  if A1 < 0 then
      A1 := A1 + 360;
  A1 := A1/radcor;

  A2 :=  53.09 + (479264.290 * JCent);
  A2 := Frac(A2/360) * 360;
  if A2 < 0 then
      A2 := A2 + 360;
  A2 := A2/radcor;

  A3 := 313.45 + (481266.484 * JCent);
  A3 := Frac(A3/360) * 360;
  if A3 < 0 then
      A3 := A3 + 360;
  A3 := A3/radcor;

{Earth's orbital eccentricity}
  EE  := 1.0 - (2.516E-3 * JCent) - (7.4E-6 * JC2);
  EES := sqr(EE);

  MoonLong :=   6288774 * sin(MP)
              + 1274027 * sin(2*D - MP)
              +  658314 * sin(2*D)
              +  213618 * sin(2*MP)
              -  185116 * sin(M) * EE
              -  114332 * sin(2*F)
              +   58793 * sin(2*(D-MP))
              +   57066 * sin(2*D-M-MP) * EE
              +   53322 * sin(2*D-MP)
              +   45758 * sin(2*D-M) * EE
              -   40923 * sin(M-MP) * EE
              -   34720 * sin(D)
              -   30383 * sin(M+MP) * EE
              +   15327 * sin(2*(D-F))
              -   12528 * sin(MP+2*F)
              +   10980 * sin(MP-2*F)
              +   10675 * sin(4*D-MP)
              +   10034 * sin(3*MP)
              +    8548 * sin(4*D-2*MP)
              -    7888 * sin(2*D+M-MP) * EE
              -    6766 * sin(2*D+M) * EE
              -    5163 * sin(D-MP)
              +    4987 * sin(D+M) * EE
              +    4036 * sin(2*D-M+MP) * EE
              +    3994 * sin(2*(D+MP))
              +    3861 * sin(4*D)
              +    3665 * sin(2*D-3*MP)
              -    2689 * sin(M-2*MP) * EE
              -    2602 * sin(2*D-MP+2*F)
              +    2390 * sin(2*D-M-2*MP) * EE
              -    2348 * sin(D-MP)
              +    2236 * sin(2*(D-M)) * EES
              -    2120 * sin(M-2*MP) * EE
              -    2069 * sin(2*M) * EE
              +    2048 * sin(2*D-2*M-MP) * EES
              -    1773 * sin(2*D+MP-2*F)
              -    1595 * sin(2*(D+F))
              +    1215 * sin(4*D-M-MP) * EE
              -    1110 * sin(2*(MP+F))
              -     892 * sin(3*D-MP)
              -     810 * sin(2*D-M-MP) * EE
              +     759 * sin(4*D-M-2*MP) * EE
              -     713 * sin(2*M-MP) * EE
              -     700 * sin(2*D+2*M-MP) * EES
              +     691 * sin(2*D+M-2*MP) * EE
              +     596 * sin(2*D-M-2*F) * EE
              +     549 * sin(4*D+MP)
              +     537 * sin(4*MP)
              +     520 * sin(4*D-M) * EE;

  MoonDst := - 20905355 * cos(MP)
             -  3699111 * cos(2*D - MP)
             -  2955968 * cos(2*D)
             -   569925 * cos(2*MP)
             +    48888 * cos(M) * EE
             -     3149 * cos(2*F)
             +   246158 * cos(2*(D-MP))
             -   152138 * cos(2*D-M-MP) * EE
             -   170733 * cos(2*D-MP)
             -   204586 * cos(2*D-M) * EE
             -   129620 * cos(M-MP) * EE
             +   108743 * cos(D)
             +   104755 * cos(M-MP) * EE
             +    10321 * cos(2*D-2*F)
             +    79661 * cos(MP-2*F)
             -    34782 * cos(4*D-MP)
             -    23210 * cos(3*MP)
             -    21636 * cos(4*D-2*MP)
             +    24208 * cos(2*D+M-MP) * EE
             +    30824 * cos(2*D-M) * EE
             -     8379 * cos(D-MP)
             -    16675 * cos(D+M) * EE
             -    12831 * cos(2*D-M+MP) * EE
             -    10445 * cos(2*D+2*MP)
             -    11650 * cos(4*D) * EE
             +    14403 * cos(2*D+3*MP)
             -     7003 * cos(M-2*MP) * EE
             +    10056 * cos(2*D-M-2*MP) * EE
             +     6322 * cos(D+MP)
             -     9884 * cos(2*D-2*M) * EES
             +     5751 * cos(M+2*MP) * EE
             -     4950 * cos(2*D-2*M-MP) * EES
             +     4130 * cos(2*D+MP+2*F)
             -     3958 * cos(4*D-M-MP) * EE
             +     3258 * cos(3*D-MP)
             +     2616 * cos(2*D+M+MP) * EE
             -     1897 * cos(4*D-M-2*MP) * EE
             -     2117 * cos(2*M-MP) * EES
             +     2354 * cos(2*D+2*M-MP) * EES
             -     1423 * cos(4*D+MP)
             -     1117 * cos(4*MP)
             -     1571 * cos(4*D-M) * EE
             -     1739 * cos(D-2*MP)
             -     4421 * cos(2*MP-2*F)
             +     1165 * cos(2*M+MP)
             +     8752 * cos(2*D-MP-2*F);

  MoonLat :=   5128122 * sin(F)
             +  280602 * sin(MP+F)
             +  277693 * sin(MP-F)
             +  173237 * sin(2*D-F)
             +   55413 * sin(2*D-MP+F)
             +   46271 * sin(2*D-MP-F)
             +   32573 * sin(2*D+F)
             +   17198 * sin(2*MP+F)
             +    9266 * sin(2*D+MP-F)
             +    8822 * sin(2*MP-F)
             +    8216 * sin(2*D-M-F) * EE
             +    4324 * sin(2*D-2*MP-F)
             +    4200 * sin(2*D+MP+F)
             -    3359 * sin(2*D+M-F) * EE
             +    2463 * sin(2*D-M-MP+F) * EE
             +    2211 * sin(2*D-M+F) * EE
             +    2065 * sin(2*D-M-MP-F) * EE
             -    1870 * sin(M-MP-F) * EE
             +    1828 * sin(4*D-MP-F)
             -    1794 * sin(M+F) * EE
             -    1749 * sin(3*F)
             -    1565 * sin(M-MP+F) * EE
             -    1491 * sin(D+F)
             -    1475 * sin(M+MP+F) * EE
             -    1410 * sin(M+MP-F) * EE
             -    1344 * sin(M-F) * EE
             -    1335 * sin(D-F)
             +    1107 * sin(3*MP+F)
             +    1021 * sin(4*D-F)
             +     833 * sin(4*D-MP+F)
             +     777 * sin(MP-3*F)
             +     671 * sin(4*D-2*MP+F)
             +     607 * sin(2*D-3*F)
             +     596 * sin(2*D+2*MP-F)
             +     491 * sin(2*D-M+MP-F) * EE
             -     451 * sin(2*D-2*MP+F)
             +     439 * sin(3*MP-F)
             +     422 * sin(2*D+2*MP+F)
             +     421 * sin(2*D-3*MP-F)
             -     366 * sin(2*D+M-MP+F) * EE
             -     351 * sin(2*D+M+F) * EE
             +     331 * sin(4*D+F)
             +     315 * sin(2*D-M+MP+F) * EE
             +     302 * sin(2*D-2*M-F) * EES
             -     283 * sin(MP + 3*F)
             -     229 * sin(2*D+M+MP-F) * EE
             +     223 * sin(D+M-F) * EE
             +     223 * sin(D+M+F) * EE;

  MoonLong := MoonLong
              + 3958 * sin(A1)
              + 1962 * sin(LP-F)
              +  318 * sin(A2);

  MoonLat := MoonLat
             - 2235 * sin(LP)
             +  382 * sin(A3)
             +  175 * sin(A1-F)
             +  175 * sin(A1+F)
             +  127 * sin(LP-MP)
             -  115 * sin(LP+MP);

  MoonLong := LP + MoonLong/1000000/radcor;
  MoonLat  := MoonLat/1000000/radcor;
  MoonDst  := 385000.56 + MoonDst/1000;

  Result.Plx := StInvSin(6378.14/MoonDst) * radcor;
  Result.Dia := 358473400 / MoonDst * 2 / 3600;

  S1 := sin(MoonLong) * cos(OB2000) - StTan(MoonLat) * sin(OB2000);
  C1 := cos(MoonLong);
  Result.RA := StInvTan2(C1, S1) * radcor;

  TD := sin(MoonLat) * cos(OB2000)
      + cos(MoonLat) * sin(OB2000) * sin(MoonLong);
  TD := StInvSin(TD);
  Result.DC := TD * radcor;

  I := sin(SunDC/radcor) * sin(TD)
     + cos(SunDC/radcor) * cos(TD) * cos((SunRA-Result.RA)/radcor);
  Result.Phase := (1 - I) / 2;

  I := StInvCos(I) * radcor;
  Result.Elong := (Result.RA - SunRA);
  if Result.Elong < 0 then
      Result.Elong := 360 + Result.Elong;
  if Result.Elong >= 180 then begin
    Result.Phase := -Result.Phase;  {waning moon}
    Result.Elong := -I
  end else
    Result.Elong := I;
end;

function AmountOfSunlight(LD : TStDate; Longitude, Latitude : Double): TStTime;
  {-compute the hours, min, sec of sunlight on a given date}
var
  RS : TStRiseSetRec;
begin
  RS := SunRiseSet(LD, Longitude, Latitude);
  with RS do begin
    if ORise = -3 then begin
    {sun is always above the horizon}
      Result := SecondsInDay;
      Exit;
    end;

    if ORise = -2 then begin
    {sun is always below horizon}
      Result := 0;
      Exit;
    end;

    if (ORise > -1) then begin
      if (OSet > -1) then
        Result := OSet - ORise
      else
        Result := SecondsInDay - ORise;
    end else begin
      if (OSet > -1) then
        Result := OSet
      else
        Result := 0;
    end;
  end;
  if (Result < 0) then
    Result := Result + SecondsInDay
  else if (Result >= SecondsInDay) then
    Result := Result - SecondsInDay;
end;

function SunPos(UT : TStDateTimeRec) : TStPosRec;
  {-compute the RA/Declination of the Sun}
var
  SP : TStSunXYZRec;
begin
  if not CheckDate(UT) then begin
    Result.RA := -1;
    Result.DC := -99;
    Exit;
  end;

  SP := SunPosPrim(UT);
  Result.RA := StInvTan2(SP.SunX, SP.SunY) * radcor;
  Result.DC := StInvSin(SP.SunZ / SP.RV) * radcor;
end;

function RiseSetPrim(LD : TStDate;
                     Longitude, Latitude, H0 : Double;
                     PR : TStPosRecArray;
                     ApproxOnly : Boolean) : TStRiseSetRec;
  {-primitive routine for finding rise/set times}
var
  ST,
  NST,
  HA,
  LatR,
  N1,
  N2,
  N3,
  TTran,
  TRise,
  TSet,
  TV1,
  TV2,
  A1,
  A2,
  DeltaR,
  DeltaS,
  RA,
  DC,
  Alt      : Double;

  ICount   : SmallInt;

  UT  : TStDateTimeRec;
begin
  H0 := H0/radcor;

  UT.D := LD;
  UT.T := 0;
  ST := SiderealTime(UT);

  LatR := Latitude/radcor;

{check if object never rises/sets}
  N1 := sin(H0) - sin(LatR) * sin(PR[2].DC/radcor);
  N2 := cos(LatR) * cos(PR[2].DC/radcor);

  HA := N1 / N2;
  if (abs(HA) >= 1) then begin
{    if ((Latitude - 90) >= 90) then begin}
    if (HA > 0) then begin
{object never rises}
      Result.ORise := -2;
      Result.OSet  := -2;
    end else begin
{object never sets, i.e., it is circumpolar}
      Result.ORise := -3;
      Result.OSet  := -3;
    end;
    Exit;
  end;

  HA := StInvCos(HA) * radcor;
  if HA > 180 then
    HA := HA - 180;
  if HA < 0 then
    HA := HA + 180;

{compute approximate hour angle at transit}
  TTran := (PR[2].RA - Longitude - ST) / 360;
  if abs(TTran) >= 1 then
    TTran:= Frac(TTran);
  if TTran < 0 then
    TTran := TTran + 1;

  TRise := TTran - HA/360;
  TSet  := TTran + HA/360;
  if abs(TRise) >= 1 then
    TRise:= Frac(TRise);
  if TRise < 0 then
    TRise := TRise + 1;

  if abs(TSet) >= 1 then
    TSet := Frac(TSet);
  if TSet < 0 then
    TSet := TSet + 1;

  if not ApproxOnly then begin
{refine rise time by interpolation/iteration}
    ICount := 0;
    TV1    := 0;
    A1     := 0;
    repeat
      NST := ST + 360.985647 * TRise;
      NST := Frac(NST / 360.0) * 360;
      N1 := PR[2].RA - PR[1].RA;
      N2 := PR[3].RA - PR[2].RA;
      N3 := N2 - N1;
      RA := PR[2].RA + TRise/2 * (N1 + N2 + TRise*N3);

      N1 := PR[2].DC - PR[1].DC;
      N2 := PR[3].DC - PR[2].DC;
      N3 := N2 - N1;
      DC := PR[2].DC + TRise/2 * (N1 + N2 + TRise*N3);
      DC := DC/radcor;

      HA := (NST + Longitude - RA) / radcor;
      Alt := StInvSin(sin(LatR) * sin(DC) + cos(LatR) * cos(DC) * cos(HA));
      DeltaR := ((Alt - H0) * radcor) / (360 * cos(DC) * cos(LatR) * sin(HA));
      TRise := TRise + DeltaR;
      Inc(ICount);
      if (ICount > 3) and (abs(DeltaR) >= 0.0005) then begin
        if (ICount = 4) then begin
          TV1 := TRise;
          A1  := (Alt-H0) * radcor;
        end else if (ICount = 5) then begin
          TV2 := TRise;
          A2  := (Alt-H0) * radcor;

          TRise := TV1 + (A1 / A2) * (TV1 - TV2);
          break;
        end;
      end;
    until (abs(DeltaR) < 0.0005); {0.0005d = 0.72 min}

  {refine set time by interpolation/iteration}
    ICount := 0;
    TV1    := 0;
    A1     := 0;
    repeat
      NST := ST + 360.985647 * TSet;
      NST := Frac(NST / 360.0) * 360;
      N1 := PR[2].RA - PR[1].RA;
      N2 := PR[3].RA - PR[2].RA;
      N3 := N2 - N1;
      RA := PR[2].RA + TSet/2 * (N1 + N2 + TSet*N3);

      N1 := PR[2].DC - PR[1].DC;
      N2 := PR[3].DC - PR[2].DC;
      N3 := N2 - N1;
      DC := PR[2].DC + TSet/2 * (N1 + N2 + TSet*N3);
      DC := DC/radcor;

      HA := (NST + Longitude - RA) / radcor;
      Alt := StInvSin(sin(LatR) * sin(DC) + cos(LatR) * cos(DC) * cos(HA));
      DeltaS := ((Alt - H0) * radcor) / (360 * cos(DC) * cos(LatR) * sin(HA));
      TSet := TSet + DeltaS;
      Inc(ICount);
      if (ICount > 3) and (abs(DeltaS) >= 0.0005) then begin
        if (ICount = 4) then begin
          TV1 := TSet;
          A1  := (Alt-H0) * radcor;
        end else if (ICount = 5) then begin
          TV2 := TSet;
          A2  := (Alt-H0) * radcor;

          TSet := TV1 + (A1 / A2) * (TV1 - TV2);
          break;
        end;
      end;
    until (abs(DeltaS) < 0.0005); {0.0005d = 0.72 min}
  end;

  if (TRise >= 0) and (TRise < 1) then
    Result.ORise := Trunc(TRise * SecondsInDay)
  else begin
    if TRise < 0 then
      Result.ORise := Trunc((TRise + 1) * SecondsInDay)
    else
      Result.ORise := Trunc(Frac(TRise) * SecondsInDay);
  end;
  if Result.ORise < 0 then
    Inc(Result.ORise, SecondsInDay);
  if Result.ORise >= SecondsInDay then
    Dec(Result.ORise, SecondsInDay);


  if (TSet >= 0) and (TSet < 1) then
    Result.OSet := Trunc(TSet * SecondsInDay)
  else begin
    if TSet < 0 then
      Result.OSet := Trunc((TSet + 1) * SecondsInDay)
    else
      Result.OSet := Trunc(Frac(TSet) * SecondsInDay);
  end;
  if Result.OSet < 0 then
    Inc(Result.OSet, SecondsInDay);
  if Result.OSet >= SecondsInDay then
    Dec(Result.OSet, SecondsInDay);
end;

function SunRiseSet(LD : TStDate; Longitude, Latitude : Double) : TStRiseSetRec;
  {-compute the Sun rise or set time}
  {the value for H0 accounts for approximate refraction of 0.5667 deg. and}
  {that rise or set is based on the upper limb instead of the center of the solar disc}
var
  I  : Integer;
  H0 : Double;
  UT : TStDateTimeRec;
  RP : TStPosRecArray;
begin
  Dec(LD);
  H0 := -0.8333;
  UT.T := 0;
  UT.D := LD-1;

  if CheckDate(UT) then begin
    UT.D := UT.D + 2;
    if (not CheckDate(UT)) then begin
      Result.ORise := -4;
      Result.OSet  := -4;
      Exit;
    end else
      UT.D := UT.D-2;
  end else begin
    Result.ORise := -4;
    Result.OSet  := -4;
    Exit;
  end;

  for I := 1 to 3 do begin
    RP[I] := SunPos(UT);
    if I >= 2 then begin
      if RP[I].RA < RP[I-1].RA then
        RP[I].RA := RP[I].RA + 360;
    end;
    Inc(UT.D);
  end;
  Result := RiseSetPrim(LD, Longitude, Latitude, H0, RP, False);
end;

function Twilight(LD : TStDate; Longitude, Latitude : Double;
                  TwiType : TStTwilight) : TStRiseSetRec;
  {-compute the beginning or end of twilight}
  {twilight computations are based on the zenith distance of the center }
  {of the solar disc.}
  {Civil = 6 deg. below the horizon}
  {Nautical = 12 deg. below the horizon}
  {Astronomical = 18 deg. below the horizon}
var
  I  : Integer;
  H0 : Double;
  UT : TStDateTimeRec;
  RP : TStPosRecArray;
begin
  UT.D := LD-1;
  UT.T := 0;

  if CheckDate(UT) then begin
    UT.D := UT.D + 2;
    if (not CheckDate(UT)) then begin
      Result.ORise := -4;
      Result.OSet  := -4;
      Exit;
    end else
      UT.D := UT.D-2;
  end else begin
    Result.ORise := -4;
    Result.OSet  := -4;
    Exit;
  end;

  case TwiType of
    ttCivil : H0 := -6.0;
    ttNautical : H0 := -12.0;
    ttAstronomical : H0 := -18.0;
  else
    H0 := -18.0;
  end;

  for I := 1 to 3 do begin
    UT.D := LD + I-1;
    RP[I] := SunPos(UT);
    if (I > 1) then begin
      if RP[I].RA < RP[I-1].RA then
        RP[I].RA := RP[I].RA + 360.0;
    end;
  end;
  Result := RiseSetPrim(LD, Longitude, Latitude, H0, RP, False);
end;

function FixedRiseSet(LD : TStDate;
                      RA, DC, Longitude, Latitude : Double) : TStRiseSetRec;
  {-compute the rise/set time for a fixed object, e.g., star}
  {the value for H0 accounts for approximate refraction of 0.5667 deg.}
  {this routine does not refine the intial estimate and so may be off by five}
  {minutes or so}
var
  H0 : Double;
  UT : TStDateTimeRec;
  RP : TStPosRecArray;
begin
  H0 := -0.5667;
  UT.T := 0;
  UT.D := LD;

  if not CheckDate(UT) then begin
    Result.ORise := -4;
    Result.OSet  := -4;
    Exit;
  end;

  RP[2].RA := RA;
  RP[2].DC := DC;
  Result := RiseSetPrim(LD, Longitude, Latitude, H0, RP, True);
end;

function MoonPos(UT : TStDateTimeRec) : TStMoonPosRec;
  {-compute the J2000 RA/Declination of the moon}
begin
  if not CheckDate(UT) then begin
    Result.RA := -1;
    Result.DC := -1;
    Exit;
  end;
  Result := MoonPosPrim(UT);
end;

function MoonRiseSet(LD : TStDate; Longitude, Latitude : Double) : TStRiseSetRec;
  {compute the Moon rise and set time}
  {the value for H0 accounts for approximate refraction of 0.5667 deg., }
  {that rise or set is based on the upper limb instead of the center of the}
  {lunar disc, and the lunar parallax. In accordance with American Ephemeris }
  {practice, the phase of the moon is not taken into account, i.e., the time}
  {is based on the upper limb whether it is lit or not}
var
  I   : Integer;
  H0  : Double;
  UT  : TStDateTimeRec;
  RP  : TStPosRecArray;
  MPR : TStMoonPosRec;
begin
  H0 := 0.125;  { default value }

  Dec(LD);
  UT.T := 0;
  UT.D := LD;

  if CheckDate(UT) then begin
    UT.D := UT.D + 2;
    if (not CheckDate(UT)) then begin
      Result.ORise := -4;
      Result.OSet  := -4;
      Exit;
    end else
      UT.D := UT.D-2;
  end else begin
    Result.ORise := -4;
    Result.OSet  := -4;
    Exit;
  end;

  for I := 1 to 3 do begin
    MPR := MoonPos(UT);
    RP[I].RA := MPR.RA;
    RP[I].DC := MPR.DC;
    if I >= 2 then begin
      if I = 2 then
        H0 := 0.7275*MPR.Plx - 0.5667;
      if RP[I].RA < RP[I-1].RA then
        RP[I].RA := RP[I].RA + 360;
    end;
    Inc(UT.D);
  end;
  Result := RiseSetPrim(LD, Longitude, Latitude, H0, RP, False);
end;

function LunarPhase(UT : TStDateTimeRec) : Double;
  {-compute the phase of the moon}
  {The value is positive if between New and Full Moon}
  {             negative if between Full and New Moon}
var
  MPR : TStMoonPosRec;
begin
  MPR := MoonPosPrim(UT);
  Result := MPR.Phase;
end;

procedure GetPhases(K : Double; var PR : TStPhaseRecord);
{primitive routine to find the date/time of phases in a lunar cycle}
var
  JD,
  NK,
  TD,
  J1,
  J2,
  J3    : Double;

  step  : Integer;

  E,
  FP,
  S1,
  M1,
  M2,
  M3    : Double;

  function AddCor : Double;
  begin
    AddCor := 0.000325 * sin((299.77 + 0.107408*K - 0.009173*J2)/radcor)
            + 0.000165 * sin((251.88 +  0.016321*K)/radcor)
            + 0.000164 * sin((251.83 + 26.651886*K)/radcor)
            + 0.000126 * sin((349.42 + 36.412478*K)/radcor)
            + 0.000110 * sin((84.660 + 18.206239*K)/radcor)
            + 0.000062 * sin((141.74 + 53.303771*K)/radcor)
            + 0.000060 * sin((207.14 +  2.453732*K)/radcor)
            + 0.000056 * sin((154.84 +  7.306860*K)/radcor)
            + 0.000047 * sin((34.520 + 27.261239*K)/radcor)
            + 0.000042 * sin((207.19 +  0.121824*K)/radcor)
            + 0.000040 * sin((291.34 +  1.844379*K)/radcor)
            + 0.000037 * sin((161.72 + 24.198154*K)/radcor)
            + 0.000035 * sin((239.56 + 25.513099*K)/radcor)
            + 0.000023 * sin((331.55 +  3.592518*K)/radcor);
  end;

begin
  NK := K;
  FillChar(PR, SizeOf(TStPhaseRecord), #0);
  for step := 0 to 3 do begin
    K := NK + (step*0.25);
    FP := Frac(K);
    if FP < 0 then
      FP := FP + 1;

{compute Julian Centuries}
    J1 := K / 1236.85;
    J2 := Sqr(J1);
    J3 := J2 * J1;


{solar mean anomaly}
    S1 := 2.5534
        + 29.1053569 * K
        - 0.0000218 * J2
        - 0.00000011 * J3;
    S1 := Frac(S1 / 360.0) * 360;
    if S1 < 0 then
      S1 := S1 + 360.0;

{lunar mean anomaly}
    M1 := 201.5643
        + 385.81693528 * K
        + 0.0107438 * J2
        + 0.00001239 * J3
        - 0.000000058 * J2 * J2;
    M1 := Frac(M1 / 360.0) * 360;
    if M1 < 0 then
      M1 := M1 + 360.0;

{lunar argument of latitude}
    M2 := 160.7108
        + 390.67050274 * K
        - 0.0016341 * J2
        - 0.00000227 * J3
        + 0.000000011 * J2 * J2;
    M2 := Frac(M2 / 360.0) * 360;
    if M2 < 0 then
      M2 := M2 + 360.0;

{lunar ascending node}
    M3 := 124.7746
        - 1.56375580 * K
        + 0.0020691 * J2
        + 0.00000215 * J3;
    M3 := Frac(M3 / 360.0) * 360;
    if M3 < 0 then
      M3 := M3 + 360.0;

{convert to radians}
    S1 := S1/radcor;
    M1 := M1/radcor;
    M2 := M2/radcor;
    M3 := M3/radcor;

{mean Julian Date for phase}
    JD := 2451550.09765
        + 29.530588853 * K
        + 0.0001337 * J2
        - 0.000000150 * J3
        + 0.00000000073 * J2 * J2;

{earth's orbital eccentricity}
    E := 1.0 - 0.002516 * J1 - 0.0000074 * J2;

{New Moon date time}
    if FP < 0.01 then begin
      TD := - 0.40720 * sin(M1)
          + 0.17241 * E * sin(S1)
          + 0.01608 * sin(2*M1)
          + 0.01039 * sin(2*M2)
          + 0.00739 * E * sin(M1-S1)
          - 0.00514 * E * sin(M1 + S1)
          + 0.00208 * E * E * sin(2 * S1)
          - 0.00111 * sin(M1 - 2*M2)
          - 0.00057 * sin(M1 + 2*M2)
          + 0.00056 * E * sin(2*M1 + S1)
          - 0.00042 * sin(3 * M1)
          + 0.00042 * E * sin(S1 + 2*M2)
          + 0.00038 * E * sin(S1 - 2*M2)
          - 0.00024 * E * sin(2*(M1-S1))
          - 0.00017 * sin(M3)
          - 0.00007 * sin(M1 + 2*S1);
      JD := JD + TD + AddCor;
      PR.NMDate := JD;
    end;

{Full Moon date/time}
    if Abs(FP - 0.5) < 0.01 then begin
      TD := - 0.40614 * sin(M1)
          + 0.17302 * E * sin(S1)
          + 0.01614 * sin(2*M1)
          + 0.01043 * sin(2*M2)
          + 0.00734 * E * sin(M1-S1)
          - 0.00515 * E * sin(M1 + S1)
          + 0.00209 * E * E * sin(2 * S1)
          - 0.00111 * sin(M1 - 2*M2)
          - 0.00057 * sin(M1 + 2*M2)
          + 0.00056 * E * sin(2*M1 + S1)
          - 0.00042 * sin(3 * M1)
          + 0.00042 * E * sin(S1 + 2*M2)
          + 0.00038 * E * sin(S1 - 2*M2)
          - 0.00024 * E * sin(2*(M1-S1))
          - 0.00017 * sin(M3)
          - 0.00007 * sin(M1 + 2*S1);
      JD := JD + TD + AddCor;
      PR.FMDate := JD;
    end;

{Quarters date/time}
    if (abs(FP - 0.25) < 0.01) or (abs(FP - 0.75) < 0.01) then begin
      TD := - 0.62801 * sin(M1)
          + 0.17172 * sin(S1) * E
          - 0.01183 * sin(M1+S1) * E
          + 0.00862 * sin(2*M1)
          + 0.00804 * sin(2*M2)
          + 0.00454 * sin(M1-S1) * E
          + 0.00204 * sin(2*S1) * E * E
          - 0.00180 * sin(M1 - 2*M2)
          - 0.00070 * sin(M1 + 2*M2)
          - 0.00040 * sin(3*M1)
          - 0.00034 * sin(2*M1-S1) * E
          + 0.00032 * sin(S1 + 2*M2) * E
          + 0.00032 * sin(S1 - 2*M2) * E
          - 0.00028 * sin(M1 + 2*S1) * E * E
          + 0.00027 * sin(2*M1 + S1) * E
          - 0.00017 * sin(M3)
          - 0.00005 * sin(M1 - S1 - 2*M2);
      JD := JD + TD + AddCor;

{adjustment to computed Julian Date}
      TD := 0.00306
          - 0.00038 * E * cos(S1)
          + 0.00026 * cos(M1)
          - 0.00002 * cos(M1-S1)
          + 0.00002 * cos(M1+S1)
          + 0.00002 * cos(2*M2);

      if Abs(FP - 0.25) < 0.01 then
        PR.FQDate := JD + TD
      else
        PR.LQDate := JD - TD;
    end;
  end;
end;

procedure PhasePrim(LD : TStDate; var PhaseArray : TStPhaseArray);
  {-primitive phase calculation}
var
  I,
  D,
  M,
  Y      : Integer;
  K, TD,
  LYear  : Double;

begin
  StDateToDMY(LD, D, M, Y);
  LYear := Y - 0.05;
  K := (LYear - 2000) * 12.3685;
  K := Int(K);
  TD := K / 12.3685 + 2000;
  if TD > Y then
    K := K-1;

{compute phases for each lunar cycle throughout the year}
  for I := 0 to 13 do begin
    GetPhases(K, PhaseArray[I]);
    K := K + 1;
  end;
end;

function GenSearchPhase(SD : TStDate; PV : Byte) : TStLunarRecord;
{searches for the specified phase in the given month/year expressed by SD}
var
  I,
  C,
  LD,
  LM,
  LY,
  TD,
  TM,
  TY    : Integer;
  ADate : TStDate;
  JD    : Double;
  PhaseArray : TStPhaseArray;
begin
  C := 0;
  FillChar(Result, SizeOf(Result), $FF);

  StDateToDMY(SD, LD, LM, LY);
  PhasePrim(SD, PhaseArray);
  for I := LM-1 to LM+1 do begin
    if (PV = 0) then
      JD := PhaseArray[I].NMDate
    else if (PV = 1) then
      JD := PhaseArray[I].FQDate
    else if (PV = 2) then
      JD := PhaseArray[I].FMDate
    else
      JD := PhaseArray[I].LQDate;
    ADate := AstJulianDateToStDate(JD, True);

    StDateToDMY(ADate, TD, TM, TY);
    if TM < LM then
      continue
    else if TM = LM then begin
      Result.T[C].D := ADate;
      Result.T[C].T := Trunc((Frac(JD) + 0.5) * 86400);
      if Result.T[C].T >= SecondsInDay then
        Dec(Result.T[C].T, SecondsInDay);
      Inc(C);
    end;
  end;
end;

function FirstQuarter(D : TStDate) : TStLunarRecord;
  {-compute date/time of FirstQuarter(s)}
begin
  Result := GenSearchPhase(D, 1);
end;

function FullMoon(D : TStDate) : TStLunarRecord;
  {-compute the date/time of FullMoon(s)}
begin
  Result := GenSearchPhase(D, 2);
end;

function LastQuarter(D: TStDate) : TStLunarRecord;
  {-compute the date/time of LastQuarter(s)}
begin
  Result := GenSearchPhase(D, 3);
end;

function NewMoon(D : TStDate) : TStLunarRecord;
  {-compute the date/time of NewMoon(s)}
begin
  Result := GenSearchPhase(D, 0);
end;

function NextPrevPhase(D : TStDate; Ph : Byte;
                       FindNext : Boolean) : TStDateTimeRec;
var
  LD,
  LM,
  LY  : Integer;
  K,
  JD,
  TJD : Double;
  PR  : TStPhaseRecord;
  OK  : Boolean;
begin
   if (D < MinDate) or (D > MaxDate) then begin
     Result.D := BadDate;
     Result.T := BadTime;
     Exit;
   end;

   StDateToDMY(D, LD, LM, LY);
   K := ((LY + LM/12 + LD/365.25) - 2000) * 12.3685 - 0.5;
   if FindNext then
     K := Round(K)-1
   else
     K := Round(K)-2;

   OK := False;
   TJD := AstJulianDate(D);
   repeat
     GetPhases(K, PR);

     if (Ph = 0) then
       JD := PR.NMDate
     else if (Ph = 1) then
       JD := PR.FQDate
     else if (Ph = 2) then
       JD := PR.FMDate
     else
       JD := PR.LQDate;

     if (FindNext) then begin
       if (JD > TJD) then
         OK := True
       else
         K := K + 1;
     end else begin
       if (JD < TJD) then
         OK := True
       else
         K := K - 1;
     end;
   until OK;

   Result.D := AstJulianDateToStDate(JD, True);
   if (Result.D <> BadDate) then begin
     Result.T := Trunc((Frac(JD) + 0.5) * 86400);
     if Result.T >= SecondsInDay then
       Dec(Result.T, SecondsInDay);
   end else
     Result.T := BadTime;
end;

function NextFirstQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest FirstQuarter}
begin
   Result := NextPrevPhase(D, 1, True);
end;

function NextFullMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest FullMoon}
begin
  Result := NextPrevPhase(D, 2, True);
end;

function NextLastQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest LastQuarter}
begin
  Result := NextPrevPhase(D, 3, True);
end;

function NextNewMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the next closest NewMoon}
begin
  Result := NextPrevPhase(D, 0, True);
end;

function PrevFirstQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest FirstQuarter}
begin
  Result := NextPrevPhase(D, 1, False);
end;

function PrevFullMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest FullMoon}
begin
  Result := NextPrevPhase(D, 2, False);
end;

function PrevLastQuarter(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest LastQuarter}
begin
  Result := NextPrevPhase(D, 3, False);
end;

function PrevNewMoon(D : TStDate) : TStDateTimeRec;
  {-compute the date/time of the prev closest NewMoon}
begin
  Result := NextPrevPhase(D, 0, False);
end;

{Calendar procedures/functions}

function SolEqPrim(Y : Integer; K : Byte) : TStDateTimeRec;
{primitive routine for finding equinoxes and solstices}
var
  JD, TD, LY,
  JCent, MA,
  SLong       : Double;

begin
  JD := 0;
  Result.D := BadDate;
  Result.T := BadTime;

{the following algorithm is valid only in the range of [1000..3000 AD]}
{but is limited to returning dates in [MinYear..MaxYear]}
  if (Y < MinYear) or (Y > MaxYear) then
    Exit;

{compute approximate date/time for specified event}
  LY := (Y - 2000) / 1000;
  case K of
    0 : JD := 2451623.80984
            + 365242.37404 * LY
            + 0.05169 * sqr(LY)
            - 0.00411 * LY * sqr(LY)
            - 0.00057 * sqr(sqr(LY));

    1 : JD := 2451716.56767
            + 365241.62603 * LY
            + 0.00325 * sqr(LY)
            + 0.00888 * LY * sqr(LY)
            - 0.00030 * sqr(sqr(LY));

    2 : JD := 2451810.21715
            + 365242.01767 * LY
            - 0.11575 * sqr(LY)
            + 0.00337 * sqr(LY) * LY
            + 0.00078 * sqr(sqr(LY));

    3 : JD := 2451900.05952
            + 365242.74049 * LY
            - 0.06223 * sqr(LY)
            - 0.00823 * LY * sqr(LY)
            + 0.00032 * sqr(sqr(LY));
  end;

{refine date/time by computing corrections due to solar longitude,}
{nutation and abberation. Iterate using the corrected time until}
{correction is less than one minute}
  repeat
    Result.D := AstJulianDateToStDate(JD, True);
    Result.T := Trunc((Frac(JD) + 0.5) * 86400);
    if Result.T >= SecondsInDay then
      Dec(Result.T, SecondsInDay);
    JCent := (JD - 2451545.0)/36525.0;

{approximate solar longitude - no FK5 correction}
    SLong := 280.46645
           + 36000.76983 * JCent
           + 0.0003032 * sqr(JCent);
    SLong := Frac((SLong)/360.0) * 360.0;
    if SLong < 0 then
      SLong := SLong + 360;

{Equation of the center correction}
    MA := 357.52910
        + 35999.05030 * JCent;
    MA := MA/radcor;
    SLong := SLong
           + (1.914600 - 0.004817 * JCent - 0.000014 * sqr(JCent)) * sin(MA)
           + (0.019993 - 0.000101 * JCent) * sin(2*MA);

{approximate nutation}
    TD := 125.04452
        - 1934.136261 * JCent
        + 0.0020708 * sqr(JCent);
    TD := TD/radcor;
    TD := (-17.20 * sin(TD) - 1.32 * sin(2*SLong/radcor))/3600;

{approximate abberation - solar distance is assumed to be 1 A.U.}
    SLong := SLong - (20.4989/3600) + TD;

{correction to compute Julian Date for event}
    TD := 58 * sin((K*90 - SLong)/radcor);
    if abs(TD) >= 0.0005 then
      JD := JD + TD;
  until abs(TD) < 0.0005;
end;

function Solstice(Y, Epoch : Integer; Summer : Boolean) : TStDateTimeRec;
  {-compute the date/time of the summer or winter solstice}
  {if Summer = True,  compute astronomical summer solstice (summer in N. Hem.)}
  {          = False, compute astronomical winter solstice (winter in N. Hem.)}
begin
  Y := CheckYear(Y, Epoch);
  if Summer then
    Result := SolEqPrim(Y, 1)
  else
    Result := SolEqPrim(Y, 3);
end;

function Equinox(Y, Epoch : Integer; Vernal : Boolean) : TStDateTimeRec;
  {-compute the date/time of the vernal/autumnal equinox}
  {if Vernal = True,  compute astronomical vernal equinox (spring in N. Hem.)}
  {          = False, compute astronomical autumnal equinox (fall in N. Hem.)}
begin
  Y := CheckYear(Y, Epoch);
  if Vernal then
    Result := SolEqPrim(Y, 0)
  else
    Result := SolEqPrim(Y, 2);
end;

function Easter(Y : Integer; Epoch : Integer) : TStDate;
  {-compute the date of Easter}
var
  A, B,
  C, D,
  E, F,
  G, H,
  I, K,
  L, M,
  N, P : Integer;
begin
  Y := CheckYear(Y, Epoch);

  if (Y < MinYear) or (Y > MaxYear) then begin
    Result := BadDate;
    Exit;
  end;

  A := Y mod 19;
  B := Y div 100;
  C := Y mod 100;
  D := B div 4;
  E := B mod 4;
  F := (B+8) div 25;
  G := (B-F+1) div 3;
  H := (19*A + B - D - G + 15) mod 30;
  I := C div 4;
  K := C mod 4;
  L := (32 + 2*E + 2*I - H - K) mod 7;
  M := (A + 11*H + 22*L) div 451;
  N := (H + L - 7*M + 114) div 31;
  P := (H + L - 7*M + 114) mod 31 + 1;

  Result := DMYToStDate(P, N, Y, Epoch);
end;

{Conversion routines}
function HoursMin(RA : Double) : String;
  {-convert RA to formatted hh:mm:ss string}
var
  HR, MR  : Double;
  HS, MS  : string[12];

begin
  if abs(RA) >= 360.0 then
    RA := Frac(RA / 360.0) * 360;
  if RA < 0 then
    RA := RA + 360.0;

  HR := Int(RA / 15.0);
  MR  := Frac(RA / 15.0) * 60;

  Str(MR:5:2, MS);
  if MS = '60.00' then begin
    MS := '00.00';
    HR := HR + 1;
    if HR = 24 then
      HS := '0'
    else
      Str(HR:2:0, HS);
  end else begin
    if MS[1] = ' ' then
      MS[1] := '0';
    Str(Hr:2:0, HS);
  end;
  Result := string(HS) + ' ' + string(MS);
end;

function DegsMin(DC : Double) : String;
  {-convert Declination to formatted +/-dd:mm:ss string}
var
  DR, MR  : Double;
  DS, MS  : string[12];
begin
  if abs(DC) > 90 then
    DC := Frac(DC / 90.0) * 90.0;

  DR := Int(DC);
  MR := Frac(abs(DC)) * 60;

  Str(MR:4:1, MS);
  if MS = '60.0' then begin
    MS := '00.0';
    if DC >= 0 then
      DR := DR + 1
    else
      DR := DR - 1;
  end;

  if abs(DC) < 10 then begin
    Str(DR:2:0, DS);
    DS := TrimLeadS(DS);
    if DC < 0 then begin
      if DC > -1 then
        DS := '- 0'
      else
        DS := '- ' + ShortString(DS[2]);
    end else
      DS := '+ ' + DS;
  end else begin
    Str(DR:3:0, DS);
    DS := TrimLeadS(DS);
    if DC < 0 then begin
      Delete(DS,1,1);
      DS := '-' + DS;
    end else
      DS := '+' + DS;
  end;
  if MS[1] = ' ' then
    MS[1] := '0';
  Result := string(DS) + ' ' + string(MS);
end;

function DateTimeToAJD(D : TDateTime) : Double;
begin
  Result := D + AJDOffset;
end;

function AJDToDateTime(D : Double) : TDateTime;
begin
  Result := D - AJDOffset;
end;


initialization
  AJDOffSet := AstJulianDatePrim(1600, 1, 1, 0) - EncodeDate(1600, 1, 1);
end.
