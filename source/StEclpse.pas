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
{* SysTools: StEclpse.pas 4.04                           *}
{*********************************************************}
{* SysTools: Lunar/Solar Eclipses                        *}
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


unit StEclpse;

interface

uses
  {$IFDEF UseMathUnit} Math, {$ENDIF}
  StBase, StList, StDate, StAstro, StMath;

type
  TStEclipseType = (etLunarPenumbral, etLunarPartial, etLunarTotal,
                    etSolarPartial, etSolarAnnular, etSolarTotal,
                    etSolarAnnularTotal);

  TStHemisphereType = (htNone, htNorthern, htSouthern);

  TStContactTimes = packed record
    UT1,                         {start of lunar penumbral phase}
    UT2,                         {end of lunar penumbral phase}
    FirstContact,                {start of partial eclipse}
    SecondContact,               {start of totality}
    MidEclipse,                  {mid-eclipse}
    ThirdContact,                {end of totality}
    FourthContact   : TDateTime; {end of partial phase}
  end;

  TStLongLat = packed record
    JD          : TDateTime;
    Longitude,
    Latitude,
    Duration    : Double;
  end;
  PStLongLat = ^TStLongLat;

  TStEclipseRecord = packed record
    EType       : TStEclipseType;        {type of Eclipse}
    Magnitude   : Double;                {magnitude of eclipse}
    Hemisphere  : TStHemisphereType;     {favored hemisphere - solar}
    LContacts   : TStContactTimes;       {Universal Times of critical points}
                                         { in lunar eclipses}
    Path        : TStList;               {longitude/latitude of moon's shadow}
  end;                                   { during solar eclipse}
  PStEclipseRecord = ^TStEclipseRecord;

  TStBesselianRecord = packed record
    JD       : TDateTime;
    Delta,
    Angle,
    XAxis,
    YAxis,
    L1,
    L2       : Double;
  end;

  TStEclipses = class(TStList)
  {.Z+}
  protected {private}
    FBesselianElements : array[1..25] of TStBesselianRecord;
    F0,
    FUPrime,
    FDPrime   : Double;

    function GetEclipse(Idx : Integer) : PStEclipseRecord;
    procedure CentralEclipseTime(JD, K, J2,
                                 SunAnom, MoonAnom,
                                 ArgLat, AscNode, EFac : Double;
                                 var F1, A1, CentralTime : Double);
    procedure CheckForEclipse(K : Double);
    procedure TotalLunarEclipse(CentralJD, MoonAnom, Mu,
                                PMag, UMag, Gamma : Double);
    procedure PartialLunarEclipse(CentralJD, MoonAnom, Mu,
                                  PMag, UMag, Gamma : Double);
    procedure PenumbralLunarEclipse(CentralJD, MoonAnom, Mu,
                                    PMag, UMag, Gamma : Double);

    procedure GetBesselianElements(CentralJD : Double);
    procedure GetShadowPath(I1, I2 : Integer; Path : TStList);
    procedure NonPartialSolarEclipse(CentralJD, Mu, Gamma : Double);
    procedure PartialSolarEclipse(CentralJD, Mu, Gamma : Double);
    {.Z-}
  public
    constructor Create(NodeClass : TStNodeClass);
      override;
    procedure FindEclipses(Year : integer);

    property Eclipses[Idx : Integer] : PStEclipseRecord
      read GetEclipse;
  end;


implementation

procedure DisposePathData(Data : Pointer); far;
begin
  Dispose(PStLongLat(Data));
end;

procedure DisposeEclipseRecord(Data : Pointer); far;
var
  EcData : TStEclipseRecord;
begin
  EcData := TStEclipseRecord(Data^);
  if (Assigned(EcData.Path)) then
    EcData.Path.Free;
  Dispose(PStEclipseRecord(Data));
end;

constructor TStEclipses.Create(NodeClass : TStNodeClass);
begin
  inherited Create(NodeClass);

  DisposeData := DisposeEclipseRecord;
end;

function TStEclipses.GetEclipse(Idx : Integer) : PStEclipseRecord;
begin
  if (Idx < 0) or (Idx > pred(Count)) then
    Result := nil
  else
    Result := PStEclipseRecord(Items[Idx].Data);
end;

procedure TStEclipses.FindEclipses(Year : integer);
var
  K,
  MeanJD,
  JDofFirst,
  JDofLast    : Double;

begin
  JDofFirst := AstJulianDatePrim(Year, 1, 1, 0);
  JDofLast  := AstJulianDatePrim(Year, 12, 31, pred(SecondsInDay));
  K := Int((Year - 2000) * 12.3685 - 1);
  repeat
    MeanJD := 2451550.09765 + 29.530588853 * K;
    if (MeanJD < JDofFirst) then
      K := K + 0.5;
  until (MeanJD >= JDofFirst);

  while (MeanJD < JDofLast) do begin
    CheckForEclipse(K);
    K := K + 0.5;
    MeanJD := 2451550.09765 + 29.530588853 * K;
  end;
end;

procedure TStEclipses.CentralEclipseTime(JD, K, J2,
                                         SunAnom, MoonAnom,
                                         ArgLat, AscNode, EFac : Double;
                                         var F1, A1, CentralTime : Double);
{the mean error of this routine is 0.36 minutes in a test between}
{1951 through 2050 with a maximum of 1.1 - Meeus}
begin
  F1 := ArgLat - (0.02665/radcor) * sin(AscNode);
  A1 := (299.77/radcor)
      + (0.107408/radcor) * K
      - (0.009173/radcor) * J2;

  if (Frac(K) > 0.1) then
  {correction at Full Moon - Lunar eclipse}
    CentralTime := JD
                 - 0.4065 * sin(MoonAnom)
                 + 0.1727 * EFac * sin(SunAnom)
  else
  {correction at New Moon - solar eclipse}
    CentralTime  := JD
                  - 0.4075 * sin(MoonAnom)
                  + 0.1721 * EFac * sin(SunAnom);

  CentralTime := CentralTime
               + 0.0161 * sin(2 * MoonAnom)
               - 0.0097 * sin(2 * F1)
               + 0.0073 * sin(MoonAnom - SunAnom) * EFac
               - 0.0050 * sin(MoonAnom + SunAnom) * EFac
               - 0.0023 * sin(MoonAnom - 2*F1)
               + 0.0021 * sin(2*SunAnom) * EFac
               + 0.0012 * sin(MoonAnom + 2*F1)
               + 0.0006 * sin(2*MoonAnom + SunAnom) * EFac
               - 0.0004 * sin(3*MoonAnom)
               - 0.0003 * sin(SunAnom + 2*F1) * EFac
               + 0.0003 * sin(A1)
               - 0.0002 * sin(SunAnom - 2*F1) * EFac
               - 0.0002 * sin(2*MoonAnom - SunAnom) * EFac
               - 0.0002 * sin(AscNode);
end;

procedure TStEclipses.CheckForEclipse(K : Double);
var
  MeanJD,
  J1, J2, J3,
  PMag, UMag,
  CentralJD,
  SunAnom,
  MoonAnom,
  ArgLat,
  AscNode,
  EFac,
  DeltaT,
  F1, A1,
  P, Q, W,
  Gamma, Mu   : Double;
begin
{compute Julian Centuries}
  J1 := K / 1236.85;
  J2 := Sqr(J1);
  J3 := J2 * J1;

{mean Julian Date for phase}
  MeanJD := 2451550.09765
          + 29.530588853 * K
          + 0.0001337 * J2
          - 0.000000150 * J3
          + 0.00000000073 * J2 * J2;

{solar mean anomaly}
  SunAnom := 2.5534
           + 29.1053569 * K
           - 0.0000218 * J2
           - 0.00000011 * J3;
  SunAnom := Frac(SunAnom / 360.0) * 360;
  if (SunAnom < 0) then
    SunAnom := SunAnom + 360.0;

{lunar mean anomaly}
  MoonAnom := 201.5643
            + 385.81693528 * K
            + 0.0107438 * J2
            + 0.00001239 * J3
            - 0.000000058 * J2 * J2;
  MoonAnom := Frac(MoonAnom / 360.0) * 360;
  if (MoonAnom < 0) then
    MoonAnom := MoonAnom + 360.0;

{lunar argument of latitude}
  ArgLat := 160.7108
          + 390.67050274 * K
          - 0.0016341 * J2
          - 0.00000227 * J3
          + 0.000000011 * J2 * J2;
  ArgLat := Frac(ArgLat / 360.0) * 360;
  if (ArgLat < 0) then
    ArgLat := ArgLat + 360.0;

{lunar ascending node}
  AscNode := 124.7746
           - 1.56375580 * K
           + 0.0020691 * J2
           + 0.00000215 * J3;
  AscNode := Frac(AscNode / 360.0) * 360;
  if (AscNode < 0) then
    AscNode := AscNode + 360.0;

{convert to radians}
  SunAnom  := SunAnom/radcor;
  MoonAnom := MoonAnom/radcor;
  ArgLat   := ArgLat/radcor;
  AscNode  := AscNode/radcor;

{correction factor}
  EFac := 1.0 - 0.002516 * J1 - 0.0000074 * J2;

{if AscNode > 21 deg. from 0 or 180 then no eclipse}
  if (abs(sin(ArgLat)) > (sin(21.0/radcor))) then Exit;

{there is probably an eclipse - what kind? when?}

  CentralEclipseTime(MeanJD, K, J2, SunAnom, MoonAnom,
                     ArgLat, AscNode, EFac,
                     F1, A1, CentralJD);

{Central JD is in Dynamical Time. Sun/Moon Positions are based on UT}
{An APPROXIMATE conversion is made to UT. This has limited accuracy}

  DeltaT := (-15 + (sqr(CentralJD - 2382148) / 41048480)) / 86400;
  CentralJD := CentralJD - DeltaT;

  P := 0.2070 * sin(SunAnom) * EFac
     + 0.0024 * sin(2*SunAnom) * EFac
     - 0.0392 * sin(MoonAnom)
     + 0.0116 * sin(2*MoonAnom)
     - 0.0073 * sin(SunAnom + MoonAnom) * EFac
     + 0.0067 * sin(MoonAnom - SunAnom) * EFac
     + 0.0118 * sin(2*F1);

  Q := 5.2207
     - 0.0048 * cos(SunAnom) * EFac
     + 0.0020 * cos(2*SunAnom) * EFac
     - 0.3299 * cos(MoonAnom)
     - 0.0060 * cos(SunAnom + MoonAnom) * EFac
     + 0.0041 * cos(MoonAnom - SunAnom) * EFac;

  W := abs(cos(F1));

  Gamma := (P * cos(F1) + Q * sin(F1)) * (1 - 0.0048 * W);

  Mu := 0.0059
      + 0.0046 * cos(SunAnom) * EFac
      - 0.0182 * cos(MoonAnom)
      + 0.0004 * cos(2*MoonAnom)
      - 0.0005 * cos(SunAnom + MoonAnom);

  if (Frac(abs(K)) > 0.1) then begin
{Check for Lunar Eclipse possibilities}
    PMag := (1.5573 + Mu - abs(Gamma)) / 0.5450;
    UMag := (1.0128 - Mu - abs(Gamma)) / 0.5450;

    if (UMag >= 1.0) then
      TotalLunarEclipse(CentralJD, MoonAnom, Mu, PMag, UMag, Gamma)
    else if (UMag > 0) then
      PartialLunarEclipse(CentralJD, MoonAnom, Mu, PMag, UMag, Gamma)
    else if ((UMag < 0) and (PMag > 0)) then
      PenumbralLunarEclipse(CentralJD, MoonAnom, Mu, PMag, UMag, Gamma);
  end else begin
{Check for Solar Eclipse possibilities}
{
 Non-partial eclipses
 --------------------
 central       Axis of moon's umbral shadow touches earth's surface
               Can be total, annular, or both

 non-central   Axis of moon's umbral shadow does not touch earth's surface
               Eclipse is usually partial but can be one of possibilities
               for central eclipse if very near one of the earth's poles

 Partial eclipses
 ----------------
 partial       None of the moon's umbral shadow touches the earth's surface
}

    if (abs(Gamma) <= (0.9972 + abs(Mu))) then
      NonPartialSolarEclipse(CentralJD, Mu, Gamma)
    else begin
      if (abs(Gamma) < 1.5433 + Mu) then
        PartialSolarEclipse(CentralJD, Mu, Gamma);
    end;
  end;
end;

procedure TStEclipses.TotalLunarEclipse(CentralJD, MoonAnom, Mu,
                                        PMag, UMag, Gamma : Double);
var
  TLE              : PStEclipseRecord;
  PartialSemiDur,
  TotalSemiDur,
  Dbl1             : Double;
begin
  New(TLE);
  TLE^.Magnitude  := UMag;
  TLE^.Hemisphere := htNone;
  TLE^.EType      := etLunarTotal;
  TLE^.Path       := nil;
  CentralJD := AJDToDateTime(CentralJD);

  PartialSemiDur := 1.0128 - Mu;
  TotalSemiDur   := 0.4678 - Mu;
  Dbl1 := 0.5458 + 0.0400 * cos(MoonAnom);

  PartialSemiDur := 60/Dbl1 * sqrt(sqr(PartialSemiDur) - sqr(Gamma)) / 1440;
  TotalSemiDur   := 60/Dbl1 * sqrt(sqr(TotalSemiDur) - sqr(Gamma)) / 1440;

  TLE^.LContacts.FirstContact  := CentralJD - PartialSemiDur;
  TLE^.LContacts.SecondContact := CentralJD - TotalSemiDur;
  TLE^.LContacts.MidEclipse    := CentralJD;
  TLE^.LContacts.ThirdContact  := CentralJD + TotalSemiDur;
  TLE^.LContacts.FourthContact := CentralJD + PartialSemiDur;

  PartialSemiDur := 60/Dbl1 * sqrt(sqr(1.5573 + Mu) - sqr(Gamma)) / 1440;
  TLE^.LContacts.UT1 := CentralJD - PartialSemiDur;
  TLE^.LContacts.UT2 := CentralJD + PartialSemiDur;

  Self.Append(TLE);
end;

procedure TStEclipses.PartialLunarEclipse(CentralJD, MoonAnom, Mu,
                                          PMag, UMag, Gamma : Double);
var
  TLE              : PStEclipseRecord;
  PartialSemiDur,
  Dbl1             : Double;
begin
  New(TLE);
  TLE^.Magnitude := UMag;
  TLE^.Hemisphere := htNone;
  TLE^.EType      := etLunarPartial;
  TLE^.Path       := nil;
  CentralJD := AJDToDateTime(CentralJD);

  PartialSemiDur := 1.0128 - Mu;
  Dbl1 := 0.5458 + 0.0400 * cos(MoonAnom);

  PartialSemiDur := 60/Dbl1 * sqrt(sqr(PartialSemiDur) - sqr(Gamma)) / 1440;

  TLE^.LContacts.FirstContact  := CentralJD - PartialSemiDur;
  TLE^.LContacts.SecondContact := 0;
  TLE^.LContacts.MidEclipse    := CentralJD;
  TLE^.LContacts.ThirdContact  := 0;
  TLE^.LContacts.FourthContact := CentralJD + PartialSemiDur;

  PartialSemiDur := 60/Dbl1 * sqrt(sqr(1.5573 + Mu) - sqr(Gamma)) / 1440;
  TLE^.LContacts.UT1 := CentralJD - PartialSemiDur;
  TLE^.LContacts.UT2 := CentralJD + PartialSemiDur;

  Self.Append(TLE);
end;

procedure TStEclipses.PenumbralLunarEclipse(CentralJD, MoonAnom, Mu,
                                            PMag, UMag, Gamma : Double);
var
  TLE              : PStEclipseRecord;
  PartialSemiDur,
  Dbl1             : Double;
begin
  New(TLE);
  TLE^.Magnitude := PMag;
  TLE^.Hemisphere := htNone;
  TLE^.EType      := etLunarPenumbral;
  TLE^.Path       := nil;
  CentralJD := AJDToDateTime(CentralJD);

  TLE^.LContacts.FirstContact  := 0;
  TLE^.LContacts.SecondContact := 0;
  TLE^.LContacts.MidEclipse    := CentralJD;
  TLE^.LContacts.ThirdContact  := 0;
  TLE^.LContacts.FourthContact := 0;

  Dbl1 := 0.5458 + 0.0400 * cos(MoonAnom);
  PartialSemiDur := 60/Dbl1 * sqrt(sqr(1.5573 + Mu) - sqr(Gamma)) / 1440;
  TLE^.LContacts.UT1 := CentralJD - PartialSemiDur;
  TLE^.LContacts.UT2 := CentralJD + PartialSemiDur;

  Self.Append(TLE);
end;

procedure TStEclipses.GetBesselianElements(CentralJD : Double);
var
  I,
  Mins       : Integer;
  CurJD,
  SidTime,
  SunDist,
  MoonDist,
  DistRatio,
  Alpha,
  Theta,
  Sun1,
  Sun2,
  Moon1,
  Moon2,
  Dbl3,
  F1, F2     : Double;
  DTRec      : TStDateTimeRec;
  SunXYZ     : TStSunXYZRec;
  Sun        : TStPosRec;
  Moon       : TStMoonPosRec;
begin
{compute BesselianElements every 10 minutes starting 2 hours prior to CentralJD}
{but forcing positions to be at multiple of ten minutes}
  for I := 1 to 25 do begin
    CurJD   := CentralJD + ((I-13) * (10/1440));
    DTRec.D := AstJulianDateToStDate(CurJD, True);
    if ((Frac(CurJD) + 0.5) >= 1) then
      Mins := Trunc(((Frac(CurJD) + 0.5)-1) * 1440)
    else
      Mins    := Trunc((Frac(CurJD) + 0.5) * 1440);
   {changed because, for example, both 25 and 35 rounded to 30}
    Mins    := ((Mins + 5) div 10) * 10;
    if (Mins = 1440) then
      Mins := 0;
    DTRec.T := Mins * 60;

    SidTime := SiderealTime(DTRec) / radcor;
    SunXYZ  := SunPosPrim(DTRec);
    Sun     := SunPos(DTRec);
    Moon    := MoonPos(DTRec);

    Sun1   := Sun.RA/radcor;
    Sun2   := Sun.DC/radcor;
    Moon1  := Moon.RA/radcor;
    Moon2  := Moon.DC/radcor;

    FBesselianElements[I].JD := StDateToDateTime(DTRec.D)
                              + StTimeToDateTime(DTRec.T);

    SunDist   := 1.0 / sin(8.794/SunXYZ.RV/3600.0/radcor);
    MoonDist  := 1.0 / sin(Moon.Plx/radcor);
    DistRatio := MoonDist / SunDist;

    Theta := DistRatio/cos(Sun2) * cos(Moon2) * (Moon1 - Sun1);
    Theta := Theta/(1.0-DistRatio);
    Alpha := Sun1 - Theta;

    Theta := DistRatio/(1.0 - DistRatio) * (Moon2 - Sun2);

    FBesselianElements[I].Delta := Sun2 - Theta;
    FBesselianElements[I].Angle := SidTime - Alpha;
    FBesselianElements[I].XAxis := MoonDist * cos(Moon2) * (sin(Moon1 - Alpha));

    Dbl3 := FBesselianElements[I].Delta;
    FBesselianElements[I].YAxis := MoonDist * (sin(Moon2) * cos(Dbl3)
                                 - cos(Moon2) * sin(Dbl3) * cos(Moon1 - Alpha));

    Theta := DistRatio * SunXYZ.RV;
    Theta := SunXYZ.RV - Theta;
    F1 := StInvSin(0.004664012/Theta);
    F2 := StInvSin(0.004640787/Theta);

    Theta := MoonDist * (sin(Moon2) * sin(Dbl3) + cos(Moon2)
             * cos(Dbl3) * cos(Moon1 - Alpha));
    FBesselianElements[I].L1 := (Theta + 0.272453/sin(F1)) * StTan(F1);
    FBesselianElements[I].L2 := (Theta - 0.272453/sin(F2)) * StTan(F2);

    if (I = 13) then
      F0 := StTan(F2);

    if (I = 16) then begin
      FUPrime := FBesselianElements[16].Angle - FBesselianElements[10].Angle;
      FDPrime := FBesselianElements[16].Delta - FBesselianElements[10].Delta;
    end;
  end;
end;

procedure TStEclipses.GetShadowPath(I1, I2 : Integer; Path : TStList);
var
  J,
  I3, I4,
  I5, I6         : integer;

  Delta,
  Dbl1,
  Dbl2,
  P1,
  XAxis,
  YAxis,
  Eta,
  R1, R2,
  D1, D2,
  D3, D4,
  V3, V4,
  V5, V6, V7,
  XPrime,
  YPrime      : Double;

  Position    : PStLongLat;
begin
  for J := I1 to I2 do begin
    Eta := 0.00669454;
    Delta := FBesselianElements[J].Delta;
    XAxis := FBesselianElements[J].XAxis;
    YAxis := FBesselianElements[J].YAxis;

    R1 := sqrt(1.0 - Eta * sqr(cos(Delta)));
    R2 := sqrt(1.0 - Eta * sqr(sin(Delta)));

    D1 := sin(Delta) / R1;
    D2 := sqrt(1.0 - Eta) * cos(Delta) / R1;
    D3 := Eta * sin(Delta) * cos(Delta) / R1 / R2;
    D4 := sqrt(1.0 - Eta) / R1 / R2;

    V3 := YAxis / R1;
    V4 := sqrt(1.0 - sqr(XAxis) - sqr(V3));
    V5 := R2 * (V4 * D4 - V3 * D3);
    V6 := FUPrime * (-YAxis * sin(Delta) + V5 * cos(Delta));
    V7 := FUPrime * XAxis * sin(Delta) - FDPrime * V5;

    if ((I2-I1) div 2) >= 4 then begin
      I3 := (I2-I1) div 2;
      I4 := I1 + I3;
      I5 := I4 - 3;
      I6 := I4 + 3;
      XPrime := FBesselianElements[I6].XAxis
              - FBesselianElements[I5].XAxis;
      YPrime := FBesselianElements[I6].YAxis
              - FBesselianElements[I5].YAxis;
    end else begin
      XPrime := (FBesselianElements[J+1].XAxis -
                 FBesselianElements[J-1].XAxis) * 3;
      YPrime := (FBesselianElements[J+1].YAxis -
                 FBesselianElements[J-1].YAxis) * 3;
    end;

    New(Position);
    Dbl1 := sqrt(sqr(XPrime - V6) + sqr(YPrime - V7));
    Position^.JD := FBesselianElements[J].JD;

    Dbl2 := (FBesselianElements[J].L2 - V5 * F0) / Dbl1;
    Dbl2 := abs(Dbl2 * 120);
    Position^.Duration := int(Dbl2) + frac(Dbl2) * 0.6;

    Dbl1 := -V3 * D1 + V4 * D2;
    P1 := StInvTan2(Dbl1, XAxis);

    Dbl2 := (FBesselianElements[J].Angle - P1) * radcor;
    Dbl2 := frac(Dbl2 / 360.0) * 360;
    if (Dbl2 < 0) then
      Dbl2 := Dbl2 + 360.0;
    if (Dbl2 > 180.0) and (Dbl2 < 360.0) then
       Dbl2 := Dbl2 - 360.0;
    Position^.Longitude := Dbl2;

    Dbl1 := StInvSin(V3 * D2 + V4 * D1);
    Dbl2 := ArcTan(1.003364 * StTan(Dbl1)) * radcor;
    Position^.Latitude := Dbl2;

    Path.Append(Position);
  end;
end;

procedure TStEclipses.NonPartialSolarEclipse(CentralJD, Mu, Gamma : Double);
var
  SolEc   : PStEclipseRecord;
  I1, I2  : Integer;
begin
  New(SolEc);
  if (Mu < 0) then
    SolEc^.EType := etSolarTotal
  else if (Mu > 0.0047) then
    SolEc^.EType := etSolarAnnular
  else begin
    if (Mu < (0.00464 * sqrt(1 - sqr(Gamma)))) then
      SolEc^.EType := etSolarAnnularTotal
    else
      SolEc^.EType := etSolarTotal;
  end;

  SolEc^.Magnitude := -1;
  if (Gamma > 0) then
    SolEc^.Hemisphere := htNorthern
  else
    SolEc^.Hemisphere := htSouthern;
  FillChar(SolEc^.LContacts, SizeOf(TStContactTimes), #0);
  SolEc^.LContacts.MidEclipse := AJDtoDateTime(CentralJD);

  GetBesselianElements(CentralJD);

{find limits - then go one step inside at each end}
  I1 := 1;
  while (sqr(FBesselianElements[I1].XAxis) +
         sqr(FBesselianElements[I1].YAxis) >= 1.0) and (I1 < 25) do
    Inc(I1);
  Inc(I1);

  I2 := I1;
  repeat
    if (I2 < 25) then begin
      if (sqr(FBesselianElements[I2+1].XAxis) +
          sqr(FBesselianElements[I2+1].YAxis) < 1) then
        Inc(I2)
      else
        break;
    end;
  until (sqr(FBesselianElements[I2].XAxis) +
        sqr(FBesselianElements[I2].YAxis) >= 1) or (I2 >= 25);
  Dec(I2);

{this test accounts for non-central eclipses, i.e., those that are total}
{and/or annular but the axis of the moon's shadow does not touch the earth}
  if (I1 <> I2) and (I1 < 26) and (I2 < 26) then begin
    SolEc^.Path := TStList.Create(TStListNode);
    SolEc^.Path.DisposeData := DisposePathData;
    GetShadowPath(I1, I2, SolEc^.Path);
  end else
    SolEc^.Path := nil;
  Self.Append(SolEc);
end;

procedure TStEclipses.PartialSolarEclipse(CentralJD, Mu, Gamma : Double);
var
  SolEc   : PStEclipseRecord;
begin
  New(SolEc);
  SolEc^.EType := etSolarPartial;
  SolEc^.Magnitude := (1.5433 + Mu - abs(Gamma)) / (0.5461 + 2*Mu);
  if (Gamma > 0) then
    SolEc^.Hemisphere := htNorthern
  else
    SolEc^.Hemisphere := htSouthern;
  FillChar(SolEc^.LContacts, SizeOf(TStContactTimes), #0);
  SolEc^.LContacts.MidEclipse := AJDToDateTime(CentralJD);
  SolEc^.Path := nil;
  Self.Append(SolEc);
end;


end.
