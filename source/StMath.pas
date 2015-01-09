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
{* SysTools: StMath.pas 4.04                             *}
{*********************************************************}
{* SysTools: Miscellaneous math functions                *}
{*********************************************************}

{$I StDefine.inc}

unit StMath;

interface

uses
  Windows,
  SysUtils, StDate, StBase, StConst;

const
  RadCor : Double = 57.29577951308232;    {number of degrees in a radian}

function StInvCos(X : Double): Double;
  {-Returns the ArcCos of Y}

function StInvSin(Y : Double): Double;
  {-Returns the ArcSin of Y}

  function StInvTan2(X, Y : Double) : Double;
  {-Returns the ArcTangent of Y / X}

function StTan(A : Double) : Double;
  {-Returns the Tangent of A}


{-------------------------------------------------------}

implementation

{-------------------------------------------------------}

function StTan(A : Double) : Double;
var
  C, S : Double;
begin
  C := Cos(A);
  S := Sin(A);
  if (Abs(C) >= 5E-12) then
    Result := S / C
  else if (C < 0) then
    Result := 5.0e-324
  else
    Result := 1.7e+308;
end;

{-------------------------------------------------------}

function StInvTan2(X, Y : Double) : Double;
begin
  if (Abs(X) < 5.0E-12) then begin
    if (X < 0) then
      Result := 3 * Pi / 2
    else
      Result := Pi / 2;
  end else begin
    Result := ArcTan(Y / X);
    if (X < 0) then
      Result := Result + Pi
    else if (Y < 0) then
      Result := Result + 2 * Pi;
  end;
end;

{-------------------------------------------------------}

function StInvSin(Y : Double): Double;
begin
  if (Abs(Abs(Y) - 1) > 5.0E-12) then
    Result := ArcTan(Y / Sqrt(1 - Y * Y))
  else begin
    if (Y < 0) then
      Result := 3 * Pi / 2
    else
      Result := Pi / 2;
  end;
end;

{-------------------------------------------------------}

function StInvCos(X : Double): Double;
begin
  if (Abs(Abs(X) - 1) > 5.0E-12) then
    Result := (90 / RadCor) - ArcTan(X / Sqrt(1 - X * X))
  else begin
    if ((X - Pi / 2) > 0) then
      Result := 0
    else
      Result := Pi;
  end;
end;


end.
