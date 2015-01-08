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
{* SysTools: StPluto.pas 4.04                            *}
{*********************************************************}
{* SysTools: Astronomical Routines (for Pluto)           *}
{*********************************************************}

{$I StDefine.inc}

unit StPluto;

interface

uses
  StAstroP, StMath;

function ComputePluto(JD : Double) : TStEclipticalCord;

implementation

function ComputePluto(JD : Double) : TStEclipticalCord;
var
  T,
  J, S, P,
  L,
  B,
  R  : Double;
begin
  T := (JD - 2451545.0) / 36525.0;
  J := ( 34.35 + 3034.9057 * T) / radcor;
  S := ( 50.08 + 1222.1138 * T) / radcor;
  P := (238.96 +  144.9600 * T) / radcor;

  L :=
     -  19798886 * sin(P)        + 19848454 * cos(P)
     +    897499 * sin(2*P)      -  4955707 * cos(2*P)
     +    610820 * sin(3*P)      +  1210521 * cos(3*P)
     -    341639 * sin(4*P)      -   189719 * cos(4*P)
     +    129027 * sin(5*P)      -    34863 * cos(5*P)
     -     38215 * sin(6*P)      +    31061 * cos(6*P)
     +     20349 * sin(S-P)      -     9886 * cos(S-P)
     -      4045 * sin(S)        -     4904 * cos(S)
     -      5885 * sin(S+P)      -     3238 * cos(S+P)
     -      3812 * sin(S+2*P)    +     3011 * cos(s+2*P)
     -       601 * sin(S+3*P)    +     3468 * cos(S+3*P)
     +      1237 * sin(2*(S-P))  +      463 * cos(2*(S-P))
     +      1086 * sin(2*S-P)    -      911 * cos(2*S-P)
     +       595 * sin(2*S)      -     1229 * cos(2*S)
     +      2484 * sin(J-S)      -      485 * cos(J-S)
     +       839 * sin(J-S+P)    -     1414 * cos(J-S+P)
     -       964 * sin(J-3*P)    +     1059 * cos(J-3*P)
     -      2303 * sin(J-2*P)    -     1038 * cos(J-2*P)
     +      7049 * sin(J-P)      +      747 * cos(J-P)
     +      1179 * sin(J)        -      358 * cos(J)
     +       393 * sin(J+P)      -       63 * cos(J+P)
     +       111 * sin(J+2*P)    -      268 * cos(J+2*P)
     -        52 * sin(J+3*P)    -      154 * cos(J+3*P)
     -        78 * sin(J+4*P)    -       30 * cos(J+4*P)
     -        34 * sin(J+S-3*P)  -       26 * cos(J+S-3*P)
     -        43 * sin(J+S-2*P)  +        1 * cos(J+S-2*P)
     -        15 * sin(J+S-P)    +       21 * cos(J+S-P)
     -         1 * sin(J+S)      +       15 * cos(J+S)
     +         4 * sin(J+S+P)    +        7 * cos(J+S+P)
     +         1 * sin(J+S+3*P)  +        5 * cos(J+S+3*P)
     +         8 * sin(2*J-6*P)  +        3 * cos(2*J-6*P)
     -         3 * sin(2*J-5*P)  +        6 * cos(2*J-5*P)
     +         6 * sin(2*J-4*P)  -       13 * cos(2*J-4*P)
     +        10 * sin(2*J-3*P)  +       22 * cos(2*J-3*P)
     -        57 * sin(2*J-2*P)  -       32 * cos(2*J-2*P)
     +       157 * sin(2*J-P)    -       46 * cos(2*J-P)
     +        12 * sin(2*J)      -       18 * cos(2*J)
     -         4 * sin(2*J-P)    +        8 * cos(2*J-P)
     -         5 * sin(2*(J+P))  +        0 * sin(2*(J+P))
     +         3 * sin(2*J+3*P)  +        4 * cos(2*J+3*P)
     -         1 * sin(3*J-2*P)  -        1 * cos(3*J-2*P)
     +         6 * sin(3*J-P)    -        3 * cos(3*J-P)
     -         1 * sin(3*J)      -        2 * cos(3*J);
  Result.L0 := (238.956785 + 144.96*T + (L/1000000)) / radcor;

  B := -5453098 * sin(P)        -  14974876 * cos(P)
     +  3527363 * sin(2*P)      +   1672673 * cos(2*P)
     -  1050939 * sin(3*P)      +    327763 * cos(3*P)
     +   178691 * sin(4*P)      -    291925 * cos(4*P)
     +    18763 * sin(5*P)      +    100448 * cos(5*P)
     -    30594 * sin(6*P)      -     25838 * cos(6*P)
     +     4965 * sin(S-P)      +     11263 * cos(S-P)
     +      310 * sin(S)        -       132 * cos(S)
     +     2036 * sin(S+P)      -       947 * cos(S+P)
     -        2 * sin(S+2*P)    -       674 * cos(S+2*P)
     -      329 * sin(S+3*P)    -       563 * cos(S+3*P)
     -       64 * sin(2*(S-P))  +        39 * cos(2*(S-P))
     -       94 * sin(2*S-P)    +       210 * cos(2*S-P)
     -        8 * sin(2*S)      -       160 * cos(2*S)
     +      177 * sin(J-S)      +       259 * cos(J-S)
     +       17 * sin(J-S+P)    +       234 * cos(J-S+P)
     +      582 * sin(J-3*P)    -       285 * cos(J-3*P)
     -      298 * sin(J-2*P)    +       692 * cos(J-2*P)
     +      157 * sin(J-P)      +       201 * cos(J-P)
     +      304 * sin(J)        +       825 * cos(J)
     -      124 * sin(J+P)      -        29 * cos(J+P)
     +       15 * sin(J+2*P)    +         8 * cos(J+2*P)
     +        7 * sin(J+3*P)    +        15 * cos(J+3*P)
     +        2 * sin(J+4*P)    +         2 * cos(J+4*P)
     +        4 * sin(J+S-3*P)  +         2 * cos(J+S-3*P)
     +        3 * sin(J+S-2*P)  +         0 * cos(J+S-2*P)
     +        1 * sin(J+S-P)    -         1 * cos(J+S-P)
     +        0 * sin(J+S)      -         2 * cos(J+S)
     +        1 * sin(J+S+P)    -         0 * cos(J+S+P)
     +        1 * sin(J+S+3*P)  -         1 * cos(J+S+3*P)
     -        2 * sin(2*J-6*P)  -         3 * cos(2*J-6*P)
     +        1 * sin(2*J-5*P)  +         2 * cos(2*J-5*P)
     -        8 * sin(2*J-4*P)  +         2 * cos(2*J-4*P)
     +       10 * sin(2*J-3*P)  -         7 * cos(2*J-3*P)
     +        0 * sin(2*J-2*P)  +        21 * cos(2*J-2*P)
     +        8 * sin(2*J-P)    +         5 * cos(2*J-P)
     +       13 * sin(2*J)      +        16 * cos(2*J)
     -        2 * sin(2*J-P)    -         3 * cos(2*J-P)
     +        0 * sin(2*(J+P))  +         0 * cos(2*(J+P))
     +        0 * sin(2*J+3*P)  +         1 * cos(2*J+3*P)
     +        0 * sin(3*J-2*P)  +         1 * cos(3*J-2*P)
     +        0 * sin(3*J-P)    +         0 * cos(3*J-P)
     +        0 * sin(3*J)      +         1 * cos(3*J);
  Result.B0 := (-3.908202 + B/1000000) / radcor;

  R := 66867334 * sin(P)        +  68955876 * cos(P)
     - 11826086 * sin(2*P)      -    333765 * cos(2*P)
     +  1593657 * sin(3*P)      -   1439953 * cos(3*P)
     -    18948 * sin(4*P)      +    482443 * cos(4*P)
     -    66634 * sin(5*P)      -     85576 * cos(5*P)
     +    30841 * sin(6*P)      -      5765 * cos(6*P)
     -     6140 * sin(S-P)      +     22254 * cos(S-P)
     +     4434 * sin(S)        +      4443 * cos(S)
     -     1518 * sin(S+P)      +       641 * cos(S+P)
     -        5 * sin(S+2*P)    +       792 * cos(s+2*P)
     +      518 * sin(S+3*P)    +       518 * cos(S+3*P)
     -       13 * sin(2*(S-P))  -       221 * cos(2*(S-P))
     +      837 * sin(2*S-P)    -       494 * cos(2*S-P)
     -      281 * sin(2*S)      +       616 * cos(2*S)
     +      260 * sin(J-S)      -       395 * cos(J-S)
     -      191 * sin(J-S+P)    -       396 * cos(J-S+P)
     -     3218 * sin(J-3*P)    +       370 * cos(J-3*P)
     +     8019 * sin(J-2*P)    -      7689 * cos(J-2*P)
     +      105 * sin(J-P)      +     45637 * cos(J-P)
     +     8623 * sin(J)        +      8444 * cos(J)
     -      896 * sin(J+P)      -       801 * cos(J+P)
     +      208 * sin(J+2*P)    -       122 * cos(J+2*P)
     -      133 * sin(J+3*P)    +        65 * cos(J+3*P)
     -       16 * sin(J+4*P)    +         1 * cos(J+4*P)
     -       22 * sin(J+S-3*P)  +         7 * cos(J+S-3*P)
     -        8 * sin(J+S-2*P)  +        16 * cos(J+S-2*P)
     +        2 * sin(J+S-P)    +         9 * cos(J+S-P)
     +       12 * sin(J+S)      +         5 * cos(J+S)
     +        1 * sin(J+S+P)    -         3 * cos(J+S+P)
     +        1 * sin(J+S+3*P)  +         0 * cos(J+S+3*P)
     +        9 * sin(2*J-6*P)  +         5 * cos(2*J-6*P)
     +        2 * sin(2*J-5*P)  -         1 * cos(2*J-5*P)
     +       14 * sin(2*J-4*P)  +        10 * cos(2*J-4*P)
     -       65 * sin(2*J-3*P)  +        12 * cos(2*J-3*P)
     +      126 * sin(2*J-2*P)  -       233 * cos(2*J-2*P)
     +      270 * sin(2*J-P)    +      1068 * cos(2*J-P)
     +      254 * sin(2*J)      +       155 * cos(2*J)
     -       26 * sin(2*J-P)    -         2 * cos(2*J-P)
     +        7 * sin(2*(J+P))  +         0 * cos(2*(J+P))
     -       11 * sin(2*J+3*P)  +         4 * cos(2*J+3*P)
     +        4 * sin(3*J-2*P)  -        14 * cos(3*J-2*P)
     +       18 * sin(3*J-P)    +        35 * cos(3*J-P)
     +       13 * sin(3*J)      +         3 * cos(3*J);
  Result.R0 := 40.7247248 + R / 10000000;
end;


end.
