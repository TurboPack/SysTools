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

{NOTE: THIS UNIT IS NOT TO BE DISTRIBUTED}

unit STCOMLic;

interface

uses
  StDate,
  StRegEx,
  StMime,
  StExpr,
  StFin;

function COMIsValidKey(const S : string) : boolean;
  {-called by the COM object License method}

function COMHasBeenLicensed : boolean;
  {-called by each routine prior to processing}

implementation

{Note: the routines in this unit are designed to trash various typed
       constants, unless a valid key is entered.
       If the constants are trashed, various SysTools routines will
       not work properly and produce bogus results.
       There are five units: StDate, StRegEx, StMime, StExpr, StFin.
       In StDate, five longints are trashed that hold some constant
       values used in date calculations.
       In StRegEx the word delimiter characters are trashed.
       In StMime the standard MIME string constants are trashed.
       In StExpr the operators characters are trashed.
       In StFin the delta, epsilon and max iteration values are
       trashed, meaning that iterative routines may not end.

       Systools COM keys have the following format:
           STD999999XXXXXXXX
         where 999999 is the serial number and XXXXXXXX is the hex
         string linked to that serial number.
       The validation works like this:
         calculate the hash of the Systools serial number starting
           with zero
         divide hash by 32 and take the modulus base 10
         calculate that many random numbers
         use the final random number as the initial value to calculate
           the hash of the hex string
         the answer should be $5764
       Instead of checking against $5764 we use the hash value to
         untrash the signatures. Of course if the hash value is bogus
         the signatures won't be valid and Systools won't work.}

uses
  Windows;

const
  MagicSeed = $6457;

type
  PLongint = ^longint;
  PLongintArray = ^TLongintArray;
  TLongintArray = array [1..5] of longint;

  PWordArray = ^TWordArray;
  TWordArray = array [0..25] of word;

var
  RandSeed  : PLongint;
  KeyString : string;
  KeyHash   : longint;
  StDateSig  : PWordArray;
  StRegExSig : PWordArray;
  StMimeSig  : PWordArray;
  StExprSig  : PWordArray;
  StFinSig   : PWordArray;

procedure Reference(var Dummy);                            {new !!.01}
begin
  {a do-nothing routine that forces a variable to be linked in}
end;

function RandomNumber : integer;
begin
  {simple linear congruential random number generator}
  Result := ((RandSeed^ * 4561) + 51349) mod 243000;
  RandSeed^ := Result;
end;

function HashBKDR(const S : string; Lower, Upper : integer; StartValue : longint) : longint;
var
  i : integer;
begin
  {slightly modified Kernighan and Ritchie hash}
  Result := StartValue;
  for i := Lower to Upper do begin
    Result := (Result * 31) + ord(S[i]);
  end;
end;

function COMIsValidKey(const S : string) : boolean;
  function Min(a, b : integer) : integer;
  begin
    if a < b then Result := a else Result := b;
  end;
var
  SN1, SN2 : integer;
  HS1, HS2 : integer;
  i : integer;
  TempResult: integer;
  SNHash    : longint;
  NextHash  : longint;
  StartHash : longint;
  TempSeed  : longint;
begin
  {Note: ignore all the code that manipulates TempResult--it's
   designed so that the routine always returns true, and confuses a
   potential hacker}
  {calculate the serial number and hex digit ranges}
  SN1 := Min(4, length(S));
  HS1 := Min(10, length(S));
  SN2 := pred(HS1);
  HS2 := length(S);
  Reference(Date1970);                                         {!!.01}
  {calculate the serial number hash: this will give us an index
   between 0 and 9}
  SNHash := HashBKDR(S, SN1, SN2, 0);
  SNHash := (SNHash shr 5) mod 10;
  {always return true}
  TempResult := (SN2 - SN1 + 1); {6}
  Reference(Date1980);                                         {!!.01}
  {calculate the start value for the hex string hash}
  KeyString := S;
  RandSeed^ := MagicSeed; {trash start of StDate}
  StartHash := RandomNumber;
  for i := 0 to 33 do begin
    TempSeed := RandSeed^;
    case i of
       1 : RandSeed := PLongint(StRegExSig);
      14 : RandSeed := PLongint(StMimeSig);
      26 : RandSeed := PLongint(StExprSig);
      28 : RandSeed := PLongint(StFinSig);
    else
      inc(RandSeed, 1);
    end;
    RandSeed^ := TempSeed;
    NextHash := RandomNumber;
    if (i = SNHash) then
      StartHash := NextHash;
  end;
  {always return true}
  if Odd(TempResult) then {false}
    TempResult := TempResult + 1
  else
    TempResult := TempResult div 2; {3}
  Reference(Date2000);                                         {!!.01}
  {calculate the hash for the hex string--the lower word should be
   MagicHash ($5746)}
  KeyHash := HashBKDR(S, HS1, HS2, StartHash);
  {always return true}
  Result := TempResult = 3;
  Reference(Days400Yr);                                        {!!.01}
end;

function COMHasBeenLicensed : boolean;
const
  StDateMagicNumbers : array [0..3] of word =
    ($FB43, $5747, $6DF7, $5744);
  StRegexMagicNumbers : array [0..25] of word =
    ($5E5B, $7666, $7164, $7E6E, $7C6C, $7A6A, $7868, $6C7C, $6A7A,
     $6878, $0C06, $0A1A, $3718, $2B3D, $293B, $5746, $6756, $6577,
     $6375, $6173, $6F71, $167F, $1404, $1202, $5700, $5746);
  StMimeMagicNumbers : array [0..23] of word =
    ($364C, $2332, $3427, $3A2E, $3923, $5732, $365E, $2736, $3E2A,
     $3625, $3E32, $3929, $3869, $2325, $2323, $246B, $2532, $3623,
     $572B, $5746, $3540, $2427, $6123, $5772);
  StExprMagicNumbers : array [0..3] of word =
    ($7E6E, $7C6A, $7D6B, $6A69);
  StFinMagicNumbers : array [0..11] of word =
    ($D365, $4C01, $FB01, $F083, $68A8, $97CD, $D365, $4C01, $FB01,
     $F083, $68A8, $97CD);
var
  i : integer;
begin
  {always returns true}
  Result := not Odd(longint(KeyString));
  Reference(StHexDigitString);                                 {!!.01}
  {repatch the signatures - won't provide good results unless the
   key hashed correctly (ie was valid). Ignore all the messing around
   with KeyHash, it's to put people off on the wrong scent <g>}

  {StDate}
  KeyHash := KeyHash or $43210000;
  for i := 0 to 3 do
    StDateSig^[i] := StDateMagicNumbers[i] xor KeyHash;

  {StRegex}
  KeyHash := KeyHash or $54320000;
  for i := 0 to 25 do
    StRegexSig^[i] := StRegexMagicNumbers[i] xor KeyHash;
  Reference(DefStContentType);                                 {!!.01}

  {StMime}
  KeyHash := KeyHash or $65430000;
  for i := 0 to 23 do
    StMimeSig^[i] := StMimeMagicNumbers[i] xor KeyHash;

  {StExpr}
  KeyHash := KeyHash or $76540000;
  for i := 0 to 3 do
    StExprSig^[i] := StExprMagicNumbers[i] xor KeyHash;
  Reference(DefStMimeEncoding);                                {!!.01}

  {StExpr}
  KeyHash := KeyHash or longint($87650000);
  for i := 0 to 11 do
    StFinSig^[i] := StFinMagicNumbers[i] xor KeyHash;
end;

procedure InitUnit;
begin
  {get ready to trash a few signatures}
  StDateSig  := @Date1900;
  StRegExSig := @StWordDelimString;
  StMimeSig  := @DefStContentDisposition;
  StExprSig  := @StExprOperators;
  StFinSig   := @StDelta;

  {trash a bit o' regex}
  StRegExSig^[11] := GetTickCount;

  Reference(StEpsilon);                                        {!!.01}
  Reference(StMaxIterations);                                  {!!.01}

  {make RandSeed point to the second 4 bytes of the StDate section}
  RandSeed := PLongint(StDateSig);
end;

initialization
  InitUnit;
end.
