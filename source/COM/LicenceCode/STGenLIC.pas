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

unit STGenLIC;

interface

function GenerateHexString(const aSerialNumber : string) : string;

implementation            

uses
  Windows,
  SysUtils;

const
  MagicSeed = $6457;
  MagicHash = $5746;

var
  RandSeed : longint;

function RandomNumber : integer;
begin
  Result := ((RandSeed * 4561) + 51349) mod 243000;
  RandSeed := Result;
end;

function HashBKDR(const S : string; Lower, Upper : integer; StartValue : longint) : longint;
var
  i : integer;
begin
  Result := StartValue;
  for i := Lower to Upper do begin
    Result := (Result * 31) + ord(S[i]);
  end;
end;

function IsDigit(aCh : char) : boolean;
begin
  Result := ('0' <= aCh) and (aCh <= '9');
end;

procedure IncString(var S : string);
var
  Done : boolean;
  i    : integer;
begin
  i := 8;
  repeat
    Done := true;
    if (S[i] = '9') then
      S[i] := 'A'
    else if (S[i] = 'F') then begin
      S[i] := '0';
      dec(i);
      if (i <> 0) then
        Done := false;
    end
    else
      inc(S[i])
  until Done;
end;


function GenerateHexString(const aSerialNumber : string) : string;
var
  i : integer;
  SNHash    : longint;
  StartHash : longint;
  NextDigit : integer;
begin
  {validate the serial number}
  if (length(aSerialNumber) <> 6) then
    raise Exception.Create('GenerateHexString: serial number must be 6 digits');
  for i := 1 to 6 do
    if not IsDigit(aSerialNumber[i]) then
      raise Exception.Create('GenerateHexString: serial number not all digits');
  {calculate the serial number hash: this will give us an index
   between 0 and 9}
  SNHash := HashBKDR(aSerialNumber, 1, 6, 0);
  SNHash := (SNHash shr 5) mod 10;
  {calculate a hex string that matches the serial number}
  RandSeed := MagicSeed;
  StartHash := RandomNumber;
  for i := 0 to SNHash do
    StartHash := RandomNumber;
  {randomize}
  RandSeed := GetTickCount;
  {create a random hex string}
  SetLength(Result, 8);
  for i := 1 to 8 do begin
    NextDigit := (RandomNumber and $F000) shr 12;
    if NextDigit <= 9 then
      Result[i] := char(ord('0') + NextDigit)
    else
      Result[i] := char(ord('A') + NextDigit - 10)
  end;
  while true do begin
    if (HashBKDR(Result, 1, 8, StartHash) and $FFFF) = MagicHash then
      Exit;
    IncString(Result);
  end;
end;

end.
