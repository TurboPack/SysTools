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
 *  alexrayne <alexraynepe196@gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

(*  this is Embarcadero RAD 10 delphi RTL System.Hash implementation *)

unit System.Hash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ;

type

(* this BobJenkins implementation takes from HashLib4Pascal:HlpOneAtTime  *)
THashBobJenkins = object
public
    class function Create(): THashBobJenkins; static;
    procedure Reset(AInitialValue: Integer = 0);

    class function GetHashValue(const AData: string): Integer; overload; static;
    class function GetHashValue(const AData; ALength: Integer;
                                    AInitialValue: Integer = 0): Integer; overload; static;
protected
    _hash   : Integer;
    procedure accumulate(const src; len: Integer);
    function final() :Integer; inline;
end;

implementation

procedure THashBobJenkins.Reset(AInitialValue: Integer);
begin
  _hash := AInitialValue;
end;

class function THashBobJenkins.Create: THashBobJenkins;
begin
  result.reset();
end;

procedure THashBobJenkins.accumulate(const src; len: Integer);
var
  LIdx: Int32;
  srcb: PByte;
begin
  srcb := PByte(src);
  LIdx := len;
  while len > 0 do begin
    _hash := _hash + srcb[LIdx];
    _hash := _hash + (_hash shl 10);
    _hash := _hash xor (_hash shr 6);
    Inc(LIdx);
    Dec(len);
  end;
end;

function THashBobJenkins.final() : Integer;
var
   y    : Integer;
begin
  y := _hash + (_hash shl 3);
  y := y xor (y shr 11);
  y := y + (y shl 15);
  result := y;
end;


class function THashBobJenkins.GetHashValue(const AData: string): Integer;
var
  tmp : THashBobJenkins;
begin
  result := GetHashValue( PChar(adata)^, length(adata)*sizeof(adata[1]) );
end;

class function THashBobJenkins.GetHashValue(const AData; ALength: Integer;
                                AInitialValue: Integer): Integer;
var
  tmp : THashBobJenkins;
begin
  tmp := create();
  tmp.reset(AInitialValue);
  tmp.accumulate( adata, alength );
  result := tmp.final();
end;


end.

