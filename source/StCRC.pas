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
{* SysTools: StCRC.pas 4.04                              *}
{*********************************************************}
{* SysTools: Cyclic redundancy check unit for SysTools   *}
{*********************************************************}

{$I StDefine.inc}
{
Note: CRC routines rely on overflows for their results,
so these need to be off:
}
{$R-}
{$Q-}

unit StCRC;

interface

uses
  Windows, SysUtils, Classes,
  StBase;

const
  CrcBufSize = 2048;
  CrcFileMode = fmOpenRead or fmShareDenyWrite;

function Adler32Prim(var Data; DataSize : Cardinal; CurCrc : Integer) : Integer;
function Adler32OfStream(Stream : TStream; CurCrc : Integer) : Integer;
function Adler32OfFile(FileName : String) : Integer;

function Crc16Prim(var Data; DataSize, CurCrc : Cardinal) : Cardinal;
function Crc16OfStream(Stream : TStream; CurCrc : Cardinal) : Cardinal;
function Crc16OfFile(FileName : String) : Cardinal;

function Crc32Prim(var Data; DataSize : Cardinal; CurCrc : Integer) : Integer;
function Crc32OfStream(Stream : TStream; CurCrc : Integer) : Integer;
function Crc32OfFile(FileName : String) : Integer;

function InternetSumPrim(var Data; DataSize, CurCrc : Cardinal) : Cardinal;
function InternetSumOfStream(Stream : TStream; CurCrc : Cardinal) : Cardinal;
function InternetSumOfFile(FileName : String) : Cardinal;

function Kermit16Prim(var Data; DataSize, CurCrc : Cardinal) : Cardinal;
function Kermit16OfStream(Stream : TStream; CurCrc : Cardinal) : Cardinal;
function Kermit16OfFile(FileName : String) : Cardinal;

const
  { Cardinal takes more space, but is about 10% faster in 32-bit }
  CrcTable: array[0..255] of Cardinal = (
    $0000,  $1021,  $2042,  $3063,  $4084,  $50a5,  $60c6,  $70e7,
    $8108,  $9129,  $a14a,  $b16b,  $c18c,  $d1ad,  $e1ce,  $f1ef,
    $1231,  $0210,  $3273,  $2252,  $52b5,  $4294,  $72f7,  $62d6,
    $9339,  $8318,  $b37b,  $a35a,  $d3bd,  $c39c,  $f3ff,  $e3de,
    $2462,  $3443,  $0420,  $1401,  $64e6,  $74c7,  $44a4,  $5485,
    $a56a,  $b54b,  $8528,  $9509,  $e5ee,  $f5cf,  $c5ac,  $d58d,
    $3653,  $2672,  $1611,  $0630,  $76d7,  $66f6,  $5695,  $46b4,
    $b75b,  $a77a,  $9719,  $8738,  $f7df,  $e7fe,  $d79d,  $c7bc,
    $48c4,  $58e5,  $6886,  $78a7,  $0840,  $1861,  $2802,  $3823,
    $c9cc,  $d9ed,  $e98e,  $f9af,  $8948,  $9969,  $a90a,  $b92b,
    $5af5,  $4ad4,  $7ab7,  $6a96,  $1a71,  $0a50,  $3a33,  $2a12,
    $dbfd,  $cbdc,  $fbbf,  $eb9e,  $9b79,  $8b58,  $bb3b,  $ab1a,
    $6ca6,  $7c87,  $4ce4,  $5cc5,  $2c22,  $3c03,  $0c60,  $1c41,
    $edae,  $fd8f,  $cdec,  $ddcd,  $ad2a,  $bd0b,  $8d68,  $9d49,
    $7e97,  $6eb6,  $5ed5,  $4ef4,  $3e13,  $2e32,  $1e51,  $0e70,
    $ff9f,  $efbe,  $dfdd,  $cffc,  $bf1b,  $af3a,  $9f59,  $8f78,
    $9188,  $81a9,  $b1ca,  $a1eb,  $d10c,  $c12d,  $f14e,  $e16f,
    $1080,  $00a1,  $30c2,  $20e3,  $5004,  $4025,  $7046,  $6067,
    $83b9,  $9398,  $a3fb,  $b3da,  $c33d,  $d31c,  $e37f,  $f35e,
    $02b1,  $1290,  $22f3,  $32d2,  $4235,  $5214,  $6277,  $7256,
    $b5ea,  $a5cb,  $95a8,  $8589,  $f56e,  $e54f,  $d52c,  $c50d,
    $34e2,  $24c3,  $14a0,  $0481,  $7466,  $6447,  $5424,  $4405,
    $a7db,  $b7fa,  $8799,  $97b8,  $e75f,  $f77e,  $c71d,  $d73c,
    $26d3,  $36f2,  $0691,  $16b0,  $6657,  $7676,  $4615,  $5634,
    $d94c,  $c96d,  $f90e,  $e92f,  $99c8,  $89e9,  $b98a,  $a9ab,
    $5844,  $4865,  $7806,  $6827,  $18c0,  $08e1,  $3882,  $28a3,
    $cb7d,  $db5c,  $eb3f,  $fb1e,  $8bf9,  $9bd8,  $abbb,  $bb9a,
    $4a75,  $5a54,  $6a37,  $7a16,  $0af1,  $1ad0,  $2ab3,  $3a92,
    $fd2e,  $ed0f,  $dd6c,  $cd4d,  $bdaa,  $ad8b,  $9de8,  $8dc9,
    $7c26,  $6c07,  $5c64,  $4c45,  $3ca2,  $2c83,  $1ce0,  $0cc1,
    $ef1f,  $ff3e,  $cf5d,  $df7c,  $af9b,  $bfba,  $8fd9,  $9ff8,
    $6e17,  $7e36,  $4e55,  $5e74,  $2e93,  $3eb2,  $0ed1,  $1ef0
  );

const
  Crc32Table : array[0..255] of DWORD = (
  $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535,
  $9e6495a3, $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd,
  $e7b82d07, $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d,
  $6ddde4eb, $f4d4b551, $83d385c7, $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
  $14015c4f, $63066cd9, $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
  $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b, $35b5a8fa, $42b2986c,
  $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59, $26d930ac,
  $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
  $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab,
  $b6662d3d, $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
  $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb,
  $086d3d2d, $91646c97, $e6635c01, $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
  $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950, $8bbeb8ea,
  $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce,
  $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
  $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
  $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409,
  $ce61e49f, $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81,
  $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739,
  $9dd277af, $04db2615, $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
  $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344, $8708a3d2, $1e01f268,
  $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7, $fed41b76, $89d32be0,
  $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5, $d6d6a3e8,
  $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
  $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
  $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703,
  $220216b9, $5505262f, $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7,
  $b5d0cf31, $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
  $9c0906a9, $eb0e363f, $72076785, $05005713, $95bf4a82, $e2b87a14, $7bb12bae,
  $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
  $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777, $88085ae6,
  $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
  $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d,
  $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5,
  $47b2cf7f, $30b5ffe9, $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
  $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
  $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );

implementation

{$IFDEF TRIALRUN}
uses
  StTrial;
{$ENDIF}

type
  CRCByteArray = array[0..Pred(High(Integer))] of Byte;

function Adler32Prim(var Data; DataSize : Cardinal; CurCrc : Integer) : Integer;
  { Calculates the Adler 32-bit CRC of a block }
var
  S1, S2, I : Integer;
begin
  if DataSize > 0 then begin
    S1 := CurCrc and $FFFF;
    S2 := (CurCrc shr 16) and $FFFF;

    for I := 0 to (DataSize-1) do begin
      S1 := (S1 + CRCByteArray(Data)[I]) mod 65521;
      S2 := (S2 + S1) mod 65521;
    end;

    Result := (S2 shl 16) + S1;
  end else
    Result := CurCrc;
end;

function Adler32OfStream(Stream : TStream; CurCrc : Integer) : Integer;
  { Calculates the Adler 32-bit CRC of a stream }
var
  BufArray : array[0..(CrcBufSize-1)] of Byte;
  Res      : Integer;
begin
  {Initialize Crc}
  Result := CurCrc;
  repeat
    Res := Stream.Read(BufArray, CrcBufSize);
    Result := Adler32Prim(BufArray, Res, Result);
  until (Res <> CrcBufSize);
end;

function Adler32OfFile(FileName : String) : Integer;
  { Calculates the Adler 32-bit CRC of a file }
var
  FileSt : TFileStream;
begin
  FileSt := TFileStream.Create(FileName, CrcFileMode);
  try
    Result := Adler32OfStream(FileSt, 1);
  finally
    FileSt.Free;
  end;
end;

function Crc16Prim(var Data; DataSize, CurCrc : Cardinal) : Cardinal;
  { Calculates the 16-bit CRC of a block }
var
  I : Integer;
begin
  Result := CurCrc;
  for I := 0 to (DataSize-1) do
    Result := (CrcTable[((Result shr 8) and 255)] xor (Result shl 8) xor
      CRCByteArray(Data)[I]) and $FFFF;
end;

function Crc16OfStream(Stream : TStream; CurCrc : Cardinal) : Cardinal;
  { Calculates the 16-bit CRC of a stream }
var
  BufArray : array[0..(CrcBufSize-1)] of Byte;
  Res      : Integer;
begin
  {Initialize Crc}
  Result := CurCrc;
  repeat
    Res := Stream.Read(BufArray, CrcBufSize);
    Result := Crc16Prim(BufArray, Res, Result);
  until (Res <> CrcBufSize);
end;

function Crc16OfFile(FileName : String) : Cardinal;
  { Calculates the 16-bit CRC of a file }
var
  FileSt : TFileStream;
begin
  FileSt := TFileStream.Create(FileName, CrcFileMode);
  try
    Result := Crc16OfStream(FileSt, 0);
  finally
    FileSt.Free;
  end;
end;

function Crc32Prim(var Data; DataSize : Cardinal; CurCrc : Integer) : Integer;
  { Calculates the 32-bit CRC of a block }
var
  I : Integer;
begin
  Result := CurCrc;
  for I := 0 to (DataSize-1) do
    Result := Crc32Table[Byte(Result xor CRCByteArray(Data)[I])] xor
      DWord((Result shr 8) and $00FFFFFF);
end;

function Crc32OfStream(Stream : TStream; CurCrc : Integer) : Integer;
  { Calculates the 32-bit CRC of a stream }
var
  BufArray : array[0..(CrcBufSize-1)] of Byte;
  Res      : Integer;
begin
  {Initialize Crc}
  Result := CurCrc;
  repeat
    Res := Stream.Read(BufArray, CrcBufSize);
    Result := Crc32Prim(BufArray, Res, Result);
  until (Res <> CrcBufSize);
end;

function Crc32OfFile(FileName : String) : Integer;
  { Calculates the 32-bit CRC of a file }
var
  FileSt : TFileStream;
begin
  FileSt := TFileStream.Create(FileName, CrcFileMode);
  try
    Result := not Crc32OfStream(FileSt, Integer($FFFFFFFF));
  finally
    FileSt.Free;
  end;
end;

function InternetSumPrim(var Data; DataSize, CurCrc : Cardinal) : Cardinal;
  { Calculates the Internet Checksum of a block }
var
  I : Integer;
begin
  Result := CurCrc;
  if DataSize = 0 then Exit;
  for I := 0 to (DataSize - 1) do begin
    if Odd(I) then
      Result := Result + (CRCByteArray(Data)[I] shl 8)
    else
      Result := Result + CRCByteArray(Data)[I];
  end;
  Result := (not((Result and $FFFF) + (Result shr 16))) and $FFFF;
end;

function InternetSumOfStream(Stream : TStream; CurCrc : Cardinal) : Cardinal;
  { Calculates the Internet Checksum of a stream }
var
  BufArray : array[0..(CrcBufSize-1)] of Byte;
  Res      : Integer;
begin
  {Initialize Crc}
  Result := CurCrc;
  repeat
    Res := Stream.Read(BufArray, CrcBufSize);
    Result := InternetSumPrim(BufArray, Res, Result);
  until (Res <> CrcBufSize);
end;

function InternetSumOfFile(FileName : String) : Cardinal;
  { Calculates the Internet Checksum of a file }
var
  FileSt : TFileStream;
begin
  FileSt := TFileStream.Create(FileName, CrcFileMode);
  try
    Result := InternetSumOfStream(FileSt, 0);
  finally
    FileSt.Free;
  end;
end;

function Kermit16Prim(var Data; DataSize, CurCrc : Cardinal) : Cardinal;
  { Calculates the Kermit 16-bit CRC of a block }
var
  I, J : Integer;
  Temp : Cardinal;
  CurrByte : Byte;
begin
  for I := 0 to (DataSize-1) do begin
    CurrByte := CRCByteArray(Data)[I];
    for J := 0 to 7 do begin
      Temp := CurCrc xor CurrByte;
      CurCrc := CurCrc shr 1;
      if Odd(Temp) then
        CurCrc := CurCrc xor $8408;
      CurrByte := CurrByte shr 1;
    end;
  end;
  Result := CurCrc;
end;

function Kermit16OfStream(Stream : TStream; CurCrc : Cardinal) : Cardinal;
  { Calculates the Kermit 16-bit CRC of a stream }
var
  BufArray : array[0..(CrcBufSize-1)] of Byte;
  Res      : Integer;
begin
  {Initialize Crc}
  Result := CurCrc;
  repeat
    Res := Stream.Read(BufArray, CrcBufSize);
    Result := Kermit16Prim(BufArray, Res, Result);
  until (Res <> CrcBufSize);
end;

function Kermit16OfFile(FileName : String) : Cardinal;
  { Calculates the Kermit 16-bit CRC of a file }
var
  FileSt : TFileStream;
begin
  FileSt := TFileStream.Create(FileName, CrcFileMode);
  try
    Result := Kermit16OfStream(FileSt, 0);
  finally
    FileSt.Free;
  end;
end;


end.
