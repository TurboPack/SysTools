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
{* SysTools: StText.pas 4.04                             *}
{*********************************************************}
{* SysTools: Routines for manipulating Delphi Text files *}
{*********************************************************}

{$I StDefine.inc}

unit StText;

interface

uses
  Windows,
  SysUtils, STConst, StBase, StSystem;

function TextSeek(var F : TextFile; Target : Integer) : Boolean;
 {-Seek to the specified position in a text file opened for input}

function TextFileSize(var F : TextFile) : Integer;
  {-Return the size of a text file}

function TextPos(var F : TextFile) : Integer;
 {-Return the current position of the logical file pointer (that is,
   the position of the physical file pointer, adjusted to account for
   buffering)}

function TextFlush(var F : TextFile) : Boolean;
  {-Flush the buffer(s) for a text file}

implementation

function TextSeek(var F : TextFile; Target : Integer) : Boolean;
  {-Do a Seek for a text file opened for input. Returns False in case of I/O
    error.}
var
  Pos : Integer;
begin
  with TTextRec(F) do begin
    {assume failure}
    Result := False;
    {check for file opened for input}
    if Mode <> fmInput then Exit;
    Pos := FileSeek(Handle, 0, FILE_CURRENT);
    if Pos = -1 then Exit;
    Dec(Pos, BufEnd);
    {see if the Target is within the buffer}
    Pos := Target-Pos;
    if (Pos >= 0) and (Pos < Integer(BufEnd)) then
      {it is--just move the buffer pointer}
      BufPos := Pos
    else begin
      if FileSeek(Handle, Target, FILE_BEGIN) = -1 then Exit;
      {tell Delphi its buffer is empty}
      BufEnd := 0;
      BufPos := 0;
    end;
  end;
  {if we get to here we succeeded}
  Result := True;
end;

function TextFileSize(var F : TextFile) : Integer;
  {-Return the size of text file F. Returns -1 in case of I/O error.}
var
  Old : Integer;
  Res : Integer;
begin
  Result := -1;
  with TTextRec(F) do begin
    {check for open file}
    if Mode = fmClosed then Exit;
    {get/save current pos of the file pointer}
    Old := FileSeek(Handle, 0, FILE_CURRENT);
    if Old = -1 then Exit;
    {have OS move to end-of-file}
    Res := FileSeek(Handle, 0, FILE_END);
    if Res = -1 then Exit;
    {reset the old position of the file pointer}
    if FileSeek(Handle, Old, FILE_BEGIN) = - 1 then Exit;
  end;
  Result := Res;
end;

function TextPos(var F : TextFile) : Integer;
  {-Return the current position of the logical file pointer (that is,
    the position of the physical file pointer, adjusted to account for
    buffering). Returns -1 in case of I/O error.}
var
  Position : Integer;
begin
  Result := -1;
  with TTextRec(F) do begin
    {check for open file}
    if Mode = fmClosed then Exit;
    Position := FileSeek(Handle, 0, FILE_CURRENT);
    if Position = -1 then Exit;
  end;
  with TTextRec(F) do
    if Mode = fmOutput then     {writing}
      Inc(Position, BufPos)
    else if BufEnd <> 0 then    {reading}
      Dec(Position, BufEnd-BufPos);
  {return the calculated position}
  Result := Position;
end;

function TextFlush(var F : TextFile) : Boolean;
  {-Flush the buffer(s) for a text file. Returns False in case of I/O error.}
var
  Position : Integer;
  Code : Integer;
begin
  Result := False;
  with TTextRec(F) do begin
    {check for open file}
    if Mode = fmClosed then Exit;
    {see if file is opened for reading or writing}
    if Mode = fmInput then begin
      {get current position of the logical file pointer}
      Position := TextPos(F);
      {exit in case of I/O error}
      if Position = -1 then Exit;
      if FileSeek(Handle, Position, FILE_BEGIN) = - 1 then Exit;
    end
    else begin
      {write the current contents of the buffer, if any}
      if BufPos <> 0 then begin
        Code := FileWrite(Handle, BufPtr^, BufPos);
        if Code = -1 {<> 0} then Exit;
      end;
      {flush OS's buffers}
      if not FlushOsBuffers(Handle) then Exit;
    end;
    {tell Delphi its buffer is empty}
    BufEnd := 0;
    BufPos := 0;
  end;
  {if we get to here we succeeded}
  Result := True;
end;


end.
