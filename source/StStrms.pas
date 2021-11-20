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
{* SysTools: StStrms.pas 4.04                            *}
{*********************************************************}
{* SysTools: Specialized Stream Classes for SysTools     *}
{*********************************************************}

{$I StDefine.inc}

unit StStrms;

interface

uses
  Windows,
  SysUtils,
  Classes,

  StBase,
  StConst;

type
  TStMemSize = Integer;

  TStBufferedStream = class(TStream)
    private
      FBufCount: TStMemSize;   {count of valid bytes in buffer}
      FBuffer  : PAnsiChar;    {buffer into underlying stream}
      FBufOfs  : Integer;      {offset of buffer in underlying stream}
      FBufPos  : TStMemSize;   {current position in buffer}
      FBufSize : TStMemSize;   {size of buffer}
      FDirty   : boolean;      {has data in buffer been changed?}
      FSize    : Int64;      {size of underlying stream}
      FStream  : TStream;      {underlying stream}
    protected
      procedure bsSetStream(aValue : TStream);

      procedure bsInitForNewStream; virtual;
      function bsReadChar(var aCh : AnsiChar) : boolean;
      procedure bsReadFromStream;
      procedure bsWriteToStream;

      procedure SetSize(NewSize : Integer); override;
    public
      constructor Create(aStream : TStream);
      constructor CreateEmpty;
      destructor Destroy; override;

      function Read(var Buffer; Count : Integer) : Integer; override;
      function Seek(Offset : Integer; Origin : word) : Integer; override;
      function Write(const Buffer; Count : Integer) : Integer; override;

      property FastSize : Int64 read FSize;
      property Stream : TStream read FStream write bsSetStream;

   end;

{!!.01 - moved to StBase.pas }
(*
  TStLineTerminator = ( {possible line terminators...}
     ltNone,            {..no terminator, ie fixed length lines}
     ltCR,              {..carriage return (#13)}
     ltLF,              {..line feed (#10)}
     ltCRLF,            {..carriage return/line feed (#13/#10)}
     ltOther);          {..another character}
*)
{!!.01 - end moved }


  // TODO-UNICODE: add TStUnicodeTextStream

  TStAnsiTextStream = class(TStBufferedStream)
    private
      FLineEndCh   : AnsiChar;
      FLineLen     : integer;
      FLineTerm    : TStLineTerminator;
      FFixedLine   : PAnsiChar;
      FLineCount   : Integer;
      FLineCurrent : Integer;
      FLineCurOfs  : Integer;
      FLineIndex   : TList;
      FLineInxStep : Integer;
      FLineInxTop  : integer;
    protected
      function atsGetLineCount : Integer;

      procedure atsSetLineTerm(aValue : TStLineTerminator);
      procedure atsSetLineEndCh(aValue : AnsiChar);
      procedure atsSetLineLen(aValue : integer);

      procedure atsGetLine(var aStartPos : Integer;
                           var aEndPos   : Integer;
                           var aLen      : Integer);
      procedure atsResetLineIndex;

      procedure bsInitForNewStream; override;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      function AtEndOfStream : boolean;

      function ReadLine : AnsiString;
      function ReadLineArray(aCharArray : PAnsiChar; aLen : TStMemSize)
                                                          : TStMemSize;
      function ReadLineZ(aSt : PAnsiChar; aMaxLen : TStMemSize) : PAnsiChar;

      function SeekNearestLine(aOffset : Integer) : Integer;
      function SeekLine(aLineNum : Integer) : Integer;

      procedure WriteLine(const aSt : UnicodeString); overload;
      procedure WriteLine(const aSt : AnsiString); overload;
      procedure WriteLineArray(aCharArray : PAnsiChar; aLen : TStMemSize);
      procedure WriteLineZ(aSt : PAnsiChar);

      property FixedLineLength : integer
                  read FLineLen write atsSetLineLen;
      property LineCount : Integer
                  read atsGetLineCount;
      property LineTermChar : AnsiChar
                  read FLineEndCh write atsSetLineEndCh;
      property LineTerminator : TStLineTerminator
                  read FLineTerm write atsSetLineTerm;
  end;

  TStMemoryMappedFile = class(TStream)
  protected {private}
    FBuffer     : Pointer;
    FHeaderSize : Word;
    FDataSize   : Cardinal;
    FHandle     : THandle;
    FMapObj     : THandle;
    FMaxHi      : Cardinal;
    FMaxLo      : Cardinal;
    FMutex      : THandle;
    FPos        : Cardinal;
    FReadOnly   : Boolean;
    FSharedData : Boolean;

  protected
    function GetDataSize : Cardinal;

  public
    constructor Create(const FileName   : string;                      {!!.02}
                             MaxSize    : Cardinal;
                             ReadOnly   : Boolean;
                             SharedData : Boolean);
    destructor Destroy; override;

    function Read(var Buffer; Count : Integer) : Integer; override;
    function Seek(Offset : Integer; Origin : Word) : Integer; override;
    function Write(const Buffer; Count : Integer) : Integer; override;

    property DataSize : Cardinal
      read GetDataSize;

    property MaxSize : Cardinal
      read FMaxLo;

    property Position : Cardinal
      read FPos;

    property ReadOnly : Boolean
      read FReadOnly;

    property SharedData : Boolean
      read FSharedData;
  end;

implementation

uses
  AnsiStrings;

const
  LineTerm : array [TStLineTerminator] of
               array [0..1] of AnsiChar =
                 ('', #13, #10, #13#10, '');

const
  LineIndexCount = 1024;
  LineIndexMax   = pred(LineIndexCount);


{--- Helper routines ---------------------------------------------------------}

function MinLong(A, B : Integer) : Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;


{-----------------------------------------------------------------------------}
{                          TStBufferedStream                                  }
{-----------------------------------------------------------------------------}

constructor TStBufferedStream.Create(aStream : TStream);
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 4096;
  GetMem(FBuffer, FBufSize);

  {save the stream}
  if (aStream = nil) then
    RaiseStError(EStBufStreamError, stscNilStream);
  FStream := aStream;

  bsInitForNewStream;
end;

{-----------------------------------------------------------------------------}

constructor TStBufferedStream.CreateEmpty;
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 4096;
  GetMem(FBuffer, FBufSize);

  bsInitForNewStream
end;

{-----------------------------------------------------------------------------}

destructor TStBufferedStream.Destroy;
begin
  if (FBuffer <> nil) then begin
    if FDirty and (FStream <> nil) then
      bsWriteToStream;
    FreeMem(FBuffer, FBufSize);
  end;

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsInitForNewStream;
begin
  if (FStream <> nil) then
    FSize := FStream.Size
  else
    FSize := 0;
  FBufCount := 0;
  FBufOfs := 0;
  FBufPos := 0;
  FDirty := false;
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.bsReadChar(var aCh : AnsiChar) : boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := false;
    Exit;
  end;
  {if we get here, we'll definitely read a character}
  Result := true;
  {make sure that the buffer has some data in it}
  if (FBufCount = 0) then
    bsReadFromStream
  else if (FBufPos = FBufCount) then begin
    if FDirty then
      bsWriteToStream;
    FBufPos := 0;
    inc(FBufOfs, FBufSize);
    bsReadFromStream;
  end;
  {get the next character}
  aCh := AnsiChar(FBuffer[FBufPos]);
  inc(FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsReadFromStream;
var
  NewPos : Integer;
begin
  {assumptions: FBufOfs is where to read the buffer
                FBufSize is the number of bytes to read
                FBufCount will be the number of bytes read}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
    RaiseStError(EStBufStreamError, stscNoSeekForRead);
  FBufCount := FStream.Read(FBuffer^, FBufSize);
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsSetStream(aValue : TStream);
begin
  if (aValue <> FStream) then begin
    {if the buffer is dirty, flush it to the current stream}
    if FDirty and (FStream <> nil) then
      bsWriteToStream;
    {remember the stream and initialize all fields}
    FStream := aValue;
    bsInitForNewStream;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.bsWriteToStream;
var
  NewPos       : Integer;
  BytesWritten : Integer;
begin
  {assumptions: FDirty is true
                FBufOfs is where to write the buffer
                FBufCount is the number of bytes to write
                FDirty will be set false afterwards}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
    RaiseStError(EStBufStreamError, stscNoSeekForWrite);
  BytesWritten := FStream.Write(FBuffer^, FBufCount);
  if (BytesWritten <> FBufCount) then
    RaiseStError(EStBufStreamError, stscCannotWrite);
  FDirty := false;
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.Read(var Buffer; Count : Integer) : Integer;
var
  BytesToGo   : Integer;
  BytesToRead : Integer;
//  BufAsBytes  : TByteArray absolute Buffer;                          {!!.02}
//  DestPos     : Integer;                                             {!!.02}
  BufAsBytes  : PByte;                                                 {!!.02}
begin
  BufAsBytes := @Buffer;                                               {!!.02}

  if (FStream = nil) then
    RaiseStError(EStBufStreamError, stscNilStream);
  {calculate the number of bytes we could read if possible}
  BytesToGo := MinLong(Count, FSize - (FBufOfs + FBufPos));
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to read some data after all?}
  if (BytesToGo > 0) then begin
    {make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {read as much as we can from the current buffer}
    BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
    {transfer that number of bytes}
//    Move(FBuffer[FBufPos], BufAsBytes[0], BytesToRead);              {!!.02}
    Move(FBuffer[FBufPos], BufAsBytes^, BytesToRead);                  {!!.02}
    {update our counters}
    inc(FBufPos, BytesToRead);
    dec(BytesToGo, BytesToRead);
    {if we have more bytes to read then we've reached the end of the
     buffer and so we need to read another, and another, etc}
//    DestPos := 0;                                                    {!!.02}
    while BytesToGo > 0 do begin
      {if the current buffer is dirty, write it out}
      if FDirty then
        bsWriteToStream;
      {position and read the next buffer}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      bsReadFromStream;
      {calculate the new destination position, and the number of bytes
       to read from this buffer}
//      inc(DestPos, BytesToRead);                                     {!!.02}
      Inc(BufAsBytes, BytesToRead);                                    {!!.02}
      BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
      {transfer that number of bytes}
//      Move(FBuffer[FBufPos], BufAsBytes[DestPos], BytesToRead);      {!!.02}
      Move(FBuffer[FBufPos], BufAsBytes^, BytesToRead);                {!!.02}

      {update our counters}
      inc(FBufPos, BytesToRead);
      dec(BytesToGo, BytesToRead);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.Seek(Offset : Integer; Origin : word) : Integer;
var
  NewPos : Integer;
  NewOfs : Integer;
begin
  if (FStream = nil) then
    RaiseStError(EStBufStreamError, stscNilStream);
  {optimization: to help code that just wants the current stream
   position (ie, reading the Position property), check for this as a
   special case}
  if (Offset = 0) and (Origin = soFromCurrent) then begin
    Result := FBufOfs + FBufPos;
    Exit;
  end;
  {calculate the desired position}
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := (FBufOfs + FBufPos) + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
    RaiseStError(EStBufStreamError, stscBadOrigin);
    NewPos := 0; {to fool the compiler's warning--we never get here}
  end;
  {force the new position to be valid}
  if (NewPos < 0) then
    NewPos := 0
  else if (NewPos > FSize) then
    NewPos := FSize;
  {calculate the offset for the buffer}
  NewOfs := (NewPos div FBufSize) * FBufSize;
  {if the offset differs, we have to move the buffer window}
  if (NewOfs <> FBufOfs) then begin
    {check to see whether we have to write the current buffer to the
     original stream first}
    if FDirty then
      bsWriteToStream;
    {mark the buffer as empty}
    FBufOfs := NewOfs;
    FBufCount := 0;
  end;
  {set the position within the buffer}
  FBufPos := NewPos - FBufOfs;
  Result := NewPos;
end;

{-----------------------------------------------------------------------------}

procedure TStBufferedStream.SetSize(NewSize : Integer);
var
  NewPos : Integer;
begin
  {get rid of the simple case first where the new size and the old
   size are the same}
  if (NewSize = FSize) then
    Exit;
  {if the buffer is dirty, write it out}
  if FDirty then
    bsWriteToStream;
  {now set the size of the underlying stream}
  FStream.Size := NewSize;
  {patch up the buffer fields so that the buffered stream points to
   somewhere in the newly resized stream}
  NewPos := FBufOfs + FBufPos;
  if (NewPos > NewSize) then
    NewPos := NewSize;
  bsInitForNewStream;
  Seek(NewPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStBufferedStream.Write(const Buffer; Count : Integer) : Integer;
var
  BytesToGo   : Integer;
  BytesToWrite: Integer;
//  BufAsBytes  : TByteArray absolute Buffer;                          {!!.02}
//  DestPos     : Integer;                                             {!!.02}
  BufAsBytes  : PByte;                                                 {!!.02}
begin
  BufAsBytes := @Buffer;                                               {!!.02}

  if (FStream = nil) then
    RaiseStError(EStBufStreamError, stscNilStream);
  {calculate the number of bytes we should be able to write}
  BytesToGo := Count;
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to write some data?}
  if (BytesToGo > 0) then begin
    {try and make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {write as much as we can to the current buffer}
    BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
    {transfer that number of bytes}
//    Move(BufAsBytes[0], FBuffer[FBufPos], BytesToWrite);             {!!.02}
    Move(BufAsBytes^, FBuffer[FBufPos], BytesToWrite);                 {!!.02}
    FDirty := true;
    {update our counters}
    inc(FBufPos, BytesToWrite);
    if (FBufCount < FBufPos) then begin
      FBufCount := FBufPos;
      FSize := FBufOfs + FBufPos;
    end;
    dec(BytesToGo, BytesToWrite);
    {if we have more bytes to write then we've reached the end of the
     buffer and so we need to write another, and another, etc}
//    DestPos := 0;                                                    {!!.02}
    while BytesToGo > 0 do begin
      {as the current buffer is dirty, write it out}
      bsWriteToStream;
      {position and read the next buffer, if required}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      if (FBufOfs < FSize) then
        bsReadFromStream
      else
        FBufCount := 0;
      {calculate the new destination position, and the number of bytes
       to write to this buffer}
//      inc(DestPos, BytesToWrite);                                    {!!.02}
      Inc(BufAsBytes, BytesToWrite);                                   {!!.02}
      BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
      {transfer that number of bytes}
//      Move(BufAsBytes[DestPos], FBuffer[0], BytesToWrite);           {!!.02}
      Move(BufAsBytes^, FBuffer[0], BytesToWrite);                     {!!.02}
      FDirty := true;
      {update our counters}
      inc(FBufPos, BytesToWrite);
      if (FBufCount < FBufPos) then begin
        FBufCount := FBufPos;
        FSize := FBufOfs + FBufPos;
      end;
      dec(BytesToGo, BytesToWrite);
    end;
  end;
end;

{-----------------------------------------------------------------------------}
{                           TStAnsiTextStream                                 }
{-----------------------------------------------------------------------------}

constructor TStAnsiTextStream.Create(aStream : TStream);
begin
  inherited Create(aStream);

  {set up the line index variables}
  atsResetLineIndex;
end;

{-----------------------------------------------------------------------------}

destructor TStAnsiTextStream.Destroy;
begin
  {if needed, free the fixed line buffer}
  if (FFixedLine <> nil) then
    FreeMem(FFixedLine, FixedLineLength);
  {free the line index}
  FLineIndex.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.AtEndOfStream : boolean;
begin
  Result := FSize = (FBufOfs + FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsGetLine(var aStartPos : Integer;
                                       var aEndPos   : Integer;
                                       var aLen      : Integer);
var
  Done   : boolean;
  Ch     : AnsiChar;
  PrevCh : AnsiChar;
begin
  if (LineTerminator = ltNone) then begin
    aStartPos := FBufOfs + FBufPos;
    aEndPos := Seek(aStartPos + FixedLineLength, soFromBeginning);
    aLen := aEndPos - aStartPos;
  end
  else begin
    aStartPos := FBufOfs + FBufPos;
    Ch := #0;
    Done := false;
    while not Done do begin
      PrevCh := Ch;
      if not bsReadChar(Ch) then begin
        Done := true;
        aEndPos := FBufOfs + FBufPos;
        aLen := aEndPos - aStartPos;
      end
      else begin
        case LineTerminator of
          ltNone : {this'll never get hit};
          ltCR   : if (Ch = #13) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
          ltLF   : if (Ch = #10) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
          ltCRLF : if (Ch = #10) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                     if PrevCh = #13 then
                       aLen := aEndPos - aStartPos - 2
                     else
                       aLen := aEndPos - aStartPos - 1;
                   end;
          ltOther: if (Ch = LineTermChar) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
        else
          RaiseStError(EStBufStreamError, stscBadTerminator);
        end;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.atsGetLineCount : Integer;
begin
  if FLineCount < 0 then
    Result := MaxLongInt
  else
    Result := FLineCount;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsResetLineIndex;
begin
  {make sure we have a line index}
  if (FLineIndex = nil) then begin
    FLineIndex := TList.Create;  {create the index: even elements are}
    FLineIndex.Count := LineIndexCount * 2; {linenums, odd are offsets}

    {if we didn't have a line index, set up some reasonable defaults}
    FLineTerm := ltCRLF;  {normal Windows text file terminator}
    FLineEndCh := #10;    {not used straight away}
    FLineLen := 80;       {not used straight away}
  end;
  FLineIndex[0] := pointer(0); {the first line is line 0 and...}
  FLineIndex[1] := pointer(0); {...it starts at position 0}
  FLineInxTop := 0;            {the top valid index}
  FLineInxStep := 1;           {step count before add a line to index}
  FLineCount := -1;            {number of lines (-1 = don't know)}
  FLineCurrent := 0;           {current line}
  FLineCurOfs := 0;            {current line offset}
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsSetLineTerm(aValue : TStLineTerminator);
begin
  if (aValue <> LineTerminator) and ((FBufOfs + FBufPos) = 0) then begin
    {if there was no terminator, free the line buffer}
    if (LineTerminator = ltNone) then begin
      FreeMem(FFixedLine, FixedLineLength);
      FFixedLine := nil;
    end;
    {set the new value}
    FLineTerm := aValue;
    {if there is no terminator now, allocate the line buffer}
    if (LineTerminator = ltNone) then begin
      GetMem(FFixedLine, FixedLineLength);
    end;
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsSetLineEndCh(aValue : AnsiChar);
begin
  if ((FBufOfs + FBufPos) = 0) then begin
    FLineEndCh := aValue;
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.atsSetLineLen(aValue : integer);
begin
  if (aValue <> FixedLineLength) and ((FBufOfs + FBufPos) = 0) then begin
    {validate the new length first}
    if (aValue < 1) or (aValue > 1024) then
      RaiseStError(EStBufStreamError, stscBadLineLength);

    {set the new value; note that if there is no terminator we need to
     free the old line buffer, and then allocate a new one}
    if (LineTerminator = ltNone) then
      FreeMem(FFixedLine, FixedLineLength);
    FLineLen := aValue;
    if (LineTerminator = ltNone) then
      GetMem(FFixedLine, FixedLineLength);
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.bsInitForNewStream;
begin
  inherited bsInitForNewStream;
  atsResetLineIndex;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.ReadLine : AnsiString;
var
  CurPos : Integer;
  EndPos : Integer;
  Len    : Integer;
  StLen  : Integer;
begin
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    SetLength(Result, StLen);
    if (Len < StLen) then
      FillChar(Result[Len+1], StLen-Len, ' ');
  end
  else {LineTerminator is not ltNone} begin
    SetLength(Result, Len);
  end;
  {read the line}
  if Len > 0 then begin
    Seek(CurPos, soFromBeginning);
    Read(Result[1], Len);
  end
  else {it's a blank line }
    Result := '';
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.ReadLineArray(aCharArray : PAnsiChar;
                                         aLen       : TStMemSize)
                                                    : TStMemSize;
var
  CurPos : Integer;
  EndPos : Integer;
  Len    : Integer;
  StLen  : Integer;
begin
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    if (StLen > aLen) then
      StLen := aLen;
    if (Len < StLen) then
      FillChar(aCharArray[Len], StLen-Len, ' ');
    Result := StLen;
  end
  else {LineTerminator is not ltNone} begin
    if (Len > aLen) then
      Len := aLen;
    Result := Len;
  end;
  Seek(CurPos, soFromBeginning);
  Read(aCharArray[0], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.ReadLineZ(aSt : PAnsiChar; aMaxLen : TStMemSize) : PAnsiChar;
var
  CurPos : Integer;
  EndPos : Integer;
  Len    : Integer;
  StLen  : Integer;
begin
  Result := aSt;
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    if (StLen > aMaxLen) then
      StLen := aMaxLen;
    if (Len < StLen) then
      FillChar(Result[Len], StLen-Len, ' ');
    Result[StLen] := #0;
  end
  else {LineTerminator is not ltNone} begin
    if (Len > aMaxLen) then
      Len := aMaxLen;
    Result[Len] := #0;
  end;
  Seek(CurPos, soFromBeginning);
  Read(Result[0], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.SeekNearestLine(aOffset : Integer) : Integer;
var
  CurLine : Integer;
  CurOfs  : Integer;
  CurPos  : Integer;
  EndPos  : Integer;
  Len     : Integer;
  i       : Integer;
  Done    : boolean;
  L, R, M : integer;
begin
  {if the offset we want is for the current line, reposition at the
   current line offset, return the current line number and exit}
  if (aOffset = FLineCurOfs) then begin
    Seek(FLineCurOfs, soFromBeginning);
    Result := FLineCurrent;
    Exit;
  end;
  {if the offset requested is less than or equal to zero, just
   position at line zero (ie, the start of the stream)}
  if (aOffset <= 0) then begin
    Seek(0, soFromBeginning);
    FLineCurrent := 0;
    FLineCurOfs := 0;
    Result := 0;
    Exit;
  end;
  {if the offset requested is greater than or equal to the size of the
   stream, position at the end of the stream (note that if we don't
   know the number of lines in the stream yet, FLineCount is set to
   -1 and we can't take this shortcut because we need to return the
   true value)}
  if (FLineCount >= 0) and (aOffset >= FSize) then begin
    Seek(0, soFromEnd);
    FLineCurrent := FLineCount;
    FLineCurOfs := FSize;
    Result := FLineCount;
    Exit;
  end;
  {if the offset requested is greater than the top item in the
   line index, we shall have to build up the index until we get to the
   line we require, or just beyond}
  if (aOffset > Integer(FLineIndex[FLineInxTop+1])) then begin
    {position at the last known line offset}
    CurLine := Integer(FLineIndex[FLineInxTop]);
    CurOfs := Integer(FLineIndex[FLineInxTop+1]);
    Seek(CurOfs, soFromBeginning);
    Done := false;
    {continue reading lines in chunks of FLineInxStep and add an index
     entry for each chunk}
    while not Done do begin
      for i := 0 to pred(FLineInxStep) do begin
        atsGetLine(CurPos, EndPos, Len);
        inc(CurLine);
        CurOfs := EndPos;
        if (EndPos = FSize) then begin
          Done := true;
          Break;
        end;
      end;
      if Done then
        FLineCount := CurLine
      else begin
        inc(FLineInxTop, 2);
        if (FLineInxTop = (LineIndexCount * 2)) then begin
          {we've exhausted the space in the index: rescale}
          FLineInxTop := FLineInxTop div 2;
          for i := 0 to pred(FLineInxTop) do begin
            if Odd(i) then
              FLineIndex.Exchange((i*2)-1, i)
            else
              FLineIndex.Exchange(i*2, i);
          end;
          FLineInxStep := FLineInxStep * 2;
        end;
        FLineIndex[FLineInxTop] := pointer(CurLine);
        FLineIndex[FLineInxTop+1] := pointer(CurOfs);
        if (aOffset <= CurOfs) then
          Done := true;
      end;
    end;
  end;
  {we can now work out where the nearest item in the index is to the
   line we require}
  L := 1;
  R := FLineInxTop+1;
  while (L <= R) do begin
    M := (L + R) div 2;
    if not Odd(M) then
      inc(M);
    if (aOffset < Integer(FLineIndex[M])) then
      R := M - 2
    else if (aOffset > Integer(FLineIndex[M])) then
      L := M + 2
    else begin
      FLineCurrent := Integer(FLineIndex[M-1]);
      FLineCurOfs := Integer(FLineIndex[M]);
      Seek(FLineCurOfs, soFromBeginning);
      Result := FLineCurrent;
      Exit;
    end;
  end;
  {the item at L-2 will have the nearest smaller offset than the
   one we want, hence the nearest smaller line is at L-3; start here
   and read through the stream forwards}
  CurLine := Integer(FLineIndex[L-3]);
  Seek(Integer(FLineIndex[L-2]), soFromBeginning);
  while true do begin
    atsGetLine(CurPos, EndPos, Len);
    inc(CurLine);
    if (EndPos > aOffset) then begin
      FLineCurrent := CurLine - 1;
       FLineCurOfs := CurPos;
      Seek(CurPos, soFromBeginning);
      Result := CurLine - 1;
      Exit;
    end
    else if (CurLine = FLineCount) or (EndPos = aOffset) then begin
      FLineCurrent := CurLine;
      FLineCurOfs := EndPos;
      Seek(EndPos, soFromBeginning);
      Result := CurLine;
      Exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TStAnsiTextStream.SeekLine(aLineNum : Integer) : Integer;
var
  CurLine : Integer;
  CurOfs  : Integer;
  CurPos  : Integer;
  EndPos  : Integer;
  Len     : Integer;
  i       : Integer;
  Done    : boolean;
  L, R, M : integer;
begin
  {if the line number we want is the current line, reposition at the
   current line offset, return the current line number and exit}
  if (aLineNum = FLineCurrent) then begin
    Seek(FLineCurOfs, soFromBeginning);
    Result := FLineCurrent;
    Exit;
  end;
  {if the line number requested is less than or equal to zero, just
   position at line zero (ie, the start of the stream)}
  if (aLineNum <= 0) then begin
    Seek(0, soFromBeginning);
    FLineCurrent := 0;
    FLineCurOfs := 0;
    Result := 0;
    Exit;
  end;
  {if the line number requested is greater than or equal to the line
   count, position at the end of the stream (note that if we don't
   know the number of lines in the stream yet, FLineCount is set to
   -1)}
  if (FLineCount >= 0) and (aLineNum > FLineCount) then begin
    Seek(0, soFromEnd);
    FLineCurrent := FLineCount;
    FLineCurOfs := FSize;
    Result := FLineCount;
    Exit;
  end;
  {if the line number requested is greater than the top item in the
   line index, we shall have to build up the index until we get to the
   line we require, or just beyond}
  if (aLineNum > Integer(FLineIndex[FLineInxTop])) then begin
    {position at the last known line offset}
    CurLine := Integer(FLineIndex[FLineInxTop]);
    CurOfs := Integer(FLineIndex[FLineInxTop+1]);
    Seek(CurOfs, soFromBeginning);
    Done := false;
    {continue reading lines in chunks of FLineInxStep and add an index
     entry for each chunk}
    while not Done do begin
      for i := 0 to pred(FLineInxStep) do begin
        atsGetLine(CurPos, EndPos, Len);
        inc(CurLine);
        CurOfs := EndPos;
        if (EndPos = FSize) then begin
          Done := true;
          Break;
        end;
      end;
      if Done then
        FLineCount := CurLine
      else begin
        inc(FLineInxTop, 2);
        if (FLineInxTop = (LineIndexCount * 2)) then begin
          {we've exhausted the space in the index: rescale}
          FLineInxTop := FLineInxTop div 2;
          for i := 0 to pred(FLineInxTop) do begin
            if Odd(i) then
              FLineIndex.Exchange((i*2)-1, i)
            else
              FLineIndex.Exchange(i*2, i);
          end;
          FLineInxStep := FLineInxStep * 2;
        end;
        FLineIndex[FLineInxTop] := pointer(CurLine);
        FLineIndex[FLineInxTop+1] := pointer(CurOfs);
        if (aLineNum <= CurLine) then
          Done := true;
      end;
    end;
  end;
  {we can now work out where the nearest item in the index is to the
   line we require}
  L := 0;
  R := FLineInxTop;
  while (L <= R) do begin
    M := (L + R) div 2;
    if Odd(M) then
      dec(M);
    if (aLineNum < Integer(FLineIndex[M])) then
      R := M - 2
    else if (aLineNum > Integer(FLineIndex[M])) then
      L := M + 2
    else begin
      FLineCurrent := Integer(FLineIndex[M]);
      FLineCurOfs := Integer(FLineIndex[M+1]);
      Seek(FLineCurOfs, soFromBeginning);
      Result := FLineCurrent;
      Exit;
    end;
  end;
  {the item at L-2 will have the nearest smaller line number than the
   one we want; start here and read through the stream forwards}
  CurLine := Integer(FLineIndex[L-2]);
  Seek(Integer(FLineIndex[L-1]), soFromBeginning);
  while true do begin
    atsGetLine(CurPos, EndPos, Len);
    inc(CurLine);
    if (CurLine = FLineCount) or (CurLine = aLineNum) then begin
      FLineCurrent := CurLine;
      FLineCurOfs := EndPos;
      Seek(EndPos, soFromBeginning);
      Result := CurLine;
      Exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.WriteLine(const aSt : AnsiString);
var
  Len : Integer;
begin
  Len := Length(aSt);
  if Len > 0 then
    WriteLineArray(PAnsiChar(aSt), Len)
  else
    WriteLineArray('', 0);
end;

procedure TStAnsiTextStream.WriteLine(const aSt : UnicodeString);
begin
  WriteLine(AnsiString(ASt));
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.WriteLineArray(aCharArray : PAnsiChar;
                                           aLen       : TStMemSize);
var
  C : AnsiChar;
begin
  if (aCharArray = nil) then
    aLen := 0;
  if (LineTerminator = ltNone) then begin
    if (aLen >= FixedLineLength) then
      Write(aCharArray[0], FixedLineLength)
    else begin
      FillChar(FFixedLine[aLen], FixedLineLength-aLen, ' ');
      if (aLen > 0) then
        Move(aCharArray[0], FFixedLine[0], aLen);
      Write(FFixedLine[0], FixedLineLength);
    end;
  end
  else begin
    if (aLen > 0) then
      Write(aCharArray[0], aLen);
    case LineTerminator of
      ltNone : {this'll never get hit};
      ltCR   : Write(LineTerm[ltCR], 1);
      ltLF   : Write(LineTerm[ltLF], 1);
      ltCRLF : Write(LineTerm[ltCRLF], 2);
      ltOther: begin
                 C := LineTermChar;
                 Write(C, 1);
               end;
    else
      RaiseStError(EStBufStreamError, stscBadTerminator);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TStAnsiTextStream.WriteLineZ(aSt : PAnsiChar);
var
  LenSt : TStMemSize;
begin
  if (aSt = nil) then
    LenSt := 0
  else
    LenSt := AnsiStrings.StrLen(aSt);
  WriteLineArray(aSt, LenSt);
end;


{-----------------------------------------------------------------------------}
{                           TStMemoryMappedFile                               }
{-----------------------------------------------------------------------------}

constructor TStMemoryMappedFile.Create(const FileName   : string;      {!!.02}
                                             MaxSize    : Cardinal;
                                             ReadOnly   : Boolean;
                                             SharedData : Boolean);
var
  RO1,
  RO2,
  RO3,
  RO4,
  FHi    : DWORD;
  SetSize: Boolean;
begin
  inherited Create;

  FMutex := CreateMutex(nil, False, nil);
  FSharedData := SharedData;
  if (FSharedData) then
    FHeaderSize := SizeOf(Word) + SizeOf(Cardinal)
  else
    FHeaderSize := 0;

  FReadOnly := ReadOnly;
  if (SharedData) then
    FReadOnly := False;
  if (FReadOnly) then begin
    RO1 := GENERIC_READ;
    RO2 := FILE_ATTRIBUTE_READONLY;
    RO3 := PAGE_READONLY;
    RO4 := FILE_MAP_READ;
    FMaxHi := 0;
    FMaxLo := 0;
  end else begin
    RO1 := GENERIC_READ or GENERIC_WRITE;
    RO2 := FILE_ATTRIBUTE_NORMAL;
    RO3 := PAGE_READWRITE;
    RO4 := FILE_MAP_WRITE;
    FMaxHi := 0;
    FMaxLo := MaxSize;
  end;

  if (not SharedData) then begin
    FHandle := CreateFile(PChar(FileName),
                          RO1,
                          FILE_SHARE_READ or FILE_SHARE_WRITE,
                          nil,
                          OPEN_ALWAYS,
                          RO2,
                          0);

    if (FHandle = INVALID_HANDLE_VALUE) then
      RaiseStError(EStMMFileError, stscCreateFileFailed);

    {reset FMaxLo if file is read/write and less < FileSize}
    {the result is that the file size cannot be changed but the contents can}
    {still be modified}
    FDataSize := GetFileSize(FHandle, @FHi);
    if (FDataSize <> $FFFFFFFF) then begin
      if (not ReadOnly) and (FDataSize > FMaxLo) then
        FMaxLo := FDataSize;
    end else begin
      CloseHandle(FHandle);
      RaiseStError(EStMMFileError, stscGetSizeFailed);
    end;
  end else
    FDataSize := 0;

  if (not SharedData) then begin
    FMapObj := CreateFileMapping(FHandle, nil, RO3, FMaxHi, FMaxLo, nil);
    SetSize := False;
  end else begin
    if (FMaxLo > (High(Cardinal) - FHeaderSize)) then
      FMaxLo := High(Cardinal) - FHeaderSize
    else
      FMaxLo := FMaxLo + FHeaderSize;
    FMapObj := CreateFileMapping(THandle($FFFFFFFF), nil, RO3,
                                 FMaxHi, FMaxLo, 'STMMFILE1');
    SetSize := (GetLastError = ERROR_ALREADY_EXISTS);
  end;

  if (FMapObj = INVALID_HANDLE_VALUE) then
    RaiseStError(EStMMFileError, stscFileMappingFailed);

  FBuffer := MapViewOfFile(FMapObj, RO4, 0, 0, FMaxLo);
  if (not Assigned(FBuffer)) then
    RaiseStError(EStMMFileError, stscCreateViewFailed);

  if (SharedData) then begin
    if (SetSize) then
      Move(PByteArray(FBuffer)[SizeOf(Word)-1], FDataSize, SizeOf(Cardinal))
    else begin
      Move(FHeaderSize, PByteArray(FBuffer)[0], SizeOf(Word));
      FDataSize := 0;
      Move(FDataSize, PByteArray(FBuffer)[SizeOf(Word)-1], SizeOf(Cardinal));
    end;
  end;
  {set position to beginning}
  FPos := FHeaderSize;
end;

{-----------------------------------------------------------------------------}

destructor TStMemoryMappedFile.Destroy;
begin
{Close the View and Mapping object}
  UnmapViewOfFile(FBuffer);
  FBuffer := nil;
  CloseHandle(FMapObj);

  if (not SharedData) then begin
{set the file pointer to the end of the actual data}
    SetFilePointer(FHandle, FDataSize, nil, FILE_BEGIN);
{set the EOF marker to the end of actual data}
    SetEndOfFile(FHandle);
    CloseHandle(FHandle);
  end;

  {now the Mutex can be cleared}
  CloseHandle(FMutex);
  FMutex := 0;

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.GetDataSize : Cardinal;
begin
  Move(PByteArray(FBuffer)[SizeOf(Word)-1], FDataSize, SizeOf(Cardinal));
  Result := FDataSize;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.Read(var Buffer; Count : Integer) : Integer;
var
//  ByteArray : TByteArray absolute Buffer;                            {!!.02}
  ByteArray : PByte;                                                   {!!.02}
begin
  ByteArray := @Buffer;                                                {!!.02}
  {check to make sure that the read does not go beyond the actual data}
  if (((FPos-FHeaderSize) + DWORD(Count)) > FDataSize) then
    Count := FDataSize - FPos + FHeaderSize;

  if (SharedData) then begin
    WaitForSingleObject(FMutex, INFINITE);
    try
//      Move(PByteArray(FBuffer)[FPos], ByteArray[0], Count);          {!!.02}
      Move(PByteArray(FBuffer)[FPos], ByteArray^, Count);              {!!.02}
      Inc(FPos, Count);
      Result := Count;
    finally
      ReleaseMutex(FMutex);
    end;
  end else begin
//    Move(PByteArray(FBuffer)[FPos], ByteArray[0], Count);            {!!.02}
    Move(PByteArray(FBuffer)[FPos], ByteArray^, Count);                {!!.02}
    Inc(FPos, Count);
    Result := Count;
  end;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.Write(const Buffer; Count : Integer) : Integer;
var
//  ByteArray : TByteArray absolute Buffer;                            {!!.02}
  ByteArray : PByte;                                                   {!!.02}
begin
  ByteArray := @Buffer;                                                {!!.02}
  if (ReadOnly) then begin
    Result := 0;
    Exit;
  end;

  {check that the write does not go beyond the maximum file size}
  if ((FPos + DWORD(Count)) > pred(FMaxLo)) then
    Count := pred(FMaxLo - FPos);

  if (SharedData) then begin
    WaitForSingleObject(FMutex, INFINITE);
    try
//      Move(ByteArray[0], PByteArray(FBuffer)[FPos], Count);          {!!.02}
      Move(ByteArray^, PByteArray(FBuffer)[FPos], Count);              {!!.02}
      Inc(FPos, Count);
      {if the write went beyond the previous end of data, update FDataSize}
      if ((FPos-FHeaderSize) > FDataSize) then
        FDataSize := FPos-FHeaderSize;
      Move(FDataSize, PByteArray(FBuffer)[SizeOf(Word)-1], SizeOf(Cardinal));
      Result := Count;
    finally
      ReleaseMutex(FMutex);
    end;
  end else begin
//    Move(ByteArray[0], PByteArray(FBuffer)[FPos], Count);            {!!.02}
    Move(ByteArray^, PByteArray(FBuffer)[FPos], Count);                {!!.02}
    Inc(FPos, Count);
    {if the write went beyond the previous end of data, update FDataSize}
    if ((FPos-FHeaderSize) > FDataSize) then
      FDataSize := FPos-FHeaderSize;
    Move(FDataSize, PByteArray(FBuffer)[SizeOf(Word)-1], SizeOf(Cardinal));
    Result := Count;
  end;
end;

{-----------------------------------------------------------------------------}

function TStMemoryMappedFile.Seek(Offset : Integer; Origin : Word) : Integer;
begin
  if (SharedData) then begin
    WaitForSingleObject(FMutex, INFINITE);
    try
      case Origin of
        {$WARNINGS OFF}
        soFromBeginning : FPos := Offset + FHeaderSize;
        soFromCurrent   : FPos := FPos + Offset + FHeaderSize;
        {the seek should be based on actual data, not the mapped size since}
        {the "data" between FDataSize and the mapped size is undefined}
        soFromEnd       : FPos := FDataSize + Offset + FHeaderSize;
        {$WARNINGS ON}
      else
        RaiseStError(EStMMFileError, stscBadOrigin);
      end;

      {force the new position to be valid}
      if ((FPos-FHeaderSize) > FDataSize) then
        FPos := FDataSize + FHeaderSize;
      Result := FPos;
    finally
      ReleaseMutex(FMutex);
    end;
  end else begin
    {$WARNINGS OFF}
    case Origin of
      soFromBeginning : FPos := Offset + FHeaderSize;
      soFromCurrent   : FPos := FPos + Offset + FHeaderSize;
      {the seek should be based on actual data, not the mapped size since}
      {the "data" between FDataSize and the mapped size is undefined}
      soFromEnd       : FPos := FDataSize + Offset + FHeaderSize;
    else
      RaiseStError(EStMMFileError, stscBadOrigin);
    end;
    {$WARNINGS ON}

    {force the new position to be valid}
    if ((FPos-FHeaderSize) > FDataSize) then
      FPos := FDataSize + FHeaderSize;
    Result := FPos;
  end;
end;

{-----------------------------------------------------------------------------}

end.

