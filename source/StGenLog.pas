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
{* SysTools: StGenLog.pas 4.04                           *}
{*********************************************************}
{* SysTools: General Logging                             *}
{*********************************************************}

{$I StDefine.inc}

unit StGenLog;

interface

uses
  Windows, SysUtils, Classes, StBase;

const

  StDefBufferSize = 65536;   { Default buffer size }
  StDefHighLevel = 0;        { Default high level point }
  StMaxLogSize = 16000000;   { Max size of general log buffer }
//  StCRLF = #10#13;                                                      {!!.01}
  StCRLF = #13#10;                                                      {!!.01}
  StLogFileFooter = '';
  StLogFileHeader = 'SysTools General Log' + StCRLF +
    '=============================================================================' +
    StCRLF + StCRLF;

  { General log constants }
  leEnabled   = 1;
  leDisabled  = 2;

  leString    = DWORD($80000000);

type

  TStGetLogStringEvent = procedure(Sender : TObject; const D1, D2, D3, D4 : DWORD;
    var LogString : AnsiString) of object;

  TStWriteMode = (wmOverwrite, wmAppend);

  { Record for log entries }
  PStLogRec = ^TStLogRec;
  TStLogRec = record
    lrTime  : DWORD;
    lrData1 : DWORD;
    lrData2 : DWORD;
    lrData3 : DWORD;
    lrData4 : DWORD;
  end;

  PStLogBuffer = ^TStLogBuffer;
  TStLogBuffer = array[0..StMaxLogSize] of Byte;

  StGenOptions = (goSuppressEnableMsg, goSuppressDisableMsg);          {!!.01}
  StGenOptionSet = set of StGenOptions;                                {!!.01}

  TStGeneralLog = class(TStComponent)
  private
    { Property variables }
    FBufferSize : DWORD;
    FEnabled : Boolean;
    FFileName : TFileName;
    FHighLevel : Byte;
    FLogFileFooter : string;
    FLogFileHeader : string;
    FLogOptions : StGenOptionSet;                                      {!!.01}
    FWriteMode : TStWriteMode;
    { Event variables }
    FOnHighLevel : TNotifyEvent;
    FOnGetLogString : TStGetLogStringEvent;
    { Private variables }
    glBuffer : PStLogBuffer;
    glBufferHead : DWORD;
    glBufferTail : DWORD;
    glHighLevelMark : DWORD;
    glHighLevelTriggered : Boolean;
    glLogCS : TRTLCriticalSection;
    glTempBuffer : PByteArray;
    glTempSize : DWORD;
    glTimeBase : DWORD;
  protected
    { Property access methods }
    procedure DoGetLogString(const D1, D2, D3, D4 : DWORD; var LogString : AnsiString); virtual;
    function GetBufferEmpty : Boolean;
    function GetBufferFree : DWORD;
    function GetBufferSize : DWORD;
    function GetEnabled : Boolean;
    function GetFileName : TFileName;
    function GetHighLevel : Byte;
    function GetLogOptions : StGenOptionSet;                           {!!.01}
    function GetWriteMode : TStWriteMode;
    procedure SetBufferSize(const Value : DWORD);
    procedure SetEnabled(const Value : Boolean); virtual;
    procedure SetFileName(const Value : TFileName); virtual;
    procedure SetHighLevel(const Value : Byte);
    procedure SetLogOptions(const Value : StGenOptionSet);             {!!.01}
    procedure SetWriteMode(const Value : TStWriteMode);
    { Internal methods }
    procedure glCalcHighLevel;
    procedure glCheckTempSize(SizeReq : DWORD);
    procedure glHighLevelCheck;
    procedure glLockLog;
    function glPopLogEntry(var LogRec : TStLogRec) : Boolean;
    function glTimeStamp(Mark : DWORD) : string;
    procedure glUnlockLog;
  public
    { Public methods }
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    procedure AddLogEntry(const D1, D2, D3, D4 : DWORD);
    procedure ClearBuffer;
    procedure DumpLog; virtual;
    procedure WriteLogString(const LogString : AnsiString);
    { Public properties }
    property BufferEmpty : Boolean read GetBufferEmpty;
    property BufferFree : DWORD read GetBufferFree;
  published
    { Published properties }
    property BufferSize : DWORD
      read GetBufferSize write SetBufferSize default StDefBufferSize;
    property Enabled : Boolean read GetEnabled write SetEnabled default True;
    property FileName : TFileName read GetFileName write SetFileName;
    property HighLevel : Byte read GetHighLevel write SetHighLevel default StDefHighLevel;
    property LogFileFooter : string read FLogFileFooter write FLogFileFooter;
    property LogFileHeader : string read FLogFileHeader write FLogFileHeader;
    property LogOptions : StGenOptionSet read GetLogOptions            {!!.01}
      write SetLogOptions default [];                                  {!!.01}
    property WriteMode : TStWriteMode read GetWriteMode write SetWriteMode;
    { Event properties }
    property OnHighLevel : TNotifyEvent read FOnHighLevel write FOnHighLevel;
    property OnGetLogString : TStGetLogStringEvent
      read FOnGetLogString write FOnGetLogString;
  end;

  function HexifyBlock(var Buffer; BufferSize : Integer) : AnsiString;

implementation

{ TStGeneralLog }

{ Gives text representation of a block of data }
function HexifyBlock(var Buffer; BufferSize : Integer) : AnsiString;
type
  TCastCharArray = array[0..Pred(High(Integer))] of AnsiChar;
const
  { Starting string to work with - this is directly written to by index }
  { below, so any positional changes here will also have to be made below. }
  StockString = '  %6.6x: 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 : 0000000000000000' + StCRLF;
  HexDigits : array[0..$F] of AnsiChar = '0123456789ABCDEF';
var
  I, J, K, Lines : Integer;
  TempStr : AnsiString;
  Hex1, Hex2 : array[0..23] of AnsiChar;
  Ascii1, Ascii2 : array[0..7] of AnsiChar;
begin
  K := 0;
  FillChar(Hex1, SizeOf(Hex1), #32);
  FillChar(Hex2, SizeOf(Hex2), #32);

  { Calculate number of lines required }
  Lines := BufferSize div 16;
  if (BufferSize mod 16) <> 0 then Inc(Lines);

  { Process and append lines }
  for I := 0 to Lines-1 do begin

    { Load string, add index marker }
    TempStr := AnsiString(Format(StockString, [I*16]));

    { Format data for first word }
    for J := 0 to 7 do begin
      if J+K >= BufferSize then begin
        Ascii1[J] := ' ';
        Hex1[J*3] := ' ';
        Hex1[J*3+1] := ' ';
      end else begin
        Ascii1[J] := TCastCharArray(Buffer)[J+K];
        Hex1[J*3] := HexDigits[Byte(Ascii1[J]) shr 4];
        Hex1[J*3+1] := HexDigits[Byte(Ascii1[J]) and $F];

        { Clamp Ascii to printable range }
        if (Ascii1[J] < #32) or (Ascii1[J] > #126) then Ascii1[J] := '.';
      end;
    end;
    Inc(K,8);

    { Format data for second word }
    for J := 0 to 7 do begin
      if J+K >= BufferSize then begin
        Ascii2[J] := ' ';
        Hex2[J*3] := ' ';
        Hex2[J*3+1] := ' ';
      end else begin
        Ascii2[J] := TCastCharArray(Buffer)[J+K];
        Hex2[J*3] := HexDigits[Byte(Ascii2[J]) shr 4];
        Hex2[J*3+1] := HexDigits[Byte(Ascii2[J]) and $F];
        { Clamp Ascii to printable range }
        if (Ascii2[J] < #32) or (Ascii2[J] > #126) then Ascii2[J] := '.';
      end;
    end;
    Inc(K,8);

    { Move data to existing temp string }
    Move(Hex1[0], TempStr[11], SizeOf(Hex1));
    Move(Hex2[0], TempStr[36], SizeOf(Hex2));

    Move(Ascii1[0], TempStr[62], SizeOf(Ascii1));
    Move(Ascii2[0], TempStr[70], SizeOf(Ascii2));

    { Append temp string to result }
    Result := Result + TempStr;
  end;
end;

constructor TStGeneralLog.Create(Owner : TComponent);
begin
  inherited Create(Owner);
  InitializeCriticalSection(glLogCS);
  BufferSize := StDefBufferSize;
  FEnabled := True;
  FFileName := 'debug.log';
  FLogFileFooter := StLogFileFooter;
  FLogFileHeader := StLogFileHeader;
  HighLevel := StDefHighLevel;
  glHighLevelTriggered := False;
  glTimeBase := GetTickCount;
end;

destructor TStGeneralLog.Destroy;
begin
  FreeMem(glBuffer);
  FreeMem(glTempBuffer);
  DeleteCriticalSection(glLogCS);
  inherited Destroy;
end;

procedure TStGeneralLog.glLockLog;
begin
  if IsMultiThread then
    EnterCriticalSection(glLogCS);
end;

procedure TStGeneralLog.glUnlockLog;
begin
  if IsMultiThread then
    LeaveCriticalSection(glLogCS);
end;

{ AddLogEntry notes:                                                  }
{                                                                     }
{ D1 = $FFFFFFFF is reserved for internal events                      }
{                                                                     }
{ D1, D2, D3, D4 are "info" fields to be used in the OnGetLogString   }
{ handler to identify the logged event and what type of data would be }
{ appropriate for the corresponding log entry.                        }
{                                                                     }
{ While you're free to come up with your own logging scheme, it was   }
{ envisioned that D1 would identify the logged event in the broadest  }
{ terms, and the event classification would be narrowed further and   }
{ further with D2 --> D4.                                             }
{                                                                     }
{ Special case: If the high bit of D2 is set, D3 becomes a pointer    }
{ to data, and D4 is the size of the data. Make *sure* the high bit   }
{ isn't set unless you are using this special situation.              }
{                                                                     }
{ If you just have a simple case for logging that probably won't get  }
{ used that often, consider adding entries with the WriteDebugString  }
{ method.                                                             }
procedure TStGeneralLog.AddLogEntry(const D1, D2, D3, D4 : DWORD);
var
  LogEntry : TStLogRec;
  EntryPtr : PStLogRec;
  SizeReq, TimeMrk, ChunkSize : DWORD;
  HasData : Boolean;
begin
  glLockLog;
  try
    { Bail if we're not logging }
    if not Enabled then Exit;

    TimeMrk := GetTickCount;

    { Determine size needed }
    SizeReq := SizeOf(TStLogRec);
    if (D2 and $80000000) = $80000000 then begin
      HasData := True;
      Inc(SizeReq, D4);
    end else begin
      HasData := False;
    end;

    { Bail if SizeReq is bigger than the whole buffer }
    if SizeReq > FBufferSize then Exit;

    { Make more room in buffer if necessary }
    while (SizeReq > BufferFree) and glPopLogEntry(LogEntry) do ;

    { Do we need to wrap this entry? }
    if (glBufferTail + SizeReq) <= FBufferSize then begin

      { Wrap not required, write directly to glBuffer }
      EntryPtr := @glBuffer[glBufferTail];
      EntryPtr.lrTime := TimeMrk;
      EntryPtr.lrData1 := D1;
      EntryPtr.lrData2 := D2;
      EntryPtr.lrData3 := D3;
      EntryPtr.lrData4 := D4;

      { Write add'l data if necessary }
      if HasData then begin
        Move(Pointer(D3)^, glBuffer[glBufferTail + SizeOf(TStLogRec)], D4);
      end;
      Inc(glBufferTail, SizeReq);

      { Fix tail if necessary }
      if glBufferTail = FBufferSize then
        glBufferTail := 0;

    end else begin

      { Wrap required, use temp buffer }
      glCheckTempSize(SizeReq);

      EntryPtr := @glTempBuffer[0];
      EntryPtr.lrTime := TimeMrk;
      EntryPtr.lrData1 := D1;
      EntryPtr.lrData2 := D2;
      EntryPtr.lrData3 := D3;
      EntryPtr.lrData4 := D4;

      { Write add'l data if necessary }
      if HasData then begin
        Move(Pointer(D3)^, glTempBuffer[SizeOf(TStLogRec)], D4);
      end;

      { Move first half }
      ChunkSize := FBufferSize - glBufferTail;
      Move(glTempBuffer[0], glBuffer[glBufferTail], ChunkSize);

      { Move second half }
      Move(glTempBuffer[ChunkSize], glBuffer[0], SizeReq - ChunkSize);

      { Set tail }
      glBufferTail := SizeReq - ChunkSize;
    end;
    glHighLevelCheck;
  finally
    glUnlockLog;
  end;
end;

{ Clears all data from buffer (does not write data to disk) }
procedure TStGeneralLog.ClearBuffer;
begin
  glLockLog;
  try
    glBufferHead := 0;
    glBufferTail := 0;
  finally
    glUnlockLog;
  end;
end;

{ Let user fill in the data for the LogString }
procedure TStGeneralLog.DoGetLogString(const D1, D2, D3, D4 : DWORD; var LogString : AnsiString);
begin
  if Assigned(FOnGetLogString) then
    FOnGetLogString(Self, D1, D2, D3, D4, LogString);
end;

{ Calculate the BufferFree level, in bytes, to trip the high level alarm }
procedure TStGeneralLog.glCalcHighLevel;
begin
  glLockLog;
  try
    glHighLevelMark := FBufferSize - Round(FBufferSize * FHighLevel / 100);
    glHighLevelCheck;
  finally
    glUnlockLog;
  end;
end;

{ Verifies the size of the temp buffer }
procedure TStGeneralLog.glCheckTempSize(SizeReq : DWORD);
begin
  if (SizeReq > glTempSize) then begin
    ReallocMem(glTempBuffer, SizeReq);
    glTempSize := SizeReq;
  end;
end;

{ Test for high level condition, fire event if necessary }
procedure TStGeneralLog.glHighLevelCheck;
begin
  glLockLog;
  try
    if FHighLevel = 0 then Exit;
    if BufferFree < glHighLevelMark then begin
      if Assigned(FOnHighLevel) and not glHighLevelTriggered then begin
        FOnHighLevel(Self);
        glHighLevelTriggered := True;
      end;
    end else begin
      glHighLevelTriggered := False;
    end;
  finally
    glUnlockLog;
  end;
end;

{ Pop log record from log, return False if no record to return }
function TStGeneralLog.glPopLogEntry(var LogRec : TStLogRec) : Boolean;
type
  BytesArray = array[0..SizeOf(TStLogRec)-1] of Byte;
var
  Bytes : BytesArray absolute LogRec;
  ChunkSize : DWORD;
begin
  glLockLog;
  try
    { Check for empty buffer }
    if (glBufferHead = glBufferTail) then begin
      Result := False;
      Exit;
    end else begin
      Result := True;
    end;

    { Check to see if log record wraps }
    if (glBufferHead + SizeOf(TStLogRec)) <= FBufferSize then begin

      { No wrap, copy directly over }
      Move(glBuffer[glBufferHead], LogRec, SizeOf(LogRec));
      Inc(glBufferHead, SizeOf(LogRec));

      { Fix head if needed }
      if (glBufferHead = FBufferSize) then glBufferHead := 0;
    end else begin

      { Need to deal with wrap -- copy first half }
      ChunkSize := (FBufferSize - glBufferHead);
      Move(glBuffer[glBufferHead], Bytes[0], ChunkSize);

      { Copy second half }
      Move(glBuffer[0], Bytes[ChunkSize], (SizeOf(LogRec) - ChunkSize));
      glBufferHead := SizeOf(LogRec) - ChunkSize;
    end;

    { Do we have data? If so, deal with it }
    if (LogRec.lrData2 and $80000000) = $80000000 then begin

      { Check to see if log data wraps }
      if (glBufferHead + LogRec.lrData4) <= FBufferSize then begin

        { No wrap -- point D2 to buffer }
        LogRec.lrData3 := DWORD(@glBuffer[glBufferHead]);
        Inc(glBufferHead, LogRec.lrData4);
      end else begin

        { Wrap -- copy first half to temp buffer }
        glCheckTempSize(LogRec.lrData4);
        ChunkSize := (FBufferSize - glBufferHead);
        Move(glBuffer[glBufferHead], glTempBuffer[0], ChunkSize);

        { Copy second half }
        Move(glBuffer[0], glTempBuffer[ChunkSize], (LogRec.lrData4 - ChunkSize));
        LogRec.lrData3 := DWORD(@glTempBuffer[0]);
        glBufferHead := LogRec.lrData4 - ChunkSize;
      end;
    end

  finally
    glUnlockLog;
  end;
end;

{ Return time stamp string }
function TStGeneralLog.glTimeStamp(Mark : DWORD) : string;
begin
  Result := Format('%07.7d : ', [Mark - glTimeBase]);
  Insert('.', Result, 5);
end;

{ Dumps log file to disk }
procedure TStGeneralLog.DumpLog;
var
  LR : TStLogRec;
  FS : TFileStream;
  S, T : AnsiString;
begin
  glLockLog;

  try
    { Open file stream }
    if FileExists(FileName) and (WriteMode = wmAppend) then begin
      FS := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
      FS.Seek(0, soFromEnd);
    end else begin
      FS := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    end;

    try
      { Do file header if appropriate }
      if (FS.Size = 0) then begin
        S := AnsiString(FLogFileHeader);
        FS.Write(S[1], Length(S));

        { Write trailing CRLF }                                        {!!.02}
        FS.Write(StCRLF[1], Length(StCRLF));                           {!!.02}
      end;

      { Cycle through all data }
      while glPopLogEntry(LR) do begin
        if LR.lrData1 <> $FFFFFFFF then begin

          { It belongs to somone else, let them process it }
          DoGetLogString(LR.lrData1, LR.lrData2, LR.lrData3, LR.lrData4, S);
        end else begin

          { Something we're supposed to know about, deal with it }
          case LR.lrData2 of

            { Logging enabled }
            leEnabled : S := '**** Logging Enabled' + StCRLF;

            { Logging disabled }
            leDisabled : S := '**** Logging Disabled' + StCRLF;

            { WriteLogString entry }
            leString :
              begin
                if LR.lrData4 > 0 then begin                           {!!.02}
                  SetLength(S, LR.lrData4);
                  Move(PByteArray(LR.lrData3)[0], S[1], LR.lrData4);
                end else begin                                         {!!.02}
                  S := ''; { empty string }                            {!!.02}
                end;                                                   {!!.02}
              end;

            else
              S := AnsiString(Format('!! Unknown log entry : [%8.8x][%8.8x][%8.8x][%8.8x]' + StCRLF,
                [LR.lrData1, LR.lrData2, LR.lrData3, LR.lrData4]));

          end;
        end;

        { Write time stamp }
        T := AnsiString(glTimeStamp(LR.lrTime));
        FS.Write(T[1], Length(T));

        { Write log string }
        if Length(S) > 0 then                                          {!!.02}
          FS.Write(S[1], Length(S));

        { Write trailing CRLF }
        FS.Write(StCRLF[1], Length(StCRLF));
      end;

      { Do file header if appropriate }
      if (FLogFileFooter <> '') then begin
        S := AnsiString(FLogFileFooter);
        FS.Write(S[1], Length(S));

        { Write trailing CRLF }                                        {!!.02}
        FS.Write(StCRLF[1], Length(StCRLF));                           {!!.02}
      end;

      glHighLevelTriggered := False;

    finally
      FS.Free;
    end;

  finally
    glUnlockLog;
  end;
end;

{ Determines whether something is in the buffer }
function TStGeneralLog.GetBufferEmpty : Boolean;
begin
  glLockLog;
  try
    Result := (glBufferHead = glBufferTail);
  finally
    glUnlockLog;
  end;
end;

{ Calculates free space in the buffer }
function TStGeneralLog.GetBufferFree : DWORD;
begin
  glLockLog;
  try
    if (glBufferHead <= glBufferTail) then
      { One less than actual, since we always leave one byte free }
      Result := Pred(FBufferSize - (glBufferTail - glBufferHead))
    else
      Result := Pred(glBufferHead - glBufferTail);
  finally
    glUnlockLog;
  end;
end;

{ Retrieves buffer size }
function TStGeneralLog.GetBufferSize : DWORD;
begin
  glLockLog;
  try
    Result := FBufferSize;
  finally
    glUnlockLog;
  end;
end;

{ Get Enabled property }
function TStGeneralLog.GetEnabled : Boolean;
begin
  glLockLog;
  try
    Result := FEnabled;
  finally
    glUnlockLog;
  end;
end;

{ Get FileName property }
function TStGeneralLog.GetFileName : TFileName;
begin
  glLockLog;
  try
    Result := FFileName;
  finally
    glUnlockLog;
  end;
end;

{ Retrieves high level setpoint }
function TStGeneralLog.GetHighLevel : Byte;
begin
  glLockLog;
  try
    Result := FHighLevel;
  finally
    glUnlockLog;
  end;
end;

{!!.01 - added}
{ Retrieves log options }
function TStGeneralLog.GetLogOptions : StGenOptionSet;
begin
  glLockLog;
  try
    Result := FLogOptions;
  finally
    glUnlockLog;
  end;
end;

{ Retrieves write mode }
function TStGeneralLog.GetWriteMode : TStWriteMode;
begin
  glLockLog;
  try
    Result := FWriteMode;
  finally
    glUnlockLog;
  end;
end;

{ Sets the size of the logging buffer }
procedure TStGeneralLog.SetBufferSize(const Value : DWORD);
begin
  glLockLog;
  try
    if Value <> FBufferSize then begin
      FBufferSize := Value;
      ReallocMem(glBuffer, Value);
      ClearBuffer;
      glCalcHighLevel;
    end;
  finally
    glUnlockLog;
  end;
end;

{ Enables (or disables) logging }
procedure TStGeneralLog.SetEnabled(const Value : Boolean);
begin
  glLockLog;
  try
    if (Value = True) then begin

      { Allocate buffer if not already done }
      if (glBuffer = nil) then begin
        GetMem(glBuffer, FBufferSize);
      end;

      { Init temp buffer if not already done }
      if (glTempBuffer = nil) then begin
        glTempSize := 1024;
        GetMem(glTempBuffer, glTempSize);
      end;
    end else if not (goSuppressDisableMsg in LogOptions) then begin    {!!.01}
      AddLogEntry($FFFFFFFF, leDisabled, 0, 0);
    end;

    FEnabled := Value;

  finally
    glUnlockLog;
  end;

  if (Value = True) and not (goSuppressEnableMsg in LogOptions) then   {!!.01}
    AddLogEntry($FFFFFFFF, leEnabled, 0, 0);
end;

{ Set FileName property }
procedure TStGeneralLog.SetFileName(const Value : TFileName);
begin
  glLockLog;
  try
    FFileName := Value;
  finally
    glUnlockLog;
  end;
end;

{ Set HighLevel property }
procedure TStGeneralLog.SetHighLevel(const Value : Byte);
begin
  glLockLog;
  try
    if (FHighLevel <> Value) and (Value <= 100) then begin
      FHighLevel := Value;
      glCalcHighLevel;
    end;
  finally
    glUnlockLog;
  end;
end;

{!!.01 - added}
{ Set LogOptions property }
procedure TStGeneralLog.SetLogOptions(const Value : StGenOptionSet);
begin
  glLockLog;
  try
    FLogOptions := Value;
  finally
    glUnlockLog;
  end;
end;

{ Set WriteMode property }
procedure TStGeneralLog.SetWriteMode(const Value : TStWriteMode);
begin
  glLockLog;
  try
    FWriteMode := Value;
  finally
    glUnlockLog;
  end;
end;

{ Write log string to log buffer }
procedure TStGeneralLog.WriteLogString(const LogString : AnsiString);
begin
  AddLogEntry($FFFFFFFF, leString, DWORD(LogString), Length(LogString));
end;

end.
