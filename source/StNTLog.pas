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
{* SysTools: StNTLog.pas 4.04                            *}
{*********************************************************}
{* SysTools: NT Event Logging                            *}
{*********************************************************}

{$I StDefine.inc}

unit StNTLog;

interface

uses
  Windows, SysUtils, Classes, Registry, StBase;

type

  TStNTEventType = (etSuccess, etError, etWarning, etInfo,
    etAuditSuccess, etAuditFailure);

  PStNTEventLogRec = ^TStNTEventLogRec;
  TStNTEventLogRec = record
    case Integer of
      0 : (Length              : DWORD;  { Length of full record }
           Reserved            : DWORD;  { Used by the service }
           RecordNumber        : DWORD;  { Absolute record number }
           TimeGenerated       : DWORD;  { Seconds since 1-1-1970 }
           TimeWritten         : DWORD;  { Seconds since 1-1-1970 }
           EventID             : DWORD;
           EventType           : WORD;
           NumStrings          : WORD;
           EventCategory       : WORD;
           ReservedFlags       : WORD;   { For use with paired events (auditing) }
           ClosingRecordNumber : DWORD;  { For use with paired events (auditing) }
           StringOffset        : DWORD;  { Offset from beginning of record }
           UserSidLength       : DWORD;
           UserSidOffset       : DWORD;
           DataLength          : DWORD;
           DataOffset          : DWORD); { Offset from beginning of record }

      1 : (VarData : array [0..65535] of Byte);

    //
    // Variable data may contain:
    //
    // WCHAR SourceName[]
    // WCHAR Computername[]
    // SID   UserSid
    // WCHAR Strings[]
    // BYTE  Data[]
    // CHAR  Pad[]
    // DWORD Length;
    //
    // Data is contained -after- the static data, the VarData field is set
    // to the beginning of the record merely to make the offsets match up.
  end;

  TStReadRecordEvent = procedure(Sender : TObject; const EventRec : TStNTEventLogRec;
    var Abort : Boolean) of object;

  TStNTEventLog = class(TStComponent)
  private
    { Internal use variables }
    elLogHandle : THandle;
    elLogList : TStringList;
    { Property variables }
    FComputerName : string;
    FEnabled : Boolean;
    FEventSource : string;
    FLogName : string;
    FOnReadRecord : TStReadRecordEvent;
  protected
    { Internal Methods }
    procedure elAddEntry(const EventType : TStNTEventType; EventCategory, EventID : DWORD;
      const Strings : TStrings; DataPtr : pointer; DataSize : DWORD);
    procedure elCloseLog;
    procedure elOpenLog;
    { Property Methods }
    function GetLogCount : DWORD;
    function GetLogs(Index : Integer) : string;
    function GetRecordCount : DWORD;
    procedure SetComputerName(const Value : string);
    procedure SetLogName(const Value : string);
  public
    { Public Methods }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddEntry(const EventType : TStNTEventType; EventCategory, EventID : DWORD);
    procedure AddEntryEx(const EventType : TStNTEventType; EventCategory, EventID : DWORD;
      const Strings : TStrings; DataPtr : pointer; DataSize : DWORD);
    procedure ClearLog(const BackupName : TFileName);
    procedure CreateBackup(const BackupName : TFileName);
    procedure ReadLog(const Reverse : Boolean);
    procedure RefreshLogList;
    { Public Properties }
    property LogCount : DWORD read GetLogCount;
    property Logs[Index : Integer] : string read GetLogs;
    property RecordCount : DWORD read GetRecordCount;
  published
    { Published Properties }
    property ComputerName : string read FComputerName write SetComputerName;
    property Enabled : Boolean read FEnabled write FEnabled default True;
    property EventSource : string read FEventSource write FEventSource;
    property LogName : string read FLogName write SetLogName;
    property OnReadRecord : TStReadRecordEvent read FOnReadRecord write FOnReadRecord;
  end;

implementation

const
  { Defines for the READ flags for Eventlogging }

  EVENTLOG_SEQUENTIAL_READ = $0001;
  EVENTLOG_SEEK_READ       = $0002;
  EVENTLOG_FORWARDS_READ   = $0004;
  EVENTLOG_BACKWARDS_READ  = $0008;

  { The types of events that can be logged. }

  EVENTLOG_SUCCESS          = $0000;
  EVENTLOG_ERROR_TYPE       = $0001;
  EVENTLOG_WARNING_TYPE     = $0002;
  EVENTLOG_INFORMATION_TYPE = $0004;
  EVENTLOG_AUDIT_SUCCESS    = $0008;
  EVENTLOG_AUDIT_FAILURE    = $0010;

  { Defines for the WRITE flags used by Auditing for paired events }
  { These are not implemented in Product 1 }

  EVENTLOG_START_PAIRED_EVENT    = $0001;
  EVENTLOG_END_PAIRED_EVENT      = $0002;
  EVENTLOG_END_ALL_PAIRED_EVENTS = $0004;
  EVENTLOG_PAIRED_EVENT_ACTIVE   = $0008;
  EVENTLOG_PAIRED_EVENT_INACTIVE = $0010;

  StEventLogKey = '\SYSTEM\CurrentControlSet\Services\EventLog';

  
{ Create instance of event log component }
constructor TStNTEventLog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  { initialization }
  elLogHandle := 0;
  elLogList := TStringList.Create;
  FEnabled := True;
  FLogName := 'Application';

  { initialize log list }
  RefreshLogList;
end;

{ Destroy instance of event log component }
destructor TStNTEventLog.Destroy;
begin
  if elLogHandle <> 0 then elCloseLog;
  elLogList.Free;
  inherited;
end;

{ Add entry to the event log }
procedure TStNTEventLog.AddEntry(const EventType : TStNTEventType;
  EventCategory, EventID : DWORD);
begin
  elAddEntry(EventType, EventCategory, EventID, nil, nil, 0);
end;

{ Add entry to the event log - more options }
procedure TStNTEventLog.AddEntryEx(const EventType : TStNTEventType;
  EventCategory, EventID : DWORD; const Strings : TStrings;
  DataPtr : pointer; DataSize : DWORD);
begin
  elAddEntry(EventType, EventCategory, EventID, Strings, DataPtr, DataSize);
end;

{ Clear the event log }
procedure TStNTEventLog.ClearLog(const BackupName : TFileName);
begin
  elOpenLog;
  try
    ClearEventLog(elLogHandle, PChar(BackupName));
  finally
    elCloseLog;
  end;
end;

{ Back up the event log }
procedure TStNTEventLog.CreateBackup(const BackupName : TFileName);
begin
  elOpenLog;
  try
    BackupEventLog(elLogHandle, PChar(BackupName));
  finally
    elCloseLog;
  end;
end;

{ Adds an entry to the event log }
procedure TStNTEventLog.elAddEntry(const EventType : TStNTEventType;
  EventCategory, EventID : DWORD; const Strings : TStrings; DataPtr : pointer; DataSize : DWORD);
const
  StrArraySize = 1024;
var
  TempType, StrCount : DWORD;
  StrArray : array[0..StrArraySize-1] of PChar;
  StrArrayPtr : pointer;
  I : Integer;
begin
  StrArrayPtr := nil;

  case EventType of
    etSuccess : TempType := EVENTLOG_SUCCESS;
    etError : TempType := EVENTLOG_ERROR_TYPE;
    etWarning : TempType := EVENTLOG_WARNING_TYPE;
    etInfo : TempType := EVENTLOG_INFORMATION_TYPE;
    etAuditSuccess : TempType := EVENTLOG_AUDIT_SUCCESS;
    etAuditFailure : TempType := EVENTLOG_AUDIT_FAILURE;
  else
    TempType := 0;
  end;

  elOpenLog;
  try
    { Fill string array }
    if Assigned(Strings) then begin
      FillChar(StrArray, SizeOf(StrArray), #0);
      StrCount := Strings.Count;
      Assert(StrCount <= StrArraySize);
      for I := 0 to StrCount-1 do begin
        StrArray[I] := StrAlloc(Length(Strings[I]));
        StrPCopy(StrArray[I], Strings[I]);
      end;
      StrArrayPtr := @StrArray;
    end else begin
      StrCount := 0;
    end;
    ReportEvent(elLogHandle, TempType, EventCategory,
      EventID, nil, StrCount, DataSize, StrArrayPtr, DataPtr);
  finally
    { Release string array memory }
    for I := 0 to StrArraySize-1 do begin
      if StrArray[I] = nil then Break;
      StrDispose(StrArray[I]);
    end;
    elCloseLog;
  end;
end;

{ Close event log }
procedure TStNTEventLog.elCloseLog;
begin
  if elLogHandle <> 0 then begin
    CloseEventLog(elLogHandle);
    elLogHandle := 0;
  end;
end;

{ Open event log }
procedure TStNTEventLog.elOpenLog;
begin
  if elLogHandle = 0 then
    elLogHandle := OpenEventLog(PChar(FComputerName), PChar(FLogName));
end;

{ Get number on logs available on system }
function TStNTEventLog.GetLogCount : DWORD;
begin
  Result := elLogList.Count;
end;

{ Get name of logs }
function TStNTEventLog.GetLogs(Index : Integer) : string;
begin
  Result := elLogList[Index];
end;

{ Get number of log entries in event log }
function TStNTEventLog.GetRecordCount : DWORD;
begin
  elOpenLog;
  try
    GetNumberOfEventLogRecords(elLogHandle, Result);
  finally
    elCloseLog;
  end;
end;

{ Reads log until complete or aborted }
procedure TStNTEventLog.ReadLog(const Reverse : Boolean);
var
  ReadDir, BytesRead, BytesNeeded, LastErr : DWORD;
  RetVal, Aborted : Boolean;
  TempBuffer : array[0..2047] of Byte;
  TempPointer : Pointer;
  TempRecPtr : PStNTEventLogRec;  { used as an alias, don't actually allocate }
  FakeBuf : Byte;
begin
  Aborted := False;
  TempPointer := nil;

  { Set direction }
  if Reverse then
    ReadDir := EVENTLOG_SEQUENTIAL_READ or EVENTLOG_BACKWARDS_READ
  else
    ReadDir := EVENTLOG_SEQUENTIAL_READ or EVENTLOG_FORWARDS_READ;

  elOpenLog;
  try
    repeat
      { Fake read to determine required buffer size }
      RetVal := ReadEventLog(elLogHandle, ReadDir, 0, @FakeBuf,
        SizeOf(FakeBuf), BytesRead, BytesNeeded);

      if not RetVal then begin
        LastErr := GetLastError;
        if (LastErr = ERROR_INSUFFICIENT_BUFFER) then begin

          { We can use local buffer, which is faster }
          if (BytesNeeded <= SizeOf(TempBuffer)) then begin
            if not (ReadEventLog(elLogHandle, ReadDir, 0, @TempBuffer,
              BytesNeeded, BytesRead, BytesNeeded)) then
              {$WARNINGS OFF}  { Yeah, we know RaiseLastWin32Error is deprecated }
              RaiseLastWin32Error;
              {$WARNINGS ON}

            TempRecPtr := @TempBuffer

          { Local buffer too small, need to allocate a buffer on the heap }
          end else begin
            if TempPointer = nil then
              GetMem(TempPointer, BytesNeeded)
            else
              ReallocMem(TempPointer, BytesNeeded);

            if not (ReadEventLog(elLogHandle, ReadDir, 0, TempPointer,
              BytesNeeded, BytesRead, BytesNeeded)) then
              {$WARNINGS OFF}  { Yeah, we know RaiseLastWin32Error is deprecated }
              RaiseLastWin32Error;
              {$WARNINGS ON}

            TempRecPtr := TempPointer;

          end;

          { At this point, we should have the data -- fire the event }
          if Assigned(FOnReadRecord) then
            FOnReadRecord(Self, TempRecPtr^, Aborted);

        end else begin
          Aborted := True;

          { Handle unexpected error }
          {$WARNINGS OFF}  { Yeah, we know RaiseLastWin32Error is deprecated }
          if (LastErr <> ERROR_HANDLE_EOF) then
            RaiseLastWin32Error;
          {$WARNINGS ON}
        end;
      end;
    until Aborted;

  finally
    elCloseLog;

    if TempPointer = nil then
      FreeMem(TempPointer);
  end;
end;

{ Refreshes log list }
procedure TStNTEventLog.RefreshLogList;
var
  Reg : TRegistry;
begin
  elLogList.Clear;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(StEventLogKey, False) then begin
      Reg.GetKeyNames(elLogList);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ Set log name }
procedure TStNTEventLog.SetLogName(const Value : string);
begin
  FLogName := Value
end;

{ Set computer name }
procedure TStNTEventLog.SetComputerName(const Value : string);
begin
  FComputerName := Value;
  RefreshLogList;
end;

end.
