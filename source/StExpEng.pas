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
{* SysTools: StExpLog.pas 4.03                           *}
{*********************************************************}
{* SysTools: Exception Logging                           *}
{*********************************************************}

{$I StDefine.inc}

unit StExpEng;

interface

uses
  Windows, SysUtils, Classes, StBase, StExpLog;

const
  OnHookInstaller : procedure = nil;

procedure DumpException;

implementation

uses
  Forms;

const
  MaxStackSize  = 48;

type
  TStExceptionHandler = class
  private
    OldOnException : TExceptionEvent;
  protected
    procedure OnException(Sender : TObject; E : Exception);
  end;

  TStExceptionTrace = record
    Count : Integer;
    Trace : array[0..pred(MaxStackSize)] of DWORD;
  end;

const
  EH : TStExceptionHandler = nil;
  WroteInfo : Boolean = False;
  HandlerInstalled : Boolean = False;
  cDelphiException = DWORD($0EEDFADE);
  cCppException = DWORD($0EEFFACE); { used by BCB }

var
  RA2 : procedure (dwExceptionCode, dwExceptionFlags, nNumberOfArguments : DWORD;
    const lpArguments : DWORD); stdcall;
  BaseOfCode, TopOfCode : DWORD;

{ Writes exception to log file }
procedure WriteException(E : Exception);
var
  p1 : Integer;
  RipFileName, S : string;
  FS : TFileStream;
  Buffer : array[0..255] of AnsiChar;
begin
  if Assigned(ExpLog) then
    RipFileName := ExpLog.FileName;

  if RipFileName = '' then begin
    GetModuleFileName(HInstance, Buffer, SizeOf(Buffer));
    RipFileName := ChangeFileExt(PChar(@Buffer),'.RIP');
  end;

  { Open file stream }
  if FileExists(RipFileName) then begin
    FS := TFileStream.Create(RipFileName, fmOpenReadWrite or fmShareDenyWrite);
    FS.Seek(0, soFromEnd);
    S := #13#10#13#10;
    FS.Write(S[1], Length(S));
  end else begin
    FS := TFileStream.Create(RipFileName, fmCreate or fmShareDenyWrite);
  end;

  try
    { Write info if necessary }
    if not WroteInfo and Assigned(ExpLog) then begin
      if (ExpLog.RipInfo <> '') then begin
        S := ExpLog.RipInfo + #13#10#13#10;
        FS.Write(S[1], Length(S));
      end;
      WroteInfo := True;
    end;

    { Write dump info from E.Message }
    p1 := Pos(#0, E.Message);
    S := Copy(E.Message, p1+1, MaxInt) + #13#10;
    FS.Write(S[1], Length(S));

    { Restore E.Message }
    S := E.Message;
    SetLength(S, P1-1);
    E.Message := S;

  finally
    FS.Free;
  end;
end;

{ Dumps Exception }
procedure DumpException;
var
  PutInLog : Boolean;
begin
  PutInLog := True;
  if Assigned(ExpLog) then
    ExpLog.DoExceptionFilter(Exception(ExceptObject),PutInLog);
  if PutInLog then
    WriteException(Exception(ExceptObject));
end;

{ TStExceptionHandler }

procedure TStExceptionHandler.OnException(Sender : TObject; E : Exception);
begin
  DumpException;
  if Assigned(OldOnException) then
    OldOnException(Sender, E)
  else
    Application.ShowException(Exception(ExceptObject));
end;

var
  SaveGetExceptionObject : function(P : PExceptionRecord) : Exception;

procedure HookInstaller;
begin
  EH := TStExceptionHandler.Create;
  EH.OldOnException := Application.OnException;
  Application.OnException := EH.OnException;
end;

procedure StackDump(E : Exception; Root : DWORD);
var
  P : PDWORD;
  C, D, StackTop, N, Prev : DWORD;
  Trace : TStExceptionTrace;
  I : Integer;
  Store : Boolean;
  MsgPtr : PChar;
  MsgLen : Integer;
begin
  if not HandlerInstalled then begin
    if Assigned(OnHookInstaller) then
      OnHookInstaller;
    HandlerInstalled := True;
  end;

  if Root = 0 then
    Trace.Count := 0
  else begin
    Trace.Count := 1;
    Trace.Trace[0] := Root;
  end;

  asm
    mov P,ebp
    mov eax,fs:[4]
    mov [StackTop],eax
  end;

  Prev := 0;
  C := 0;

  while DWORD(P) < DWORD(StackTop) do begin
    D := P^;
    N := 0;
    if (D >= BaseOfCode) and (D < TopOfCode) then
      if (PByte(D-5)^ = $E8)
      or ((PByte(D-6)^ = $FF) and (((PByte(D-5)^ and $38) = $10)))
      or ((PByte(D-4)^ = $FF) and (((PByte(D-3)^ and $38) = $10)))
      or ((PByte(D-3)^ = $FF) and (((PByte(D-2)^ and $38) = $10)))
      or ((PByte(D-2)^ = $FF) and (((PByte(D-1)^ and $38) = $10))) then
        N := D-BaseOfCode;
    if (N <> 0) and (N <> Prev) then begin
     if (Root = 0) then
        Store := C > 0
      else
        Store := C > 1;
      if Store then
        begin
          Trace.Trace[Trace.Count] := N;
          Inc(Trace.Count);
        end;
      Inc(C);
      if C > MaxStackSize then Break;
      Prev := N;
    end;
    Inc(P);
  end;

  if C > 0 then begin
    MsgPtr := PChar(E.Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then
      E.Message := E.Message + '.';
    E.Message := E.Message  + #0 + Format('Fault : %s'#13#10'Date/time : %s %s'#13#10'Stack dump'#13#10+
      '----------'#13#10,[E.Message,DateToStr(Now),TimeToStr(Now)]);
    for i := 0 to pred(Trace.Count) do
      E.Message := E.Message + Format('%8.8x'#13#10,[Trace.Trace[i]]);
  end;
end;

procedure LRE(dwExceptionCode, dwExceptionFlags, nNumberOfArguments : DWORD;
  const lpArguments : DWORD); stdcall;
var
  E : Exception;
begin
  if (dwExceptionCode = cDelphiException) or (dwExceptionCode = cCppException) then begin
    asm
      push ebx
      mov  ebx,lpArguments
      mov  eax,ss:[ebx+4]
      mov  E,eax
      pop  ebx
    end;
    if assigned(E) then
      StackDump(E, 0);
  end;
  if Assigned(RA2) then
    RA2(dwExceptionCode, dwExceptionFlags, nNumberOfArguments, lpArguments);
end;

function HookGetExceptionObject(P : PExceptionRecord) : Exception;
begin
  Result := SaveGetExceptionObject(P);
  StackDump(Result, DWORD(P^.ExceptionAddress)-BaseOfCode);
end;

procedure InitializeEng;
const
  ImageNumberofDirectoryEntries = 16;
  ImageDirectoryEntryImport     = 1;

type

  PImageImportByName = ^TImageImportByName;
  TImageImportByName = packed record
    Hint : WORD;
    Name : array[0..255] of char;
  end;

  PImageThunkData = ^TImageThunkData;
  TImageThunkData = packed record
    case Integer of
    1 : (Funct : ^DWORD);
    2 : (Ordinal : DWORD);
    3 : (AddressOfData : PImageImportByName);
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    Characteristics : DWORD;
    TimeDateStamp : DWORD;
    ForwarderChain : DWORD;
    Name : DWORD;
    FirstThunk : PImageThunkData;
  end;

  PImageDosHeader = ^TImageDosHeader;
  TImageDosHeader = packed record
    e_magic : WORD;
    e_cblp : WORD;
    e_cp : WORD;
    e_crlc : WORD;
    e_cparhdr : WORD;
    e_minalloc : WORD;
    e_maxalloc : WORD;
    e_ss : WORD;
    e_sp : WORD;
    e_csum : WORD;
    e_ip : WORD;
    e_cs : WORD;
    e_lfarlc : WORD;
    e_ovno : WORD;
    e_res : array [0..3] of WORD;
    e_oemid : WORD;
    e_oeminfo : WORD;
    e_res2 : array [0..9] of WORD;
    e_lfanew : DWORD;
  end;

var
  OriginalProc : Pointer;
  NTHeader : PImageNTHeaders;
  ImportDesc : PImageImportDescriptor;
  Thunk : PImageThunkData;

begin
  RA2 := nil;
  OriginalProc := GetProcAddress(GetModuleHandle('kernel32.dll'), 'RaiseException');

  if OriginalProc <> nil then begin
    NTHeader := PImageNTHeaders(DWORD(hInstance) + PImageDosHeader(hInstance).e_lfanew);
    ImportDesc := PImageImportDescriptor(DWORD(hInstance) +
      NTHeader.OptionalHeader.DataDirectory[ImageDirectoryEntryImport].VirtualAddress);

    BaseOfCode := DWORD(hInstance) + NTHeader.OptionalHeader.BaseOfCode;
    TopOfCode := BaseOfCode + NTHeader.OptionalHeader.SizeOfCode;

    while ImportDesc.Name <> 0 do begin
      if StriComp(PChar(DWORD(hInstance) + ImportDesc.Name), 'kernel32.dll') = 0 then begin
        Thunk := PImageThunkData(DWORD(hInstance) + DWORD(ImportDesc.FirstThunk));
        while Thunk.Funct <> nil do begin
          if Thunk.Funct = OriginalProc then
            Thunk.Funct := @LRE;
          Inc(Thunk);
        end;
      end;
      Inc(ImportDesc);
    end;
    RA2 := OriginalProc;
  end;
  SaveGetExceptionObject := ExceptObjProc;
  ExceptObjProc := @HookGetExceptionObject;
end;

initialization
  OnHookInstaller := HookInstaller;
  {$WARNINGS OFF}  { Yeah, we know DebugHook is platform specific }
  if DebugHook = 0 then InitializeEng;
  {$WARNINGS ON}

finalization
  EH.Free;

end.
