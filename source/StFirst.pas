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
{* SysTools: StFirst.pas 4.04                            *}
{*********************************************************}
{* SysTools: limit instance routines                     *}
{*********************************************************}

{$I StDefine.inc}

unit StFirst;

interface

uses
  Windows, Messages, Forms, SysUtils, Dialogs,

  StBase;

function IsFirstInstance: Boolean;
 {- check if previous instance is running}
procedure ActivateFirst(AString : PChar);
 {- Activate previous instance, passing a string}
procedure ActivateFirstCommandLine;
 {- Activate previous instance, passing the command line}


implementation

const
  MAX_CMDLEN = 1024;

var
  CmdLine : array[0..MAX_CMDLEN] of char;

var
  FirstInstance : Boolean;
  InstanceMutex : THandle;

{limit instances routines}
function IsFirstInstance : Boolean;
begin
  Result := FirstInstance;
end;

procedure ActivateFirstCommandLine;
var
  S : String;
  I : Longint;
begin
  S := '';
  for I := 0 to ParamCount-1 do
    S := S + ParamStr(I) + ' ';
  S := S + ParamStr(ParamCount);
  StrPCopy(CmdLine, Copy(S, 1, MAX_CMDLEN));
  ActivateFirst(CmdLine);
end;


procedure ActivateFirst(AString : PChar);
var
  ClassBuf,
  WindowBuf : array [0..255] of Char;
  Wnd,
  TopWnd    : hWnd;
  ThreadID  : DWord;
  CDS       : TCopyDataStruct;
begin
  if (strlen(AString) > 0) then begin
    CDS.dwData := WMCOPYID;
    CDS.cbData := (StrLen(AString) + 1) * SizeOf(Char);
    CDS.lpData := AString;
  end else begin
    CDS.dwData := WMCOPYID;
    CDS.cbData := 0;
    CDS.lpData := nil;
  end;

  if IsFirstInstance then begin
    if IsIconic(Application.Handle) then
      Application.Restore
    else
      Application.BringToFront;
  end else begin
    GetClassName(Application.Handle, ClassBuf, Length(ClassBuf));
    GetWindowText(Application.Handle, WindowBuf, Length(WindowBuf));
    Wnd := FindWindow(ClassBuf, WindowBuf);
    if (Wnd <> 0) then begin
      GetWindowThreadProcessId(Wnd, @ThreadID);
      if (ThreadID = GetCurrentProcessId) then begin
        Wnd := FindWindowEx(0, Wnd, ClassBuf, WindowBuf);
        if (Wnd <> 0) then begin
          if IsIconic(Wnd) then
            ShowWindow(Wnd, SW_RESTORE);
          SetForegroundWindow(Wnd);
          TopWnd := GetLastActivePopup(Wnd);
          if (TopWnd <> 0) and (TopWnd <> Wnd) and
              IsWindowVisible(TopWnd) and IsWindowEnabled(TopWnd) then begin
            BringWindowToTop(TopWnd);
            SendMessage(TopWnd, WM_COPYDATA, 0, lparam(@CDS));
          end else begin
            BringWindowToTop(Wnd);
            SendMessage(Wnd, WM_COPYDATA, 0, lparam(@CDS));
          end;
        end;
      end;
    end;
  end;
end;

function GetMutexName : string;
var
  WindowBuf : array [0..512] of Char;
begin
  GetWindowText(Application.Handle, WindowBuf, Length(WindowBuf));
  Result := 'PREVINST:' + WindowBuf;
end;

initialization
  InstanceMutex := CreateMutex(nil, True, PChar(GetMutexName));
  if (InstanceMutex <> 0) and (GetLastError = 0) then
    FirstInstance := True
  else
    FirstInstance := False;

finalization
  if (InstanceMutex <> 0) then
    CloseHandle(InstanceMutex);
end.
