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
{* SysTools: StSpawn.pas 4.04                            *}
{*********************************************************}
{* SysTools: Component to spawn another application      *}
{*********************************************************}

{$I StDefine.inc}
{$TYPEDADDRESS ON}

unit StSpawn;

interface

uses
  SysUtils, Windows, ExtCtrls, Messages, Classes, ShellAPI,

  StBase, StConst;

type

  TStWaitThread = class(TThread)
    protected
      FTimeOut  : Integer;
      procedure Execute; override;
    public
      CancelWaitEvent : THandle;
      WaitResult  : DWORD;
      WaitFors    : PWOHandleArray;                                      {!!.01}

      constructor Create(aInst, CancelIt : THandle; ATimeOut : Integer);
      destructor Destroy; override;                                      {!!.01}
  end;

  TStSpawnCommand = (scOpen, scPrint, scOther);
  TStShowState = (ssMinimized, ssMaximized, ssNormal, ssMinNotActive);

  TStSpawnErrorEvent = procedure (Sender : TObject; Error : Word) of object;
  TStSpawnCompletedEvent = procedure (Sender : TObject) of object;
  TStSpawnTimeOutEvent = procedure (Sender : TObject) of object;

  TStSpawnApplication = class(TStComponent)
  protected {private}
    { Private declarations }

    FAllowChange    : Boolean;
    FCancelEvent    : THandle;
    FDefaultDir     : String;
    FFileName       : String;
    FInstance       : THandle;
    FNotifyWhenDone : Boolean;
    FOnCompleted    : TStSpawnCompletedEvent;
    FOnSpawnError   : TStSpawnErrorEvent;
    FOnSpawnTimeOut : TStSpawnTimeOutEvent;
    FRunParameters  : String;
    FShowState      : TStShowState;
    FSpawnCommand   : TStSpawnCommand;
    FTimer          : TTimer;
    FTimeOut        : Integer;
    FWaitResult     : DWORD;
    FWaitThread     : TStWaitThread;
    FSpawnCommandStr     : String;

  protected
    { Protected declarations }

    CountDownValue : Integer;
    procedure DoOnThreadEnd(Sender : TObject);
    procedure SetDefaultDir(const Value : String);                 {!!.02}
    procedure SetFileName(const Value : String);                   {!!.02}
    procedure SetOnCompleted(Value : TStSpawnCompletedEvent);
    procedure SetOnSpawnError(Value : TStSpawnErrorEvent);
    procedure SetNotifyWhenDone(Value : Boolean);
    procedure SetRunParameters(const Value : String);              {!!.02}
    procedure SetShowState(Value : TStShowState);
    procedure SetSpawnCommand(Value : TStSpawnCommand);
    procedure SetSpawnTimeOut(Value : TStSpawnTimeOutEvent);
    procedure SetTimeOut(Value : Integer);
  public
    { Public declarations }

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure CancelWait;
    function Execute : THandle;
  published
    { Published declarations }

    property DefaultDir : String
      read FDefaultDir write SetDefaultDir;

    property FileName : String
      read FFileName write SetFileName;

    property NotifyWhenDone : Boolean
      read FNotifyWhenDone write SetNotifyWhenDone default True;

    property OnCompleted : TStSpawnCompletedEvent
      read FOnCompleted write SetOnCompleted;

    property OnSpawnError : TStSpawnErrorEvent
      read FOnSpawnError write SetOnSpawnError;

    property OnTimeOut : TStSpawnTimeOutEvent
      read FOnSpawnTimeOut write SetSpawnTimeOut;

    property RunParameters : String
      read FRunParameters write SetRunParameters;

    property ShowState : TStShowState
      read FShowState write SetShowState default ssNormal;

    property SpawnCommand : TStSpawnCommand
      read FSpawnCommand write SetSpawnCommand default scOpen;

    property TimeOut : Integer
      read FTimeOut write SetTimeOut default 0;

    property SpawnCommandStr : String
      read FSpawnCommandStr write FSpawnCommandStr;

  end;

implementation
{$IFDEF FPC}
  uses Delphi.Windows ;
{$ENDIF}

{-----------------------------------------------------------------------------}
{                               WIN32  WAIT THREAD                            }
{-----------------------------------------------------------------------------}

const                                                                    {!!.01}
  WAIT_HANDLE_COUNT = 2;                                                 {!!.01}

constructor TStWaitThread.Create(aInst, CancelIt : THandle; ATimeOut : Integer);
begin
  GetMem(WaitFors, WAIT_HANDLE_COUNT * SizeOf(THandle));                 {!!.01}
  WaitFors^[0] := aInst;                                                 {!!.01}
  WaitFors^[1] := CancelIt;                                              {!!.01}
  FTimeOut := ATimeOut * 1000;
  CancelWaitEvent := CancelIt;

  inherited Create(True);
end;

{!!.01 - Added}
destructor TStWaitThread.Destroy;
begin
  FreeMem(WaitFors, WAIT_HANDLE_COUNT * SizeOf(THandle));
  inherited Destroy;
end;
{!!.01 - End Added}

procedure TStWaitThread.Execute;
begin
  if (FTimeOut > 0) then
    WaitResult := WaitForMultipleObjects(WAIT_HANDLE_COUNT, WaitFors,    {!!.01}
      False, FTimeOut)                                                   {!!.01}
  else
    WaitResult := WaitForMultipleObjects(WAIT_HANDLE_COUNT, WaitFors,    {!!.01}
      False, INFINITE);                                                  {!!.01}
end;



{-----------------------------------------------------------------------------}
{                               TStSpawnApplication                           }
{-----------------------------------------------------------------------------}

constructor TStSpawnApplication.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FAllowChange     := True;
  FDefaultDir      := '';
  FFileName        := '';
  FNotifyWhenDone  := True;
  FShowState       := ssNormal;
  FSpawnCommand    := scOpen;
  FSpawnCommandStr := '';
  FTimer           := nil;
  FTimeOut         := 0;

end;

destructor TStSpawnApplication.Destroy;
begin
  FTimer.Free;
  FTimer := nil;

  inherited Destroy;
end;


procedure TStSpawnApplication.CancelWait;
begin
  if (FCancelEvent <> 0) then
    SetEvent(FWaitThread.CancelWaitEvent);
end;


procedure TStSpawnApplication.DoOnThreadEnd(Sender : TObject);
begin
  FWaitResult := FWaitThread.WaitResult;

  case FWaitResult of
    WAIT_FAILED :
      begin
        if (Assigned(FOnSpawnError)) then
          FOnSpawnError(Self, GetLastError);
      end;

    WAIT_TIMEOUT :
      begin
        if Assigned(FOnSpawnTimeOut) then
          FOnSpawnTimeOut(Self);
      end;

    WAIT_OBJECT_0,
    WAIT_OBJECT_0 + 1 :
      begin
        if (FNotifyWhenDone) and (Assigned(FOnCompleted)) then
          FOnCompleted(Self);
      end;
  end;

  if (FCancelEvent <> 0) then begin
    SetEvent(FCancelEvent);
    CloseHandle(FCancelEvent);
    FCancelEvent := 0;
  end;
end;


function TStSpawnApplication.Execute : THandle;
var
  Cmd         : String;
  HowShow     : integer;
  Res         : Bool;
  Startup     : TShellExecuteInfo;

begin
  if (FileName = '') and (RunParameters > '') then
    RaiseStError(EStSpawnError, stscInsufficientData);

  case FSpawnCommand of
    scOpen : Cmd := 'open';
    scPrint: Cmd := 'print';
    scOther: Cmd := FSpawnCommandStr;
  end;

  case FShowState of
    ssNormal       : HowShow := SW_NORMAL;
    ssMinimized    : HowShow := SW_MINIMIZE;
    ssMaximized    : HowShow := SW_SHOWMAXIMIZED;
    ssMinNotActive : HowShow := SW_SHOWMINNOACTIVE;
  else
    HowShow := SW_NORMAL;
  end;
  FInstance := 0;

  with Startup do begin
    cbSize       := SizeOf(Startup);
    fMask        := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI;
    Wnd          := 0;
    lpVerb       := Pointer(Cmd);
    if (FFileName > '') then
      lpFile       := PChar(FFileName)
    else
      lpFile       := nil;
    if (FRunParameters > '') then
      lpParameters := PChar(FRunParameters)
    else
      lpParameters := nil;
    if (FDefaultDir > '') then
      lpDirectory  := PChar(FDefaultDir)
    else
      lpDirectory  := nil;
    nShow        := HowShow;
    hInstApp     := 0;
  end;

  Res := ShellExecuteEx(@Startup);
  FInstance := Startup.hProcess;

  if (not Res) then begin
    Result := 0;
    if (Assigned(FOnSpawnError)) then begin
      FOnSpawnError(Self, GetLastError);
    end;
  end else
    Result := FInstance;

  if (NotifyWhenDone) then begin
    FTimer := nil;
    FCancelEvent := CreateEvent(nil, False, False, PChar(FloatToStr(Now)));

    FWaitThread := TStWaitThread.Create(FInstance, FCancelEvent, FTimeOut);
    FWaitThread.OnTerminate := DoOnThreadEnd;
    FWaitThread.FreeOnTerminate := True;
    FWaitThread.Start;
  end;
end;

procedure TStSpawnApplication.SetDefaultDir(const Value : String); {!!.02}
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FDefaultDir) then
      FDefaultDir := Value;
  end;
end;


procedure TStSpawnApplication.SetFileName(const Value : String);   {!!.02}
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FileName) then
      FFileName := Value;
  end;
end;


procedure TStSpawnApplication.SetNotifyWhenDone(Value : Boolean);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FNotifyWhenDone) then
      FNotifyWhenDone := Value;
  end;
end;


procedure TStSpawnApplication.SetRunParameters(const Value : String);  {!!.02}
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FRunParameters) then
      FRunParameters := Value;
  end;
end;


procedure TStSpawnApplication.SetOnCompleted(Value : TStSpawnCompletedEvent);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then
    FOnCompleted := Value;
end;


procedure TStSpawnApplication.SetOnSpawnError(Value : TStSpawnErrorEvent);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then
    FOnSpawnError := Value;
end;


procedure TStSpawnApplication.SetShowState(Value : TStShowState);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FShowState) then
      FShowState := Value;
  end;
end;


procedure TStSpawnApplication.SetSpawnCommand(Value : TStSpawnCommand);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FSpawnCommand) then
      FSpawnCommand := Value;
  end;
end;


procedure TStSpawnApplication.SetSpawnTimeOut(Value : TStSpawnTimeOutEvent);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then
    FOnSpawnTimeOut := Value;
end;


procedure TStSpawnApplication.SetTimeOut(Value : Integer);
begin
  if (FAllowChange) or (csDesigning in ComponentState) then begin
    if (Value <> FTimeOut) and (Value >= 0) then
      FTimeOut := Value;
  end;
end;


end.
