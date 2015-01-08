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
{* SysTools: StNetMsg.pas 4.04                           *}
{*********************************************************}
{* SysTools: Net Message Class                           *}
{*********************************************************}

{$I StDefine.inc}

{$H+} {Huge strings}

unit StNetMsg;

interface

uses
  Windows, Classes, StBase;

type
  TStNetMessage = class(TStComponent)
   private   { Private Methods/Properties   }
    FAliasNames    : TStringList;
    FMsgFrom       : string;
    FMsgText       : string;
    FMsgTo         : string;
    FServerName    : string;
    FOnMessageSent : TNotifyEvent;
   protected { Protected Methods/Properties }
    function GetServer: string;
    procedure SetServer(Value: string);

   public    { Public Methods/Properties    }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddAlias(AName: string);
    function AliasNames: TStringList;
    procedure RemoveAlias(AName: string);
    procedure Send;
   published { Published Methods/Properties }
    property MsgFrom: string
      read FMsgFrom  write FMsgFrom;
    property MsgText: string
      read FMsgText  write FMsgText;
    property MsgTo  : string
      read FMsgTo    write FMsgTo;
    property Server : string
      read GetServer write SetServer;
    property OnMessageSent: TNotifyEvent
      read FOnMessageSent write FOnMessageSent;
  end;

implementation

uses SysUtils, StStrL, StNetApi,
  Dialogs;

constructor TStNetMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAliasNames := TStringList.Create;
end;

destructor TStNetMessage.Destroy;
begin
  FAliasNames.Free;

  inherited Destroy;
end;

procedure TStNetMessage.AddAlias(AName: string);
var
  ErrorD : DWord;
begin
  ErrorD := StNetMessageNameAdd(FServerName, AName);
  if ErrorD <> NERR_SUCCESS then
    RaiseStWin32Error(EStNetException, ErrorD);
end;

function TStNetMessage.AliasNames: TStringList;
var
  ErrorD : DWord;
  Buffer : Pointer;
  TotalEntries : DWord;
  EntriesRead : DWord;
  I : Integer;
begin
  ErrorD := StNetMessageNameEnum(FServerName, 0, Buffer, DWord(-1),
                                 EntriesRead, TotalEntries, nil);
  if ErrorD = NERR_SUCCESS then begin
    FAliasNames.Clear;
    for I := 0 to EntriesRead-1 do begin
      FAliasNames.Add(TMsgInfo0Array(Buffer^)[I].msgi0_name);
    end;
    StNetApiBufferFree(Buffer);
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
  Result := FAliasNames;
end;

procedure TStNetMessage.RemoveAlias(AName: string);
var
  ErrorD : DWord;
begin
  ErrorD := StNetMessageNameDel(FServerName, AName);
  if ErrorD <> NERR_SUCCESS then
    RaiseStWin32Error(EStNetException, ErrorD);
end;

procedure TStNetMessage.Send;
var
  ErrorD : DWord;
  Buffer : TLMWideChar;
begin
  Buffer.Value := nil;
  try
    CvtToWideChar(FMsgText, Buffer);

    ErrorD := StNetMessageBufferSend(FServerName, FMsgTo, FMsgFrom,
                                     Buffer.Value, Buffer.Length);
    if ErrorD <> NERR_SUCCESS then
      RaiseStWin32Error(EStNetException, ErrorD)
    else
      if Assigned(FOnMessageSent) then FOnMessageSent(Self);
  finally
    FreeMem(Buffer.Value, Buffer.Length);
  end;
end;

function TStNetMessage.GetServer: string;
begin
  { don't return any UNC notation }
  Result := FilterL(FServerName, '\');
end;

procedure TStNetMessage.SetServer(Value: string);
begin
  { get rid of any UNC notation or trailing marks }
  Value := FilterL(Value, '\');

  { do we have a valid server name? }
  if (Length(Value) > 0) then

    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
      FServerName := Value
    else
      FServerName := '\\' + Value
  else
    FServerName := Value;
end;


end.
