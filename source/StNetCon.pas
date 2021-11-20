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
{* SysTools: StNetCon.pas 4.04                           *}
{*********************************************************}
{* SysTools: Net Connection Class                        *}
{*********************************************************}

{$I STDEFINE.INC}

{$H+} {Huge strings}

unit StNetCon;

interface

uses
  Windows, Classes, StBase
{$IFDEF FPC}
  , Delphi.Windows
{$ENDIF}
  ;

type
  TStNetConnectOptions    = (coUseConnectDialog, coPersistentConnection,
                             coReadOnlyPath, coUseMRU, coHideRestoreBox,
                             coPromptForAccount, coAlwaysPromptForAccount,
                             coRedirectIfNeeded);
  TStNetDisconnectOptions = (doUseDisconnectDialog, doUpdateProfile,
                             doForceFilesClosed, doPromptToForceFilesClosed);

  TStNetConnectOptionsSet    = set of TStNetConnectOptions;
  TStNetDisconnectOptionsSet = set of TStNetDisconnectOptions;

  TOnConnectFailEvent      = procedure(Sender: TObject; ErrorCode: DWord) of object;
  TOnConnectCancelEvent    = procedure(Sender: TObject; ErrorCode: DWord) of object;

  TOnDisconnectFailEvent   = procedure(Sender: TObject; ErrorCode: DWord) of object;
  TOnDisconnectCancelEvent = procedure(Sender: TObject; ErrorCode: DWord) of object;


  TStNetConnection = class(TStComponent)
   protected { Protected declarations }
    FLocalDevice       : String;
    FPassword          : String;
    FServerName        : String;
    FShareName         : String;
    FUserName          : String;
    FConnectOptions    : TStNetConnectOptionsSet;
    FDisconnectOptions : TStNetDisconnectOptionsSet;

    FOnConnect         : TNotifyEvent;
    FOnConnectFail     : TOnConnectFailEvent;
    FOnConnectCancel   : TOnConnectCancelEvent;
    FOnDisconnect      : TNotifyEvent;
    FOnDisconnectFail  : TOnDisconnectFailEvent;
    FOnDisconnectCancel: TOnDisconnectCancelEvent;
   private   { Private declarations   }
    function GetServerName: string;
    procedure SetServerName(Value: string);
   public    { Public declarations    }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Connect: DWord;
    function Disconnect: DWord;

    property Password: String read FPassword write FPassword;
    property UserName: String read FUserName write FUserName;
   published { Published declarations }
    property ConnectOptions : TStNetConnectOptionsSet
      read FConnectOptions write FConnectOptions;
    property DisconnectOptions : TStNetDisconnectOptionsSet
      read FDisconnectOptions write FDisconnectOptions;

    property LocalDevice: String
      read FLocalDevice write FLocalDevice;
    property ServerName : String
      read GetServerName write SetServerName;
    property ShareName  : String
      read FShareName write FShareName;

    property OnConnect: TNotifyEvent
      read FOnConnect write FOnConnect;
    property OnConnectFail: TOnConnectFailEvent
      read FOnConnectFail write FOnConnectFail;
    property OnConnectCancel: TOnConnectCancelEvent
      read FOnConnectCancel write FOnConnectCancel;
    property OnDisconnect: TNotifyEvent
      read FOnDisconnect write FOnDisconnect;
    property OnDisconnectFail: TOnDisconnectFailEvent
      read FOnDisconnectFail write FOnDisconnectFail;
    property OnDisconnectCancel: TOnDisconnectCancelEvent
      read FOnDisconnectCancel write FOnDisconnectCancel;
  end;

implementation

uses StStrL,
  SysUtils;

constructor TStNetConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectOptions := [coUseConnectDialog, coUseMRU, coPromptForAccount];
  FDisconnectOptions := [doUseDisconnectDialog, doPromptToForceFilesClosed];
end;

destructor TStNetConnection.Destroy;
begin
  inherited Destroy;
end;

function TStNetConnection.GetServerName: string;
begin
  { don't return any UNC notation }
  Result := FilterL(FServerName, '\');
end;

procedure TStNetConnection.SetServerName(Value : string);
begin
  { get rid of any UNC notation or trailing marks }
  Value := FilterL(Value, '\');

  { do we have a valid server name? }
  if (Length(Value) > 0) then
      FServerName := '\\' + Value
  else
    FServerName := Value;
end;

function TStNetConnection.Connect: DWord;
var
  CDS            : TConnectDlgStruct;
  NR             : TNetResource;
  ServerAndShare : String;
  DevRedirect    : Pointer;
  DevRedirectSize: DWord;
  COFlags        : DWord;
  LDResult       : DWord;
  UN, PW         : PChar;
  X : string;
begin
  { Fill in the structures with 'nil' values as the default }
  FillChar(CDS, SizeOf(CDS), 0);
  FillChar(NR, SizeOf(NR), 0);

  { we can only connect to DISK resources }
  NR.dwType        := RESOURCETYPE_DISK;

  { fill in the default server and share names }
  if (Length(FServerName) > 0) then begin
    ServerAndShare := FServerName;
    if (Length(FShareName) > 0) then
      ServerAndShare := ServerAndShare + '\' + FShareName;
    NR.lpRemoteName := PChar(ServerAndShare);
  end;

  { Get the needed memory for any device redirections that occur .. 20 seems like a good buffer }
  DevRedirectSize := Length(NR.lpRemoteName) + 20;
  GetMem(DevRedirect, DevRedirectSize * SizeOf(Char));
  LDResult := 0;

  { do we have a LocalDevice name to use? }
  if Length(FLocalDevice) > 0 then
    NR.lpLocalName := PChar(FLocalDevice);


  if (coUseConnectDialog in FConnectOptions) then begin
    { always set the size of the record structure }
    CDS.cbStructure := SizeOf(CDS);

    { what options, if any, do we need to display? }
    if (coReadOnlyPath in FConnectOptions) and (Length(NR.lpRemoteName) > 1) and
     (not (coUseMRU in FConnectOptions)) then
      CDS.dwFlags   := CDS.dwFlags + CONNDLG_RO_PATH;
    if (coUseMRU in FConnectOptions) then
      CDS.dwFlags   := CDS.dwFlags + CONNDLG_USE_MRU;
    if (coHideRestoreBox in FConnectOptions) then
      CDS.dwFlags   := CDS.dwFlags + CONNDLG_HIDE_BOX;
    if (coPersistentConnection in FConnectOptions) then
      CDS.dwFlags   := CDS.dwFlags + CONNDLG_PERSIST
    else
      CDS.dwFlags   := CDS.dwFlags + CONNDLG_NOT_PERSIST;

    { set the netresource information of the connect structure }
    CDS.lpConnRes   := @NR;

    { call the API and display the dialog }
    Result := WNetConnectionDialog1(CDS);
    if (Result = NO_ERROR) and (CDS.dwDevNum > 0) then begin
      LDResult := CONNECT_LOCALDRIVE;
      X := Char(Ord('A') + CDS.dwDevNum - 1) + ':';
      StrCopy(DevRedirect, PChar(X));
    end;
  end else begin
    { fill in the necessary NetResource information }
    COFlags := 0;
    if (coAlwaysPromptForAccount in FConnectOptions) then
      COFlags := COFlags + CONNECT_INTERACTIVE + CONNECT_PROMPT
    else if (coPromptForAccount in FConnectOptions) then
      COFlags := COFlags + CONNECT_INTERACTIVE;
    if (coRedirectIfNeeded in FConnectOptions) then
      COFlags := COFlags + CONNECT_REDIRECT;
    if (coPersistentConnection in FConnectOptions) then
      COFLags := COFlags + CONNECT_UPDATE_PROFILE;

    { Set up the Username and password }
    UN := nil;
    PW := nil;
    if Length(FUserName) > 0 then
      UN := PChar(FUserName);
    if Length(FPassword) > 0 then
      PW := PChar(FPassword);

    { Call the API .. the Parameter order is different for NT and 9x }

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      Result := WNetUseConnection(0, NR, UN, PW, COFlags, DevRedirect,
                                  DevRedirectSize, LDResult)
    else
      Result := WNetUseConnection(0, NR, PW, UN, COFlags, DevRedirect,
                                  DevRedirectSize, LDResult);

{
    Result := WNetUseConnection(0, NR, UN, PW, COFlags, DevRedirect,
                                DevRedirectSize, LDResult);
    if Result = ERROR_INVALID_PASSWORD then
      Result := WNetUseConnection(0, NR, PW, UN, COFlags, DevRedirect,
                                DevRedirectSize, LDResult);
}
  end;

  case Result of
    NO_ERROR  :
      if Assigned(FOnConnect) then
        FOnConnect(Self);
    1223, $FFFFFFFF :
      if Assigned(FOnConnectCancel) then
        FOnConnectCancel(Self, Result);
  else
    if Assigned(FOnConnectFail) then
      FOnConnectFail(Self, Result)
  end;

  { Free up the device redirection memory }
  FreeMem(DevRedirect);
end;

function TStNetConnection.Disconnect: DWord;
var
  DDS : TDiscDlgStruct;
  ServerAndShare : String;
  UpdateProfile : DWord;
begin
  if (doUseDisconnectDialog in FDisconnectOptions) then begin
    Result := WNetDisconnectDialog(0, RESOURCETYPE_DISK);
  end else begin
    { fill in the default server and share names }
    if (Length(FServerName) > 0) then begin
      ServerAndShare := FServerName;
      if (Length(FShareName) > 0) then
        ServerAndShare := ServerAndShare + '\' + FShareName;
    end;

    if (doForceFilesClosed in FDisconnectOptions) and
       (not (doPromptToForceFilesClosed in FDisconnectOptions)) then begin
      { what options, if any, do we need? }
      if (doUpdateProfile in FDisconnectOptions) then
        UpdateProfile := CONNECT_UPDATE_PROFILE
      else
        UpdateProfile := 0;

      { call the API }
      if Length(FLocalDevice) > 0 then
        Result := WNetCancelConnection2(PChar(FLocalDevice),
                                        UpdateProfile, True)
      else
        Result := WNetCancelConnection2(PChar(ServerAndShare),
                                        UpdateProfile, True)
    end else begin
      { Fill in the structure with 'nil' values as the default }
      FillChar(DDS, SizeOf(DDS), 0);

      { always set the size of the record structure }
      DDS.cbStructure := SizeOf(DDS);

      if Length(FLocalDevice) > 0 then
        DDS.lpLocalName := PChar(FLocalDevice);

      DDS.lpRemoteName := PChar(ServerAndShare);

      { what options, if any, do we need to display? }
      if (doUpdateProfile in FDisconnectOptions) then
        DDS.dwFlags := DDS.dwFlags + DISC_UPDATE_PROFILE;

      if not (doForceFilesClosed in FDisconnectOptions) then
        DDS.dwFlags := DDS.dwFlags + DISC_NO_FORCE;

      { call the API }
      Result := WNetDisconnectDialog1(DDS);
    end;
  end;

  case Result of
    NO_ERROR  :
      if Assigned(FOnDisconnect) then
        FOnDisconnect(Self);
    $FFFFFFFF :
      if Assigned(FOnDisconnectCancel)
        then FOnDisconnectCancel(Self, Result);
  else
    if Assigned(FOnDisconnectFail) then
      FOnDisconnectFail(Self, Result)
  end;
end;

end.
