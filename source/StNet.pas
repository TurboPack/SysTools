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
{* SysTools: StNet.pas 4.04                              *}
{*********************************************************}
{* SysTools: Base Unit for Network Components            *}
{*********************************************************}

{$I StDefine.inc}

{$H+} {Huge strings}

unit StNet;

interface

uses
  Windows, Classes, StBase, StDate, StNetApi, StConst;

type
  TStNetItemType = (nitUnknown, nitLocalUser, nitGlobalUser, nitLocalGroup,
                    nitGlobalGroup, nitComputer, nitInterdomainTrust,
                    nitWorkstationTrust, nitServerTrust, nitShare);

  TStNetSidType  = (nstNone, nstUser, nstGroup, nstDomain, nstAlias,
                    nstWellKnownGroup, nstDeletedAccount, nstInvalid, nstUnknown,
                    nstComputer);

  TStNetUserPrivType     = (uptUnknown, uptGuest, uptUser, uptAdmin);
  TStNetUserAuthPrivType = (uaptPrint, uaptCommunications, uaptServer,
                            uaptAccounts);
  TStNetUserAuthPrivSet  = set of TStNetUserAuthPrivType;

  TStNetShareType = (stUnknown, stDisk, stPrint, stDevice, stIPC, stSpecial);

  TStNetServerPlatformType = (sptUnknown, sptDOS, sptOS2, sptNT, sptOSF, sptVMS);

  TStNetServerType = (nsvtWorkstation, nsvtServer, nsvtSQLServer, nsvtDomainCtrl,
                      nsvtDomainBackupCtrl, nsvtTimeSource, nsvtAFP, nsvtNovell,
                      nsvtDomainMember, nsvtPrintQServer, nsvtDialinServer,
                      nsvtUNIXServer, nsvtNT, nsvtWFW, nsvtMFPN, nsvtServerNT,
                      nsvtPotentialBrowser, nsvtBackupBrowser, nsvtMasterBrowser,
                      nsvtDomainMaster, nsvtOSF, nsvtVMS, nsvtWindows,
                      nsvtDFS, nsvtClusterNT, nsvtTerminalServer, nsvtDCE,
                      nsvtAlternateXPORT, nsvtLocalListOnly, nsvtDomainEnum);

  TStNetServerSet  = set of TStNetServerType;

  TStNetUserLogonTimes = array[TStDayType] of array[0..23] of Boolean;

  TStNetServerRoleType = (srtUnknown, srtStandAlone, srtMember, srtBackup,
                          strPrimary);

  TStNetwork = class;

  TStSidRecord = record
    Value : Pointer;
    ValueS: string;
    Length: DWord;
    Usage : TStNetSidType;
  end;

  TStNetItem = class(TObject)
   private
    FNetwork : TStNetwork;
    FItemType: TStNetItemType;
    FName    : string;
    FServer  : string;
   protected
    FComment : string;
    constructor CreateEx(AName, AComment, AServer:
                         string; AItemType: TStNetItemType);
   public
    constructor Create;
    destructor Destroy; override;

    property Comment : string
      read FComment;
    property ItemType: TStNetItemType
      read FItemType;
    property Name    : string
      read FName;
    property Server  : string
      read FServer;
  end;

  TStNetUserItem = class(TStNetItem)
   private
    FFullName : string;
    FID       : Cardinal;
    FGroupList: TStrings;
    FUserData : Pointer;
    FSidRecord: TStSidRecord;
    FDomain   : string;
    FWorkStationList : TStrings;
   protected
    function GetAccountDisabled      : Boolean;
    function GetAccountExpires       : TStDateTimeRec;
    function GetBadPasswordCount     : Cardinal;
    function GetDomain               : string;
    function GetFullName             : string;
    function GetGroupItemList        : TStringList;
    function GetHomeDirectory        : string;
    function GetHomeDrive            : string;
    function GetLastLogon            : TStDateTimeRec;
    function GetLastLogoff           : TStDateTimeRec;
    function GetLockedOut            : Boolean;
    function GetName                 : string;
    function GetNoUserPasswordChange : Boolean;
    function GetNumberOfLogons       : Cardinal;
    function GetOperatorPrivilege    : TStNetUserAuthPrivSet;
    function GetPasswordNeverExpires : Boolean;
    function GetPasswordExpired      : Boolean;
    function GetPasswordLastChanged  : TStDateTimeRec;
    function GetPasswordNotRequired  : Boolean;
    function GetPrimaryGroup         : TStNetItem;
    function GetProfilePath          : string;
    function GetSid                  : TStSidRecord;
    function GetScriptPath           : string;
    function GetUserComment          : string;
    function GetUserPrivilege        : TStNetUserPrivType;
    function GetWorkstations         : TStrings;

    procedure SetAccountDisabled(Value: Boolean);
    procedure SetAccountExpires(Value: TStDateTimeRec);
    procedure SetComment(Value: string);
    procedure SetFullName(Value: string);
    procedure SetHomeDirectory(Value: string);
    procedure SetHomeDrive(Value: string);
    procedure SetLockedOut(Value: Boolean);
    procedure SetName(Value: string);
    procedure SetNoUserPasswordChange(Value: Boolean);
    procedure SetPasswordAdmin(Value: string);
    procedure SetPasswordNeverExpires(Value: Boolean);
    procedure SetPasswordExpired(Value: Boolean);
    procedure SetPasswordNotRequired(Value: Boolean);
    procedure SetPrimaryGroup(Value: TStNetItem);
    procedure SetProfilePath(Value: string);
    procedure SetScriptPath(Value: string);
    procedure SetUserComment(Value: string);
    procedure SetWorkstations(Value: TStrings);

    constructor CreateEx(AName, AComment, AServer: string;
                         AItemType: TStNetItemType);
    constructor CreateExEnum(AName, AComment, AServer, AFullName: string;
                             AGlobal: Boolean; AID: Cardinal);
   public
    destructor Destroy; override;
    procedure Delete;
    procedure Refresh;
    procedure AddToGroup(AGroup: TStNetItem);
    procedure RemoveFromGroup(AGroup: TStNetItem);

    procedure GetLogonHours(var LogonHours: TStNetUserLogonTimes);
    procedure SetLogonHours(LogonHours: TStNetUserLogonTimes);
    procedure SetPassword(OldPassword, NewPassword: string);

    property AccountDisabled     : Boolean
      read GetAccountDisabled      write SetAccountDisabled;
    property AccountExpires      : TStDateTimeRec
      read GetAccountExpires       write SetAccountExpires;
    property BadPasswordCount    : Cardinal
      read GetBadPasswordCount;
    property Comment             : string
      read FComment                write SetComment;
    property Domain              : string
      read GetDomain;
    property FullName            : string
      read GetFullName             write SetFullName;
    property Groups              : TStringList
      read GetGroupItemList;
    property HomeDirectory       : string
      read GetHomeDirectory        write SetHomeDirectory;
    property HomeDrive           : string
      read GetHomeDrive            write SetHomeDrive;
    property ID                  : Cardinal
      read FID;
    property LastLogon           : TStDateTimeRec
      read GetLastLogon;
    property LastLogoff          : TStDateTimeRec
      read GetLastLogoff;
    property LockedOut           : Boolean
      read GetLockedOut            write SetLockedOut;
    property Name                : string
      read GetName                 write SetName;
    property NoUserPasswordChange: Boolean
      read GetNoUserPasswordChange write SetNoUserPasswordChange;
    property NumberOfLogons      : Cardinal
      read GetNumberOfLogons;
    property OperatorPrivilege   : TStNetUserAuthPrivSet
      read GetOperatorPrivilege;
    property Password            : string
                                   write SetPasswordAdmin;
    property PasswordNeverExpires: Boolean
      read GetPasswordNeverExpires write SetPasswordNeverExpires;
    property PasswordExpired     : Boolean
      read GetPasswordExpired      write SetPasswordExpired;
    property PasswordLastChanged : TStDateTimeRec
      read GetPasswordLastChanged;
    property PasswordNotRequired : Boolean
      read GetPasswordNotRequired  write SetPasswordNotRequired;
    property PrimaryGroup        : TStNetItem
      read GetPrimaryGroup         write SetPrimaryGroup;
    property ProfilePath         : string
      read GetProfilePath          write SetProfilePath;
    property ScriptPath          : string
      read GetScriptPath           write SetScriptPath;
    property Sid                 : TStSidRecord
      read GetSid;
    property UserComment         : string
      read GetUserComment          write SetUserComment;
    property UserPrivilege       : TStNetUserPrivType
      read GetUserPrivilege;
    property Workstations        : TStrings
      read GetWorkstations         write SetWorkstations;
  end;

  TStNetGroupItem = class(TStNetItem)
   private
    FID       : Cardinal;
    FItemList : TStrings;
    FSidRecord: TStSidRecord;
    FDomain   : string;
   protected
    constructor CreateEx(AName, AComment, AServer: string;
                         AItemType: TStNetItemType);
    constructor CreateExEnum(AName, AComment, AServer: string; AID: Cardinal);

    function GetGroupItemList: TStringList;
    function GetDomain: string;
    function GetSid: TStSidRecord;
    function GetName: string;

    procedure SetComment(Value: string);
    procedure SetName(Value: string);
   public
    destructor Destroy; override;

    procedure AddToGroup(AItem: TStNetItem);
    procedure RemoveFromGroup(AItem: TStNetItem);
    procedure Delete;
    procedure Refresh;

    property Comment  : string read FComment write SetComment;
    property Name     : string read GetName write SetName;
    property ID       : Cardinal read FID;
    property Items    : TStringList read GetGroupItemList;
    property Sid      : TStSidRecord read GetSid;
    property Domain   : string read GetDomain;
  end;

  TStNetShareItem = class(TStNetItem)
   private
    FShareType : TStNetShareType;
   protected
    constructor CreateEx(AName, AComment, AServer: string;
                         AShareType: TStNetShareType);
   public
    property ShareType : TStNetShareType read FShareType;
  end;

  TStNetServerItem = class(TStNetItem)
   private
    FUserList    : TStringList;
    FGroupList   : TStringList;
    FDriveList   : TStringList;
    FShareList   : TStringList;
    FServerData  : Pointer;
    FServerMData0: Pointer;
    FServerMData1: Pointer;
    FServerMData2: Pointer;
    FServerMData3: Pointer;
   protected
    constructor CreateEx(AName, AComment: string);

    function GetUser(AName: string): TStNetUserItem;
    function GetGroup(AName: string): TStNetGroupItem;

    function GetDriveList: TStringList;
    function GetUserList: TStringList;
    function GetGroupList : TStringList;
    function GetShareList : TStringList;

    procedure SetComment(Value: string);

    function GetAnnounceRate: DWord;
    function GetAnnounceRateDelta: DWord;
    function GetDisconnectTime: DWord;
    function GetMaxUsers: DWord;
    function GetPlatform: TStNetServerPlatformType;
    function GetServerType: TStNetServerSet;
    function GetUserPath: string;
    function GetVisible: Boolean;
    function GetVersion: DWord;

    function GetMinPasswordLen: DWord;
    function GetMaxPasswordAge: DWord;
    function GetMinPasswordAge: DWord;
    function GetForceLogoff: TStTime;
    function GetPasswordHistoryLength: DWord;
    function GetRole: TStNetServerRoleType;
    function GetPrimaryDC: string;
    function GetDomainName: string;
    function GetDomainSid : TStSidRecord;
    function GetLockOutDuration : DWord;
    function GetLockoutObservationWindow: DWord;
    function GetLockoutThreshold: DWord;

    procedure SetMinPasswordLen(Value: DWord);
    procedure SetMaxPasswordAge(Value: DWord);
    procedure SetMinPasswordAge(Value: DWord);
    procedure SetForceLogoff(Value: TStTime);
    procedure SetPasswordHistoryLength(Value: DWord);
    procedure SetLockOutDuration(Value: DWord);
    procedure SetLockoutObservationWindow(Value: DWord);
    procedure SetLockoutThreshold(Value: DWord);

    procedure SetAnnounceRate(Value: DWord);
    procedure SetAnnounceRateDelta(Value: DWord);
    procedure SetDisconnectTime(Value: DWord);
    procedure SetMaxUsers(Value: DWord);
    procedure SetVisible(Value: Boolean);

    procedure RefreshM0;
    procedure RefreshM1;
    procedure RefreshM2;
    procedure RefreshM3;
   public
    destructor Destroy; override;

    function AddGroup(AName, ADescription: string; AGlobal: Boolean): TStNetGroupItem;
    function AddUser(AName, APassword: string; AGlobal: Boolean): TStNetUserItem;
    procedure Refresh;

    property AnnounceRate      : DWord
      read GetAnnounceRate write SetAnnounceRate;
    property AnnounceRateDelta : DWord
      read GetAnnounceRateDelta write SetAnnounceRateDelta;
    property Comment           : string
      read FComment write SetComment;
    property DisconnectTime    : DWord
      read GetDisconnectTime write SetDisconnectTime;
    property MaxUsers          : DWord
      read GetMaxUsers write SetMaxUsers;
    property Platform          : TStNetServerPlatformType
      read GetPlatform;
    property ServerType        : TStNetServerSet
      read GetServerType;
    property UserPath          : string
      read GetUserPath;
    property Version           : DWord
      read GetVersion;
    property Visible           : Boolean
      read GetVisible write SetVisible;

    { MODALS_INFO_0 }
    property MinPasswordLen        : DWord
      read GetMinPasswordLen write SetMinPasswordLen;
    property MaxPasswordAge        : DWord
      read GetMaxPasswordAge write SetMaxPasswordAge;
    property MinPasswordAge        : DWord
      read GetMinPasswordAge write SetMinPasswordAge;
    property ForceLogoff           : TStTime
      read GetForceLogoff    write SetForceLogoff;
    property PasswordHistoryLength : DWord
      read GetPasswordHistoryLength write SetPasswordHistoryLength;

    { MODALS_INFO_1 }
    property Role                  : TStNetServerRoleType
      read GetRole;
    property PrimaryDC             : string
      read GetPrimaryDC;

    { MODALS_INFO_2 }
    property DomainName            : string
      read GetDomainName;
    property DomainSid             : TStSidRecord
      read GetDomainSid;

    { MODALS_INFO_3 }
    property LockOutDuration       : DWord
      read GetLockOutDuration write SetLockOutDuration;
    property LockoutObservationWindow: DWord
      read GetLockoutObservationWindow write SetLockoutObservationWindow;
    property LockoutThreshold      : DWord
      read GetLockoutThreshold write SetLockoutThreshold;

    property User[AName: string] : TStNetUserItem
      read GetUser;
    property Group[AName: string] : TStNetGroupItem
      read GetGroup;
    property Drives : TStringList
      read GetDriveList;
    property Users: TStringList
      read GetUserList;
    property Groups: TStringList
      read GetGroupList;
    property Shares: TStringList
      read GetShareList;
  end;

  TStNetwork = class(TObject)
   private
    FList : TStringList;
   protected
    function GetServer(AServer: string): TStNetServerItem;
    function GetUser(AServer, AName: string): TStNetUserItem;
    function GetGroup(AServer, AName: string): TStNetGroupItem;
    function GetPrimaryDC(ADomain: string): TStNetServerItem;
   public
    constructor Create;
    destructor Destroy; override;

    property Server[AServer: string] : TStNetServerItem
      read GetServer;
    property User[AServer, AName: string]: TStNetUserItem
      read GetUser;
    property Group[AServer, AName: string]: TStNetGroupItem
      read GetGroup;
    property PrimaryDC[ADomain: string] : TStNetServerItem
      read GetPrimaryDC;
  end;

var
  StNetwork : TStNetwork;

implementation

uses SysUtils, StUtils, StSystem, StStrL;

{ --- TStNetItem ------------------------------------------------------------ }
constructor TStNetItem.Create;
begin
  { don't allow manual creation of this object }
  RaiseStError(EStNetException, stscNetNoManualCreate);
end;

constructor TStNetItem.CreateEx(AName, AComment, AServer: string;
                                AItemType: TStNetItemType);
begin
  inherited Create;
  FName := AName;
  FComment := AComment;
  FServer := AServer;
  FItemType := AItemType;
  FNetwork := nil;
end;

destructor TStNetItem.Destroy;
begin
  inherited Destroy;
end;

{ --- TStNetUserItem -------------------------------------------------------- }

constructor TStNetUserItem.CreateEx(AName, AComment, AServer: string;
                                    AItemType: TStNetItemType);
var
  ErrorD : DWord;
  Flags  : DWord;
begin
  inherited CreateEx(AName, AComment, AServer, AItemType);
  FGroupList := TStringList.Create;
  FUserData  := nil;

  { need to find out what type of user this is }
  if (AItemType = nitUnknown) then begin
    ErrorD := StNetUserGetInfo(FServer, FName, 3, FUserData);
    if ErrorD = NERR_SUCCESS then begin
      Flags := TUSER_INFO_3(FUserData^).usri3_flags;
      if LongFlagIsSet(Flags, UF_TEMP_DUPLICATE_ACCOUNT) then
        FItemType := nitLocalUser
      else if LongFlagIsSet(Flags, UF_NORMAL_ACCOUNT) then
        FItemType := nitGlobalUser
      else if LongFlagIsSet(Flags, UF_INTERDOMAIN_TRUST_ACCOUNT) then
        FItemType := nitInterdomainTrust
      else if LongFlagIsSet(Flags, UF_WORKSTATION_TRUST_ACCOUNT) then
        FItemType := nitWorkstationTrust
      else if LongFlagIsSet(Flags, UF_SERVER_TRUST_ACCOUNT) then
        FItemType := nitServerTrust;

      FComment := TUSER_INFO_3(FUserData^).usri3_comment;
      FID      := TUSER_INFO_3(FUserData^).usri3_user_id;
      FFullName:= TUSER_INFO_3(FUserData^).usri3_full_name;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

constructor TStNetUserItem.CreateExEnum(AName, AComment, AServer,
                                        AFullName: string; AGlobal: Boolean;
                                        AID: Cardinal);
begin
  inherited CreateEx(AName, AComment, AServer, nitUnknown);
  FGroupList := TStringList.Create;

  FFullName := AFullName;
  FID       := AID;

  if AGlobal then
    FItemType := nitGlobalUser
  else
    FItemType := nitLocalUser;
end;

destructor TStNetUserItem.Destroy;
begin
  FGroupList.Free;

  if (FSidRecord.Value <> nil) then
    StNetApiBufferFree(FSidRecord.Value);

  if (FUserData <> nil) then
    StNetApiBufferFree(FUserData);

  inherited Destroy;
end;

function TStNetUserItem.GetGroupItemList: TStringList;
var
  NewList      : TStringList;
  ErrorD       : DWord;
  Index        : Integer;
  Buffer       : Pointer;
  EntriesRead  : DWord;
  TotalEntries : DWord;
  S            : TStNetServerItem;
  NewItem      : TStNetGroupItem;
  CurrentName  : string;
begin
  NewList := TStringList.Create;
  try
    { get the list of global groups that this user belongs to }
    ErrorD := StNetUserGetGroups(FServer, FName, 0, Buffer, DWord(-1),
                                 EntriesRead, TotalEntries);
    if ErrorD = NERR_SUCCESS then begin
      try
        if EntriesRead > 0 then begin                                  {!!.02}
          for Index := 0 to EntriesRead-1 do begin
            CurrentName := TGroupUsersInfo0Array(Buffer^)[Index].grui0_name;
            { does this group already exist in the servers list of groups }
            NewItem := nil;
            if (FNetwork <> nil) then begin
              S := FNetwork.Server[FServer];
              NewItem := S.Group[CurrentName];
              NewItem.FItemType := nitGlobalGroup;
            end;

            if (NewItem = nil) then begin
              NewItem := TStNetGroupItem.CreateEx(CurrentName, '', FServer,
                                                  nitGlobalGroup);
              NewItem.FNetwork := FNetwork;
            end;
            NewList.AddObject(CurrentName, NewItem);
          end;
        end;                                                           {!!.02}
      finally
        StNetApiBufferFree(Buffer);
        Buffer := nil;
      end;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;

    { get the list of local groups that this user belongs to }
    ErrorD := StNetUserGetLocalGroups(FServer, FName, 0, 0, Buffer,
                               DWord(-1), EntriesRead, TotalEntries);
    if ErrorD = NERR_SUCCESS then begin
      try
        if EntriesRead > 0 then begin                                  {!!.02}
          for Index := 0 to EntriesRead-1 do begin
            CurrentName := TLocalGroupUsersInfo0Array(Buffer^)[Index].lgrui0_name;
            { does this group already exist in the servers list of groups }
            NewItem := nil;
            if (FNetwork <> nil) then begin
              S := FNetwork.Server[FServer];
              NewItem := S.Group[CurrentName];
              NewItem.FItemType := nitLocalGroup;
            end;

            if (NewItem = nil) then begin
              NewItem := TStNetGroupItem.CreateEx(CurrentName, '', FServer,
                                                  nitLocalGroup);
              NewItem.FNetwork := FNetwork;
            end;
            NewList.AddObject(CurrentName, NewItem);
          end;
        end;                                                           {!!.02}
      finally
        StNetApiBufferFree(Buffer);
        Buffer := nil;
      end;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;

    FGroupList.Assign(NewList);
  finally
    NewList.Free;
  end;
  Result := FGroupList as TStringList
end;

function TStNetUserItem.GetPrimaryGroup: TStNetItem;
var
  GList : TStringList;
  I : Integer;
begin
  GList := Groups;
  Result := nil;
  for I := 0 to GList.Count -1 do begin
    if TStNetGroupItem(GList.Objects[I]).ID = TUSER_INFO_3(FUserData^).usri3_primary_group_id then begin
      Result := TStNetItem(GList.Objects[I]);
      Exit;
    end;
  end;
end;

function TStNetUserItem.GetDomain: string;
begin
  if (FSidRecord.Value = nil) then begin
    FSidRecord := GetSid;
  end;
  Result := FDomain;
end;

function TStNetUserItem.GetSid: TStSidRecord;
var
  NoError: Boolean;
  _SidUse: DWord;
  Index  : Integer;
begin
  if (FSidRecord.Value = nil) then begin
    NoError := StLookupAccountName(FServer, FName, FSidRecord.Value,
                                   FSidRecord.Length, FDomain, _SidUse);
    if NoError then begin
      { and the SID usage flag to the enumeration }
      FSidRecord.Usage := TStNetSidType(_SidUse);

      { convert the sid to a readable string }
      FSidRecord.ValueS :=
        Format('S-1-%d',
            [TSIDIdentifierAuthority(
               StGetSidIdentifierAuthority(FSidRecord.Value)^).Value[5]]);

          for Index := 0 to StGetSidSubAuthorityCount(FSidRecord.Value) - 1 do begin
            FSidRecord.ValueS :=
              Format(FSidRecord.ValueS + '-%d', [StGetSidSubAuthority(
                                                  FSidRecord.Value, Index)]);
          end;
    end else begin
      RaiseStWin32Error(EStNetException, GetLastError);
    end;
  end;
  Result := FSidRecord;
end;

procedure TStNetUserItem.Refresh;
var
  ErrorD  : DWord;
  Buffer  : Pointer;
begin
  { get the account name, comment and ID for the item }
  ErrorD := StNetUserGetInfo(FServer, FName, 3, Buffer);
  if ErrorD = NERR_SUCCESS then begin
    StNetApiBufferFree(FUserData);
    FUserData := Buffer;

    FComment := TUSER_INFO_3(FUserData^).usri3_comment;
    FID      := TUSER_INFO_3(FUserData^).usri3_user_id;

    if LongFlagIsSet(TUSER_INFO_3(FUserData^).usri3_flags, UF_NORMAL_ACCOUNT) then
      FItemType := nitGlobalUser
    else if LongFlagIsSet(TUSER_INFO_3(FUserData^).usri3_flags, UF_TEMP_DUPLICATE_ACCOUNT) then
      FItemType := nitLocalUser
    else
      FItemType := nitUnknown;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetUserItem.AddToGroup(AGroup : TStNetItem);
var
  ErrorD : DWord;
begin
  if AGroup.ItemType = nitGlobalGroup then begin
    ErrorD := StNetGroupAddUser(AGroup.Server, AGroup.Name, FName);
    if ErrorD = NERR_SUCCESS then begin
      if FGroupList.IndexOf(AGroup.Name) < 0 then
        FGroupList.AddObject(AGroup.Name, AGroup);
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end else if AGroup.ItemType = nitLocalGroup then begin
    ErrorD := StNetLocalGroupAddMembers(FServer, AGroup.Name, 0, Sid.Value, 1);
    if ErrorD = NERR_SUCCESS then begin
      if FGroupList.IndexOf(AGroup.Name) < 0 then
        FGroupList.AddObject(AGroup.Name, AGroup);
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end else begin
    RaiseStError(EStNetException, stscNetGroupNotSpecified);
  end;
end;

procedure TStNetUserItem.RemoveFromGroup(AGroup: TStNetItem);
var
  ErrorD : DWord;
begin
  if AGroup.ItemType = nitGlobalGroup then begin
    ErrorD := StNetGroupDelUser(FServer, AGroup.Name, FName);
    if ErrorD = NERR_SUCCESS then begin
      FGroupList.Delete(FGroupList.IndexOf(AGroup.Name));
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end else if AGroup.ItemType = nitLocalGroup then begin
    ErrorD := StNetLocalGroupDelMembers(AGroup.Server, AGroup.Name, 0, Sid.Value, 1);
    if ErrorD = NERR_SUCCESS then begin
    FGroupList.Delete(FGroupList.IndexOf(AGroup.Name));
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end else begin
    RaiseStError(EStNetException, stscNetGroupNotSpecified);
  end;
end;

procedure TStNetUserItem.Delete;
  { removes this user from the server }
var
  ErrorD : DWord;
  S : TStNetServerItem;
begin
  { remove from the server }
  ErrorD := StNetUserDel(FServer, FName);
  if ErrorD = NERR_SUCCESS then begin
    if FNetwork <> nil then begin
      S := FNetwork.Server[FServer];
      S.FUserList.Objects[S.FUserList.IndexOf(FName)] := nil;
      S.FUserList.Delete(S.FUserList.IndexOf(FName));
    end;
    Free;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

function TStNetUserItem.GetAccountDisabled: Boolean;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_flags;
  Result := LongFlagIsSet(Data, UF_ACCOUNTDISABLE);
end;

function TStNetUserItem.GetAccountExpires: TStDateTimeRec;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_acct_expires;
  if Data = TIMEQ_FOREVER then begin
    Result.D := 0;
    Result.T := 0;
  end else
    Result := GlobalDateTimeToLocal(UnixTimeToStDateTime(Data), 0);
end;

function TStNetUserItem.GetBadPasswordCount: Cardinal;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_bad_pw_count;
end;

function TStNetUserItem.GetFullName: string;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_full_name;
end;

function TStNetUserItem.GetHomeDirectory: string;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_home_dir;
end;

function TStNetUserItem.GetHomeDrive: string;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_home_dir_drive;
end;

function TStNetUserItem.GetLastLogon: TStDateTimeRec;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_last_logon;
  Result := UnixTimeToStDateTime(Data);
end;

function TStNetUserItem.GetLastLogoff: TStDateTimeRec;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_last_logoff;
  Result := UnixTimeToStDateTime(Data);
end;

function TStNetUserItem.GetLockedOut: Boolean;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_flags;
  Result := LongFlagIsSet(Data, UF_LOCKOUT);
end;

function TStNetUserItem.GetName: string;
begin
  Result := FName;
end;

function TStNetUserItem.GetNoUserPasswordChange: Boolean;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_flags;
  Result := LongFlagIsSet(Data, UF_PASSWD_CANT_CHANGE);
end;

function TStNetUserItem.GetNumberOfLogons: Cardinal;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_num_logons;
end;

function TStNetUserItem.GetOperatorPrivilege: TStNetUserAuthPrivSet;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_auth_flags;
  Result := [];
  if LongFlagIsSet(Data, AF_OP_PRINT) then
    Include(Result, uaptPrint);
  if LongFlagIsSet(Data, AF_OP_COMM) then
    Include(Result, uaptCommunications);
  if LongFlagIsSet(Data, AF_OP_SERVER) then
    Include(Result, uaptServer);
  if LongFlagIsSet(Data, AF_OP_ACCOUNTS) then
    Include(Result, uaptAccounts);
end;

function TStNetUserItem.GetPasswordNeverExpires: Boolean;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_flags;
  Result := LongFlagIsSet(Data, UF_DONT_EXPIRE_PASSWD);
end;

function TStNetUserItem.GetPasswordExpired: Boolean;
begin
  if (FUserData = nil) then
    Refresh;
  Result := Boolean(TUSER_INFO_3(FUserData^).usri3_password_expired);
end;

function TStNetUserItem.GetPasswordLastChanged: TStDateTimeRec;
var
  Current:TStDateTimeRec;
  Data   : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Current.D := CurrentDate;
  Current.T := CurrentTime;
  Data := TUSER_INFO_3(FUserData^).usri3_password_age;
  IncDateTime(Current, Result, -Abs(Data div SecondsInDay),
                               -Abs(Data mod SecondsInDay));
end;

function TStNetUserItem.GetPasswordNotRequired: Boolean;
var
  Data   : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_flags;
  Result := LongFlagIsSet(Data, UF_PASSWD_NOTREQD);
end;

function TStNetUserItem.GetProfilePath: string;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_profile;
end;

function TStNetUserItem.GetScriptPath: string;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_script_path;
end;

function TStNetUserItem.GetUserComment: string;
begin
  if (FUserData = nil) then
    Refresh;
  Result := TUSER_INFO_3(FUserData^).usri3_usr_comment;
end;

function TStNetUserItem.GetUserPrivilege: TStNetUserPrivType;
var
  Data : DWord;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_priv;
  Result := TStNetUserPrivType(Data+1);
end;

function TStNetUserItem.GetWorkstations: TStrings;
var
  Data: string;
begin
  if (FUserData = nil) then
    Refresh;
  Data := TUSER_INFO_3(FUserData^).usri3_workstations;
  FWorkStationList.CommaText := Data;
  Result := FWorkStationList;
end;

procedure TStNetUserItem.SetAccountDisabled(Value: Boolean);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> AccountDisabled then begin
    Data := TUSER_INFO_3(FUserData^).usri3_flags;
    if Value then
      SetLongFlag(Integer(Data), UF_ACCOUNTDISABLE)
    else
      ClearLongFlag(Integer(Data), UF_ACCOUNTDISABLE);

    ErrorD := StNetUserSetInfo(FServer, FName, 1008, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then
      TUSER_INFO_3(FUserData^).usri3_flags := Data
    else
      RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetUserItem.SetAccountExpires(Value: TStDateTimeRec);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
const
  MaxDate = $0002D210; { 2/5/2106 }
begin
  {
    1. Setting Date/Time to 0 will make a Non-Expiring account.
    2. Dates must be >= 1/1/1980 12:00:00 AM
    3. Dates must be <= 2/5/2106 23:59:59 AM
  }
  if (Value.D <> AccountExpires.D) or (Value.T <> AccountExpires.T) then begin
    if (Value.D = 0) and (Value.T = 0) then
      Data := TIMEQ_FOREVER
    else begin
      if ((Value.D >= Date1980) and (Value.T >= 0)) and
         ((Value.D <= MaxDate) and (Value.T < SecondsInDay)) then begin
        Data := StDateTimeToUnixTime(LocalDateTimeToGlobal(Value, 0));
      end else begin
        RaiseStError(EStNetException, stscNetDateSpecifiedOutOfRange);
        Exit;
      end;
    end;

    ErrorD := StNetUserSetInfo(FServer, FName, 1017, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      if (Value.D = 0) and (Value.T = 0) then
        Data := TIMEQ_FOREVER;
      TUSER_INFO_3(FUserData^).usri3_acct_expires := Data;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetUserItem.SetComment(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> Comment then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1007, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_comment);
        TUSER_INFO_3(FUserData^).usri3_comment := Data.Value;
        FComment := Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetFullName(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> FullName then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1011, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_full_name);
        TUSER_INFO_3(FUserData^).usri3_full_name := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetHomeDirectory(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> HomeDirectory then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1006, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_home_dir);
        TUSER_INFO_3(FUserData^).usri3_home_dir := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetHomeDrive(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> HomeDirectory then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1053, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_home_dir_drive);
        TUSER_INFO_3(FUserData^).usri3_home_dir_drive := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetLockedOut(Value: Boolean);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if LockedOut then begin
    if Value = False then begin
      Data := TUSER_INFO_3(FUserData^).usri3_flags;
      ClearLongFlag(Integer(Data), UF_LOCKOUT);

      ErrorD := StNetUserSetInfo(FServer, FName, 1008, @Data, ParmErr);
      if ErrorD = NERR_SUCCESS then
        TUSER_INFO_3(FUserData^).usri3_flags := Data
      else
        RaiseStWin32Error(EStNetException, ErrorD);
    end else begin
      {can only CLEAR a lockout }
      RaiseStError(EStNetException, stscNetInvalidParameter);
    end;
  end;
end;

procedure TStNetUserItem.SetName(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
  S       : TStNetServerItem;
begin
  if Value <> Name then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 0, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_name);
        TUSER_INFO_3(FUserData^).usri3_name := Data.Value;

        S := FNetwork.Server[FServer];
        S.FUserList[S.FUserList.IndexOf(FName)] := Value;;

        FName := Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetNoUserPasswordChange(Value: Boolean);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> NoUserPasswordChange then begin
    Data := TUSER_INFO_3(FUserData^).usri3_flags;
    if Value then
      SetLongFlag(Integer(Data), UF_PASSWD_CANT_CHANGE)
    else
      ClearLongFlag(Integer(Data), UF_PASSWD_CANT_CHANGE);

    ErrorD := StNetUserSetInfo(FServer, FName, 1008, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then
      TUSER_INFO_3(FUserData^).usri3_flags := Data
    else
      RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetUserItem.SetPasswordAdmin(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  Data.Value := nil;
  try
    { allocate the unicode password }
    CvtToWideCharLM(Value, Data);

    ErrorD := StNetUserSetInfo(FServer, FName, 1003, @Data.Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      { no need to update internal data .. password is write-only}
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  finally
    StNetApiBufferFree(Data.Value);
  end;
end;

procedure TStNetUserItem.SetPassword(OldPassword, NewPassword: string);
var
  ErrorD : DWord;
  TempServer : string;
  S : TStNetServerItem;
begin
  { if the server is a BDC, we need to change it on the PDC }
  S := StNetwork.Server[FServer];
  if (nsvtDomainBackupCtrl in S.ServerType) then begin
    TempServer := S.PrimaryDC;
  end else begin
    TempServer := FServer;
  end;

  ErrorD := StNetUserChangePassword(TempServer, FName, OldPassword, NewPassword);
  if ErrorD = NERR_SUCCESS then begin
    { Nothing else to do }
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetUserItem.SetPasswordNeverExpires(Value: Boolean);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> PasswordNeverExpires then begin
    Data := TUSER_INFO_3(FUserData^).usri3_flags;
    if Value then
      SetLongFlag(Integer(Data), UF_DONT_EXPIRE_PASSWD)
    else
      ClearLongFlag(Integer(Data), UF_DONT_EXPIRE_PASSWD);

    ErrorD := StNetUserSetInfo(FServer, FName, 1008, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then
      TUSER_INFO_3(FUserData^).usri3_flags := Data
    else
      RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetUserItem.SetPasswordExpired(Value: Boolean);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> PasswordExpired then begin
    Data := TUSER_INFO_3(FUserData^).usri3_password_expired;
    if Value then
      TUSER_INFO_3(FUserData^).usri3_password_expired := DWord(-1)
    else
      TUSER_INFO_3(FUserData^).usri3_password_expired := 0;

    ErrorD := StNetUserSetInfo(FServer, FName, 3, FUserData, ParmErr);
    if ErrorD = NERR_SUCCESS then
      { The data was already updated }
    else begin
      TUSER_INFO_3(FUserData^).usri3_password_expired := Data;
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetUserItem.SetPasswordNotRequired(Value: Boolean);
var
  Data    : DWord;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> PasswordNotRequired then begin
    Data := TUSER_INFO_3(FUserData^).usri3_flags;
    if Value then
      SetLongFlag(Integer(Data), UF_PASSWD_NOTREQD)
    else
      ClearLongFlag(Integer(Data), UF_PASSWD_NOTREQD);

    ErrorD := StNetUserSetInfo(FServer, FName, 1008, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then
      TUSER_INFO_3(FUserData^).usri3_flags := Data
    else
      RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetUserItem.SetPrimaryGroup(Value: TStNetItem);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : DWord;
begin
  if TStNetGroupItem(Value).ID <> TStNetGroupItem(PrimaryGroup).ID then begin
    Data := TStNetGroupItem(Value).ID;
    ErrorD := StNetUserSetInfo(FServer, FName, 1051, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_INFO_3(FUserData^).usri3_primary_group_id := Data
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetUserItem.SetProfilePath(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> ProfilePath then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1052, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_profile);
        TUSER_INFO_3(FUserData^).usri3_profile := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetScriptPath(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> ScriptPath then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1009, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_script_path);
        TUSER_INFO_3(FUserData^).usri3_script_path := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetUserComment(Value: string);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value <> UserComment then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1012, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_usr_comment);
        TUSER_INFO_3(FUserData^).usri3_usr_comment := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.SetWorkstations(Value: TStrings);
var
  ErrorD  : DWord;
  ParmErr : DWord;
  Data    : TLMWideChar;
begin
  if Value.CommaText <> WorkStations.CommaText then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value.CommaText, Data);
      ErrorD := StNetUserSetInfo(FServer, FName, 1014, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TUSER_INFO_3(FUserData^).usri3_workstations);
        TUSER_INFO_3(FUserData^).usri3_workstations := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

procedure TStNetUserItem.GetLogonHours(var LogonHours: TStNetUserLogonTimes);
{$IFNDEF VERSION4}
const
  TIME_ZONE_ID_INVALID  = DWORD($FFFFFFFF);
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}
type
  LHFlags = array[0..167] of Boolean;
  TLHData = array[TStDayType] of array[0..2] of Byte;
var
  Data        : TLHData;
  GMTOffset   : Integer;
  BitOffset   : Integer;
  DayOfWeek   : Integer;
  HourOfDay   : Integer;
  TZ          : TTimeZoneInformation;
const
  BitFlags : array[0..7] of byte = ($01, $02, $04, $08, $10, $20, $40, $80);
begin
  if (FUserData = nil) then
    Refresh;

  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_UNKNOWN :
      GMTOffset := TZ.Bias div MinutesInHour;
    TIME_ZONE_ID_STANDARD:
      GMTOffset := (TZ.Bias + TZ.StandardBias) div MinutesInHour;
    TIME_ZONE_ID_DAYLIGHT:
      GMTOffset := (TZ.Bias + TZ.DaylightBias) div MinutesInHour;
  else
    GMTOffset := 0;
  end;
  Data := TLHData(TUSER_INFO_3(FUserData^).usri3_logon_hours^);

  { Populate the logon hours array with correct information adjusted for GMT}
  for DayOfWeek := Ord(Sunday) to Ord(Saturday) do begin
    for HourOfDay := 0 to HoursInDay-1 do begin

      { Get the correct offset into the array of booleans }
      BitOffset := -(GMTOffset) + ((DayOfWeek * HoursInDay) + HourOfDay);
      if BitOffset > 168 then
        Dec(BitOffset, 168)
      else if BitOffset < 1 then
        Inc(BitOffset, 168);

      { set the correct True or False flag in the array  }
      LHFlags(LogonHours)[BitOffset-1] :=
        ByteFlagIsSet(Data[TStDayType(DayOfWeek)][HourOfDay div 8],
                      BitFlags[HourOfDay mod 8]);
    end;
  end;
end;

procedure TStNetUserItem.SetLogonHours(LogonHours: TStNetUserLogonTimes);
{$IFNDEF VERSION4}
const
  TIME_ZONE_ID_INVALID  = DWORD($FFFFFFFF);
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}
type
  LHFlags = array[0..167] of Boolean;
  TLHData = array[TStDayType] of array[0..2] of Byte;
var
  Data      : TLHData;
  GMTOffset : Integer;
  BitOffset : Integer;
  DayOfWeek : Integer;
  HourOfDay : Integer;
  TZ        : TTimeZoneInformation;
  USER_INFO : TUSER_INFO_1020;
  ErrorD    : DWord;
  ParmErr   : DWord;
const
  BitFlags : array[0..7] of byte = ($01, $02, $04, $08, $10, $20, $40, $80);
begin
  if (FUserData = nil) then
    Refresh;

  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_UNKNOWN :
      GMTOffset := TZ.Bias div MinutesInHour;
    TIME_ZONE_ID_STANDARD:
      GMTOffset := (TZ.Bias + TZ.StandardBias) div MinutesInHour;
    TIME_ZONE_ID_DAYLIGHT:
      GMTOffset := (TZ.Bias + TZ.DaylightBias) div MinutesInHour;
  else
    GMTOffset := 0;
  end;

  { Default to ALL hours for logon }
  FillChar(Data, SizeOf(Data), 255);

  { Populate the logon hours structure with correct information }
  for DayOfWeek := Ord(Sunday) to Ord(Saturday) do begin
    for HourOfDay := 0 to HoursInDay-1 do begin

      { Get the correct offset into the array of booleans }
      BitOffset := -(GMTOffset) + ((DayOfWeek * HoursInDay) + HourOfDay);
      if BitOffset > 168 then
        Dec(BitOffset, 168)
      else if BitOffset < 1 then
        Inc(BitOffset, 168);

      { just set the True logon hours (already set to False) }
      if not LHFlags(LogonHours)[BitOffset-1] then
        ClearByteFlag(Data[TStDayType(DayOfWeek)][HourOfDay div 8],
                      BitFlags[HourOfDay mod 8]);
    end;
  end;

  { Update the server with the new logon hours }
  USER_INFO.usri1020_logon_hours    := @Data;
  USER_INFO.usri1020_units_per_week := UNITS_PER_WEEK;
  ErrorD := StNetUserSetInfo(FServer, FName, 1020, @USER_INFO, ParmErr);
  if ErrorD = NERR_SUCCESS then begin
    TUSER_INFO_3(FUserData^).usri3_logon_hours := @Data;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;


{ --- TStNetGroupItem ------------------------------------------------------- }

constructor TStNetGroupItem.CreateEx(AName, AComment, AServer: string;
                                     AItemType: TStNetItemType);
var
  Buffer : Pointer;
  ErrorD : DWord;
begin
  inherited CreateEx(AName, AComment, AServer, AItemType);

  FItemList := TStringList.Create;
  FillChar(FSidRecord, SizeOf(TStSidRecord), 0);

  { need to find out what type of group this is }
  if (AItemType = nitUnknown) or ((AItemType = nitGlobalGroup) and (FID = 0)) then begin
    ErrorD := StNetGroupGetInfo(FServer, FName, 2, Buffer);
    if ErrorD = NERR_SUCCESS then begin
      try
        FItemType := nitGlobalGroup;
        FID       := TGROUP_INFO_2(Buffer^).grpi2_group_id;
        FComment  := TGROUP_INFO_2(Buffer^).grpi2_comment;
      finally
        StNetApiBufferFree(Buffer);
        Buffer := nil;
      end;
    end else begin
      { its not a global group ... try a local one }
      ErrorD := StNetLocalGroupGetInfo(FServer, FName, 1, Buffer);
      if ErrorD = NERR_SUCCESS then begin
        try
          FItemType := nitLocalGroup;
          FComment := TLOCALGROUP_INFO_1(Buffer^).lgrpi1_comment;
        finally
          StNetApiBufferFree(Buffer);
          Buffer := nil;
        end;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    end;
  end;
end;

constructor TStNetGroupItem.CreateExEnum(AName, AComment, AServer: string;
                                         AID: Cardinal);
begin
  inherited CreateEx(AName, AComment, AServer, nitUnknown);
  FItemList := TStringList.Create;

  FID := AID;

  if AID > 0 then
    FItemType := nitGlobalGroup
  else
    FItemType := nitLocalGroup;
end;

destructor TStNetGroupItem.Destroy;
begin
  FItemList.Free;

  if (FSidRecord.Value <> nil) then
    StNetApiBufferFree(FSidRecord.Value);

  inherited Destroy;
end;

function TStNetGroupItem.GetGroupItemList: TStringList;

  function JustDomain(Value: string): string;
  begin
    Result := StrStCopyL(Value, 1, Pos('\', Value)-1);
  end;

  function JustName(Value: string): string;
  begin
    Result := StrStCopyL(Value, Pos('\', Value)+1, Length(Value));
  end;

var
  NewList      : TStringList;
  ErrorD       : DWord;
  Index        : Integer;
  Buffer       : Pointer;
  DBuffer      : string;
  EntriesRead  : DWord;
  TotalEntries : DWord;
  S            : TStNetServerItem;
  NewItem      : TStNetItem;
  CurrentName  : string;
  CurrentDandN : string;
  CurrentDomain: string;
begin
  NewList := TStringList.Create;
  try
    { get the list of items that this group has }
    case ItemType of
      nitGlobalGroup :
        begin
          ErrorD := StNetGroupGetUsers(FServer, FName, 0, Buffer, DWord(-1),
                                       EntriesRead, TotalEntries, nil);
          if ErrorD = NERR_SUCCESS then begin
            try
              if EntriesRead > 0 then begin                            {!!.02}
                for Index := 0 to EntriesRead-1 do begin
                  CurrentName := TGroupUsersInfo0Array(Buffer^)[Index].grui0_name;

                  { does this group already exist in the servers list of groups }
                  NewItem := nil;
                  if (FNetwork <> nil) then begin
                    S := FNetwork.Server[FServer];
                    NewItem := S.User[CurrentName];
                  end;

                  if (NewItem = nil) then begin
                    NewItem := TStNetUserItem.CreateEx(CurrentName, '', FServer,
                                                       nitUnknown);
                    NewItem.FNetwork := FNetwork;
                  end;
                  NewList.AddObject(CurrentName, NewItem);
                end;
              end;                                                     {!!.02}
            finally
              StNetApiBufferFree(Buffer);
              Buffer := nil;
            end;
          end else begin
            RaiseStWin32Error(EStNetException, ErrorD);
          end;
        end;
      nitLocalGroup :
        begin
          ErrorD := StNetLocalGroupGetMembers(FServer, FName, 2, Buffer,
                                              DWord(-1), EntriesRead,
                                              TotalEntries, nil);
          if ErrorD = NERR_SUCCESS then begin
            try
              if EntriesRead > 0 then begin                            {!!.02}
                for Index := 0 to EntriesRead-1 do begin
                  CurrentDandN := TLocalGroupMembersInfo2Array(Buffer^)[Index].lgrmi2_domainandname;
                  CurrentDomain := JustDomain(CurrentDandN);
                  CurrentName  := JustName(CurrentDandN);

                  if (UpperCase(CurrentDomain) <> UpperCase(FServer)) and
                     (UpperCase(CurrentDomain) <> 'NT AUTHORITY') then begin

                    ErrorD := StNetGetDCName('', CurrentDomain, DBuffer);
                    if ErrorD = NERR_SUCCESS then
                      CurrentDomain := FilterL(DBuffer, '\')
                    else
                      RaiseStWin32Error(EStNetException, ErrorD);
                  end else begin
                    CurrentDomain := FServer;
                  end;

                  NewItem := nil;
                  case TLocalGroupMembersInfo2Array(Buffer^)[Index].lgrmi2_sidusage of
                    SidTypeUser :
                      begin
                        if (FNetwork <> nil) then begin
                          S := FNetwork.Server[CurrentDomain];
                          NewItem := S.User[CurrentName];
                        end;

                        if (NewItem = nil) then begin
                          NewItem := TStNetUserItem.CreateEx(CurrentName, '',
                                                             CurrentDomain,
                                                             nitUnknown);
                          NewItem.FNetwork := FNetwork;
                        end;
                        NewList.AddObject(CurrentName, NewItem);
                      end;
                    SidTypeGroup :
                      begin
                        if (FNetwork <> nil) then begin
                          S := FNetwork.Server[CurrentDomain];
                          NewItem := S.Group[CurrentName]
                        end;

                        if (NewItem = nil) then begin
                          NewItem := TStNetGroupItem.CreateEx(CurrentName, '',
                                                              CurrentDomain,
                                                              nitUnknown);
                          NewItem.FNetwork := FNetwork;
                        end;
                        NewList.AddObject(CurrentName, NewItem);
                      end;
                    SidTypeWellKnownGroup :
                      begin
                        { filter out these groups }
                      end;
                  end;
                end;
              end;                                                     {!!.02}
            finally
              StNetApiBufferFree(Buffer);
              Buffer := nil;
            end;
          end else begin
            RaiseStWin32Error(EStNetException, ErrorD);
          end;
        end;
    end;
    FItemList.Assign(NewList);
  finally
    NewList.Free;
  end;
  Result := FItemList as TStringList;
end;

function TStNetGroupItem.GetDomain: string;
begin
  if (FSidRecord.Value = nil) then begin
    FSidRecord := GetSid;
  end;
  Result := FDomain;
end;

function TStNetGroupItem.GetSid: TStSidRecord;
var
  NoError: Boolean;
  _SidUse: DWord;
  Index  : Integer;
begin
  if (FSidRecord.Value = nil) then begin
    NoError := StLookupAccountName(FServer, FName, FSidRecord.Value,
                                   FSidRecord.Length, FDomain, _SidUse);
    if NoError then begin
      { and the SID usage flag to the enumeration }
      FSidRecord.Usage := TStNetSidType(_SidUse);

      { convert the sid to a readable string }
      FSidRecord.ValueS :=
        Format('S-1-%d',
            [TSIDIdentifierAuthority(
               StGetSidIdentifierAuthority(FSidRecord.Value)^).Value[5]]);

          for Index := 0 to StGetSidSubAuthorityCount(FSidRecord.Value) - 1 do begin
            FSidRecord.ValueS :=
              Format(FSidRecord.ValueS + '-%d', [StGetSidSubAuthority(
                                                   FSidRecord.Value, Index)]);
          end;
    end else begin
      RaiseStWin32Error(EStNetException, GetLastError);
    end;
  end;
  Result := FSidRecord;
end;

procedure TStNetGroupItem.AddToGroup(AItem: TStNetItem);
var
  ErrorD : DWord;
  S : PSid;
begin
  case ItemType of
    nitGlobalGroup :
      begin
        if (AItem.ItemType = nitLocalUser) or (AItem.ItemType = nitGlobalUser) then begin
          ErrorD := StNetGroupAddUser(FServer, FName, AItem.Name);
          if ErrorD = NERR_SUCCESS then begin
            { add this user to this groups internal list }
            FItemList.AddObject(AItem.Name, AItem);
          end else begin
            RaiseStWin32Error(EStNetException, ErrorD);
          end;
        end else begin
          { can't add this item type to the group }
          RaiseStWin32Error(EStNetException, stscNetInvalidItemType);
        end;
      end;
    nitLocalGroup  :
      begin
        if (AItem.ItemType = nitLocalUser) or (AItem.ItemType = nitGlobalUser) or
           (AItem.ItemType = nitGlobalGroup) then begin

          { get the SID for this user }
          S := nil;
          case AItem.ItemType of
            nitLocalUser, nitGlobalUser : S := TStNetUserItem(AItem).Sid.Value;
            nitGlobalGroup              : S := TStNetGroupItem(AItem).Sid.Value;
          end;

          ErrorD := StNetLocalGroupAddMembers(FServer, FName, 0, S, 1);
          if ErrorD = NERR_SUCCESS then begin
            { add this user to this groups internal list }
            FItemList.AddObject(AItem.Name, AItem);
          end else begin
            RaiseStWin32Error(EStNetException, ErrorD);
          end;
        end else begin
          { can't add this item type to the group }
          RaiseStWin32Error(EStNetException, stscNetInvalidItemType);
        end;
      end;
  end;
end;

procedure TStNetGroupItem.RemoveFromGroup(AItem: TStNetItem);
var
  ErrorD : DWord;
  S : PSid;
begin
  case ItemType of
    nitGlobalGroup :
      begin
        if (AItem.ItemType = nitLocalUser) or (AItem.ItemType = nitGlobalUser) then begin
          ErrorD := StNetGroupDelUser(FServer, FName, AItem.Name);
          if ErrorD = NERR_SUCCESS then begin
            { remove this user from the groups internal list }
            FItemList.Delete(FItemList.IndexOf(AItem.Name));
          end else begin
            RaiseStWin32Error(EStNetException, ErrorD);
          end;
        end else begin
          { this item can't be in a group .. unable to remove it }
          RaiseStWin32Error(EStNetException, stscNetInvalidItemType);
        end;
      end;
    nitLocalGroup  :
      begin
        if (AItem.ItemType = nitLocalUser) or (AItem.ItemType = nitGlobalUser) or
           (AItem.ItemType = nitGlobalGroup) then begin

          { get the SID for this user }
          S := nil;
          case AItem.ItemType of
            nitLocalUser, nitGlobalUser : S := TStNetUserItem(AItem).Sid.Value;
            nitGlobalGroup              : S := TStNetGroupItem(AItem).Sid.Value;
          end;

          ErrorD := StNetLocalGroupDelMembers(FServer, FName, 0, S, 1);
          if ErrorD = NERR_SUCCESS then begin
            { remove this user from the groups internal list }
            FItemList.Delete(FItemList.IndexOf(AItem.Name));
          end else begin
            RaiseStWin32Error(EStNetException, ErrorD);
          end;
        end else begin
          { this item can't be in a group .. unable to remove it }
          RaiseStWin32Error(EStNetException, stscNetInvalidItemType);
        end;
      end;
  end;
end;

procedure TStNetGroupItem.Delete;
var
  ErrorD : DWord;
begin
  case ItemType of
    nitGlobalGroup :
      begin
        ErrorD := StNetGroupDel(FServer, FName);
        if ErrorD = NERR_SUCCESS then begin
         { can't possible remove it from all lists, but at least remove from server list }
         FNetwork.Server[FServer].FGroupList.Delete(FNetwork.Server[FServer].FGroupList.IndexOf(FName));

         { free this item }
         Free;
        end else begin
          RaiseStWin32Error(EStNetException, ErrorD);
        end;
      end;
    nitLocalGroup :
      begin
        ErrorD := StNetLocalGroupDel(FServer, FName);
        if ErrorD = NERR_SUCCESS then begin
         { can't possible remove it from all lists, but at least remove from server list }
         FNetwork.Server[FServer].FGroupList.Delete(FNetwork.Server[FServer].FGroupList.IndexOf(FName));

         { free this item }
         Free;
        end else begin
          RaiseStWin32Error(EStNetException, ErrorD);
        end;
      end;
  end;
end;

procedure TStNetGroupItem.Refresh;
var
  ErrorD : DWord;
  Buffer : Pointer;
begin
  case ItemType of
    nitGlobalGroup :
      begin
        ErrorD := StNetGroupGetInfo(FServer, FName, 2, Buffer);
        if ErrorD = NERR_SUCCESS then begin
          try
            FID       := TGROUP_INFO_2(Buffer^).grpi2_group_id;
            FComment  := TGROUP_INFO_2(Buffer^).grpi2_comment;
          finally
            StNetApiBufferFree(Buffer);
            Buffer := nil;
          end;
        end else begin
          RaiseStWin32Error(EStNetException, ErrorD);
        end;;
      end;
    nitLocalGroup :
      begin
        ErrorD := StNetLocalGroupGetInfo(FServer, FName, 1, Buffer);
        if ErrorD = NERR_SUCCESS then begin
          try
            FID       := TGROUP_INFO_2(Buffer^).grpi2_group_id;
            FComment  := TGROUP_INFO_2(Buffer^).grpi2_comment;
          finally
            StNetApiBufferFree(Buffer);
            Buffer := nil;
          end;
        end else begin
          RaiseStWin32Error(EStNetException, ErrorD);
        end;;
      end;
  end;
end;

function TStNetGroupItem.GetName: string;
begin
  Result := FName;
end;

procedure TStNetGroupItem.SetName(Value: string);
var
  ErrorD : DWord;
  Data : TLMWideChar;
  ParmErr : DWord;
  S : TStNetServerItem;
begin
  if Value <> FName then begin
    case ItemType of
      nitGlobalGroup :
        begin
          Data.Value := nil;
          try
            CvtToWideCharLM(Value, Data);
            ErrorD := StNetGroupSetInfo(FServer, FName, 0, @Data.Value, ParmErr);
            if ErrorD = NERR_SUCCESS then begin
              S := FNetwork.Server[FServer];
              S.FGroupList[S.FGroupList.IndexOf(FName)] := Value;;

              FName := Value;
            end else begin
              RaiseStWin32Error(EStNetException, ErrorD);
            end;
          finally
            StNetApiBufferFree(Data.Value);
          end;
        end;
      nitLocalGroup  :
        begin
          Data.Value := nil;
          try
            CvtToWideCharLM(Value, Data);
            ErrorD := StNetLocalGroupSetInfo(FServer, FName, 0, @Data.Value, ParmErr);
            if ErrorD = NERR_SUCCESS then begin
              S := FNetwork.Server[FServer];
              S.FGroupList[S.FGroupList.IndexOf(FName)] := Value;;

              FName := Value;
            end else begin
              RaiseStWin32Error(EStNetException, ErrorD);
            end;
          finally
            StNetApiBufferFree(Data.Value);
          end;
        end;
    end;
  end;
end;

procedure TStNetGroupItem.SetComment(Value: string);
var
  ErrorD : DWord;
  Data : TLMWideChar;
  ParmErr : DWord;
begin
  if Value <> FComment then begin
    case ItemType of
      nitGlobalGroup :
        begin
          Data.Value := nil;
          try
            CvtToWideCharLM(Value, Data);
            ErrorD := StNetGroupSetInfo(FServer, FName, 1002, @Data.Value, ParmErr);
            if ErrorD = NERR_SUCCESS then begin
              FComment := Value;
            end else begin
              RaiseStWin32Error(EStNetException, ErrorD);
            end;
          finally
            StNetApiBufferFree(Data.Value);
          end;
        end;
      nitLocalGroup  :
        begin
          Data.Value := nil;
          try
            CvtToWideCharLM(Value, Data);
            ErrorD := StNetLocalGroupSetInfo(FServer, FName, 1002, @Data.Value, ParmErr);
            if ErrorD = NERR_SUCCESS then begin
              FComment := Value;
            end else begin
              RaiseStWin32Error(EStNetException, ErrorD);
            end;
          finally
            StNetApiBufferFree(Data.Value);
          end;
        end;
    end;
  end;
end;

{ --- TStNetServerItem ------------------------------------------------------ }
constructor TStNetShareItem.CreateEx(AName, AComment, AServer: string;
                                     AShareType: TStNetShareType);
begin
  inherited CreateEx(AName, AComment, AServer, nitShare);
  FShareType := AShareType;
end;


{ --- TStNetServerItem ------------------------------------------------------ }
constructor TStNetServerItem.CreateEx(AName, AComment: string);
var
  ErrorD : DWord;
begin
  inherited CreateEx(AName, AComment, AName, nitComputer);
  FUserList := TStringList.Create;
  FGroupList:= TStringList.Create;
  FDriveList:= TStringLIst.Create;
  FShareList:= TStringLIst.Create;

  FServerData   := nil;
  FServerMData0 := nil;
  FServerMData1 := nil;
  FServerMData2 := nil;
  FServerMData3 := nil;

  ErrorD := StNetServerGetInfo(AName, 101, FServerData);
  if ErrorD = NERR_SUCCESS then begin
    with TSERVER_INFO_101(FServerData^) do begin
      FComment := sv101_comment;
    end;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

destructor TStNetServerItem.Destroy;
var
  I : Integer;
begin
  for I := 0 to FUserList.Count-1 do
    FUserList.Objects[I].Free;
  FUserList.Free;

  for I := 0 to FGroupList.Count-1 do
    FGroupList.Objects[I].Free;
  FGroupList.Free;

  FDriveList.Free;

  for I := 0 to FShareList.Count-1 do
    FShareList.Objects[I].Free;
  FShareList.Free;

  if FServerData <> nil then
    StNetApiBufferFree(FServerData);

  if FServerMData0 <> nil then
    StNetApiBufferFree(FServerMData0);

  if FServerMData1 <> nil then
    StNetApiBufferFree(FServerMData1);

  if FServerMData2 <> nil then
    StNetApiBufferFree(FServerMData2);

  if FServerMData3 <> nil then
    StNetApiBufferFree(FServerMData3);

  inherited Destroy;
end;

procedure TStNetServerItem.Refresh;
var
  ErrorD : DWord;
  Data : Pointer;
begin
  { refresh the base data }
  ErrorD := StNetServerGetInfo(FServer, 102, Data);
  if ErrorD = NERR_SUCCESS then begin
    if FServerData <> nil then
      StNetApiBufferFree(FServerData);
    FServerData := Data;

    with TSERVER_INFO_102(FServerData^) do begin
      FComment := sv102_comment;
    end;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;

  { refresh the modal data }
  RefreshM0;
  RefreshM1;
  RefreshM2;
  RefreshM3;
end;

procedure TStNetServerItem.RefreshM0;
var
  ErrorD : DWord;
  Data : Pointer;
begin
  { refresh the base data }
  ErrorD := StNetUserModalsGet(FServer, 0, Data);
  if ErrorD = NERR_SUCCESS then begin
    if FServerMData0 <> nil then
      StNetApiBufferFree(FServerMData0);
    FServerMData0 := Data;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetServerItem.RefreshM1;
var
  ErrorD : DWord;
  Data : Pointer;
begin
  { refresh the base data }
  ErrorD := StNetUserModalsGet(FServer, 1, Data);
  if ErrorD = NERR_SUCCESS then begin
    if FServerMData1 <> nil then
      StNetApiBufferFree(FServerMData1);
    FServerMData1 := Data;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetServerItem.RefreshM2;
var
  ErrorD : DWord;
  Data : Pointer;
begin
  { refresh the base data }
  ErrorD := StNetUserModalsGet(FServer, 2, Data);
  if ErrorD = NERR_SUCCESS then begin
    if FServerMData2 <> nil then
      StNetApiBufferFree(FServerMData2);
    FServerMData2 := Data;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

procedure TStNetServerItem.RefreshM3;
var
  ErrorD : DWord;
  Data : Pointer;
begin
  { refresh the base data }
  ErrorD := StNetUserModalsGet(FServer, 3, Data);
  if ErrorD = NERR_SUCCESS then begin
    if FServerMData3 <> nil then
      StNetApiBufferFree(FServerMData3);
    FServerMData3 := Data;
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;


function TStNetServerItem.GetUser(AName: string): TStNetUserItem;
var
  Index : Integer;
  ErrorD : DWord;
  Buffer : Pointer;
begin
  Result := nil;

  { look in the internal list first }
  Index := FUserList.IndexOf(AName);

  if Index >= 0 then begin
    Result := TStNetUserItem(FUserList.Objects[Index]);
  end else begin
    { Need to find the user on the server }
    ErrorD := StNetUserGetInfo(FServer, AName, 1, Buffer);

    { if the user existed on the server, then add that user to the list }
    { and return a reference to that object; otherwise return nil       }
    if ErrorD = NERR_SUCCESS then begin
      Result := TStNetUserItem.CreateEx(AName, '', FServer, nitUnknown);
      Result.FNetwork := FNetwork;

      StNetApiBufferFree(Buffer);

      { Add this item to the internal list }
      FUserList.AddObject(AName, Result);
    end;
  end;
end;

function TStNetServerItem.GetGroup(AName: string): TStNetGroupItem;
var
  Index : Integer;
  ErrorD : DWord;
  Buffer : Pointer;
begin
  Result := nil;

  { look in the internal list first }
  Index := FGroupList.IndexOf(AName);

  if Index >= 0 then begin
    Result := TStNetGroupItem(FGroupList.Objects[Index]);
  end else begin

    { Need to find the group on the server }
    ErrorD := StNetGroupGetInfo(FServer, AName, 0, Buffer);

    { if the group existed on the server, then add that group to the list }
    { and return a reference to that object; otherwise return nil       }
    if ErrorD = NERR_SUCCESS then begin
      Result := TStNetGroupItem.CreateEx(AName, '', FServer, nitGlobalGroup);
      Result.FNetwork := FNetwork;

      StNetApiBufferFree(Buffer);

      { Add this item to the internal list }
      FGroupList.AddObject(AName, Result);
    end else if ErrorD = NERR_GroupNotFound then begin
      { cound be a local group }
      ErrorD := StNetLocalGroupGetInfo(FServer, AName, 1, Buffer);

      if ErrorD = NERR_SUCCESS then begin
        Result := TStNetGroupItem.CreateEx(AName,
          TLocalGroup_Info_1(Buffer^).lgrpi1_comment, FServer, nitLocalGroup);
        Result.FNetwork := FNetwork;

        StNetApiBufferFree(Buffer);

        { Add this item to the internal list }
        FGroupList.AddObject(AName, Result);
      end else begin
        {RaiseStWin32Error(EStNetException, ErrorD);}
      end;
    end else begin
      {RaiseStWin32Error(EStNetException, ErrorD);}
    end;
  end;
end;

function TStNetServerItem.GetDriveList: TStringList;
type
  TDriveArray = array[0..MaxByte] of array[0..2] of WideChar;
var
  NewList      : TStringList;
  ErrorD       : DWord;
  Index        : Integer;
  Buffer       : Pointer;
  EntriesRead  : DWord;
  TotalEntries : DWord;
  MoreData     : Boolean;
  NextIndex    : DWord;
begin
  NewList := TStringList.Create;
  MoreData := True;
  NextIndex := 0;
  try
    { get the list of drives from the server }
    while MoreData do begin
      ErrorD := StNetServerDiskEnum(FServer, 0, Buffer, DWord(-1), EntriesRead,
                                    TotalEntries, NextIndex);
      if ((ErrorD = NERR_SUCCESS) or (ErrorD = ERROR_MORE_DATA)) then begin
        try
          if EntriesRead > 0 then                                      {!!.02}
          for Index := 0 to EntriesRead-1 do
            NewList.Add((TDriveArray(Buffer^)[Index]));

          MoreData := (ErrorD = ERROR_MORE_DATA);
        finally
          StNetApiBufferFree(Buffer);
        end;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    end;
    FDriveList.Assign(NewList);
  finally
    NewList.Free;
  end;
  Result := FDriveList as TStringList;
end;

function TStNetServerItem.GetUserList: TStringList;
var
  NewList      : TStringList;
  ErrorD       : DWord;
  Index        : Integer;
  CurrentIndex : Integer;
  Buffer       : Pointer;
  EntriesRead  : DWord;
  NewItem      : TStNetUserItem;
  MoreData     : Boolean;
  NextIndex    : DWord;
begin
  NewList  := TStringList.Create;
  MoreData := True;
  NextIndex := 0;
  try
    while MoreData do begin
      ErrorD := StNetQueryDisplayInformation(FServer, 1, NextIndex, DWord(-1),
                                             DWord(-1), EntriesRead, Buffer);
      if ((ErrorD = NERR_SUCCESS) or (ErrorD = ERROR_MORE_DATA)) then begin
        try
          if EntriesRead > 0 then begin                                {!!.02}
            for Index := 0 to EntriesRead-1 do begin
              { see if this user alread exists in the local list }
              CurrentIndex :=
                FUserList.IndexOf(TNetDisplayUserArray(Buffer^)[Index].usri1_name);
              if CurrentIndex >= 0 then
                NewItem := TStNetUserItem(FUserList.Objects[CurrentIndex])
              else begin
                NewItem :=
                  TStNetUserItem.CreateExEnum(
                    TNetDisplayUserArray(Buffer^)[Index].usri1_name,
                    TNetDisplayUserArray(Buffer^)[Index].usri1_comment,
                    FName,
                    TNetDisplayUserArray(Buffer^)[Index].usri1_full_name,
                    LongFlagIsSet(TNetDisplayUserArray(Buffer^)[Index].usri1_flags,
                                  UF_NORMAL_ACCOUNT),
                    TNetDisplayUserArray(Buffer^)[Index].usri1_user_id);
                NewItem.FNetwork := FNetwork;
              end;
              NewList.AddObject(NewItem.Name, NewItem);
              NextIndex := TNetDisplayUserArray(Buffer^)[Index].usri1_next_index;
            end;
          end;                                                         {!!.02}
          MoreData := (ErrorD = ERROR_MORE_DATA);
        finally
          StNetApiBufferFree(Buffer);
        end;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    end;
    FUserList.Assign(NewList);
  finally
    NewList.Free;
  end;
  Result := FUserList as TStringList;
end;

function TStNetServerItem.GetGroupList : TStringList;
var
  NewList      : TStringList;
  ErrorD       : DWord;
  Index        : Integer;
  CurrentIndex : Integer;
  Buffer       : Pointer;
  EntriesRead  : DWord;
  TotalEntries : DWord;
  NewItem      : TStNetGroupItem;
  MoreData     : Boolean;
  NextIndex    : DWord;
begin
  NewList := TStringList.Create;
  MoreData := True;
  NextIndex := 0;
  try
    { Get the global list of groups from the server }
    while MoreData do begin
      ErrorD := StNetQueryDisplayInformation(FServer, 3, NextIndex, DWord(-1),
                                             DWord(-1), EntriesRead, Buffer);
      if ((ErrorD = NERR_SUCCESS) or (ErrorD = ERROR_MORE_DATA)) then begin
        try
          if EntriesRead > 0 then begin                                {!!.02}
            for Index := 0 to EntriesRead-1 do begin
              { see if this group already exists in the local list }
              CurrentIndex :=
                FGroupList.IndexOf(TNetDisplayGroupArray(Buffer^)[Index].grpi3_name);
              if CurrentIndex >= 0 then begin
                NewItem := TStNetGroupItem(FGroupList.Objects[CurrentIndex]);
                NewItem.FComment := TNetDisplayGroupArray(Buffer^)[Index].grpi3_comment;
                NewItem.FID := TNetDisplayGroupArray(Buffer^)[Index].grpi3_group_id;
                NewItem.FItemType := nitGlobalGroup;
              end else begin
                NewItem :=
                  TStNetGroupItem.CreateExEnum(
                    TNetDisplayGroupArray(Buffer^)[Index].grpi3_name,
                    TNetDisplayGroupArray(Buffer^)[Index].grpi3_comment,
                    FName,
                    TNetDisplayGroupArray(Buffer^)[Index].grpi3_group_id);
                NewItem.FNetwork := FNetwork;
              end;
              NewList.AddObject(NewItem.Name, NewItem);
              NextIndex := TNetDisplayGroupArray(Buffer^)[Index].grpi3_next_index;
            end;
          end;                                                         {!!.02}
          MoreData := (ErrorD = ERROR_MORE_DATA);
        finally
          StNetApiBufferFree(Buffer);
          Buffer := nil;
        end;
      end else begin
       RaiseStWin32Error(EStNetException, ErrorD);
      end;
    end;

    MoreData := True;
    NextIndex := 0;
    while MoreData do begin
      { get the local list of groups from the server }
      ErrorD := StNetLocalGroupEnum(FServer, 1, Buffer, DWord(-1), EntriesRead,
                                    TotalEntries, NextIndex);
      if ((ErrorD = NERR_SUCCESS) or (ErrorD = ERROR_MORE_DATA)) then begin
        try
          if EntriesRead > 0 then begin                                {!!.02}
            for Index := 0 to EntriesRead-1 do begin
              { see if this user alread exists in the local list }
              CurrentIndex :=
                FGroupList.IndexOf(TLocalGroupInfo1Array(Buffer^)[Index].lgrpi1_name);
              if CurrentIndex >= 0 then begin
                NewItem := TStNetGroupItem(FGroupList.Objects[CurrentIndex]);
                NewItem.FComment := TLocalGroupInfo1Array(Buffer^)[Index].lgrpi1_comment;
                NewItem.FID := 0;
                NewItem.FItemType := nitLocalGroup;
              end else begin
                NewItem :=
                  TStNetGroupItem.CreateExEnum(
                    TLocalGroupInfo1Array(Buffer^)[Index].lgrpi1_name,
                    TLocalGroupInfo1Array(Buffer^)[Index].lgrpi1_comment,
                    FName,
                    0);
                NewItem.FNetwork := FNetwork;
              end;
              NewList.AddObject(NewItem.Name, NewItem);
            end;
          end;                                                         {!!.02}
          MoreData := (ErrorD = ERROR_MORE_DATA);
        finally
          StNetApiBufferFree(Buffer);
        end;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    end;

    FGroupList.Assign(NewList);
  finally
    NewList.Free;
  end;
  Result := FGroupList as TStringList;
end;

function TStNetServerItem.GetShareList : TStringList;
var
  NewList      : TStringList;
  ErrorD       : DWord;
  Index        : Integer;
  CurrentIndex : Integer;
  Buffer       : Pointer;
  EntriesRead  : DWord;
  TotalEntries : DWord;
  NewItem      : TStNetShareItem;
  NewShareType : TStNetShareType;
  MoreData     : Boolean;
  NextIndex    : DWord;
begin
  NewList := TStringList.Create;
  MoreData := True;
  NextIndex := 0;
  try
    while MoreData do begin
      { Get the list of shares on the server }
      ErrorD := StNetShareEnum(FServer, 1, Buffer, DWord(-1), EntriesRead,
                               TotalEntries, NextIndex);
      if ((ErrorD = NERR_SUCCESS) or (ErrorD = ERROR_MORE_DATA)) then begin
        try
          if EntriesRead > 0 then begin                                {!!.02}
            for Index := 0 to EntriesRead-1 do begin
              { see if this group already exists in the local list }
              CurrentIndex :=
                FShareList.IndexOf(TShareInfo1Array(Buffer^)[Index].shi1_netname);
             if CurrentIndex >= 0 then begin
                NewItem := TStNetShareItem(FShareList.Objects[CurrentIndex]);
                NewItem.FComment := TShareInfo1Array(Buffer^)[Index].shi1_remark;
                NewItem.FItemType := nitShare;

                case TShareInfo1Array(Buffer^)[Index].shi1_type of
                  STYPE_DISKTREE : NewItem.FShareType := stDisk;
                  STYPE_PRINTQ   : NewItem.FShareType := stPrint;
                  STYPE_DEVICE   : NewItem.FShareType := stDevice;
                  STYPE_IPC      : NewItem.FShareType := stIPC;
                  STYPE_SPECIAL  : NewItem.FShareType := stSpecial;
                else
                   NewItem.FShareType := stUnknown;
                end;
              end else begin
                case TShareInfo1Array(Buffer^)[Index].shi1_type of
                  STYPE_DISKTREE : NewShareType := stDisk;
                  STYPE_PRINTQ   : NewShareType := stPrint;
                  STYPE_DEVICE   : NewShareType := stDevice;
                  STYPE_IPC      : NewShareType := stIPC;
                  STYPE_SPECIAL  : NewShareType := stSpecial;
                else
                   NewShareType := stUnknown;
                end;

                NewItem :=
                  TStNetShareItem.CreateEx(
                    TShareInfo1Array(Buffer^)[Index].shi1_netname,
                    TShareInfo1Array(Buffer^)[Index].shi1_remark,
                    FServer,
                    NewShareType);
                NewItem.FNetwork := FNetwork;
              end;
              NewList.AddObject(NewItem.Name, NewItem);
            end;
          end;                                                         {!!.02}
          MoreData := (ErrorD = ERROR_MORE_DATA);
        finally
          StNetApiBufferFree(Buffer);
          Buffer := nil;
        end;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    end;
    FShareList.Assign(NewList);
  finally
    NewList.Free;
  end;
  Result := FShareList as TStringList;
end;

function TStNetServerItem.AddGroup(AName, ADescription: string;
                                   AGlobal: Boolean): TStNetGroupItem;
  { Adds a group to the server }
var
  Data : TLMWideChar;
  ErrorD : DWord;
  ParmErr : DWord;
begin
  { does the group already exist ? }
  Result := Group[AName];

  { if not, try creating it }
  if Result = nil then begin
    case AGlobal of
      True :
        begin
          Data.Value := nil;
          try
            CvtToWideCharLM(AName, Data);
            ErrorD := StNetGroupAdd(FServer, 0, @Data.Value, ParmErr);
            if ErrorD = NERR_SUCCESS then begin
              Result := TStNetGroupItem.CreateEx(AName, '', FServer, nitGlobalGroup);
              Result.FNetwork := FNetwork;
              Result.Comment := ADescription;
            end else begin
              RaiseStWin32Error(EStNetException, ErrorD);
            end;
          finally
            StNetApiBufferFree(Data.Value);
          end;
        end;
      False :
        begin
          Data.Value := nil;
          try
            CvtToWideCharLM(AName, Data);
            ErrorD := StNetLocalGroupAdd(FServer, 0, @Data.Value, ParmErr);
            if ErrorD = NERR_SUCCESS then begin
              Result := TStNetGroupItem.CreateEx(AName, '', FServer, nitLocalGroup);
              Result.FNetwork := FNetwork;
              Result.Comment := ADescription;
            end else begin
              RaiseStWin32Error(EStNetException, ErrorD);
            end;
          finally
            StNetApiBufferFree(Data.Value);
          end;
        end;
    end;
  end;
end;

function TStNetServerItem.AddUser(AName, APassword: string;
                                  AGlobal: Boolean): TStNetUserItem;
  { Adds a user to the server }
var
  Data : TUSER_INFO_1;
  ErrorD : DWord;
  ParmErr : DWord;
  ANameLen : DWord;
  APasswordLen : DWord;
const
  IT : array[Boolean] of TStNetItemType = (nitGlobalUser, nitLocalUser);
begin
  { does the user already exist ? }
  Result := User[AName];

  { if not, try creating it }
  if Result = nil then begin
    FillChar(Data, SizeOf(Data), 0);

    ANameLen := (Length(AName)+1) * 2;
    StNetApiBufferAllocate(ANameLen, Pointer(Data.usri1_name));

    APasswordLen := (Length(APassword)+1) * 2;
    StNetApiBufferAllocate(APasswordLen, Pointer(Data.usri1_password));
    try
      StringToWideChar(AName, Data.usri1_name, ANameLen);
      StringToWideChar(APassword, Data.usri1_password, APasswordLen);

      Data.usri1_priv := USER_PRIV_USER;
      Data.usri1_flags := UF_SCRIPT;
      if AGlobal then
        SetLongFlag(Integer(Data.usri1_flags),
          UF_NORMAL_ACCOUNT)
      else
        SetLongFlag(Integer(Data.usri1_flags),
          UF_TEMP_DUPLICATE_ACCOUNT);

      ErrorD := StNetUserAdd(FServer, 1, @Data, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        Result := TStNetUserItem.CreateEx(AName, '', FServer, IT[AGlobal]);
        Result.FNetwork := FNetwork;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    finally
      StNetApiBufferFree(Data.usri1_password);
      StNetApiBufferFree(Data.usri1_name);
    end;
  end;
end;

procedure TStNetServerItem.SetComment(Value: string);
var
  Data    : TLMWideChar;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> Comment then begin
    Data.Value := nil;
    try
      CvtToWideCharLM(Value, Data);
      ErrorD := StNetServerSetInfo(FServer, 1005, @Data.Value, ParmErr);
      if ErrorD = NERR_SUCCESS then begin
        StNetApiBufferFree(TSERVER_INFO_101(FServerData^).sv101_comment);
        TSERVER_INFO_101(FServerData^).sv101_comment := Data.Value;
        FComment := Data.Value;
      end else begin
        RaiseStWin32Error(EStNetException, ErrorD);
      end;
    except
      StNetApiBufferFree(Data.Value);
      raise;
    end;
  end;
end;

function TStNetServerItem.GetAnnounceRate: DWord;
begin
  if FServerData = nil then
    Refresh;
  Result := TSERVER_INFO_102(FServerData^).sv102_announce;
end;

function TStNetServerItem.GetAnnounceRateDelta: DWord;
begin
  if FServerData = nil then
    Refresh;
  Result := TSERVER_INFO_102(FServerData^).sv102_anndelta;
end;

function TStNetServerItem.GetDisconnectTime: DWord;
begin
  if FServerData = nil then
    Refresh;
  Result := TSERVER_INFO_102(FServerData^).sv102_disc;
end;

function TStNetServerItem.GetMaxUsers: DWord;
begin
  if FServerData = nil then
    Refresh;
  Result := TSERVER_INFO_102(FServerData^).sv102_users;
end;

function TStNetServerItem.GetPlatform: TStNetServerPlatformType;
begin
  if FServerData = nil then
    Refresh;

  Result := sptUnknown;
  case TSERVER_INFO_102(FServerData^).sv102_platform_id of
    PLATFORM_ID_DOS : Result := sptDOS;
    PLATFORM_ID_OS2 : Result := sptOS2;
    PLATFORM_ID_NT  : Result := sptNT;
    PLATFORM_ID_OSF : Result := sptOSF;
    PLATFORM_ID_VMS : Result := sptVMS;
  end;
end;

function TStNetServerItem.GetServerType: TStNetServerSet;
begin
  if FServerData = nil then
    Refresh;

  Result := [];
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_WORKSTATION) then
    Include(Result, nsvtWorkstation);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SERVER) then
    Include(Result, nsvtServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SQLSERVER) then
    Include(Result, nsvtSQLServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DOMAIN_CTRL) then
    Include(Result, nsvtDomainCtrl);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DOMAIN_BAKCTRL) then
    Include(Result, nsvtDomainBackupCtrl);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_TIME_SOURCE) then
    Include(Result, nsvtTimeSource);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_AFP) then
    Include(Result, nsvtAFP);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_NOVELL) then
    Include(Result, nsvtNovell);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DOMAIN_MEMBER) then
    Include(Result, nsvtDomainMember);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_PRINTQ_SERVER) then
    Include(Result, nsvtPrintQServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DIALIN_SERVER) then
    Include(Result, nsvtDialinServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_XENIX_SERVER) then
    Include(Result, nsvtUNIXServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SERVER_UNIX) then
    Include(Result, nsvtUNIXServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_NT) then
    Include(Result, nsvtNT);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_WFW) then
    Include(Result, nsvtWFW);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SERVER_MFPN) then
    Include(Result, nsvtMFPN);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SERVER_NT) then
    Include(Result, nsvtServerNT);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_POTENTIAL_BROWSER) then
    Include(Result, nsvtPotentialBrowser);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_BACKUP_BROWSER) then
    Include(Result, nsvtBackupBrowser);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_MASTER_BROWSER) then
    Include(Result, nsvtMasterBrowser);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DOMAIN_MASTER) then
    Include(Result, nsvtDomainMaster);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SERVER_OSF) then
    Include(Result, nsvtOSF);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_SERVER_VMS) then
    Include(Result, nsvtVMS);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_WINDOWS) then
    Include(Result, nsvtWindows);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DFS) then
    Include(Result, nsvtDFS);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_CLUSTER_NT) then
    Include(Result, nsvtClusterNT);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_TERMINALSERVER) then
    Include(Result, nsvtTerminalServer);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_DCE) then
    Include(Result, nsvtDCE);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_ALTERNATE_XPORT) then
    Include(Result, nsvtAlternateXPORT);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, SV_TYPE_LOCAL_LIST_ONLY) then
    Include(Result, nsvtLocalListOnly);
  if LongFlagIsSet(TSERVER_INFO_102(FServerData^).sv102_type, Integer(SV_TYPE_DOMAIN_ENUM)) then
    Include(Result, nsvtDomainEnum);
end;

function TStNetServerItem.GetUserPath: string;
begin
  if FServerData = nil then
    Refresh;
  Result := TSERVER_INFO_102(FServerData^).sv102_userpath;
end;

function TStNetServerItem.GetVersion: DWord;
begin
  if FServerData = nil then
    Refresh;
  Result := MakeLong(TSERVER_INFO_102(FServerData^).sv102_version_major,
                     TSERVER_INFO_102(FServerData^).sv102_version_minor);
end;

function TStNetServerItem.GetVisible: Boolean;
begin
  if FServerData = nil then
    Refresh;
  Result := not Boolean(TSERVER_INFO_102(FServerData^).sv102_hidden);
end;

procedure TStNetServerItem.SetAnnounceRate(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> AnnounceRate then begin
    ErrorD := StNetServerSetInfo(FServer, 1017, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TSERVER_INFO_102(FServerData^).sv102_announce := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetAnnounceRateDelta(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> AnnounceRateDelta then begin
    ErrorD := StNetServerSetInfo(FServer, 1018, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TSERVER_INFO_102(FServerData^).sv102_anndelta := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetDisconnectTime(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> DisconnectTime then begin
    ErrorD := StNetServerSetInfo(FServer, 1010, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TSERVER_INFO_102(FServerData^).sv102_disc := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetMaxUsers(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> MaxUsers then begin
    ErrorD := StNetServerSetInfo(FServer, 1107, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TSERVER_INFO_102(FServerData^).sv102_users := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetVisible(Value: Boolean);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> Visible then begin
    ErrorD := StNetServerSetInfo(FServer, 1016, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TSERVER_INFO_102(FServerData^).sv102_hidden := not Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

function TStNetServerItem.GetMinPasswordLen: DWord;
begin
  if FServerMData0 = nil then
    RefreshM0;
  Result := TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_min_passwd_len;
end;

function TStNetServerItem.GetMaxPasswordAge: DWord;
begin
  if FServerMData0 = nil then
    RefreshM0;
  Result := TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_max_passwd_age;
end;

function TStNetServerItem.GetMinPasswordAge: DWord;
begin
  if FServerMData0 = nil then
    RefreshM0;
  Result := TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_min_passwd_age;
end;

function TStNetServerItem.GetForceLogoff: TStTime;
begin
  if FServerMData0 = nil then
    RefreshM0;
  Result := TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_force_logoff;
end;

function TStNetServerItem.GetPasswordHistoryLength: DWord;
begin
  if FServerMData0 = nil then
    RefreshM0;
  Result := TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_password_hist_len;
end;

function TStNetServerItem.GetRole: TStNetServerRoleType;
begin
  if FServerMData1 = nil then
    RefreshM1;

  Result := srtUnknown;
  case TUSER_MODALS_INFO_1(FServerMData1^).usrmod1_role of
    UAS_ROLE_STANDALONE : Result := srtStandAlone;
    UAS_ROLE_MEMBER     : Result := srtMember;
    UAS_ROLE_BACKUP     : Result := srtBackup;
    UAS_ROLE_PRIMARY    : Result := strPrimary;
  end;
end;

function TStNetServerItem.GetPrimaryDC: string;
begin
  if FServerMData1 = nil then
    RefreshM1;
  Result := TUSER_MODALS_INFO_1(FServerMData1^).usrmod1_primary;
end;

function TStNetServerItem.GetDomainName: string;
begin
  if FServerMData2 = nil then
    RefreshM2;
  Result := TUSER_MODALS_INFO_2(FServerMData2^).usrmod2_domain_name;
end;

function TStNetServerItem.GetDomainSid : TStSidRecord;
var
  Index : Integer;
begin
  if FServerMData2 = nil then
    RefreshM2;

  Result.Value := TUSER_MODALS_INFO_2(FServerMData2^).usrmod2_domain_id;
  Result.Usage := nstDomain;
  Result.Length:= High(DWord);

  Result.ValueS :=
    Format('S-1-%d',
        [TSIDIdentifierAuthority(StGetSidIdentifierAuthority(Result.Value)^).Value[5]]);

      for Index := 0 to StGetSidSubAuthorityCount(Result.Value) - 1 do begin
        Result.ValueS :=
          Format(Result.ValueS + '-%d', [StGetSidSubAuthority(Result.Value, Index)]);
      end;
end;

function TStNetServerItem.GetLockOutDuration : DWord;
begin
  if FServerMData3 = nil then
    RefreshM3;
  Result := TUSER_MODALS_INFO_3(FServerMData3^).usrmod3_lockout_duration;
end;

function TStNetServerItem.GetLockoutObservationWindow: DWord;
begin
  if FServerMData3 = nil then
    RefreshM3;
  Result := TUSER_MODALS_INFO_3(FServerMData3^).usrmod3_lockout_observation_window;
end;

function TStNetServerItem.GetLockoutThreshold: DWord;
begin
  if FServerMData3 = nil then
    RefreshM3;
  Result := TUSER_MODALS_INFO_3(FServerMData3^).usrmod3_lockout_threshold;
end;

procedure TStNetServerItem.SetMinPasswordLen(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> MinPasswordLen then begin
    ErrorD := StNetUserModalsSet(FServer, 1001, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_min_passwd_len := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetMaxPasswordAge(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> MaxPasswordAge then begin
    ErrorD := StNetUserModalsSet(FServer, 1002, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_max_passwd_age := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetMinPasswordAge(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> MinPasswordAge then begin
    ErrorD := StNetUserModalsSet(FServer, 1003, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_min_passwd_age := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetForceLogoff(Value: TStTime);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> ForceLogoff then begin
    ErrorD := StNetUserModalsSet(FServer, 1004, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_force_logoff := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetPasswordHistoryLength(Value: DWord);
var
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> PasswordHistoryLength then begin
    ErrorD := StNetUserModalsSet(FServer, 1005, @Value, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_0(FServerMData0^).usrmod0_password_hist_len := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetLockOutDuration(Value: DWord);
var
  Data    : TUSER_MODALS_INFO_3;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> LockOutDuration then begin
    Data.usrmod3_lockout_duration           := Value;
    Data.usrmod3_lockout_observation_window := LockoutObservationWindow;
    Data.usrmod3_lockout_threshold          := LockoutThreshold;

    ErrorD := StNetUserModalsSet(FServer, 3, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_3(FServerMData3^).usrmod3_lockout_duration := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetLockoutObservationWindow(Value: DWord);
var
  Data    : TUSER_MODALS_INFO_3;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> LockoutObservationWindow then begin
    Data.usrmod3_lockout_duration           := LockOutDuration;
    Data.usrmod3_lockout_observation_window := Value;
    Data.usrmod3_lockout_threshold          := LockoutThreshold;

    ErrorD := StNetUserModalsSet(FServer, 3, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_3(FServerMData3^).usrmod3_lockout_observation_window := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

procedure TStNetServerItem.SetLockoutThreshold(Value: DWord);
var
  Data    : TUSER_MODALS_INFO_3;
  ErrorD  : DWord;
  ParmErr : DWord;
begin
  if Value <> LockoutThreshold then begin
    Data.usrmod3_lockout_duration           := LockOutDuration;
    Data.usrmod3_lockout_observation_window := LockoutObservationWindow;
    Data.usrmod3_lockout_threshold          := Value;

    ErrorD := StNetUserModalsSet(FServer, 3, @Data, ParmErr);
    if ErrorD = NERR_SUCCESS then begin
      TUSER_MODALS_INFO_3(FServerMData3^).usrmod3_lockout_threshold := Value;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;
end;

{ --- TStNetwork ------------------------------------------------------------ }
constructor TStNetwork.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TStNetwork.Destroy;
var
  I : Integer;
begin
  for I := 0 to FList.Count-1 do
    FList.Objects[I].Free;

  FList.Free;
  inherited Destroy;
end;

function TStNetwork.GetServer(AServer: string): TStNetServerItem;
var
  Index : Integer;
  ErrorD: DWord;
  Buffer : Pointer;
begin
  if Trim(AServer) = '' then begin
    ErrorD := StNetServerGetInfo('', 100, Buffer);
    if ErrorD = NERR_SUCCESS then begin
      try
        AServer := TSERVER_INFO_100(Buffer^).sv100_name;
      finally
        StNetApiBufferFree(Buffer);
        Buffer := nil;
      end;
    end else begin
      RaiseStWin32Error(EStNetException, ErrorD);
    end;
  end;

  { look in the internal list first }
  Index := FList.IndexOf(AServer);
  if Index >= 0 then begin
    Result := TStNetServerItem(FList.Objects[Index]);
  end else begin
    Result := TStNetServerItem.CreateEx(AServer, '');
    Result.FNetwork := Self;

    { Add this item to the internal list }
    FList.AddObject(AServer, Result);
  end;
end;

function TStNetwork.GetUser(AServer, AName: string): TStNetUserItem;
var
  S : TStNetServerItem;
begin
  Result := nil;

  { we first need the server that this user is on }
  S := Server[AServer];

  if (S <> nil) then
    { now that we have the server, get the user from it }
    Result := S.User[AName];
end;

function TStNetwork.GetGroup(AServer, AName: string): TStNetGroupItem;
var
  S : TStNetServerItem;
begin
  Result := nil;

  { we first need the server that this group is on }
  S := Server[AServer];

  if (S <> nil) then
    { now that we have the server, get the group from it }
    Result := S.Group[AName];
end;

function TStNetwork.GetPrimaryDC(ADomain: string): TStNetServerItem;
var
  Buffer : string;
  ErrorD : DWord;
begin
  Result := nil;

  { lookup the domain }
  ErrorD := StNetGetDCName('', ADomain, Buffer);
  if ErrorD = NERR_SUCCESS then begin
    { get this server from the list }
    Result := Server[FilterL(Buffer, '\')];
  end else begin
    RaiseStWin32Error(EStNetException, ErrorD);
  end;
end;

{ --------------------------------------------------------------------------- }

initialization
  StNetwork := TStNetwork.Create;
finalization
  StNetwork.Free
end.
