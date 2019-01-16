// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNet.pas' rev: 33.00 (Windows)

#ifndef StnetHPP
#define StnetHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <StBase.hpp>
#include <StDate.hpp>
#include <StNetAPI.hpp>
#include <StConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stnet
{
//-- forward type declarations -----------------------------------------------
struct TStSidRecord;
class DELPHICLASS TStNetItem;
class DELPHICLASS TStNetUserItem;
class DELPHICLASS TStNetGroupItem;
class DELPHICLASS TStNetShareItem;
class DELPHICLASS TStNetServerItem;
class DELPHICLASS TStNetwork;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStNetItemType : unsigned char { nitUnknown, nitLocalUser, nitGlobalUser, nitLocalGroup, nitGlobalGroup, nitComputer, nitInterdomainTrust, nitWorkstationTrust, nitServerTrust, nitShare };

enum DECLSPEC_DENUM TStNetSidType : unsigned char { nstNone, nstUser, nstGroup, nstDomain, nstAlias, nstWellKnownGroup, nstDeletedAccount, nstInvalid, nstUnknown, nstComputer };

enum DECLSPEC_DENUM TStNetUserPrivType : unsigned char { uptUnknown, uptGuest, uptUser, uptAdmin };

enum DECLSPEC_DENUM TStNetUserAuthPrivType : unsigned char { uaptPrint, uaptCommunications, uaptServer, uaptAccounts };

typedef System::Set<TStNetUserAuthPrivType, TStNetUserAuthPrivType::uaptPrint, TStNetUserAuthPrivType::uaptAccounts> TStNetUserAuthPrivSet;

enum DECLSPEC_DENUM TStNetShareType : unsigned char { stUnknown, stDisk, stPrint, stDevice, stIPC, stSpecial };

enum DECLSPEC_DENUM TStNetServerPlatformType : unsigned char { sptUnknown, sptDOS, sptOS2, sptNT, sptOSF, sptVMS };

enum DECLSPEC_DENUM TStNetServerType : unsigned char { nsvtWorkstation, nsvtServer, nsvtSQLServer, nsvtDomainCtrl, nsvtDomainBackupCtrl, nsvtTimeSource, nsvtAFP, nsvtNovell, nsvtDomainMember, nsvtPrintQServer, nsvtDialinServer, nsvtUNIXServer, nsvtNT, nsvtWFW, nsvtMFPN, nsvtServerNT, nsvtPotentialBrowser, nsvtBackupBrowser, nsvtMasterBrowser, nsvtDomainMaster, nsvtOSF, nsvtVMS, nsvtWindows, nsvtDFS, nsvtClusterNT, nsvtTerminalServer, nsvtDCE, nsvtAlternateXPORT, nsvtLocalListOnly, nsvtDomainEnum };

typedef System::Set<TStNetServerType, TStNetServerType::nsvtWorkstation, TStNetServerType::nsvtDomainEnum> TStNetServerSet;

typedef System::StaticArray<System::StaticArray<bool, 24>, 7> TStNetUserLogonTimes;

enum DECLSPEC_DENUM TStNetServerRoleType : unsigned char { srtUnknown, srtStandAlone, srtMember, srtBackup, strPrimary };

struct DECLSPEC_DRECORD TStSidRecord
{
public:
	void *Value;
	System::UnicodeString ValueS;
	unsigned Length;
	TStNetSidType Usage;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNetItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TStNetwork* FNetwork;
	TStNetItemType FItemType;
	System::UnicodeString FName;
	System::UnicodeString FServer;
	
protected:
	System::UnicodeString FComment;
	__fastcall TStNetItem(System::UnicodeString AName, System::UnicodeString AComment, System::UnicodeString AServer, TStNetItemType AItemType);
	
public:
	__fastcall TStNetItem();
	__fastcall virtual ~TStNetItem();
	__property System::UnicodeString Comment = {read=FComment};
	__property TStNetItemType ItemType = {read=FItemType, nodefault};
	__property System::UnicodeString Name = {read=FName};
	__property System::UnicodeString Server = {read=FServer};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNetUserItem : public TStNetItem
{
	typedef TStNetItem inherited;
	
private:
	System::UnicodeString FFullName;
	unsigned FID;
	System::Classes::TStrings* FGroupList;
	void *FUserData;
	TStSidRecord FSidRecord;
	System::UnicodeString FDomain;
	System::Classes::TStrings* FWorkStationList;
	
protected:
	bool __fastcall GetAccountDisabled();
	Stdate::TStDateTimeRec __fastcall GetAccountExpires();
	unsigned __fastcall GetBadPasswordCount();
	System::UnicodeString __fastcall GetDomain();
	System::UnicodeString __fastcall GetFullName();
	System::Classes::TStringList* __fastcall GetGroupItemList();
	System::UnicodeString __fastcall GetHomeDirectory();
	System::UnicodeString __fastcall GetHomeDrive();
	Stdate::TStDateTimeRec __fastcall GetLastLogon();
	Stdate::TStDateTimeRec __fastcall GetLastLogoff();
	bool __fastcall GetLockedOut();
	System::UnicodeString __fastcall GetName();
	bool __fastcall GetNoUserPasswordChange();
	unsigned __fastcall GetNumberOfLogons();
	TStNetUserAuthPrivSet __fastcall GetOperatorPrivilege();
	bool __fastcall GetPasswordNeverExpires();
	bool __fastcall GetPasswordExpired();
	Stdate::TStDateTimeRec __fastcall GetPasswordLastChanged();
	bool __fastcall GetPasswordNotRequired();
	TStNetItem* __fastcall GetPrimaryGroup();
	System::UnicodeString __fastcall GetProfilePath();
	TStSidRecord __fastcall GetSid();
	System::UnicodeString __fastcall GetScriptPath();
	System::UnicodeString __fastcall GetUserComment();
	TStNetUserPrivType __fastcall GetUserPrivilege();
	System::Classes::TStrings* __fastcall GetWorkstations();
	void __fastcall SetAccountDisabled(bool Value);
	void __fastcall SetAccountExpires(const Stdate::TStDateTimeRec &Value);
	void __fastcall SetComment(System::UnicodeString Value);
	void __fastcall SetFullName(System::UnicodeString Value);
	void __fastcall SetHomeDirectory(System::UnicodeString Value);
	void __fastcall SetHomeDrive(System::UnicodeString Value);
	void __fastcall SetLockedOut(bool Value);
	void __fastcall SetName(System::UnicodeString Value);
	void __fastcall SetNoUserPasswordChange(bool Value);
	void __fastcall SetPasswordAdmin(System::UnicodeString Value);
	void __fastcall SetPasswordNeverExpires(bool Value);
	void __fastcall SetPasswordExpired(bool Value);
	void __fastcall SetPasswordNotRequired(bool Value);
	void __fastcall SetPrimaryGroup(TStNetItem* Value);
	void __fastcall SetProfilePath(System::UnicodeString Value);
	void __fastcall SetScriptPath(System::UnicodeString Value);
	void __fastcall SetUserComment(System::UnicodeString Value);
	void __fastcall SetWorkstations(System::Classes::TStrings* Value);
	__fastcall TStNetUserItem(System::UnicodeString AName, System::UnicodeString AComment, System::UnicodeString AServer, TStNetItemType AItemType);
	__fastcall TStNetUserItem(System::UnicodeString AName, System::UnicodeString AComment, System::UnicodeString AServer, System::UnicodeString AFullName, bool AGlobal, unsigned AID);
	
public:
	__fastcall virtual ~TStNetUserItem();
	void __fastcall Delete();
	void __fastcall Refresh();
	void __fastcall AddToGroup(TStNetItem* AGroup);
	void __fastcall RemoveFromGroup(TStNetItem* AGroup);
	void __fastcall GetLogonHours(TStNetUserLogonTimes &LogonHours);
	void __fastcall SetLogonHours(const TStNetUserLogonTimes &LogonHours);
	void __fastcall SetPassword(System::UnicodeString OldPassword, System::UnicodeString NewPassword);
	__property bool AccountDisabled = {read=GetAccountDisabled, write=SetAccountDisabled, nodefault};
	__property Stdate::TStDateTimeRec AccountExpires = {read=GetAccountExpires, write=SetAccountExpires};
	__property unsigned BadPasswordCount = {read=GetBadPasswordCount, nodefault};
	__property System::UnicodeString Comment = {read=FComment, write=SetComment};
	__property System::UnicodeString Domain = {read=GetDomain};
	__property System::UnicodeString FullName = {read=GetFullName, write=SetFullName};
	__property System::Classes::TStringList* Groups = {read=GetGroupItemList};
	__property System::UnicodeString HomeDirectory = {read=GetHomeDirectory, write=SetHomeDirectory};
	__property System::UnicodeString HomeDrive = {read=GetHomeDrive, write=SetHomeDrive};
	__property unsigned ID = {read=FID, nodefault};
	__property Stdate::TStDateTimeRec LastLogon = {read=GetLastLogon};
	__property Stdate::TStDateTimeRec LastLogoff = {read=GetLastLogoff};
	__property bool LockedOut = {read=GetLockedOut, write=SetLockedOut, nodefault};
	__property System::UnicodeString Name = {read=GetName, write=SetName};
	__property bool NoUserPasswordChange = {read=GetNoUserPasswordChange, write=SetNoUserPasswordChange, nodefault};
	__property unsigned NumberOfLogons = {read=GetNumberOfLogons, nodefault};
	__property TStNetUserAuthPrivSet OperatorPrivilege = {read=GetOperatorPrivilege, nodefault};
	__property System::UnicodeString Password = {write=SetPasswordAdmin};
	__property bool PasswordNeverExpires = {read=GetPasswordNeverExpires, write=SetPasswordNeverExpires, nodefault};
	__property bool PasswordExpired = {read=GetPasswordExpired, write=SetPasswordExpired, nodefault};
	__property Stdate::TStDateTimeRec PasswordLastChanged = {read=GetPasswordLastChanged};
	__property bool PasswordNotRequired = {read=GetPasswordNotRequired, write=SetPasswordNotRequired, nodefault};
	__property TStNetItem* PrimaryGroup = {read=GetPrimaryGroup, write=SetPrimaryGroup};
	__property System::UnicodeString ProfilePath = {read=GetProfilePath, write=SetProfilePath};
	__property System::UnicodeString ScriptPath = {read=GetScriptPath, write=SetScriptPath};
	__property TStSidRecord Sid = {read=GetSid};
	__property System::UnicodeString UserComment = {read=GetUserComment, write=SetUserComment};
	__property TStNetUserPrivType UserPrivilege = {read=GetUserPrivilege, nodefault};
	__property System::Classes::TStrings* Workstations = {read=GetWorkstations, write=SetWorkstations};
public:
	/* TStNetItem.Create */ inline __fastcall TStNetUserItem() : TStNetItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNetGroupItem : public TStNetItem
{
	typedef TStNetItem inherited;
	
private:
	unsigned FID;
	System::Classes::TStrings* FItemList;
	TStSidRecord FSidRecord;
	System::UnicodeString FDomain;
	
protected:
	__fastcall TStNetGroupItem(System::UnicodeString AName, System::UnicodeString AComment, System::UnicodeString AServer, TStNetItemType AItemType);
	__fastcall TStNetGroupItem(System::UnicodeString AName, System::UnicodeString AComment, System::UnicodeString AServer, unsigned AID);
	System::Classes::TStringList* __fastcall GetGroupItemList();
	System::UnicodeString __fastcall GetDomain();
	TStSidRecord __fastcall GetSid();
	System::UnicodeString __fastcall GetName();
	void __fastcall SetComment(System::UnicodeString Value);
	void __fastcall SetName(System::UnicodeString Value);
	
public:
	__fastcall virtual ~TStNetGroupItem();
	void __fastcall AddToGroup(TStNetItem* AItem);
	void __fastcall RemoveFromGroup(TStNetItem* AItem);
	void __fastcall Delete();
	void __fastcall Refresh();
	__property System::UnicodeString Comment = {read=FComment, write=SetComment};
	__property System::UnicodeString Name = {read=GetName, write=SetName};
	__property unsigned ID = {read=FID, nodefault};
	__property System::Classes::TStringList* Items = {read=GetGroupItemList};
	__property TStSidRecord Sid = {read=GetSid};
	__property System::UnicodeString Domain = {read=GetDomain};
public:
	/* TStNetItem.Create */ inline __fastcall TStNetGroupItem() : TStNetItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNetShareItem : public TStNetItem
{
	typedef TStNetItem inherited;
	
private:
	TStNetShareType FShareType;
	
protected:
	__fastcall TStNetShareItem(System::UnicodeString AName, System::UnicodeString AComment, System::UnicodeString AServer, TStNetShareType AShareType);
	
public:
	__property TStNetShareType ShareType = {read=FShareType, nodefault};
public:
	/* TStNetItem.Create */ inline __fastcall TStNetShareItem() : TStNetItem() { }
	/* TStNetItem.Destroy */ inline __fastcall virtual ~TStNetShareItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNetServerItem : public TStNetItem
{
	typedef TStNetItem inherited;
	
private:
	System::Classes::TStringList* FUserList;
	System::Classes::TStringList* FGroupList;
	System::Classes::TStringList* FDriveList;
	System::Classes::TStringList* FShareList;
	void *FServerData;
	void *FServerMData0;
	void *FServerMData1;
	void *FServerMData2;
	void *FServerMData3;
	
protected:
	__fastcall TStNetServerItem(System::UnicodeString AName, System::UnicodeString AComment);
	TStNetUserItem* __fastcall GetUser(System::UnicodeString AName);
	TStNetGroupItem* __fastcall GetGroup(System::UnicodeString AName);
	System::Classes::TStringList* __fastcall GetDriveList();
	System::Classes::TStringList* __fastcall GetUserList();
	System::Classes::TStringList* __fastcall GetGroupList();
	System::Classes::TStringList* __fastcall GetShareList();
	void __fastcall SetComment(System::UnicodeString Value);
	unsigned __fastcall GetAnnounceRate();
	unsigned __fastcall GetAnnounceRateDelta();
	unsigned __fastcall GetDisconnectTime();
	unsigned __fastcall GetMaxUsers();
	TStNetServerPlatformType __fastcall GetPlatform();
	TStNetServerSet __fastcall GetServerType();
	System::UnicodeString __fastcall GetUserPath();
	bool __fastcall GetVisible();
	unsigned __fastcall GetVersion();
	unsigned __fastcall GetMinPasswordLen();
	unsigned __fastcall GetMaxPasswordAge();
	unsigned __fastcall GetMinPasswordAge();
	int __fastcall GetForceLogoff();
	unsigned __fastcall GetPasswordHistoryLength();
	TStNetServerRoleType __fastcall GetRole();
	System::UnicodeString __fastcall GetPrimaryDC();
	System::UnicodeString __fastcall GetDomainName();
	TStSidRecord __fastcall GetDomainSid();
	unsigned __fastcall GetLockOutDuration();
	unsigned __fastcall GetLockoutObservationWindow();
	unsigned __fastcall GetLockoutThreshold();
	void __fastcall SetMinPasswordLen(unsigned Value);
	void __fastcall SetMaxPasswordAge(unsigned Value);
	void __fastcall SetMinPasswordAge(unsigned Value);
	void __fastcall SetForceLogoff(int Value);
	void __fastcall SetPasswordHistoryLength(unsigned Value);
	void __fastcall SetLockOutDuration(unsigned Value);
	void __fastcall SetLockoutObservationWindow(unsigned Value);
	void __fastcall SetLockoutThreshold(unsigned Value);
	void __fastcall SetAnnounceRate(unsigned Value);
	void __fastcall SetAnnounceRateDelta(unsigned Value);
	void __fastcall SetDisconnectTime(unsigned Value);
	void __fastcall SetMaxUsers(unsigned Value);
	void __fastcall SetVisible(bool Value);
	void __fastcall RefreshM0();
	void __fastcall RefreshM1();
	void __fastcall RefreshM2();
	void __fastcall RefreshM3();
	
public:
	__fastcall virtual ~TStNetServerItem();
	TStNetGroupItem* __fastcall AddGroup(System::UnicodeString AName, System::UnicodeString ADescription, bool AGlobal);
	TStNetUserItem* __fastcall AddUser(System::UnicodeString AName, System::UnicodeString APassword, bool AGlobal);
	void __fastcall Refresh();
	__property unsigned AnnounceRate = {read=GetAnnounceRate, write=SetAnnounceRate, nodefault};
	__property unsigned AnnounceRateDelta = {read=GetAnnounceRateDelta, write=SetAnnounceRateDelta, nodefault};
	__property System::UnicodeString Comment = {read=FComment, write=SetComment};
	__property unsigned DisconnectTime = {read=GetDisconnectTime, write=SetDisconnectTime, nodefault};
	__property unsigned MaxUsers = {read=GetMaxUsers, write=SetMaxUsers, nodefault};
	__property TStNetServerPlatformType Platform = {read=GetPlatform, nodefault};
	__property TStNetServerSet ServerType = {read=GetServerType, nodefault};
	__property System::UnicodeString UserPath = {read=GetUserPath};
	__property unsigned Version = {read=GetVersion, nodefault};
	__property bool Visible = {read=GetVisible, write=SetVisible, nodefault};
	__property unsigned MinPasswordLen = {read=GetMinPasswordLen, write=SetMinPasswordLen, nodefault};
	__property unsigned MaxPasswordAge = {read=GetMaxPasswordAge, write=SetMaxPasswordAge, nodefault};
	__property unsigned MinPasswordAge = {read=GetMinPasswordAge, write=SetMinPasswordAge, nodefault};
	__property int ForceLogoff = {read=GetForceLogoff, write=SetForceLogoff, nodefault};
	__property unsigned PasswordHistoryLength = {read=GetPasswordHistoryLength, write=SetPasswordHistoryLength, nodefault};
	__property TStNetServerRoleType Role = {read=GetRole, nodefault};
	__property System::UnicodeString PrimaryDC = {read=GetPrimaryDC};
	__property System::UnicodeString DomainName = {read=GetDomainName};
	__property TStSidRecord DomainSid = {read=GetDomainSid};
	__property unsigned LockOutDuration = {read=GetLockOutDuration, write=SetLockOutDuration, nodefault};
	__property unsigned LockoutObservationWindow = {read=GetLockoutObservationWindow, write=SetLockoutObservationWindow, nodefault};
	__property unsigned LockoutThreshold = {read=GetLockoutThreshold, write=SetLockoutThreshold, nodefault};
	__property TStNetUserItem* User[System::UnicodeString AName] = {read=GetUser};
	__property TStNetGroupItem* Group[System::UnicodeString AName] = {read=GetGroup};
	__property System::Classes::TStringList* Drives = {read=GetDriveList};
	__property System::Classes::TStringList* Users = {read=GetUserList};
	__property System::Classes::TStringList* Groups = {read=GetGroupList};
	__property System::Classes::TStringList* Shares = {read=GetShareList};
public:
	/* TStNetItem.Create */ inline __fastcall TStNetServerItem() : TStNetItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNetwork : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* FList;
	
protected:
	TStNetServerItem* __fastcall GetServer(System::UnicodeString AServer);
	TStNetUserItem* __fastcall GetUser(System::UnicodeString AServer, System::UnicodeString AName);
	TStNetGroupItem* __fastcall GetGroup(System::UnicodeString AServer, System::UnicodeString AName);
	TStNetServerItem* __fastcall GetPrimaryDC(System::UnicodeString ADomain);
	
public:
	__fastcall TStNetwork();
	__fastcall virtual ~TStNetwork();
	__property TStNetServerItem* Server[System::UnicodeString AServer] = {read=GetServer};
	__property TStNetUserItem* User[System::UnicodeString AServer][System::UnicodeString AName] = {read=GetUser};
	__property TStNetGroupItem* Group[System::UnicodeString AServer][System::UnicodeString AName] = {read=GetGroup};
	__property TStNetServerItem* PrimaryDC[System::UnicodeString ADomain] = {read=GetPrimaryDC};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TStNetwork* StNetwork;
}	/* namespace Stnet */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNET)
using namespace Stnet;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnetHPP
