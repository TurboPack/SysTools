// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNetAPI.pas' rev: 28.00 (Windows)

#ifndef StnetapiHPP
#define StnetapiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stnetapi
{
//-- type declarations -------------------------------------------------------
typedef unsigned NET_API_STATUS;

typedef unsigned API_RET_TYPE;

typedef unsigned __stdcall (*TNetUserAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetUserEnum)(System::WideChar * ServerName, unsigned Level, unsigned Filter, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetUserGetInfo)(System::WideChar * ServerName, System::WideChar * UserName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetUserSetInfo)(System::WideChar * ServerName, System::WideChar * UserName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetUserDel)(System::WideChar * ServerName, System::WideChar * UserName);

typedef unsigned __stdcall (*TNetUserGetGroups)(System::WideChar * ServerName, System::WideChar * UserName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries);

typedef unsigned __stdcall (*TNetUserSetGroups)(System::WideChar * ServerName, System::WideChar * UserName, unsigned Level, void * Buffer, unsigned NumEntries);

typedef unsigned __stdcall (*TNetUserGetLocalGroups)(System::WideChar * ServerName, System::WideChar * UserName, unsigned Level, unsigned Flags, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries);

typedef unsigned __stdcall (*TNetUserModalsGet)(System::WideChar * ServerName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetUserModalsSet)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetUserChangePassword)(System::WideChar * DomainName, System::WideChar * UserName, System::WideChar * OldPassword, System::WideChar * NewPassword);

typedef unsigned __stdcall (*TNetGroupAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetGroupAddUser)(System::WideChar * ServerName, System::WideChar * GroupName, System::WideChar * UserName);

typedef unsigned __stdcall (*TNetGroupEnum)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetGroupGetInfo)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetGroupSetInfo)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetGroupDel)(System::WideChar * ServerName, System::WideChar * GroupName);

typedef unsigned __stdcall (*TNetGroupDelUser)(System::WideChar * ServerName, System::WideChar * GroupName, System::WideChar * UserName);

typedef unsigned __stdcall (*TNetGroupGetUsers)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, void * ResumeHandle);

typedef unsigned __stdcall (*TNetGroupSetUsers)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * Buffer, unsigned &TotalEntries);

typedef unsigned __stdcall (*TNetLocalGroupAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetLocalGroupAddMember)(System::WideChar * ServerName, System::WideChar * GroupName, void * MembersID);

typedef unsigned __stdcall (*TNetLocalGroupEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetLocalGroupGetInfo)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetLocalGroupSetInfo)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetLocalGroupDel)(System::WideChar * ServerName, System::WideChar * GroupName);

typedef unsigned __stdcall (*TNetLocalGroupDelMember)(System::WideChar * ServerName, System::WideChar * GroupName, void * MembersID);

typedef unsigned __stdcall (*TNetLocalGroupGetMembers)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, void * ResumeHandle);

typedef unsigned __stdcall (*TNetLocalGroupSetMembers)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * Buffer, unsigned &TotalEntries);

typedef unsigned __stdcall (*TNetLocalGroupAddMembers)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * Buffer, unsigned TotalEntries);

typedef unsigned __stdcall (*TNetLocalGroupDelMembers)(System::WideChar * ServerName, System::WideChar * GroupName, unsigned Level, void * Buffer, unsigned TotalEntries);

typedef unsigned __stdcall (*TNetQueryDisplayInformation)(System::WideChar * ServerName, unsigned Level, unsigned Index, unsigned EntriesRequested, unsigned PrefMaxLen, unsigned &ReturnedCount, void * &Buffer);

typedef unsigned __stdcall (*TNetGetDisplayInformationIndex)(System::WideChar * ServerName, unsigned Level, System::WideChar * Prefix, unsigned &Index);

typedef unsigned __stdcall (*TNetAccessAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetAccessEnum)(System::WideChar * ServerName, System::WideChar * BasePath, unsigned Recursive, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetAccessGetInfo)(System::WideChar * ServerName, System::WideChar * Resource, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetAccessSetInfo)(System::WideChar * ServerName, System::WideChar * Resource, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetAccessDel)(System::WideChar * ServerName, System::WideChar * Resource);

typedef unsigned __stdcall (*TNetAccessGetUserPerms)(System::WideChar * ServerName, System::WideChar * UGname, System::WideChar * Resource, unsigned &Perms);

typedef unsigned __stdcall (*TNetGetDCName)(System::WideChar * ServerName, System::WideChar * DomainName, void * &Buffer);

typedef unsigned __stdcall (*TNetGetAnyDCName)(System::WideChar * ServerName, System::WideChar * DomainName, System::PByte &Buffer);

typedef unsigned __stdcall (*TI_NetLogonControl)(System::WideChar * ServerName, unsigned FunctionCode, unsigned QueryLevel, void * &Buffer);

typedef unsigned __stdcall (*TI_NetLogonControl2)(System::WideChar * ServerName, unsigned FunctionCode, unsigned QueryLevel, void * Data, void * &Buffer);

typedef int __stdcall (*TNetEnumerateTrustedDomains)(System::WideChar * ServerName, void * &Buffer);

struct TUSER_INFO_0;
typedef TUSER_INFO_0 *PUSER_INFO_0;

struct DECLSPEC_DRECORD TUSER_INFO_0
{
public:
	System::WideChar *usri0_name;
};


struct TUSER_INFO_1;
typedef TUSER_INFO_1 *PUSER_INFO_1;

struct DECLSPEC_DRECORD TUSER_INFO_1
{
public:
	System::WideChar *usri1_name;
	System::WideChar *usri1_password;
	unsigned usri1_password_age;
	unsigned usri1_priv;
	System::WideChar *usri1_home_dir;
	System::WideChar *usri1_comment;
	unsigned usri1_flags;
	System::WideChar *usri1_script_path;
};


struct TUSER_INFO_2;
typedef TUSER_INFO_2 *PUSER_INFO_2;

struct DECLSPEC_DRECORD TUSER_INFO_2
{
public:
	System::WideChar *usri2_name;
	System::WideChar *usri2_password;
	unsigned usri2_password_age;
	unsigned usri2_priv;
	System::WideChar *usri2_home_dir;
	System::WideChar *usri2_comment;
	unsigned usri2_flags;
	System::WideChar *usri2_script_path;
	unsigned usri2_auth_flags;
	System::WideChar *usri2_full_name;
	System::WideChar *usri2_usr_comment;
	System::WideChar *usri2_parms;
	System::WideChar *usri2_workstations;
	unsigned usri2_last_logon;
	unsigned usri2_last_logoff;
	unsigned usri2_acct_expires;
	unsigned usri2_max_storage;
	unsigned usri2_units_per_week;
	System::Byte *usri2_logon_hours;
	unsigned usri2_bad_pw_count;
	unsigned usri2_num_logons;
	System::WideChar *usri2_logon_server;
	unsigned usri2_country_code;
	unsigned usri2_code_page;
};


struct TUSER_INFO_3;
typedef TUSER_INFO_3 *PUSER_INFO_3;

struct DECLSPEC_DRECORD TUSER_INFO_3
{
public:
	System::WideChar *usri3_name;
	System::WideChar *usri3_password;
	unsigned usri3_password_age;
	unsigned usri3_priv;
	System::WideChar *usri3_home_dir;
	System::WideChar *usri3_comment;
	unsigned usri3_flags;
	System::WideChar *usri3_script_path;
	unsigned usri3_auth_flags;
	System::WideChar *usri3_full_name;
	System::WideChar *usri3_usr_comment;
	System::WideChar *usri3_parms;
	System::WideChar *usri3_workstations;
	unsigned usri3_last_logon;
	unsigned usri3_last_logoff;
	unsigned usri3_acct_expires;
	unsigned usri3_max_storage;
	unsigned usri3_units_per_week;
	void *usri3_logon_hours;
	unsigned usri3_bad_pw_count;
	unsigned usri3_num_logons;
	System::WideChar *usri3_logon_server;
	unsigned usri3_country_code;
	unsigned usri3_code_page;
	unsigned usri3_user_id;
	unsigned usri3_primary_group_id;
	System::WideChar *usri3_profile;
	System::WideChar *usri3_home_dir_drive;
	unsigned usri3_password_expired;
};


struct TUSER_INFO_10;
typedef TUSER_INFO_10 *PUSER_INFO_10;

struct DECLSPEC_DRECORD TUSER_INFO_10
{
public:
	System::WideChar *usri10_name;
	System::WideChar *usri10_comment;
	System::WideChar *usri10_usr_comment;
	System::WideChar *usri10_full_name;
};


struct TUSER_INFO_11;
typedef TUSER_INFO_11 *PUSER_INFO_11;

struct DECLSPEC_DRECORD TUSER_INFO_11
{
public:
	System::WideChar *usri11_name;
	System::WideChar *usri11_comment;
	System::WideChar *usri11_usr_comment;
	System::WideChar *usri11_full_name;
	unsigned usri11_priv;
	unsigned usri11_auth_flags;
	unsigned usri11_password_age;
	System::WideChar *usri11_home_dir;
	System::WideChar *usri11_parms;
	unsigned usri11_last_logon;
	unsigned usri11_last_logoff;
	unsigned usri11_bad_pw_count;
	unsigned usri11_num_logons;
	System::WideChar *usri11_logon_server;
	unsigned usri11_country_code;
	System::WideChar *usri11_workstations;
	unsigned usri11_max_storage;
	unsigned usri11_units_per_week;
	System::Byte *usri11_logon_hours;
	unsigned usri11_code_page;
};


struct TUSER_INFO_20;
typedef TUSER_INFO_20 *PUSER_INFO_20;

struct DECLSPEC_DRECORD TUSER_INFO_20
{
public:
	System::WideChar *usri20_name;
	System::WideChar *usri20_full_name;
	System::WideChar *usri20_comment;
	unsigned usri20_flags;
	unsigned usri20_user_id;
};


struct TUSER_INFO_21;
typedef TUSER_INFO_21 *PUSER_INFO_21;

struct DECLSPEC_DRECORD TUSER_INFO_21
{
public:
	System::StaticArray<System::Byte, 16> usri21_password;
};


struct TUSER_INFO_22;
typedef TUSER_INFO_22 *PUSER_INFO_22;

struct DECLSPEC_DRECORD TUSER_INFO_22
{
public:
	System::WideChar *usri22_name;
	System::StaticArray<System::Byte, 16> usri22_password;
	unsigned usri22_password_age;
	unsigned usri22_priv;
	System::WideChar *usri22_home_dir;
	System::WideChar *usri22_comment;
	unsigned usri22_flags;
	System::WideChar *usri22_script_path;
	unsigned usri22_auth_flags;
	System::WideChar *usri22_full_name;
	System::WideChar *usri22_usr_comment;
	System::WideChar *usri22_parms;
	System::WideChar *usri22_workstations;
	unsigned usri22_last_logon;
	unsigned usri22_last_logoff;
	unsigned usri22_acct_expires;
	unsigned usri22_max_storage;
	unsigned usri22_units_per_week;
	System::Byte *usri22_logon_hours;
	unsigned usri22_bad_pw_count;
	unsigned usri22_num_logons;
	System::WideChar *usri22_logon_server;
	unsigned usri22_country_code;
	unsigned usri22_code_page;
};


struct TUSER_INFO_1003;
typedef TUSER_INFO_1003 *PUSER_INFO_1003;

struct DECLSPEC_DRECORD TUSER_INFO_1003
{
public:
	System::WideChar *usri1003_password;
};


struct TUSER_INFO_1005;
typedef TUSER_INFO_1005 *PUSER_INFO_1005;

struct DECLSPEC_DRECORD TUSER_INFO_1005
{
public:
	unsigned usri1005_priv;
};


struct TUSER_INFO_1006;
typedef TUSER_INFO_1006 *PUSER_INFO_1006;

struct DECLSPEC_DRECORD TUSER_INFO_1006
{
public:
	System::WideChar *usri1006_home_dir;
};


struct TUSER_INFO_1007;
typedef TUSER_INFO_1007 *PUSER_INFO_1007;

struct DECLSPEC_DRECORD TUSER_INFO_1007
{
public:
	System::WideChar *usri1007_comment;
};


struct TUSER_INFO_1008;
typedef TUSER_INFO_1008 *PUSER_INFO_1008;

struct DECLSPEC_DRECORD TUSER_INFO_1008
{
public:
	unsigned usri1008_flags;
};


struct TUSER_INFO_1009;
typedef TUSER_INFO_1009 *PUSER_INFO_1009;

struct DECLSPEC_DRECORD TUSER_INFO_1009
{
public:
	System::WideChar *usri1009_script_path;
};


struct TUSER_INFO_1010;
typedef TUSER_INFO_1010 *PUSER_INFO_1010;

struct DECLSPEC_DRECORD TUSER_INFO_1010
{
public:
	unsigned usri1010_auth_flags;
};


struct TUSER_INFO_1011;
typedef TUSER_INFO_1011 *PUSER_INFO_1011;

struct DECLSPEC_DRECORD TUSER_INFO_1011
{
public:
	System::WideChar *usri1011_full_name;
};


struct TUSER_INFO_1012;
typedef TUSER_INFO_1012 *PUSER_INFO_1012;

struct DECLSPEC_DRECORD TUSER_INFO_1012
{
public:
	System::WideChar *usri1012_usr_comment;
};


struct TUSER_INFO_1013;
typedef TUSER_INFO_1013 *PUSER_INFO_1013;

struct DECLSPEC_DRECORD TUSER_INFO_1013
{
public:
	System::WideChar *usri1013_parms;
};


struct TUSER_INFO_1014;
typedef TUSER_INFO_1014 *PUSER_INFO_1014;

struct DECLSPEC_DRECORD TUSER_INFO_1014
{
public:
	System::WideChar *usri1014_workstations;
};


struct TUSER_INFO_1017;
typedef TUSER_INFO_1017 *PUSER_INFO_1017;

struct DECLSPEC_DRECORD TUSER_INFO_1017
{
public:
	unsigned usri1017_acct_expires;
};


struct TUSER_INFO_1018;
typedef TUSER_INFO_1018 *PUSER_INFO_1018;

struct DECLSPEC_DRECORD TUSER_INFO_1018
{
public:
	unsigned usri1018_max_storage;
};


struct TUSER_INFO_1020;
typedef TUSER_INFO_1020 *PUSER_INFO_1020;

struct DECLSPEC_DRECORD TUSER_INFO_1020
{
public:
	unsigned usri1020_units_per_week;
	System::Byte *usri1020_logon_hours;
};


struct TUSER_INFO_1023;
typedef TUSER_INFO_1023 *PUSER_INFO_1023;

struct DECLSPEC_DRECORD TUSER_INFO_1023
{
public:
	System::WideChar *usri1023_logon_server;
};


struct TUSER_INFO_1024;
typedef TUSER_INFO_1024 *PUSER_INFO_1024;

struct DECLSPEC_DRECORD TUSER_INFO_1024
{
public:
	unsigned usri1024_country_code;
};


struct TUSER_INFO_1025;
typedef TUSER_INFO_1025 *PUSER_INFO_1025;

struct DECLSPEC_DRECORD TUSER_INFO_1025
{
public:
	unsigned usri1025_code_page;
};


struct TUSER_INFO_1051;
typedef TUSER_INFO_1051 *PUSER_INFO_1051;

struct DECLSPEC_DRECORD TUSER_INFO_1051
{
public:
	unsigned usri1051_primary_group_id;
};


struct TUSER_INFO_1052;
typedef TUSER_INFO_1052 *PUSER_INFO_1052;

struct DECLSPEC_DRECORD TUSER_INFO_1052
{
public:
	System::WideChar *usri1052_profile;
};


struct TUSER_INFO_1053;
typedef TUSER_INFO_1053 *PUSER_INFO_1053;

struct DECLSPEC_DRECORD TUSER_INFO_1053
{
public:
	System::WideChar *usri1053_home_dir_drive;
};


struct TUSER_MODALS_INFO_0;
typedef TUSER_MODALS_INFO_0 *PUSER_MODALS_INFO_0;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_0
{
public:
	unsigned usrmod0_min_passwd_len;
	unsigned usrmod0_max_passwd_age;
	unsigned usrmod0_min_passwd_age;
	unsigned usrmod0_force_logoff;
	unsigned usrmod0_password_hist_len;
};


struct TUSER_MODALS_INFO_1;
typedef TUSER_MODALS_INFO_1 *PUSER_MODALS_INFO_1;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1
{
public:
	unsigned usrmod1_role;
	System::WideChar *usrmod1_primary;
};


struct TUSER_MODALS_INFO_2;
typedef TUSER_MODALS_INFO_2 *PUSER_MODALS_INFO_2;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_2
{
public:
	System::WideChar *usrmod2_domain_name;
	void *usrmod2_domain_id;
};


struct TUSER_MODALS_INFO_3;
typedef TUSER_MODALS_INFO_3 *PUSER_MODALS_INFO_3;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_3
{
public:
	unsigned usrmod3_lockout_duration;
	unsigned usrmod3_lockout_observation_window;
	unsigned usrmod3_lockout_threshold;
};


struct TUSER_MODALS_INFO_1001;
typedef TUSER_MODALS_INFO_1001 *PUSER_MODALS_INFO_1001;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1001
{
public:
	unsigned usrmod1001_min_passwd_len;
};


struct TUSER_MODALS_INFO_1002;
typedef TUSER_MODALS_INFO_1002 *PUSER_MODALS_INFO_1002;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1002
{
public:
	unsigned usrmod1002_max_passwd_age;
};


struct TUSER_MODALS_INFO_1003;
typedef TUSER_MODALS_INFO_1003 *PUSER_MODALS_INFO_1003;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1003
{
public:
	unsigned usrmod1003_min_passwd_age;
};


struct TUSER_MODALS_INFO_1004;
typedef TUSER_MODALS_INFO_1004 *PUSER_MODALS_INFO_1004;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1004
{
public:
	unsigned usrmod1004_force_logoff;
};


struct TUSER_MODALS_INFO_1005;
typedef TUSER_MODALS_INFO_1005 *PUSER_MODALS_INFO_1005;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1005
{
public:
	unsigned usrmod1005_password_hist_len;
};


struct TUSER_MODALS_INFO_1006;
typedef TUSER_MODALS_INFO_1006 *PUSER_MODALS_INFO_1006;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1006
{
public:
	unsigned usrmod1006_role;
};


struct TUSER_MODALS_INFO_1007;
typedef TUSER_MODALS_INFO_1007 *PUSER_MODALS_INFO_1007;

struct DECLSPEC_DRECORD TUSER_MODALS_INFO_1007
{
public:
	System::WideChar *usrmod1007_primary;
};


struct TGROUP_INFO_0;
typedef TGROUP_INFO_0 *PGROUP_INFO_0;

struct DECLSPEC_DRECORD TGROUP_INFO_0
{
public:
	System::WideChar *grpi0_name;
};


struct TGROUP_INFO_1;
typedef TGROUP_INFO_1 *PGROUP_INFO_1;

struct DECLSPEC_DRECORD TGROUP_INFO_1
{
public:
	System::WideChar *grpi1_name;
	System::WideChar *grpi1_comment;
};


struct TGROUP_INFO_2;
typedef TGROUP_INFO_2 *PGROUP_INFO_2;

struct DECLSPEC_DRECORD TGROUP_INFO_2
{
public:
	System::WideChar *grpi2_name;
	System::WideChar *grpi2_comment;
	unsigned grpi2_group_id;
	unsigned grpi2_attributes;
};


struct TGROUP_INFO_1002;
typedef TGROUP_INFO_1002 *PGROUP_INFO_1002;

struct DECLSPEC_DRECORD TGROUP_INFO_1002
{
public:
	System::WideChar *grpi1002_comment;
};


struct TGROUP_INFO_1005;
typedef TGROUP_INFO_1005 *PGROUP_INFO_1005;

struct DECLSPEC_DRECORD TGROUP_INFO_1005
{
public:
	unsigned grpi1005_attributes;
};


struct TGROUP_USERS_INFO_0;
typedef TGROUP_USERS_INFO_0 *PGROUP_USERS_INFO_0;

struct DECLSPEC_DRECORD TGROUP_USERS_INFO_0
{
public:
	System::WideChar *grui0_name;
};


struct TGROUP_USERS_INFO_1;
typedef TGROUP_USERS_INFO_1 *PGROUP_USERS_INFO_1;

struct DECLSPEC_DRECORD TGROUP_USERS_INFO_1
{
public:
	System::WideChar *grui1_name;
	unsigned grui1_attributes;
};


struct TLOCALGROUP_INFO_0;
typedef TLOCALGROUP_INFO_0 *PLOCALGROUP_INFO_0;

struct DECLSPEC_DRECORD TLOCALGROUP_INFO_0
{
public:
	System::WideChar *lgrpi0_name;
};


struct TLOCALGROUP_INFO_1;
typedef TLOCALGROUP_INFO_1 *PLOCALGROUP_INFO_1;

struct DECLSPEC_DRECORD TLOCALGROUP_INFO_1
{
public:
	System::WideChar *lgrpi1_name;
	System::WideChar *lgrpi1_comment;
};


struct TLOCALGROUP_INFO_1002;
typedef TLOCALGROUP_INFO_1002 *PLOCALGROUP_INFO_1002;

struct DECLSPEC_DRECORD TLOCALGROUP_INFO_1002
{
public:
	System::WideChar *lgrpi1002_comment;
};


struct TLOCALGROUP_MEMBERS_INFO_0;
typedef TLOCALGROUP_MEMBERS_INFO_0 *PLOCALGROUP_MEMBERS_INFO_0;

struct DECLSPEC_DRECORD TLOCALGROUP_MEMBERS_INFO_0
{
public:
	void *lgrmi0_sid;
};


struct TLOCALGROUP_MEMBERS_INFO_1;
typedef TLOCALGROUP_MEMBERS_INFO_1 *PLOCALGROUP_MEMBERS_INFO_1;

struct DECLSPEC_DRECORD TLOCALGROUP_MEMBERS_INFO_1
{
public:
	void *lgrmi1_sid;
	unsigned lgrmi1_sidusage;
	System::WideChar *lgrmi1_name;
};


struct TLOCALGROUP_MEMBERS_INFO_2;
typedef TLOCALGROUP_MEMBERS_INFO_2 *PLOCALGROUP_MEMBERS_INFO_2;

struct DECLSPEC_DRECORD TLOCALGROUP_MEMBERS_INFO_2
{
public:
	void *lgrmi2_sid;
	unsigned lgrmi2_sidusage;
	System::WideChar *lgrmi2_domainandname;
};


struct TLOCALGROUP_MEMBERS_INFO_3;
typedef TLOCALGROUP_MEMBERS_INFO_3 *PLOCALGROUP_MEMBERS_INFO_3;

struct DECLSPEC_DRECORD TLOCALGROUP_MEMBERS_INFO_3
{
public:
	System::WideChar *lgrmi3_domainandname;
};


struct TLOCALGROUP_USERS_INFO_0;
typedef TLOCALGROUP_USERS_INFO_0 *PLOCALGROUP_USERS_INFO_0;

struct DECLSPEC_DRECORD TLOCALGROUP_USERS_INFO_0
{
public:
	System::WideChar *lgrui0_name;
};


struct TNET_DISPLAY_USER;
typedef TNET_DISPLAY_USER *PNET_DISPLAY_USER;

struct DECLSPEC_DRECORD TNET_DISPLAY_USER
{
public:
	System::WideChar *usri1_name;
	System::WideChar *usri1_comment;
	unsigned usri1_flags;
	System::WideChar *usri1_full_name;
	unsigned usri1_user_id;
	unsigned usri1_next_index;
};


struct TNET_DISPLAY_MACHINE;
typedef TNET_DISPLAY_MACHINE *PNET_DISPLAY_MACHINE;

struct DECLSPEC_DRECORD TNET_DISPLAY_MACHINE
{
public:
	System::WideChar *usri2_name;
	System::WideChar *usri2_comment;
	unsigned usri2_flags;
	unsigned usri2_user_id;
	unsigned usri2_next_index;
};


struct TNET_DISPLAY_GROUP;
typedef TNET_DISPLAY_GROUP *PNET_DISPLAY_GROUP;

struct DECLSPEC_DRECORD TNET_DISPLAY_GROUP
{
public:
	System::WideChar *grpi3_name;
	System::WideChar *grpi3_comment;
	unsigned grpi3_group_id;
	unsigned grpi3_attributes;
	unsigned grpi3_next_index;
};


struct TACCESS_INFO_0;
typedef TACCESS_INFO_0 *PACCESS_INFO_0;

struct DECLSPEC_DRECORD TACCESS_INFO_0
{
public:
	System::WideChar *acc0_resource_name;
};


struct TACCESS_INFO_1;
typedef TACCESS_INFO_1 *PACCESS_INFO_1;

struct DECLSPEC_DRECORD TACCESS_INFO_1
{
public:
	System::WideChar *acc1_resource_name;
	unsigned acc1_attr;
	unsigned acc1_count;
};


struct TACCESS_INFO_1002;
typedef TACCESS_INFO_1002 *PACCESS_INFO_1002;

struct DECLSPEC_DRECORD TACCESS_INFO_1002
{
public:
	unsigned acc1002_attr;
};


struct TACCESS_LIST;
typedef TACCESS_LIST *PACCESS_LIST;

struct DECLSPEC_DRECORD TACCESS_LIST
{
public:
	System::WideChar *acl_ugname;
	unsigned acl_access;
};


struct TNETLOGON_INFO_1;
typedef TNETLOGON_INFO_1 *PNETLOGON_INFO_1;

struct DECLSPEC_DRECORD TNETLOGON_INFO_1
{
public:
	unsigned netlog1_flags;
	unsigned netlog1_pdc_connection_status;
};


struct TNETLOGON_INFO_2;
typedef TNETLOGON_INFO_2 *PNETLOGON_INFO_2;

struct DECLSPEC_DRECORD TNETLOGON_INFO_2
{
public:
	unsigned netlog2_flags;
	unsigned netlog2_pdc_connection_status;
	System::WideChar *netlog2_trusted_dc_name;
	unsigned netlog2_tc_connection_status;
};


struct TNETLOGON_INFO_3;
typedef TNETLOGON_INFO_3 *PNETLOGON_INFO_3;

struct DECLSPEC_DRECORD TNETLOGON_INFO_3
{
public:
	unsigned netlog3_flags;
	unsigned netlog3_logon_attempts;
	unsigned netlog3_reserved1;
	unsigned netlog3_reserved2;
	unsigned netlog3_reserved3;
	unsigned netlog3_reserved4;
	unsigned netlog3_reserved5;
};


struct TNETLOGON_INFO_4;
typedef TNETLOGON_INFO_4 *PNETLOGON_INFO_4;

struct DECLSPEC_DRECORD TNETLOGON_INFO_4
{
public:
	System::WideChar *netlog4_trusted_dc_name;
	System::WideChar *netlog4_trusted_domain_name;
};


typedef unsigned __stdcall (*TNetAlertRaise)(System::WideChar * AlertEventName, void * Buffer, unsigned BufferSize);

typedef unsigned __stdcall (*TNetAlertRaiseEx)(System::WideChar * AlertEventName, void * VariableInfo, unsigned VariableInfoSize, System::WideChar * ServiceName);

struct TSTD_ALERT;
typedef TSTD_ALERT *PSTD_ALERT;

struct DECLSPEC_DRECORD TSTD_ALERT
{
public:
	unsigned alrt_timestamp;
	System::StaticArray<System::WideChar, 17> alrt_eventname;
	System::StaticArray<System::WideChar, 81> alrt_servicename;
};


struct TADMIN_OTHER_INFO;
typedef TADMIN_OTHER_INFO *PADMIN_OTHER_INFO;

struct DECLSPEC_DRECORD TADMIN_OTHER_INFO
{
public:
	unsigned alrtad_errcode;
	unsigned alrtad_numstrings;
};


struct TERRLOG_OTHER_INFO;
typedef TERRLOG_OTHER_INFO *PERRLOG_OTHER_INFO;

struct DECLSPEC_DRECORD TERRLOG_OTHER_INFO
{
public:
	unsigned alrter_errcode;
	unsigned alrter_offset;
};


struct TPRINT_OTHER_INFO;
typedef TPRINT_OTHER_INFO *PPRINT_OTHER_INFO;

struct DECLSPEC_DRECORD TPRINT_OTHER_INFO
{
public:
	unsigned alrtpr_jobid;
	unsigned alrtpr_status;
	unsigned alrtpr_submitted;
	unsigned alrtpr_size;
};


struct TUSER_OTHER_INFO;
typedef TUSER_OTHER_INFO *PUSER_OTHER_INFO;

struct DECLSPEC_DRECORD TUSER_OTHER_INFO
{
public:
	unsigned alrtus_errcode;
	unsigned alrtus_numstrings;
};


typedef unsigned __stdcall (*TNetShareAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetShareEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetShareEnumSticky)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetShareGetInfo)(System::WideChar * ServerName, System::WideChar * NetName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetShareSetInfo)(System::WideChar * ServerName, System::WideChar * NetName, unsigned Level, void * &Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetShareDel)(System::WideChar * ServerName, System::WideChar * NetName, unsigned Reserved);

typedef unsigned __stdcall (*TNetShareDelSticky)(System::WideChar * ServerName, System::WideChar * NetName, unsigned Reserved);

typedef unsigned __stdcall (*TNetShareCheck)(System::WideChar * ServerName, System::WideChar * Device, unsigned &ShareType);

typedef unsigned __stdcall (*TNetSessionEnum)(System::WideChar * ServerName, System::WideChar * UncClientName, System::WideChar * UserName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetSessionDel)(System::WideChar * ServerName, System::WideChar * UncClientName, System::WideChar * UserName);

typedef unsigned __stdcall (*TNetSessionGetInfo)(System::WideChar * ServerName, System::WideChar * UncClientName, System::WideChar * UserName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetConnectionEnum)(System::WideChar * ServerName, System::WideChar * Qualifier, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetFileClose)(System::WideChar * ServerName, unsigned FileID);

typedef unsigned __stdcall (*TNetFileEnum)(System::WideChar * ServerName, System::WideChar * BasePath, System::WideChar * UserName, unsigned Level, void * &Buffer, unsigned PrefMexLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetFileGetInfo)(System::WideChar * ServerName, unsigned FileID, unsigned Level, void * &Buffer);

struct TShare_Info_0;
typedef TShare_Info_0 *PShare_Info_0;

struct DECLSPEC_DRECORD TShare_Info_0
{
public:
	System::WideChar *shi0_netname;
};


struct TShare_Info_1;
typedef TShare_Info_1 *PShare_Info_1;

struct DECLSPEC_DRECORD TShare_Info_1
{
public:
	System::WideChar *shi1_netname;
	unsigned shi1_type;
	System::WideChar *shi1_remark;
};


struct TShare_Info_2;
typedef TShare_Info_2 *PShare_Info_2;

struct DECLSPEC_DRECORD TShare_Info_2
{
public:
	System::WideChar *shi2_netname;
	unsigned shi2_type;
	System::WideChar *shi2_remark;
	unsigned shi2_permissions;
	unsigned shi2_max_uses;
	unsigned shi2_current_uses;
	System::WideChar *shi2_path;
	System::WideChar *shi2_passwd;
};


struct TShare_Info_501;
typedef TShare_Info_501 *PShare_Info_501;

struct DECLSPEC_DRECORD TShare_Info_501
{
public:
	System::WideChar *shi501_netname;
	unsigned shi501_type;
	System::WideChar *shi501_remark;
	unsigned shi501_flags;
};


struct TShare_Info_502;
typedef TShare_Info_502 *PShare_Info_502;

struct DECLSPEC_DRECORD TShare_Info_502
{
public:
	System::WideChar *shi502_netname;
	unsigned shi502_type;
	System::WideChar *shi502_remark;
	unsigned shi502_permissions;
	unsigned shi502_max_uses;
	unsigned shi502_current_uses;
	System::WideChar *shi502_path;
	System::WideChar *shi502_passwd;
	unsigned shi502_reserved;
	void *shi502_security_descriptor;
};


struct TShare_Info_1004;
typedef TShare_Info_1004 *PShare_Info_1004;

struct DECLSPEC_DRECORD TShare_Info_1004
{
public:
	System::WideChar *shi1004_remark;
};


struct TShare_Info_1005;
typedef TShare_Info_1005 *PShare_Info_1005;

struct DECLSPEC_DRECORD TShare_Info_1005
{
public:
	unsigned shi1005_flags;
};


struct TShare_Info_1006;
typedef TShare_Info_1006 *PShare_Info_1006;

struct DECLSPEC_DRECORD TShare_Info_1006
{
public:
	unsigned shi1006_max_uses;
};


struct TShare_Info_1501;
typedef TShare_Info_1501 *PShare_Info_1501;

struct DECLSPEC_DRECORD TShare_Info_1501
{
public:
	unsigned shi1501_reserved;
	void *shi1501_security_descriptor;
};


struct TSession_Info_0;
typedef TSession_Info_0 *PSession_Info_0;

struct DECLSPEC_DRECORD TSession_Info_0
{
public:
	System::WideChar *sesi0_cname;
};


struct TSession_Info_1;
typedef TSession_Info_1 *PSession_Info_1;

struct DECLSPEC_DRECORD TSession_Info_1
{
public:
	System::WideChar *sesi1_cname;
	System::WideChar *sesi1_username;
	unsigned sesi1_num_opens;
	unsigned sesi1_time;
	unsigned sesi1_idle_time;
	unsigned sesi1_user_flags;
};


struct TSession_Info_2;
typedef TSession_Info_2 *PSession_Info_2;

struct DECLSPEC_DRECORD TSession_Info_2
{
public:
	System::WideChar *sesi2_cname;
	System::WideChar *sesi2_username;
	unsigned sesi2_num_opens;
	unsigned sesi2_time;
	unsigned sesi2_idle_time;
	unsigned sesi2_user_flags;
	System::WideChar *sesi2_cltype_name;
};


struct TSession_Info_10;
typedef TSession_Info_10 *PSession_Info_10;

struct DECLSPEC_DRECORD TSession_Info_10
{
public:
	System::WideChar *sesi10_cname;
	System::WideChar *sesi10_username;
	unsigned sesi10_time;
	unsigned sesi10_idle_time;
};


struct TSession_Info_502;
typedef TSession_Info_502 *PSession_Info_502;

struct DECLSPEC_DRECORD TSession_Info_502
{
public:
	System::WideChar *sesi502_cname;
	System::WideChar *sesi502_username;
	unsigned sesi502_num_opens;
	unsigned sesi502_time;
	unsigned sesi502_idle_time;
	unsigned sesi502_user_flags;
	System::WideChar *sesi502_cltype_name;
	System::WideChar *sesi502_transport;
};


struct TConnection_Info_0;
typedef TConnection_Info_0 *PConnection_Info_0;

struct DECLSPEC_DRECORD TConnection_Info_0
{
public:
	unsigned coni0_id;
};


struct TConnection_Info_1;
typedef TConnection_Info_1 *PConnection_Info_1;

struct DECLSPEC_DRECORD TConnection_Info_1
{
public:
	unsigned coni1_id;
	unsigned coni1_type;
	unsigned coni1_num_opens;
	unsigned coni1_num_users;
	unsigned coni1_time;
	System::WideChar *coni1_username;
	System::WideChar *coni1_netname;
};


struct TFile_Info_2;
typedef TFile_Info_2 *PFile_Info_2;

struct DECLSPEC_DRECORD TFile_Info_2
{
public:
	unsigned fi2_id;
};


struct TFile_Info_3;
typedef TFile_Info_3 *PFile_Info_3;

struct DECLSPEC_DRECORD TFile_Info_3
{
public:
	unsigned fi3_id;
	unsigned fi3_permissions;
	unsigned fi3_num_locks;
	System::WideChar *fi3_pathname;
	System::WideChar *fi3_username;
};


typedef unsigned __stdcall (*TNetMessageNameAdd)(System::WideChar * ServerName, System::WideChar * MsgName);

typedef unsigned __stdcall (*TNetMessageNameEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, PDWORD ResumeHandle);

typedef unsigned __stdcall (*TNetMessageNameGetInfo)(System::WideChar * ServerName, System::WideChar * MsgName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetMessageNameDel)(System::WideChar * ServerName, System::WideChar * MsgName);

typedef unsigned __stdcall (*TNetMessageBufferSend)(System::WideChar * ServerName, System::WideChar * MsgName, System::WideChar * FromName, void * Buffer, unsigned BufferLen);

struct TMSG_INFO_0;
typedef TMSG_INFO_0 *PMSG_INFO_0;

struct DECLSPEC_DRECORD TMSG_INFO_0
{
public:
	System::WideChar *msgi0_name;
};


struct TMSG_INFO_1;
typedef TMSG_INFO_1 *PMSG_INFO_1;

struct DECLSPEC_DRECORD TMSG_INFO_1
{
public:
	System::WideChar *msgi1_name;
	unsigned msgi1_forward_flag;
	System::WideChar *msgi1_forward;
};


typedef unsigned __stdcall (*TNetRemoteTOD)(System::WideChar * UncServerName, void * &Buffer);

typedef unsigned __stdcall (*TNetRemoteComputerSupports)(System::WideChar * UncServerName, unsigned OptionsWanted, unsigned &OptionsSupported);

struct DECLSPEC_DRECORD T_TIME_OF_DAY_INFO
{
public:
	unsigned tod_elapsedt;
	unsigned tod_msecs;
	unsigned tod_hours;
	unsigned tod_mins;
	unsigned tod_secs;
	unsigned tod_hunds;
	int tod_timezone;
	unsigned tod_tinterval;
	unsigned tod_day;
	unsigned tod_month;
	unsigned tod_year;
	unsigned tod_weekday;
};


typedef unsigned __stdcall (*TNetServerEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned ServerType, System::WideChar * Domain, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetServerEnumEx)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned ServerType, System::WideChar * Domain, System::WideChar * FirstNameToReturn);

typedef unsigned __stdcall (*TNetServerGetInfo)(System::WideChar * ServerName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetServerSetInfo)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetServerDiskEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetServerComputerNameAdd)(System::WideChar * ServerName, System::WideChar * EmulatedDomainName, System::WideChar * EmulatedServerName);

typedef unsigned __stdcall (*TNetServerComputerNameDel)(System::WideChar * ServerName, System::WideChar * EmulatedServerName);

typedef unsigned __stdcall (*TNetServerTransportAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer);

typedef unsigned __stdcall (*TNetServerTransportAddEx)(System::WideChar * ServerName, unsigned Level, void * Buffer);

typedef unsigned __stdcall (*TNetServerTransportDel)(System::WideChar * ServerName, unsigned Level, void * Buffer);

typedef unsigned __stdcall (*TNetServerTransportEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

struct DECLSPEC_DRECORD TSERVER_INFO_100
{
public:
	unsigned sv100_platform_id;
	System::WideChar *sv100_name;
};


struct DECLSPEC_DRECORD TSERVER_INFO_101
{
public:
	unsigned sv101_platform_id;
	System::WideChar *sv101_name;
	unsigned sv101_version_major;
	unsigned sv101_version_minor;
	unsigned sv101_type;
	System::WideChar *sv101_comment;
};


struct DECLSPEC_DRECORD TSERVER_INFO_102
{
public:
	unsigned sv102_platform_id;
	System::WideChar *sv102_name;
	unsigned sv102_version_major;
	unsigned sv102_version_minor;
	unsigned sv102_type;
	System::WideChar *sv102_comment;
	unsigned sv102_users;
	int sv102_disc;
	BOOL sv102_hidden;
	unsigned sv102_announce;
	unsigned sv102_anndelta;
	unsigned sv102_licenses;
	System::WideChar *sv102_userpath;
};


struct DECLSPEC_DRECORD TSERVER_INFO_402
{
public:
	unsigned sv402_ulist_mtime;
	unsigned sv402_glist_mtime;
	unsigned sv402_alist_mtime;
	System::WideChar *sv402_alerts;
	unsigned sv402_security;
	unsigned sv402_numadmin;
	unsigned sv402_lanmask;
	System::WideChar *sv402_guestacct;
	unsigned sv402_chdevs;
	unsigned sv402_chdevq;
	unsigned sv402_chdevjobs;
	unsigned sv402_connections;
	unsigned sv402_shares;
	unsigned sv402_openfiles;
	unsigned sv402_sessopens;
	unsigned sv402_sessvcs;
	unsigned sv402_sessreqs;
	unsigned sv402_opensearch;
	unsigned sv402_activelocks;
	unsigned sv402_numreqbuf;
	unsigned sv402_sizreqbuf;
	unsigned sv402_numbigbuf;
	unsigned sv402_numfiletasks;
	unsigned sv402_alertsched;
	unsigned sv402_erroralert;
	unsigned sv402_logonalert;
	unsigned sv402_accessalert;
	unsigned sv402_diskalert;
	unsigned sv402_netioalert;
	unsigned sv402_maxauditsz;
	System::WideChar *sv402_srvheuristics;
};


struct DECLSPEC_DRECORD TSERVER_INFO_403
{
public:
	unsigned sv403_ulist_mtime;
	unsigned sv403_glist_mtime;
	unsigned sv403_alist_mtime;
	System::WideChar *sv403_alerts;
	unsigned sv403_security;
	unsigned sv403_numadmin;
	unsigned sv403_lanmask;
	System::WideChar *sv403_guestacct;
	unsigned sv403_chdevs;
	unsigned sv403_chdevq;
	unsigned sv403_chdevjobs;
	unsigned sv403_connections;
	unsigned sv403_shares;
	unsigned sv403_openfiles;
	unsigned sv403_sessopens;
	unsigned sv403_sessvcs;
	unsigned sv403_sessreqs;
	unsigned sv403_opensearch;
	unsigned sv403_activelocks;
	unsigned sv403_numreqbuf;
	unsigned sv403_sizreqbuf;
	unsigned sv403_numbigbuf;
	unsigned sv403_numfiletasks;
	unsigned sv403_alertsched;
	unsigned sv403_erroralert;
	unsigned sv403_logonalert;
	unsigned sv403_accessalert;
	unsigned sv403_diskalert;
	unsigned sv403_netioalert;
	unsigned sv403_maxauditsz;
	System::WideChar *sv403_srvheuristics;
	unsigned sv403_auditedevents;
	unsigned sv403_autoprofile;
	System::WideChar *sv403_autopath;
};


struct DECLSPEC_DRECORD TSERVER_INFO_502
{
public:
	unsigned sv502_sessopens;
	unsigned sv502_sessvcs;
	unsigned sv502_opensearch;
	unsigned sv502_sizreqbuf;
	unsigned sv502_initworkitems;
	unsigned sv502_maxworkitems;
	unsigned sv502_rawworkitems;
	unsigned sv502_irpstacksize;
	unsigned sv502_maxrawbuflen;
	unsigned sv502_sessusers;
	unsigned sv502_sessconns;
	unsigned sv502_maxpagedmemoryusage;
	unsigned sv502_maxnonpagedmemoryusage;
	BOOL sv502_enablesoftcompat;
	BOOL sv502_enableforcedlogoff;
	BOOL sv502_timesource;
	BOOL sv502_acceptdownlevelapis;
	BOOL sv502_lmannounce;
};


struct DECLSPEC_DRECORD TSERVER_INFO_503
{
public:
	unsigned sv503_sessopens;
	unsigned sv503_sessvcs;
	unsigned sv503_opensearch;
	unsigned sv503_sizreqbuf;
	unsigned sv503_initworkitems;
	unsigned sv503_maxworkitems;
	unsigned sv503_rawworkitems;
	unsigned sv503_irpstacksize;
	unsigned sv503_maxrawbuflen;
	unsigned sv503_sessusers;
	unsigned sv503_sessconns;
	unsigned sv503_maxpagedmemoryusage;
	unsigned sv503_maxnonpagedmemoryusage;
	BOOL sv503_enablesoftcompat;
	BOOL sv503_enableforcedlogoff;
	BOOL sv503_timesource;
	BOOL sv503_acceptdownlevelapis;
	BOOL sv503_lmannounce;
	System::WideChar *sv503_domain;
	unsigned sv503_maxcopyreadlen;
	unsigned sv503_maxcopywritelen;
	unsigned sv503_minkeepsearch;
	unsigned sv503_maxkeepsearch;
	unsigned sv503_minkeepcomplsearch;
	unsigned sv503_maxkeepcomplsearch;
	unsigned sv503_threadcountadd;
	unsigned sv503_numblockthreads;
	unsigned sv503_scavtimeout;
	unsigned sv503_minrcvqueue;
	unsigned sv503_minfreeworkitems;
	unsigned sv503_xactmemsize;
	unsigned sv503_threadpriority;
	unsigned sv503_maxmpxct;
	unsigned sv503_oplockbreakwait;
	unsigned sv503_oplockbreakresponsewait;
	BOOL sv503_enableoplocks;
	BOOL sv503_enableoplockforceclose;
	BOOL sv503_enablefcbopens;
	BOOL sv503_enableraw;
	BOOL sv503_enablesharednetdrives;
	unsigned sv503_minfreeconnections;
	unsigned sv503_maxfreeconnections;
};


struct DECLSPEC_DRECORD TSERVER_INFO_599
{
public:
	unsigned sv599_sessopens;
	unsigned sv599_sessvcs;
	unsigned sv599_opensearch;
	unsigned sv599_sizreqbuf;
	unsigned sv599_initworkitems;
	unsigned sv599_maxworkitems;
	unsigned sv599_rawworkitems;
	unsigned sv599_irpstacksize;
	unsigned sv599_maxrawbuflen;
	unsigned sv599_sessusers;
	unsigned sv599_sessconns;
	unsigned sv599_maxpagedmemoryusage;
	unsigned sv599_maxnonpagedmemoryusage;
	BOOL sv599_enablesoftcompat;
	BOOL sv599_enableforcedlogoff;
	BOOL sv599_timesource;
	BOOL sv599_acceptdownlevelapis;
	BOOL sv599_lmannounce;
	System::WideChar *sv599_domain;
	unsigned sv599_maxcopyreadlen;
	unsigned sv599_maxcopywritelen;
	unsigned sv599_minkeepsearch;
	unsigned sv599_maxkeepsearch;
	unsigned sv599_minkeepcomplsearch;
	unsigned sv599_maxkeepcomplsearch;
	unsigned sv599_threadcountadd;
	unsigned sv599_numblockthreads;
	unsigned sv599_scavtimeout;
	unsigned sv599_minrcvqueue;
	unsigned sv599_minfreeworkitems;
	unsigned sv599_xactmemsize;
	unsigned sv599_threadpriority;
	unsigned sv599_maxmpxct;
	unsigned sv599_oplockbreakwait;
	unsigned sv599_oplockbreakresponsewait;
	BOOL sv599_enableoplocks;
	BOOL sv599_enableoplockforceclose;
	BOOL sv599_enablefcbopens;
	BOOL sv599_enableraw;
	BOOL sv599_enablesharednetdrives;
	unsigned sv599_minfreeconnections;
	unsigned sv599_maxfreeconnections;
	unsigned sv599_initsesstable;
	unsigned sv599_initconntable;
	unsigned sv599_initfiletable;
	unsigned sv599_initsearchtable;
	unsigned sv599_alertschedule;
	unsigned sv599_errorthreshold;
	unsigned sv599_networkerrorthreshold;
	unsigned sv599_diskspacethreshold;
	unsigned sv599_reserved;
	unsigned sv599_maxlinkdelay;
	unsigned sv599_minlinkthroughput;
	unsigned sv599_linkinfovalidtime;
	unsigned sv599_scavqosinfoupdatetime;
	unsigned sv599_maxworkitemidletime;
};


struct DECLSPEC_DRECORD TSERVER_INFO_598
{
public:
	unsigned sv598_maxrawworkitems;
	unsigned sv598_maxthreadsperqueue;
	unsigned sv598_producttype;
	unsigned sv598_serversize;
	unsigned sv598_connectionlessautodisc;
	unsigned sv598_sharingviolationretries;
	unsigned sv598_sharingviolationdelay;
	unsigned sv598_maxglobalopensearch;
	unsigned sv598_removeduplicatesearches;
	unsigned sv598_lockviolationoffset;
	unsigned sv598_lockviolationdelay;
	unsigned sv598_mdlreadswitchover;
	unsigned sv598_cachedopenlimit;
	unsigned sv598_otherqueueaffinity;
	BOOL sv598_restrictnullsessaccess;
	BOOL sv598_enablewfw311directipx;
	unsigned sv598_queuesamplesecs;
	unsigned sv598_balancecount;
	unsigned sv598_preferredaffinity;
	unsigned sv598_maxfreerfcbs;
	unsigned sv598_maxfreemfcbs;
	unsigned sv598_maxfreelfcbs;
	unsigned sv598_maxfreepagedpoolchunks;
	unsigned sv598_minpagedpoolchunksize;
	unsigned sv598_maxpagedpoolchunksize;
	BOOL sv598_sendsfrompreferredprocesso;
	unsigned sv598_cacheddirectorylimit;
	unsigned sv598_maxcopylength;
	BOOL sv598_enablebulktransfer;
	BOOL sv598_enablecompression;
	BOOL sv598_autosharewks;
	BOOL sv598_autoshareserver;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1005
{
public:
	System::WideChar *sv1005_comment;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1107
{
public:
	unsigned sv1107_users;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1010
{
public:
	int sv1010_disc;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1016
{
public:
	BOOL sv1016_hidden;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1017
{
public:
	unsigned sv1017_announce;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1018
{
public:
	unsigned sv1018_anndelta;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1501
{
public:
	unsigned sv1501_sessopens;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1502
{
public:
	unsigned sv1502_sessvcs;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1503
{
public:
	unsigned sv1503_opensearch;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1506
{
public:
	unsigned sv1506_maxworkitems;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1509
{
public:
	unsigned sv1509_maxrawbuflen;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1510
{
public:
	unsigned sv1510_sessusers;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1511
{
public:
	unsigned sv1511_sessconns;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1512
{
public:
	unsigned sv1512_maxnonpagedmemoryusage;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1513
{
public:
	unsigned sv1513_maxpagedmemoryusage;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1514
{
public:
	BOOL sv1514_enablesoftcompat;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1515
{
public:
	BOOL sv1515_enableforcedlogoff;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1516
{
public:
	BOOL sv1516_timesource;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1518
{
public:
	BOOL sv1518_lmannounce;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1520
{
public:
	unsigned sv1520_maxcopyreadlen;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1521
{
public:
	unsigned sv1521_maxcopywritelen;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1522
{
public:
	unsigned sv1522_minkeepsearch;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1523
{
public:
	unsigned sv1523_maxkeepsearch;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1524
{
public:
	unsigned sv1524_minkeepcomplsearch;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1525
{
public:
	unsigned sv1525_maxkeepcomplsearch;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1528
{
public:
	unsigned sv1528_scavtimeout;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1529
{
public:
	unsigned sv1529_minrcvqueue;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1530
{
public:
	unsigned sv1530_minfreeworkitems;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1533
{
public:
	unsigned sv1533_maxmpxct;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1534
{
public:
	unsigned sv1534_oplockbreakwait;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1535
{
public:
	unsigned sv1535_oplockbreakresponsewait;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1536
{
public:
	BOOL sv1536_enableoplocks;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1537
{
public:
	BOOL sv1537_enableoplockforceclose;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1538
{
public:
	BOOL sv1538_enablefcbopens;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1539
{
public:
	BOOL sv1539_enableraw;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1540
{
public:
	BOOL sv1540_enablesharednetdrives;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1541
{
public:
	BOOL sv1541_minfreeconnections;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1542
{
public:
	BOOL sv1542_maxfreeconnections;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1543
{
public:
	unsigned sv1543_initsesstable;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1544
{
public:
	unsigned sv1544_initconntable;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1545
{
public:
	unsigned sv1545_initfiletable;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1546
{
public:
	unsigned sv1546_initsearchtable;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1547
{
public:
	unsigned sv1547_alertschedule;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1548
{
public:
	unsigned sv1548_errorthreshold;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1549
{
public:
	unsigned sv1549_networkerrorthreshold;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1550
{
public:
	unsigned sv1550_diskspacethreshold;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1552
{
public:
	unsigned sv1552_maxlinkdelay;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1553
{
public:
	unsigned sv1553_minlinkthroughput;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1554
{
public:
	unsigned sv1554_linkinfovalidtime;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1555
{
public:
	unsigned sv1555_scavqosinfoupdatetime;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1556
{
public:
	unsigned sv1556_maxworkitemidletime;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1557
{
public:
	unsigned sv1557_maxrawworkitems;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1560
{
public:
	unsigned sv1560_producttype;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1561
{
public:
	unsigned sv1561_serversize;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1562
{
public:
	unsigned sv1562_connectionlessautodisc;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1563
{
public:
	unsigned sv1563_sharingviolationretries;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1564
{
public:
	unsigned sv1564_sharingviolationdelay;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1565
{
public:
	unsigned sv1565_maxglobalopensearch;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1566
{
public:
	BOOL sv1566_removeduplicatesearches;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1567
{
public:
	unsigned sv1567_lockviolationretries;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1568
{
public:
	unsigned sv1568_lockviolationoffset;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1569
{
public:
	unsigned sv1569_lockviolationdelay;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1570
{
public:
	unsigned sv1570_mdlreadswitchover;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1571
{
public:
	unsigned sv1571_cachedopenlimit;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1572
{
public:
	unsigned sv1572_criticalthreads;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1573
{
public:
	unsigned sv1573_restrictnullsessaccess;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1574
{
public:
	unsigned sv1574_enablewfw311directipx;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1575
{
public:
	unsigned sv1575_otherqueueaffinity;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1576
{
public:
	unsigned sv1576_queuesamplesecs;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1577
{
public:
	unsigned sv1577_balancecount;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1578
{
public:
	unsigned sv1578_preferredaffinity;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1579
{
public:
	unsigned sv1579_maxfreerfcbs;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1580
{
public:
	unsigned sv1580_maxfreemfcbs;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1581
{
public:
	unsigned sv1581_maxfreemlcbs;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1582
{
public:
	unsigned sv1582_maxfreepagedpoolchunks;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1583
{
public:
	unsigned sv1583_minpagedpoolchunksize;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1584
{
public:
	unsigned sv1584_maxpagedpoolchunksize;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1585
{
public:
	BOOL sv1585_sendsfrompreferredprocessor;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1586
{
public:
	unsigned sv1586_maxthreadsperqueue;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1587
{
public:
	unsigned sv1587_cacheddirectorylimit;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1588
{
public:
	unsigned sv1588_maxcopylength;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1589
{
public:
	unsigned sv1589_enablebulktransfer;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1590
{
public:
	unsigned sv1590_enablecompression;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1591
{
public:
	unsigned sv1591_autosharewks;
};


struct DECLSPEC_DRECORD TSERVER_INFO_1592
{
public:
	unsigned sv1592_autoshareserver;
};


struct DECLSPEC_DRECORD TSERVER_TRANSPORT_INFO_0
{
public:
	unsigned svti0_numberofvcs;
	char *svti0_transportname;
	void *svti0_transportaddress;
	unsigned svti0_transportaddresslength;
	System::WideChar *svti0_networkaddress;
};


struct DECLSPEC_DRECORD TSERVER_TRANSPORT_INFO_1
{
public:
	unsigned svti1_numberofvcs;
	System::WideChar *svti1_transportname;
	void *svti1_transportaddress;
	unsigned svti1_transportaddresslength;
	System::WideChar *svti1_networkaddress;
	System::WideChar *svti1_domain;
};


typedef unsigned __stdcall (*TNetUseAdd)(System::WideChar * UncServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetUseDel)(System::WideChar * UncServerName, System::WideChar * UseName, unsigned ForceCond);

typedef unsigned __stdcall (*TNetUseEnum)(System::WideChar * UncServerName, unsigned Level, void * &Buffer, unsigned PrefMaxSize, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetUseGetInfo)(System::WideChar * UncServerName, System::WideChar * UseName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetWkstaGetInfo)(System::WideChar * ServerName, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetWkstaSetInfo)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetWkstaUserGetInfo)(System::WideChar * Reserved, unsigned Level, void * &Buffer);

typedef unsigned __stdcall (*TNetWkstaUserSetInfo)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetWkstaUserEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetWkstaTransportAdd)(System::WideChar * ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);

typedef unsigned __stdcall (*TNetWkstaTransportDel)(System::WideChar * ServerName, System::WideChar * TransportName, unsigned Ucond);

typedef unsigned __stdcall (*TNetWkstaTransportEnum)(System::WideChar * ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);

typedef unsigned __stdcall (*TNetApiBufferAllocate)(unsigned ByteCount, void * &Buffer);

typedef unsigned __stdcall (*TNetApiBufferFree)(void * Buffer);

typedef unsigned __stdcall (*TNetApiBufferReallocate)(void * OldBuffer, unsigned NewByteCount, void * &NewBuffer);

typedef unsigned __stdcall (*TNetApiBufferSize)(void * Buffer, unsigned &ByteCount);

typedef unsigned __stdcall (*TNetStatisticsGet)(System::WideChar * Server, System::WideChar * Service, unsigned Level, unsigned Options, void * &Buffer);

typedef Winapi::Windows::PSIDIdentifierAuthority __stdcall (*TGetSidIdentifierAuthority)(void * SID);

typedef PDWORD __stdcall (*TGetSidSubAuthority)(void * SID, unsigned nSubAuthority);

typedef System::PByte __stdcall (*TGetSidSubAuthorityCount)(void * SID);

typedef bool __stdcall (*TLookupAccountName)(System::WideChar * SystemName, System::WideChar * AccountName, void * Sid, unsigned &cbSid, System::WideChar * DomainName, unsigned &cbDomainName, unsigned &peUse);

typedef bool __stdcall (*TLookupAccountSid)(System::WideChar * SystemName, void * Sid, System::WideChar * AccountName, unsigned &AccountLen, System::WideChar * DomainName, void *DomainLen, unsigned &peUse);

struct DECLSPEC_DRECORD TLMWideChar
{
public:
	System::WideChar *Value;
	unsigned Length;
};


typedef System::StaticArray<TNET_DISPLAY_USER, 89478485> TNetDisplayUserArray;

typedef System::StaticArray<TNET_DISPLAY_GROUP, 107374182> TNetDisplayGroupArray;

typedef System::StaticArray<TUSER_INFO_3, 18512790> TUserInfo3Array;

typedef System::StaticArray<TLOCALGROUP_INFO_1, 268435455> TLocalGroupInfo1Array;

typedef System::StaticArray<TLOCALGROUP_USERS_INFO_0, 536870911> TLocalGroupUsersInfo0Array;

typedef System::StaticArray<TLOCALGROUP_MEMBERS_INFO_1, 178956970> TLocalGroupMembersInfo1Array;

typedef System::StaticArray<TLOCALGROUP_MEMBERS_INFO_2, 178956970> TLocalGroupMembersInfo2Array;

typedef System::StaticArray<TGROUP_INFO_1, 268435455> TGroupInfo1Array;

typedef System::StaticArray<TGROUP_USERS_INFO_0, 536870911> TGroupUsersInfo0Array;

typedef System::StaticArray<TShare_Info_1, 178956970> TShareInfo1Array;

typedef System::StaticArray<TMSG_INFO_0, 536870911> TMsgInfo0Array;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 NERR_SUCCESS = System::Int8(0x0);
static const System::Word NERR_BASE = System::Word(0x834);
static const System::Word NERR_NetNotStarted = System::Word(0x836);
static const System::Word NERR_UnknownServer = System::Word(0x837);
static const System::Word NERR_ShareMem = System::Word(0x838);
static const System::Word NERR_NoNetworkResource = System::Word(0x839);
static const System::Word NERR_RemoteOnly = System::Word(0x83a);
static const System::Word NERR_DevNotRedirected = System::Word(0x83b);
static const System::Word NERR_ServerNotStarted = System::Word(0x842);
static const System::Word NERR_ItemNotFound = System::Word(0x843);
static const System::Word NERR_UnknownDevDir = System::Word(0x844);
static const System::Word NERR_RedirectedPath = System::Word(0x845);
static const System::Word NERR_DuplicateShare = System::Word(0x846);
static const System::Word NERR_NoRoom = System::Word(0x847);
static const System::Word NERR_TooManyItems = System::Word(0x849);
static const System::Word NERR_InvalidMaxUsers = System::Word(0x84a);
static const System::Word NERR_BufTooSmall = System::Word(0x84b);
static const System::Word NERR_RemoteErr = System::Word(0x84f);
static const System::Word NERR_LanmanIniError = System::Word(0x853);
static const System::Word NERR_NetworkError = System::Word(0x858);
static const System::Word NERR_WkstaInconsistentState = System::Word(0x859);
static const System::Word NERR_WkstaNotStarted = System::Word(0x85a);
static const System::Word NERR_BrowserNotStarted = System::Word(0x85b);
static const System::Word NERR_InternalError = System::Word(0x85c);
static const System::Word NERR_BadTransactConfig = System::Word(0x85d);
static const System::Word NERR_InvalidAPI = System::Word(0x85e);
static const System::Word NERR_BadEventName = System::Word(0x85f);
static const System::Word NERR_DupNameReboot = System::Word(0x860);
static const System::Word NERR_CfgCompNotFound = System::Word(0x862);
static const System::Word NERR_CfgParamNotFound = System::Word(0x863);
static const System::Word NERR_LineTooLong = System::Word(0x865);
static const System::Word NERR_QNotFound = System::Word(0x866);
static const System::Word NERR_JobNotFound = System::Word(0x867);
static const System::Word NERR_DestNotFound = System::Word(0x868);
static const System::Word NERR_DestExists = System::Word(0x869);
static const System::Word NERR_QExists = System::Word(0x86a);
static const System::Word NERR_QNoRoom = System::Word(0x86b);
static const System::Word NERR_JobNoRoom = System::Word(0x86c);
static const System::Word NERR_DestNoRoom = System::Word(0x86d);
static const System::Word NERR_DestIdle = System::Word(0x86e);
static const System::Word NERR_DestInvalidOp = System::Word(0x86f);
static const System::Word NERR_ProcNoRespond = System::Word(0x870);
static const System::Word NERR_SpoolerNotLoaded = System::Word(0x871);
static const System::Word NERR_DestInvalidState = System::Word(0x872);
static const System::Word NERR_QInvalidState = System::Word(0x873);
static const System::Word NERR_JobInvalidState = System::Word(0x874);
static const System::Word NERR_SpoolNoMemory = System::Word(0x875);
static const System::Word NERR_DriverNotFound = System::Word(0x876);
static const System::Word NERR_DataTypeInvalid = System::Word(0x877);
static const System::Word NERR_ProcNotFound = System::Word(0x878);
static const System::Word NERR_ServiceTableLocked = System::Word(0x884);
static const System::Word NERR_ServiceTableFull = System::Word(0x885);
static const System::Word NERR_ServiceInstalled = System::Word(0x886);
static const System::Word NERR_ServiceEntryLocked = System::Word(0x887);
static const System::Word NERR_ServiceNotInstalled = System::Word(0x888);
static const System::Word NERR_BadServiceName = System::Word(0x889);
static const System::Word NERR_ServiceCtlTimeout = System::Word(0x88a);
static const System::Word NERR_ServiceCtlBusy = System::Word(0x88b);
static const System::Word NERR_BadServiceProgName = System::Word(0x88c);
static const System::Word NERR_ServiceNotCtrl = System::Word(0x88d);
static const System::Word NERR_ServiceKillProc = System::Word(0x88e);
static const System::Word NERR_ServiceCtlNotValid = System::Word(0x88f);
static const System::Word NERR_NotInDispatchTbl = System::Word(0x890);
static const System::Word NERR_BadControlRecv = System::Word(0x891);
static const System::Word NERR_ServiceNotStarting = System::Word(0x892);
static const System::Word NERR_AlreadyLoggedOn = System::Word(0x898);
static const System::Word NERR_NotLoggedOn = System::Word(0x899);
static const System::Word NERR_BadUsername = System::Word(0x89a);
static const System::Word NERR_BadPassword = System::Word(0x89b);
static const System::Word NERR_UnableToAddName_W = System::Word(0x89c);
static const System::Word NERR_UnableToAddName_F = System::Word(0x89d);
static const System::Word NERR_UnableToDelName_W = System::Word(0x89e);
static const System::Word NERR_UnableToDelName_F = System::Word(0x89f);
static const System::Word NERR_LogonsPaused = System::Word(0x8a1);
static const System::Word NERR_LogonServerConflict = System::Word(0x8a2);
static const System::Word NERR_LogonNoUserPath = System::Word(0x8a3);
static const System::Word NERR_LogonScriptError = System::Word(0x8a4);
static const System::Word NERR_StandaloneLogon = System::Word(0x8a6);
static const System::Word NERR_LogonServerNotFound = System::Word(0x8a7);
static const System::Word NERR_LogonDomainExists = System::Word(0x8a8);
static const System::Word NERR_NonValidatedLogon = System::Word(0x8a9);
static const System::Word NERR_ACFNotFound = System::Word(0x8ab);
static const System::Word NERR_GroupNotFound = System::Word(0x8ac);
static const System::Word NERR_UserNotFound = System::Word(0x8ad);
static const System::Word NERR_ResourceNotFound = System::Word(0x8ae);
static const System::Word NERR_GroupExists = System::Word(0x8af);
static const System::Word NERR_UserExists = System::Word(0x8b0);
static const System::Word NERR_ResourceExists = System::Word(0x8b1);
static const System::Word NERR_NotPrimary = System::Word(0x8b2);
static const System::Word NERR_ACFNotLoaded = System::Word(0x8b3);
static const System::Word NERR_ACFNoRoom = System::Word(0x8b4);
static const System::Word NERR_ACFFileIOFail = System::Word(0x8b5);
static const System::Word NERR_ACFTooManyLists = System::Word(0x8b6);
static const System::Word NERR_UserLogon = System::Word(0x8b7);
static const System::Word NERR_ACFNoParent = System::Word(0x8b8);
static const System::Word NERR_CanNotGrowSegment = System::Word(0x8b9);
static const System::Word NERR_SpeGroupOp = System::Word(0x8ba);
static const System::Word NERR_NotInCache = System::Word(0x8bb);
static const System::Word NERR_UserInGroup = System::Word(0x8bc);
static const System::Word NERR_UserNotInGroup = System::Word(0x8bd);
static const System::Word NERR_AccountUndefined = System::Word(0x8be);
static const System::Word NERR_AccountExpired = System::Word(0x8bf);
static const System::Word NERR_InvalidWorkstation = System::Word(0x8c0);
static const System::Word NERR_InvalidLogonHours = System::Word(0x8c1);
static const System::Word NERR_PasswordExpired = System::Word(0x8c2);
static const System::Word NERR_PasswordCantChange = System::Word(0x8c3);
static const System::Word NERR_PasswordHistConflict = System::Word(0x8c4);
static const System::Word NERR_PasswordTooShort = System::Word(0x8c5);
static const System::Word NERR_PasswordTooRecent = System::Word(0x8c6);
static const System::Word NERR_InvalidDatabase = System::Word(0x8c7);
static const System::Word NERR_DatabaseUpToDate = System::Word(0x8c8);
static const System::Word NERR_SyncRequired = System::Word(0x8c9);
static const System::Word NERR_UseNotFound = System::Word(0x8ca);
static const System::Word NERR_BadAsgType = System::Word(0x8cb);
static const System::Word NERR_DeviceIsShared = System::Word(0x8cc);
static const System::Word NERR_NoComputerName = System::Word(0x8de);
static const System::Word NERR_MsgAlreadyStarted = System::Word(0x8df);
static const System::Word NERR_MsgInitFailed = System::Word(0x8e0);
static const System::Word NERR_NameNotFound = System::Word(0x8e1);
static const System::Word NERR_AlreadyForwarded = System::Word(0x8e2);
static const System::Word NERR_AddForwarded = System::Word(0x8e3);
static const System::Word NERR_AlreadyExists = System::Word(0x8e4);
static const System::Word NERR_TooManyNames = System::Word(0x8e5);
static const System::Word NERR_DelComputerName = System::Word(0x8e6);
static const System::Word NERR_LocalForward = System::Word(0x8e7);
static const System::Word NERR_GrpMsgProcessor = System::Word(0x8e8);
static const System::Word NERR_PausedRemote = System::Word(0x8e9);
static const System::Word NERR_BadReceive = System::Word(0x8ea);
static const System::Word NERR_NameInUse = System::Word(0x8eb);
static const System::Word NERR_MsgNotStarted = System::Word(0x8ec);
static const System::Word NERR_NotLocalName = System::Word(0x8ed);
static const System::Word NERR_NoForwardName = System::Word(0x8ee);
static const System::Word NERR_RemoteFull = System::Word(0x8ef);
static const System::Word NERR_NameNotForwarded = System::Word(0x8f0);
static const System::Word NERR_TruncatedBroadcast = System::Word(0x8f1);
static const System::Word NERR_InvalidDevice = System::Word(0x8f6);
static const System::Word NERR_WriteFault = System::Word(0x8f7);
static const System::Word NERR_DuplicateName = System::Word(0x8f9);
static const System::Word NERR_DeleteLater = System::Word(0x8fa);
static const System::Word NERR_IncompleteDel = System::Word(0x8fb);
static const System::Word NERR_MultipleNets = System::Word(0x8fc);
static const System::Word NERR_NetNameNotFound = System::Word(0x906);
static const System::Word NERR_DeviceNotShared = System::Word(0x907);
static const System::Word NERR_ClientNameNotFound = System::Word(0x908);
static const System::Word NERR_FileIdNotFound = System::Word(0x90a);
static const System::Word NERR_ExecFailure = System::Word(0x90b);
static const System::Word NERR_TmpFile = System::Word(0x90c);
static const System::Word NERR_TooMuchData = System::Word(0x90d);
static const System::Word NERR_DeviceShareConflict = System::Word(0x90e);
static const System::Word NERR_BrowserTableIncomplete = System::Word(0x90f);
static const System::Word NERR_NotLocalDomain = System::Word(0x910);
static const System::Word NERR_DevInvalidOpCode = System::Word(0x91b);
static const System::Word NERR_DevNotFound = System::Word(0x91c);
static const System::Word NERR_DevNotOpen = System::Word(0x91d);
static const System::Word NERR_BadQueueDevString = System::Word(0x91e);
static const System::Word NERR_BadQueuePriority = System::Word(0x91f);
static const System::Word NERR_NoCommDevs = System::Word(0x921);
static const System::Word NERR_QueueNotFound = System::Word(0x922);
static const System::Word NERR_BadDevString = System::Word(0x924);
static const System::Word NERR_BadDev = System::Word(0x925);
static const System::Word NERR_InUseBySpooler = System::Word(0x926);
static const System::Word NERR_CommDevInUse = System::Word(0x927);
static const System::Word NERR_InvalidComputer = System::Word(0x92f);
static const System::Word NERR_MaxLenExceeded = System::Word(0x932);
static const System::Word NERR_BadComponent = System::Word(0x934);
static const System::Word NERR_CantType = System::Word(0x935);
static const System::Word NERR_TooManyEntries = System::Word(0x93a);
static const System::Word NERR_ProfileFileTooBig = System::Word(0x942);
static const System::Word NERR_ProfileOffset = System::Word(0x943);
static const System::Word NERR_ProfileCleanup = System::Word(0x944);
static const System::Word NERR_ProfileUnknownCmd = System::Word(0x945);
static const System::Word NERR_ProfileLoadErr = System::Word(0x946);
static const System::Word NERR_ProfileSaveErr = System::Word(0x947);
static const System::Word NERR_LogOverflow = System::Word(0x949);
static const System::Word NERR_LogFileChanged = System::Word(0x94a);
static const System::Word NERR_LogFileCorrupt = System::Word(0x94b);
static const System::Word NERR_SourceIsDir = System::Word(0x94c);
static const System::Word NERR_BadSource = System::Word(0x94d);
static const System::Word NERR_BadDest = System::Word(0x94e);
static const System::Word NERR_DifferentServers = System::Word(0x94f);
static const System::Word NERR_RunSrvPaused = System::Word(0x951);
static const System::Word NERR_ErrCommRunSrv = System::Word(0x955);
static const System::Word NERR_ErrorExecingGhost = System::Word(0x957);
static const System::Word NERR_ShareNotFound = System::Word(0x958);
static const System::Word NERR_InvalidLana = System::Word(0x960);
static const System::Word NERR_OpenFiles = System::Word(0x961);
static const System::Word NERR_ActiveConns = System::Word(0x962);
static const System::Word NERR_BadPasswordCore = System::Word(0x963);
static const System::Word NERR_DevInUse = System::Word(0x964);
static const System::Word NERR_LocalDrive = System::Word(0x965);
static const System::Word NERR_AlertExists = System::Word(0x97e);
static const System::Word NERR_TooManyAlerts = System::Word(0x97f);
static const System::Word NERR_NoSuchAlert = System::Word(0x980);
static const System::Word NERR_BadRecipient = System::Word(0x981);
static const System::Word NERR_AcctLimitExceeded = System::Word(0x982);
static const System::Word NERR_InvalidLogSeek = System::Word(0x988);
static const System::Word NERR_BadUasConfig = System::Word(0x992);
static const System::Word NERR_InvalidUASOp = System::Word(0x993);
static const System::Word NERR_LastAdmin = System::Word(0x994);
static const System::Word NERR_DCNotFound = System::Word(0x995);
static const System::Word NERR_LogonTrackingError = System::Word(0x996);
static const System::Word NERR_NetlogonNotStarted = System::Word(0x997);
static const System::Word NERR_CanNotGrowUASFile = System::Word(0x998);
static const System::Word NERR_TimeDiffAtDC = System::Word(0x999);
static const System::Word NERR_PasswordMismatch = System::Word(0x99a);
static const System::Word NERR_NoSuchServer = System::Word(0x99c);
static const System::Word NERR_NoSuchSession = System::Word(0x99d);
static const System::Word NERR_NoSuchConnection = System::Word(0x99e);
static const System::Word NERR_TooManyServers = System::Word(0x99f);
static const System::Word NERR_TooManySessions = System::Word(0x9a0);
static const System::Word NERR_TooManyConnections = System::Word(0x9a1);
static const System::Word NERR_TooManyFiles = System::Word(0x9a2);
static const System::Word NERR_NoAlternateServers = System::Word(0x9a3);
static const System::Word NERR_TryDownLevel = System::Word(0x9a6);
static const System::Word NERR_UPSDriverNotStarted = System::Word(0x9b0);
static const System::Word NERR_UPSInvalidConfig = System::Word(0x9b1);
static const System::Word NERR_UPSInvalidCommPort = System::Word(0x9b2);
static const System::Word NERR_UPSSignalAsserted = System::Word(0x9b3);
static const System::Word NERR_UPSShutdownFailed = System::Word(0x9b4);
static const System::Word NERR_BadDosRetCode = System::Word(0x9c4);
static const System::Word NERR_ProgNeedsExtraMem = System::Word(0x9c5);
static const System::Word NERR_BadDosFunction = System::Word(0x9c6);
static const System::Word NERR_RemoteBootFailed = System::Word(0x9c7);
static const System::Word NERR_BadFileCheckSum = System::Word(0x9c8);
static const System::Word NERR_NoRplBootSystem = System::Word(0x9c9);
static const System::Word NERR_RplLoadrNetBiosErr = System::Word(0x9ca);
static const System::Word NERR_RplLoadrDiskErr = System::Word(0x9cb);
static const System::Word NERR_ImageParamErr = System::Word(0x9cc);
static const System::Word NERR_TooManyImageParams = System::Word(0x9cd);
static const System::Word NERR_NonDosFloppyUsed = System::Word(0x9ce);
static const System::Word NERR_RplBootRestart = System::Word(0x9cf);
static const System::Word NERR_RplSrvrCallFailed = System::Word(0x9d0);
static const System::Word NERR_CantConnectRplSrvr = System::Word(0x9d1);
static const System::Word NERR_CantOpenImageFile = System::Word(0x9d2);
static const System::Word NERR_CallingRplSrvr = System::Word(0x9d3);
static const System::Word NERR_StartingRplBoot = System::Word(0x9d4);
static const System::Word NERR_RplBootServiceTerm = System::Word(0x9d5);
static const System::Word NERR_RplBootStartFailed = System::Word(0x9d6);
static const System::Word NERR_RPL_CONNECTED = System::Word(0x9d7);
static const System::Word NERR_BrowserConfiguredToNotRun = System::Word(0x9f6);
static const System::Word NERR_RplNoAdaptersStarted = System::Word(0xa32);
static const System::Word NERR_RplBadRegistry = System::Word(0xa33);
static const System::Word NERR_RplBadDatabase = System::Word(0xa34);
static const System::Word NERR_RplRplfilesShare = System::Word(0xa35);
static const System::Word NERR_RplNotRplServer = System::Word(0xa36);
static const System::Word NERR_RplCannotEnum = System::Word(0xa37);
static const System::Word NERR_RplWkstaInfoCorrupted = System::Word(0xa38);
static const System::Word NERR_RplWkstaNotFound = System::Word(0xa39);
static const System::Word NERR_RplWkstaNameUnavailable = System::Word(0xa3a);
static const System::Word NERR_RplProfileInfoCorrupted = System::Word(0xa3b);
static const System::Word NERR_RplProfileNotFound = System::Word(0xa3c);
static const System::Word NERR_RplProfileNameUnavailable = System::Word(0xa3d);
static const System::Word NERR_RplProfileNotEmpty = System::Word(0xa3e);
static const System::Word NERR_RplConfigInfoCorrupted = System::Word(0xa3f);
static const System::Word NERR_RplConfigNotFound = System::Word(0xa40);
static const System::Word NERR_RplAdapterInfoCorrupted = System::Word(0xa41);
static const System::Word NERR_RplInternal = System::Word(0xa42);
static const System::Word NERR_RplVendorInfoCorrupted = System::Word(0xa43);
static const System::Word NERR_RplBootInfoCorrupted = System::Word(0xa44);
static const System::Word NERR_RplWkstaNeedsUserAcct = System::Word(0xa45);
static const System::Word NERR_RplNeedsRPLUSERAcct = System::Word(0xa46);
static const System::Word NERR_RplBootNotFound = System::Word(0xa47);
static const System::Word NERR_RplIncompatibleProfile = System::Word(0xa48);
static const System::Word NERR_RplAdapterNameUnavailable = System::Word(0xa49);
static const System::Word NERR_RplConfigNotEmpty = System::Word(0xa4a);
static const System::Word NERR_RplBootInUse = System::Word(0xa4b);
static const System::Word NERR_RplBackupDatabase = System::Word(0xa4c);
static const System::Word NERR_RplAdapterNotFound = System::Word(0xa4d);
static const System::Word NERR_RplVendorNotFound = System::Word(0xa4e);
static const System::Word NERR_RplVendorNameUnavailable = System::Word(0xa4f);
static const System::Word NERR_RplBootNameUnavailable = System::Word(0xa50);
static const System::Word NERR_RplConfigNameUnavailable = System::Word(0xa51);
static const System::Word NERR_DfsInternalCorruption = System::Word(0xa64);
static const System::Word NERR_DfsVolumeDataCorrupt = System::Word(0xa65);
static const System::Word NERR_DfsNoSuchVolume = System::Word(0xa66);
static const System::Word NERR_DfsVolumeAlreadyExists = System::Word(0xa67);
static const System::Word NERR_DfsAlreadyShared = System::Word(0xa68);
static const System::Word NERR_DfsNoSuchShare = System::Word(0xa69);
static const System::Word NERR_DfsNotALeafVolume = System::Word(0xa6a);
static const System::Word NERR_DfsLeafVolume = System::Word(0xa6b);
static const System::Word NERR_DfsVolumeHasMultipleServers = System::Word(0xa6c);
static const System::Word NERR_DfsCantCreateJunctionPoint = System::Word(0xa6d);
static const System::Word NERR_DfsServerNotDfsAware = System::Word(0xa6e);
static const System::Word NERR_DfsBadRenamePath = System::Word(0xa6f);
static const System::Word NERR_DfsVolumeIsOffline = System::Word(0xa70);
static const System::Word NERR_DfsNoSuchServer = System::Word(0xa71);
static const System::Word NERR_DfsCyclicalName = System::Word(0xa72);
static const System::Word NERR_DfsNotSupportedInServerDfs = System::Word(0xa73);
static const System::Word NERR_DfsInternalError = System::Word(0xa82);
static const System::Word MAX_NERR = System::Word(0xbb7);
static const System::Int8 CNLEN = System::Int8(0xf);
static const System::Int8 LM20_CNLEN = System::Int8(0xf);
static const System::Int8 DNLEN = System::Int8(0xf);
static const System::Int8 LM20_DNLEN = System::Int8(0xf);
static const System::Int8 UNCLEN = System::Int8(0x11);
static const System::Int8 LM20_UNCLEN = System::Int8(0x11);
static const System::Int8 NNLEN = System::Int8(0x50);
static const System::Int8 LM20_NNLEN = System::Int8(0xc);
static const System::Int8 RMLEN = System::Int8(0x62);
static const System::Int8 LM20_RMLEN = System::Int8(0x1e);
static const System::Int8 SNLEN = System::Int8(0x50);
static const System::Int8 LM20_SNLEN = System::Int8(0xf);
static const System::Word STXTLEN = System::Word(0x100);
static const System::Int8 LM20_STXTLEN = System::Int8(0x3f);
static const System::Word PATHLEN = System::Word(0x100);
static const System::Word LM20_PATHLEN = System::Word(0x100);
static const System::Int8 DEVLEN = System::Int8(0x50);
static const System::Int8 LM20_DEVLEN = System::Int8(0x8);
static const System::Int8 EVLEN = System::Int8(0x10);
static const System::Word UNLEN = System::Word(0x100);
static const System::Int8 LM20_UNLEN = System::Int8(0x14);
static const System::Word GNLEN = System::Word(0x100);
static const System::Int8 LM20_GNLEN = System::Int8(0x14);
static const System::Word PWLEN = System::Word(0x100);
static const System::Int8 LM20_PWLEN = System::Int8(0xe);
static const System::Int8 SHPWLEN = System::Int8(0x8);
static const System::Int8 CLTYPE_LEN = System::Int8(0xc);
static const System::Word MAXCOMMENTSZ = System::Word(0x100);
static const System::Int8 LM20_MAXCOMMENTSZ = System::Int8(0x30);
static const System::Int8 QNLEN = System::Int8(0x50);
static const System::Int8 LM20_QNLEN = System::Int8(0xc);
static const System::Byte ALERTSZ = System::Byte(0x80);
static const System::Int8 MAXDEVENTRIES = System::Int8(0x20);
static const System::Int8 NETBIOS_NAME_LEN = System::Int8(0x10);
static const unsigned MAX_PREFERRED_LENGTH = unsigned(0xffffffff);
static const System::Int8 CRYPT_KEY_LEN = System::Int8(0x7);
static const System::Int8 CRYPT_TXT_LEN = System::Int8(0x8);
static const System::Int8 ENCRYPTED_PWLEN = System::Int8(0x10);
static const System::Int8 SESSION_PWLEN = System::Int8(0x18);
static const System::Int8 SESSION_CRYPT_KLEN = System::Int8(0x15);
static const System::Int8 PARMNUM_ALL = System::Int8(0x0);
static const unsigned PARM_ERROR_UNKNOWN = unsigned(0xffffffff);
static const System::Int8 PARM_ERROR_NONE = System::Int8(0x0);
static const System::Word PARMNUM_BASE_INFOLEVEL = System::Word(0x3e8);
#define MESSAGE_FILENAME L"NETMSG"
#define OS2MSG_FILENAME L"BASE"
#define HELP_MSG_FILENAME L"NETH"
#define BACKUP_MSG_FILENAME L"BAK.MSG"
static const System::Word PLATFORM_ID_DOS = System::Word(0x12c);
static const System::Word PLATFORM_ID_OS2 = System::Word(0x190);
static const System::Word PLATFORM_ID_NT = System::Word(0x1f4);
static const System::Word PLATFORM_ID_OSF = System::Word(0x258);
static const System::Word PLATFORM_ID_VMS = System::Word(0x2bc);
static const System::Word MIN_LANMAN_MESSAGE_ID = System::Word(0x834);
static const System::Word MAX_LANMAN_MESSAGE_ID = System::Word(0x16a7);
static const System::Int8 USER_POSIX_ID_PARMNUM = System::Int8(0x1);
static const System::Int8 GROUP_POSIX_ID_PARMNUM = System::Int8(0x1);
static const System::Int8 UF_SCRIPT = System::Int8(0x1);
static const System::Int8 UF_ACCOUNTDISABLE = System::Int8(0x2);
static const System::Int8 UF_HOMEDIR_REQUIRED = System::Int8(0x8);
static const System::Int8 UF_LOCKOUT = System::Int8(0x10);
static const System::Int8 UF_PASSWD_NOTREQD = System::Int8(0x20);
static const System::Int8 UF_PASSWD_CANT_CHANGE = System::Int8(0x40);
static const System::Word UF_TEMP_DUPLICATE_ACCOUNT = System::Word(0x100);
static const System::Word UF_NORMAL_ACCOUNT = System::Word(0x200);
static const System::Word UF_INTERDOMAIN_TRUST_ACCOUNT = System::Word(0x800);
static const System::Word UF_WORKSTATION_TRUST_ACCOUNT = System::Word(0x1000);
static const System::Word UF_SERVER_TRUST_ACCOUNT = System::Word(0x2000);
static const System::Word UF_MACHINE_ACCOUNT_MASK = System::Word(0x3800);
static const System::Word UF_ACCOUNT_TYPE_MASK = System::Word(0x3b00);
static const int UF_DONT_EXPIRE_PASSWD = int(0x10000);
static const int UF_MNS_LOGON_ACCOUNT = int(0x20000);
static const int UF_SETTABLE_BITS = int(0x33b7b);
static const System::Int8 FILTER_TEMP_DUPLICATE_ACCOUNT = System::Int8(0x1);
static const System::Int8 FILTER_NORMAL_ACCOUNT = System::Int8(0x2);
static const System::Int8 FILTER_INTERDOMAIN_TRUST_ACCOUNT = System::Int8(0x8);
static const System::Int8 FILTER_WORKSTATION_TRUST_ACCOUNT = System::Int8(0x10);
static const System::Int8 FILTER_SERVER_TRUST_ACCOUNT = System::Int8(0x20);
static const System::Int8 LG_INCLUDE_INDIRECT = System::Int8(0x1);
static const System::Int8 AF_OP_PRINT = System::Int8(0x1);
static const System::Int8 AF_OP_COMM = System::Int8(0x2);
static const System::Int8 AF_OP_SERVER = System::Int8(0x4);
static const System::Int8 AF_OP_ACCOUNTS = System::Int8(0x8);
static const System::Int8 AF_SETTABLE_BITS = System::Int8(0xf);
static const System::Int8 UAS_ROLE_STANDALONE = System::Int8(0x0);
static const System::Int8 UAS_ROLE_MEMBER = System::Int8(0x1);
static const System::Int8 UAS_ROLE_BACKUP = System::Int8(0x2);
static const System::Int8 UAS_ROLE_PRIMARY = System::Int8(0x3);
static const System::Int8 USER_NAME_PARMNUM = System::Int8(0x1);
static const System::Int8 USER_PASSWORD_PARMNUM = System::Int8(0x3);
static const System::Int8 USER_PASSWORD_AGE_PARMNUM = System::Int8(0x4);
static const System::Int8 USER_PRIV_PARMNUM = System::Int8(0x5);
static const System::Int8 USER_HOME_DIR_PARMNUM = System::Int8(0x6);
static const System::Int8 USER_COMMENT_PARMNUM = System::Int8(0x7);
static const System::Int8 USER_FLAGS_PARMNUM = System::Int8(0x8);
static const System::Int8 USER_SCRIPT_PATH_PARMNUM = System::Int8(0x9);
static const System::Int8 USER_AUTH_FLAGS_PARMNUM = System::Int8(0xa);
static const System::Int8 USER_FULL_NAME_PARMNUM = System::Int8(0xb);
static const System::Int8 USER_USR_COMMENT_PARMNUM = System::Int8(0xc);
static const System::Int8 USER_PARMS_PARMNUM = System::Int8(0xd);
static const System::Int8 USER_WORKSTATIONS_PARMNUM = System::Int8(0xe);
static const System::Int8 USER_LAST_LOGON_PARMNUM = System::Int8(0xf);
static const System::Int8 USER_LAST_LOGOFF_PARMNUM = System::Int8(0x10);
static const System::Int8 USER_ACCT_EXPIRES_PARMNUM = System::Int8(0x11);
static const System::Int8 USER_MAX_STORAGE_PARMNUM = System::Int8(0x12);
static const System::Int8 USER_UNITS_PER_WEEK_PARMNUM = System::Int8(0x13);
static const System::Int8 USER_LOGON_HOURS_PARMNUM = System::Int8(0x14);
static const System::Int8 USER_PAD_PW_COUNT_PARMNUM = System::Int8(0x15);
static const System::Int8 USER_NUM_LOGONS_PARMNUM = System::Int8(0x16);
static const System::Int8 USER_LOGON_SERVER_PARMNUM = System::Int8(0x17);
static const System::Int8 USER_COUNTRY_CODE_PARMNUM = System::Int8(0x18);
static const System::Int8 USER_CODE_PAGE_PARMNUM = System::Int8(0x19);
static const System::Int8 USER_PRIMARY_GROUP_PARMNUM = System::Int8(0x33);
static const System::Int8 USER_PROFILE = System::Int8(0x34);
static const System::Int8 USER_PROFILE_PARMNUM = System::Int8(0x34);
static const System::Int8 USER_HOME_DIR_DRIVE_PARMNUM = System::Int8(0x35);
static const System::Word USER_NAME_INFOLEVEL = System::Word(0x3e9);
static const System::Word USER_PASSWORD_INFOLEVEL = System::Word(0x3eb);
static const System::Word USER_PASSWORD_AGE_INFOLEVEL = System::Word(0x3ec);
static const System::Word USER_PRIV_INFOLEVEL = System::Word(0x3ed);
static const System::Word USER_HOME_DIR_INFOLEVEL = System::Word(0x3ee);
static const System::Word USER_COMMENT_INFOLEVEL = System::Word(0x3ef);
static const System::Word USER_FLAGS_INFOLEVEL = System::Word(0x3f0);
static const System::Word USER_SCRIPT_PATH_INFOLEVEL = System::Word(0x3f1);
static const System::Word USER_AUTH_FLAGS_INFOLEVEL = System::Word(0x3f2);
static const System::Word USER_FULL_NAME_INFOLEVEL = System::Word(0x3f3);
static const System::Word USER_USR_COMMENT_INFOLEVEL = System::Word(0x3f4);
static const System::Word USER_PARMS_INFOLEVEL = System::Word(0x3f5);
static const System::Word USER_WORKSTATIONS_INFOLEVEL = System::Word(0x3f6);
static const System::Word USER_LAST_LOGON_INFOLEVEL = System::Word(0x3f7);
static const System::Word USER_LAST_LOGOFF_INFOLEVEL = System::Word(0x3f8);
static const System::Word USER_ACCT_EXPIRES_INFOLEVEL = System::Word(0x3f9);
static const System::Word USER_MAX_STORAGE_INFOLEVEL = System::Word(0x3fa);
static const System::Word USER_UNITS_PER_WEEK_INFOLEVEL = System::Word(0x3fb);
static const System::Word USER_LOGON_HOURS_INFOLEVEL = System::Word(0x3fc);
static const System::Word USER_PAD_PW_COUNT_INFOLEVEL = System::Word(0x3fd);
static const System::Word USER_NUM_LOGONS_INFOLEVEL = System::Word(0x3fe);
static const System::Word USER_LOGON_SERVER_INFOLEVEL = System::Word(0x3ff);
static const System::Word USER_COUNTRY_CODE_INFOLEVEL = System::Word(0x400);
static const System::Word USER_CODE_PAGE_INFOLEVEL = System::Word(0x401);
static const System::Word USER_PRIMARY_GROUP_INFOLEVEL = System::Word(0x41b);
static const System::Word USER_POSIX_ID_INFOLEVEL = System::Word(0x3e9);
static const System::Word USER_HOME_DIR_DRIVE_INFOLEVEL = System::Word(0x41d);
#define NULL_USERSETINFO_PASSWD L"              "
static const unsigned TIMEQ_FOREVER = unsigned(0xffffffff);
static const unsigned USER_MAXSTORAGE_UNLIMITED = unsigned(0xffffffff);
static const unsigned USER_NO_LOGOFF = unsigned(0xffffffff);
static const System::Int8 UNITS_PER_DAY = System::Int8(0x18);
static const System::Byte UNITS_PER_WEEK = System::Byte(0xa8);
static const System::Int8 USER_PRIV_MASK = System::Int8(0x3);
static const System::Int8 USER_PRIV_GUEST = System::Int8(0x0);
static const System::Int8 USER_PRIV_USER = System::Int8(0x1);
static const System::Int8 USER_PRIV_ADMIN = System::Int8(0x2);
static const System::Word MAX_PASSWD_LEN = System::Word(0x100);
static const System::Int8 DEF_MIN_PWLEN = System::Int8(0x6);
static const System::Int8 DEF_PWUNIQUENESS = System::Int8(0x5);
static const System::Int8 DEF_MAX_PWHIST = System::Int8(0x8);
static const unsigned DEF_MAX_PWAGE = unsigned(0xffffffff);
static const unsigned DEF_MIN_PWAGE = unsigned(0x0);
static const unsigned DEF_FORCE_LOGOFF = unsigned(0xffffffff);
static const System::Int8 DEF_MAX_BADPW = System::Int8(0x0);
static const unsigned ONE_DAY = unsigned(0x15180);
static const System::Int8 VALIDATED_LOGON = System::Int8(0x0);
static const System::Int8 PASSWORD_EXPIRED = System::Int8(0x2);
static const System::Int8 NON_VALIDATED_LOGON = System::Int8(0x3);
static const System::Int8 VALID_LOGOFF = System::Int8(0x1);
static const System::Int8 MODALS_MIN_PASSWD_LEN_PARMNUM = System::Int8(0x1);
static const System::Int8 MODALS_MAX_PASSWD_AGE_PARMNUM = System::Int8(0x2);
static const System::Int8 MODALS_MIN_PASSWD_AGE_PARMNUM = System::Int8(0x3);
static const System::Int8 MODALS_FORCE_LOGOFF_PARMNUM = System::Int8(0x4);
static const System::Int8 MODALS_PASSWD_HIST_LEN_PARMNUM = System::Int8(0x5);
static const System::Int8 MODALS_ROLE_PARMNUM = System::Int8(0x6);
static const System::Int8 MODALS_PRIMARY_PARMNUM = System::Int8(0x7);
static const System::Int8 MODALS_DOMAIN_NAME_PARMNUM = System::Int8(0x8);
static const System::Int8 MODALS_DOMAIN_ID_PARMNUM = System::Int8(0x9);
static const System::Int8 MODALS_LOCKOUT_DURATION_PARMNUM = System::Int8(0xa);
static const System::Int8 MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM = System::Int8(0xb);
static const System::Int8 MODALS_LOCKOUT_THRESHOLD_PARMNUM = System::Int8(0xc);
static const System::Word MODALS_MIN_PASSWD_LEN_INFOLEVEL = System::Word(0x3e9);
static const System::Word MODALS_MAX_PASSWD_AGE_INFOLEVEL = System::Word(0x3ea);
static const System::Word MODALS_MIN_PASSWD_AGE_INFOLEVEL = System::Word(0x3eb);
static const System::Word MODALS_FORCE_LOGOFF_INFOLEVEL = System::Word(0x3ec);
static const System::Word MODALS_PASSWD_HIST_LEN_INFOLEVEL = System::Word(0x3ed);
static const System::Word MODALS_ROLE_INFOLEVEL = System::Word(0x3ee);
static const System::Word MODALS_PRIMARY_INFOLEVEL = System::Word(0x3ef);
static const System::Word MODALS_DOMAIN_NAME_INFOLEVEL = System::Word(0x3f0);
static const System::Word MODALS_DOMAIN_ID_INFOLEVEL = System::Word(0x3f1);
static const System::Word GROUPIDMASK = System::Word(0x8000);
#define GROUP_SPECIALGRP_USERS L"USERS"
#define GROUP_SPECIALGRP_ADMINS L"ADMINS"
#define GROUP_SPECIALGRP_GUESTS L"GUESTS"
#define GROUP_SPECIALGRP_LOCAL L"LOCAL"
static const System::Int8 GROUP_ALL_PARMNUM = System::Int8(0x0);
static const System::Int8 GROUP_NAME_PARMNUM = System::Int8(0x1);
static const System::Int8 GROUP_COMMENT_PARMNUM = System::Int8(0x2);
static const System::Int8 GROUP_ATTRIBUTES_PARMNUM = System::Int8(0x3);
static const System::Word GROUP_ALL_INFOLEVEL = System::Word(0x3e8);
static const System::Word GROUP_NAME_INFOLEVEL = System::Word(0x3e9);
static const System::Word GROUP_COMMENT_INFOLEVEL = System::Word(0x3ea);
static const System::Word GROUP_ATTRIBUTES_INFOLEVEL = System::Word(0x3eb);
static const System::Word GROUP_POSIX_ID_INFOLEVEL = System::Word(0x3e9);
static const System::Int8 LOCALGROUP_NAME_PARMNUM = System::Int8(0x1);
static const System::Int8 LOCALGROUP_COMMENT_PARMNUM = System::Int8(0x2);
static const System::Int8 MAXPERMENTRIES = System::Int8(0x40);
static const System::Int8 ACCESS_NONE = System::Int8(0x0);
static const System::Int8 ACCESS_READ = System::Int8(0x1);
static const System::Int8 ACCESS_WRITE = System::Int8(0x2);
static const System::Int8 ACCESS_CREATE = System::Int8(0x4);
static const System::Int8 ACCESS_EXEC = System::Int8(0x8);
static const System::Int8 ACCESS_DELETE = System::Int8(0x10);
static const System::Int8 ACCESS_ATRIB = System::Int8(0x20);
static const System::Int8 ACCESS_PERM = System::Int8(0x40);
static const System::Int8 ACCESS_ALL = System::Int8(0x7f);
static const System::Word ACCESS_GROUP = System::Word(0x8000);
static const System::Int8 ACCESS_AUDIT = System::Int8(0x1);
static const System::Int8 ACCESS_SUCCESS_OPEN = System::Int8(0x10);
static const System::Int8 ACCESS_SUCCESS_WRITE = System::Int8(0x20);
static const System::Int8 ACCESS_SUCCESS_DELETE = System::Int8(0x40);
static const System::Byte ACCESS_SUCCESS_ACL = System::Byte(0x80);
static const System::Byte ACCESS_SUCCESS_MASK = System::Byte(0xf0);
static const System::Word ACCESS_FAIL_OPEN = System::Word(0x100);
static const System::Word ACCESS_FAIL_WRITE = System::Word(0x200);
static const System::Word ACCESS_FAIL_DELETE = System::Word(0x400);
static const System::Word ACCESS_FAIL_ACL = System::Word(0x800);
static const System::Word ACCESS_FAIL_MASK = System::Word(0xf00);
static const System::Int8 ACCESS_FAIL_SHIFT = System::Int8(0x4);
static const System::Int8 ACCESS_RESOURCE_NAME_PARMNUM = System::Int8(0x1);
static const System::Int8 ACCESS_ATTR_PARMNUM = System::Int8(0x2);
static const System::Int8 ACCESS_COUNT_PARMNUM = System::Int8(0x3);
static const System::Int8 ACCESS_ACCESS_LIST_PARMNUM = System::Int8(0x4);
static const System::Word ACCESS_RESOURCE_NAME_INFOLEVEL = System::Word(0x3e9);
static const System::Word ACCESS_ATTR_INFOLEVEL = System::Word(0x3ea);
static const System::Word ACCESS_COUNT_INFOLEVEL = System::Word(0x3eb);
static const System::Word ACCESS_ACCESS_LIST_INFOLEVEL = System::Word(0x3ec);
#define ACCESS_LETTERS L"RWCXDAP         "
static const System::Int8 NETLOGON_CONTROL_QUERY = System::Int8(0x1);
static const System::Int8 NETLOGON_CONTROL_REPLICATE = System::Int8(0x2);
static const System::Int8 NETLOGON_CONTROL_SYNCHRONIZE = System::Int8(0x3);
static const System::Int8 NETLOGON_CONTROL_PDC_REPLICATE = System::Int8(0x4);
static const System::Int8 NETLOGON_CONTROL_REDISCOVER = System::Int8(0x5);
static const System::Int8 NETLOGON_CONTROL_TC_QUERY = System::Int8(0x6);
static const System::Int8 NETLOGON_CONTROL_TRANSPORT_NOTIFY = System::Int8(0x7);
static const System::Int8 NETLOGON_CONTROL_FIND_USER = System::Int8(0x8);
static const System::Word NETLOGON_CONTROL_UNLOAD_NETLOGON_DLL = System::Word(0xfffb);
static const System::Word NETLOGON_CONTROL_BACKUP_CHANGE_LOG = System::Word(0xfffc);
static const System::Word NETLOGON_CONTROL_TRUNCATE_LOG = System::Word(0xfffd);
static const System::Word NETLOGON_CONTROL_SET_DBFLAG = System::Word(0xfffe);
static const System::Word NETLOGON_CONTROL_BREAKPOINT = System::Word(0xffff);
static const System::Int8 NETLOGON_REPLICATION_NEEDED = System::Int8(0x1);
static const System::Int8 NETLOGON_REPLICATION_IN_PROGRESS = System::Int8(0x2);
static const System::Int8 NETLOGON_FULL_SYNC_REPLICATION = System::Int8(0x4);
static const System::Int8 NETLOGON_REDO_NEEDED = System::Int8(0x8);
#define ALERTER_MAILSLOT L"\\\\.\\MAILSLOT\\Alerter"
#define ALERT_PRINT_EVENT L"PRINTING"
#define ALERT_MESSAGE_EVENT L"MESSAGE"
#define ALERT_ERRORLOG_EVENT L"ERRORLOG"
#define ALERT_ADMIN_EVENT L"ADMIN"
#define ALERT_USER_EVENT L"USER"
static const System::Int8 PRJOB_QSTATUS = System::Int8(0x3);
static const System::Word PRJOB_DEVSTATUS = System::Word(0x1fc);
static const System::Int8 PRJOB_COMPLETE = System::Int8(0x4);
static const System::Int8 PRJOB_INTERV = System::Int8(0x8);
static const System::Int8 PRJOB_ERROR = System::Int8(0x10);
static const System::Int8 PRJOB_DESTOFFLINE = System::Int8(0x20);
static const System::Int8 PRJOB_DESTPAUSED = System::Int8(0x40);
static const System::Byte PRJOB_NOTIFY = System::Byte(0x80);
static const System::Word PRJOB_DESTNOPAPER = System::Word(0x100);
static const System::Word PRJOB_DELETED = System::Word(0x8000);
static const System::Int8 PRJOB_QS_QUEUED = System::Int8(0x0);
static const System::Int8 PRJOB_QS_PAUSED = System::Int8(0x1);
static const System::Int8 PRJOB_QS_SPOOLING = System::Int8(0x2);
static const System::Int8 PRJOB_QS_PRINTING = System::Int8(0x3);
static const System::Int8 SHARE_NETNAME_PARMNUM = System::Int8(0x1);
static const System::Int8 SHARE_TYPE_PARMNUM = System::Int8(0x3);
static const System::Int8 SHARE_REMARK_PARMNUM = System::Int8(0x4);
static const System::Int8 SHARE_PERMISSIONS_PARMNUM = System::Int8(0x5);
static const System::Int8 SHARE_MAX_USES_PARMNUM = System::Int8(0x6);
static const System::Int8 SHARE_CURRENT_USES_PARMNUM = System::Int8(0x7);
static const System::Int8 SHARE_PATH_PARMNUM = System::Int8(0x8);
static const System::Int8 SHARE_PASSWD_PARMNUM = System::Int8(0x9);
static const System::Word SHARE_FILE_SD_PARMNUM = System::Word(0x1f5);
static const System::Word SHARE_REMARK_INFOLEVEL = System::Word(0x3ec);
static const System::Word SHARE_MAX_USES_INFOLEVEL = System::Word(0x3ee);
static const System::Word SHARE_FILE_SD_INFOLEVEL = System::Word(0x5dd);
static const System::Int8 SHI1_NUM_ELEMENTS = System::Int8(0x4);
static const System::Int8 SHI2_NUM_ELEMENTS = System::Int8(0xa);
static const System::Int8 STYPE_DISKTREE = System::Int8(0x0);
static const System::Int8 STYPE_PRINTQ = System::Int8(0x1);
static const System::Int8 STYPE_DEVICE = System::Int8(0x2);
static const System::Int8 STYPE_IPC = System::Int8(0x3);
static const unsigned STYPE_SPECIAL = unsigned(0x80000000);
static const unsigned SHI_USES_UNLIMITED = unsigned(0xffffffff);
static const System::Int8 SHI1005_FLAGS_DFS = System::Int8(0x1);
static const System::Int8 SHI1005_FLAGS_DFS_ROOT = System::Int8(0x2);
static const System::Int8 CSC_MASK = System::Int8(0x30);
static const System::Int8 CSC_CACHE_MANUAL_REINT = System::Int8(0x0);
static const System::Int8 CSC_CACHE_AUTO_REINT = System::Int8(0x10);
static const System::Int8 CSC_CACHE_VDO = System::Int8(0x20);
static const System::Int8 CSC_CACHE_NONE = System::Int8(0x30);
static const System::Int8 SHI1005_VALID_FLAGS_SET = System::Int8(0x30);
static const System::Int8 SESS_GUEST = System::Int8(0x1);
static const System::Int8 SESS_NOENCRYPTION = System::Int8(0x2);
static const System::Int8 SESI1_NUM_ELEMENTS = System::Int8(0x8);
static const System::Int8 SESI2_NUM_ELEMENTS = System::Int8(0x9);
static const System::Int8 PERM_FILE_READ = System::Int8(0x1);
static const System::Int8 PERM_FILE_WRITE = System::Int8(0x2);
static const System::Int8 PERM_FILE_CREATE = System::Int8(0x4);
static const System::Int8 MSGNAME_NOT_FORWARDED = System::Int8(0x0);
static const System::Int8 MSGNAME_FORWARDED_TO = System::Int8(0x4);
static const System::Int8 MSGNAME_FORWARDED_FROM = System::Int8(0x10);
static const System::Int8 SUPPORTS_REMOTE_ADMIN_PROTOCOL = System::Int8(0x2);
static const System::Int8 SUPPORTS_RPC = System::Int8(0x4);
static const System::Int8 SUPPORTS_SAM_PROTOCOL = System::Int8(0x8);
static const System::Int8 SUPPORTS_UNICODE = System::Int8(0x10);
static const System::Int8 SUPPORTS_LOCAL = System::Int8(0x20);
static const unsigned SUPPORTS_ANY = unsigned(0xffffffff);
static const System::Word SV_PLATFORM_ID_OS2 = System::Word(0x190);
static const System::Word SV_PLATFORM_ID_NT = System::Word(0x1f4);
static const System::Int8 MAJOR_VERSION_MASK = System::Int8(0xf);
static const System::Int8 SV_TYPE_WORKSTATION = System::Int8(0x1);
static const System::Int8 SV_TYPE_SERVER = System::Int8(0x2);
static const System::Int8 SV_TYPE_SQLSERVER = System::Int8(0x4);
static const System::Int8 SV_TYPE_DOMAIN_CTRL = System::Int8(0x8);
static const System::Int8 SV_TYPE_DOMAIN_BAKCTRL = System::Int8(0x10);
static const System::Int8 SV_TYPE_TIME_SOURCE = System::Int8(0x20);
static const System::Int8 SV_TYPE_AFP = System::Int8(0x40);
static const System::Byte SV_TYPE_NOVELL = System::Byte(0x80);
static const System::Word SV_TYPE_DOMAIN_MEMBER = System::Word(0x100);
static const System::Word SV_TYPE_PRINTQ_SERVER = System::Word(0x200);
static const System::Word SV_TYPE_DIALIN_SERVER = System::Word(0x400);
static const System::Word SV_TYPE_XENIX_SERVER = System::Word(0x800);
static const System::Word SV_TYPE_SERVER_UNIX = System::Word(0x800);
static const System::Word SV_TYPE_NT = System::Word(0x1000);
static const System::Word SV_TYPE_WFW = System::Word(0x2000);
static const System::Word SV_TYPE_SERVER_MFPN = System::Word(0x4000);
static const System::Word SV_TYPE_SERVER_NT = System::Word(0x8000);
static const int SV_TYPE_POTENTIAL_BROWSER = int(0x10000);
static const int SV_TYPE_BACKUP_BROWSER = int(0x20000);
static const int SV_TYPE_MASTER_BROWSER = int(0x40000);
static const int SV_TYPE_DOMAIN_MASTER = int(0x80000);
static const int SV_TYPE_SERVER_OSF = int(0x100000);
static const int SV_TYPE_SERVER_VMS = int(0x200000);
static const int SV_TYPE_WINDOWS = int(0x400000);
static const int SV_TYPE_DFS = int(0x800000);
static const int SV_TYPE_CLUSTER_NT = int(0x1000000);
static const int SV_TYPE_TERMINALSERVER = int(0x2000000);
static const int SV_TYPE_DCE = int(0x10000000);
static const int SV_TYPE_ALTERNATE_XPORT = int(0x20000000);
static const int SV_TYPE_LOCAL_LIST_ONLY = int(0x40000000);
static const unsigned SV_TYPE_DOMAIN_ENUM = unsigned(0x80000000);
static const unsigned SV_TYPE_ALL = unsigned(0xffffffff);
static const System::Int8 SV_NODISC = System::Int8(-1);
static const System::Int8 SV_USERSECURITY = System::Int8(0x1);
static const System::Int8 SV_SHARESECURITY = System::Int8(0x0);
static const System::Int8 SV_HIDDEN = System::Int8(0x1);
static const System::Int8 SV_VISIBLE = System::Int8(0x0);
static const System::Int8 SV_PLATFORM_ID_PARMNUM = System::Int8(0x65);
static const System::Int8 SV_NAME_PARMNUM = System::Int8(0x66);
static const System::Int8 SV_VERSION_MAJOR_PARMNUM = System::Int8(0x67);
static const System::Int8 SV_VERSION_MINOR_PARMNUM = System::Int8(0x68);
static const System::Int8 SV_TYPE_PARMNUM = System::Int8(0x69);
static const System::Int8 SV_COMMENT_PARMNUM = System::Int8(0x5);
static const System::Int8 SV_USERS_PARMNUM = System::Int8(0x6b);
static const System::Int8 SV_DISC_PARMNUM = System::Int8(0xa);
static const System::Int8 SV_HIDDEN_PARMNUM = System::Int8(0x10);
static const System::Int8 SV_ANNOUNCE_PARMNUM = System::Int8(0x11);
static const System::Int8 SV_ANNDELTA_PARMNUM = System::Int8(0x12);
static const System::Int8 SV_USERPATH_PARMNUM = System::Int8(0x70);
static const System::Word SV_ULIST_MTIME_PARMNUM = System::Word(0x191);
static const System::Word SV_GLIST_MTIME_PARMNUM = System::Word(0x192);
static const System::Word SV_ALIST_MTIME_PARMNUM = System::Word(0x193);
static const System::Int8 SV_ALERTS_PARMNUM = System::Int8(0xb);
static const System::Word SV_SECURITY_PARMNUM = System::Word(0x195);
static const System::Word SV_NUMADMIN_PARMNUM = System::Word(0x196);
static const System::Word SV_LANMASK_PARMNUM = System::Word(0x197);
static const System::Word SV_GUESTACC_PARMNUM = System::Word(0x198);
static const System::Word SV_CHDEVQ_PARMNUM = System::Word(0x19a);
static const System::Word SV_CHDEVJOBS_PARMNUM = System::Word(0x19b);
static const System::Word SV_CONNECTIONS_PARMNUM = System::Word(0x19c);
static const System::Word SV_SHARES_PARMNUM = System::Word(0x19d);
static const System::Word SV_OPENFILES_PARMNUM = System::Word(0x19e);
static const System::Word SV_SESSREQS_PARMNUM = System::Word(0x1a1);
static const System::Word SV_ACTIVELOCKS_PARMNUM = System::Word(0x1a3);
static const System::Word SV_NUMREQBUF_PARMNUM = System::Word(0x1a4);
static const System::Word SV_NUMBIGBUF_PARMNUM = System::Word(0x1a6);
static const System::Word SV_NUMFILETASKS_PARMNUM = System::Word(0x1a7);
static const System::Int8 SV_ALERTSCHED_PARMNUM = System::Int8(0x25);
static const System::Int8 SV_ERRORALERT_PARMNUM = System::Int8(0x26);
static const System::Int8 SV_LOGONALERT_PARMNUM = System::Int8(0x27);
static const System::Int8 SV_ACCESSALERT_PARMNUM = System::Int8(0x28);
static const System::Int8 SV_DISKALERT_PARMNUM = System::Int8(0x29);
static const System::Int8 SV_NETIOALERT_PARMNUM = System::Int8(0x2a);
static const System::Int8 SV_MAXAUDITSZ_PARMNUM = System::Int8(0x2b);
static const System::Word SV_SRVHEURISTICS_PARMNUM = System::Word(0x1af);
static const System::Word SV_SESSOPENS_PARMNUM = System::Word(0x1f5);
static const System::Word SV_SESSVCS_PARMNUM = System::Word(0x1f6);
static const System::Word SV_OPENSEARCH_PARMNUM = System::Word(0x1f7);
static const System::Word SV_SIZREQBUF_PARMNUM = System::Word(0x1f8);
static const System::Word SV_INITWORKITEMS_PARMNUM = System::Word(0x1f9);
static const System::Word SV_MAXWORKITEMS_PARMNUM = System::Word(0x1fa);
static const System::Word SV_RAWWORKITEMS_PARMNUM = System::Word(0x1fb);
static const System::Word SV_IRPSTACKSIZE_PARMNUM = System::Word(0x1fc);
static const System::Word SV_MAXRAWBUFLEN_PARMNUM = System::Word(0x1fd);
static const System::Word SV_SESSUSERS_PARMNUM = System::Word(0x1fe);
static const System::Word SV_SESSCONNS_PARMNUM = System::Word(0x1ff);
static const System::Word SV_MAXNONPAGEDMEMORYUSAGE_PARMNUM = System::Word(0x200);
static const System::Word SV_MAXPAGEDMEMORYUSAGE_PARMNUM = System::Word(0x201);
static const System::Word SV_ENABLESOFTCOMPAT_PARMNUM = System::Word(0x202);
static const System::Word SV_ENABLEFORCEDLOGOFF_PARMNUM = System::Word(0x203);
static const System::Word SV_TIMESOURCE_PARMNUM = System::Word(0x204);
static const System::Word SV_ACCEPTDOWNLEVELAPIS_PARMNUM = System::Word(0x205);
static const System::Word SV_LMANNOUNCE_PARMNUM = System::Word(0x206);
static const System::Word SV_DOMAIN_PARMNUM = System::Word(0x207);
static const System::Word SV_MAXCOPYREADLEN_PARMNUM = System::Word(0x208);
static const System::Word SV_MAXCOPYWRITELEN_PARMNUM = System::Word(0x209);
static const System::Word SV_MINKEEPSEARCH_PARMNUM = System::Word(0x20a);
static const System::Word SV_MAXKEEPSEARCH_PARMNUM = System::Word(0x20b);
static const System::Word SV_MINKEEPCOMPLSEARCH_PARMNUM = System::Word(0x20c);
static const System::Word SV_MAXKEEPCOMPLSEARCH_PARMNUM = System::Word(0x20d);
static const System::Word SV_THREADCOUNTADD_PARMNUM = System::Word(0x20e);
static const System::Word SV_NUMBLOCKTHREADS_PARMNUM = System::Word(0x20f);
static const System::Word SV_SCAVTIMEOUT_PARMNUM = System::Word(0x210);
static const System::Word SV_MINRCVQUEUE_PARMNUM = System::Word(0x211);
static const System::Word SV_MINFREEWORKITEMS_PARMNUM = System::Word(0x212);
static const System::Word SV_XACTMEMSIZE_PARMNUM = System::Word(0x213);
static const System::Word SV_THREADPRIORITY_PARMNUM = System::Word(0x214);
static const System::Word SV_MAXMPXCT_PARMNUM = System::Word(0x215);
static const System::Word SV_OPLOCKBREAKWAIT_PARMNUM = System::Word(0x216);
static const System::Word SV_OPLOCKBREAKRESPONSEWAIT_PARMNUM = System::Word(0x217);
static const System::Word SV_ENABLEOPLOCKS_PARMNUM = System::Word(0x218);
static const System::Word SV_ENABLEOPLOCKFORCECLOSE_PARMNUM = System::Word(0x219);
static const System::Word SV_ENABLEFCBOPENS_PARMNUM = System::Word(0x21a);
static const System::Word SV_ENABLERAW_PARMNUM = System::Word(0x21b);
static const System::Word SV_ENABLESHAREDNETDRIVES_PARMNUM = System::Word(0x21c);
static const System::Word SV_MINFREECONNECTIONS_PARMNUM = System::Word(0x21d);
static const System::Word SV_MAXFREECONNECTIONS_PARMNUM = System::Word(0x21e);
static const System::Word SV_INITSESSTABLE_PARMNUM = System::Word(0x21f);
static const System::Word SV_INITCONNTABLE_PARMNUM = System::Word(0x220);
static const System::Word SV_INITFILETABLE_PARMNUM = System::Word(0x221);
static const System::Word SV_INITSEARCHTABLE_PARMNUM = System::Word(0x222);
static const System::Word SV_ALERTSCHEDULE_PARMNUM = System::Word(0x223);
static const System::Word SV_ERRORTHRESHOLD_PARMNUM = System::Word(0x224);
static const System::Word SV_NETWORKERRORTHRESHOLD_PARMNUM = System::Word(0x225);
static const System::Word SV_DISKSPACETHRESHOLD_PARMNUM = System::Word(0x226);
static const System::Word SV_MAXLINKDELAY_PARMNUM = System::Word(0x228);
static const System::Word SV_MINLINKTHROUGHPUT_PARMNUM = System::Word(0x229);
static const System::Word SV_LINKINFOVALIDTIME_PARMNUM = System::Word(0x22a);
static const System::Word SV_SCAVQOSINFOUPDATETIME_PARMNUM = System::Word(0x22b);
static const System::Word SV_MAXWORKITEMIDLETIME_PARMNUM = System::Word(0x22c);
static const System::Word SV_MAXRAWWORKITEMS_PARMNUM = System::Word(0x22d);
static const System::Word SV_PRODUCTTYPE_PARMNUM = System::Word(0x230);
static const System::Word SV_SERVERSIZE_PARMNUM = System::Word(0x231);
static const System::Word SV_CONNECTIONLESSAUTODISC_PARMNUM = System::Word(0x232);
static const System::Word SV_SHARINGVIOLATIONRETRIES_PARMNUM = System::Word(0x233);
static const System::Word SV_SHARINGVIOLATIONDELAY_PARMNUM = System::Word(0x234);
static const System::Word SV_MAXGLOBALOPENSEARCH_PARMNUM = System::Word(0x235);
static const System::Word SV_REMOVEDUPLICATESEARCHES_PARMNUM = System::Word(0x236);
static const System::Word SV_LOCKVIOLATIONRETRIES_PARMNUM = System::Word(0x237);
static const System::Word SV_LOCKVIOLATIONOFFSET_PARMNUM = System::Word(0x238);
static const System::Word SV_LOCKVIOLATIONDELAY_PARMNUM = System::Word(0x239);
static const System::Word SV_MDLREADSWITCHOVER_PARMNUM = System::Word(0x23a);
static const System::Word SV_CACHEDOPENLIMIT_PARMNUM = System::Word(0x23b);
static const System::Word SV_CRITICALTHREADS_PARMNUM = System::Word(0x23c);
static const System::Word SV_RESTRICTNULLSESSACCESS_PARMNUM = System::Word(0x23d);
static const System::Word SV_ENABLEWFW311DIRECTIPX_PARMNUM = System::Word(0x23e);
static const System::Word SV_OTHERQUEUEAFFINITY_PARMNUM = System::Word(0x23f);
static const System::Word SV_QUEUESAMPLESECS_PARMNUM = System::Word(0x240);
static const System::Word SV_BALANCECOUNT_PARMNUM = System::Word(0x241);
static const System::Word SV_PREFERREDAFFINITY_PARMNUM = System::Word(0x242);
static const System::Word SV_MAXFREERFCBS_PARMNUM = System::Word(0x243);
static const System::Word SV_MAXFREEMFCBS_PARMNUM = System::Word(0x244);
static const System::Word SV_MAXFREELFCBS_PARMNUM = System::Word(0x245);
static const System::Word SV_MAXFREEPAGEDPOOLCHUNKS_PARMNUM = System::Word(0x246);
static const System::Word SV_MINPAGEDPOOLCHUNKSIZE_PARMNUM = System::Word(0x247);
static const System::Word SV_MAXPAGEDPOOLCHUNKSIZE_PARMNUM = System::Word(0x248);
static const System::Word SV_SENDSFROMPREFERREDPROCESSOR_PARMNUM = System::Word(0x249);
static const System::Word SV_MAXTHREADSPERQUEUE_PARMNUM = System::Word(0x24a);
static const System::Word SV_CACHEDDIRECTORYLIMIT_PARMNUM = System::Word(0x24b);
static const System::Word SV_MAXCOPYLENGTH_PARMNUM = System::Word(0x24c);
static const System::Word SV_ENABLEBULKTRANSFER_PARMNUM = System::Word(0x24d);
static const System::Word SV_ENABLECOMPRESSION_PARMNUM = System::Word(0x24e);
static const System::Word SV_AUTOSHAREWKS_PARMNUM = System::Word(0x24f);
static const System::Word SV_AUTOSHARESERVER_PARMNUM = System::Word(0x250);
static const System::Word SV_COMMENT_INFOLEVEL = System::Word(0x3ed);
static const System::Word SV_USERS_INFOLEVEL = System::Word(0x453);
static const System::Word SV_DISC_INFOLEVEL = System::Word(0x3f2);
static const System::Word SV_HIDDEN_INFOLEVEL = System::Word(0x3f8);
static const System::Word SV_ANNOUNCE_INFOLEVEL = System::Word(0x3f9);
static const System::Word SV_ANNDELTA_INFOLEVEL = System::Word(0x3fa);
static const System::Word SV_SESSOPENS_INFOLEVEL = System::Word(0x5dd);
static const System::Word SV_SESSVCS_INFOLEVEL = System::Word(0x5de);
static const System::Word SV_OPENSEARCH_INFOLEVEL = System::Word(0x5df);
static const System::Word SV_MAXWORKITEMS_INFOLEVEL = System::Word(0x5e2);
static const System::Word SV_MAXRAWBUFLEN_INFOLEVEL = System::Word(0x5e5);
static const System::Word SV_SESSUSERS_INFOLEVEL = System::Word(0x5e6);
static const System::Word SV_SESSCONNS_INFOLEVEL = System::Word(0x5e7);
static const System::Word SV_MAXNONPAGEDMEMORYUSAGE_INFOLEVEL = System::Word(0x5e8);
static const System::Word SV_MAXPAGEDMEMORYUSAGE_INFOLEVEL = System::Word(0x5e9);
static const System::Word SV_ENABLESOFTCOMPAT_INFOLEVEL = System::Word(0x5ea);
static const System::Word SV_ENABLEFORCEDLOGOFF_INFOLEVEL = System::Word(0x5eb);
static const System::Word SV_TIMESOURCE_INFOLEVEL = System::Word(0x5ec);
static const System::Word SV_LMANNOUNCE_INFOLEVEL = System::Word(0x5ee);
static const System::Word SV_MAXCOPYREADLEN_INFOLEVEL = System::Word(0x5f0);
static const System::Word SV_MAXCOPYWRITELEN_INFOLEVEL = System::Word(0x5f1);
static const System::Word SV_MINKEEPSEARCH_INFOLEVEL = System::Word(0x5f2);
static const System::Word SV_MAXKEEPSEARCH_INFOLEVEL = System::Word(0x5f3);
static const System::Word SV_MINKEEPCOMPLSEARCH_INFOLEVEL = System::Word(0x5f4);
static const System::Word SV_MAXKEEPCOMPLSEARCH_INFOLEVEL = System::Word(0x5f5);
static const System::Word SV_SCAVTIMEOUT_INFOLEVEL = System::Word(0x5f8);
static const System::Word SV_MINRCVQUEUE_INFOLEVEL = System::Word(0x5f9);
static const System::Word SV_MINFREEWORKITEMS_INFOLEVEL = System::Word(0x5fa);
static const System::Word SV_MAXMPXCT_INFOLEVEL = System::Word(0x5fd);
static const System::Word SV_OPLOCKBREAKWAIT_INFOLEVEL = System::Word(0x5fe);
static const System::Word SV_OPLOCKBREAKRESPONSEWAIT_INFOLEVEL = System::Word(0x5ff);
static const System::Word SV_ENABLEOPLOCKS_INFOLEVEL = System::Word(0x600);
static const System::Word SV_ENABLEOPLOCKFORCECLOSE_INFOLEVEL = System::Word(0x601);
static const System::Word SV_ENABLEFCBOPENS_INFOLEVEL = System::Word(0x602);
static const System::Word SV_ENABLERAW_INFOLEVEL = System::Word(0x603);
static const System::Word SV_ENABLESHAREDNETDRIVES_INFOLEVEL = System::Word(0x604);
static const System::Word SV_MINFREECONNECTIONS_INFOLEVEL = System::Word(0x605);
static const System::Word SV_MAXFREECONNECTIONS_INFOLEVEL = System::Word(0x606);
static const System::Word SV_INITSESSTABLE_INFOLEVEL = System::Word(0x607);
static const System::Word SV_INITCONNTABLE_INFOLEVEL = System::Word(0x608);
static const System::Word SV_INITFILETABLE_INFOLEVEL = System::Word(0x609);
static const System::Word SV_INITSEARCHTABLE_INFOLEVEL = System::Word(0x60a);
static const System::Word SV_ALERTSCHEDULE_INFOLEVEL = System::Word(0x60b);
static const System::Word SV_ERRORTHRESHOLD_INFOLEVEL = System::Word(0x60c);
static const System::Word SV_NETWORKERRORTHRESHOLD_INFOLEVEL = System::Word(0x60d);
static const System::Word SV_DISKSPACETHRESHOLD_INFOLEVEL = System::Word(0x60e);
static const System::Word SV_MAXLINKDELAY_INFOLEVEL = System::Word(0x610);
static const System::Word SV_MINLINKTHROUGHPUT_INFOLEVEL = System::Word(0x611);
static const System::Word SV_LINKINFOVALIDTIME_INFOLEVEL = System::Word(0x612);
static const System::Word SV_SCAVQOSINFOUPDATETIME_INFOLEVEL = System::Word(0x613);
static const System::Word SV_MAXWORKITEMIDLETIME_INFOLEVEL = System::Word(0x614);
static const System::Word SV_MAXRAWWORKITEMS_INFOLOEVEL = System::Word(0x615);
static const System::Word SV_PRODUCTTYPE_INFOLOEVEL = System::Word(0x618);
static const System::Word SV_SERVERSIZE_INFOLOEVEL = System::Word(0x619);
static const System::Word SV_CONNECTIONLESSAUTODISC_INFOLOEVEL = System::Word(0x61a);
static const System::Word SV_SHARINGVIOLATIONRETRIES_INFOLOEVEL = System::Word(0x61b);
static const System::Word SV_SHARINGVIOLATIONDELAY_INFOLOEVEL = System::Word(0x61c);
static const System::Word SV_MAXGLOBALOPENSEARCH_INFOLOEVEL = System::Word(0x61d);
static const System::Word SV_REMOVEDUPLICATESEARCHES_INFOLOEVEL = System::Word(0x61e);
static const System::Word SV_LOCKVIOLATIONRETRIES_INFOLOEVEL = System::Word(0x61f);
static const System::Word SV_LOCKVIOLATIONOFFSET_INFOLOEVEL = System::Word(0x620);
static const System::Word SV_LOCKVIOLATIONDELAY_INFOLOEVEL = System::Word(0x621);
static const System::Word SV_MDLREADSWITCHOVER_INFOLOEVEL = System::Word(0x622);
static const System::Word SV_CACHEDOPENLIMIT_INFOLOEVEL = System::Word(0x623);
static const System::Word SV_CRITICALTHREADS_INFOLOEVEL = System::Word(0x624);
static const System::Word SV_RESTRICTNULLSESSACCESS_INFOLOEVEL = System::Word(0x625);
static const System::Word SV_ENABLEWFW311DIRECTIPX_INFOLOEVEL = System::Word(0x626);
static const System::Word SV_OTHERQUEUEAFFINITY_INFOLEVEL = System::Word(0x627);
static const System::Word SV_QUEUESAMPLESECS_INFOLEVEL = System::Word(0x628);
static const System::Word SV_BALANCECOUNT_INFOLEVEL = System::Word(0x629);
static const System::Word SV_PREFERREDAFFINITY_INFOLEVEL = System::Word(0x62a);
static const System::Word SV_MAXFREERFCBS_INFOLEVEL = System::Word(0x62b);
static const System::Word SV_MAXFREEMFCBS_INFOLEVEL = System::Word(0x62c);
static const System::Word SV_MAXFREELFCBS_INFOLEVEL = System::Word(0x62d);
static const System::Word SV_MAXFREEPAGEDPOOLCHUNKS_INFOLEVEL = System::Word(0x62e);
static const System::Word SV_MINPAGEDPOOLCHUNKSIZE_INFOLEVEL = System::Word(0x62f);
static const System::Word SV_MAXPAGEDPOOLCHUNKSIZE_INFOLEVEL = System::Word(0x630);
static const System::Word SV_SENDSFROMPREFERREDPROCESSOR_INFOLEVEL = System::Word(0x631);
static const System::Word SV_MAXTHREADSPERQUEUE_INFOLEVEL = System::Word(0x632);
static const System::Word SV_CACHEDDIRECTORYLIMIT_INFOLEVEL = System::Word(0x633);
static const System::Word SV_MAXCOPYLENGTH_INFOLEVEL = System::Word(0x634);
static const System::Word SV_ENABLEBULKTRANSFER_INFOLEVEL = System::Word(0x635);
static const System::Word SV_ENABLECOMPRESSION_INFOLEVEL = System::Word(0x636);
static const System::Word SV_AUTOSHAREWKS_INFOLEVEL = System::Word(0x637);
static const System::Word SV_AUTOSHARESERVER_INFOLEVEL = System::Word(0x638);
static const System::Int8 SVI1_NUM_ELEMENTS = System::Int8(0x5);
static const System::Int8 SVI2_NUM_ELEMENTS = System::Int8(0x28);
static const System::Int8 SVI3_NUM_ELEMENTS = System::Int8(0x2c);
static const System::Word SV_MAX_CMD_LEN = System::Word(0x100);
static const System::Int8 SW_AUTOPROF_LOAD_MASK = System::Int8(0x1);
static const System::Int8 SW_AUTOPROF_SAVE_MASK = System::Int8(0x2);
static const System::Int8 SV_MAX_SRV_HEUR_LEN = System::Int8(0x20);
static const System::Int8 SV_USERS_PER_LICENSE = System::Int8(0x5);
extern DELPHI_PACKAGE void __fastcall CvtToWideChar(const System::UnicodeString S, TLMWideChar &WS);
extern DELPHI_PACKAGE void __fastcall CvtToWideCharLM(const System::UnicodeString S, TLMWideChar &WS);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserEnum(System::UnicodeString ServerName, unsigned Level, unsigned Filter, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserGetInfo(System::UnicodeString ServerName, System::UnicodeString UserName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserSetInfo(System::UnicodeString ServerName, System::UnicodeString UserName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserDel(System::UnicodeString ServerName, System::UnicodeString UserName);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserGetGroups(System::UnicodeString ServerName, System::UnicodeString UserName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserSetGroups(System::UnicodeString ServerName, System::UnicodeString UserName, unsigned Level, void * Buffer, unsigned NumEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserGetLocalGroups(System::UnicodeString ServerName, System::UnicodeString UserName, unsigned Level, unsigned Flags, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserModalsGet(System::UnicodeString ServerName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserModalsSet(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetUserChangePassword(System::UnicodeString DomainName, System::UnicodeString UserName, System::UnicodeString OldPassword, System::UnicodeString NewPassword);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupAddUser(System::UnicodeString ServerName, System::UnicodeString GroupName, System::UnicodeString UserName);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupEnum(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupGetInfo(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupSetInfo(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupDel(System::UnicodeString ServerName, System::UnicodeString GroupName);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupDelUser(System::UnicodeString ServerName, System::UnicodeString GroupName, System::UnicodeString UserName);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupGetUsers(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, void * ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetGroupSetUsers(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * Buffer, unsigned &TotalEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupAddMember(System::UnicodeString ServerName, System::UnicodeString GroupName, void * MembersID);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupGetInfo(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupSetInfo(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupDel(System::UnicodeString ServerName, System::UnicodeString GroupName);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupDelMember(System::UnicodeString ServerName, System::UnicodeString GroupName, void * MembersID);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupGetMembers(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, void * ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupSetMembers(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * Buffer, unsigned &TotalEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupAddMembers(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * Buffer, unsigned TotalEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetLocalGroupDelMembers(System::UnicodeString ServerName, System::UnicodeString GroupName, unsigned Level, void * Buffer, unsigned TotalEntries);
extern DELPHI_PACKAGE unsigned __fastcall StNetQueryDisplayInformation(System::UnicodeString ServerName, unsigned Level, unsigned Index, unsigned EntriesRequested, unsigned PrefMaxLen, unsigned &ReturnedCount, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetGetDisplayInformationIndex(System::UnicodeString ServerName, unsigned Level, System::UnicodeString Prefix, unsigned &Index);
extern DELPHI_PACKAGE unsigned __fastcall StNetAccessAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetAccessEnum(System::UnicodeString ServerName, System::UnicodeString BasePath, unsigned Recursive, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetAccessGetInfo(System::UnicodeString ServerName, System::UnicodeString Resource, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetAccessSetInfo(System::UnicodeString ServerName, System::UnicodeString Resource, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetAccessDel(System::UnicodeString ServerName, System::UnicodeString Resource);
extern DELPHI_PACKAGE unsigned __fastcall StNetAccessGetUserPerms(System::UnicodeString ServerName, System::UnicodeString UGname, System::UnicodeString Resource, unsigned &Perms);
extern DELPHI_PACKAGE unsigned __fastcall StNetGetDCName(System::UnicodeString ServerName, System::UnicodeString DomainName, System::UnicodeString &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetGetAnyDCName(System::UnicodeString ServerName, System::UnicodeString DomainName, System::UnicodeString &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StI_NetLogonControl(System::UnicodeString ServerName, unsigned FunctionCode, unsigned QueryLevel, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StI_NetLogonControl2(System::UnicodeString ServerName, unsigned FunctionCode, unsigned QueryLevel, void * Data, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetEnumerateTrustedDomains(System::UnicodeString ServerName, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetAlertRaise(System::UnicodeString AlertEventName, void * Buffer, unsigned BufferSize);
extern DELPHI_PACKAGE unsigned __fastcall StNetAlertRaiseEx(System::UnicodeString AlertEventName, void * VariableInfo, unsigned VariableInfoSize, System::UnicodeString ServiceName);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareEnumSticky(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareGetInfo(System::UnicodeString ServerName, System::UnicodeString NetName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareSetInfo(System::UnicodeString ServerName, System::UnicodeString NetName, unsigned Level, void * &Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareDel(System::UnicodeString ServerName, System::UnicodeString NetName, unsigned Reserved);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareDelSticky(System::UnicodeString ServerName, System::UnicodeString NetName, unsigned Reserved);
extern DELPHI_PACKAGE unsigned __fastcall StNetShareCheck(System::UnicodeString ServerName, System::UnicodeString Device, unsigned &ShareType);
extern DELPHI_PACKAGE unsigned __fastcall StNetSessionEnum(System::UnicodeString ServerName, System::UnicodeString UncClientName, System::UnicodeString UserName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetSessionDel(System::UnicodeString ServerName, System::UnicodeString UncClientName, System::UnicodeString UserName);
extern DELPHI_PACKAGE unsigned __fastcall StNetSessionGetInfo(System::UnicodeString ServerName, System::UnicodeString UncClientName, System::UnicodeString UserName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetConnectionEnum(System::UnicodeString ServerName, System::UnicodeString Qualifier, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetFileClose(System::UnicodeString ServerName, unsigned FileID);
extern DELPHI_PACKAGE unsigned __fastcall StNetFileEnum(System::UnicodeString ServerName, System::UnicodeString BasePath, System::UnicodeString UserName, unsigned Level, void * &Buffer, unsigned PrefMexLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetFileGetInfo(System::UnicodeString ServerName, unsigned FileID, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetMessageNameAdd(System::UnicodeString ServerName, System::UnicodeString MsgName);
extern DELPHI_PACKAGE unsigned __fastcall StNetMessageNameEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, PDWORD ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetMessageNameGetInfo(System::UnicodeString ServerName, System::UnicodeString MsgName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetMessageNameDel(System::UnicodeString ServerName, System::UnicodeString MsgName);
extern DELPHI_PACKAGE unsigned __fastcall StNetMessageBufferSend(System::UnicodeString ServerName, System::UnicodeString MsgName, System::UnicodeString FromName, void * Buffer, unsigned BufferLen);
extern DELPHI_PACKAGE unsigned __fastcall StNetRemoteTOD(System::UnicodeString UncServerName, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetRemoteComputerSupports(System::UnicodeString UncServerName, unsigned OptionsWanted, unsigned &OptionsSupported);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned ServerType, System::UnicodeString Domain, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerEnumEx(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned ServerType, System::UnicodeString Domain, System::UnicodeString FirstNameToReturn);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerGetInfo(System::UnicodeString ServerName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerSetInfo(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerDiskEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerComputerNameAdd(System::UnicodeString ServerName, System::UnicodeString EmulatedDomainName, System::UnicodeString EmulatedServerName);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerComputerNameDel(System::UnicodeString ServerName, System::UnicodeString EmulatedServerName);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerTransportAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerTransportAddEx(System::WideString ServerName, unsigned Level, void * Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerTransportDel(System::UnicodeString ServerName, unsigned Level, void * Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetServerTransportEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetUseAdd(System::UnicodeString UncServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetUseDel(System::UnicodeString UncServerName, System::UnicodeString UseName, unsigned ForceCond);
extern DELPHI_PACKAGE unsigned __fastcall StNetUseEnum(System::UnicodeString UncServerName, unsigned Level, void * &Buffer, unsigned PrefMaxSize, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetUseGetInfo(System::UnicodeString UncServerName, System::UnicodeString UseName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaGetInfo(System::UnicodeString ServerName, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaSetInfo(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaUserGetInfo(System::UnicodeString Reserved, unsigned Level, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaUserSetInfo(System::UnicodeString Reserved, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaUserEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaTransportAdd(System::UnicodeString ServerName, unsigned Level, void * Buffer, unsigned &ParmErr);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaTransportDel(System::UnicodeString ServerName, System::UnicodeString TransportName, unsigned Ucond);
extern DELPHI_PACKAGE unsigned __fastcall StNetWkstaTransportEnum(System::UnicodeString ServerName, unsigned Level, void * &Buffer, unsigned PrefMaxLen, unsigned &EntriesRead, unsigned &TotalEntries, unsigned &ResumeHandle);
extern DELPHI_PACKAGE unsigned __fastcall StNetApiBufferAllocate(unsigned ByteCount, void * &Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetApiBufferFree(void * Buffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetApiBufferReallocate(void * OldBuffer, unsigned NewByteCount, void * &NewBuffer);
extern DELPHI_PACKAGE unsigned __fastcall StNetApiBufferSize(void * Buffer, unsigned &ByteCount);
extern DELPHI_PACKAGE unsigned __fastcall StNetStatisticsGet(System::UnicodeString ServerName, System::UnicodeString ServiceName, unsigned Level, unsigned Options, void * &Buffer);
extern DELPHI_PACKAGE Winapi::Windows::PSIDIdentifierAuthority __fastcall StGetSidIdentifierAuthority(void * Sid);
extern DELPHI_PACKAGE unsigned __fastcall StGetSidSubAuthority(void * Sid, unsigned SubAuthority);
extern DELPHI_PACKAGE System::Byte __fastcall StGetSidSubAuthorityCount(void * SID);
extern DELPHI_PACKAGE bool __fastcall StLookupAccountName(System::UnicodeString SystemName, System::UnicodeString AccountName, void * &Sid, unsigned &SidLength, System::UnicodeString &DomainName, unsigned &peUse);
extern DELPHI_PACKAGE bool __fastcall StLookupAccountSid(System::UnicodeString SystemName, void * Sid, System::UnicodeString &AccountName, System::UnicodeString &DomainName, unsigned &peUse);
}	/* namespace Stnetapi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNETAPI)
using namespace Stnetapi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnetapiHPP
