// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StSystem.pas' rev: 32.00 (Windows)

#ifndef StsystemHPP
#define StsystemHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.FileCtrl.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StUtils.hpp>
#include <StDate.hpp>
#include <StStrL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stsystem
{
//-- forward type declarations -----------------------------------------------
struct MediaIDType;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM DiskClass : unsigned char { Floppy360, Floppy720, Floppy12, Floppy144, OtherFloppy, HardDisk, RamDisk, UnknownDisk, InvalidDrive, RemoteDrive, CDRomDisk };

typedef MediaIDType *PMediaIDType;

#pragma pack(push,1)
struct DECLSPEC_DRECORD MediaIDType
{
public:
	System::Word InfoLevel;
	int SerialNumber;
	System::StaticArray<System::WideChar, 11> VolumeLabel;
	System::StaticArray<System::WideChar, 8> FileSystemID;
};
#pragma pack(pop)


typedef bool __fastcall (*TIncludeItemFunc)(const System::Sysutils::TSearchRec &SR, bool ForInclusion, bool &Abort);

//-- var, const, procedure ---------------------------------------------------
static const System::WideChar StPathDelim = (System::WideChar)(0x5c);
static const System::WideChar StPathSep = (System::WideChar)(0x3b);
static const System::WideChar StDriveDelim = (System::WideChar)(0x3a);
static const System::WideChar StDosPathDelim = (System::WideChar)(0x5c);
static const System::WideChar StUnixPathDelim = (System::WideChar)(0x2f);
static const System::WideChar StDosPathSep = (System::WideChar)(0x3b);
static const System::WideChar StUnixPathSep = (System::WideChar)(0x3a);
#define StDosAnyFile L"*.*"
static const System::WideChar StUnixAnyFile = (System::WideChar)(0x2a);
#define StAnyFile L"*.*"
static const System::WideChar StThisDir = (System::WideChar)(0x2e);
#define StParentDir L".."
extern DELPHI_PACKAGE unsigned __fastcall CopyFile(const System::UnicodeString SrcPath, const System::UnicodeString DestPath);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CreateTempFile(const System::UnicodeString aFolder, const System::UnicodeString aPrefix);
extern DELPHI_PACKAGE unsigned __fastcall DeleteVolumeLabel(System::WideChar Drive);
extern DELPHI_PACKAGE void __fastcall EnumerateDirectories(const System::UnicodeString StartDir, System::Classes::TStrings* FL, bool SubDirs, TIncludeItemFunc IncludeItem);
extern DELPHI_PACKAGE void __fastcall EnumerateFiles(const System::UnicodeString StartDir, System::Classes::TStrings* FL, bool SubDirs, TIncludeItemFunc IncludeItem);
extern DELPHI_PACKAGE unsigned __fastcall FileHandlesLeft(unsigned MaxHandles);
extern DELPHI_PACKAGE bool __fastcall FileMatchesMask(const System::UnicodeString FileName, const System::UnicodeString FileMask);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall FileTimeToStDateTime(int FileTime);
extern DELPHI_PACKAGE int __fastcall FindNthSlash(const System::UnicodeString Path, int n);
extern DELPHI_PACKAGE bool __fastcall FlushOsBuffers(int Handle);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetCurrentUser(void);
extern DELPHI_PACKAGE DiskClass __fastcall GetDiskClass(System::WideChar Drive);
extern DELPHI_PACKAGE bool __fastcall GetDiskInfo(System::WideChar Drive, unsigned &ClustersAvailable, unsigned &TotalClusters, unsigned &BytesPerSector, unsigned &SectorsPerCluster);
extern DELPHI_PACKAGE bool __fastcall GetDiskSpace(System::WideChar Drive, System::Comp &UserSpaceAvail, System::Comp &TotalSpaceAvail, System::Comp &DiskSize);
extern DELPHI_PACKAGE System::TDateTime __fastcall GetFileCreateDate(const System::UnicodeString FileName);
extern DELPHI_PACKAGE System::TDateTime __fastcall GetFileLastAccess(const System::UnicodeString FileName);
extern DELPHI_PACKAGE System::TDateTime __fastcall GetFileLastModify(const System::UnicodeString FileName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetHomeFolder(bool aForceSlash);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetLongPath(const System::UnicodeString APath);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetMachineName(void);
extern DELPHI_PACKAGE unsigned __fastcall GetMediaID(System::WideChar Drive, MediaIDType &MediaIDRec);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetParentFolder(const System::UnicodeString APath, bool aForceSlash);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetShortPath(const System::UnicodeString APath);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetSystemFolder(bool aForceSlash);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetTempFolder(bool aForceSlash);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetWindowsFolder(bool aForceSlash);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetWorkingFolder(bool aForceSlash);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall GlobalDateTimeToLocal(const Stdate::TStDateTimeRec UTC, int MinOffset);
extern DELPHI_PACKAGE bool __fastcall IsDirectory(const System::UnicodeString DirName);
extern DELPHI_PACKAGE int __fastcall IsDirectoryEmpty(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall IsDriveReady(System::WideChar Drive);
extern DELPHI_PACKAGE bool __fastcall IsFile(const System::UnicodeString FileName);
extern DELPHI_PACKAGE int __fastcall IsFileArchive(const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall IsFileHidden(const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall IsFileReadOnly(const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall IsFileSystem(const System::UnicodeString S);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall LocalDateTimeToGlobal(const Stdate::TStDateTimeRec DT1, int MinOffset);
extern DELPHI_PACKAGE unsigned __fastcall ReadVolumeLabel(System::UnicodeString &VolName, System::WideChar Drive);
extern DELPHI_PACKAGE bool __fastcall SameFile(const System::UnicodeString FilePath1, const System::UnicodeString FilePath2, int &ErrorCode);
extern DELPHI_PACKAGE unsigned __fastcall SetMediaID(System::WideChar Drive, MediaIDType &MediaIDRec);
extern DELPHI_PACKAGE void __fastcall SplitPath(const System::UnicodeString APath, System::Classes::TStrings* Parts);
extern DELPHI_PACKAGE int __fastcall StDateTimeToFileTime(const Stdate::TStDateTimeRec FileTime);
extern DELPHI_PACKAGE int __fastcall StDateTimeToUnixTime(const Stdate::TStDateTimeRec DT1);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall UnixTimeToStDateTime(int UnixTime);
extern DELPHI_PACKAGE bool __fastcall ValidDrive(System::WideChar Drive);
extern DELPHI_PACKAGE unsigned __fastcall WriteVolumeLabel(const System::UnicodeString VolName, System::WideChar Drive);
}	/* namespace Stsystem */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSYSTEM)
using namespace Stsystem;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StsystemHPP
