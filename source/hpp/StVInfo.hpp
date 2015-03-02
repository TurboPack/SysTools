// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StVInfo.pas' rev: 29.00 (Windows)

#ifndef StvinfoHPP
#define StvinfoHPP

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
#include <StBase.hpp>
#include <StConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stvinfo
{
//-- forward type declarations -----------------------------------------------
struct TVerTranslation;
class DELPHICLASS TStCustomVersionInfo;
class DELPHICLASS TStVersionInfo;
//-- type declarations -------------------------------------------------------
typedef TVerTranslation *PVerTranslation;

struct DECLSPEC_DRECORD TVerTranslation
{
public:
	System::Word Language;
	System::Word CharSet;
};


class PASCALIMPLEMENTATION TStCustomVersionInfo : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	System::UnicodeString FComments;
	System::UnicodeString FCompanyName;
	System::UnicodeString FFileDescription;
	System::TDateTime FFileDate;
	unsigned FFileFlags;
	unsigned FFileFlagsMask;
	unsigned FFileMajorVersion;
	unsigned FFileMinorVersion;
	System::UnicodeString FFileName;
	unsigned FFileOS;
	unsigned FFileType;
	unsigned FFileSubtype;
	System::UnicodeString FFileVersion;
	double FFileVersionFloat;
	System::UnicodeString FInternalName;
	int FLanguageCount;
	System::UnicodeString FLanguageName;
	System::UnicodeString FLegalCopyright;
	System::UnicodeString FLegalTrademark;
	System::UnicodeString FOriginalFilename;
	unsigned FProductMajorVersion;
	unsigned FProductMinorVersion;
	System::UnicodeString FProductName;
	System::UnicodeString FProductVersion;
	double FProductVersionFloat;
	int FTranslationValue;
	bool VInfoLoaded;
	System::UnicodeString __fastcall GetComments(void);
	System::UnicodeString __fastcall GetCompanyName(void);
	System::TDateTime __fastcall GetFileDate(void);
	System::UnicodeString __fastcall GetFileDescription(void);
	unsigned __fastcall GetFileFlags(void);
	unsigned __fastcall GetFileFlagsMask(void);
	unsigned __fastcall GetFileMajorVersion(void);
	unsigned __fastcall GetFileMinorVersion(void);
	unsigned __fastcall GetFileOS(void);
	unsigned __fastcall GetFileSubtype(void);
	unsigned __fastcall GetFileType(void);
	System::UnicodeString __fastcall GetFileVersion(void);
	double __fastcall GetFileVersionFloat(void);
	System::UnicodeString __fastcall GetInternalName(void);
	int __fastcall GetLanguageCount(void);
	System::UnicodeString __fastcall GetLanguageName(void);
	System::UnicodeString __fastcall GetLegalCopyright(void);
	System::UnicodeString __fastcall GetLegalTrademark(void);
	System::UnicodeString __fastcall GetOriginalFilename(void);
	unsigned __fastcall GetProductMajorVersion(void);
	unsigned __fastcall GetProductMinorVersion(void);
	System::UnicodeString __fastcall GetProductName(void);
	System::UnicodeString __fastcall GetProductVersion(void);
	double __fastcall GetProductVersionFloat(void);
	int __fastcall GetTranslationValue(void);
	void __fastcall SetFileName(const System::UnicodeString Value);
	System::UnicodeString __fastcall LoadVersionInfo(const System::UnicodeString Key);
	virtual void __fastcall Loaded(void);
	System::Word __fastcall GetFileVerSubPart(int Index);
	System::Word __fastcall GetProdVerSubPart(int Index);
	__property System::UnicodeString Comments = {read=GetComments};
	__property System::UnicodeString CompanyName = {read=GetCompanyName};
	__property System::TDateTime FileDate = {read=GetFileDate};
	__property System::UnicodeString FileDescription = {read=GetFileDescription};
	__property unsigned FileFlags = {read=GetFileFlags, nodefault};
	__property unsigned FileFlagsMask = {read=GetFileFlagsMask, nodefault};
	__property unsigned FileMajorVersion = {read=GetFileMajorVersion, nodefault};
	__property unsigned FileMinorVersion = {read=GetFileMinorVersion, nodefault};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName};
	__property unsigned FileOS = {read=GetFileOS, nodefault};
	__property unsigned FileType = {read=GetFileType, nodefault};
	__property unsigned FileSubtype = {read=GetFileSubtype, nodefault};
	__property System::UnicodeString FileVersion = {read=GetFileVersion};
	__property double FileVersionFloat = {read=GetFileVersionFloat};
	__property System::UnicodeString InternalName = {read=GetInternalName};
	__property int LanguageCount = {read=GetLanguageCount, nodefault};
	__property System::UnicodeString LanguageName = {read=GetLanguageName};
	__property System::UnicodeString LegalCopyright = {read=GetLegalCopyright};
	__property System::UnicodeString LegalTrademark = {read=GetLegalTrademark};
	__property System::UnicodeString OriginalFilename = {read=GetOriginalFilename};
	__property System::UnicodeString ProductName = {read=GetProductName};
	__property unsigned ProductMajorVersion = {read=GetProductMajorVersion, nodefault};
	__property unsigned ProductMinorVersion = {read=GetProductMinorVersion, nodefault};
	__property System::UnicodeString ProductVersion = {read=GetProductVersion};
	__property double ProductVersionFloat = {read=GetProductVersionFloat};
	__property int TranslationValue = {read=GetTranslationValue, nodefault};
	__property System::Word FileVerMajor = {read=GetFileVerSubPart, index=0, nodefault};
	__property System::Word FileVerMinor = {read=GetFileVerSubPart, index=1, nodefault};
	__property System::Word FileVerBuild = {read=GetFileVerSubPart, index=2, nodefault};
	__property System::Word FileVerRelease = {read=GetFileVerSubPart, index=3, nodefault};
	__property System::Word ProductVerMajor = {read=GetProdVerSubPart, index=0, nodefault};
	__property System::Word ProductVerMinor = {read=GetProdVerSubPart, index=1, nodefault};
	__property System::Word ProductVerBuild = {read=GetProdVerSubPart, index=2, nodefault};
	__property System::Word ProductVerRelease = {read=GetProdVerSubPart, index=3, nodefault};
	
public:
	__fastcall virtual TStCustomVersionInfo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStCustomVersionInfo(void);
	System::UnicodeString __fastcall GetKeyValue(const System::UnicodeString Key);
};


class PASCALIMPLEMENTATION TStVersionInfo : public TStCustomVersionInfo
{
	typedef TStCustomVersionInfo inherited;
	
public:
	__property Comments = {default=0};
	__property CompanyName = {default=0};
	__property FileDescription = {default=0};
	__property FileDate = {default=0};
	__property FileFlags;
	__property FileFlagsMask;
	__property FileMajorVersion;
	__property FileMinorVersion;
	__property FileOS;
	__property FileType;
	__property FileSubtype;
	__property FileVersion = {default=0};
	__property FileVersionFloat = {default=0};
	__property InternalName = {default=0};
	__property LanguageCount;
	__property LanguageName = {default=0};
	__property LegalCopyright = {default=0};
	__property LegalTrademark = {default=0};
	__property OriginalFilename = {default=0};
	__property ProductMajorVersion;
	__property ProductMinorVersion;
	__property ProductName = {default=0};
	__property ProductVersion = {default=0};
	__property ProductVersionFloat = {default=0};
	__property TranslationValue;
	__property FileVerMajor;
	__property FileVerMinor;
	__property FileVerBuild;
	__property FileVerRelease;
	__property ProductVerMajor;
	__property ProductVerMinor;
	__property ProductVerBuild;
	__property ProductVerRelease;
	
__published:
	__property FileName = {default=0};
public:
	/* TStCustomVersionInfo.Create */ inline __fastcall virtual TStVersionInfo(System::Classes::TComponent* AOwner) : TStCustomVersionInfo(AOwner) { }
	/* TStCustomVersionInfo.Destroy */ inline __fastcall virtual ~TStVersionInfo(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 STVERMAJOR = System::Int8(0x0);
static const System::Int8 STVERMINOR = System::Int8(0x1);
static const System::Int8 STVERBUILD = System::Int8(0x2);
static const System::Int8 STVERRELEASE = System::Int8(0x3);
}	/* namespace Stvinfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STVINFO)
using namespace Stvinfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StvinfoHPP
