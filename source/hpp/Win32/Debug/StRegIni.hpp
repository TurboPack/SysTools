// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StRegIni.pas' rev: 30.00 (Windows)

#ifndef StreginiHPP
#define StreginiHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <StStrL.hpp>
#include <StDate.hpp>
#include <StConst.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stregini
{
//-- forward type declarations -----------------------------------------------
struct TQueryKeyInfo;
class DELPHICLASS TStRegIni;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TRegIniType : unsigned char { riIniType, riRegType };

enum DECLSPEC_DENUM TRegIniMode : unsigned char { riSet, riGet };

enum DECLSPEC_DENUM TWinVerType : unsigned char { riWin31, riWin32s, riWin95, riWinNT };

struct DECLSPEC_DRECORD TQueryKeyInfo
{
public:
	HKEY QIKey;
	System::UnicodeString QIClassName;
	unsigned QINumSubKeys;
	unsigned QIMaxSKNLen;
	unsigned QIMaxCNLen;
	unsigned QINumValues;
	unsigned QIMaxVNLen;
	unsigned QIMaxDataLen;
	unsigned QISDescLen;
	_FILETIME QIFileTime;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStRegIni : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TRegIniMode riMode;
	TWinVerType riWinVer;
	TRegIniType riType;
	HKEY riHoldPrimary;
	HKEY riPrimaryKey;
	HKEY riRemoteKey;
	System::WideChar *riCurSubKey;
	System::WideChar *riTrueString;
	System::WideChar *riFalseString;
	_RTL_CRITICAL_SECTION riThreadSafe;
	_SECURITY_ATTRIBUTES __fastcall GetAttributes(void);
	void __fastcall SetAttributes(const _SECURITY_ATTRIBUTES &Value);
	System::UnicodeString __fastcall GetCurSubKey(void);
	void __fastcall SetCurSubKey(System::UnicodeString Value);
	bool __fastcall GetIsIniFile(void);
	void __fastcall ParseIniFile(System::Classes::TStrings* SList);
	System::UnicodeString FCurSubKey;
	_SECURITY_ATTRIBUTES FriSecAttr;
	bool FIsIniFile;
	System::WideChar *riRootName;
	Vcl::Graphics::TBitmap* BmpText;
	Vcl::Graphics::TBitmap* BmpBinary;
	HKEY __fastcall OpenRegKey(void);
	void __fastcall CloseRegKey(const HKEY Key);
	void __fastcall EnterCS(void);
	void __fastcall LeaveCS(void);
	bool __fastcall WriteIniData(const System::UnicodeString ValueName, System::UnicodeString Data);
	int __fastcall ReadIniData(const System::UnicodeString ValueName, System::UnicodeString &Value, System::UnicodeString Default);
	int __fastcall WriteRegData(HKEY Key, const System::UnicodeString ValueName, void * Data, unsigned DType, int Size);
	int __fastcall ReadRegData(HKEY Key, const System::UnicodeString ValueName, System::PByte Data, int Size, unsigned DType);
	
public:
	__fastcall virtual TStRegIni(System::UnicodeString RootName, bool IsIniFile);
	__fastcall virtual ~TStRegIni(void);
	void __fastcall SetPrimary(System::UnicodeString Value);
	System::UnicodeString __fastcall GetPrimary(void);
	int __fastcall GetDataInfo(HKEY Key, const System::UnicodeString ValueName, int &Size, unsigned &DType);
	System::UnicodeString __fastcall BytesToString(System::PByte Value, unsigned Size);
	bool __fastcall StringToBytes(const System::UnicodeString IString, void *Value, unsigned Size);
	System::UnicodeString __fastcall GetFullKeyPath(void);
	void __fastcall WriteBoolean(const System::UnicodeString ValueName, bool Value);
	bool __fastcall ReadBoolean(const System::UnicodeString ValueName, bool Default);
	void __fastcall WriteInteger(const System::UnicodeString ValueName, unsigned Value);
	unsigned __fastcall ReadInteger(const System::UnicodeString ValueName, unsigned Default);
	void __fastcall WriteString(const System::UnicodeString ValueName, const System::UnicodeString Value);
	System::UnicodeString __fastcall ReadString(const System::UnicodeString ValueName, const System::UnicodeString Default);
	void __fastcall WriteBinaryData(const System::UnicodeString ValueName, const void *Value, int Size);
	void __fastcall ReadBinaryData(const System::UnicodeString ValueName, const void *Default, void *Value, int &Size);
	void __fastcall WriteFloat(const System::UnicodeString ValueName, const double Value);
	System::Extended __fastcall ReadFloat(const System::UnicodeString ValueName, const System::Extended Default);
	void __fastcall WriteDate(const System::UnicodeString ValueName, const int Value);
	int __fastcall ReadDate(const System::UnicodeString ValueName, const int Default);
	void __fastcall WriteDateTime(const System::UnicodeString ValueName, const System::TDateTime Value);
	System::TDateTime __fastcall ReadDateTime(const System::UnicodeString ValueName, const System::TDateTime Default);
	void __fastcall WriteTime(const System::UnicodeString ValueName, const int Value);
	int __fastcall ReadTime(const System::UnicodeString ValueName, const int Default);
	void __fastcall CreateKey(const System::UnicodeString KeyName);
	void __fastcall GetSubKeys(System::Classes::TStrings* SK);
	void __fastcall GetValues(System::Classes::TStrings* SKV);
	void __fastcall DeleteKey(const System::UnicodeString KeyName, bool DeleteSubKeys);
	void __fastcall DeleteValue(const System::UnicodeString ValueName);
	void __fastcall QueryKey(TQueryKeyInfo &KeyInfo);
	bool __fastcall KeyExists(System::UnicodeString KeyName);
	bool __fastcall IsKeyEmpty(System::UnicodeString Primary, System::UnicodeString SubKey);
	void __fastcall SaveKey(const System::UnicodeString SubKey, System::UnicodeString FileName);
	void __fastcall LoadKey(const System::UnicodeString SubKey, const System::UnicodeString FileName);
	void __fastcall UnLoadKey(const System::UnicodeString SubKey);
	void __fastcall ReplaceKey(const System::UnicodeString SubKey, const System::UnicodeString InputFile, const System::UnicodeString SaveFile);
	void __fastcall RestoreKey(const System::UnicodeString SubKey, const System::UnicodeString KeyFile, unsigned Options);
	void __fastcall RegOpenRemoteKey(System::UnicodeString CompName);
	void __fastcall RegCloseRemoteKey(void);
	__property _SECURITY_ATTRIBUTES Attributes = {read=GetAttributes, write=SetAttributes};
	__property System::UnicodeString CurSubKey = {read=GetCurSubKey, write=SetCurSubKey};
	__property bool IsIniFile = {read=GetIsIniFile, nodefault};
	void __fastcall RegGetKeySecurity(const System::UnicodeString SubKey, _SECURITY_DESCRIPTOR &SD);
	void __fastcall RegSetKeySecurity(const System::UnicodeString SubKey, const _SECURITY_DESCRIPTOR &SD);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 RI_INVALID_VALUE = System::Int8(-1);
static const System::Int8 RIVOLATILE = System::Int8(0x1);
static const System::Byte ShortBufSize = System::Byte(0xff);
static const System::Word MaxBufSize = System::Word(0x2000);
static const System::Int8 MaxByteArraySize = System::Int8(0x7f);
#define RIMachine L"MACHINE"
#define RIUsers L"USERS"
#define RIRoot L"ROOT"
#define RICUser L"C_USERS"
}	/* namespace Stregini */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STREGINI)
using namespace Stregini;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StreginiHPP
