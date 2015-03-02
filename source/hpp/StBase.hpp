// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StBase.pas' rev: 29.00 (Windows)

#ifndef StbaseHPP
#define StbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.StdCtrls.hpp>
#include <StConst.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stbase
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EStException;
class DELPHICLASS EStContainerError;
class DELPHICLASS EStSortError;
class DELPHICLASS EStRegIniError;
class DELPHICLASS EStBCDError;
class DELPHICLASS EStStringError;
class DELPHICLASS EStVersionInfoError;
class DELPHICLASS EStNetException;
class DELPHICLASS EStBarCodeError;
class DELPHICLASS EStPNBarCodeError;
class DELPHICLASS EStStatError;
class DELPHICLASS EStFinError;
class DELPHICLASS EStMimeError;
class DELPHICLASS EStToHTMLError;
class DELPHICLASS EStSpawnError;
class DELPHICLASS EStMMFileError;
class DELPHICLASS EStBufStreamError;
class DELPHICLASS EStRegExError;
class DELPHICLASS EStDecMathError;
class DELPHICLASS EStPRNGError;
class DELPHICLASS EStExprError;
class DELPHICLASS TStNode;
class DELPHICLASS TStContainer;
struct TAssignRowData;
class DELPHICLASS TStComponent;
class DELPHICLASS TStBaseEdit;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStLineTerminator : unsigned char { ltNone, ltCR, ltLF, ltCRLF, ltOther };

typedef HWND TStHwnd;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
protected:
	int FErrorCode;
	
public:
	__fastcall EStException(int Ident, System::Word Dummy);
	__fastcall EStException(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy);
	__property int ErrorCode = {read=FErrorCode, write=FErrorCode, nodefault};
public:
	/* Exception.Create */ inline __fastcall EStException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStException(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStException(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStException(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* EStExceptionClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStContainerError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStContainerError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStContainerError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStContainerError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStContainerError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStContainerError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStContainerError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStContainerError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStContainerError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStContainerError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStContainerError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStContainerError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStContainerError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStContainerError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStContainerError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStContainerError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStSortError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStSortError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStSortError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStSortError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStSortError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStSortError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStSortError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStSortError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStSortError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStSortError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStSortError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStSortError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStSortError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStSortError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStSortError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStSortError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStRegIniError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStRegIniError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStRegIniError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStRegIniError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStRegIniError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStRegIniError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStRegIniError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStRegIniError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStRegIniError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStRegIniError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStRegIniError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStRegIniError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStRegIniError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStRegIniError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStRegIniError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStRegIniError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStBCDError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStBCDError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStBCDError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStBCDError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStBCDError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStBCDError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStBCDError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStBCDError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStBCDError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStBCDError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStBCDError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStBCDError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStBCDError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStBCDError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStBCDError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStBCDError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStStringError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStStringError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStStringError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStStringError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStStringError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStStringError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStStringError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStStringError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStStringError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStStringError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStStringError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStStringError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStStringError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStStringError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStStringError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStStringError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStVersionInfoError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStVersionInfoError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStVersionInfoError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStVersionInfoError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStVersionInfoError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStVersionInfoError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStVersionInfoError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStVersionInfoError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStVersionInfoError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStVersionInfoError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStVersionInfoError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStVersionInfoError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStVersionInfoError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStVersionInfoError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStVersionInfoError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStVersionInfoError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStNetException : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStNetException(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStNetException(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStNetException(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStNetException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStNetException(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStNetException(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStNetException(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStNetException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStNetException(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStNetException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStNetException(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStNetException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStNetException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStNetException(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStNetException(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStBarCodeError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStBarCodeError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStBarCodeError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStBarCodeError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStBarCodeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStBarCodeError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStBarCodeError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStBarCodeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStBarCodeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStBarCodeError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStBarCodeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStBarCodeError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStBarCodeError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStBarCodeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStBarCodeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStBarCodeError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStPNBarCodeError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStPNBarCodeError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStPNBarCodeError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStPNBarCodeError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStPNBarCodeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStPNBarCodeError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStPNBarCodeError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStPNBarCodeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStPNBarCodeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStPNBarCodeError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStPNBarCodeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStPNBarCodeError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStPNBarCodeError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStPNBarCodeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStPNBarCodeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStPNBarCodeError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStStatError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStStatError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStStatError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStStatError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStStatError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStStatError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStStatError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStStatError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStStatError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStStatError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStStatError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStStatError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStStatError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStStatError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStStatError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStStatError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStFinError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStFinError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStFinError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStFinError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStFinError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStFinError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStFinError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStFinError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStFinError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStFinError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStFinError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStFinError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStFinError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStFinError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStFinError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStFinError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStMimeError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStMimeError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStMimeError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStMimeError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStMimeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStMimeError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStMimeError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStMimeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStMimeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStMimeError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStMimeError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStMimeError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStMimeError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStMimeError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStMimeError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStMimeError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStToHTMLError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStToHTMLError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStToHTMLError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStToHTMLError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStToHTMLError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStToHTMLError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStToHTMLError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStToHTMLError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStToHTMLError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStToHTMLError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStToHTMLError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStToHTMLError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStToHTMLError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStToHTMLError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStToHTMLError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStToHTMLError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStSpawnError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStSpawnError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStSpawnError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStSpawnError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStSpawnError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStSpawnError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStSpawnError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStSpawnError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStSpawnError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStSpawnError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStSpawnError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStSpawnError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStSpawnError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStSpawnError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStSpawnError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStSpawnError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStMMFileError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStMMFileError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStMMFileError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStMMFileError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStMMFileError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStMMFileError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStMMFileError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStMMFileError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStMMFileError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStMMFileError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStMMFileError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStMMFileError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStMMFileError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStMMFileError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStMMFileError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStMMFileError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStBufStreamError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStBufStreamError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStBufStreamError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStBufStreamError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStBufStreamError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStBufStreamError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStBufStreamError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStBufStreamError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStBufStreamError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStBufStreamError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStBufStreamError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStBufStreamError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStBufStreamError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStBufStreamError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStBufStreamError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStBufStreamError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStRegExError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStRegExError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStRegExError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStRegExError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStRegExError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStRegExError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStRegExError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStRegExError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStRegExError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStRegExError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStRegExError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStRegExError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStRegExError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStRegExError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStRegExError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStRegExError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStDecMathError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStDecMathError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStDecMathError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStDecMathError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStDecMathError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStDecMathError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStDecMathError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStDecMathError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStDecMathError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStDecMathError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStDecMathError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStDecMathError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStDecMathError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStDecMathError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStDecMathError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStDecMathError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStPRNGError : public EStException
{
	typedef EStException inherited;
	
public:
	/* EStException.CreateResTP */ inline __fastcall EStPRNGError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStPRNGError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStPRNGError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStPRNGError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStPRNGError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStPRNGError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStPRNGError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStPRNGError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStPRNGError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStPRNGError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStPRNGError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStPRNGError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStPRNGError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStPRNGError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStPRNGError(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION EStExprError : public EStException
{
	typedef EStException inherited;
	
protected:
	int FErrorCol;
	
public:
	__fastcall EStExprError(int Ident, int Column, int Dummy);
	__property int ErrorColumn = {read=FErrorCol, nodefault};
public:
	/* EStException.CreateResTP */ inline __fastcall EStExprError(int Ident, System::Word Dummy) : EStException(Ident, Dummy) { }
	/* EStException.CreateResFmtTP */ inline __fastcall EStExprError(int Ident, System::TVarRec const *Args, const int Args_High, System::Word Dummy) : EStException(Ident, Args, Args_High, Dummy) { }
	
public:
	/* Exception.Create */ inline __fastcall EStExprError(const System::UnicodeString Msg) : EStException(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EStExprError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High) : EStException(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EStExprError(NativeUInt Ident)/* overload */ : EStException(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EStExprError(System::PResStringRec ResStringRec)/* overload */ : EStException(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EStExprError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EStExprError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High)/* overload */ : EStException(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EStExprError(const System::UnicodeString Msg, int AHelpContext) : EStException(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EStExprError(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_High, int AHelpContext) : EStException(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStExprError(NativeUInt Ident, int AHelpContext)/* overload */ : EStException(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EStExprError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : EStException(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStExprError(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EStExprError(NativeUInt Ident, System::TVarRec const *Args, const int Args_High, int AHelpContext)/* overload */ : EStException(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EStExprError(void) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<char, 260> TSmallArrayA;

typedef System::StaticArray<System::WideChar, 260> TSmallArray;

typedef System::StaticArray<System::Byte, 256> BTable;

typedef System::StaticArray<System::Byte, 65536> BTableU;

typedef double *PDouble;

typedef System::StaticArray<double, 268435455> TDoubleArray;

typedef TDoubleArray *PDoubleArray;

typedef System::StaticArray<int, 536870911> TIntArray;

typedef TIntArray *PIntArray;

typedef System::Extended TStFloat;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNode : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	void *FData;
	
public:
	__fastcall virtual TStNode(void * AData);
	__property void * Data = {read=FData, write=FData};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TStNode(void) { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TStNodeClass;

typedef int __fastcall (*TCompareFunc)(void * Data1, void * Data2);

typedef void __fastcall (__closure *TStCompareEvent)(System::TObject* Sender, void * Data1, void * Data2, int &Compare);

typedef void __fastcall (*TDisposeDataProc)(void * Data);

typedef void __fastcall (__closure *TStDisposeDataEvent)(System::TObject* Sender, void * Data);

typedef void * __fastcall (*TLoadDataFunc)(System::Classes::TReader* Reader);

typedef void __fastcall (__closure *TStLoadDataEvent)(System::TObject* Sender, System::Classes::TReader* Reader, void * &Data);

typedef void __fastcall (*TStoreDataProc)(System::Classes::TWriter* Writer, void * Data);

typedef void __fastcall (__closure *TStStoreDataEvent)(System::TObject* Sender, System::Classes::TWriter* Writer, void * Data);

typedef int __fastcall (*TStringCompareFunc)(const System::UnicodeString String1, const System::UnicodeString String2);

typedef void __fastcall (__closure *TStStringCompareEvent)(System::TObject* Sender, const System::UnicodeString String1, const System::UnicodeString String2, int &Compare);

typedef int __fastcall (*TUntypedCompareFunc)(const void *El1, const void *El2);

typedef void __fastcall (__closure *TStUntypedCompareEvent)(System::TObject* Sender, const void *El1, const void *El2, int &Compare);

typedef bool __fastcall (*TIterateFunc)(TStContainer* Container, TStNode* Node, void * OtherData);

typedef bool __fastcall (*TIteratePointerFunc)(TStContainer* Container, void * Data, void * OtherData);

typedef bool __fastcall (*TIterateUntypedFunc)(TStContainer* Container, void *Data, void * OtherData);

class PASCALIMPLEMENTATION TStContainer : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	TCompareFunc FCompare;
	TDisposeDataProc FDisposeData;
	TLoadDataFunc FLoadData;
	TStoreDataProc FStoreData;
	TStCompareEvent FOnCompare;
	TStDisposeDataEvent FOnDisposeData;
	TStLoadDataEvent FOnLoadData;
	TStStoreDataEvent FOnStoreData;
	_RTL_CRITICAL_SECTION conThreadSafe;
	void __fastcall SetCompare(TCompareFunc C);
	void __fastcall SetDisposeData(TDisposeDataProc D);
	void __fastcall SetLoadData(TLoadDataFunc L);
	void __fastcall SetStoreData(TStoreDataProc S);
	TStNodeClass conNodeClass;
	int conNodeProt;
	int FCount;
	bool __fastcall AssignPointers(System::Classes::TPersistent* Source, TIteratePointerFunc AssignData);
	bool __fastcall AssignUntypedVars(System::Classes::TPersistent* Source, TIterateUntypedFunc AssignData);
	virtual void __fastcall ForEachPointer(TIteratePointerFunc Action, void * OtherData);
	virtual void __fastcall ForEachUntypedVar(TIterateUntypedFunc Action, void * OtherData);
	virtual void __fastcall GetArraySizes(unsigned &RowCount, unsigned &ColCount, unsigned &ElSize);
	virtual void __fastcall SetArraySizes(unsigned RowCount, unsigned ColCount, unsigned ElSize);
	virtual bool __fastcall StoresPointers(void);
	virtual bool __fastcall StoresUntypedVars(void);
	void __fastcall IncNodeProtection(void);
	void __fastcall DecNodeProtection(void);
	void __fastcall EnterCS(void);
	void __fastcall LeaveCS(void);
	
public:
	__fastcall TStContainer(TStNodeClass NodeClass, int Dummy);
	__fastcall virtual ~TStContainer(void);
	virtual void __fastcall Clear(void) = 0 ;
	void __fastcall DisposeNodeData(TStNode* P);
	virtual int __fastcall DoCompare(void * Data1, void * Data2);
	virtual void __fastcall DoDisposeData(void * Data);
	virtual void * __fastcall DoLoadData(System::Classes::TReader* Reader);
	virtual void __fastcall DoStoreData(System::Classes::TWriter* Writer, void * Data);
	DYNAMIC void __fastcall LoadFromFile(const System::UnicodeString FileName);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S) = 0 ;
	DYNAMIC void __fastcall StoreToFile(const System::UnicodeString FileName);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S) = 0 ;
	__property int Count = {read=FCount, nodefault};
	__property TCompareFunc Compare = {read=FCompare, write=SetCompare};
	__property TDisposeDataProc DisposeData = {read=FDisposeData, write=SetDisposeData};
	__property TLoadDataFunc LoadData = {read=FLoadData, write=SetLoadData};
	__property TStoreDataProc StoreData = {read=FStoreData, write=SetStoreData};
	__property TStCompareEvent OnCompare = {read=FOnCompare, write=FOnCompare};
	__property TStDisposeDataEvent OnDisposeData = {read=FOnDisposeData, write=FOnDisposeData};
	__property TStLoadDataEvent OnLoadData = {read=FOnLoadData, write=FOnLoadData};
	__property TStStoreDataEvent OnStoreData = {read=FOnStoreData, write=FOnStoreData};
public:
	/* TObject.Create */ inline __fastcall TStContainer(void) : System::Classes::TPersistent() { }
	
};


struct DECLSPEC_DRECORD TAssignRowData
{
public:
	int RowNum;
	System::StaticArray<System::Byte, 1> Data;
};


class PASCALIMPLEMENTATION TStComponent : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	System::UnicodeString __fastcall GetVersion(void);
	void __fastcall SetVersion(const System::UnicodeString Value);
	
__published:
	__property System::UnicodeString Version = {read=GetVersion, write=SetVersion, stored=false};
public:
	/* TComponent.Create */ inline __fastcall virtual TStComponent(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TStComponent(void) { }
	
};


class PASCALIMPLEMENTATION TStBaseEdit : public Vcl::Stdctrls::TEdit
{
	typedef Vcl::Stdctrls::TEdit inherited;
	
protected:
	System::UnicodeString __fastcall GetVersion(void);
	void __fastcall SetVersion(const System::UnicodeString Value);
	
__published:
	__property System::UnicodeString Version = {read=GetVersion, write=SetVersion, stored=false};
public:
	/* TCustomEdit.Create */ inline __fastcall virtual TStBaseEdit(System::Classes::TComponent* AOwner) : Vcl::Stdctrls::TEdit(AOwner) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TStBaseEdit(HWND ParentWindow) : Vcl::Stdctrls::TEdit(ParentWindow) { }
	/* TWinControl.Destroy */ inline __fastcall virtual ~TStBaseEdit(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const int StMaxBlockSize = int(2147483647);
static const System::Word StMaxFileLen = System::Word(0x104);
static const System::Int8 StRLEMaxCount = System::Int8(0x7f);
static const System::Byte StRLERunMode = System::Byte(0x80);
extern DELPHI_PACKAGE System::StaticArray<char, 16> StHexDigitsA;
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 16> StHexDigits;
extern DELPHI_PACKAGE System::Set<char, _DELPHI_SET_CHAR(0), _DELPHI_SET_CHAR(255)> DosDelimSet;
extern DELPHI_PACKAGE System::WideString StHexDigitsW;
extern DELPHI_PACKAGE System::WideString DosDelimSetW;
extern DELPHI_PACKAGE unsigned WMCOPYID;
extern DELPHI_PACKAGE void __fastcall RaiseStError(EStExceptionClass ExceptionClass, int Code);
extern DELPHI_PACKAGE void __fastcall RaiseStWin32Error(EStExceptionClass ExceptionClass, int Code);
extern DELPHI_PACKAGE void __fastcall RaiseStWin32ErrorEx(EStExceptionClass ExceptionClass, int Code, System::UnicodeString Info);
extern DELPHI_PACKAGE bool __fastcall DestroyNode(TStContainer* Container, TStNode* Node, void * OtherData);
extern DELPHI_PACKAGE void __fastcall HugeFillStruc(void *ADest, int ADestSize, const void *ASource, int ASourceSize);
extern DELPHI_PACKAGE void __fastcall HugeFreeMem(void * &P, int Size);
extern DELPHI_PACKAGE bool __fastcall ProductOverflow(int A, int B);
extern DELPHI_PACKAGE void __fastcall ValLongInt(System::UnicodeString S, int &LI, int &ErrorCode);
extern DELPHI_PACKAGE void __fastcall ValSmallint(const System::UnicodeString S, short &SI, int &ErrorCode);
extern DELPHI_PACKAGE void __fastcall ValWord(const System::UnicodeString S, System::Word &Wd, int &ErrorCode);
extern DELPHI_PACKAGE bool __fastcall IsOrInheritsFrom(System::TClass Root, System::TClass Candidate);
extern DELPHI_PACKAGE void __fastcall RaiseContainerError(int Code);
extern DELPHI_PACKAGE void __fastcall RaiseContainerErrorFmt(int Code, System::TVarRec *Data, const int Data_High);
}	/* namespace Stbase */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STBASE)
using namespace Stbase;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StbaseHPP
