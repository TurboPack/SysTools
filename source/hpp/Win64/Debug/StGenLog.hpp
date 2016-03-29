// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StGenLog.pas' rev: 31.00 (Windows)

#ifndef StgenlogHPP
#define StgenlogHPP

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

//-- user supplied -----------------------------------------------------------

namespace Stgenlog
{
//-- forward type declarations -----------------------------------------------
struct TStLogRec;
class DELPHICLASS TStGeneralLog;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TStGetLogStringEvent)(System::TObject* Sender, const unsigned D1, const unsigned D2, const unsigned D3, const unsigned D4, System::AnsiString &LogString);

enum DECLSPEC_DENUM TStWriteMode : unsigned char { wmOverwrite, wmAppend };

typedef TStLogRec *PStLogRec;

struct DECLSPEC_DRECORD TStLogRec
{
public:
	unsigned lrTime;
	unsigned lrData1;
	unsigned lrData2;
	unsigned lrData3;
	unsigned lrData4;
};


typedef System::StaticArray<System::Byte, 16000001> TStLogBuffer;

typedef TStLogBuffer *PStLogBuffer;

enum DECLSPEC_DENUM StGenOptions : unsigned char { goSuppressEnableMsg, goSuppressDisableMsg };

typedef System::Set<StGenOptions, StGenOptions::goSuppressEnableMsg, StGenOptions::goSuppressDisableMsg> StGenOptionSet;

class PASCALIMPLEMENTATION TStGeneralLog : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
private:
	unsigned FBufferSize;
	bool FEnabled;
	System::Sysutils::TFileName FFileName;
	System::Byte FHighLevel;
	System::UnicodeString FLogFileFooter;
	System::UnicodeString FLogFileHeader;
	StGenOptionSet FLogOptions;
	TStWriteMode FWriteMode;
	System::Classes::TNotifyEvent FOnHighLevel;
	TStGetLogStringEvent FOnGetLogString;
	TStLogBuffer *glBuffer;
	unsigned glBufferHead;
	unsigned glBufferTail;
	unsigned glHighLevelMark;
	bool glHighLevelTriggered;
	_RTL_CRITICAL_SECTION glLogCS;
	System::Sysutils::TByteArray *glTempBuffer;
	unsigned glTempSize;
	unsigned glTimeBase;
	
protected:
	virtual void __fastcall DoGetLogString(const unsigned D1, const unsigned D2, const unsigned D3, const unsigned D4, System::AnsiString &LogString);
	bool __fastcall GetBufferEmpty(void);
	unsigned __fastcall GetBufferFree(void);
	unsigned __fastcall GetBufferSize(void);
	bool __fastcall GetEnabled(void);
	System::Sysutils::TFileName __fastcall GetFileName(void);
	System::Byte __fastcall GetHighLevel(void);
	StGenOptionSet __fastcall GetLogOptions(void);
	TStWriteMode __fastcall GetWriteMode(void);
	void __fastcall SetBufferSize(const unsigned Value);
	virtual void __fastcall SetEnabled(const bool Value);
	virtual void __fastcall SetFileName(const System::Sysutils::TFileName Value);
	void __fastcall SetHighLevel(const System::Byte Value);
	void __fastcall SetLogOptions(const StGenOptionSet Value);
	void __fastcall SetWriteMode(const TStWriteMode Value);
	void __fastcall glCalcHighLevel(void);
	void __fastcall glCheckTempSize(unsigned SizeReq);
	void __fastcall glHighLevelCheck(void);
	void __fastcall glLockLog(void);
	bool __fastcall glPopLogEntry(TStLogRec &LogRec);
	System::UnicodeString __fastcall glTimeStamp(unsigned Mark);
	void __fastcall glUnlockLog(void);
	
public:
	__fastcall virtual TStGeneralLog(System::Classes::TComponent* Owner);
	__fastcall virtual ~TStGeneralLog(void);
	void __fastcall AddLogEntry(const unsigned D1, const unsigned D2, const unsigned D3, const unsigned D4);
	void __fastcall ClearBuffer(void);
	virtual void __fastcall DumpLog(void);
	void __fastcall WriteLogString(const System::AnsiString LogString);
	__property bool BufferEmpty = {read=GetBufferEmpty, nodefault};
	__property unsigned BufferFree = {read=GetBufferFree, nodefault};
	
__published:
	__property unsigned BufferSize = {read=GetBufferSize, write=SetBufferSize, default=65536};
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, default=1};
	__property System::Sysutils::TFileName FileName = {read=GetFileName, write=SetFileName};
	__property System::Byte HighLevel = {read=GetHighLevel, write=SetHighLevel, default=0};
	__property System::UnicodeString LogFileFooter = {read=FLogFileFooter, write=FLogFileFooter};
	__property System::UnicodeString LogFileHeader = {read=FLogFileHeader, write=FLogFileHeader};
	__property StGenOptionSet LogOptions = {read=GetLogOptions, write=SetLogOptions, default=0};
	__property TStWriteMode WriteMode = {read=GetWriteMode, write=SetWriteMode, nodefault};
	__property System::Classes::TNotifyEvent OnHighLevel = {read=FOnHighLevel, write=FOnHighLevel};
	__property TStGetLogStringEvent OnGetLogString = {read=FOnGetLogString, write=FOnGetLogString};
};


//-- var, const, procedure ---------------------------------------------------
static const int StDefBufferSize = int(0x10000);
static const System::Int8 StDefHighLevel = System::Int8(0x0);
static const int StMaxLogSize = int(0xf42400);
#define StCRLF L"\r\n"
#define StLogFileFooter L""
#define StLogFileHeader L"SysTools General Log\r\n=================================="\
	L"===========================================\r\n\r\n"
static const System::Int8 leEnabled = System::Int8(0x1);
static const System::Int8 leDisabled = System::Int8(0x2);
static const unsigned leString = unsigned(0x80000000);
extern DELPHI_PACKAGE System::AnsiString __fastcall HexifyBlock(void *Buffer, int BufferSize);
}	/* namespace Stgenlog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STGENLOG)
using namespace Stgenlog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StgenlogHPP
