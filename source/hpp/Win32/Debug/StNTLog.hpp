// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNTLog.pas' rev: 31.00 (Windows)

#ifndef StntlogHPP
#define StntlogHPP

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
#include <System.Win.Registry.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stntlog
{
//-- forward type declarations -----------------------------------------------
struct TStNTEventLogRec;
class DELPHICLASS TStNTEventLog;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStNTEventType : unsigned char { etSuccess, etError, etWarning, etInfo, etAuditSuccess, etAuditFailure };

typedef TStNTEventLogRec *PStNTEventLogRec;

struct DECLSPEC_DRECORD TStNTEventLogRec
{
	
public:
	union
	{
		struct 
		{
			System::StaticArray<System::Byte, 65536> VarData;
		};
		struct 
		{
			unsigned Length;
			unsigned Reserved;
			unsigned RecordNumber;
			unsigned TimeGenerated;
			unsigned TimeWritten;
			unsigned EventID;
			System::Word EventType;
			System::Word NumStrings;
			System::Word EventCategory;
			System::Word ReservedFlags;
			unsigned ClosingRecordNumber;
			unsigned StringOffset;
			unsigned UserSidLength;
			unsigned UserSidOffset;
			unsigned DataLength;
			unsigned DataOffset;
		};
		
	};
};


typedef void __fastcall (__closure *TStReadRecordEvent)(System::TObject* Sender, const TStNTEventLogRec &EventRec, bool &Abort);

class PASCALIMPLEMENTATION TStNTEventLog : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
private:
	NativeUInt elLogHandle;
	System::Classes::TStringList* elLogList;
	System::UnicodeString FComputerName;
	bool FEnabled;
	System::UnicodeString FEventSource;
	System::UnicodeString FLogName;
	TStReadRecordEvent FOnReadRecord;
	
protected:
	void __fastcall elAddEntry(const TStNTEventType EventType, unsigned EventCategory, unsigned EventID, System::Classes::TStrings* const Strings, void * DataPtr, unsigned DataSize);
	void __fastcall elCloseLog(void);
	void __fastcall elOpenLog(void);
	unsigned __fastcall GetLogCount(void);
	System::UnicodeString __fastcall GetLogs(int Index);
	unsigned __fastcall GetRecordCount(void);
	void __fastcall SetComputerName(const System::UnicodeString Value);
	void __fastcall SetLogName(const System::UnicodeString Value);
	
public:
	__fastcall virtual TStNTEventLog(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNTEventLog(void);
	void __fastcall AddEntry(const TStNTEventType EventType, unsigned EventCategory, unsigned EventID);
	void __fastcall AddEntryEx(const TStNTEventType EventType, unsigned EventCategory, unsigned EventID, System::Classes::TStrings* const Strings, void * DataPtr, unsigned DataSize);
	void __fastcall ClearLog(const System::Sysutils::TFileName BackupName);
	void __fastcall CreateBackup(const System::Sysutils::TFileName BackupName);
	void __fastcall ReadLog(const bool Reverse);
	void __fastcall RefreshLogList(void);
	__property unsigned LogCount = {read=GetLogCount, nodefault};
	__property System::UnicodeString Logs[int Index] = {read=GetLogs};
	__property unsigned RecordCount = {read=GetRecordCount, nodefault};
	
__published:
	__property System::UnicodeString ComputerName = {read=FComputerName, write=SetComputerName};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property System::UnicodeString EventSource = {read=FEventSource, write=FEventSource};
	__property System::UnicodeString LogName = {read=FLogName, write=SetLogName};
	__property TStReadRecordEvent OnReadRecord = {read=FOnReadRecord, write=FOnReadRecord};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stntlog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNTLOG)
using namespace Stntlog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StntlogHPP
