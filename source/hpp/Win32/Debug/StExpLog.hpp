// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StExpLog.pas' rev: 30.00 (Windows)

#ifndef StexplogHPP
#define StexplogHPP

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

namespace Stexplog
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStExceptionLog;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TStOnExceptionFilter)(System::TObject* Sender, System::Sysutils::Exception* E, bool &PutInLog);

class PASCALIMPLEMENTATION TStExceptionLog : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
private:
	static TStExceptionLog* ExpLog;
	bool FEnabled;
	System::Sysutils::TFileName FFileName;
	System::UnicodeString FRipInfo;
	TStOnExceptionFilter FOnExceptionFilter;
	
public:
	__fastcall virtual TStExceptionLog(System::Classes::TComponent* Owner);
	__fastcall virtual ~TStExceptionLog(void);
	virtual void __fastcall DoExceptionFilter(System::Sysutils::Exception* E, bool &PutInLog);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property System::Sysutils::TFileName FileName = {read=FFileName, write=FFileName};
	__property System::UnicodeString RipInfo = {read=FRipInfo, write=FRipInfo};
	__property TStOnExceptionFilter OnExceptionFilter = {read=FOnExceptionFilter, write=FOnExceptionFilter};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stexplog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STEXPLOG)
using namespace Stexplog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StexplogHPP
