// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDateSt.pas' rev: 28.00 (Windows)

#ifndef StdatestHPP
#define StdatestHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <StStrL.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StUtils.hpp>	// Pascal unit
#include <StDate.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stdatest
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::WideChar MonthOnly = (System::WideChar)(0x6d);
static const System::WideChar DayOnly = (System::WideChar)(0x64);
static const System::WideChar YearOnly = (System::WideChar)(0x79);
static const System::WideChar MonthOnlyU = (System::WideChar)(0x4d);
static const System::WideChar DayOnlyU = (System::WideChar)(0x44);
static const System::WideChar DateSlash = (System::WideChar)(0x2f);
static const System::WideChar NameOnly = (System::WideChar)(0x6e);
static const System::WideChar NameOnlyU = (System::WideChar)(0x4e);
static const System::WideChar WeekDayOnly = (System::WideChar)(0x77);
static const System::WideChar WeekDayOnlyU = (System::WideChar)(0x57);
static const System::WideChar LongDateSub1 = (System::WideChar)(0x66);
static const System::WideChar LongDateSub2 = (System::WideChar)(0x67);
static const System::WideChar LongDateSub3 = (System::WideChar)(0x68);
static const System::WideChar HourOnly = (System::WideChar)(0x68);
static const System::WideChar MinOnly = (System::WideChar)(0x6d);
static const System::WideChar SecOnly = (System::WideChar)(0x73);
static const System::WideChar HourOnlyU = (System::WideChar)(0x48);
static const System::WideChar MinOnlyU = (System::WideChar)(0x4d);
static const System::WideChar SecOnlyU = (System::WideChar)(0x53);
static const System::WideChar TimeOnly = (System::WideChar)(0x74);
static const System::WideChar TimeColon = (System::WideChar)(0x3a);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DayOfWeekToString(const Stdate::TStDayType WeekDay);
extern DELPHI_PACKAGE System::UnicodeString __fastcall MonthToString(const int Month);
extern DELPHI_PACKAGE double __fastcall DateStringHMStoAstJD(const System::UnicodeString Picture, const System::UnicodeString DS, int H, int M, int S, int Epoch);
extern DELPHI_PACKAGE bool __fastcall DateStringToDMY(const System::UnicodeString Picture, const System::UnicodeString S, int Epoch, int &D, int &M, int &Y);
extern DELPHI_PACKAGE bool __fastcall DateStringIsBlank(const System::UnicodeString Picture, const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall DateStringToStDate(const System::UnicodeString Picture, const System::UnicodeString S, int Epoch);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DMYtoDateString(const System::UnicodeString Picture, int Day, int Month, int Year, int Epoch, bool Pack);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StDateToDateString(const System::UnicodeString Picture, const int Julian, bool Pack);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CurrentDateString(const System::UnicodeString Picture, bool Pack);
extern DELPHI_PACKAGE bool __fastcall TimeStringToHMS(const System::UnicodeString Picture, const System::UnicodeString St, int &H, int &M, int &S);
extern DELPHI_PACKAGE int __fastcall TimeStringToStTime(const System::UnicodeString Picture, const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StTimeToTimeString(const System::UnicodeString Picture, const int T, bool Pack);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StTimeToAmPmString(const System::UnicodeString Picture, const int T, bool Pack);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CurrentTimeString(const System::UnicodeString Picture, bool Pack);
extern DELPHI_PACKAGE System::UnicodeString __fastcall InternationalDate(bool ForceCentury);
extern DELPHI_PACKAGE System::UnicodeString __fastcall InternationalLongDate(bool ShortNames, bool ExcludeDOW);
extern DELPHI_PACKAGE System::UnicodeString __fastcall InternationalTime(bool ShowSeconds);
extern DELPHI_PACKAGE void __fastcall ResetInternationalInfo(void);
}	/* namespace Stdatest */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STDATEST)
using namespace Stdatest;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StdatestHPP
