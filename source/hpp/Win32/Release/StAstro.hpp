// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StAstro.pas' rev: 30.00 (Windows)

#ifndef StastroHPP
#define StastroHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StDate.hpp>
#include <StStrL.hpp>
#include <StDateSt.hpp>
#include <StMath.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stastro
{
//-- forward type declarations -----------------------------------------------
struct TStRiseSetRec;
struct TStPosRec;
struct TStSunXYZRec;
struct TStLunarRecord;
struct TStPhaseRecord;
struct TStDLSDateRec;
struct TStMoonPosRec;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStTwilight : unsigned char { ttCivil, ttNautical, ttAstronomical };

struct DECLSPEC_DRECORD TStRiseSetRec
{
public:
	int ORise;
	int OSet;
};


struct DECLSPEC_DRECORD TStPosRec
{
public:
	double RA;
	double DC;
};


typedef System::StaticArray<TStPosRec, 3> TStPosRecArray;

struct DECLSPEC_DRECORD TStSunXYZRec
{
public:
	double SunX;
	double SunY;
	double SunZ;
	double RV;
	double SLong;
	double SLat;
};


struct DECLSPEC_DRECORD TStLunarRecord
{
public:
	System::StaticArray<Stdate::TStDateTimeRec, 2> T;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TStPhaseRecord
{
public:
	double NMDate;
	double FQDate;
	double FMDate;
	double LQDate;
};
#pragma pack(pop)


typedef System::StaticArray<TStPhaseRecord, 14> TStPhaseArray;

struct DECLSPEC_DRECORD TStDLSDateRec
{
public:
	int Starts;
	int Ends;
};


struct DECLSPEC_DRECORD TStMoonPosRec
{
public:
	double RA;
	double DC;
	double Phase;
	double Dia;
	double Plx;
	double Elong;
};


//-- var, const, procedure ---------------------------------------------------
static const System::Extended radcor = 5.729578E+01;
#define StdDate  (2.451545E+06)
static const System::Extended OB2000 = 4.090928E-01;
extern DELPHI_PACKAGE double __fastcall SiderealTime(const Stdate::TStDateTimeRec &UT);
extern DELPHI_PACKAGE TStSunXYZRec __fastcall SunPosPrim(const Stdate::TStDateTimeRec &UT);
extern DELPHI_PACKAGE int __fastcall AmountOfSunlight(int LD, double Longitude, double Latitude);
extern DELPHI_PACKAGE TStPosRec __fastcall SunPos(const Stdate::TStDateTimeRec &UT);
extern DELPHI_PACKAGE TStRiseSetRec __fastcall SunRiseSet(int LD, double Longitude, double Latitude);
extern DELPHI_PACKAGE TStRiseSetRec __fastcall Twilight(int LD, double Longitude, double Latitude, TStTwilight TwiType);
extern DELPHI_PACKAGE TStRiseSetRec __fastcall FixedRiseSet(int LD, double RA, double DC, double Longitude, double Latitude);
extern DELPHI_PACKAGE TStMoonPosRec __fastcall MoonPos(const Stdate::TStDateTimeRec &UT);
extern DELPHI_PACKAGE TStRiseSetRec __fastcall MoonRiseSet(int LD, double Longitude, double Latitude);
extern DELPHI_PACKAGE double __fastcall LunarPhase(const Stdate::TStDateTimeRec &UT);
extern DELPHI_PACKAGE TStLunarRecord __fastcall FirstQuarter(int D);
extern DELPHI_PACKAGE TStLunarRecord __fastcall FullMoon(int D);
extern DELPHI_PACKAGE TStLunarRecord __fastcall LastQuarter(int D);
extern DELPHI_PACKAGE TStLunarRecord __fastcall NewMoon(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall NextFirstQuarter(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall NextFullMoon(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall NextLastQuarter(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall NextNewMoon(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall PrevFirstQuarter(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall PrevFullMoon(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall PrevLastQuarter(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall PrevNewMoon(int D);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall Solstice(int Y, int Epoch, bool Summer);
extern DELPHI_PACKAGE Stdate::TStDateTimeRec __fastcall Equinox(int Y, int Epoch, bool Vernal);
extern DELPHI_PACKAGE int __fastcall Easter(int Y, int Epoch);
extern DELPHI_PACKAGE System::UnicodeString __fastcall HoursMin(double RA);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DegsMin(double DC);
extern DELPHI_PACKAGE double __fastcall DateTimeToAJD(System::TDateTime D);
extern DELPHI_PACKAGE System::TDateTime __fastcall AJDToDateTime(double D);
}	/* namespace Stastro */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STASTRO)
using namespace Stastro;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StastroHPP
