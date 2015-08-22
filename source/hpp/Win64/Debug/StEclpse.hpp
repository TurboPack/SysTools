// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StEclpse.pas' rev: 30.00 (Windows)

#ifndef SteclpseHPP
#define SteclpseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Math.hpp>
#include <StBase.hpp>
#include <StList.hpp>
#include <StDate.hpp>
#include <StAstro.hpp>
#include <StMath.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Steclpse
{
//-- forward type declarations -----------------------------------------------
struct TStContactTimes;
struct TStLongLat;
struct TStEclipseRecord;
struct TStBesselianRecord;
class DELPHICLASS TStEclipses;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStEclipseType : unsigned char { etLunarPenumbral, etLunarPartial, etLunarTotal, etSolarPartial, etSolarAnnular, etSolarTotal, etSolarAnnularTotal };

enum DECLSPEC_DENUM TStHemisphereType : unsigned char { htNone, htNorthern, htSouthern };

#pragma pack(push,1)
struct DECLSPEC_DRECORD TStContactTimes
{
public:
	System::TDateTime UT1;
	System::TDateTime UT2;
	System::TDateTime FirstContact;
	System::TDateTime SecondContact;
	System::TDateTime MidEclipse;
	System::TDateTime ThirdContact;
	System::TDateTime FourthContact;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TStLongLat
{
public:
	System::TDateTime JD;
	double Longitude;
	double Latitude;
	double Duration;
};
#pragma pack(pop)


typedef TStLongLat *PStLongLat;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TStEclipseRecord
{
public:
	TStEclipseType EType;
	double Magnitude;
	TStHemisphereType Hemisphere;
	TStContactTimes LContacts;
	Stlist::TStList* Path;
};
#pragma pack(pop)


typedef TStEclipseRecord *PStEclipseRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TStBesselianRecord
{
public:
	System::TDateTime JD;
	double Delta;
	double Angle;
	double XAxis;
	double YAxis;
	double L1;
	double L2;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TStEclipses : public Stlist::TStList
{
	typedef Stlist::TStList inherited;
	
protected:
	System::StaticArray<TStBesselianRecord, 25> FBesselianElements;
	double F0;
	double FUPrime;
	double FDPrime;
	PStEclipseRecord __fastcall GetEclipse(int Idx);
	void __fastcall CentralEclipseTime(double JD, double K, double J2, double SunAnom, double MoonAnom, double ArgLat, double AscNode, double EFac, double &F1, double &A1, double &CentralTime);
	void __fastcall CheckForEclipse(double K);
	void __fastcall TotalLunarEclipse(double CentralJD, double MoonAnom, double Mu, double PMag, double UMag, double Gamma);
	void __fastcall PartialLunarEclipse(double CentralJD, double MoonAnom, double Mu, double PMag, double UMag, double Gamma);
	void __fastcall PenumbralLunarEclipse(double CentralJD, double MoonAnom, double Mu, double PMag, double UMag, double Gamma);
	void __fastcall GetBesselianElements(double CentralJD);
	void __fastcall GetShadowPath(int I1, int I2, Stlist::TStList* Path);
	void __fastcall NonPartialSolarEclipse(double CentralJD, double Mu, double Gamma);
	void __fastcall PartialSolarEclipse(double CentralJD, double Mu, double Gamma);
	
public:
	__fastcall virtual TStEclipses(Stbase::TStNodeClass NodeClass);
	void __fastcall FindEclipses(int Year);
	__property PStEclipseRecord Eclipses[int Idx] = {read=GetEclipse};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStEclipses(Stbase::TStNodeClass NodeClass, int Dummy) : Stlist::TStList(NodeClass, Dummy) { }
	/* TStContainer.Destroy */ inline __fastcall virtual ~TStEclipses(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Steclpse */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STECLPSE)
using namespace Steclpse;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SteclpseHPP
