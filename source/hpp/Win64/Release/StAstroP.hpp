// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StAstroP.pas' rev: 30.00 (Windows)

#ifndef StastropHPP
#define StastropHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stastrop
{
//-- forward type declarations -----------------------------------------------
struct TStEclipticalCord;
struct TStRectangularCord;
struct TStPlanetsRec;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TStEclipticalCord
{
public:
	double L0;
	double B0;
	double R0;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TStRectangularCord
{
public:
	double X;
	double Y;
	double Z;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TStPlanetsRec
{
public:
	double RA;
	double DC;
	double Elong;
};
#pragma pack(pop)


typedef System::StaticArray<TStPlanetsRec, 8> TStPlanetsArray;

//-- var, const, procedure ---------------------------------------------------
#define StdDate  (2.451545E+06)
static const double OB2000 = 4.090928E-01;
extern DELPHI_PACKAGE void __fastcall PlanetsPos(double JD, TStPlanetsArray &PA);
}	/* namespace Stastrop */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STASTROP)
using namespace Stastrop;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StastropHPP
