// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StJupsat.pas' rev: 29.00 (Windows)

#ifndef StjupsatHPP
#define StjupsatHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stjupsat
{
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TStJupSatPos
{
public:
	double X;
	double Y;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TStJupSats
{
public:
	TStJupSatPos Io;
	TStJupSatPos Europa;
	TStJupSatPos Ganymede;
	TStJupSatPos Callisto;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TStJupSats __fastcall GetJupSats(System::TDateTime JD, bool HighPrecision, bool Shadows);
}	/* namespace Stjupsat */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STJUPSAT)
using namespace Stjupsat;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StjupsatHPP
