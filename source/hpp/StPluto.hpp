// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StPluto.pas' rev: 28.00 (Windows)

#ifndef StplutoHPP
#define StplutoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <StAstroP.hpp>	// Pascal unit
#include <StMath.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stpluto
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE Stastrop::TStEclipticalCord __fastcall ComputePluto(double JD);
}	/* namespace Stpluto */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STPLUTO)
using namespace Stpluto;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StplutoHPP
