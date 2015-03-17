// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StRandom.pas' rev: 29.00 (Windows)

#ifndef StrandomHPP
#define StrandomHPP

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

namespace Strandom
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStRandomBase;
class DELPHICLASS TStRandomSystem;
class DELPHICLASS TStRandomCombined;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStRandomBase : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	double __fastcall rbMarsagliaGamma(double aShape);
	double __fastcall rbMontyPythonNormal(void);
	
public:
	virtual double __fastcall AsFloat(void) = 0 ;
	int __fastcall AsInt(int aUpperLimit);
	int __fastcall AsIntInRange(int aLowerLimit, int aUpperLimit);
	double __fastcall AsBeta(double aShape1, double aShape2);
	double __fastcall AsCauchy(void);
	double __fastcall AsChiSquared(int aFreedom);
	double __fastcall AsErlang(double aMean, int aOrder);
	double __fastcall AsExponential(double aMean);
	double __fastcall AsF(int aFreedom1, int aFreedom2);
	double __fastcall AsGamma(double aShape, double aScale);
	double __fastcall AsLogNormal(double aMean, double aStdDev);
	double __fastcall AsNormal(double aMean, double aStdDev);
	double __fastcall AsT(int aFreedom);
	double __fastcall AsWeibull(double aShape, double aScale);
public:
	/* TObject.Create */ inline __fastcall TStRandomBase(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStRandomBase(void) { }
	
};


class PASCALIMPLEMENTATION TStRandomSystem : public TStRandomBase
{
	typedef TStRandomBase inherited;
	
private:
	int FSeed;
	
protected:
	void __fastcall rsSetSeed(int aValue);
	
public:
	__fastcall TStRandomSystem(int aSeed);
	virtual double __fastcall AsFloat(void);
	__property int Seed = {read=FSeed, write=rsSetSeed, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStRandomSystem(void) { }
	
};


class PASCALIMPLEMENTATION TStRandomCombined : public TStRandomBase
{
	typedef TStRandomBase inherited;
	
private:
	int FSeed1;
	int FSeed2;
	
protected:
	void __fastcall rcSetSeed1(int aValue);
	void __fastcall rcSetSeed2(int aValue);
	
public:
	__fastcall TStRandomCombined(int aSeed1, int aSeed2);
	virtual double __fastcall AsFloat(void);
	__property int Seed1 = {read=FSeed1, write=rcSetSeed1, nodefault};
	__property int Seed2 = {read=FSeed2, write=rcSetSeed2, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStRandomCombined(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Strandom */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STRANDOM)
using namespace Strandom;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StrandomHPP
