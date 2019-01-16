// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDecMth.pas' rev: 32.00 (Windows)

#ifndef StdecmthHPP
#define StdecmthHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stdecmth
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStDecimal;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStRoundMethod : unsigned char { rmNormal, rmTrunc, rmBankers, rmUp };

typedef System::StaticArray<int, 4> TStInt128;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStDecimal : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TStInt128 FInt;
	
protected:
	System::AnsiString __fastcall dcGetAsStr(void);
	void __fastcall dcSetFromStr(const System::AnsiString aValue);
	
public:
	__fastcall TStDecimal(void);
	__fastcall virtual ~TStDecimal(void);
	int __fastcall Compare(TStDecimal* X);
	bool __fastcall IsNegative(void);
	bool __fastcall IsOne(void);
	bool __fastcall IsPositive(void);
	bool __fastcall IsZero(void);
	void __fastcall SetToOne(void);
	void __fastcall SetToZero(void);
	void __fastcall Assign(TStDecimal* X);
	void __fastcall AssignFromFloat(double aValue);
	void __fastcall AssignFromInt(int aValue);
	double __fastcall AsFloat(void);
	int __fastcall AsInt(TStRoundMethod aRound);
	void __fastcall Abs(void);
	void __fastcall Add(TStDecimal* X);
	void __fastcall AddOne(void);
	void __fastcall ChangeSign(void);
	void __fastcall Divide(TStDecimal* X);
	void __fastcall Multiply(TStDecimal* X);
	void __fastcall RaiseToPower(int N);
	void __fastcall Round(TStRoundMethod aRound, int aDecPl);
	void __fastcall Subtract(TStDecimal* X);
	void __fastcall SubtractOne(void);
	__property System::AnsiString AsString = {read=dcGetAsStr, write=dcSetFromStr};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stdecmth */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STDECMTH)
using namespace Stdecmth;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StdecmthHPP
