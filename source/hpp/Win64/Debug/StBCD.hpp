// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StBCD.pas' rev: 32.00 (Windows)

#ifndef StbcdHPP
#define StbcdHPP

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
#include <StStrL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stbcd
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<System::Byte, 10> TBcd;

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 BcdSize = System::Int8(0xa);
static const System::Int8 MantissaDigits = System::Int8(0x12);
static const System::WideChar OverflowChar = (System::WideChar)(0x2a);
extern DELPHI_PACKAGE TBcd ZeroBcd;
extern DELPHI_PACKAGE TBcd MinBcd;
extern DELPHI_PACKAGE TBcd MaxBcd;
extern DELPHI_PACKAGE TBcd BadBcd;
extern DELPHI_PACKAGE TBcd PiBcd;
extern DELPHI_PACKAGE TBcd eBcd;
extern DELPHI_PACKAGE TBcd Ln10Bcd;
extern DELPHI_PACKAGE TBcd __fastcall AbsBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall AddBcd(const TBcd &B1, const TBcd &B2);
extern DELPHI_PACKAGE System::Extended __fastcall BcdExt(const TBcd &B);
extern DELPHI_PACKAGE void __fastcall ConvertBcd(const void *SrcB, System::Byte SrcSize, void *DestB, System::Byte DestSize);
extern DELPHI_PACKAGE bool __fastcall EqDigitsBcd(const TBcd &B1, const TBcd &B2, unsigned Digits);
extern DELPHI_PACKAGE bool __fastcall EqPlacesBcd(const TBcd &B1, const TBcd &B2, unsigned Digits);
extern DELPHI_PACKAGE int __fastcall CmpBcd(const TBcd &B1, const TBcd &B2);
extern DELPHI_PACKAGE TBcd __fastcall ModBcd(const TBcd &B1, const TBcd &B2);
extern DELPHI_PACKAGE TBcd __fastcall DivBcd(const TBcd &B1, const TBcd &B2);
extern DELPHI_PACKAGE TBcd __fastcall ExpBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall ExtBcd(System::Extended E);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrGeneralBcd(const TBcd &B);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FormatBcd(const System::UnicodeString Format, const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall FracBcd(const TBcd &B);
extern DELPHI_PACKAGE bool __fastcall IsIntBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall IntBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall IntPowBcd(const TBcd &B, int E);
extern DELPHI_PACKAGE TBcd __fastcall LnBcd20(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall LnBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall LongBcd(int L);
extern DELPHI_PACKAGE TBcd __fastcall MulBcd(const TBcd &B1, const TBcd &B2);
extern DELPHI_PACKAGE TBcd __fastcall NegBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall PowBcd(const TBcd &B, const TBcd &E);
extern DELPHI_PACKAGE int __fastcall RoundBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall RoundDigitsBcd(const TBcd &B, unsigned Digits);
extern DELPHI_PACKAGE TBcd __fastcall RoundPlacesBcd(const TBcd &B, unsigned Places);
extern DELPHI_PACKAGE TBcd __fastcall SqrtBcd(const TBcd &B);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrBcd(const TBcd &B, unsigned Width, unsigned Places);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrExpBcd(const TBcd &B, unsigned Width);
extern DELPHI_PACKAGE TBcd __fastcall SubBcd(const TBcd &B1, const TBcd &B2);
extern DELPHI_PACKAGE int __fastcall TruncBcd(const TBcd &B);
extern DELPHI_PACKAGE TBcd __fastcall ValBcd(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FloatFormBcd(const System::UnicodeString Mask, const TBcd &B, const System::UnicodeString LtCurr, const System::UnicodeString RtCurr, System::WideChar Sep, System::WideChar DecPt);
}	/* namespace Stbcd */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STBCD)
using namespace Stbcd;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StbcdHPP
