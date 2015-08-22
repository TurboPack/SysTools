// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StUtils.pas' rev: 30.00 (Windows)

#ifndef StutilsHPP
#define StutilsHPP

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
#include <StConst.hpp>
#include <StBase.hpp>
#include <StDate.hpp>
#include <StStrL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE short __fastcall MakeInteger16(System::Byte H, System::Byte L);
extern DELPHI_PACKAGE System::Byte __fastcall SwapNibble(System::Byte B);
extern DELPHI_PACKAGE int __fastcall SwapWord(int L);
extern DELPHI_PACKAGE void __fastcall SetFlag(System::Word &Flags, System::Word FlagMask);
extern DELPHI_PACKAGE void __fastcall ClearFlag(System::Word &Flags, System::Word FlagMask);
extern DELPHI_PACKAGE bool __fastcall FlagIsSet(System::Word Flags, System::Word FlagMask);
extern DELPHI_PACKAGE void __fastcall SetByteFlag(System::Byte &Flags, System::Byte FlagMask);
extern DELPHI_PACKAGE void __fastcall ClearByteFlag(System::Byte &Flags, System::Byte FlagMask);
extern DELPHI_PACKAGE bool __fastcall ByteFlagIsSet(System::Byte Flags, System::Byte FlagMask);
extern DELPHI_PACKAGE void __fastcall SetLongFlag(int &Flags, int FlagMask);
extern DELPHI_PACKAGE void __fastcall ClearLongFlag(int &Flags, int FlagMask);
extern DELPHI_PACKAGE bool __fastcall LongFlagIsSet(int Flags, int FlagMask);
extern DELPHI_PACKAGE void __fastcall ExchangeBytes(System::Byte &I, System::Byte &J);
extern DELPHI_PACKAGE void __fastcall ExchangeLongInts(int &I, int &J);
extern DELPHI_PACKAGE void __fastcall ExchangeStructs(void *I, void *J, int Size);
extern DELPHI_PACKAGE void __fastcall FillWord(void *ADest, int ACount, System::Word AFiller);
extern DELPHI_PACKAGE void * __fastcall AddWordToPtr(void * P, System::Word W);
extern DELPHI_PACKAGE System::Word __fastcall MakeWord(System::Byte H, System::Byte L);
extern DELPHI_PACKAGE System::Word __fastcall MinWord(System::Word A, System::Word B);
extern DELPHI_PACKAGE System::Word __fastcall MaxWord(System::Word A, System::Word B);
extern DELPHI_PACKAGE int __fastcall MinLong(int A, int B);
extern DELPHI_PACKAGE int __fastcall MaxLong(int A, int B);
extern DELPHI_PACKAGE int __fastcall SignL(int L);
extern DELPHI_PACKAGE int __fastcall SignF(System::Extended F);
extern DELPHI_PACKAGE System::Word __fastcall MidWord(System::Word W1, System::Word W2, System::Word W3);
extern DELPHI_PACKAGE int __fastcall MidLong(int L1, int L2, int L3);
extern DELPHI_PACKAGE System::Extended __fastcall MidFloat(System::Extended F1, System::Extended F2, System::Extended F3);
extern DELPHI_PACKAGE System::Extended __fastcall MinFloat(System::Extended F1, System::Extended F2);
extern DELPHI_PACKAGE System::Extended __fastcall MaxFloat(System::Extended F1, System::Extended F2);
}	/* namespace Stutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STUTILS)
using namespace Stutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StutilsHPP
