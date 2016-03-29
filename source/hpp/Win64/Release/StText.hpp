// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StText.pas' rev: 31.00 (Windows)

#ifndef SttextHPP
#define SttextHPP

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
#include <StSystem.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sttext
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall TextSeek(System::TextFile &F, int Target);
extern DELPHI_PACKAGE int __fastcall TextFileSize(System::TextFile &F);
extern DELPHI_PACKAGE int __fastcall TextPos(System::TextFile &F);
extern DELPHI_PACKAGE bool __fastcall TextFlush(System::TextFile &F);
}	/* namespace Sttext */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STTEXT)
using namespace Sttext;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SttextHPP
