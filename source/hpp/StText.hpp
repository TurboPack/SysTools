// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StText.pas' rev: 28.00 (Windows)

#ifndef SttextHPP
#define SttextHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StSystem.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Sttext
{
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
