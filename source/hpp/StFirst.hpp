// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StFirst.pas' rev: 29.00 (Windows)

#ifndef StfirstHPP
#define StfirstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Forms.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Dialogs.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stfirst
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall IsFirstInstance(void);
extern DELPHI_PACKAGE void __fastcall ActivateFirstCommandLine(void);
extern DELPHI_PACKAGE void __fastcall ActivateFirst(System::WideChar * AString);
}	/* namespace Stfirst */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STFIRST)
using namespace Stfirst;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StfirstHPP
