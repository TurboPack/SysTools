// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StWmDCpy.pas' rev: 31.00 (Windows)

#ifndef StwmdcpyHPP
#define StwmdcpyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Dialogs.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stwmdcpy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStWMDataCopy;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TStOnDataReceivedEvent)(System::TObject* Sender, const tagCOPYDATASTRUCT &CopyData);

class PASCALIMPLEMENTATION TStWMDataCopy : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	void *NewWndProc;
	void *PrevWndProc;
	TStOnDataReceivedEvent FOnDataReceived;
	void __fastcall AppWndProc(Winapi::Messages::TMessage &Msg);
	void __fastcall HookForm(bool Value);
	
public:
	__fastcall virtual TStWMDataCopy(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStWMDataCopy(void);
	
__published:
	__property TStOnDataReceivedEvent OnDataReceived = {read=FOnDataReceived, write=FOnDataReceived};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stwmdcpy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STWMDCPY)
using namespace Stwmdcpy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StwmdcpyHPP
