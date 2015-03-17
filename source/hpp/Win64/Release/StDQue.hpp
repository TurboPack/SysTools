// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDQue.pas' rev: 29.00 (Windows)

#ifndef StdqueHPP
#define StdqueHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StList.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stdque
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStDQue;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStDQue : public Stlist::TStList
{
	typedef Stlist::TStList inherited;
	
public:
	void __fastcall PushTail(void * Data);
	void __fastcall PopTail(void);
	void __fastcall PeekTail(void * &Data);
	void __fastcall PushHead(void * Data);
	void __fastcall PopHead(void);
	void __fastcall PeekHead(void * &Data);
public:
	/* TStList.Create */ inline __fastcall virtual TStDQue(Stbase::TStNodeClass NodeClass) : Stlist::TStList(NodeClass) { }
	
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStDQue(Stbase::TStNodeClass NodeClass, int Dummy) : Stlist::TStList(NodeClass, Dummy) { }
	/* TStContainer.Destroy */ inline __fastcall virtual ~TStDQue(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stdque */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STDQUE)
using namespace Stdque;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StdqueHPP
