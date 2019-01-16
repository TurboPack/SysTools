// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVBits.pas' rev: 32.00 (Windows)

#ifndef StnvbitsHPP
#define StnvbitsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <StBase.hpp>
#include <StBits.hpp>
#include <StNVCont.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stnvbits
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNVBits;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStNVBits : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stbits::TStBits* FContainer;
	int FMaxBits;
	void __fastcall SetMaxBits(int Value);
	virtual Stnvcont::TStLoadDataEvent __fastcall GetOnLoadData(void);
	virtual Stnvcont::TStStoreDataEvent __fastcall GetOnStoreData(void);
	virtual void __fastcall SetOnLoadData(Stnvcont::TStLoadDataEvent Value);
	virtual void __fastcall SetOnStoreData(Stnvcont::TStStoreDataEvent Value);
	
public:
	__fastcall virtual TStNVBits(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVBits(void);
	__property Stbits::TStBits* Container = {read=FContainer};
	
__published:
	__property int MaxBits = {read=FMaxBits, write=SetMaxBits, default=100};
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvbits */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVBITS)
using namespace Stnvbits;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvbitsHPP
