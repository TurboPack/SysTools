// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVColl.pas' rev: 29.00 (Windows)

#ifndef StnvcollHPP
#define StnvcollHPP

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
#include <StColl.hpp>
#include <StNVCont.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stnvcoll
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNVCollection;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStNVCollection : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stcoll::TStCollection* FContainer;
	int FPageElements;
	void __fastcall SetPageElements(int Value);
	void __fastcall RecreateContainer(void);
	virtual Stbase::TStCompareEvent __fastcall GetOnCompare(void);
	virtual Stnvcont::TStDisposeDataEvent __fastcall GetOnDisposeData(void);
	virtual Stnvcont::TStLoadDataEvent __fastcall GetOnLoadData(void);
	virtual Stnvcont::TStStoreDataEvent __fastcall GetOnStoreData(void);
	virtual void __fastcall SetOnCompare(Stbase::TStCompareEvent Value);
	virtual void __fastcall SetOnDisposeData(Stnvcont::TStDisposeDataEvent Value);
	virtual void __fastcall SetOnLoadData(Stnvcont::TStLoadDataEvent Value);
	virtual void __fastcall SetOnStoreData(Stnvcont::TStStoreDataEvent Value);
	
public:
	__fastcall virtual TStNVCollection(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVCollection(void);
	__property Stcoll::TStCollection* Container = {read=FContainer};
	
__published:
	__property int PageElements = {read=FPageElements, write=SetPageElements, default=1000};
	__property OnCompare;
	__property OnDisposeData;
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvcoll */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVCOLL)
using namespace Stnvcoll;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvcollHPP
