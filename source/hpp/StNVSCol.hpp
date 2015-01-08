// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVSCol.pas' rev: 28.00 (Windows)

#ifndef StnvscolHPP
#define StnvscolHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StColl.hpp>	// Pascal unit
#include <StNVCont.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stnvscol
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStNVSortedCollection;
class PASCALIMPLEMENTATION TStNVSortedCollection : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stcoll::TStSortedCollection* FContainer;
	bool FDuplicates;
	int FPageElements;
	void __fastcall SetDuplicates(bool Value);
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
	__fastcall virtual TStNVSortedCollection(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVSortedCollection(void);
	__property Stcoll::TStSortedCollection* Container = {read=FContainer};
	
__published:
	__property bool Duplicates = {read=FDuplicates, write=SetDuplicates, default=0};
	__property int PageElements = {read=FPageElements, write=SetPageElements, default=1000};
	__property OnCompare;
	__property OnDisposeData;
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvscol */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVSCOL)
using namespace Stnvscol;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvscolHPP
