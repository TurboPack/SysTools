// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVLAry.pas' rev: 33.00 (Windows)

#ifndef StnvlaryHPP
#define StnvlaryHPP

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
#include <StLArr.hpp>
#include <StNVCont.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stnvlary
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNVLArray;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStNVLArray : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stlarr::TStLArray* FContainer;
	int FElementCount;
	unsigned FElementSize;
	bool __fastcall GetStoreable();
	void __fastcall SetElementCount(int Value);
	void __fastcall SetElementSize(unsigned Value);
	void __fastcall SetStoreable(bool Value);
	void __fastcall RecreateContainer();
	virtual Stbase::TStCompareEvent __fastcall GetOnCompare();
	virtual Stnvcont::TStDisposeDataEvent __fastcall GetOnDisposeData();
	virtual Stnvcont::TStLoadDataEvent __fastcall GetOnLoadData();
	virtual Stnvcont::TStStoreDataEvent __fastcall GetOnStoreData();
	virtual void __fastcall SetOnCompare(Stbase::TStCompareEvent Value);
	virtual void __fastcall SetOnDisposeData(Stnvcont::TStDisposeDataEvent Value);
	virtual void __fastcall SetOnLoadData(Stnvcont::TStLoadDataEvent Value);
	virtual void __fastcall SetOnStoreData(Stnvcont::TStStoreDataEvent Value);
	
public:
	__fastcall virtual TStNVLArray(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVLArray();
	__property Stlarr::TStLArray* Container = {read=FContainer};
	
__published:
	__property int ElementCount = {read=FElementCount, write=SetElementCount, default=10};
	__property unsigned ElementSize = {read=FElementSize, write=SetElementSize, default=4};
	__property bool ElementsStorable = {read=GetStoreable, write=SetStoreable, default=0};
	__property OnCompare;
	__property OnDisposeData;
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvlary */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVLARY)
using namespace Stnvlary;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvlaryHPP
