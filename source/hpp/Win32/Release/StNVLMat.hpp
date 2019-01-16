// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVLMat.pas' rev: 33.00 (Windows)

#ifndef StnvlmatHPP
#define StnvlmatHPP

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

namespace Stnvlmat
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNVLMatrix;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStNVLMatrix : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stlarr::TStLMatrix* FContainer;
	unsigned FCols;
	unsigned FRows;
	unsigned FElementSize;
	bool __fastcall GetStoreable();
	void __fastcall SetCols(unsigned Value);
	void __fastcall SetRows(unsigned Value);
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
	__fastcall virtual TStNVLMatrix(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVLMatrix();
	__property Stlarr::TStLMatrix* Container = {read=FContainer};
	
__published:
	__property unsigned Cols = {read=FCols, write=SetCols, default=2};
	__property unsigned Rows = {read=FRows, write=SetRows, default=10};
	__property unsigned ElementSize = {read=FElementSize, write=SetElementSize, default=4};
	__property bool ElementsStorable = {read=GetStoreable, write=SetStoreable, default=0};
	__property OnCompare;
	__property OnDisposeData;
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvlmat */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVLMAT)
using namespace Stnvlmat;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvlmatHPP
