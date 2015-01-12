// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVLMat.pas' rev: 28.00 (Windows)

#ifndef StnvlmatHPP
#define StnvlmatHPP

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
#include <StLArr.hpp>	// Pascal unit
#include <StNVCont.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stnvlmat
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStNVLMatrix;
class PASCALIMPLEMENTATION TStNVLMatrix : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stlarr::TStLMatrix* FContainer;
	unsigned FCols;
	unsigned FRows;
	unsigned FElementSize;
	bool __fastcall GetStoreable(void);
	void __fastcall SetCols(unsigned Value);
	void __fastcall SetRows(unsigned Value);
	void __fastcall SetElementSize(unsigned Value);
	void __fastcall SetStoreable(bool Value);
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
	__fastcall virtual TStNVLMatrix(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVLMatrix(void);
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
