// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVTree.pas' rev: 28.00 (Windows)

#ifndef StnvtreeHPP
#define StnvtreeHPP

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
#include <StTree.hpp>	// Pascal unit
#include <StNVCont.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stnvtree
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStNVTree;
class PASCALIMPLEMENTATION TStNVTree : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Sttree::TStTree* FContainer;
	virtual Stbase::TStCompareEvent __fastcall GetOnCompare(void);
	virtual Stnvcont::TStDisposeDataEvent __fastcall GetOnDisposeData(void);
	virtual Stnvcont::TStLoadDataEvent __fastcall GetOnLoadData(void);
	virtual Stnvcont::TStStoreDataEvent __fastcall GetOnStoreData(void);
	virtual void __fastcall SetOnCompare(Stbase::TStCompareEvent Value);
	virtual void __fastcall SetOnDisposeData(Stnvcont::TStDisposeDataEvent Value);
	virtual void __fastcall SetOnLoadData(Stnvcont::TStLoadDataEvent Value);
	virtual void __fastcall SetOnStoreData(Stnvcont::TStStoreDataEvent Value);
	
public:
	__fastcall virtual TStNVTree(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVTree(void);
	__property Sttree::TStTree* Container = {read=FContainer};
	
__published:
	__property OnCompare;
	__property OnDisposeData;
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvtree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVTREE)
using namespace Stnvtree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvtreeHPP
