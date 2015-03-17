// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVDict.pas' rev: 29.00 (Windows)

#ifndef StnvdictHPP
#define StnvdictHPP

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
#include <StDict.hpp>
#include <StNVCont.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stnvdict
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNVDictionary;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStNVDictionary : public Stnvcont::TStNVContainerBase
{
	typedef Stnvcont::TStNVContainerBase inherited;
	
protected:
	Stdict::TStDictionary* FContainer;
	int FHashSize;
	int __fastcall GetHashSize(void);
	Stbase::TStStringCompareEvent __fastcall GetOnEqual(void);
	void __fastcall SetHashSize(int Value);
	void __fastcall SetOnEqual(Stbase::TStStringCompareEvent Value);
	virtual Stnvcont::TStDisposeDataEvent __fastcall GetOnDisposeData(void);
	virtual Stnvcont::TStLoadDataEvent __fastcall GetOnLoadData(void);
	virtual Stnvcont::TStStoreDataEvent __fastcall GetOnStoreData(void);
	virtual void __fastcall SetOnDisposeData(Stnvcont::TStDisposeDataEvent Value);
	virtual void __fastcall SetOnLoadData(Stnvcont::TStLoadDataEvent Value);
	virtual void __fastcall SetOnStoreData(Stnvcont::TStStoreDataEvent Value);
	
public:
	__fastcall virtual TStNVDictionary(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNVDictionary(void);
	__property Stdict::TStDictionary* Container = {read=FContainer};
	
__published:
	__property int HashSize = {read=GetHashSize, write=SetHashSize, default=509};
	__property Stbase::TStStringCompareEvent OnEqual = {read=GetOnEqual, write=SetOnEqual};
	__property OnDisposeData;
	__property OnLoadData;
	__property OnStoreData;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvdict */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVDICT)
using namespace Stnvdict;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvdictHPP
