// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNVCont.pas' rev: 29.00 (Windows)

#ifndef StnvcontHPP
#define StnvcontHPP

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

//-- user supplied -----------------------------------------------------------

namespace Stnvcont
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNVContainerBase;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TStContainerClass;

typedef void __fastcall (__closure *TStDisposeDataEvent)(System::TObject* Sender, void * Data);

typedef void __fastcall (__closure *TStLoadDataEvent)(System::TObject* Sender, System::Classes::TReader* Reader, void * &Data);

typedef void __fastcall (__closure *TStStoreDataEvent)(System::TObject* Sender, System::Classes::TWriter* Writer, void * Data);

class PASCALIMPLEMENTATION TStNVContainerBase : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	virtual Stbase::TStCompareEvent __fastcall GetOnCompare(void);
	virtual TStDisposeDataEvent __fastcall GetOnDisposeData(void);
	virtual TStLoadDataEvent __fastcall GetOnLoadData(void);
	virtual TStStoreDataEvent __fastcall GetOnStoreData(void);
	virtual void __fastcall SetOnCompare(Stbase::TStCompareEvent Value);
	virtual void __fastcall SetOnDisposeData(TStDisposeDataEvent Value);
	virtual void __fastcall SetOnLoadData(TStLoadDataEvent Value);
	virtual void __fastcall SetOnStoreData(TStStoreDataEvent Value);
	__property Stbase::TStCompareEvent OnCompare = {read=GetOnCompare, write=SetOnCompare};
	__property TStDisposeDataEvent OnDisposeData = {read=GetOnDisposeData, write=SetOnDisposeData};
	__property TStLoadDataEvent OnLoadData = {read=GetOnLoadData, write=SetOnLoadData};
	__property TStStoreDataEvent OnStoreData = {read=GetOnStoreData, write=SetOnStoreData};
public:
	/* TComponent.Create */ inline __fastcall virtual TStNVContainerBase(System::Classes::TComponent* AOwner) : Stbase::TStComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TStNVContainerBase(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnvcont */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNVCONT)
using namespace Stnvcont;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnvcontHPP
