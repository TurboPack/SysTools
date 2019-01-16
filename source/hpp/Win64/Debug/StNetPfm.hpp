// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNetPfm.pas' rev: 32.00 (Windows)

#ifndef StnetpfmHPP
#define StnetpfmHPP

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

//-- user supplied -----------------------------------------------------------

namespace Stnetpfm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNetPerformance;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStCPFlags : unsigned char { cpfForNetCard, cpfNotRouted, cpfSlowLink, cpfDynamic };

typedef System::Set<TStCPFlags, TStCPFlags::cpfForNetCard, TStCPFlags::cpfDynamic> TStCPFlagsSet;

class PASCALIMPLEMENTATION TStNetPerformance : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
private:
	bool FGotData;
	System::UnicodeString FLocalName;
	System::UnicodeString FRemoteName;
	System::UnicodeString FProviderName;
	TStCPFlagsSet FFlags;
	unsigned FSpeed;
	unsigned FDelay;
	unsigned FOptDataSize;
	
protected:
	TStCPFlagsSet __fastcall GetFlags(void);
	unsigned __fastcall GetSpeed(void);
	unsigned __fastcall GetDelay(void);
	unsigned __fastcall GetOptDataSize(void);
	void __fastcall SetLocalName(System::UnicodeString Value);
	void __fastcall SetRemoteName(System::UnicodeString Value);
	void __fastcall SetProviderName(System::UnicodeString Value);
	
public:
	void __fastcall QueryPerformance(void);
	__property TStCPFlagsSet Flags = {read=GetFlags, nodefault};
	__property unsigned Speed = {read=GetSpeed, nodefault};
	__property unsigned Delay = {read=GetDelay, nodefault};
	__property unsigned OptDataSize = {read=GetOptDataSize, nodefault};
	
__published:
	__property System::UnicodeString LocalName = {read=FLocalName, write=SetLocalName};
	__property System::UnicodeString RemoteName = {read=FRemoteName, write=SetRemoteName};
	__property System::UnicodeString ProviderName = {read=FProviderName, write=SetProviderName};
public:
	/* TComponent.Create */ inline __fastcall virtual TStNetPerformance(System::Classes::TComponent* AOwner) : Stbase::TStComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TStNetPerformance(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnetpfm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNETPFM)
using namespace Stnetpfm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnetpfmHPP
