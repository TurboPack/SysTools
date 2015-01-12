// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StBits.pas' rev: 28.00 (Windows)

#ifndef StbitsHPP
#define StbitsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stbits
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStBits;
typedef bool __fastcall (*TBitIterateFunc)(TStBits* Container, int N, void * OtherData);

class PASCALIMPLEMENTATION TStBits : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
public:
	bool operator[](int N) { return Items[N]; }
	
protected:
	int FMax;
	int btBlockSize;
	System::Byte *btBits;
	void __fastcall btSetMax(int Max);
	void __fastcall btRecount(void);
	System::PByte __fastcall btByte(int I);
	
public:
	__fastcall virtual TStBits(int Max);
	__fastcall virtual ~TStBits(void);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Clear(void);
	void __fastcall CopyBits(TStBits* B);
	void __fastcall SetBits(void);
	void __fastcall InvertBits(void);
	void __fastcall OrBits(TStBits* B);
	void __fastcall AndBits(TStBits* B);
	void __fastcall SubBits(TStBits* B);
	void __fastcall SetBit(int N);
	void __fastcall ClearBit(int N);
	void __fastcall ToggleBit(int N);
	void __fastcall ControlBit(int N, bool State);
	bool __fastcall BitIsSet(int N);
	int __fastcall FirstSet(void);
	int __fastcall LastSet(void);
	int __fastcall FirstClear(void);
	int __fastcall LastClear(void);
	int __fastcall NextSet(int N);
	int __fastcall PrevSet(int N);
	int __fastcall NextClear(int N);
	int __fastcall PrevClear(int N);
	int __fastcall Iterate(TBitIterateFunc Action, bool UseSetBits, bool Up, void * OtherData);
	int __fastcall IterateFrom(TBitIterateFunc Action, bool UseSetBits, bool Up, void * OtherData, int From);
	__property int Max = {read=FMax, write=btSetMax, nodefault};
	__property bool Items[int N] = {read=BitIsSet, write=ControlBit/*, default*/};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStBits(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stbits */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STBITS)
using namespace Stbits;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StbitsHPP
