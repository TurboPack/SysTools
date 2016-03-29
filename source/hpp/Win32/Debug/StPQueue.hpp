// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StPQueue.pas' rev: 31.00 (Windows)

#ifndef StpqueueHPP
#define StpqueueHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <StConst.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stpqueue
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStPQueue;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<void *, 536870911> TStPQData;

typedef TStPQData *PStPQData;

class PASCALIMPLEMENTATION TStPQueue : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
protected:
	TStPQData *pqData;
	int pqCapacity;
	int pqDelta;
	virtual void __fastcall ForEachPointer(Stbase::TIteratePointerFunc Action, void * OtherData);
	virtual bool __fastcall StoresPointers(void);
	void __fastcall Expand(int Need);
	void __fastcall InsertMin(int I, void * Data);
	void __fastcall InsertMax(int I, void * Data);
	void __fastcall ModifiedInsert(int I, void * Data);
	
public:
	__fastcall virtual TStPQueue(int InitCapacity, int Delta);
	__fastcall virtual ~TStPQueue(void);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Clear(void);
	void __fastcall Insert(void * Data);
	void * __fastcall DeleteMin(void);
	void * __fastcall DeleteMax(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Join(TStPQueue* Q);
	void * __fastcall Iterate(Stbase::TIteratePointerFunc Action, void * OtherData);
	bool __fastcall Test(void);
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStPQueue(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


typedef System::TMetaClass* TStPQueueClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stpqueue */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STPQUEUE)
using namespace Stpqueue;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StpqueueHPP
