// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StColl.pas' rev: 32.00 (Windows)

#ifndef StcollHPP
#define StcollHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StList.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stcoll
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPageDescriptor;
class DELPHICLASS TStCollection;
class DELPHICLASS TStSortedCollection;
//-- type declarations -------------------------------------------------------
typedef System::StaticArray<void *, 536870911> TPointerArray;

typedef TPointerArray *PPointerArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPageDescriptor : public Stlist::TStListNode
{
	typedef Stlist::TStListNode inherited;
	
protected:
	TPointerArray *pdPage;
	int pdStart;
	int pdCount;
	
public:
	__fastcall virtual TPageDescriptor(void * AData);
	__fastcall virtual ~TPageDescriptor(void);
};

#pragma pack(pop)

typedef bool __fastcall (*TCollIterateFunc)(Stbase::TStContainer* Container, void * Data, void * OtherData);

class PASCALIMPLEMENTATION TStCollection : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
public:
	void * operator[](int Index) { return this->Items[Index]; }
	
protected:
	Stlist::TStList* colPageList;
	int colPageElements;
	TPageDescriptor* colCachePage;
	void __fastcall colAdjustPagesAfter(TPageDescriptor* N, int Delta);
	void __fastcall colAtInsertInPage(TPageDescriptor* N, int PageIndex, void * AData);
	void __fastcall colAtDeleteInPage(TPageDescriptor* N, int PageIndex);
	int __fastcall colGetCount(void);
	int __fastcall colGetEfficiency(void);
	virtual void __fastcall ForEachPointer(Stbase::TIteratePointerFunc Action, void * OtherData);
	virtual bool __fastcall StoresPointers(void);
	
public:
	__fastcall virtual TStCollection(int PageElements);
	__fastcall virtual ~TStCollection(void);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Pack(void);
	void * __fastcall At(int Index);
	virtual int __fastcall IndexOf(void * Data);
	void __fastcall AtInsert(int Index, void * Data);
	void __fastcall AtPut(int Index, void * Data);
	virtual void __fastcall Insert(void * Data);
	void __fastcall AtDelete(int Index);
	void __fastcall Delete(void * Data);
	void * __fastcall Iterate(TCollIterateFunc Action, bool Up, void * OtherData);
	__property int Count = {read=colGetCount, nodefault};
	__property int Efficiency = {read=colGetEfficiency, nodefault};
	__property void * Items[int Index] = {read=At, write=AtPut/*, default*/};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStCollection(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


enum DECLSPEC_DENUM TSCSearch : unsigned char { SCSPageEmpty, SCSLessThanThisPage, SCSInThisPageRange, SCSFound, SCSGreaterThanThisPage };

class PASCALIMPLEMENTATION TStSortedCollection : public TStCollection
{
	typedef TStCollection inherited;
	
protected:
	bool FDuplicates;
	TSCSearch __fastcall scSearchPage(void * AData, TPageDescriptor* N, int &PageIndex);
	void __fastcall scSetDuplicates(bool D);
	
public:
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual int __fastcall IndexOf(void * Data);
	virtual void __fastcall Insert(void * Data);
	__property bool Duplicates = {read=FDuplicates, write=scSetDuplicates, nodefault};
public:
	/* TStCollection.Create */ inline __fastcall virtual TStSortedCollection(int PageElements) : TStCollection(PageElements) { }
	/* TStCollection.Destroy */ inline __fastcall virtual ~TStSortedCollection(void) { }
	
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStSortedCollection(Stbase::TStNodeClass NodeClass, int Dummy) : TStCollection(NodeClass, Dummy) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stcoll */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STCOLL)
using namespace Stcoll;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StcollHPP
