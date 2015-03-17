// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StList.pas' rev: 29.00 (Windows)

#ifndef StlistHPP
#define StlistHPP

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

namespace Stlist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStListNode;
class DELPHICLASS TStList;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStListNode : public Stbase::TStNode
{
	typedef Stbase::TStNode inherited;
	
protected:
	TStListNode* FNext;
	TStListNode* FPrev;
	
public:
	__fastcall virtual TStListNode(void * AData);
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TStListNode(void) { }
	
};


class PASCALIMPLEMENTATION TStList : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
public:
	TStListNode* operator[](int Index) { return Items[Index]; }
	
protected:
	TStListNode* FHead;
	TStListNode* FTail;
	int lsLastI;
	TStListNode* lsLastP;
	virtual void __fastcall ForEachPointer(Stbase::TIteratePointerFunc Action, void * OtherData);
	virtual bool __fastcall StoresPointers(void);
	
public:
	__fastcall virtual TStList(Stbase::TStNodeClass NodeClass);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Clear(void);
	TStListNode* __fastcall Append(void * Data);
	TStListNode* __fastcall Insert(void * Data);
	TStListNode* __fastcall Place(void * Data, TStListNode* P);
	TStListNode* __fastcall PlaceBefore(void * Data, TStListNode* P);
	TStListNode* __fastcall InsertSorted(void * Data);
	void __fastcall MoveToHead(TStListNode* P);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Join(TStListNode* P, TStList* L);
	TStList* __fastcall Split(TStListNode* P);
	void __fastcall Sort(void);
	void __fastcall Delete(TStListNode* P);
	TStListNode* __fastcall Next(TStListNode* P);
	TStListNode* __fastcall Prev(TStListNode* P);
	TStListNode* __fastcall Nth(int Index);
	TStListNode* __fastcall NthFrom(TStListNode* P, int Index);
	int __fastcall Posn(TStListNode* P);
	int __fastcall Distance(TStListNode* P1, TStListNode* P2);
	TStListNode* __fastcall Find(void * Data);
	TStListNode* __fastcall Iterate(Stbase::TIterateFunc Action, bool Up, void * OtherData);
	__property TStListNode* Head = {read=FHead};
	__property TStListNode* Tail = {read=FTail};
	__property TStListNode* Items[int Index] = {read=Nth/*, default*/};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStList(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	/* TStContainer.Destroy */ inline __fastcall virtual ~TStList(void) { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TStListClass);

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stlist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STLIST)
using namespace Stlist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StlistHPP
