// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StHASH.pas' rev: 31.00 (Windows)

#ifndef SthashHPP
#define SthashHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Winapi.Windows.hpp>
#include <StConst.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sthash
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStHashNode;
class DELPHICLASS TStHashTable;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStHashNode : public Stbase::TStNode
{
	typedef Stbase::TStNode inherited;
	
protected:
	TStHashNode* hnNext;
	void *hnValue;
	unsigned hnValSize;
	int FLRU;
	void * __fastcall GetValue(void);
	
public:
	__fastcall virtual TStHashNode(const void *AValue, unsigned AValSize, void * AData);
	__fastcall virtual ~TStHashNode(void);
	__property void * Value = {read=GetValue};
	__property int LRU = {read=FLRU, write=FLRU, nodefault};
public:
	/* TStNode.Create */ inline __fastcall virtual TStHashNode(void * AData) : Stbase::TStNode(AData) { }
	
};

#pragma pack(pop)

typedef System::StaticArray<TStHashNode*, 536870911> THashArray;

typedef THashArray *PHashArray;

typedef int __fastcall (*THashFunc)(const void *V, int Size);

class PASCALIMPLEMENTATION TStHashTable : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
protected:
	unsigned FValSize;
	int FHashSize;
	Stbase::TUntypedCompareFunc FEqual;
	THashFunc FHash;
	int FMaxNodes;
	THashArray *htHeads;
	THashArray *htTails;
	int htLRU;
	bool htIgnoreDups;
	void __fastcall htInsertNode(int H, TStHashNode* This);
	void __fastcall htIterate(Stbase::TIterateFunc Action, void * OtherData, int &H, TStHashNode* &Prev, TStHashNode* &This);
	void __fastcall htSetEqual(Stbase::TUntypedCompareFunc E);
	void __fastcall htSetHash(THashFunc H);
	void __fastcall htSetHashSize(int Size);
	void __fastcall htSetMaxNodes(int Nodes);
	void __fastcall htMoveToFront(int H, TStHashNode* Prev, TStHashNode* This);
	void __fastcall htFindNode(const void *V, int &H, TStHashNode* &Prev, TStHashNode* &This);
	void __fastcall htUpdateLRU(TStHashNode* This);
	void __fastcall htDeleteOldestNode(void);
	
public:
	__fastcall virtual TStHashTable(unsigned AValSize, int AHashSize);
	__fastcall virtual ~TStHashTable(void);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Clear(void);
	bool __fastcall Exists(const void *V, void * &Data);
	void __fastcall Add(const void *V, void * Data);
	void __fastcall Delete(const void *V);
	void __fastcall Update(const void *V, void * Data);
	bool __fastcall Find(void * Data, void *V);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Join(TStHashTable* H, bool IgnoreDups);
	TStHashNode* __fastcall Iterate(Stbase::TIterateFunc Action, void * OtherData);
	virtual void __fastcall NodeRemoved(const void *V, void * Data);
	int __fastcall BinCount(int H);
	__property Stbase::TUntypedCompareFunc Equal = {read=FEqual, write=htSetEqual};
	__property THashFunc Hash = {read=FHash, write=htSetHash};
	__property int HashSize = {read=FHashSize, write=htSetHashSize, nodefault};
	__property unsigned ValSize = {read=FValSize, nodefault};
	__property int MaxNodes = {read=FMaxNodes, write=htSetMaxNodes, nodefault};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStHashTable(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sthash */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STHASH)
using namespace Sthash;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SthashHPP
