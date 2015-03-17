// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StTree.pas' rev: 29.00 (Windows)

#ifndef SttreeHPP
#define SttreeHPP

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

namespace Sttree
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStTreeNode;
class DELPHICLASS TStTree;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStTreeNode : public Stbase::TStNode
{
	typedef Stbase::TStNode inherited;
	
protected:
	System::StaticArray<TStTreeNode*, 2> tnPos;
	int tnBal;
	
public:
	__fastcall virtual TStTreeNode(void * AData);
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TStTreeNode(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TStTree : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
protected:
	TStTreeNode* trRoot;
	bool trIgnoreDups;
	virtual void __fastcall ForEachPointer(Stbase::TIteratePointerFunc Action, void * OtherData);
	virtual bool __fastcall StoresPointers(void);
	void __fastcall trInsertNode(TStTreeNode* N);
	
public:
	__fastcall virtual TStTree(Stbase::TStNodeClass NodeClass);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Clear(void);
	TStTreeNode* __fastcall Insert(void * Data);
	void __fastcall Delete(void * Data);
	TStTreeNode* __fastcall Find(void * Data);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Join(TStTree* T, bool IgnoreDups);
	TStTree* __fastcall Split(void * Data);
	TStTreeNode* __fastcall Iterate(Stbase::TIterateFunc Action, bool Up, void * OtherData);
	TStTreeNode* __fastcall First(void);
	TStTreeNode* __fastcall Last(void);
	TStTreeNode* __fastcall Next(TStTreeNode* N);
	TStTreeNode* __fastcall Prev(TStTreeNode* N);
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStTree(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	/* TStContainer.Destroy */ inline __fastcall virtual ~TStTree(void) { }
	
};


typedef System::TMetaClass* TStTreeClass;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sttree */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STTREE)
using namespace Sttree;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SttreeHPP
