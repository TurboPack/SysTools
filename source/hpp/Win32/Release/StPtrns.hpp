// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StPtrns.pas' rev: 30.00 (Windows)

#ifndef StptrnsHPP
#define StptrnsHPP

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

//-- user supplied -----------------------------------------------------------

namespace Stptrns
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStSingleton;
class DELPHICLASS TStMediator;
class DELPHICLASS TStObserver;
class DELPHICLASS TStChain;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStSingleton : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FRefCount;
	
public:
	__classmethod virtual System::TObject* __fastcall NewInstance();
	virtual void __fastcall FreeInstance(void);
	virtual void __fastcall AllocResources(void);
	virtual void __fastcall FreeResources(void);
public:
	/* TObject.Create */ inline __fastcall TStSingleton(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStSingleton(void) { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TStMediatorAction)(System::TObject* aInputData, System::TObject* aResultData);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStMediator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* FEventTable;
	
protected:
	int __fastcall GetCount(void);
	
public:
	__fastcall TStMediator(void);
	__fastcall virtual ~TStMediator(void);
	void __fastcall Add(const System::UnicodeString aEventName, TStMediatorAction aHandler);
	void __fastcall Remove(const System::UnicodeString aEventName);
	void __fastcall Handle(const System::UnicodeString aEventName, System::TObject* aInputData, System::TObject* aResultData);
	bool __fastcall IsHandled(const System::UnicodeString aEventName);
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TStObserverAction)(System::TObject* aInputData);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStObserver : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FEventTable;
	
protected:
	TStObserverAction __fastcall GetObserver(int Index);
	void __fastcall SetObserver(int Index, TStObserverAction InObserver);
	int __fastcall GetCount(void);
	
public:
	__fastcall TStObserver(void);
	__fastcall virtual ~TStObserver(void);
	void __fastcall Add(TStObserverAction aHandler);
	void __fastcall Remove(int aIndex);
	void __fastcall Notify(System::TObject* aInputData);
	__property TStObserverAction Handler[int aIndex] = {read=GetObserver, write=SetObserver};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

typedef void __fastcall (__closure *TStChainAction)(System::TObject* aInputData, System::TObject* aResultData, bool &aStopNow);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStChain : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TList* FEventTable;
	
protected:
	TStChainAction __fastcall GetHandler(int Index);
	void __fastcall SetHandler(int Index, TStChainAction InHandler);
	int __fastcall GetCount(void);
	
public:
	__fastcall TStChain(void);
	__fastcall virtual ~TStChain(void);
	void __fastcall Add(TStChainAction aHandler);
	void __fastcall Remove(int aIndex);
	void __fastcall Handle(System::TObject* aInputData, System::TObject* aResultData);
	void __fastcall Insert(int aIndex, TStChainAction aHandler);
	__property TStChainAction Handler[int aIndex] = {read=GetHandler, write=SetHandler};
	__property int Count = {read=GetCount, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stptrns */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STPTRNS)
using namespace Stptrns;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StptrnsHPP
