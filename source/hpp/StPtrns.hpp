// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StPtrns.pas' rev: 28.00 (Windows)

#ifndef StptrnsHPP
#define StptrnsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stptrns
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStSingleton;
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

class DELPHICLASS TStMediator;
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

class DELPHICLASS TStObserver;
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

class DELPHICLASS TStChain;
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
