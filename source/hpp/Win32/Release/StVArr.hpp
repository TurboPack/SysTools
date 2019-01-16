// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StVArr.pas' rev: 32.00 (Windows)

#ifndef StvarrHPP
#define StvarrHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stvarr
{
//-- forward type declarations -----------------------------------------------
struct TStCacheRec;
class DELPHICLASS TStVMatrix;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TStCacheRec
{
public:
	unsigned crRow;
	void *crRowData;
	int crTime;
	int crDirty;
};


typedef System::StaticArray<TStCacheRec, 134217727> TStCacheArray;

typedef TStCacheArray *PStCacheArray;

class PASCALIMPLEMENTATION TStVMatrix : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
protected:
	unsigned FRows;
	int FCacheRows;
	unsigned FCols;
	int FElSize;
	int vmRowSize;
	int vmCacheCnt;
	int vmCacheTime;
	TStCacheArray *vmCache;
	int vmDataF;
	virtual void __fastcall ForEachUntypedVar(Stbase::TIterateUntypedFunc Action, void * OtherData);
	virtual void __fastcall GetArraySizes(unsigned &RowCount, unsigned &ColCount, unsigned &ElSize);
	virtual void __fastcall SetArraySizes(unsigned RowCount, unsigned ColCount, unsigned ElSize);
	virtual bool __fastcall StoresUntypedVars(void);
	void __fastcall vmSetCacheRows(int CacheRows);
	void __fastcall vmAllocateCache(void);
	void __fastcall vmDeallocateCache(void);
	void __fastcall vmInvalidateCache(void);
	void __fastcall vmFlushCacheNode(int CacheIndex);
	int __fastcall vmIncCacheTime(void);
	bool __fastcall vmSearchCache(unsigned Row, int &CacheIndex);
	void * __fastcall vmGetRowData(unsigned Row, bool MakeDirty);
	void __fastcall vmWriteRow(unsigned Row, void * Data, bool Seek);
	void __fastcall vmSetRows(unsigned Rows);
	
public:
	__fastcall virtual TStVMatrix(unsigned Rows, unsigned Cols, unsigned ElementSize, int CacheRows, const System::UnicodeString DataFile, System::Word OpenMode);
	__fastcall virtual ~TStVMatrix(void);
	void __fastcall FlushCache(void);
	virtual int __fastcall HeaderSize(void);
	virtual void __fastcall WriteHeader(void);
	virtual void __fastcall ReadHeader(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear(void);
	void __fastcall Fill(const void *Value);
	void __fastcall Put(unsigned Row, unsigned Col, const void *Value);
	void __fastcall Get(unsigned Row, unsigned Col, void *Value);
	void __fastcall PutRow(unsigned Row, const void *RowValue);
	void __fastcall GetRow(unsigned Row, void *RowValue);
	void __fastcall ExchangeRows(unsigned Row1, unsigned Row2);
	void __fastcall SortRows(unsigned KeyCol, Stbase::TUntypedCompareFunc Compare);
	__property unsigned Rows = {read=FRows, write=vmSetRows, nodefault};
	__property int CacheRows = {read=FCacheRows, write=vmSetCacheRows, nodefault};
	__property unsigned Cols = {read=FCols, nodefault};
	__property int ElementSize = {read=FElSize, nodefault};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStVMatrix(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stvarr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STVARR)
using namespace Stvarr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StvarrHPP
