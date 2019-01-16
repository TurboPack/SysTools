// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StLArr.pas' rev: 32.00 (Windows)

#ifndef StlarrHPP
#define StlarrHPP

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
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stlarr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStLArray;
class DELPHICLASS TStLMatrix;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStLArray : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
protected:
	int FElSize;
	bool FElStorable;
	void *laData;
	virtual void __fastcall ForEachUntypedVar(Stbase::TIterateUntypedFunc Action, void * OtherData);
	virtual void __fastcall GetArraySizes(unsigned &RowCount, unsigned &ColCount, unsigned &ElSize);
	virtual void __fastcall SetArraySizes(unsigned RowCount, unsigned ColCount, unsigned ElSize);
	virtual bool __fastcall StoresUntypedVars(void);
	void __fastcall laSetCount(int Elements);
	
public:
	__fastcall TStLArray(int Elements, unsigned ElementSize);
	__fastcall virtual ~TStLArray(void);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear(void);
	void __fastcall Fill(const void *Value);
	void __fastcall Put(int El, const void *Value);
	void __fastcall Get(int El, void *Value);
	void __fastcall Exchange(int El1, int El2);
	void __fastcall Sort(Stbase::TUntypedCompareFunc Compare);
	__property int Count = {read=FCount, write=laSetCount, nodefault};
	__property int ElementSize = {read=FElSize, nodefault};
	__property bool ElementsStorable = {read=FElStorable, write=FElStorable, nodefault};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStLArray(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


class PASCALIMPLEMENTATION TStLMatrix : public Stbase::TStContainer
{
	typedef Stbase::TStContainer inherited;
	
protected:
	int FElSize;
	unsigned FCols;
	unsigned FRows;
	bool FElStorable;
	void *lmData;
	int lmRowSize;
	virtual void __fastcall ForEachUntypedVar(Stbase::TIterateUntypedFunc Action, void * OtherData);
	virtual void __fastcall GetArraySizes(unsigned &RowCount, unsigned &ColCount, unsigned &ElSize);
	virtual void __fastcall SetArraySizes(unsigned RowCount, unsigned ColCount, unsigned ElSize);
	virtual bool __fastcall StoresUntypedVars(void);
	void __fastcall lmSetRows(unsigned Rows);
	void __fastcall lmSetCols(unsigned Cols);
	
public:
	__fastcall TStLMatrix(unsigned Rows, unsigned Cols, unsigned ElementSize);
	__fastcall virtual ~TStLMatrix(void);
	DYNAMIC void __fastcall LoadFromStream(System::Classes::TStream* S);
	DYNAMIC void __fastcall StoreToStream(System::Classes::TStream* S);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear(void);
	void __fastcall Fill(const void *Value);
	void __fastcall Put(unsigned Row, unsigned Col, const void *Value);
	void __fastcall Get(unsigned Row, unsigned Col, void *Value);
	void __fastcall PutRow(unsigned Row, const void *RowValue);
	void __fastcall GetRow(unsigned Row, void *RowValue);
	void __fastcall ExchangeRows(unsigned Row1, unsigned Row2);
	void __fastcall SortRows(unsigned KeyCol, Stbase::TUntypedCompareFunc Compare);
	__property unsigned Rows = {read=FRows, write=lmSetRows, nodefault};
	__property unsigned Cols = {read=FCols, write=lmSetCols, nodefault};
	__property int ElementSize = {read=FElSize, nodefault};
	__property bool ElementsStorable = {read=FElStorable, write=FElStorable, nodefault};
public:
	/* TStContainer.CreateContainer */ inline __fastcall TStLMatrix(Stbase::TStNodeClass NodeClass, int Dummy) : Stbase::TStContainer(NodeClass, Dummy) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stlarr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STLARR)
using namespace Stlarr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StlarrHPP
