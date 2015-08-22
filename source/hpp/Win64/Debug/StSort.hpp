// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StSort.pas' rev: 30.00 (Windows)

#ifndef StsortHPP
#define StsortHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <StConst.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stsort
{
//-- forward type declarations -----------------------------------------------
struct TMergeInfo;
class DELPHICLASS TStSorter;
//-- type declarations -------------------------------------------------------
typedef System::UnicodeString __fastcall (*TMergeNameFunc)(int MergeNum);

struct DECLSPEC_DRECORD TMergeInfo
{
public:
	int SortStatus;
	int MergeFiles;
	int MergeHandles;
	int MergePhases;
	int MaxDiskSpace;
	int HeapUsed;
};


typedef System::StaticArray<int, 5> TMergeIntArray;

typedef System::StaticArray<int, 5> TMergeLongArray;

typedef System::StaticArray<void *, 5> TMergePtrArray;

class PASCALIMPLEMENTATION TStSorter : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	int FCount;
	unsigned FRecLen;
	Stbase::TUntypedCompareFunc FCompare;
	TMergeNameFunc FMergeName;
	int sorRunCapacity;
	int sorRunCount;
	int sorGetIndex;
	void *sorPivotPtr;
	void *sorSwapPtr;
	int sorState;
	int sorMergeFileCount;
	int sorMergeFileMerged;
	int sorMergeOpenCount;
	int sorMergeBufSize;
	TMergeIntArray sorMergeFileNumber;
	TMergeIntArray sorMergeFiles;
	TMergeLongArray sorMergeBytesLoaded;
	TMergeLongArray sorMergeBytesUsed;
	TMergePtrArray sorMergeBases;
	TMergePtrArray sorMergePtrs;
	int sorOutFile;
	void *sorOutPtr;
	int sorOutBytesUsed;
	_RTL_CRITICAL_SECTION sorThreadSafe;
	void *sorBuffer;
	void __fastcall sorAllocBuffer(int MaxHeap);
	void __fastcall sorCreateNewMergeFile(int &Handle);
	void __fastcall sorDeleteMergeFiles(void);
	void * __fastcall sorElementPtr(int Index);
	void __fastcall sorFlushOutBuffer(void);
	void __fastcall sorFreeBuffer(void);
	void __fastcall sorGetMergeElementPtr(int M);
	int __fastcall sorGetNextElementIndex(void);
	void __fastcall sorMergeFileGroup(void);
	void __fastcall sorMoveElement(void * Src, void * Dest);
	void __fastcall sorOpenMergeFiles(void);
	void __fastcall sorPrimaryMerge(void);
	void __fastcall sorRunSort(int L, int R);
	void __fastcall sorStoreElement(void * Src);
	void __fastcall sorStoreNewMergeFile(void);
	void __fastcall sorSwapElements(int L, int R);
	void __fastcall sorSetCompare(Stbase::TUntypedCompareFunc Comp);
	void __fastcall EnterCS(void);
	void __fastcall LeaveCS(void);
	
public:
	__fastcall virtual TStSorter(int MaxHeap, unsigned RecLen);
	__fastcall virtual ~TStSorter(void);
	void __fastcall Put(const void *X);
	bool __fastcall Get(void *X);
	void __fastcall Reset(void);
	__property int Count = {read=FCount, nodefault};
	__property Stbase::TUntypedCompareFunc Compare = {read=FCompare, write=sorSetCompare};
	__property TMergeNameFunc MergeName = {read=FMergeName, write=FMergeName};
	__property unsigned RecLen = {read=FRecLen, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 MinRecsPerRun = System::Int8(0x4);
static const System::Int8 MergeOrder = System::Int8(0x5);
static const System::Int8 MedianThreshold = System::Int8(0x10);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DefaultMergeName(int MergeNum);
extern DELPHI_PACKAGE TMergeInfo __fastcall MergeInfo(int MaxHeap, unsigned RecLen, int NumRecs);
extern DELPHI_PACKAGE int __fastcall MinimumHeapToUse(unsigned RecLen);
extern DELPHI_PACKAGE int __fastcall OptimumHeapToUse(unsigned RecLen, int NumRecs);
extern DELPHI_PACKAGE void __fastcall ArraySort(void *A, unsigned RecLen, unsigned NumRecs, Stbase::TUntypedCompareFunc Compare);
}	/* namespace Stsort */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSORT)
using namespace Stsort;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StsortHPP
