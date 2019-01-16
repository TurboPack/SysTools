// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StStrms.pas' rev: 32.00 (Windows)

#ifndef StstrmsHPP
#define StstrmsHPP

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
#include <StBase.hpp>
#include <StConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ststrms
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStBufferedStream;
class DELPHICLASS TStAnsiTextStream;
class DELPHICLASS TStMemoryMappedFile;
//-- type declarations -------------------------------------------------------
typedef int TStMemSize;

class PASCALIMPLEMENTATION TStBufferedStream : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
private:
	int FBufCount;
	char *FBuffer;
	int FBufOfs;
	int FBufPos;
	int FBufSize;
	bool FDirty;
	__int64 FSize;
	System::Classes::TStream* FStream;
	
protected:
	void __fastcall bsSetStream(System::Classes::TStream* aValue);
	virtual void __fastcall bsInitForNewStream(void);
	bool __fastcall bsReadChar(char &aCh);
	void __fastcall bsReadFromStream(void);
	void __fastcall bsWriteToStream(void);
	virtual void __fastcall SetSize(int NewSize)/* overload */;
	
public:
	__fastcall TStBufferedStream(System::Classes::TStream* aStream);
	__fastcall TStBufferedStream(void);
	__fastcall virtual ~TStBufferedStream(void);
	virtual int __fastcall Read(void *Buffer, int Count)/* overload */;
	virtual int __fastcall Seek(int Offset, System::Word Origin)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count)/* overload */;
	__property __int64 FastSize = {read=FSize};
	__property System::Classes::TStream* Stream = {read=FStream, write=bsSetStream};
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  SetSize(const __int64 NewSize){ System::Classes::TStream::SetSize(NewSize); }
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline __int64 __fastcall  Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline __int64 __fastcall  Seek _DEPRECATED_ATTRIBUTE0 (const __int64 Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	
};


class PASCALIMPLEMENTATION TStAnsiTextStream : public TStBufferedStream
{
	typedef TStBufferedStream inherited;
	
private:
	char FLineEndCh;
	int FLineLen;
	Stbase::TStLineTerminator FLineTerm;
	char *FFixedLine;
	int FLineCount;
	int FLineCurrent;
	int FLineCurOfs;
	System::Classes::TList* FLineIndex;
	int FLineInxStep;
	int FLineInxTop;
	
protected:
	int __fastcall atsGetLineCount(void);
	void __fastcall atsSetLineTerm(Stbase::TStLineTerminator aValue);
	void __fastcall atsSetLineEndCh(char aValue);
	void __fastcall atsSetLineLen(int aValue);
	void __fastcall atsGetLine(int &aStartPos, int &aEndPos, int &aLen);
	void __fastcall atsResetLineIndex(void);
	virtual void __fastcall bsInitForNewStream(void);
	
public:
	__fastcall TStAnsiTextStream(System::Classes::TStream* aStream);
	__fastcall virtual ~TStAnsiTextStream(void);
	bool __fastcall AtEndOfStream(void);
	System::AnsiString __fastcall ReadLine(void);
	int __fastcall ReadLineArray(char * aCharArray, int aLen);
	char * __fastcall ReadLineZ(char * aSt, int aMaxLen);
	int __fastcall SeekNearestLine(int aOffset);
	int __fastcall SeekLine(int aLineNum);
	void __fastcall WriteLine(const System::UnicodeString aSt)/* overload */;
	void __fastcall WriteLine(const System::AnsiString aSt)/* overload */;
	void __fastcall WriteLineArray(char * aCharArray, int aLen);
	void __fastcall WriteLineZ(char * aSt);
	__property int FixedLineLength = {read=FLineLen, write=atsSetLineLen, nodefault};
	__property int LineCount = {read=atsGetLineCount, nodefault};
	__property char LineTermChar = {read=FLineEndCh, write=atsSetLineEndCh, nodefault};
	__property Stbase::TStLineTerminator LineTerminator = {read=FLineTerm, write=atsSetLineTerm, nodefault};
public:
	/* TStBufferedStream.CreateEmpty */ inline __fastcall TStAnsiTextStream(void) : TStBufferedStream() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStMemoryMappedFile : public System::Classes::TStream
{
	typedef System::Classes::TStream inherited;
	
protected:
	void *FBuffer;
	System::Word FHeaderSize;
	unsigned FDataSize;
	NativeUInt FHandle;
	NativeUInt FMapObj;
	unsigned FMaxHi;
	unsigned FMaxLo;
	NativeUInt FMutex;
	unsigned FPos;
	bool FReadOnly;
	bool FSharedData;
	unsigned __fastcall GetDataSize(void);
	
public:
	__fastcall TStMemoryMappedFile(const System::UnicodeString FileName, unsigned MaxSize, bool ReadOnly, bool SharedData);
	__fastcall virtual ~TStMemoryMappedFile(void);
	virtual int __fastcall Read(void *Buffer, int Count)/* overload */;
	virtual int __fastcall Seek(int Offset, System::Word Origin)/* overload */;
	virtual int __fastcall Write(const void *Buffer, int Count)/* overload */;
	__property unsigned DataSize = {read=GetDataSize, nodefault};
	__property unsigned MaxSize = {read=FMaxLo, nodefault};
	__property unsigned Position = {read=FPos, nodefault};
	__property bool ReadOnly = {read=FReadOnly, nodefault};
	__property bool SharedData = {read=FSharedData, nodefault};
	/* Hoisted overloads: */
	
public:
	inline int __fastcall  Read(System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Read(Buffer, Offset, Count); }
	inline int __fastcall  Read(System::DynamicArray<System::Byte> &Buffer, int Count){ return System::Classes::TStream::Read(Buffer, Count); }
	inline __int64 __fastcall  Seek(const __int64 Offset, System::Classes::TSeekOrigin Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline __int64 __fastcall  Seek _DEPRECATED_ATTRIBUTE0 (const __int64 Offset, System::Word Origin){ return System::Classes::TStream::Seek(Offset, Origin); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Offset, int Count){ return System::Classes::TStream::Write(Buffer, Offset, Count); }
	inline int __fastcall  Write(const System::DynamicArray<System::Byte> Buffer, int Count){ return System::Classes::TStream::Write(Buffer, Count); }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ststrms */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSTRMS)
using namespace Ststrms;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StstrmsHPP
