// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StOStr.pas' rev: 28.00 (Windows)

#ifndef StostrHPP
#define StostrHPP

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
#include <StConst.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StStrL.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stostr
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStString;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStString : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	System::WideChar operator[](unsigned Index) { return AtIndex[Index]; }
	
protected:
	unsigned FAlloc;
	Stbase::BTable FBM;
	System::WideChar *FBMString;
	System::WideChar *FCursor;
	System::WideChar *FDelimiters;
	bool FEnableCursor;
	System::Classes::TStringList* FItems;
	bool FOneBased;
	unsigned FRepeatValue;
	bool FResetRepeat;
	System::WideChar FQuote;
	System::WideChar *FString;
	System::Byte FTabSize;
	System::WideChar *FTemp;
	unsigned FTempAlloc;
	unsigned FWrap;
	System::WideChar FLineTermChar;
	Stbase::TStLineTerminator FLineTerminator;
	void __fastcall SetLineTerm(const Stbase::TStLineTerminator Value);
	void __fastcall SetLineTermChar(const System::WideChar Value);
	void __fastcall AddIntToList(int Num);
	void __fastcall AllocTemp(unsigned Size);
	void __fastcall BMMakeTable(System::WideChar * S);
	void __fastcall CheckAlloc(unsigned Size);
	System::WideChar * __fastcall DesiredCursor(void);
	void __fastcall FixCursor(unsigned Pos, unsigned Size, bool Delete);
	System::WideChar __fastcall Get(unsigned Index);
	unsigned __fastcall GetAsciiCount(void);
	System::UnicodeString __fastcall GetAsShortStr(void);
	unsigned __fastcall GetCursorPos(void);
	System::UnicodeString __fastcall GetDelimiters(void);
	unsigned __fastcall GetLength(void);
	unsigned __fastcall GetRelativePos(unsigned Pos);
	System::UnicodeString __fastcall GetSoundex(void);
	unsigned __fastcall GetWordCount(void);
	void __fastcall Put(unsigned Index, System::WideChar Item);
	void __fastcall SetAllocLength(unsigned Value);
	void __fastcall SetAsShortStr(const System::UnicodeString Value);
	void __fastcall SetCursorPos(unsigned Value);
	void __fastcall SetDelimiters(System::UnicodeString Value);
	void __fastcall SetItems(System::Classes::TStringList* Value);
	unsigned __fastcall SuggestSize(unsigned Size);
	void __fastcall TempToString(void);
	void __fastcall UpdateCursor(unsigned Pos);
	System::UnicodeString __fastcall GetAsLongStr(void);
	void __fastcall SetAsLongStr(System::UnicodeString Value);
	System::Variant __fastcall GetAsVariant(void);
	void __fastcall SetAsVariant(const System::Variant &Value);
	int __fastcall MakeTerminator(System::WideChar * &Terminator);
	
public:
	__fastcall TStString(void);
	__fastcall virtual TStString(unsigned Size);
	__fastcall virtual TStString(const System::UnicodeString S);
	__fastcall virtual TStString(const System::WideChar * S);
	__fastcall virtual TStString(const System::Variant &S);
	__fastcall virtual ~TStString(void);
	void __fastcall AppendPChar(System::WideChar * S);
	void __fastcall AppendString(System::UnicodeString S);
	bool __fastcall AsciiPosition(unsigned N, unsigned &Pos);
	bool __fastcall BMSearch(const System::UnicodeString S, unsigned &Pos);
	bool __fastcall BMSearchUC(const System::UnicodeString S, unsigned &Pos);
	void __fastcall Center(unsigned Size);
	void __fastcall CenterCh(const System::WideChar C, unsigned Size);
	unsigned __fastcall CharCount(const System::WideChar C);
	bool __fastcall CharExists(const System::WideChar C);
	void __fastcall CharStr(const System::WideChar C, unsigned Size);
	void __fastcall ClearItems(void);
	void __fastcall CursorNextWord(void);
	void __fastcall CursorNextWordPrim(void);
	void __fastcall CursorPrevWord(void);
	void __fastcall CursorPrevWordPrim(void);
	void __fastcall CursorToEnd(void);
	void __fastcall DeleteAsciiAtCursor(void);
	void __fastcall DeleteAtCursor(unsigned Length);
	void __fastcall DeleteWordAtCursor(void);
	void __fastcall Detab(void);
	void __fastcall Entab(void);
	System::UnicodeString __fastcall ExtractAscii(unsigned N);
	System::UnicodeString __fastcall ExtractWord(unsigned N);
	void __fastcall Filter(const System::WideChar * Filters);
	System::UnicodeString __fastcall GetAsciiAtCursor(void);
	System::WideChar * __fastcall GetAsciiAtCursorZ(System::WideChar * Dest);
	System::WideChar * __fastcall GetAsPChar(System::WideChar * Dest);
	System::UnicodeString __fastcall GetWordAtCursor(void);
	System::WideChar * __fastcall GetWordAtCursorZ(System::WideChar * Dest);
	void __fastcall InsertLineTerminatorAtCursor(void);
	void __fastcall InsertLineTerminator(unsigned Pos);
	void __fastcall InsertPCharAtCursor(System::WideChar * S);
	void __fastcall InsertStringAtCursor(System::UnicodeString S);
	void __fastcall ItemsToString(void);
	void __fastcall LeftPad(unsigned Size);
	void __fastcall LeftPadCh(const System::WideChar C, unsigned Size);
	int __fastcall MakeLetterSet(void);
	void __fastcall MoveCursor(int Delta);
	void __fastcall Pack(void);
	void __fastcall Pad(unsigned Size);
	void __fastcall PadCh(const System::WideChar C, unsigned Size);
	void __fastcall ResetCursor(void);
	void __fastcall Scramble(const System::UnicodeString Key);
	void __fastcall SetAsPChar(System::WideChar * S);
	unsigned __fastcall SizeAsciiAtCursor(bool InclTrailers);
	unsigned __fastcall SizeWordAtCursor(bool InclTrailers);
	void __fastcall StrChDelete(unsigned Pos);
	void __fastcall StrChInsert(const System::WideChar C, unsigned Pos);
	bool __fastcall StrChPos(const System::WideChar C, int &Pos);
	void __fastcall StringToItems(void);
	void __fastcall StripLineTerminators(void);
	void __fastcall StrStDelete(const unsigned Pos, const unsigned Length);
	void __fastcall StrStInsert(const System::UnicodeString S, unsigned Pos);
	bool __fastcall StrStPos(const System::UnicodeString S, unsigned &Pos);
	void __fastcall Substitute(System::WideChar * FromStr, System::WideChar * ToStr);
	void __fastcall Trim(void);
	void __fastcall TrimLead(void);
	void __fastcall TrimSpaces(void);
	void __fastcall TrimTrail(void);
	bool __fastcall WordPosition(unsigned N, unsigned &Pos);
	void __fastcall WrapToItems(void);
	__property unsigned AllocLength = {read=FAlloc, write=SetAllocLength, nodefault};
	__property unsigned AsciiCount = {read=GetAsciiCount, nodefault};
	__property System::UnicodeString AsLongStr = {read=GetAsLongStr, write=SetAsLongStr};
	__property System::Variant AsVariant = {read=GetAsVariant, write=SetAsVariant};
	__property System::UnicodeString AsShortStr = {read=GetAsShortStr, write=SetAsShortStr};
	__property System::WideChar AtIndex[unsigned Index] = {read=Get, write=Put/*, default*/};
	__property unsigned CursorPos = {read=GetCursorPos, write=SetCursorPos, nodefault};
	__property System::UnicodeString Delimiters = {read=GetDelimiters, write=SetDelimiters};
	__property bool EnableCursor = {read=FEnableCursor, write=FEnableCursor, nodefault};
	__property unsigned Length = {read=GetLength, nodefault};
	__property System::WideChar LineTermChar = {read=FLineTermChar, write=SetLineTermChar, default=10};
	__property Stbase::TStLineTerminator LineTerminator = {read=FLineTerminator, write=SetLineTerm, default=3};
	__property System::Classes::TStringList* Items = {read=FItems, write=SetItems};
	__property bool OneBased = {read=FOneBased, write=FOneBased, nodefault};
	__property unsigned RepeatValue = {read=FRepeatValue, write=FRepeatValue, nodefault};
	__property bool ResetRepeat = {read=FResetRepeat, write=FResetRepeat, nodefault};
	__property System::UnicodeString Soundex = {read=GetSoundex};
	__property System::WideChar Quote = {read=FQuote, write=FQuote, nodefault};
	__property System::Byte TabSize = {read=FTabSize, write=FTabSize, nodefault};
	__property unsigned WordCount = {read=GetWordCount, nodefault};
	__property unsigned WrapColumn = {read=FWrap, write=FWrap, nodefault};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 DefAllocSize = System::Int8(0x8);
static const System::WideChar DefDelimiters = (System::WideChar)(0x20);
static const System::WideChar DefQuote = (System::WideChar)(0x27);
static const System::Int8 DefRepeatValue = System::Int8(0x1);
static const bool DefResetRepeat = true;
static const System::Int8 DefTabSize = System::Int8(0x8);
static const System::Int8 DefWrap = System::Int8(0x50);
}	/* namespace Stostr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STOSTR)
using namespace Stostr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StostrHPP
