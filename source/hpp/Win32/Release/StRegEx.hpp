// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StRegEx.pas' rev: 33.00 (Windows)

#ifndef StregexHPP
#define StregexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StStrms.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stregex
{
//-- forward type declarations -----------------------------------------------
struct TMatchPosition;
struct TStPatRecord;
class DELPHICLASS TStNodeHeap;
class DELPHICLASS TStStreamRegEx;
class DELPHICLASS TStRegEx;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TMatchPosition
{
public:
	unsigned StartPos;
	unsigned EndPos;
	unsigned Length;
	unsigned LineNum;
};
#pragma pack(pop)


enum DECLSPEC_DENUM TStOutputOption : unsigned char { ooUnselected, ooModified, ooCountOnly };

typedef System::Set<TStOutputOption, TStOutputOption::ooUnselected, TStOutputOption::ooCountOnly> TStOutputOptions;

enum DECLSPEC_DENUM TStTokens : unsigned char { tknNil, tknLitChar, tknCharClass, tknNegCharClass, tknClosure, tknMaybeOne, tknAnyChar, tknBegOfLine, tknEndOfLine, tknGroup, tknBegTag, tknEndTag, tknDitto };

typedef TStPatRecord *PStPatRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TStPatRecord
{
public:
	System::UnicodeString *StrPtr;
	TStPatRecord *NestedPattern;
	TStPatRecord *NextPattern;
	TStTokens Token;
	System::WideChar OneChar;
	bool NextOK;
};
#pragma pack(pop)


typedef System::Int8 TStTagLevel;

typedef System::StaticArray<TStTagLevel, 1024> TStFlag;

typedef void __fastcall (__closure *TStOnRegExProgEvent)(System::TObject* Sender, System::Word Percent);

typedef void __fastcall (__closure *TStOnMatchEvent)(System::TObject* Sender, const TMatchPosition &REPosition);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNodeHeap : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TStPatRecord *FFreeList;
	
protected:
	void __fastcall nhClearHeap();
	PStPatRecord __fastcall nhDeepCloneNode(PStPatRecord aNode);
	
public:
	__fastcall TStNodeHeap();
	__fastcall virtual ~TStNodeHeap();
	PStPatRecord __fastcall AllocNode();
	void __fastcall FreeNode(PStPatRecord aNode);
	PStPatRecord __fastcall CloneNode(PStPatRecord aNode);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TStStreamRegEx : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FAvoid;
	bool FIgnoreCase;
	Ststrms::TStAnsiTextStream* FInTextStream;
	unsigned FInFileSize;
	System::Classes::TStream* FInputStream;
	System::WideChar *FInLineBuf;
	unsigned FInLineCount;
	unsigned FInLineNum;
	System::WideChar FInLineTermChar;
	Stbase::TStLineTerminator FInLineTerminator;
	int FInLineLength;
	bool FLineNumbers;
	unsigned FLinesPerSec;
	unsigned FMatchCount;
	System::Classes::TStringList* FMatchPatSL;
	System::WideChar *FMatchPatStr;
	TStPatRecord *FMatchPatPtr;
	unsigned FMaxLineLength;
	TStNodeHeap* FNodes;
	TStOnMatchEvent FOnMatch;
	int FOutLineLength;
	System::WideChar FOutLineTermChar;
	Stbase::TStLineTerminator FOutLineTerminator;
	unsigned FReplaceCount;
	System::Classes::TStringList* FReplacePatSL;
	System::WideChar *FReplacePatStr;
	TStPatRecord *FReplacePatPtr;
	TStOnRegExProgEvent FOnProgress;
	System::Classes::TStream* FOutputStream;
	Ststrms::TStAnsiTextStream* FOutTextStream;
	System::WideChar *FOutLineBuf;
	TStOutputOptions FOutputOptions;
	System::Classes::TStringList* FSelAvoidPatSL;
	System::WideChar *FSelAvoidPatStr;
	TStPatRecord *FSelAvoidPatPtr;
	unsigned FSelectCount;
	void __fastcall AddTokenToPattern(PStPatRecord &PatRec, PStPatRecord LastPatRec, TStTokens Token, System::UnicodeString S);
	void __fastcall AddTokenToReplace(PStPatRecord &PatRec, PStPatRecord LastPatRec, TStTokens Token, const System::UnicodeString S);
	System::WideChar * __fastcall AppendS(System::WideChar * Dest, System::WideChar * S1, System::WideChar * S2, unsigned Count);
	bool __fastcall BuildAllPatterns();
	bool __fastcall BuildPatternStr(System::WideChar * &PStr, int &Len, System::Classes::TStringList* SL);
	System::UnicodeString __fastcall ConvertMaskToRegEx(const System::UnicodeString S);
	void __fastcall DisposeItems(PStPatRecord &Data);
	void __fastcall InsertLineNumber(System::WideChar * Dest, const System::WideChar * S, int LineNum);
	bool __fastcall GetPattern(System::WideChar * &Pattern, PStPatRecord &PatList);
	bool __fastcall GetReplace(System::WideChar * Pattern, PStPatRecord &PatList);
	int __fastcall MakePattern(System::WideChar * &Pattern, int Start, System::WideChar Delim, bool &TagOn, PStPatRecord &PatList);
	int __fastcall MakeReplacePattern(System::WideChar * Pattern, int Start, System::WideChar Delim, PStPatRecord &PatList);
	bool __fastcall FindMatch(System::WideChar * &Buf, PStPatRecord PatPtr, TMatchPosition &REPosition);
	bool __fastcall MatchOnePatternElement(System::WideChar * &Buf, int &I, bool &TagOn, int &TagNum, PStPatRecord PatPtr);
	bool __fastcall ProcessLine(System::WideChar * Buf, int Len, int LineNum, bool CheckOnly, TMatchPosition &REPosition);
	int __fastcall SearchMatchPattern(System::WideChar * &Buf, int OffSet, bool &TagOn, int &TagNum, PStPatRecord PatPtr);
	void __fastcall SetMatchPatSL(System::Classes::TStringList* Value);
	void __fastcall SetOptions(TStOutputOptions Value);
	void __fastcall SetReplacePatSL(System::Classes::TStringList* Value);
	void __fastcall SetSelAvoidPatSL(System::Classes::TStringList* Value);
	void __fastcall SubLine(System::WideChar * Buf);
	bool __fastcall SubLineFindTag(System::WideChar * Buf, int I, int IEnd, int TagNum, TStFlag &Flags, int &IStart, int &IStop);
	bool __fastcall SubLineMatchOne(System::WideChar * Buf, TStFlag &Flags, bool &TagOn, int &I, int &TagNum, PStPatRecord PatPtr);
	int __fastcall SubLineMatchPattern(System::WideChar * Buf, TStFlag &Flags, bool &TagOn, int &TagNum, int OffSet, PStPatRecord PatPtr);
	void __fastcall SubLineWrite(System::WideChar * Buf, System::WideChar * S, PStPatRecord RepRec, int I, int IEnd, TStFlag &Flags);
	
public:
	__property System::Classes::TStream* InputStream = {read=FInputStream, write=FInputStream};
	__property System::Classes::TStream* OutputStream = {read=FOutputStream, write=FOutputStream};
	__fastcall TStStreamRegEx();
	__fastcall virtual ~TStStreamRegEx();
	bool __fastcall CheckString(const System::UnicodeString S, TMatchPosition &REPosition);
	bool __fastcall FileMasksToRegEx(System::UnicodeString Masks);
	bool __fastcall Execute();
	bool __fastcall ReplaceString(System::UnicodeString &S, TMatchPosition &REPosition);
	__property bool Avoid = {read=FAvoid, write=FAvoid, nodefault};
	__property bool IgnoreCase = {read=FIgnoreCase, write=FIgnoreCase, nodefault};
	__property int InFixedLineLength = {read=FInLineLength, write=FInLineLength, nodefault};
	__property System::WideChar InLineTermChar = {read=FInLineTermChar, write=FInLineTermChar, nodefault};
	__property Stbase::TStLineTerminator InLineTerminator = {read=FInLineTerminator, write=FInLineTerminator, nodefault};
	__property unsigned LineCount = {read=FInLineCount, nodefault};
	__property bool LineNumbers = {read=FLineNumbers, write=FLineNumbers, nodefault};
	__property unsigned LinesMatched = {read=FMatchCount, nodefault};
	__property unsigned LinesPerSecond = {read=FLinesPerSec, nodefault};
	__property unsigned LinesReplaced = {read=FReplaceCount, nodefault};
	__property unsigned LinesSelected = {read=FSelectCount, nodefault};
	__property System::Classes::TStringList* MatchPattern = {read=FMatchPatSL, write=SetMatchPatSL};
	__property unsigned MaxLineLength = {read=FMaxLineLength, write=FMaxLineLength, nodefault};
	__property TStOnMatchEvent OnMatch = {read=FOnMatch, write=FOnMatch};
	__property TStOnRegExProgEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property int OutFixedLineLength = {read=FOutLineLength, write=FOutLineLength, nodefault};
	__property System::WideChar OutLineTermChar = {read=FOutLineTermChar, write=FOutLineTermChar, nodefault};
	__property Stbase::TStLineTerminator OutLineTerminator = {read=FOutLineTerminator, write=FOutLineTerminator, nodefault};
	__property TStOutputOptions OutputOptions = {read=FOutputOptions, write=SetOptions, nodefault};
	__property System::Classes::TStringList* ReplacePattern = {read=FReplacePatSL, write=SetReplacePatSL};
	__property System::Classes::TStringList* SelAvoidPattern = {read=FSelAvoidPatSL, write=SetSelAvoidPatSL};
};


class PASCALIMPLEMENTATION TStRegEx : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	bool FAvoid;
	bool FIgnoreCase;
	unsigned FInFileSize;
	System::Classes::TFileStream* FInFileStream;
	unsigned FInLineCount;
	System::WideChar FInLineTermChar;
	Stbase::TStLineTerminator FInLineTerminator;
	int FInFixedLineLength;
	System::UnicodeString FInputFile;
	bool FLineNumbers;
	unsigned FLinesPerSec;
	unsigned FMatchCount;
	System::Classes::TStringList* FMatchPatSL;
	System::WideChar *FMatchPatStr;
	TStPatRecord *FMatchPatPtr;
	unsigned FMaxLineLength;
	TStNodeHeap* FNodes;
	TStOnRegExProgEvent FOnProgress;
	TStOnMatchEvent FOnMatch;
	System::Classes::TFileStream* FOutFileStream;
	Ststrms::TStAnsiTextStream* FOutTextStream;
	System::WideChar *FOutLineBuf;
	int FOutFixedLineLength;
	System::WideChar FOutLineTermChar;
	Stbase::TStLineTerminator FOutLineTerminator;
	System::UnicodeString FOutputFile;
	TStOutputOptions FOutputOptions;
	unsigned FReplaceCount;
	System::Classes::TStringList* FReplacePatSL;
	System::WideChar *FReplacePatStr;
	TStPatRecord *FReplacePatPtr;
	System::Classes::TStringList* FSelAvoidPatSL;
	System::WideChar *FSelAvoidPatStr;
	TStPatRecord *FSelAvoidPatPtr;
	unsigned FSelectCount;
	TStStreamRegEx* FStream;
	void __fastcall SetMatchPatSL(System::Classes::TStringList* Value);
	void __fastcall SetOptions(TStOutputOptions Value);
	void __fastcall SetReplacePatSL(System::Classes::TStringList* Value);
	void __fastcall SetSelAvoidPatSL(System::Classes::TStringList* Value);
	void __fastcall SetStreamProperties();
	
public:
	__fastcall virtual TStRegEx(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStRegEx();
	bool __fastcall CheckString(const System::UnicodeString S, TMatchPosition &REPosition);
	bool __fastcall FileMasksToRegEx(const System::UnicodeString Masks);
	bool __fastcall Execute();
	bool __fastcall ReplaceString(System::UnicodeString &S, TMatchPosition &REPosition);
	__property unsigned LineCount = {read=FInLineCount, nodefault};
	__property unsigned LinesMatched = {read=FMatchCount, nodefault};
	__property unsigned LinesPerSecond = {read=FLinesPerSec, nodefault};
	__property unsigned LinesReplaced = {read=FReplaceCount, nodefault};
	__property unsigned LinesSelected = {read=FSelectCount, nodefault};
	__property unsigned MaxLineLength = {read=FMaxLineLength, write=FMaxLineLength, nodefault};
	
__published:
	__property bool Avoid = {read=FAvoid, write=FAvoid, default=0};
	__property bool IgnoreCase = {read=FIgnoreCase, write=FIgnoreCase, default=0};
	__property int InFixedLineLength = {read=FInFixedLineLength, write=FInFixedLineLength, default=80};
	__property System::WideChar InLineTermChar = {read=FInLineTermChar, write=FInLineTermChar, default=10};
	__property Stbase::TStLineTerminator InLineTerminator = {read=FInLineTerminator, write=FInLineTerminator, default=3};
	__property System::UnicodeString InputFile = {read=FInputFile, write=FInputFile};
	__property bool LineNumbers = {read=FLineNumbers, write=FLineNumbers, default=0};
	__property System::Classes::TStringList* MatchPattern = {read=FMatchPatSL, write=SetMatchPatSL};
	__property TStOnMatchEvent OnMatch = {read=FOnMatch, write=FOnMatch};
	__property TStOnRegExProgEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property int OutFixedLineLength = {read=FOutFixedLineLength, write=FOutFixedLineLength, default=80};
	__property System::WideChar OutLineTermChar = {read=FOutLineTermChar, write=FOutLineTermChar, default=10};
	__property Stbase::TStLineTerminator OutLineTerminator = {read=FOutLineTerminator, write=FOutLineTerminator, default=3};
	__property System::UnicodeString OutputFile = {read=FOutputFile, write=FOutputFile};
	__property TStOutputOptions OutputOptions = {read=FOutputOptions, write=SetOptions, nodefault};
	__property System::Classes::TStringList* ReplacePattern = {read=FReplacePatSL, write=SetReplacePatSL};
	__property System::Classes::TStringList* SelAvoidPattern = {read=FSelAvoidPatSL, write=SetSelAvoidPatSL};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString StWordDelimString;
extern DELPHI_PACKAGE System::UnicodeString StHexDigitString;
}	/* namespace Stregex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STREGEX)
using namespace Stregex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StregexHPP
