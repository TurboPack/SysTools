// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StRegEx.pas' rev: 28.00 (Windows)

#ifndef StregexHPP
#define StregexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Forms.hpp>	// Pascal unit
#include <Vcl.Dialogs.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StStrms.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stregex
{
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

struct TStPatRecord;
typedef TStPatRecord *PStPatRecord;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TStPatRecord
{
public:
	System::ShortString *StrPtr;
	TStPatRecord *NestedPattern;
	TStPatRecord *NextPattern;
	TStTokens Token;
	char OneChar;
	bool NextOK;
};
#pragma pack(pop)


typedef System::Int8 TStTagLevel;

typedef System::StaticArray<TStTagLevel, 1024> TStFlag;

typedef void __fastcall (__closure *TStOnRegExProgEvent)(System::TObject* Sender, System::Word Percent);

typedef void __fastcall (__closure *TStOnMatchEvent)(System::TObject* Sender, const TMatchPosition &REPosition);

class DELPHICLASS TStNodeHeap;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStNodeHeap : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TStPatRecord *FFreeList;
	
protected:
	void __fastcall nhClearHeap(void);
	PStPatRecord __fastcall nhDeepCloneNode(PStPatRecord aNode);
	
public:
	__fastcall TStNodeHeap(void);
	__fastcall virtual ~TStNodeHeap(void);
	PStPatRecord __fastcall AllocNode(void);
	void __fastcall FreeNode(PStPatRecord aNode);
	PStPatRecord __fastcall CloneNode(PStPatRecord aNode);
};

#pragma pack(pop)

class DELPHICLASS TStStreamRegEx;
class PASCALIMPLEMENTATION TStStreamRegEx : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FAvoid;
	bool FIgnoreCase;
	Ststrms::TStAnsiTextStream* FInTextStream;
	unsigned FInFileSize;
	System::Classes::TStream* FInputStream;
	char *FInLineBuf;
	unsigned FInLineCount;
	unsigned FInLineNum;
	char FInLineTermChar;
	Stbase::TStLineTerminator FInLineTerminator;
	int FInLineLength;
	bool FLineNumbers;
	unsigned FLinesPerSec;
	unsigned FMatchCount;
	System::Classes::TStringList* FMatchPatSL;
	char *FMatchPatStr;
	TStPatRecord *FMatchPatPtr;
	unsigned FMaxLineLength;
	TStNodeHeap* FNodes;
	TStOnMatchEvent FOnMatch;
	int FOutLineLength;
	char FOutLineTermChar;
	Stbase::TStLineTerminator FOutLineTerminator;
	unsigned FReplaceCount;
	System::Classes::TStringList* FReplacePatSL;
	char *FReplacePatStr;
	TStPatRecord *FReplacePatPtr;
	TStOnRegExProgEvent FOnProgress;
	System::Classes::TStream* FOutputStream;
	Ststrms::TStAnsiTextStream* FOutTextStream;
	char *FOutLineBuf;
	TStOutputOptions FOutputOptions;
	System::Classes::TStringList* FSelAvoidPatSL;
	char *FSelAvoidPatStr;
	TStPatRecord *FSelAvoidPatPtr;
	unsigned FSelectCount;
	void __fastcall AddTokenToPattern(PStPatRecord &PatRec, PStPatRecord LastPatRec, TStTokens Token, System::ShortString &S);
	void __fastcall AddTokenToReplace(PStPatRecord &PatRec, PStPatRecord LastPatRec, TStTokens Token, const System::ShortString &S);
	char * __fastcall AppendS(char * Dest, char * S1, char * S2, unsigned Count);
	bool __fastcall BuildAllPatterns(void);
	bool __fastcall BuildPatternStr(char * &PStr, int &Len, System::Classes::TStringList* SL);
	System::AnsiString __fastcall ConvertMaskToRegEx(const System::AnsiString S);
	void __fastcall DisposeItems(PStPatRecord &Data);
	void __fastcall InsertLineNumber(char * Dest, const char * S, int LineNum);
	bool __fastcall GetPattern(char * &Pattern, PStPatRecord &PatList);
	bool __fastcall GetReplace(char * Pattern, PStPatRecord &PatList);
	int __fastcall MakePattern(char * &Pattern, int Start, char Delim, bool &TagOn, PStPatRecord &PatList);
	int __fastcall MakeReplacePattern(char * Pattern, int Start, char Delim, PStPatRecord &PatList);
	bool __fastcall FindMatch(char * &Buf, PStPatRecord PatPtr, TMatchPosition &REPosition);
	bool __fastcall MatchOnePatternElement(char * &Buf, int &I, bool &TagOn, int &TagNum, PStPatRecord PatPtr);
	bool __fastcall ProcessLine(char * Buf, int Len, int LineNum, bool CheckOnly, TMatchPosition &REPosition);
	int __fastcall SearchMatchPattern(char * &Buf, int OffSet, bool &TagOn, int &TagNum, PStPatRecord PatPtr);
	void __fastcall SetMatchPatSL(System::Classes::TStringList* Value);
	void __fastcall SetOptions(TStOutputOptions Value);
	void __fastcall SetReplacePatSL(System::Classes::TStringList* Value);
	void __fastcall SetSelAvoidPatSL(System::Classes::TStringList* Value);
	void __fastcall SubLine(char * Buf);
	bool __fastcall SubLineFindTag(char * Buf, int I, int IEnd, int TagNum, TStFlag &Flags, int &IStart, int &IStop);
	bool __fastcall SubLineMatchOne(char * Buf, TStFlag &Flags, bool &TagOn, int &I, int &TagNum, PStPatRecord PatPtr);
	int __fastcall SubLineMatchPattern(char * Buf, TStFlag &Flags, bool &TagOn, int &TagNum, int OffSet, PStPatRecord PatPtr);
	void __fastcall SubLineWrite(char * Buf, char * S, PStPatRecord RepRec, int I, int IEnd, TStFlag &Flags);
	
public:
	__property System::Classes::TStream* InputStream = {read=FInputStream, write=FInputStream};
	__property System::Classes::TStream* OutputStream = {read=FOutputStream, write=FOutputStream};
	__fastcall TStStreamRegEx(void);
	__fastcall virtual ~TStStreamRegEx(void);
	bool __fastcall CheckString(const System::AnsiString S, TMatchPosition &REPosition);
	bool __fastcall FileMasksToRegEx(System::AnsiString Masks);
	bool __fastcall Execute(void);
	bool __fastcall ReplaceString(System::AnsiString &S, TMatchPosition &REPosition);
	__property bool Avoid = {read=FAvoid, write=FAvoid, nodefault};
	__property bool IgnoreCase = {read=FIgnoreCase, write=FIgnoreCase, nodefault};
	__property int InFixedLineLength = {read=FInLineLength, write=FInLineLength, nodefault};
	__property char InLineTermChar = {read=FInLineTermChar, write=FInLineTermChar, nodefault};
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
	__property char OutLineTermChar = {read=FOutLineTermChar, write=FOutLineTermChar, nodefault};
	__property Stbase::TStLineTerminator OutLineTerminator = {read=FOutLineTerminator, write=FOutLineTerminator, nodefault};
	__property TStOutputOptions OutputOptions = {read=FOutputOptions, write=SetOptions, nodefault};
	__property System::Classes::TStringList* ReplacePattern = {read=FReplacePatSL, write=SetReplacePatSL};
	__property System::Classes::TStringList* SelAvoidPattern = {read=FSelAvoidPatSL, write=SetSelAvoidPatSL};
};


class DELPHICLASS TStRegEx;
class PASCALIMPLEMENTATION TStRegEx : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	bool FAvoid;
	bool FIgnoreCase;
	unsigned FInFileSize;
	System::Classes::TFileStream* FInFileStream;
	unsigned FInLineCount;
	char FInLineTermChar;
	Stbase::TStLineTerminator FInLineTerminator;
	int FInFixedLineLength;
	System::UnicodeString FInputFile;
	bool FLineNumbers;
	unsigned FLinesPerSec;
	unsigned FMatchCount;
	System::Classes::TStringList* FMatchPatSL;
	char *FMatchPatStr;
	TStPatRecord *FMatchPatPtr;
	unsigned FMaxLineLength;
	TStNodeHeap* FNodes;
	TStOnRegExProgEvent FOnProgress;
	TStOnMatchEvent FOnMatch;
	System::Classes::TFileStream* FOutFileStream;
	Ststrms::TStAnsiTextStream* FOutTextStream;
	char *FOutLineBuf;
	int FOutFixedLineLength;
	char FOutLineTermChar;
	Stbase::TStLineTerminator FOutLineTerminator;
	System::UnicodeString FOutputFile;
	TStOutputOptions FOutputOptions;
	unsigned FReplaceCount;
	System::Classes::TStringList* FReplacePatSL;
	char *FReplacePatStr;
	TStPatRecord *FReplacePatPtr;
	System::Classes::TStringList* FSelAvoidPatSL;
	char *FSelAvoidPatStr;
	TStPatRecord *FSelAvoidPatPtr;
	unsigned FSelectCount;
	TStStreamRegEx* FStream;
	void __fastcall SetMatchPatSL(System::Classes::TStringList* Value);
	void __fastcall SetOptions(TStOutputOptions Value);
	void __fastcall SetReplacePatSL(System::Classes::TStringList* Value);
	void __fastcall SetSelAvoidPatSL(System::Classes::TStringList* Value);
	void __fastcall SetStreamProperties(void);
	
public:
	__fastcall virtual TStRegEx(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStRegEx(void);
	bool __fastcall CheckString(const System::AnsiString S, TMatchPosition &REPosition);
	bool __fastcall FileMasksToRegEx(const System::AnsiString Masks);
	bool __fastcall Execute(void);
	bool __fastcall ReplaceString(System::AnsiString &S, TMatchPosition &REPosition);
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
	__property char InLineTermChar = {read=FInLineTermChar, write=FInLineTermChar, default=10};
	__property Stbase::TStLineTerminator InLineTerminator = {read=FInLineTerminator, write=FInLineTerminator, default=3};
	__property System::UnicodeString InputFile = {read=FInputFile, write=FInputFile};
	__property bool LineNumbers = {read=FLineNumbers, write=FLineNumbers, default=0};
	__property System::Classes::TStringList* MatchPattern = {read=FMatchPatSL, write=SetMatchPatSL};
	__property TStOnMatchEvent OnMatch = {read=FOnMatch, write=FOnMatch};
	__property TStOnRegExProgEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property int OutFixedLineLength = {read=FOutFixedLineLength, write=FOutFixedLineLength, default=80};
	__property char OutLineTermChar = {read=FOutLineTermChar, write=FOutLineTermChar, default=10};
	__property Stbase::TStLineTerminator OutLineTerminator = {read=FOutLineTerminator, write=FOutLineTerminator, default=3};
	__property System::UnicodeString OutputFile = {read=FOutputFile, write=FOutputFile};
	__property TStOutputOptions OutputOptions = {read=FOutputOptions, write=SetOptions, nodefault};
	__property System::Classes::TStringList* ReplacePattern = {read=FReplacePatSL, write=SetReplacePatSL};
	__property System::Classes::TStringList* SelAvoidPattern = {read=FSelAvoidPatSL, write=SetSelAvoidPatSL};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::SmallString<31> StWordDelimString;
extern DELPHI_PACKAGE System::SmallString<19> StHexDigitString;
}	/* namespace Stregex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STREGEX)
using namespace Stregex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StregexHPP
