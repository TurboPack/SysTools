// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StMerge.pas' rev: 31.00 (Windows)

#ifndef StmergeHPP
#define StmergeHPP

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

namespace Stmerge
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStTextMerge;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TStGotMergeTagEvent)(System::TObject* Sender, System::AnsiString Tag, System::AnsiString &Value, bool &Discard);

typedef void __fastcall (*TStMergeProgressEvent)(System::TObject* Sender, int Index, bool &Abort);

class PASCALIMPLEMENTATION TStTextMerge : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::AnsiString FBadTag;
	System::Classes::TStrings* FDefaultTags;
	char FEscapeChar;
	System::Classes::TStrings* FMergedText;
	System::Classes::TStrings* FMergeTags;
	System::AnsiString FTagEnd;
	System::AnsiString FTagStart;
	System::Classes::TStrings* FTemplate;
	System::Classes::TNotifyEvent FOnMergeStart;
	System::Classes::TNotifyEvent FOnMergeDone;
	TStMergeProgressEvent FOnLineStart;
	TStMergeProgressEvent FOnLineDone;
	TStGotMergeTagEvent FOnGotMergeTag;
	TStGotMergeTagEvent FOnGotUnknownTag;
	
protected:
	void __fastcall DoGotUnknownTag(System::AnsiString Tag, System::AnsiString &Value, bool &Discard);
	void __fastcall DoGotMergeTag(System::AnsiString Tag, System::AnsiString &Value, bool &Discard);
	void __fastcall SetEscapeChar(const char Value);
	void __fastcall SetTagEnd(const System::AnsiString Value);
	void __fastcall SetTagStart(const System::AnsiString Value);
	
public:
	__fastcall TStTextMerge(void);
	__fastcall virtual ~TStTextMerge(void);
	void __fastcall Merge(void);
	void __fastcall LoadTemplateFromFile(const System::Sysutils::TFileName AFile);
	void __fastcall LoadTemplateFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveTemplateToFile(const System::Sysutils::TFileName AFile);
	void __fastcall SaveTemplateToStream(System::Classes::TStream* AStream);
	void __fastcall SaveMergeToFile(const System::Sysutils::TFileName AFile);
	void __fastcall SaveMergeToStream(System::Classes::TStream* AStream);
	__property System::AnsiString BadTag = {read=FBadTag, write=FBadTag};
	__property System::Classes::TStrings* DefaultTags = {read=FDefaultTags};
	__property char EscapeChar = {read=FEscapeChar, write=SetEscapeChar, nodefault};
	__property System::Classes::TStrings* MergedText = {read=FMergedText};
	__property System::Classes::TStrings* MergeTags = {read=FMergeTags};
	__property System::AnsiString TagEnd = {read=FTagEnd, write=SetTagEnd};
	__property System::AnsiString TagStart = {read=FTagStart, write=SetTagStart};
	__property System::Classes::TStrings* Template = {read=FTemplate};
	__property TStGotMergeTagEvent OnGotMergeTag = {read=FOnGotMergeTag, write=FOnGotMergeTag};
	__property TStGotMergeTagEvent OnGotUnknownTag = {read=FOnGotUnknownTag, write=FOnGotUnknownTag};
	__property TStMergeProgressEvent OnLineDone = {read=FOnLineDone, write=FOnLineDone};
	__property TStMergeProgressEvent OnLineStart = {read=FOnLineStart, write=FOnLineStart};
	__property System::Classes::TNotifyEvent OnMergeDone = {read=FOnMergeDone, write=FOnMergeDone};
	__property System::Classes::TNotifyEvent OnMergeStart = {read=FOnMergeStart, write=FOnMergeStart};
};


//-- var, const, procedure ---------------------------------------------------
static const System::WideChar StDefaultTagStart = (System::WideChar)(0x3c);
static const System::WideChar StDefaultTagEnd = (System::WideChar)(0x3e);
static const System::WideChar StDefaultEscapeChar = (System::WideChar)(0x5c);
}	/* namespace Stmerge */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STMERGE)
using namespace Stmerge;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StmergeHPP
