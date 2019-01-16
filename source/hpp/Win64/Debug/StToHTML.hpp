// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StToHTML.pas' rev: 32.00 (Windows)

#ifndef SttohtmlHPP
#define SttohtmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <StStrms.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sttohtml
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStStreamToHTML;
class DELPHICLASS TStFileToHTML;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TStOnProgressEvent)(System::TObject* Sender, System::Word Percent);

class PASCALIMPLEMENTATION TStStreamToHTML : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	bool FCaseSensitive;
	System::Classes::TStringList* FCommentMarkers;
	System::Classes::TStringList* FEmbeddedHTML;
	unsigned FInFileSize;
	int FInFixedLineLen;
	System::WideChar FInLineTermChar;
	Stbase::TStLineTerminator FInLineTerminator;
	System::Classes::TStream* FInputStream;
	unsigned FInSize;
	Ststrms::TStAnsiTextStream* FInTextStream;
	bool FIsCaseSensitive;
	System::Classes::TStringList* FKeywords;
	TStOnProgressEvent FOnProgress;
	System::Classes::TStream* FOutputStream;
	Ststrms::TStAnsiTextStream* FOutTextStream;
	System::Classes::TStringList* FPageFooter;
	System::Classes::TStringList* FPageHeader;
	System::Classes::TStringList* FStringMarkers;
	System::UnicodeString FWordDelims;
	bool __fastcall ParseBuffer(void);
	void __fastcall SetCommentMarkers(System::Classes::TStringList* Value);
	void __fastcall SetEmbeddedHTML(System::Classes::TStringList* Value);
	void __fastcall SetKeywords(System::Classes::TStringList* Value);
	void __fastcall SetPageFooter(System::Classes::TStringList* Value);
	void __fastcall SetPageHeader(System::Classes::TStringList* Value);
	void __fastcall SetStringMarkers(System::Classes::TStringList* Value);
	
public:
	__property bool CaseSensitive = {read=FCaseSensitive, write=FCaseSensitive, nodefault};
	__property System::Classes::TStringList* CommentMarkers = {read=FCommentMarkers, write=SetCommentMarkers};
	__property System::Classes::TStringList* EmbeddedHTML = {read=FEmbeddedHTML, write=SetEmbeddedHTML};
	__property int InFixedLineLength = {read=FInFixedLineLen, write=FInFixedLineLen, nodefault};
	__property System::WideChar InLineTermChar = {read=FInLineTermChar, write=FInLineTermChar, nodefault};
	__property Stbase::TStLineTerminator InLineTerminator = {read=FInLineTerminator, write=FInLineTerminator, nodefault};
	__property System::Classes::TStream* InputStream = {read=FInputStream, write=FInputStream};
	__property System::Classes::TStringList* Keywords = {read=FKeywords, write=SetKeywords};
	__property TStOnProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property System::Classes::TStream* OutputStream = {read=FOutputStream, write=FOutputStream};
	__property System::Classes::TStringList* PageFooter = {read=FPageFooter, write=SetPageFooter};
	__property System::Classes::TStringList* PageHeader = {read=FPageHeader, write=SetPageHeader};
	__property System::Classes::TStringList* StringMarkers = {read=FStringMarkers, write=SetStringMarkers};
	__property System::UnicodeString WordDelimiters = {read=FWordDelims, write=FWordDelims};
	__fastcall TStStreamToHTML(void);
	__fastcall virtual ~TStStreamToHTML(void);
	void __fastcall GenerateHTML(void);
};


class PASCALIMPLEMENTATION TStFileToHTML : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	bool FCaseSensitive;
	System::Classes::TStringList* FCommentMarkers;
	System::Classes::TStringList* FEmbeddedHTML;
	System::Classes::TFileStream* FInFile;
	System::UnicodeString FInFileName;
	int FInLineLength;
	System::WideChar FInLineTermChar;
	Stbase::TStLineTerminator FInLineTerminator;
	System::Classes::TStringList* FKeywords;
	TStOnProgressEvent FOnProgress;
	System::Classes::TFileStream* FOutFile;
	System::UnicodeString FOutFileName;
	System::Classes::TStringList* FPageFooter;
	System::Classes::TStringList* FPageHeader;
	TStStreamToHTML* FStream;
	System::Classes::TStringList* FStringMarkers;
	System::UnicodeString FWordDelims;
	void __fastcall SetCommentMarkers(System::Classes::TStringList* Value);
	void __fastcall SetEmbeddedHTML(System::Classes::TStringList* Value);
	void __fastcall SetKeywords(System::Classes::TStringList* Value);
	void __fastcall SetPageFooter(System::Classes::TStringList* Value);
	void __fastcall SetPageHeader(System::Classes::TStringList* Value);
	void __fastcall SetStringMarkers(System::Classes::TStringList* Value);
	
public:
	__fastcall virtual TStFileToHTML(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStFileToHTML(void);
	void __fastcall Execute(void);
	
__published:
	__property bool CaseSensitive = {read=FCaseSensitive, write=FCaseSensitive, default=0};
	__property System::Classes::TStringList* CommentMarkers = {read=FCommentMarkers, write=SetCommentMarkers};
	__property System::Classes::TStringList* EmbeddedHTML = {read=FEmbeddedHTML, write=SetEmbeddedHTML};
	__property System::UnicodeString InFileName = {read=FInFileName, write=FInFileName};
	__property int InFixedLineLength = {read=FInLineLength, write=FInLineLength, default=80};
	__property System::WideChar InLineTermChar = {read=FInLineTermChar, write=FInLineTermChar, default=10};
	__property Stbase::TStLineTerminator InLineTerminator = {read=FInLineTerminator, write=FInLineTerminator, default=3};
	__property System::Classes::TStringList* Keywords = {read=FKeywords, write=SetKeywords};
	__property TStOnProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property System::UnicodeString OutFileName = {read=FOutFileName, write=FOutFileName};
	__property System::Classes::TStringList* PageFooter = {read=FPageFooter, write=SetPageFooter};
	__property System::Classes::TStringList* PageHeader = {read=FPageHeader, write=SetPageHeader};
	__property System::Classes::TStringList* StringMarkers = {read=FStringMarkers, write=SetStringMarkers};
	__property System::UnicodeString WordDelimiters = {read=FWordDelims, write=FWordDelims};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sttohtml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STTOHTML)
using namespace Sttohtml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SttohtmlHPP
