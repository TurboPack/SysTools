// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StIniStm.pas' rev: 29.00 (Windows)

#ifndef StinistmHPP
#define StinistmHPP

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
#include <StStrms.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stinistm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStIniStream;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStIniStream : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Ststrms::TStAnsiTextStream* FAnsiStream;
	System::Classes::TStringList* FSections;
	void __fastcall GetSecStrings(System::Classes::TStrings* Strs);
	
protected:
	void __fastcall GotoSection(const System::UnicodeString Section);
	void __fastcall UpdateSections(void);
	void __fastcall WriteSectionName(const System::UnicodeString Section);
	void __fastcall WriteValue(const System::UnicodeString Key, const System::UnicodeString Value);
	
public:
	__fastcall TStIniStream(System::Classes::TStream* aStream);
	__fastcall virtual ~TStIniStream(void);
	bool __fastcall SectionExists(const System::UnicodeString Section);
	System::UnicodeString __fastcall ReadString(const System::UnicodeString Section, const System::UnicodeString Ident, const System::UnicodeString Default);
	void __fastcall WriteString(const System::UnicodeString Section, const System::UnicodeString Ident, const System::UnicodeString Value);
	void __fastcall WriteSection(const System::UnicodeString Section, System::Classes::TStrings* Strings);
	void __fastcall ReadSection(const System::UnicodeString Section, System::Classes::TStrings* Strings);
	void __fastcall ReadSections(System::Classes::TStrings* Strings);
	void __fastcall ReadSectionValues(const System::UnicodeString Section, System::Classes::TStrings* Strings);
	void __fastcall EraseSection(const System::UnicodeString Section);
	void __fastcall DeleteKey(const System::UnicodeString Section, const System::UnicodeString Ident);
	bool __fastcall ValueExists(const System::UnicodeString Section, const System::UnicodeString Ident);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall SplitNameValue(const System::UnicodeString Line, System::UnicodeString &Name, System::UnicodeString &Value);
}	/* namespace Stinistm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STINISTM)
using namespace Stinistm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StinistmHPP
