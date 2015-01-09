// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StStrL.pas' rev: 28.00 (Windows)

#ifndef StstrlHPP
#define StstrlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Ststrl
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD LStrRec
{
public:
	int AllocSize;
	int RefCount;
	int Length;
};


//-- var, const, procedure ---------------------------------------------------
static const int StrOffset = int(0xc);
extern DELPHI_PACKAGE System::AnsiString __fastcall HexBL(System::Byte B);
extern DELPHI_PACKAGE System::AnsiString __fastcall HexWL(System::Word W);
extern DELPHI_PACKAGE System::AnsiString __fastcall HexLL(int L);
extern DELPHI_PACKAGE System::AnsiString __fastcall HexPtrL(void * P);
extern DELPHI_PACKAGE System::AnsiString __fastcall BinaryBL(System::Byte B);
extern DELPHI_PACKAGE System::AnsiString __fastcall BinaryWL(System::Word W);
extern DELPHI_PACKAGE System::AnsiString __fastcall BinaryLL(int L);
extern DELPHI_PACKAGE System::AnsiString __fastcall OctalBL(System::Byte B);
extern DELPHI_PACKAGE System::AnsiString __fastcall OctalWL(System::Word W);
extern DELPHI_PACKAGE System::AnsiString __fastcall OctalLL(int L);
extern DELPHI_PACKAGE bool __fastcall Str2Int16L(const System::UnicodeString S, short &I);
extern DELPHI_PACKAGE bool __fastcall Str2WordL(const System::UnicodeString S, System::Word &I);
extern DELPHI_PACKAGE bool __fastcall Str2LongL(const System::UnicodeString S, int &I);
extern DELPHI_PACKAGE bool __fastcall Str2RealL(const System::UnicodeString S, double &R);
extern DELPHI_PACKAGE bool __fastcall Str2ExtL(const System::UnicodeString S, System::Extended &R);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Long2StrL(int L);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Real2StrL(double R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Ext2StrL(System::Extended R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ValPrepL(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CharStrL(System::WideChar C, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall PadChL(const System::UnicodeString S, System::WideChar C, int Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall PadL(const System::UnicodeString S, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LeftPadChL(const System::UnicodeString S, System::WideChar C, int Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LeftPadL(const System::UnicodeString S, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TrimLeadL(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TrimTrailL(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TrimL(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TrimSpacesL(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CenterChL(const System::UnicodeString S, System::WideChar C, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CenterL(const System::UnicodeString S, unsigned Len);
extern DELPHI_PACKAGE System::AnsiString __fastcall EntabL(const System::AnsiString S, System::Byte TabSize);
extern DELPHI_PACKAGE System::AnsiString __fastcall DetabL(const System::AnsiString S, System::Byte TabSize);
extern DELPHI_PACKAGE System::AnsiString __fastcall ScrambleL(const System::AnsiString S, const System::AnsiString Key);
extern DELPHI_PACKAGE System::UnicodeString __fastcall SubstituteL(const System::UnicodeString S, const System::UnicodeString FromStr, const System::UnicodeString ToStr);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FilterL(const System::UnicodeString S, const System::UnicodeString Filters);
extern DELPHI_PACKAGE bool __fastcall CharExistsL(const System::UnicodeString S, System::WideChar C);
extern DELPHI_PACKAGE unsigned __fastcall CharCountL(const System::UnicodeString S, System::WideChar C);
extern DELPHI_PACKAGE unsigned __fastcall WordCountL(const System::UnicodeString S, const System::UnicodeString WordDelims);
extern DELPHI_PACKAGE bool __fastcall WordPositionL(unsigned N, const System::UnicodeString S, const System::UnicodeString WordDelims, unsigned &Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractWordL(unsigned N, const System::UnicodeString S, const System::UnicodeString WordDelims);
extern DELPHI_PACKAGE unsigned __fastcall AsciiCountL(const System::UnicodeString S, const System::UnicodeString WordDelims, System::WideChar Quote);
extern DELPHI_PACKAGE bool __fastcall AsciiPositionL(unsigned N, const System::UnicodeString S, const System::UnicodeString WordDelims, System::WideChar Quote, unsigned &Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractAsciiL(unsigned N, const System::UnicodeString S, const System::UnicodeString WordDelims, System::WideChar Quote);
extern DELPHI_PACKAGE void __fastcall WordWrapL(const System::UnicodeString InSt, System::UnicodeString &OutSt, System::UnicodeString &Overlap, int Margin, bool PadToMargin);
extern DELPHI_PACKAGE int __fastcall CompStringL(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE int __fastcall CompUCStringL(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE System::AnsiString __fastcall SoundexL(const System::AnsiString S);
extern DELPHI_PACKAGE int __fastcall MakeLetterSetL(const System::AnsiString S);
extern DELPHI_PACKAGE void __fastcall BMMakeTableL(const System::UnicodeString MatchString, Stbase::BTable &BT)/* overload */;
extern DELPHI_PACKAGE bool __fastcall BMSearchL(void *Buffer, unsigned BufLength, Stbase::BTable &BT, const System::UnicodeString MatchString, /* out */ unsigned &Pos)/* overload */;
extern DELPHI_PACKAGE bool __fastcall BMSearchUCL(void *Buffer, unsigned BufLength, Stbase::BTable &BT, const System::UnicodeString MatchString, unsigned &Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DefaultExtensionL(const System::UnicodeString Name, const System::UnicodeString Ext);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ForceExtensionL(const System::UnicodeString Name, const System::UnicodeString Ext);
extern DELPHI_PACKAGE System::UnicodeString __fastcall JustFilenameL(const System::UnicodeString PathName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall JustNameL(const System::UnicodeString PathName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall JustExtensionL(const System::UnicodeString Name);
extern DELPHI_PACKAGE System::UnicodeString __fastcall JustPathnameL(const System::UnicodeString PathName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall AddBackSlashL(const System::UnicodeString DirName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CleanPathNameL(const System::UnicodeString PathName);
extern DELPHI_PACKAGE bool __fastcall HasExtensionL(const System::UnicodeString Name, unsigned &DotPos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CommaizeChL(int L, System::WideChar Ch);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CommaizeL(int L);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FloatFormL(const System::UnicodeString Mask, System::Extended R, const System::UnicodeString LtCurr, const System::UnicodeString RtCurr, System::WideChar Sep, System::WideChar DecPt);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LongIntFormL(const System::UnicodeString Mask, int L, const System::UnicodeString LtCurr, const System::UnicodeString RtCurr, System::WideChar Sep);
extern DELPHI_PACKAGE bool __fastcall StrChPosL(const System::UnicodeString P, System::WideChar C, int &Pos);
extern DELPHI_PACKAGE bool __fastcall StrStPosL(const System::UnicodeString P, const System::UnicodeString S, unsigned &Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrStCopyL(const System::UnicodeString S, unsigned Pos, unsigned Count);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrChInsertL(const System::UnicodeString S, System::WideChar C, unsigned Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrStInsertL(const System::UnicodeString S1, const System::UnicodeString S2, unsigned Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrChDeleteL(const System::UnicodeString S, unsigned Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrStDeleteL(const System::UnicodeString S, unsigned Pos, unsigned Count);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CopyLeftL(const System::UnicodeString S, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CopyMidL(const System::UnicodeString S, unsigned First, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CopyRightL(const System::UnicodeString S, unsigned First);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CopyRightAbsL(const System::UnicodeString S, unsigned NumChars);
extern DELPHI_PACKAGE bool __fastcall WordPosL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString AWord, unsigned N, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall CopyFromNthWordL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString AWord, unsigned N, System::UnicodeString &SubString);
extern DELPHI_PACKAGE bool __fastcall DeleteFromNthWordL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString AWord, unsigned N, System::UnicodeString &SubString);
extern DELPHI_PACKAGE bool __fastcall CopyFromToWordL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString Word1, const System::UnicodeString Word2, unsigned N1, unsigned N2, System::UnicodeString &SubString);
extern DELPHI_PACKAGE bool __fastcall DeleteFromToWordL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString Word1, const System::UnicodeString Word2, unsigned N1, unsigned N2, System::UnicodeString &SubString);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CopyWithinL(const System::UnicodeString S, const System::UnicodeString Delimiter, bool Strip);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DeleteWithinL(const System::UnicodeString S, const System::UnicodeString Delimiter);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceWordL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString OldWord, const System::UnicodeString NewWord, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceWordAllL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString OldWord, const System::UnicodeString NewWord, unsigned &Replacements);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceStringL(const System::UnicodeString S, const System::UnicodeString OldString, const System::UnicodeString NewString, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceStringAllL(const System::UnicodeString S, const System::UnicodeString OldString, const System::UnicodeString NewString, unsigned &Replacements);
extern DELPHI_PACKAGE bool __fastcall LastWordL(const System::UnicodeString S, const System::UnicodeString WordDelims, const System::UnicodeString AWord, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastWordAbsL(const System::UnicodeString S, const System::UnicodeString WordDelims, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastStringL(const System::UnicodeString S, const System::UnicodeString AString, unsigned &Position);
extern DELPHI_PACKAGE System::UnicodeString __fastcall KeepCharsL(const System::UnicodeString S, const System::UnicodeString Chars);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RepeatStringL(const System::UnicodeString RepeatString, unsigned &Repetitions, unsigned MaxLen);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TrimCharsL(const System::UnicodeString S, const System::UnicodeString Chars);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RightTrimCharsL(const System::UnicodeString S, const System::UnicodeString Chars);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LeftTrimCharsL(const System::UnicodeString S, const System::UnicodeString Chars);
extern DELPHI_PACKAGE unsigned __fastcall ExtractTokensL(const System::UnicodeString S, const System::UnicodeString Delims, System::WideChar QuoteChar, bool AllowNulls, System::Classes::TStrings* Tokens)/* overload */;
extern DELPHI_PACKAGE bool __fastcall ContainsOnlyL(const System::UnicodeString S, const System::UnicodeString Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall ContainsOtherThanL(const System::UnicodeString S, const System::UnicodeString Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaL(System::WideChar C);
extern DELPHI_PACKAGE bool __fastcall IsChNumericL(System::WideChar C, const System::UnicodeString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaNumericL(System::WideChar C, const System::UnicodeString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaL(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall IsStrNumericL(const System::UnicodeString S, const System::UnicodeString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaNumericL(const System::UnicodeString S, const System::UnicodeString Numbers);
extern DELPHI_PACKAGE bool __fastcall StrWithinL(const System::UnicodeString S, const System::UnicodeString SearchStr, unsigned Start, unsigned &Position);
}	/* namespace Ststrl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSTRL)
using namespace Ststrl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StstrlHPP
