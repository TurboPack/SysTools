// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StStrW.pas' rev: 28.00 (Windows)

#ifndef StstrwHPP
#define StstrwHPP

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

namespace Ststrw
{
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD WStrRec
{
public:
	int Length;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
static const int StrOffset = int(0x4);
extern DELPHI_PACKAGE System::WideString __fastcall HexBW(System::Byte B);
extern DELPHI_PACKAGE System::WideString __fastcall HexWW(System::Word W);
extern DELPHI_PACKAGE System::WideString __fastcall HexLW(int L);
extern DELPHI_PACKAGE System::WideString __fastcall HexPtrW(void * P);
extern DELPHI_PACKAGE System::WideString __fastcall BinaryBW(System::Byte B);
extern DELPHI_PACKAGE System::WideString __fastcall BinaryWW(System::Word W);
extern DELPHI_PACKAGE System::WideString __fastcall BinaryLW(int L);
extern DELPHI_PACKAGE System::WideString __fastcall OctalBW(System::Byte B);
extern DELPHI_PACKAGE System::WideString __fastcall OctalWW(System::Word W);
extern DELPHI_PACKAGE System::WideString __fastcall OctalLW(int L);
extern DELPHI_PACKAGE bool __fastcall Str2Int16W(const System::WideString S, short &I);
extern DELPHI_PACKAGE bool __fastcall Str2WordW(const System::WideString S, System::Word &I);
extern DELPHI_PACKAGE bool __fastcall Str2LongW(const System::WideString S, int &I);
extern DELPHI_PACKAGE bool __fastcall Str2RealW(const System::WideString S, double &R);
extern DELPHI_PACKAGE bool __fastcall Str2ExtW(const System::WideString S, System::Extended &R);
extern DELPHI_PACKAGE System::WideString __fastcall Long2StrW(int L);
extern DELPHI_PACKAGE System::WideString __fastcall Real2StrW(double R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE System::WideString __fastcall Ext2StrW(System::Extended R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE System::WideString __fastcall ValPrepW(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall CharStrW(System::WideChar C, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall PadChW(const System::WideString S, System::WideChar C, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall PadW(const System::WideString S, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall LeftPadChW(const System::WideString S, System::WideChar C, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall LeftPadW(const System::WideString S, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall TrimLeadW(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall TrimTrailW(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall TrimW(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall TrimSpacesW(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall CenterChW(const System::WideString S, System::WideChar C, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall CenterW(const System::WideString S, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall EntabW(const System::WideString S, System::Byte TabSize);
extern DELPHI_PACKAGE System::WideString __fastcall DetabW(const System::WideString S, System::Byte TabSize);
extern DELPHI_PACKAGE System::WideString __fastcall ScrambleW(const System::WideString S, const System::WideString Key);
extern DELPHI_PACKAGE System::WideString __fastcall SubstituteW(const System::WideString S, const System::WideString FromStr, const System::WideString ToStr);
extern DELPHI_PACKAGE System::WideString __fastcall FilterW(const System::WideString S, const System::WideString Filters);
extern DELPHI_PACKAGE bool __fastcall CharExistsW(const System::WideString S, System::WideChar C);
extern DELPHI_PACKAGE unsigned __fastcall CharCountW(const System::WideString S, System::WideChar C);
extern DELPHI_PACKAGE unsigned __fastcall WordCountW(const System::WideString S, const System::WideString WordDelims);
extern DELPHI_PACKAGE bool __fastcall WordPositionW(unsigned N, const System::WideString S, const System::WideString WordDelims, unsigned &Pos);
extern DELPHI_PACKAGE System::WideString __fastcall ExtractWordW(unsigned N, const System::WideString S, const System::WideString WordDelims);
extern DELPHI_PACKAGE unsigned __fastcall AsciiCountW(const System::WideString S, const System::WideString WordDelims, System::WideChar Quote);
extern DELPHI_PACKAGE bool __fastcall AsciiPositionW(unsigned N, const System::WideString S, const System::WideString WordDelims, System::WideChar Quote, unsigned &Pos);
extern DELPHI_PACKAGE System::WideString __fastcall ExtractAsciiW(unsigned N, const System::WideString S, const System::WideString WordDelims, System::WideChar Quote);
extern DELPHI_PACKAGE void __fastcall WordWrapW(const System::WideString InSt, System::WideString &OutSt, System::WideString &Overlap, unsigned Margin, bool PadToMargin);
extern DELPHI_PACKAGE int __fastcall CompStringW(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE int __fastcall CompUCStringW(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE System::WideString __fastcall DefaultExtensionW(const System::WideString Name, const System::WideString Ext);
extern DELPHI_PACKAGE System::WideString __fastcall ForceExtensionW(const System::WideString Name, const System::WideString Ext);
extern DELPHI_PACKAGE System::WideString __fastcall JustFilenameW(const System::WideString PathName);
extern DELPHI_PACKAGE System::WideString __fastcall JustNameW(const System::WideString PathName);
extern DELPHI_PACKAGE System::WideString __fastcall JustExtensionW(const System::WideString Name);
extern DELPHI_PACKAGE System::WideString __fastcall JustPathnameW(const System::WideString PathName);
extern DELPHI_PACKAGE System::WideString __fastcall AddBackSlashW(const System::WideString DirName);
extern DELPHI_PACKAGE System::WideString __fastcall CleanPathNameW(const System::WideString PathName);
extern DELPHI_PACKAGE bool __fastcall HasExtensionW(const System::WideString Name, unsigned &DotPos);
extern DELPHI_PACKAGE System::WideString __fastcall CommaizeChW(int L, System::WideChar Ch);
extern DELPHI_PACKAGE System::WideString __fastcall CommaizeW(int L);
extern DELPHI_PACKAGE System::WideString __fastcall FloatFormW(const System::WideString Mask, System::Extended R, const System::WideString LtCurr, const System::WideString RtCurr, System::WideChar Sep, System::WideChar DecPt);
extern DELPHI_PACKAGE System::WideString __fastcall LongIntFormW(const System::WideString Mask, int L, const System::WideString LtCurr, const System::WideString RtCurr, System::WideChar Sep);
extern DELPHI_PACKAGE bool __fastcall StrChPosW(const System::WideString P, System::WideChar C, unsigned &Pos);
extern DELPHI_PACKAGE bool __fastcall StrStPosW(const System::WideString P, const System::WideString S, unsigned &Pos);
extern DELPHI_PACKAGE System::WideString __fastcall StrStCopyW(const System::WideString S, unsigned Pos, unsigned Count);
extern DELPHI_PACKAGE System::WideString __fastcall StrChInsertW(const System::WideString S, System::WideChar C, unsigned Pos);
extern DELPHI_PACKAGE System::WideString __fastcall StrStInsertW(const System::WideString S1, const System::WideString S2, unsigned Pos);
extern DELPHI_PACKAGE System::WideString __fastcall StrChDeleteW(const System::WideString S, unsigned Pos);
extern DELPHI_PACKAGE System::WideString __fastcall StrStDeleteW(const System::WideString S, unsigned Pos, unsigned Count);
extern DELPHI_PACKAGE System::WideString __fastcall CopyLeftW(const System::WideString S, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall CopyMidW(const System::WideString S, unsigned First, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall CopyRightW(const System::WideString S, unsigned First);
extern DELPHI_PACKAGE System::WideString __fastcall CopyRightAbsW(const System::WideString S, unsigned NumChars);
extern DELPHI_PACKAGE bool __fastcall WordPosW(const System::WideString S, const System::WideString WordDelims, const System::WideString AWord, unsigned N, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall CopyFromNthWordW(const System::WideString S, const System::WideString WordDelims, System::WideString AWord, unsigned N, System::WideString &SubString);
extern DELPHI_PACKAGE bool __fastcall DeleteFromNthWordW(const System::WideString S, const System::WideString WordDelims, System::WideString AWord, unsigned N, System::WideString &SubString);
extern DELPHI_PACKAGE bool __fastcall CopyFromToWordW(const System::WideString S, const System::WideString WordDelims, const System::WideString Word1, const System::WideString Word2, unsigned N1, unsigned N2, System::WideString &SubString);
extern DELPHI_PACKAGE bool __fastcall DeleteFromToWordW(const System::WideString S, const System::WideString WordDelims, const System::WideString Word1, const System::WideString Word2, unsigned N1, unsigned N2, System::WideString &SubString);
extern DELPHI_PACKAGE System::WideString __fastcall CopyWithinW(const System::WideString S, const System::WideString Delimiter, bool Strip);
extern DELPHI_PACKAGE System::WideString __fastcall DeleteWithinW(const System::WideString S, const System::WideString Delimiter);
extern DELPHI_PACKAGE System::WideString __fastcall ReplaceWordW(const System::WideString S, const System::WideString WordDelims, const System::WideString OldWord, const System::WideString NewWord, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE System::WideString __fastcall ReplaceWordAllW(const System::WideString S, const System::WideString WordDelims, const System::WideString OldWord, const System::WideString NewWord, unsigned &Replacements);
extern DELPHI_PACKAGE System::WideString __fastcall ReplaceStringW(const System::WideString S, const System::WideString OldString, const System::WideString NewString, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE System::WideString __fastcall ReplaceStringAllW(const System::WideString S, const System::WideString OldString, const System::WideString NewString, unsigned &Replacements);
extern DELPHI_PACKAGE bool __fastcall LastWordW(const System::WideString S, const System::WideString WordDelims, const System::WideString AWord, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastWordAbsW(const System::WideString S, const System::WideString WordDelims, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastStringW(const System::WideString S, const System::WideString AString, unsigned &Position);
extern DELPHI_PACKAGE System::WideString __fastcall KeepCharsW(const System::WideString S, const System::WideString Chars);
extern DELPHI_PACKAGE System::WideString __fastcall RepeatStringW(const System::WideString RepeatString, unsigned &Repetitions, unsigned MaxLen);
extern DELPHI_PACKAGE System::WideString __fastcall TrimCharsW(const System::WideString S, const System::WideString Chars);
extern DELPHI_PACKAGE System::WideString __fastcall RightTrimCharsW(const System::WideString S, const System::WideString Chars);
extern DELPHI_PACKAGE System::WideString __fastcall LeftTrimCharsW(const System::WideString S, const System::WideString Chars);
extern DELPHI_PACKAGE unsigned __fastcall ExtractTokensW(const System::WideString S, const System::WideString Delims, System::WideChar QuoteChar, bool AllowNulls, System::Classes::TStrings* Tokens);
extern DELPHI_PACKAGE bool __fastcall ContainsOnlyW(const System::WideString S, const System::WideString Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall ContainsOtherThanW(const System::WideString S, const System::WideString Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaW(System::WideChar C);
extern DELPHI_PACKAGE bool __fastcall IsChNumericW(System::WideChar C, System::WideString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaNumericW(System::WideChar C, System::WideString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaW(const System::WideString S);
extern DELPHI_PACKAGE bool __fastcall IsStrNumericW(const System::WideString S, const System::WideString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaNumericW(const System::WideString S, const System::WideString Numbers);
extern DELPHI_PACKAGE bool __fastcall StrWithinW(const System::WideString S, const System::WideString SearchStr, unsigned Start, unsigned &Position);
}	/* namespace Ststrw */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSTRW)
using namespace Ststrw;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StstrwHPP
