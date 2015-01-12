// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StStrS.pas' rev: 28.00 (Windows)

#ifndef StstrsHPP
#define StstrsHPP

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

namespace Ststrs
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ShortString __fastcall HexBS(System::Byte B);
extern DELPHI_PACKAGE System::ShortString __fastcall HexWS(System::Word W);
extern DELPHI_PACKAGE System::ShortString __fastcall HexLS(int L);
extern DELPHI_PACKAGE System::ShortString __fastcall HexPtrS(void * P);
extern DELPHI_PACKAGE System::ShortString __fastcall BinaryBS(System::Byte B);
extern DELPHI_PACKAGE System::ShortString __fastcall BinaryWS(System::Word W);
extern DELPHI_PACKAGE System::ShortString __fastcall BinaryLS(int L);
extern DELPHI_PACKAGE System::ShortString __fastcall OctalBS(System::Byte B);
extern DELPHI_PACKAGE System::ShortString __fastcall OctalWS(System::Word W);
extern DELPHI_PACKAGE System::ShortString __fastcall OctalLS(int L);
extern DELPHI_PACKAGE bool __fastcall Str2Int16S(const System::UnicodeString S, short &I);
extern DELPHI_PACKAGE bool __fastcall Str2WordS(const System::UnicodeString S, System::Word &I);
extern DELPHI_PACKAGE bool __fastcall Str2LongS(const System::UnicodeString S, int &I);
extern DELPHI_PACKAGE bool __fastcall Str2RealS(const System::UnicodeString S, double &R);
extern DELPHI_PACKAGE bool __fastcall Str2ExtS(const System::UnicodeString S, System::Extended &R);
extern DELPHI_PACKAGE System::ShortString __fastcall Long2StrS(int L);
extern DELPHI_PACKAGE System::ShortString __fastcall Real2StrS(double R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE System::ShortString __fastcall Ext2StrS(System::Extended R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ValPrepS(const System::UnicodeString S);
extern DELPHI_PACKAGE System::ShortString __fastcall CharStrS(char C, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall PadChS(const System::ShortString &S, char C, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall PadS(const System::ShortString &S, unsigned Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LeftPadChS(const System::UnicodeString S, System::WideChar C, int Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LeftPadS(const System::UnicodeString S, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall TrimLeadS(const System::ShortString &S);
extern DELPHI_PACKAGE System::ShortString __fastcall TrimTrailS(const System::ShortString &S);
extern DELPHI_PACKAGE System::ShortString __fastcall TrimS(const System::ShortString &S);
extern DELPHI_PACKAGE System::ShortString __fastcall CenterChS(const System::ShortString &S, char C, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall CenterS(const System::ShortString &S, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall ScrambleS(const System::ShortString &S, const System::ShortString &Key);
extern DELPHI_PACKAGE System::ShortString __fastcall SubstituteS(const System::ShortString &S, const System::ShortString &FromStr, const System::ShortString &ToStr);
extern DELPHI_PACKAGE System::ShortString __fastcall FilterS(const System::ShortString &S, const System::ShortString &Filters);
extern DELPHI_PACKAGE bool __fastcall CharExistsS(const System::UnicodeString S, System::WideChar C)/* overload */;
extern DELPHI_PACKAGE bool __fastcall CharExistsS(const System::ShortString &S, char C)/* overload */;
extern DELPHI_PACKAGE System::Byte __fastcall CharCountS(const System::ShortString &S, char C);
extern DELPHI_PACKAGE unsigned __fastcall WordCountS(const System::ShortString &S, const System::ShortString &WordDelims);
extern DELPHI_PACKAGE bool __fastcall WordPositionS(unsigned N, const System::ShortString &S, const System::ShortString &WordDelims, unsigned &Pos);
extern DELPHI_PACKAGE System::ShortString __fastcall ExtractWordS(unsigned N, const System::ShortString &S, const System::ShortString &WordDelims);
extern DELPHI_PACKAGE unsigned __fastcall AsciiCountS(const System::ShortString &S, const System::ShortString &WordDelims, char Quote);
extern DELPHI_PACKAGE bool __fastcall AsciiPositionS(unsigned N, const System::ShortString &S, const System::ShortString &WordDelims, char Quote, unsigned &Pos);
extern DELPHI_PACKAGE System::ShortString __fastcall ExtractAsciiS(unsigned N, const System::ShortString &S, const System::ShortString &WordDelims, char Quote);
extern DELPHI_PACKAGE void __fastcall WordWrapS(const System::ShortString &InSt, System::SmallString<255> &OutSt, const int OutSt_High, System::SmallString<255> &Overlap, const int Overlap_High, unsigned Margin, bool PadToMargin);
extern DELPHI_PACKAGE System::ShortString __fastcall DefaultExtensionS(const System::ShortString &Name, const System::ShortString &Ext);
extern DELPHI_PACKAGE System::ShortString __fastcall ForceExtensionS(const System::ShortString &Name, const System::ShortString &Ext);
extern DELPHI_PACKAGE System::ShortString __fastcall JustFilenameS(const System::ShortString &PathName);
extern DELPHI_PACKAGE System::ShortString __fastcall JustNameS(const System::ShortString &PathName);
extern DELPHI_PACKAGE System::ShortString __fastcall JustExtensionS(const System::ShortString &Name);
extern DELPHI_PACKAGE System::ShortString __fastcall JustPathnameS(const System::ShortString &PathName);
extern DELPHI_PACKAGE System::ShortString __fastcall AddBackSlashS(const System::ShortString &DirName);
extern DELPHI_PACKAGE System::ShortString __fastcall CleanPathNameS(const System::ShortString &PathName);
extern DELPHI_PACKAGE bool __fastcall HasExtensionS(const System::ShortString &Name, unsigned &DotPos);
extern DELPHI_PACKAGE System::ShortString __fastcall CommaizeChS(int L, char Ch);
extern DELPHI_PACKAGE System::ShortString __fastcall CommaizeS(int L);
extern DELPHI_PACKAGE System::ShortString __fastcall FloatFormS(const System::ShortString &Mask, System::Extended R, const System::ShortString &LtCurr, const System::ShortString &RtCurr, char Sep, char DecPt);
extern DELPHI_PACKAGE System::ShortString __fastcall LongIntFormS(const System::ShortString &Mask, int L, const System::ShortString &LtCurr, const System::ShortString &RtCurr, char Sep);
extern DELPHI_PACKAGE bool __fastcall StrChPosS(const System::UnicodeString P, System::WideChar C, int &Pos)/* overload */;
extern DELPHI_PACKAGE bool __fastcall StrChPosS(const System::ShortString &P, char C, int &Pos)/* overload */;
extern DELPHI_PACKAGE bool __fastcall StrStPosS(const System::ShortString &P, const System::ShortString &S, unsigned &Pos);
extern DELPHI_PACKAGE System::ShortString __fastcall StrStCopyS(const System::ShortString &S, unsigned Pos, unsigned Count);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrChInsertS(const System::UnicodeString S, System::WideChar C, int Pos);
extern DELPHI_PACKAGE System::ShortString __fastcall StrStInsertS(const System::ShortString &S1, const System::ShortString &S2, unsigned Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrChDeleteS(const System::UnicodeString S, int Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrStDeleteS(const System::UnicodeString S, int Pos, int Count);
extern DELPHI_PACKAGE System::ShortString __fastcall CopyLeftS(const System::ShortString &S, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall CopyMidS(const System::ShortString &S, unsigned First, unsigned Len);
extern DELPHI_PACKAGE System::ShortString __fastcall CopyRightS(const System::ShortString &S, unsigned First);
extern DELPHI_PACKAGE System::ShortString __fastcall CopyRightAbsS(const System::ShortString &S, unsigned NumChars);
extern DELPHI_PACKAGE bool __fastcall CopyFromNthWordS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &AWord, unsigned N, System::SmallString<255> &SubString, const int SubString_High);
extern DELPHI_PACKAGE bool __fastcall DeleteFromNthWordS(const System::ShortString &S, const System::ShortString &WordDelims, System::ShortString &AWord, unsigned N, System::SmallString<255> &SubString, const int SubString_High);
extern DELPHI_PACKAGE bool __fastcall CopyFromToWordS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &Word1, const System::ShortString &Word2, unsigned N1, unsigned N2, System::SmallString<255> &SubString, const int SubString_High);
extern DELPHI_PACKAGE bool __fastcall DeleteFromToWordS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &Word1, const System::ShortString &Word2, unsigned N1, unsigned N2, System::SmallString<255> &SubString, const int SubString_High);
extern DELPHI_PACKAGE System::ShortString __fastcall CopyWithinS(const System::ShortString &S, const System::ShortString &Delimiter, bool Strip);
extern DELPHI_PACKAGE System::ShortString __fastcall DeleteWithinS(const System::ShortString &S, const System::ShortString &Delimiter);
extern DELPHI_PACKAGE System::ShortString __fastcall ReplaceWordS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &OldWord, const System::ShortString &NewWord, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE System::ShortString __fastcall ReplaceWordAllS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &OldWord, const System::ShortString &NewWord, unsigned &Replacements);
extern DELPHI_PACKAGE System::ShortString __fastcall ReplaceStringS(const System::ShortString &S, const System::ShortString &OldString, const System::ShortString &NewString, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceStringAllS(const System::UnicodeString S, const System::UnicodeString OldString, const System::UnicodeString NewString, unsigned &Replacements);
extern DELPHI_PACKAGE bool __fastcall LastWordS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &AWord, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastWordAbsS(const System::ShortString &S, const System::ShortString &WordDelims, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastStringS(const System::ShortString &S, const System::ShortString &AString, unsigned &Position);
extern DELPHI_PACKAGE System::ShortString __fastcall KeepCharsS(const System::ShortString &S, const System::ShortString &Chars);
extern DELPHI_PACKAGE System::ShortString __fastcall RepeatStringS(const System::ShortString &RepeatString, unsigned &Repetitions, unsigned MaxLen);
extern DELPHI_PACKAGE System::ShortString __fastcall TrimCharsS(const System::ShortString &S, const System::ShortString &Chars);
extern DELPHI_PACKAGE System::ShortString __fastcall RightTrimCharsS(const System::ShortString &S, const System::ShortString &Chars);
extern DELPHI_PACKAGE System::ShortString __fastcall LeftTrimCharsS(const System::ShortString &S, const System::ShortString &Chars);
extern DELPHI_PACKAGE unsigned __fastcall ExtractTokensS(const System::UnicodeString S, const System::UnicodeString Delims, System::WideChar QuoteChar, bool AllowNulls, System::Classes::TStrings* Tokens);
extern DELPHI_PACKAGE bool __fastcall ContainsOnlyS(const System::ShortString &S, const System::ShortString &Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall ContainsOtherThanS(const System::ShortString &S, const System::ShortString &Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaS(System::WideChar C);
extern DELPHI_PACKAGE bool __fastcall IsChNumericS(char C, const System::ShortString &Numbers);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaNumericS(System::WideChar C, const System::UnicodeString Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaS(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall IsStrNumericS(const System::ShortString &S, const System::ShortString &Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaNumericS(const System::UnicodeString S, const System::UnicodeString Numbers);
extern DELPHI_PACKAGE bool __fastcall StrWithinS(const System::ShortString &S, const System::ShortString &SearchStr, unsigned Start, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall WordPosS(const System::ShortString &S, const System::ShortString &WordDelims, const System::ShortString &AWord, unsigned N, unsigned &Position);
}	/* namespace Ststrs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSTRS)
using namespace Ststrs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StstrsHPP
