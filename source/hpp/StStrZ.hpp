// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StStrZ.pas' rev: 29.00 (Windows)

#ifndef StstrzHPP
#define StstrzHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <StConst.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ststrz
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE char * __fastcall HexBZ(char * Dest, System::Byte B)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall HexBZ(System::WideChar * Dest, System::Byte B)/* overload */;
extern DELPHI_PACKAGE char * __fastcall HexWZ(char * Dest, System::Word W);
extern DELPHI_PACKAGE char * __fastcall HexLZ(char * Dest, int L);
extern DELPHI_PACKAGE char * __fastcall HexPtrZ(char * Dest, void * P);
extern DELPHI_PACKAGE char * __fastcall BinaryBZ(char * Dest, System::Byte B);
extern DELPHI_PACKAGE char * __fastcall BinaryWZ(char * Dest, System::Word W);
extern DELPHI_PACKAGE char * __fastcall BinaryLZ(char * Dest, int L);
extern DELPHI_PACKAGE char * __fastcall OctalBZ(char * Dest, System::Byte B);
extern DELPHI_PACKAGE char * __fastcall OctalWZ(char * Dest, System::Word W);
extern DELPHI_PACKAGE char * __fastcall OctalLZ(char * Dest, int L);
extern DELPHI_PACKAGE char * __fastcall CharStrZ(char * Dest, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall PadChPrimZ(char * S, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall PadPrimZ(char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall LeftPadChPrimZ(char * S, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall LeftPadPrimZ(char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall PadChZ(char * Dest, char * S, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall PadZ(char * Dest, char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall LeftPadChZ(char * Dest, char * S, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall LeftPadZ(char * Dest, char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall TrimLeadPrimZ(char * S);
extern DELPHI_PACKAGE char * __fastcall TrimLeadZ(char * Dest, char * S);
extern DELPHI_PACKAGE char * __fastcall TrimTrailPrimZ(char * S)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall TrimTrailPrimZ(System::WideChar * S)/* overload */;
extern DELPHI_PACKAGE char * __fastcall TrimTrailZ(char * Dest, char * S);
extern DELPHI_PACKAGE char * __fastcall TrimPrimZ(char * S);
extern DELPHI_PACKAGE char * __fastcall TrimZ(char * Dest, char * S);
extern DELPHI_PACKAGE char * __fastcall TrimSpacesPrimZ(char * S)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall TrimSpacesPrimZ(System::WideChar * S)/* overload */;
extern DELPHI_PACKAGE char * __fastcall TrimSpacesZ(char * Dest, char * S);
extern DELPHI_PACKAGE char * __fastcall CenterChPrimZ(char * S, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall CenterChZ(char * Dest, char * S, char C, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall CenterPrimZ(char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall CenterZ(char * Dest, char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall ScramblePrimZ(char * S, char * Key);
extern DELPHI_PACKAGE char * __fastcall ScrambleZ(char * Dest, char * S, char * Key);
extern DELPHI_PACKAGE char * __fastcall SubstituteZ(char * Dest, char * Src, char * FromStr, char * ToStr);
extern DELPHI_PACKAGE char * __fastcall FilterZ(char * Dest, char * Src, char * Filters);
extern DELPHI_PACKAGE char * __fastcall EntabZ(char * Dest, char * Src, System::Byte TabSize);
extern DELPHI_PACKAGE char * __fastcall DetabZ(char * Dest, char * Src, System::Byte TabSize);
extern DELPHI_PACKAGE bool __fastcall HasExtensionZ(char * Name, unsigned &DotPos)/* overload */;
extern DELPHI_PACKAGE bool __fastcall HasExtensionZ(System::WideChar * Name, unsigned &DotPos)/* overload */;
extern DELPHI_PACKAGE char * __fastcall DefaultExtensionZ(char * Dest, char * Name, char * Ext);
extern DELPHI_PACKAGE char * __fastcall ForceExtensionZ(char * Dest, char * Name, char * Ext);
extern DELPHI_PACKAGE char * __fastcall JustExtensionZ(char * Dest, char * Name);
extern DELPHI_PACKAGE char * __fastcall JustFilenameZ(char * Dest, char * PathName)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall JustFilenameZ(System::WideChar * Dest, System::WideChar * PathName)/* overload */;
extern DELPHI_PACKAGE char * __fastcall JustNameZ(char * Dest, char * PathName);
extern DELPHI_PACKAGE char * __fastcall JustPathnameZ(char * Dest, char * PathName);
extern DELPHI_PACKAGE char * __fastcall AddBackSlashZ(char * Dest, char * DirName);
extern DELPHI_PACKAGE char * __fastcall CleanPathNameZ(char * Dest, char * PathName);
extern DELPHI_PACKAGE bool __fastcall Str2Int16Z(char * S, short &I);
extern DELPHI_PACKAGE bool __fastcall Str2WordZ(char * S, System::Word &I);
extern DELPHI_PACKAGE bool __fastcall Str2LongZ(char * S, int &I);
extern DELPHI_PACKAGE bool __fastcall Str2RealZ(System::WideChar * S, double &R);
extern DELPHI_PACKAGE bool __fastcall Str2ExtZ(char * S, System::Extended &R);
extern DELPHI_PACKAGE char * __fastcall Long2StrZ(char * Dest, int L);
extern DELPHI_PACKAGE char * __fastcall Real2StrZ(char * Dest, double R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE char * __fastcall Ext2StrZ(char * Dest, System::Extended R, System::Byte Width, System::Int8 Places);
extern DELPHI_PACKAGE char * __fastcall ValPrepZ(char * S)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall ValPrepZ(System::WideChar * S)/* overload */;
extern DELPHI_PACKAGE bool __fastcall CharExistsZ(char * S, char C)/* overload */;
extern DELPHI_PACKAGE bool __fastcall CharExistsZ(System::WideChar * S, System::WideChar C)/* overload */;
extern DELPHI_PACKAGE unsigned __fastcall CharCountZ(char * S, char C);
extern DELPHI_PACKAGE unsigned __fastcall WordCountZ(char * S, char * WordDelims);
extern DELPHI_PACKAGE bool __fastcall WordPositionZ(unsigned N, char * S, char * WordDelims, unsigned &Pos);
extern DELPHI_PACKAGE char * __fastcall ExtractWordZ(char * Dest, unsigned N, char * Src, char * WordDelims);
extern DELPHI_PACKAGE unsigned __fastcall AsciiCountZ(char * S, char * WordDelims, char Quote);
extern DELPHI_PACKAGE bool __fastcall AsciiPositionZ(unsigned N, char * S, char * WordDelims, char Quote, unsigned &Pos);
extern DELPHI_PACKAGE char * __fastcall ExtractAsciiZ(char * Dest, unsigned N, char * Src, char * WordDelims, char Quote);
extern DELPHI_PACKAGE void __fastcall WordWrapZ(char * Dest, char * InSt, char * Overlap, unsigned Margin, bool PadToMargin);
extern DELPHI_PACKAGE int __fastcall CompStringZ(char * S1, char * S2);
extern DELPHI_PACKAGE int __fastcall CompUCStringZ(char * S1, char * S2);
extern DELPHI_PACKAGE char * __fastcall SoundexZ(char * Dest, char * S);
extern DELPHI_PACKAGE int __fastcall MakeLetterSetZ(char * S);
extern DELPHI_PACKAGE void __fastcall BMMakeTableZ(char * MatchString, Stbase::BTable &BT);
extern DELPHI_PACKAGE bool __fastcall BMSearchZ(void *Buffer, unsigned BufLength, Stbase::BTable &BT, char * MatchString, unsigned &Pos);
extern DELPHI_PACKAGE bool __fastcall BMSearchUCZ(void *Buffer, unsigned BufLength, Stbase::BTable &BT, char * MatchString, unsigned &Pos);
extern DELPHI_PACKAGE char * __fastcall CommaizeChZ(char * Dest, int L, char Ch);
extern DELPHI_PACKAGE char * __fastcall CommaizeZ(char * Dest, int L);
extern DELPHI_PACKAGE char * __fastcall FloatFormZ(char * Dest, char * Mask, System::Extended R, char * LtCurr, char * RtCurr, char Sep, char DecPt);
extern DELPHI_PACKAGE char * __fastcall LongIntFormZ(char * Dest, char * Mask, int L, char * LtCurr, char * RtCurr, char Sep);
extern DELPHI_PACKAGE bool __fastcall StrChPosZ(char * P, char C, unsigned &Pos)/* overload */;
extern DELPHI_PACKAGE bool __fastcall StrChPosZ(System::WideChar * P, System::WideChar C, unsigned &Pos)/* overload */;
extern DELPHI_PACKAGE bool __fastcall StrStPosZ(char * P, char * S, unsigned &Pos);
extern DELPHI_PACKAGE char * __fastcall StrChInsertPrimZ(char * Dest, char C, unsigned Pos);
extern DELPHI_PACKAGE char * __fastcall StrStInsertPrimZ(char * Dest, char * S, unsigned Pos);
extern DELPHI_PACKAGE char * __fastcall StrStCopyZ(char * Dest, char * S, unsigned Pos, unsigned Count)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall StrStCopyZ(System::WideChar * Dest, System::WideChar * S, unsigned Pos, unsigned Count)/* overload */;
extern DELPHI_PACKAGE char * __fastcall StrChDeletePrimZ(char * P, unsigned Pos);
extern DELPHI_PACKAGE char * __fastcall StrStDeletePrimZ(char * P, unsigned Pos, unsigned Count)/* overload */;
extern DELPHI_PACKAGE System::WideChar * __fastcall StrStDeletePrimZ(System::WideChar * P, unsigned Pos, unsigned Count)/* overload */;
extern DELPHI_PACKAGE char * __fastcall StrChDeleteZ(char * Dest, char * S, unsigned Pos);
extern DELPHI_PACKAGE char * __fastcall StrStDeleteZ(char * Dest, char * S, unsigned Pos, unsigned Count);
extern DELPHI_PACKAGE char * __fastcall StrChInsertZ(char * Dest, char * S, char C, unsigned Pos);
extern DELPHI_PACKAGE char * __fastcall StrStInsertZ(char * Dest, char * S1, char * S2, unsigned Pos);
extern DELPHI_PACKAGE char * __fastcall CopyLeftZ(char * Dest, char * S, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall CopyMidZ(char * Dest, char * S, unsigned First, unsigned Len);
extern DELPHI_PACKAGE char * __fastcall CopyRightZ(char * Dest, char * S, unsigned First);
extern DELPHI_PACKAGE char * __fastcall CopyRightAbsZ(char * Dest, char * S, unsigned NumChars);
extern DELPHI_PACKAGE bool __fastcall WordPosZ(char * S, char * WordDelims, char * AWord, unsigned N, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall CopyFromNthWordZ(char * Dest, char * S, char * WordDelims, char * AWord, unsigned N);
extern DELPHI_PACKAGE bool __fastcall DeleteFromNthWordZ(char * Dest, char * S, char * WordDelims, char * AWord, unsigned N);
extern DELPHI_PACKAGE bool __fastcall CopyFromToWordZ(char * Dest, char * S, char * WordDelims, char * Word1, char * Word2, unsigned N1, unsigned N2);
extern DELPHI_PACKAGE bool __fastcall DeleteFromToWordZ(char * Dest, char * S, char * WordDelims, char * Word1, char * Word2, unsigned N1, unsigned N2);
extern DELPHI_PACKAGE char * __fastcall CopyWithinZ(char * Dest, char * S, char * Delimiter, bool Strip);
extern DELPHI_PACKAGE char * __fastcall DeleteWithinZ(char * Dest, char * S, char * Delimiter);
extern DELPHI_PACKAGE char * __fastcall ReplaceWordZ(char * Dest, char * S, char * WordDelims, char * OldWord, char * NewWord, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE char * __fastcall ReplaceWordAllZ(char * Dest, char * S, char * WordDelims, char * OldWord, char * NewWord, unsigned &Replacements);
extern DELPHI_PACKAGE char * __fastcall ReplaceStringZ(char * Dest, char * S, char * OldString, char * NewString, unsigned N, unsigned &Replacements);
extern DELPHI_PACKAGE char * __fastcall ReplaceStringAllZ(char * Dest, char * S, char * OldString, char * NewString, unsigned &Replacements);
extern DELPHI_PACKAGE bool __fastcall LastWordZ(char * S, char * WordDelims, char * AWord, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastWordAbsZ(char * S, char * WordDelims, unsigned &Position);
extern DELPHI_PACKAGE bool __fastcall LastStringZ(char * S, char * AString, unsigned &Position);
extern DELPHI_PACKAGE char * __fastcall KeepCharsZ(char * Dest, char * S, char * Chars);
extern DELPHI_PACKAGE char * __fastcall RepeatStringZ(char * Dest, char * RepeatString, unsigned &Repetitions, unsigned MaxLen);
extern DELPHI_PACKAGE char * __fastcall TrimCharsZ(char * Dest, char * S, char * Chars);
extern DELPHI_PACKAGE char * __fastcall RightTrimCharsZ(char * Dest, char * S, char * Chars);
extern DELPHI_PACKAGE char * __fastcall LeftTrimCharsZ(char * Dest, char * S, char * Chars);
extern DELPHI_PACKAGE unsigned __fastcall ExtractTokensZ(char * S, char * Delims, char QuoteChar, bool AllowNulls, System::Classes::TStrings* Tokens);
extern DELPHI_PACKAGE bool __fastcall ContainsOnlyZ(const char * S, const char * Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall ContainsOtherThanZ(const char * S, const char * Chars, unsigned &BadPos);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaZ(System::WideChar C);
extern DELPHI_PACKAGE bool __fastcall IsChNumericZ(char C, char * Numbers);
extern DELPHI_PACKAGE bool __fastcall IsChAlphaNumericZ(System::WideChar C, System::WideChar * Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaZ(System::WideChar * S);
extern DELPHI_PACKAGE bool __fastcall IsStrNumericZ(char * S, char * Numbers);
extern DELPHI_PACKAGE bool __fastcall IsStrAlphaNumericZ(System::WideChar * S, System::WideChar * Numbers);
extern DELPHI_PACKAGE bool __fastcall StrWithinZ(char * S, char * SearchStr, unsigned Start, unsigned &Position);
}	/* namespace Ststrz */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSTRZ)
using namespace Ststrz;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StstrzHPP
