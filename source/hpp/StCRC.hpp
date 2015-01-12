// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StCRC.pas' rev: 28.00 (Windows)

#ifndef StcrcHPP
#define StcrcHPP

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
#include <StBase.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stcrc
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
static const System::Word CrcBufSize = System::Word(0x800);
static const System::Int8 CrcFileMode = System::Int8(0x20);
extern DELPHI_PACKAGE System::StaticArray<unsigned, 256> CrcTable;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 256> Crc32Table;
extern DELPHI_PACKAGE int __fastcall Adler32Prim(void *Data, unsigned DataSize, int CurCrc);
extern DELPHI_PACKAGE int __fastcall Adler32OfStream(System::Classes::TStream* Stream, int CurCrc);
extern DELPHI_PACKAGE int __fastcall Adler32OfFile(System::UnicodeString FileName);
extern DELPHI_PACKAGE unsigned __fastcall Crc16Prim(void *Data, unsigned DataSize, unsigned CurCrc);
extern DELPHI_PACKAGE unsigned __fastcall Crc16OfStream(System::Classes::TStream* Stream, unsigned CurCrc);
extern DELPHI_PACKAGE unsigned __fastcall Crc16OfFile(System::UnicodeString FileName);
extern DELPHI_PACKAGE int __fastcall Crc32Prim(void *Data, unsigned DataSize, int CurCrc);
extern DELPHI_PACKAGE int __fastcall Crc32OfStream(System::Classes::TStream* Stream, int CurCrc);
extern DELPHI_PACKAGE int __fastcall Crc32OfFile(System::UnicodeString FileName);
extern DELPHI_PACKAGE unsigned __fastcall InternetSumPrim(void *Data, unsigned DataSize, unsigned CurCrc);
extern DELPHI_PACKAGE unsigned __fastcall InternetSumOfStream(System::Classes::TStream* Stream, unsigned CurCrc);
extern DELPHI_PACKAGE unsigned __fastcall InternetSumOfFile(System::UnicodeString FileName);
extern DELPHI_PACKAGE unsigned __fastcall Kermit16Prim(void *Data, unsigned DataSize, unsigned CurCrc);
extern DELPHI_PACKAGE unsigned __fastcall Kermit16OfStream(System::Classes::TStream* Stream, unsigned CurCrc);
extern DELPHI_PACKAGE unsigned __fastcall Kermit16OfFile(System::UnicodeString FileName);
}	/* namespace Stcrc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STCRC)
using namespace Stcrc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StcrcHPP
