// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StConst.pas' rev: 33.00 (Windows)

#ifndef StconstHPP
#define StconstHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stconst
{
//-- forward type declarations -----------------------------------------------
struct StStrRec;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD StStrRec
{
public:
	int ID;
	System::UnicodeString Str;
};


typedef System::StaticArray<StStrRec, 175> Stconst__1;

//-- var, const, procedure ---------------------------------------------------
#define StVersionStr L"4.04"
static const System::Int8 stscFalseString = System::Int8(0x0);
static const System::Int8 stscTrueString = System::Int8(0x1);
static const System::Int8 stscNoFileKey = System::Int8(0x2);
static const System::Int8 stscInvalidPKey = System::Int8(0x3);
static const System::Int8 stscNoWin32S = System::Int8(0x4);
static const System::Int8 stscCreateKeyFail = System::Int8(0x5);
static const System::Int8 stscOpenKeyFail = System::Int8(0x6);
static const System::Int8 stscIniWriteFail = System::Int8(0x7);
static const System::Int8 stscRegWriteFail = System::Int8(0x8);
static const System::Int8 stscNoKeyName = System::Int8(0x9);
static const System::Int8 stscQueryKeyFail = System::Int8(0xa);
static const System::Int8 stscEnumKeyFail = System::Int8(0xb);
static const System::Int8 stscEnumValueFail = System::Int8(0xc);
static const System::Int8 stscIniDeleteFail = System::Int8(0xd);
static const System::Int8 stscKeyHasSubKeys = System::Int8(0xe);
static const System::Int8 stscDeleteKeyFail = System::Int8(0xf);
static const System::Int8 stscIniDelValueFail = System::Int8(0x10);
static const System::Int8 stscRegDelValueFail = System::Int8(0x11);
static const System::Int8 stscOutputFileExists = System::Int8(0x12);
static const System::Int8 stscFileHasExtension = System::Int8(0x13);
static const System::Int8 stscSaveKeyFail = System::Int8(0x14);
static const System::Int8 stscNo16bitSupport = System::Int8(0x15);
static const System::Int8 stscCantFindInputFile = System::Int8(0x16);
static const System::Int8 stscLoadKeyFail = System::Int8(0x17);
static const System::Int8 stscUnloadKeyFail = System::Int8(0x18);
static const System::Int8 stscNotWinNTPlatform = System::Int8(0x19);
static const System::Int8 stscBadOptionsKeyCombo = System::Int8(0x1a);
static const System::Int8 stscRestoreKeyFail = System::Int8(0x1b);
static const System::Int8 stscReplaceKeyFail = System::Int8(0x1c);
static const System::Int8 stscNoIniFileSupport = System::Int8(0x1d);
static const System::Int8 stscRemoteKeyIsOpen = System::Int8(0x1e);
static const System::Int8 stscConnectRemoteKeyFail = System::Int8(0x1f);
static const System::Int8 stscCloseRemoteKeyFail = System::Int8(0x20);
static const System::Int8 stscFlushKeyFail = System::Int8(0x21);
static const System::Int8 stscBufferDataSizesDif = System::Int8(0x22);
static const System::Int8 stscKeyIsEmptyNotExists = System::Int8(0x23);
static const System::Int8 stscGetSecurityFail = System::Int8(0x24);
static const System::Int8 stscSetSecurityFail = System::Int8(0x25);
static const System::Int8 stscByteArrayTooLarge = System::Int8(0x26);
static const System::Int8 stscQueryValueFail = System::Int8(0x27);
static const System::Int8 stscNoValueNameSpecified = System::Int8(0x28);
static const System::Int8 stscNoCompare = System::Int8(0x33);
static const System::Int8 stscBadType = System::Int8(0x34);
static const System::Int8 stscBadSize = System::Int8(0x35);
static const System::Int8 stscDupNode = System::Int8(0x36);
static const System::Int8 stscBadIndex = System::Int8(0x37);
static const System::Int8 stscBadWinMode = System::Int8(0x38);
static const System::Int8 stscUnknownClass = System::Int8(0x39);
static const System::Int8 stscUnknownNodeClass = System::Int8(0x3a);
static const System::Int8 stscNoStoreData = System::Int8(0x3b);
static const System::Int8 stscNoLoadData = System::Int8(0x3c);
static const System::Int8 stscWrongClass = System::Int8(0x3d);
static const System::Int8 stscWrongNodeClass = System::Int8(0x3e);
static const System::Int8 stscBadCompare = System::Int8(0x3f);
static const System::Int8 stscTooManyCols = System::Int8(0x40);
static const System::Int8 stscBadColCount = System::Int8(0x41);
static const System::Int8 stscBadElSize = System::Int8(0x42);
static const System::Int8 stscBadDups = System::Int8(0x43);
static const System::Int8 stscTooManyFiles = System::Int8(0x47);
static const System::Int8 stscFileCreate = System::Int8(0x48);
static const System::Int8 stscFileOpen = System::Int8(0x49);
static const System::Int8 stscFileWrite = System::Int8(0x4a);
static const System::Int8 stscFileRead = System::Int8(0x4b);
static const System::Int8 stscBadState = System::Int8(0x4c);
static const System::Int8 stscBcdBadFormat = System::Int8(0x51);
static const System::Int8 stscBcdOverflow = System::Int8(0x52);
static const System::Int8 stscBcdDivByZero = System::Int8(0x53);
static const System::Int8 stscBcdBadInput = System::Int8(0x54);
static const System::Int8 stscBcdBufOverflow = System::Int8(0x55);
static const System::Int8 stscNoVerInfo = System::Int8(0x64);
static const System::Int8 stscVerInfoFail = System::Int8(0x65);
static const System::Byte stscBadVerInfoKey = System::Byte(0x86);
static const System::Byte stscInvalidUPCACodeLen = System::Byte(0x8c);
static const System::Byte stscInvalidCharacter = System::Byte(0x8d);
static const System::Byte stscInvalidCheckCharacter = System::Byte(0x8e);
static const System::Byte stscInvalidUPCECodeLen = System::Byte(0x8f);
static const System::Byte stscInvalidEAN8CodeLen = System::Byte(0x90);
static const System::Byte stscInvalidEAN13CodeLen = System::Byte(0x91);
static const System::Byte stscInvalidSupCodeLen = System::Byte(0x92);
static const System::Byte stscExprEmpty = System::Byte(0x96);
static const System::Byte stscExprBadNum = System::Byte(0x97);
static const System::Byte stscExprBadChar = System::Byte(0x98);
static const System::Byte stscExprOpndExp = System::Byte(0x99);
static const System::Byte stscExprNumeric = System::Byte(0x9a);
static const System::Byte stscExprBadExp = System::Byte(0x9b);
static const System::Byte stscExprOpndOvfl = System::Byte(0x9c);
static const System::Byte stscExprUnkFunc = System::Byte(0x9d);
static const System::Byte stscExprLParExp = System::Byte(0x9e);
static const System::Byte stscExprRParExp = System::Byte(0x9f);
static const System::Byte stscExprCommExp = System::Byte(0xa0);
static const System::Byte stscExprDupIdent = System::Byte(0xa1);
static const System::Byte stscStatBadCount = System::Byte(0xaa);
static const System::Byte stscStatBadParam = System::Byte(0xab);
static const System::Byte stscStatBadData = System::Byte(0xac);
static const System::Byte stscStatNoConverge = System::Byte(0xad);
static const System::Byte stscFinBadArg = System::Byte(0xb4);
static const System::Byte stscFinNoConverge = System::Byte(0xb5);
static const System::Byte stscBadEncodeFmt = System::Byte(0xbe);
static const System::Byte stscBadAttachment = System::Byte(0xbf);
static const System::Byte stscDupeString = System::Byte(0xc0);
static const System::Byte stscInStream = System::Byte(0xc1);
static const System::Byte stscOutOfBounds = System::Byte(0xc8);
static const System::Byte stscInvalidLength = System::Byte(0xd2);
static const System::Byte stscNoInputFile = System::Byte(0xd7);
static const System::Byte stscNoOutputFile = System::Byte(0xd8);
static const System::Byte stscInFileError = System::Byte(0xd9);
static const System::Byte stscOutFileError = System::Byte(0xda);
static const System::Byte stscWordDelimiters = System::Byte(0xdb);
static const System::Byte stscInvalidSLEntry = System::Byte(0xdc);
static const System::Byte stscBadStream = System::Byte(0xdd);
static const System::Byte stscName = System::Byte(0xe6);
static const System::Byte stscSize = System::Byte(0xe7);
static const System::Byte stscType = System::Byte(0xe8);
static const System::Byte stscModified = System::Byte(0xe9);
static const System::Byte stscAttributes = System::Byte(0xea);
static const System::Byte stscFileFolder = System::Byte(0xeb);
static const System::Byte stscSystemFolder = System::Byte(0xec);
static const System::Byte stscOriginalLoc = System::Byte(0xed);
static const System::Byte stscDateDeleted = System::Byte(0xee);
static const System::Byte stscFile = System::Byte(0xef);
static const System::Byte stscInvalidFolder = System::Byte(0xf0);
static const System::Byte stscFolderReadOnly = System::Byte(0xf1);
static const System::Byte stscInsufficientData = System::Byte(0xfa);
static const System::Word stscCreateFileFailed = System::Word(0x104);
static const System::Word stscFileMappingFailed = System::Word(0x105);
static const System::Word stscCreateViewFailed = System::Word(0x106);
static const System::Word stscBadOrigin = System::Word(0x107);
static const System::Word stscGetSizeFailed = System::Word(0x108);
static const System::Word stscNilStream = System::Word(0x10e);
static const System::Word stscNoSeekForRead = System::Word(0x10f);
static const System::Word stscNoSeekForWrite = System::Word(0x110);
static const System::Word stscCannotWrite = System::Word(0x111);
static const System::Word stscBadTerminator = System::Word(0x112);
static const System::Word stscBadLineLength = System::Word(0x113);
static const System::Word stscCannotSetSize = System::Word(0x114);
static const System::Word stscUnknownError = System::Word(0x122);
static const System::Word stscExpandingClass = System::Word(0x123);
static const System::Word stscAlternationFollowsClosure = System::Word(0x124);
static const System::Word stscUnbalancedParens = System::Word(0x125);
static const System::Word stscFollowingClosure = System::Word(0x126);
static const System::Word stscPatternError = System::Word(0x127);
static const System::Word stscUnbalancedTag = System::Word(0x128);
static const System::Word stscNoPatterns = System::Word(0x129);
static const System::Word stscPatternTooLarge = System::Word(0x12a);
static const System::Word stscStreamsNil = System::Word(0x12b);
static const System::Word stscInTextStreamError = System::Word(0x12c);
static const System::Word stscOutTextStreamError = System::Word(0x12d);
static const System::Word stscClosureMaybeEmpty = System::Word(0x12e);
static const System::Word stscInFileNotFound = System::Word(0x12f);
static const System::Word stscREInFileError = System::Word(0x130);
static const System::Word stscOutFileDelete = System::Word(0x131);
static const System::Word stscOutFileCreate = System::Word(0x132);
static const System::Word stscNetNoManualCreate = System::Word(0x140);
static const System::Word stscNetUnknownError = System::Word(0x141);
static const System::Word stscNetGroupNotSpecified = System::Word(0x142);
static const System::Word stscNetDateSpecifiedOutOfRange = System::Word(0x143);
static const System::Word stscNetInvalidParameter = System::Word(0x144);
static const System::Word stscNetInvalidItemType = System::Word(0x145);
static const System::Word stscMoneyNilResult = System::Word(0x191);
static const System::Word stscMoneyNilParameter = System::Word(0x192);
static const System::Word stscMoneyCurrenciesNotMatch = System::Word(0x193);
static const System::Word stscMoneyNoExchangeRatesAvail = System::Word(0x19a);
static const System::Word stscMoneyInvalidExchangeParams = System::Word(0x19b);
static const System::Word stscMoneyInvalidTriangleExchange = System::Word(0x19c);
static const System::Word stscMoneyNoSuchExchange = System::Word(0x19d);
static const System::Word stscMoneyMissingIntermediateRate = System::Word(0x19e);
static const System::Word stscMoneyInvalidExchRate = System::Word(0x19f);
static const System::Word stscMoneyTriExchUsesTriExch = System::Word(0x19f);
static const System::Word stscDecMathRoundPlaces = System::Word(0x1a7);
static const System::Word stscDecMathAsIntOverflow = System::Word(0x1a8);
static const System::Word stscDecMathConversion = System::Word(0x1a9);
static const System::Word stscDecMathDivByZero = System::Word(0x1aa);
static const System::Word stscDecMathNegExp = System::Word(0x1ab);
static const System::Word stscDecMathMultOverflow = System::Word(0x1ac);
static const System::Word stscDecMathDivOverflow = System::Word(0x1ad);
static const System::Word stscTxtDatNoSuchField = System::Word(0x1ae);
static const System::Word stscTxtDatUniqueNameRequired = System::Word(0x1af);
static const System::Word stscTxtDatUnhandledVariant = System::Word(0x1b0);
static const System::Word stscTxtDatInvalidSchema = System::Word(0x1b1);
static const System::Word stscTxtDatRecordSetOpen = System::Word(0x1b2);
static const System::Word stscPRNGDegFreedom = System::Word(0x1cc);
static const System::Word stscPRNGBetaShape = System::Word(0x1cd);
static const System::Word stscPRNGMean = System::Word(0x1ce);
static const System::Word stscPRNGGammaShape = System::Word(0x1cf);
static const System::Word stscPRNGGammaScale = System::Word(0x1d0);
static const System::Word stscPRNGStdDev = System::Word(0x1d1);
static const System::Word stscPRNGWeibullShape = System::Word(0x1d2);
static const System::Word stscPRNGWeibullScale = System::Word(0x1d3);
static const System::Word stscPRNGLimit = System::Word(0x1d4);
static const System::Word stscPRNGUpperLimit = System::Word(0x1d5);
static const System::Word stscPRNGErlangOrder = System::Word(0x1d6);
extern DELPHI_PACKAGE System::ResourceString _stscSysStringListFull;
#define Stconst_stscSysStringListFull System::LoadResourceString(&Stconst::_stscSysStringListFull)
extern DELPHI_PACKAGE System::ResourceString _stscSysBadStartDir;
#define Stconst_stscSysBadStartDir System::LoadResourceString(&Stconst::_stscSysBadStartDir)
extern DELPHI_PACKAGE System::ResourceString _stscFalseStringS;
#define Stconst_stscFalseStringS System::LoadResourceString(&Stconst::_stscFalseStringS)
extern DELPHI_PACKAGE System::ResourceString _stscTrueStringS;
#define Stconst_stscTrueStringS System::LoadResourceString(&Stconst::_stscTrueStringS)
extern DELPHI_PACKAGE System::ResourceString _stscNoFileKeyS;
#define Stconst_stscNoFileKeyS System::LoadResourceString(&Stconst::_stscNoFileKeyS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidPKeyS;
#define Stconst_stscInvalidPKeyS System::LoadResourceString(&Stconst::_stscInvalidPKeyS)
extern DELPHI_PACKAGE System::ResourceString _stscNoWin32SS;
#define Stconst_stscNoWin32SS System::LoadResourceString(&Stconst::_stscNoWin32SS)
extern DELPHI_PACKAGE System::ResourceString _stscCreateKeyFailS;
#define Stconst_stscCreateKeyFailS System::LoadResourceString(&Stconst::_stscCreateKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscOpenKeyFailS;
#define Stconst_stscOpenKeyFailS System::LoadResourceString(&Stconst::_stscOpenKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscIniWriteFailS;
#define Stconst_stscIniWriteFailS System::LoadResourceString(&Stconst::_stscIniWriteFailS)
extern DELPHI_PACKAGE System::ResourceString _stscRegWriteFailS;
#define Stconst_stscRegWriteFailS System::LoadResourceString(&Stconst::_stscRegWriteFailS)
extern DELPHI_PACKAGE System::ResourceString _stscNoKeyNameS;
#define Stconst_stscNoKeyNameS System::LoadResourceString(&Stconst::_stscNoKeyNameS)
extern DELPHI_PACKAGE System::ResourceString _stscQueryKeyFailS;
#define Stconst_stscQueryKeyFailS System::LoadResourceString(&Stconst::_stscQueryKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscEnumKeyFailS;
#define Stconst_stscEnumKeyFailS System::LoadResourceString(&Stconst::_stscEnumKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscEnumValueFailS;
#define Stconst_stscEnumValueFailS System::LoadResourceString(&Stconst::_stscEnumValueFailS)
extern DELPHI_PACKAGE System::ResourceString _stscIniDeleteFailS;
#define Stconst_stscIniDeleteFailS System::LoadResourceString(&Stconst::_stscIniDeleteFailS)
extern DELPHI_PACKAGE System::ResourceString _stscKeyHasSubKeysS;
#define Stconst_stscKeyHasSubKeysS System::LoadResourceString(&Stconst::_stscKeyHasSubKeysS)
extern DELPHI_PACKAGE System::ResourceString _stscDeleteKeyFailS;
#define Stconst_stscDeleteKeyFailS System::LoadResourceString(&Stconst::_stscDeleteKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscIniDelValueFailS;
#define Stconst_stscIniDelValueFailS System::LoadResourceString(&Stconst::_stscIniDelValueFailS)
extern DELPHI_PACKAGE System::ResourceString _stscRegDelValueFailS;
#define Stconst_stscRegDelValueFailS System::LoadResourceString(&Stconst::_stscRegDelValueFailS)
extern DELPHI_PACKAGE System::ResourceString _stscOutputFileExistsS;
#define Stconst_stscOutputFileExistsS System::LoadResourceString(&Stconst::_stscOutputFileExistsS)
extern DELPHI_PACKAGE System::ResourceString _stscFileHasExtensionS;
#define Stconst_stscFileHasExtensionS System::LoadResourceString(&Stconst::_stscFileHasExtensionS)
extern DELPHI_PACKAGE System::ResourceString _stscSaveKeyFailS;
#define Stconst_stscSaveKeyFailS System::LoadResourceString(&Stconst::_stscSaveKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscNo16bitSupportS;
#define Stconst_stscNo16bitSupportS System::LoadResourceString(&Stconst::_stscNo16bitSupportS)
extern DELPHI_PACKAGE System::ResourceString _stscCantFindInputFileS;
#define Stconst_stscCantFindInputFileS System::LoadResourceString(&Stconst::_stscCantFindInputFileS)
extern DELPHI_PACKAGE System::ResourceString _stscLoadKeyFailS;
#define Stconst_stscLoadKeyFailS System::LoadResourceString(&Stconst::_stscLoadKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscUnloadKeyFailS;
#define Stconst_stscUnloadKeyFailS System::LoadResourceString(&Stconst::_stscUnloadKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscNotWinNTPlatformS;
#define Stconst_stscNotWinNTPlatformS System::LoadResourceString(&Stconst::_stscNotWinNTPlatformS)
extern DELPHI_PACKAGE System::ResourceString _stscBadOptionsKeyComboS;
#define Stconst_stscBadOptionsKeyComboS System::LoadResourceString(&Stconst::_stscBadOptionsKeyComboS)
extern DELPHI_PACKAGE System::ResourceString _stscRestoreKeyFailS;
#define Stconst_stscRestoreKeyFailS System::LoadResourceString(&Stconst::_stscRestoreKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscReplaceKeyFailS;
#define Stconst_stscReplaceKeyFailS System::LoadResourceString(&Stconst::_stscReplaceKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscNoIniFileSupportS;
#define Stconst_stscNoIniFileSupportS System::LoadResourceString(&Stconst::_stscNoIniFileSupportS)
extern DELPHI_PACKAGE System::ResourceString _stscRemoteKeyIsOpenS;
#define Stconst_stscRemoteKeyIsOpenS System::LoadResourceString(&Stconst::_stscRemoteKeyIsOpenS)
extern DELPHI_PACKAGE System::ResourceString _stscConnectRemoteKeyFailS;
#define Stconst_stscConnectRemoteKeyFailS System::LoadResourceString(&Stconst::_stscConnectRemoteKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscCloseRemoteKeyFailS;
#define Stconst_stscCloseRemoteKeyFailS System::LoadResourceString(&Stconst::_stscCloseRemoteKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscFlushKeyFailS;
#define Stconst_stscFlushKeyFailS System::LoadResourceString(&Stconst::_stscFlushKeyFailS)
extern DELPHI_PACKAGE System::ResourceString _stscBufferDataSizesDifS;
#define Stconst_stscBufferDataSizesDifS System::LoadResourceString(&Stconst::_stscBufferDataSizesDifS)
extern DELPHI_PACKAGE System::ResourceString _stscKeyIsEmptyNotExistsS;
#define Stconst_stscKeyIsEmptyNotExistsS System::LoadResourceString(&Stconst::_stscKeyIsEmptyNotExistsS)
extern DELPHI_PACKAGE System::ResourceString _stscGetSecurityFailS;
#define Stconst_stscGetSecurityFailS System::LoadResourceString(&Stconst::_stscGetSecurityFailS)
extern DELPHI_PACKAGE System::ResourceString _stscSetSecurityFailS;
#define Stconst_stscSetSecurityFailS System::LoadResourceString(&Stconst::_stscSetSecurityFailS)
extern DELPHI_PACKAGE System::ResourceString _stscByteArrayTooLargeS;
#define Stconst_stscByteArrayTooLargeS System::LoadResourceString(&Stconst::_stscByteArrayTooLargeS)
extern DELPHI_PACKAGE System::ResourceString _stscQueryValueFailS;
#define Stconst_stscQueryValueFailS System::LoadResourceString(&Stconst::_stscQueryValueFailS)
extern DELPHI_PACKAGE System::ResourceString _stscNoValueNameSpecifiedS;
#define Stconst_stscNoValueNameSpecifiedS System::LoadResourceString(&Stconst::_stscNoValueNameSpecifiedS)
extern DELPHI_PACKAGE System::ResourceString _stscNoCompareS;
#define Stconst_stscNoCompareS System::LoadResourceString(&Stconst::_stscNoCompareS)
extern DELPHI_PACKAGE System::ResourceString _stscBadTypeS;
#define Stconst_stscBadTypeS System::LoadResourceString(&Stconst::_stscBadTypeS)
extern DELPHI_PACKAGE System::ResourceString _stscBadSizeS;
#define Stconst_stscBadSizeS System::LoadResourceString(&Stconst::_stscBadSizeS)
extern DELPHI_PACKAGE System::ResourceString _stscDupNodeS;
#define Stconst_stscDupNodeS System::LoadResourceString(&Stconst::_stscDupNodeS)
extern DELPHI_PACKAGE System::ResourceString _stscBadIndexS;
#define Stconst_stscBadIndexS System::LoadResourceString(&Stconst::_stscBadIndexS)
extern DELPHI_PACKAGE System::ResourceString _stscBadWinModeS;
#define Stconst_stscBadWinModeS System::LoadResourceString(&Stconst::_stscBadWinModeS)
extern DELPHI_PACKAGE System::ResourceString _stscUnknownClassS;
#define Stconst_stscUnknownClassS System::LoadResourceString(&Stconst::_stscUnknownClassS)
extern DELPHI_PACKAGE System::ResourceString _stscUnknownNodeClassS;
#define Stconst_stscUnknownNodeClassS System::LoadResourceString(&Stconst::_stscUnknownNodeClassS)
extern DELPHI_PACKAGE System::ResourceString _stscNoStoreDataS;
#define Stconst_stscNoStoreDataS System::LoadResourceString(&Stconst::_stscNoStoreDataS)
extern DELPHI_PACKAGE System::ResourceString _stscNoLoadDataS;
#define Stconst_stscNoLoadDataS System::LoadResourceString(&Stconst::_stscNoLoadDataS)
extern DELPHI_PACKAGE System::ResourceString _stscWrongClassS;
#define Stconst_stscWrongClassS System::LoadResourceString(&Stconst::_stscWrongClassS)
extern DELPHI_PACKAGE System::ResourceString _stscWrongNodeClassS;
#define Stconst_stscWrongNodeClassS System::LoadResourceString(&Stconst::_stscWrongNodeClassS)
extern DELPHI_PACKAGE System::ResourceString _stscBadCompareS;
#define Stconst_stscBadCompareS System::LoadResourceString(&Stconst::_stscBadCompareS)
extern DELPHI_PACKAGE System::ResourceString _stscTooManyColsS;
#define Stconst_stscTooManyColsS System::LoadResourceString(&Stconst::_stscTooManyColsS)
extern DELPHI_PACKAGE System::ResourceString _stscBadColCountS;
#define Stconst_stscBadColCountS System::LoadResourceString(&Stconst::_stscBadColCountS)
extern DELPHI_PACKAGE System::ResourceString _stscBadElSizeS;
#define Stconst_stscBadElSizeS System::LoadResourceString(&Stconst::_stscBadElSizeS)
extern DELPHI_PACKAGE System::ResourceString _stscBadDupsS;
#define Stconst_stscBadDupsS System::LoadResourceString(&Stconst::_stscBadDupsS)
extern DELPHI_PACKAGE System::ResourceString _stscTooManyFilesS;
#define Stconst_stscTooManyFilesS System::LoadResourceString(&Stconst::_stscTooManyFilesS)
extern DELPHI_PACKAGE System::ResourceString _stscFileCreateS;
#define Stconst_stscFileCreateS System::LoadResourceString(&Stconst::_stscFileCreateS)
extern DELPHI_PACKAGE System::ResourceString _stscFileOpenS;
#define Stconst_stscFileOpenS System::LoadResourceString(&Stconst::_stscFileOpenS)
extern DELPHI_PACKAGE System::ResourceString _stscFileWriteS;
#define Stconst_stscFileWriteS System::LoadResourceString(&Stconst::_stscFileWriteS)
extern DELPHI_PACKAGE System::ResourceString _stscFileReadS;
#define Stconst_stscFileReadS System::LoadResourceString(&Stconst::_stscFileReadS)
extern DELPHI_PACKAGE System::ResourceString _stscBadStateS;
#define Stconst_stscBadStateS System::LoadResourceString(&Stconst::_stscBadStateS)
extern DELPHI_PACKAGE System::ResourceString _stscBcdBadFormatS;
#define Stconst_stscBcdBadFormatS System::LoadResourceString(&Stconst::_stscBcdBadFormatS)
extern DELPHI_PACKAGE System::ResourceString _stscBcdOverflowS;
#define Stconst_stscBcdOverflowS System::LoadResourceString(&Stconst::_stscBcdOverflowS)
extern DELPHI_PACKAGE System::ResourceString _stscBcdDivByZeroS;
#define Stconst_stscBcdDivByZeroS System::LoadResourceString(&Stconst::_stscBcdDivByZeroS)
extern DELPHI_PACKAGE System::ResourceString _stscBcdBadInputS;
#define Stconst_stscBcdBadInputS System::LoadResourceString(&Stconst::_stscBcdBadInputS)
extern DELPHI_PACKAGE System::ResourceString _stscBcdBufOverflowS;
#define Stconst_stscBcdBufOverflowS System::LoadResourceString(&Stconst::_stscBcdBufOverflowS)
extern DELPHI_PACKAGE System::ResourceString _stscNoVerInfoS;
#define Stconst_stscNoVerInfoS System::LoadResourceString(&Stconst::_stscNoVerInfoS)
extern DELPHI_PACKAGE System::ResourceString _stscVerInfoFailS;
#define Stconst_stscVerInfoFailS System::LoadResourceString(&Stconst::_stscVerInfoFailS)
extern DELPHI_PACKAGE System::ResourceString _stscBadVerInfoKeyS;
#define Stconst_stscBadVerInfoKeyS System::LoadResourceString(&Stconst::_stscBadVerInfoKeyS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidUPCACodeLenS;
#define Stconst_stscInvalidUPCACodeLenS System::LoadResourceString(&Stconst::_stscInvalidUPCACodeLenS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidCharacterS;
#define Stconst_stscInvalidCharacterS System::LoadResourceString(&Stconst::_stscInvalidCharacterS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidCheckCharacterS;
#define Stconst_stscInvalidCheckCharacterS System::LoadResourceString(&Stconst::_stscInvalidCheckCharacterS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidUPCECodeLenS;
#define Stconst_stscInvalidUPCECodeLenS System::LoadResourceString(&Stconst::_stscInvalidUPCECodeLenS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidEAN8CodeLenS;
#define Stconst_stscInvalidEAN8CodeLenS System::LoadResourceString(&Stconst::_stscInvalidEAN8CodeLenS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidEAN13CodeLenS;
#define Stconst_stscInvalidEAN13CodeLenS System::LoadResourceString(&Stconst::_stscInvalidEAN13CodeLenS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidSupCodeLenS;
#define Stconst_stscInvalidSupCodeLenS System::LoadResourceString(&Stconst::_stscInvalidSupCodeLenS)
extern DELPHI_PACKAGE System::ResourceString _stscFinBadArgS;
#define Stconst_stscFinBadArgS System::LoadResourceString(&Stconst::_stscFinBadArgS)
extern DELPHI_PACKAGE System::ResourceString _stscFinNoConvergeS;
#define Stconst_stscFinNoConvergeS System::LoadResourceString(&Stconst::_stscFinNoConvergeS)
extern DELPHI_PACKAGE System::ResourceString _stscExprEmptyS;
#define Stconst_stscExprEmptyS System::LoadResourceString(&Stconst::_stscExprEmptyS)
extern DELPHI_PACKAGE System::ResourceString _stscExprBadNumS;
#define Stconst_stscExprBadNumS System::LoadResourceString(&Stconst::_stscExprBadNumS)
extern DELPHI_PACKAGE System::ResourceString _stscExprBadCharS;
#define Stconst_stscExprBadCharS System::LoadResourceString(&Stconst::_stscExprBadCharS)
extern DELPHI_PACKAGE System::ResourceString _stscExprOpndExpS;
#define Stconst_stscExprOpndExpS System::LoadResourceString(&Stconst::_stscExprOpndExpS)
extern DELPHI_PACKAGE System::ResourceString _stscExprNumericS;
#define Stconst_stscExprNumericS System::LoadResourceString(&Stconst::_stscExprNumericS)
extern DELPHI_PACKAGE System::ResourceString _stscExprBadExpS;
#define Stconst_stscExprBadExpS System::LoadResourceString(&Stconst::_stscExprBadExpS)
extern DELPHI_PACKAGE System::ResourceString _stscExprOpndOvflS;
#define Stconst_stscExprOpndOvflS System::LoadResourceString(&Stconst::_stscExprOpndOvflS)
extern DELPHI_PACKAGE System::ResourceString _stscExprUnkFuncS;
#define Stconst_stscExprUnkFuncS System::LoadResourceString(&Stconst::_stscExprUnkFuncS)
extern DELPHI_PACKAGE System::ResourceString _stscExprLParExpS;
#define Stconst_stscExprLParExpS System::LoadResourceString(&Stconst::_stscExprLParExpS)
extern DELPHI_PACKAGE System::ResourceString _stscExprRParExpS;
#define Stconst_stscExprRParExpS System::LoadResourceString(&Stconst::_stscExprRParExpS)
extern DELPHI_PACKAGE System::ResourceString _stscExprCommExpS;
#define Stconst_stscExprCommExpS System::LoadResourceString(&Stconst::_stscExprCommExpS)
extern DELPHI_PACKAGE System::ResourceString _stscExprDupIdentS;
#define Stconst_stscExprDupIdentS System::LoadResourceString(&Stconst::_stscExprDupIdentS)
extern DELPHI_PACKAGE System::ResourceString _stscBadEncodeFmtS;
#define Stconst_stscBadEncodeFmtS System::LoadResourceString(&Stconst::_stscBadEncodeFmtS)
extern DELPHI_PACKAGE System::ResourceString _stscBadAttachmentS;
#define Stconst_stscBadAttachmentS System::LoadResourceString(&Stconst::_stscBadAttachmentS)
extern DELPHI_PACKAGE System::ResourceString _stscDupeStringS;
#define Stconst_stscDupeStringS System::LoadResourceString(&Stconst::_stscDupeStringS)
extern DELPHI_PACKAGE System::ResourceString _stscInStreamS;
#define Stconst_stscInStreamS System::LoadResourceString(&Stconst::_stscInStreamS)
extern DELPHI_PACKAGE System::ResourceString _stscOutOfBoundsS;
#define Stconst_stscOutOfBoundsS System::LoadResourceString(&Stconst::_stscOutOfBoundsS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidLengthS;
#define Stconst_stscInvalidLengthS System::LoadResourceString(&Stconst::_stscInvalidLengthS)
extern DELPHI_PACKAGE System::ResourceString _stscNoInputFileS;
#define Stconst_stscNoInputFileS System::LoadResourceString(&Stconst::_stscNoInputFileS)
extern DELPHI_PACKAGE System::ResourceString _stscNoOutputFileS;
#define Stconst_stscNoOutputFileS System::LoadResourceString(&Stconst::_stscNoOutputFileS)
extern DELPHI_PACKAGE System::ResourceString _stscInFileErrorS;
#define Stconst_stscInFileErrorS System::LoadResourceString(&Stconst::_stscInFileErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscOutFileErrorS;
#define Stconst_stscOutFileErrorS System::LoadResourceString(&Stconst::_stscOutFileErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscNameS;
#define Stconst_stscNameS System::LoadResourceString(&Stconst::_stscNameS)
extern DELPHI_PACKAGE System::ResourceString _stscSizeS;
#define Stconst_stscSizeS System::LoadResourceString(&Stconst::_stscSizeS)
extern DELPHI_PACKAGE System::ResourceString _stscTypeS;
#define Stconst_stscTypeS System::LoadResourceString(&Stconst::_stscTypeS)
extern DELPHI_PACKAGE System::ResourceString _stscModifiedS;
#define Stconst_stscModifiedS System::LoadResourceString(&Stconst::_stscModifiedS)
extern DELPHI_PACKAGE System::ResourceString _stscAttributesS;
#define Stconst_stscAttributesS System::LoadResourceString(&Stconst::_stscAttributesS)
extern DELPHI_PACKAGE System::ResourceString _stscFileFolderS;
#define Stconst_stscFileFolderS System::LoadResourceString(&Stconst::_stscFileFolderS)
extern DELPHI_PACKAGE System::ResourceString _stscSystemFolderS;
#define Stconst_stscSystemFolderS System::LoadResourceString(&Stconst::_stscSystemFolderS)
extern DELPHI_PACKAGE System::ResourceString _stscOriginalLocS;
#define Stconst_stscOriginalLocS System::LoadResourceString(&Stconst::_stscOriginalLocS)
extern DELPHI_PACKAGE System::ResourceString _stscDateDeletedS;
#define Stconst_stscDateDeletedS System::LoadResourceString(&Stconst::_stscDateDeletedS)
extern DELPHI_PACKAGE System::ResourceString _stscFileS;
#define Stconst_stscFileS System::LoadResourceString(&Stconst::_stscFileS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidFolderS;
#define Stconst_stscInvalidFolderS System::LoadResourceString(&Stconst::_stscInvalidFolderS)
extern DELPHI_PACKAGE System::ResourceString _stscFolderReadOnlyS;
#define Stconst_stscFolderReadOnlyS System::LoadResourceString(&Stconst::_stscFolderReadOnlyS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidSortDirS;
#define Stconst_stscInvalidSortDirS System::LoadResourceString(&Stconst::_stscInvalidSortDirS)
extern DELPHI_PACKAGE System::ResourceString _stscInsufficientDataS;
#define Stconst_stscInsufficientDataS System::LoadResourceString(&Stconst::_stscInsufficientDataS)
extern DELPHI_PACKAGE System::ResourceString _stscCreateFileFailedS;
#define Stconst_stscCreateFileFailedS System::LoadResourceString(&Stconst::_stscCreateFileFailedS)
extern DELPHI_PACKAGE System::ResourceString _stscFileMappingFailedS;
#define Stconst_stscFileMappingFailedS System::LoadResourceString(&Stconst::_stscFileMappingFailedS)
extern DELPHI_PACKAGE System::ResourceString _stscCreateViewFailedS;
#define Stconst_stscCreateViewFailedS System::LoadResourceString(&Stconst::_stscCreateViewFailedS)
extern DELPHI_PACKAGE System::ResourceString _stscBadOriginS;
#define Stconst_stscBadOriginS System::LoadResourceString(&Stconst::_stscBadOriginS)
extern DELPHI_PACKAGE System::ResourceString _stscGetSizeFailedS;
#define Stconst_stscGetSizeFailedS System::LoadResourceString(&Stconst::_stscGetSizeFailedS)
extern DELPHI_PACKAGE System::ResourceString _stscNilStreamS;
#define Stconst_stscNilStreamS System::LoadResourceString(&Stconst::_stscNilStreamS)
extern DELPHI_PACKAGE System::ResourceString _stscNoSeekForReadS;
#define Stconst_stscNoSeekForReadS System::LoadResourceString(&Stconst::_stscNoSeekForReadS)
extern DELPHI_PACKAGE System::ResourceString _stscNoSeekForWriteS;
#define Stconst_stscNoSeekForWriteS System::LoadResourceString(&Stconst::_stscNoSeekForWriteS)
extern DELPHI_PACKAGE System::ResourceString _stscCannotWriteS;
#define Stconst_stscCannotWriteS System::LoadResourceString(&Stconst::_stscCannotWriteS)
extern DELPHI_PACKAGE System::ResourceString _stscBadTerminatorS;
#define Stconst_stscBadTerminatorS System::LoadResourceString(&Stconst::_stscBadTerminatorS)
extern DELPHI_PACKAGE System::ResourceString _stscBadLineLengthS;
#define Stconst_stscBadLineLengthS System::LoadResourceString(&Stconst::_stscBadLineLengthS)
extern DELPHI_PACKAGE System::ResourceString _stscCannotSetSizeS;
#define Stconst_stscCannotSetSizeS System::LoadResourceString(&Stconst::_stscCannotSetSizeS)
extern DELPHI_PACKAGE System::ResourceString _stscUnknownErrorS;
#define Stconst_stscUnknownErrorS System::LoadResourceString(&Stconst::_stscUnknownErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscExpandingClassS;
#define Stconst_stscExpandingClassS System::LoadResourceString(&Stconst::_stscExpandingClassS)
extern DELPHI_PACKAGE System::ResourceString _stscAlternationFollowsClosureS;
#define Stconst_stscAlternationFollowsClosureS System::LoadResourceString(&Stconst::_stscAlternationFollowsClosureS)
extern DELPHI_PACKAGE System::ResourceString _stscUnbalancedParensS;
#define Stconst_stscUnbalancedParensS System::LoadResourceString(&Stconst::_stscUnbalancedParensS)
extern DELPHI_PACKAGE System::ResourceString _stscFollowingClosureS;
#define Stconst_stscFollowingClosureS System::LoadResourceString(&Stconst::_stscFollowingClosureS)
extern DELPHI_PACKAGE System::ResourceString _stscPatternErrorS;
#define Stconst_stscPatternErrorS System::LoadResourceString(&Stconst::_stscPatternErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscUnbalancedTagS;
#define Stconst_stscUnbalancedTagS System::LoadResourceString(&Stconst::_stscUnbalancedTagS)
extern DELPHI_PACKAGE System::ResourceString _stscNoPatternsS;
#define Stconst_stscNoPatternsS System::LoadResourceString(&Stconst::_stscNoPatternsS)
extern DELPHI_PACKAGE System::ResourceString _stscPatternTooLargeS;
#define Stconst_stscPatternTooLargeS System::LoadResourceString(&Stconst::_stscPatternTooLargeS)
extern DELPHI_PACKAGE System::ResourceString _stscStreamsNilS;
#define Stconst_stscStreamsNilS System::LoadResourceString(&Stconst::_stscStreamsNilS)
extern DELPHI_PACKAGE System::ResourceString _stscInTextStreamErrorS;
#define Stconst_stscInTextStreamErrorS System::LoadResourceString(&Stconst::_stscInTextStreamErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscOutTextStreamErrorS;
#define Stconst_stscOutTextStreamErrorS System::LoadResourceString(&Stconst::_stscOutTextStreamErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscClosureMaybeEmptyS;
#define Stconst_stscClosureMaybeEmptyS System::LoadResourceString(&Stconst::_stscClosureMaybeEmptyS)
extern DELPHI_PACKAGE System::ResourceString _stscOutFileDeleteS;
#define Stconst_stscOutFileDeleteS System::LoadResourceString(&Stconst::_stscOutFileDeleteS)
extern DELPHI_PACKAGE System::ResourceString _stscInFileNotFoundS;
#define Stconst_stscInFileNotFoundS System::LoadResourceString(&Stconst::_stscInFileNotFoundS)
extern DELPHI_PACKAGE System::ResourceString _stscREInFileErrorS;
#define Stconst_stscREInFileErrorS System::LoadResourceString(&Stconst::_stscREInFileErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscOutFileCreateS;
#define Stconst_stscOutFileCreateS System::LoadResourceString(&Stconst::_stscOutFileCreateS)
extern DELPHI_PACKAGE System::ResourceString _stscNetNoManualCreateS;
#define Stconst_stscNetNoManualCreateS System::LoadResourceString(&Stconst::_stscNetNoManualCreateS)
extern DELPHI_PACKAGE System::ResourceString _stscNetUnknownErrorS;
#define Stconst_stscNetUnknownErrorS System::LoadResourceString(&Stconst::_stscNetUnknownErrorS)
extern DELPHI_PACKAGE System::ResourceString _stscNetGroupNotSpecifiedS;
#define Stconst_stscNetGroupNotSpecifiedS System::LoadResourceString(&Stconst::_stscNetGroupNotSpecifiedS)
extern DELPHI_PACKAGE System::ResourceString _stscNetDateSpecifiedOutOfRangeS;
#define Stconst_stscNetDateSpecifiedOutOfRangeS System::LoadResourceString(&Stconst::_stscNetDateSpecifiedOutOfRangeS)
extern DELPHI_PACKAGE System::ResourceString _stscNetInvalidParameterS;
#define Stconst_stscNetInvalidParameterS System::LoadResourceString(&Stconst::_stscNetInvalidParameterS)
extern DELPHI_PACKAGE System::ResourceString _stscNetInvalidItemTypeS;
#define Stconst_stscNetInvalidItemTypeS System::LoadResourceString(&Stconst::_stscNetInvalidItemTypeS)
extern DELPHI_PACKAGE System::ResourceString _stscStatBadCountS;
#define Stconst_stscStatBadCountS System::LoadResourceString(&Stconst::_stscStatBadCountS)
extern DELPHI_PACKAGE System::ResourceString _stscStatBadParamS;
#define Stconst_stscStatBadParamS System::LoadResourceString(&Stconst::_stscStatBadParamS)
extern DELPHI_PACKAGE System::ResourceString _stscStatBadDataS;
#define Stconst_stscStatBadDataS System::LoadResourceString(&Stconst::_stscStatBadDataS)
extern DELPHI_PACKAGE System::ResourceString _stscStatNoConvergeS;
#define Stconst_stscStatNoConvergeS System::LoadResourceString(&Stconst::_stscStatNoConvergeS)
extern DELPHI_PACKAGE System::ResourceString _stscWordDelimitersS;
#define Stconst_stscWordDelimitersS System::LoadResourceString(&Stconst::_stscWordDelimitersS)
extern DELPHI_PACKAGE System::ResourceString _stscInvalidSLEntryS;
#define Stconst_stscInvalidSLEntryS System::LoadResourceString(&Stconst::_stscInvalidSLEntryS)
extern DELPHI_PACKAGE System::ResourceString _stscBadStreamS;
#define Stconst_stscBadStreamS System::LoadResourceString(&Stconst::_stscBadStreamS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyIdxOutOfRangeS;
#define Stconst_stscMoneyIdxOutOfRangeS System::LoadResourceString(&Stconst::_stscMoneyIdxOutOfRangeS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyNilResultS;
#define Stconst_stscMoneyNilResultS System::LoadResourceString(&Stconst::_stscMoneyNilResultS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyNilParameterS;
#define Stconst_stscMoneyNilParameterS System::LoadResourceString(&Stconst::_stscMoneyNilParameterS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyCurrenciesNotMatchS;
#define Stconst_stscMoneyCurrenciesNotMatchS System::LoadResourceString(&Stconst::_stscMoneyCurrenciesNotMatchS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyNoExchangeRatesAvailS;
#define Stconst_stscMoneyNoExchangeRatesAvailS System::LoadResourceString(&Stconst::_stscMoneyNoExchangeRatesAvailS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyInvalidExchangeParamsS;
#define Stconst_stscMoneyInvalidExchangeParamsS System::LoadResourceString(&Stconst::_stscMoneyInvalidExchangeParamsS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyInvalidTriangleExchangeS;
#define Stconst_stscMoneyInvalidTriangleExchangeS System::LoadResourceString(&Stconst::_stscMoneyInvalidTriangleExchangeS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyNoSuchExchangeS;
#define Stconst_stscMoneyNoSuchExchangeS System::LoadResourceString(&Stconst::_stscMoneyNoSuchExchangeS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyMissingIntermediateRateS;
#define Stconst_stscMoneyMissingIntermediateRateS System::LoadResourceString(&Stconst::_stscMoneyMissingIntermediateRateS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyInvalidExchRateS;
#define Stconst_stscMoneyInvalidExchRateS System::LoadResourceString(&Stconst::_stscMoneyInvalidExchRateS)
extern DELPHI_PACKAGE System::ResourceString _stscMoneyTriExchUsesTriExchS;
#define Stconst_stscMoneyTriExchUsesTriExchS System::LoadResourceString(&Stconst::_stscMoneyTriExchUsesTriExchS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathRoundPlacesS;
#define Stconst_stscDecMathRoundPlacesS System::LoadResourceString(&Stconst::_stscDecMathRoundPlacesS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathAsIntOverflowS;
#define Stconst_stscDecMathAsIntOverflowS System::LoadResourceString(&Stconst::_stscDecMathAsIntOverflowS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathConversionS;
#define Stconst_stscDecMathConversionS System::LoadResourceString(&Stconst::_stscDecMathConversionS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathDivByZeroS;
#define Stconst_stscDecMathDivByZeroS System::LoadResourceString(&Stconst::_stscDecMathDivByZeroS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathNegExpS;
#define Stconst_stscDecMathNegExpS System::LoadResourceString(&Stconst::_stscDecMathNegExpS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathMultOverflowS;
#define Stconst_stscDecMathMultOverflowS System::LoadResourceString(&Stconst::_stscDecMathMultOverflowS)
extern DELPHI_PACKAGE System::ResourceString _stscDecMathDivOverflowS;
#define Stconst_stscDecMathDivOverflowS System::LoadResourceString(&Stconst::_stscDecMathDivOverflowS)
extern DELPHI_PACKAGE System::ResourceString _stscTxtDatNoSuchFieldS;
#define Stconst_stscTxtDatNoSuchFieldS System::LoadResourceString(&Stconst::_stscTxtDatNoSuchFieldS)
extern DELPHI_PACKAGE System::ResourceString _stscTxtDatUniqueNameRequiredS;
#define Stconst_stscTxtDatUniqueNameRequiredS System::LoadResourceString(&Stconst::_stscTxtDatUniqueNameRequiredS)
extern DELPHI_PACKAGE System::ResourceString _stscTxtDatUnhandledVariantS;
#define Stconst_stscTxtDatUnhandledVariantS System::LoadResourceString(&Stconst::_stscTxtDatUnhandledVariantS)
extern DELPHI_PACKAGE System::ResourceString _stscTxtDatInvalidSchemaS;
#define Stconst_stscTxtDatInvalidSchemaS System::LoadResourceString(&Stconst::_stscTxtDatInvalidSchemaS)
extern DELPHI_PACKAGE System::ResourceString _stscTxtDatRecordSetOpenS;
#define Stconst_stscTxtDatRecordSetOpenS System::LoadResourceString(&Stconst::_stscTxtDatRecordSetOpenS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGDegFreedomS;
#define Stconst_stscPRNGDegFreedomS System::LoadResourceString(&Stconst::_stscPRNGDegFreedomS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGBetaShapeS;
#define Stconst_stscPRNGBetaShapeS System::LoadResourceString(&Stconst::_stscPRNGBetaShapeS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGMeanS;
#define Stconst_stscPRNGMeanS System::LoadResourceString(&Stconst::_stscPRNGMeanS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGGammaShapeS;
#define Stconst_stscPRNGGammaShapeS System::LoadResourceString(&Stconst::_stscPRNGGammaShapeS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGGammaScaleS;
#define Stconst_stscPRNGGammaScaleS System::LoadResourceString(&Stconst::_stscPRNGGammaScaleS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGStdDevS;
#define Stconst_stscPRNGStdDevS System::LoadResourceString(&Stconst::_stscPRNGStdDevS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGWeibullShapeS;
#define Stconst_stscPRNGWeibullShapeS System::LoadResourceString(&Stconst::_stscPRNGWeibullShapeS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGWeibullScaleS;
#define Stconst_stscPRNGWeibullScaleS System::LoadResourceString(&Stconst::_stscPRNGWeibullScaleS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGLimitS;
#define Stconst_stscPRNGLimitS System::LoadResourceString(&Stconst::_stscPRNGLimitS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGUpperLimitS;
#define Stconst_stscPRNGUpperLimitS System::LoadResourceString(&Stconst::_stscPRNGUpperLimitS)
extern DELPHI_PACKAGE System::ResourceString _stscPRNGErlangOrderS;
#define Stconst_stscPRNGErlangOrderS System::LoadResourceString(&Stconst::_stscPRNGErlangOrderS)
extern DELPHI_PACKAGE Stconst__1 SysToolsStrArray;
extern DELPHI_PACKAGE System::UnicodeString __fastcall SysToolsStr(int Index);
}	/* namespace Stconst */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STCONST)
using namespace Stconst;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StconstHPP
