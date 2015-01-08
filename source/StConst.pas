// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StConst.pas 4.04                            *}
{*********************************************************}
{* SysTools: Base unit for SysTools                      *}
{*********************************************************}

{$I StDefine.inc}

unit StConst;
  {-Resource constants for SysTools}

interface

uses
  SysUtils;

const
  StVersionStr = '4.04';

const
  {string table constants for STREGINI}
  stscFalseString          = 0;
  stscTrueString           = 1;
  stscNoFileKey            = 2;
  stscInvalidPKey          = 3;
  stscNoWin32S             = 4;
  stscCreateKeyFail        = 5;
  stscOpenKeyFail          = 6;
  stscIniWriteFail         = 7;
  stscRegWriteFail         = 8;
  stscNoKeyName            = 9;
  stscQueryKeyFail         = 10;
  stscEnumKeyFail          = 11;
  stscEnumValueFail        = 12;
  stscIniDeleteFail        = 13;
  stscKeyHasSubKeys        = 14;
  stscDeleteKeyFail        = 15;
  stscIniDelValueFail      = 16;
  stscRegDelValueFail      = 17;
  stscOutputFileExists     = 18;
  stscFileHasExtension     = 19;
  stscSaveKeyFail          = 20;
  stscNo16bitSupport       = 21;
  stscCantFindInputFile    = 22;
  stscLoadKeyFail          = 23;
  stscUnloadKeyFail        = 24;
  stscNotWinNTPlatform     = 25;
  stscBadOptionsKeyCombo   = 26;
  stscRestoreKeyFail       = 27;
  stscReplaceKeyFail       = 28;
  stscNoIniFileSupport     = 29;
  stscRemoteKeyIsOpen      = 30;
  stscConnectRemoteKeyFail = 31;
  stscCloseRemoteKeyFail   = 32;
  stscFlushKeyFail         = 33;
  stscBufferDataSizesDif   = 34;
  stscKeyIsEmptyNotExists  = 35;
  stscGetSecurityFail      = 36;
  stscSetSecurityFail      = 37;
  stscByteArrayTooLarge    = 38;
  stscQueryValueFail       = 39;
  stscNoValueNameSpecified = 40;

  {string table constants for container classes}
  stscNoCompare        = 51; {Compare property must be set}
  stscBadType          = 52; {an incompatible class is passed to a method}
  stscBadSize          = 53; {bad size for TStDictionary, TStBits, TStCollection}
  stscDupNode          = 54; {attempt to add duplicate node to TStTree}
  stscBadIndex         = 55; {bad index passed to TStBits or large array}
  stscBadWinMode       = 56; {requires enhanced mode operation}
  stscUnknownClass     = 57; {container class name not registered}
  stscUnknownNodeClass = 58; {container node class name not registered}
  stscNoStoreData      = 59; {container has no store data routine}
  stscNoLoadData       = 60; {container has no load data routine}
  stscWrongClass       = 61; {container class and streamed class not equal}
  stscWrongNodeClass   = 62; {container node class and streamed class not equal}
  stscBadCompare       = 63; {invalid compare function or unable to assign now}
  stscTooManyCols      = 64; {assign a matrix with >1 col to array}
  stscBadColCount      = 65; {assign a matrix with wrong col count to virtual matrix}
  stscBadElSize        = 66; {assign a matrix with wrong elem size to virtual matrix}
  stscBadDups          = 67; {setting Dups to False in a non-empty sorted collection}

  {string table constants for sorting unit}
  stscTooManyFiles = 71; {too many merge files in TStSorter}
  stscFileCreate   = 72; {error creating file in TStSorter}
  stscFileOpen     = 73; {error opening file in TStSorter}
  stscFileWrite    = 74; {error writing file in TStSorter}
  stscFileRead     = 75; {error reading file in TStSorter}
  stscBadState     = 76; {TStSorter in wrong state}

  {string table constants for Bcd unit}
  stscBcdBadFormat   = 81; {bad BCD format}
  stscBcdOverflow    = 82; {BCD larger than 10**64}
  stscBcdDivByZero   = 83; {BCD divide by zero}
  stscBcdBadInput    = 84; {BCD negative input to sqrt, ln, or power}
  stscBcdBufOverflow = 85; {buffer overflow in FormatBcd}

  stscNoVerInfo      = 100; {no version info in file}
  stscVerInfoFail    = 101; {error reading version info}

(*
  {shell string constants}
  stscShellVersionError   = 110; {not available in this version of Shell32.dll}
  stscShellFileOpSrcError = 111; {no source files specified}
  stscShellFileOpDstError = 112; {no destination files specified}
  stscShellFileOpMapError = 113; {mapping incomplete}
  stscShellFormatError    = 114; {format error}
  stscShellFormatCancel   = 115; {format cancelled}
  stscShellFormatNoFormat = 116; {drive cannot be formatted}
  stscShellFormatBadDrive = 117; {not removable drive}
  stscTrayIconInvalidOS   = 118; {bad OS (NT 3.51)}
  stscTrayIconCantAdd     = 119; {can't add icon to the tray}
  stscTrayIconCantDelete  = 120; {can't delete icon from the tray}
  stscTrayIconError       = 121; {general tray icon error}
  stscBadDropTarget       = 122; {drop target is not TWinControl}
  stscCOMInitFailed       = 123; {COInitialize failed}
  stscNoPathSpecified     = 124; {No destination path for shortcut}
  stscIShellLinkError     = 125; {Error creating IShellLink}
  stscNotShortcut         = 126; {File is not a shortcut}
  stscTrayIconClose       = 127; {Close}
  stscTrayIconRestore     = 128; {Restore}
  stscInvalidTargetFile   = 130; {Shortcut target file not found}
  stscShellFileOpDelete   = 131; {Can't use file mappings with delete op}
  stscShellFileNotFound   = 132; {One or more source files is missing}
  stscTrayIconDuplicate   = 133; {Cant' have more than one tray icon}
  stscBadVerInfoKey       = 134; {User-defined key not found in ver info}
  stscImageListInvalid    = 135; {No image list assigned.}
*)
  stscBadVerInfoKey       = 134; {User-defined key not found in ver info}

  {barcode errors}
  stscInvalidUPCACodeLen    = 140;
  stscInvalidCharacter      = 141;
  stscInvalidCheckCharacter = 142;
  stscInvalidUPCECodeLen    = 143;
  stscInvalidEAN8CodeLen    = 144;
  stscInvalidEAN13CodeLen   = 145;
  stscInvalidSupCodeLen     = 146;

  {stexpr errors}
  stscExprEmpty      = 150;  {empty expression}
  stscExprBadNum     = 151;  {error in floating point number}
  stscExprBadChar    = 152;  {unknown character}
  stscExprOpndExp    = 153;  {expected function, number, sign, or (}
  stscExprNumeric    = 154;  {numeric error}
  stscExprBadExp     = 155;  {invalid expression}
  stscExprOpndOvfl   = 156;  {operand stack overflow}
  stscExprUnkFunc    = 157;  {unknown function identifier}
  stscExprLParExp    = 158;  {left parenthesis expected}
  stscExprRParExp    = 159;  {right parenthesis expected}
  stscExprCommExp    = 160;  {list separator (comma) expected}
  stscExprDupIdent   = 161;  {duplicate identifier}

  {ststat errors}
  stscStatBadCount   = 170;  {unequal or bad counts of array elements}
  stscStatBadParam   = 171;  {invalid parameter}
  stscStatBadData    = 172;  {invalid data point in array}
  stscStatNoConverge = 173;  {no convergence in numerical routine}

  {stfin errors}
  stscFinBadArg       = 180;
  stscFinNoConverge   = 181;

  {stmime errors}
  stscBadEncodeFmt    = 190;
  stscBadAttachment   = 191;
  stscDupeString      = 192;
  stscInStream        = 193;

  {ststring errors}
  stscOutOfBounds     = 200;  {Index out of string bounds}


  {stBarPN errors}
  stscInvalidLength   = 210;

  {StHTML errors}
  stscNoInputFile     = 215;
  stscNoOutputFile    = 216;
  stscInFileError     = 217;
  stscOutFileError    = 218;
  stscWordDelimiters  = 219;
  stscInvalidSLEntry  = 220;
  stscBadStream       = 221;

  {StShlCtl constansts}
  stscName            = 230;
  stscSize            = 231;
  stscType            = 232;
  stscModified        = 233;
  stscAttributes      = 234;
  stscFileFolder      = 235;
  stscSystemFolder    = 236;
  stscOriginalLoc     = 237;
  stscDateDeleted     = 238;
  stscFile            = 239;
  stscInvalidFolder   = 240;
  stscFolderReadOnly  = 241;

  {StSpawnApplication errors}
  stscInsufficientData= 250;

  {StMemoryMappedFile errors}
  stscCreateFileFailed = 260;
  stscFileMappingFailed= 261;
  stscCreateViewFailed = 262;
  stscBadOrigin        = 263;
  stscGetSizeFailed    = 264;

  {buffered stream errors}
  stscNilStream        = 270;
  stscNoSeekForRead    = 271;
  stscNoSeekForWrite   = 272;
  stscCannotWrite      = 273;
  stscBadTerminator    = 274;
  stscBadLineLength    = 275;
  stscCannotSetSize    = 276;

  {RegEx errors}
  stscUnknownError              = 290;
  stscExpandingClass            = 291;
  stscAlternationFollowsClosure = 292;
  stscUnbalancedParens          = 293;
  stscFollowingClosure          = 294;
  stscPatternError              = 295;
  stscUnbalancedTag             = 296;
  stscNoPatterns                = 297;
  stscPatternTooLarge           = 298;
  stscStreamsNil                = 299;
  stscInTextStreamError         = 300;
  stscOutTextStreamError        = 301;
  stscClosureMaybeEmpty         = 302;
  stscInFileNotFound            = 303;
  stscREInFileError             = 304;
  stscOutFileDelete             = 305;
  stscOutFileCreate             = 306;


  {StNet errors 320-339}
  stscNetNoManualCreate          = 320;
  stscNetUnknownError            = 321;
  stscNetGroupNotSpecified       = 322;
  stscNetDateSpecifiedOutOfRange = 323;
  stscNetInvalidParameter        = 324;
  stscNetInvalidItemType         = 325;

  {StNetConnection errors 330-334}

  {StNetPerformance errors 335-339}

  {StNetMessage errors 340-344}

  {StMoney errors 400-429}
//  stscMoneyIdxOutOfRange           = 400; //'Index out of range (%s)'
  stscMoneyNilResult               = 401; //'Nil result parameter'
  stscMoneyNilParameter            = 402; //'Nil parameter to operation'
  stscMoneyCurrenciesNotMatch      = 403; //'Currencies do not match'
  stscMoneyNoExchangeRatesAvail    = 410; //'No Exchange Rates Available'
  stscMoneyInvalidExchangeParams   = 411; //'Invalid exchange parameters'
  stscMoneyInvalidTriangleExchange = 412; //'Invalid triangle exchange'
  stscMoneyNoSuchExchange          = 413; //'No exchange rate for %s->%s available'
  stscMoneyMissingIntermediateRate = 414; //''Intermediate exchange rate for %s->%s missing'
  stscMoneyInvalidExchRate         = 415; //'Exchange rate is missing a property value'
  stscMoneyTriExchUsesTriExch      = 415; //'Triangular exchange rate is using triangular exchange rates'

  stscDecMathRoundPlaces           = 423; //'Decimal math: the number of decimal places to round to must be betwen 0 and 16'
  stscDecMathAsIntOverflow         = 424; //'Decimal math: current value overflows an integer'
  stscDecMathConversion            = 425; //'Decimal math: string value not a valid number';
  stscDecMathDivByZero             = 426; //'Decimal math: division by zero attempted'
  stscDecMathNegExp                = 427; //'Decimal math: cannot raise to a negative power';
  stscDecMathMultOverflow          = 428; //'Decimal math: result overflowed during multiplication'
  stscDecMathDivOverflow           = 429; //'Decimal math: result overflowed during division'

  { Text Data Set, Merge, and Export errors }
  stscTxtDatNoSuchField            = 430; //'No such field'
  stscTxtDatUniqueNameRequired     = 431; //'Field name must be unique'
  stscTxtDatUnhandledVariant       = 432; //'Unhandled Variant Type'
  stscTxtDatInvalidSchema          = 433; //'Invalid Schema'
  stscTxtDatRecordSetOpen          = 434; //'Cannot perform this operation on an open record set'

  {PRNG errors 460-479}
  stscPRNGDegFreedom               = 460; //'StRandom: the number of degrees of freedom should be greater than zero'
  stscPRNGBetaShape                = 461; //'StRandom: the Beta distribution shape values should be greater than zero'
  stscPRNGMean                     = 462; //'StRandom: the mean must be greater than zero'
  stscPRNGGammaShape               = 463; //'StRandom: the Gamma distribution shape must be greater than zero'
  stscPRNGGammaScale               = 464; //'StRandom: the Gamma distribution scale must be greater than zero'
  stscPRNGStdDev                   = 465; //'StRandom: the standard deviation must be greater than zero'
  stscPRNGWeibullShape             = 466; //'StRandom: the Weibull distribution shape must be greater than zero'
  stscPRNGWeibullScale             = 467; //'StRandom: the Weibull distribution scale must be greater than zero'
  stscPRNGLimit                    = 468; //'StRandom: the limit must be greater than zero'
  stscPRNGUpperLimit               = 469; //'StRandom: the upper limit must be greater than the lower limit'
  stscPRNGErlangOrder              = 470; //'StRandom: the Erlang distribution's order must be greater than zero'

resourcestring
  stscSysStringListFull          = 'String list is full';
  stscSysBadStartDir             = 'Invalid starting directory';

  stscFalseStringS               = 'FALSE';
  stscTrueStringS                = 'TRUE';
  stscNoFileKeyS                 = 'No Ini File or Primary Key specified';
  stscInvalidPKeyS               = 'Invalid primary key specified';
  stscNoWin32SS                  = 'RegIni Class not supported under Win32s';
  stscCreateKeyFailS             = 'Failed to create key\nError Code: %d';
  stscOpenKeyFailS               = 'Failed to open key\nError Code: %d';
  stscIniWriteFailS              = 'Failed to write value to INI file';
  stscRegWriteFailS              = 'Failed to write value to Registry\nError Code: %d';
  stscNoKeyNameS                 = 'No key name specified';
  stscQueryKeyFailS              = 'Unable to query specified key\nError Code: %d';
  stscEnumKeyFailS               = 'Unable to enumerate key\nError Code: %d';
  stscEnumValueFailS             = 'Unable to enumerate value\nError Code: %d';
  stscIniDeleteFailS             = 'Unable to delete section from INI file';
  stscKeyHasSubKeysS             = 'Can not delete key which has subkeys (%d)';
  stscDeleteKeyFailS             = 'Unable to delete key\nError Code: %d';
  stscIniDelValueFailS           = 'Unable to delete value from INI file';
  stscRegDelValueFailS           = 'Unable to delete value from key\nError Code: %d';
  stscOutputFileExistsS          = 'Output file exists';
  stscFileHasExtensionS          = 'File name can not have an extension';
  stscSaveKeyFailS               = 'Unable to save key\nError Code: %d';
  stscNo16bitSupportS            = 'Function not supported in 16-bit applications';
  stscCantFindInputFileS         = 'Can not find input file';
  stscLoadKeyFailS               = 'Unable to load key\nError Code: %d';
  stscUnloadKeyFailS             = 'Unable to unload key\nErrorCode: %d';
  stscNotWinNTPlatformS          = 'Function not supported on this platform';
  stscBadOptionsKeyComboS        = 'Selection options incompatible\nwith specified primary key';
  stscRestoreKeyFailS            = 'Unable to restore key\nError Code: %d';
  stscReplaceKeyFailS            = 'Unable to replace key\nError Code: %d';
  stscNoIniFileSupportS          = 'Function not supported on INI files';
  stscRemoteKeyIsOpenS           = 'Remote key already open';
  stscConnectRemoteKeyFailS      = 'Unable to connect to remote registry key\nError Code: %d';
  stscCloseRemoteKeyFailS        = 'Unable to close remote registry key';
  stscFlushKeyFailS              = 'Unable to flush specified key';
  stscBufferDataSizesDifS        = 'Buffer size differs from data size\nBuffer: %d   Data: %d';
  stscKeyIsEmptyNotExistsS       = 'Specified Key is empty or does not exist';
  stscGetSecurityFailS           = 'Failed to Get Security Information\nError Code: %d';
  stscSetSecurityFailS           = 'Failed to Set Security Information\nError Code: %d';
  stscByteArrayTooLargeS         = 'Size of byte array exceeds limit';
  stscQueryValueFailS            = 'Unable to query value in key';
  stscNoValueNameSpecifiedS      = 'No Value Name specified';

  stscNoCompareS                 = 'Compare property must be set';
  stscBadTypeS                   = 'An incompatible class is passed to a method';
  stscBadSizeS                   = 'Bad size parameter';
  stscDupNodeS                   = 'Attempt to add duplicate node to TStTree';
  stscBadIndexS                  = 'Index is out of range';
  stscBadWinModeS                = 'Requires enhanced mode operation for Windows 3.1x';
  stscUnknownClassS              = 'Container class name %s read from stream is unregistered';
  stscUnknownNodeClassS          = 'Node class name %s read from stream is unregistered';
  stscNoStoreDataS               = 'Container''s StoreData property is unassigned';
  stscNoLoadDataS                = 'Container''s LoadData property is unassigned';
  stscWrongClassS                = 'Class name on stream differs from object''s class';
  stscWrongNodeClassS            = 'Node class name on stream differs from object''s node class';
  stscBadCompareS                = 'Unable to assign this compare function now';
  stscTooManyColsS               = 'Cannot assign a matrix with more than 1 column to an array';
  stscBadColCountS               = 'Can only assign a matrix to a virtual matrix if column counts are equal';
  stscBadElSizeS                 = 'Can only assign a matrix to a virtual matrix if element sizes are equal';
  stscBadDupsS                   = 'Can only set Duplicates to False in an empty sorted collection';

  stscTooManyFilesS              = 'Too many merge files in TStSorter';
  stscFileCreateS                = 'Error creating file';
  stscFileOpenS                  = 'Error opening file';
  stscFileWriteS                 = 'Error writing file (bytes written <> bytes requested)';
  stscFileReadS                  = 'Error reading file (bytes read <> bytes requested)';
  stscBadStateS                  = 'TStSorter in wrong state';

  stscBcdBadFormatS              = 'Bad BCD format';
  stscBcdOverflowS               = 'BCD larger than 10**64';
  stscBcdDivByZeroS              = 'BCD divide by zero';
  stscBcdBadInputS               = 'BCD negative input to sqrt, ln, or power';
  stscBcdBufOverflowS            = 'Buffer overflow in FormatBcd';

  stscNoVerInfoS                 = 'File does not contain version info';
  stscVerInfoFailS               = 'Unable to read version info';

(*
  stscShellVersionErrorS         = 'Operation not supported in this version of the shell';
  stscShellFileOpSrcErrorS       = 'No source files specified';
  stscShellFileOpDstErrorS       = 'No destination files specified';
  stscShellFileOpMapErrorS       = 'File mapping incomplete';
  stscShellFormatErrorS          = 'Format failed';
  stscShellFormatCancelS         = 'Format cancelled';
  stscShellFormatNoFormatS       = 'Drive cannot be formatted';
  stscShellFormatBadDriveS       = 'Invalid drive. Drive is not removable';
  stscTrayIconInvalidOSS         = 'Operating system does not support tray icons';
  stscTrayIconCantAddS           = 'Error adding tray icon';
  stscTrayIconCantDeleteS        = 'Error removing tray icon';
  stscTrayIconErrorS             = 'Tray icon error';
  stscBadDropTargetS             = 'Drop target must be a TWinControl descendant';
  stscCOMInitFailedS             = 'Cannot initialize COM';
  stscNoPathSpecifiedS           = 'Destination directory not specified';
  stscIShellLinkErrorS           = 'Error creating IShellLink';
  stscNotShortcutS               = 'File is not a shortcut';
  stscTrayIconCloseS             = '&Close';
  stscTrayIconRestoreS           = '&Restore';
  stscInvalidTargetFileS         = 'Cannot create shortcut. Target file does not exist';
  stscShellFileOpDeleteS         = 'Cannot use file mappings in a delete operation';
  stscShellFileNotFoundS         = 'Source file error, file not found';
  stscTrayIconDuplicateS         = 'Cannot have more than one StTrayIcon per application';
  stscBadVerInfoKeyS             = 'The specified key cannnot be found in version info';
  stscImageListInvalidS          = 'ImageList is not assigned';
*)
  stscBadVerInfoKeyS             = 'The specified key cannnot be found in version info';

  stscInvalidUPCACodeLenS        = 'Invalid code length (must be 11 or 12)';
  stscInvalidCharacterS          = 'Invalid character';
  stscInvalidCheckCharacterS     = 'Invalid check character';
  stscInvalidUPCECodeLenS        = 'Invalid code length (must be 6)';
  stscInvalidEAN8CodeLenS        = 'Invalid code length (must be 7 or 8)';
  stscInvalidEAN13CodeLenS       = 'Invalid code length (must be 12 or 13)';
  stscInvalidSupCodeLenS         = 'Invalid supplemental code length (must be 2 or 5)';

  stscFinBadArgS                 = 'Invalid argument to financial function';
  stscFinNoConvergeS             = 'Function does not converge';

  stscExprEmptyS                 = 'Empty expression';
  stscExprBadNumS                = 'Error in floating point number';
  stscExprBadCharS               = 'Unknown character';
  stscExprOpndExpS               = 'Expected function, number, sign, or (';
  stscExprNumericS               = 'Numeric error';
  stscExprBadExpS                = 'Invalid expression';
  stscExprOpndOvflS              = 'Operand stack overflow';
  stscExprUnkFuncS               = 'Unknown function identifier';
  stscExprLParExpS               = 'Left parenthesis expected';
  stscExprRParExpS               = 'Right parenthesis expected';
  stscExprCommExpS               = 'List separator expected';
  stscExprDupIdentS              = 'Duplicate identifier';

  stscBadEncodeFmtS              = 'Encoding Format Not Supported';
  stscBadAttachmentS             = 'Attachment Doesn''t Exist';
  stscDupeStringS                = 'Duplicate string';
  stscInStreamS                  = 'Error in input stream';

  stscOutOfBoundsS               = 'Index out of string bounds';

  stscInvalidLengthS             = 'POSTNET code must be 5, 9 or 11 digits';


  stscNoInputFileS               = 'Input file not specified';
  stscNoOutputFileS              = 'Output file not specified';
  stscInFileErrorS               = 'Error opening input file';
  stscOutFileErrorS              = 'Error creating output file';


  stscNameS                      = 'Name';
  stscSizeS                      = 'Size';
  stscTypeS                      = 'Type';
  stscModifiedS                  = 'Modified';
  stscAttributesS                = 'Attributes';
  stscFileFolderS                = 'File Folder';
  stscSystemFolderS              = 'System Folder';
  stscOriginalLocS               = 'Original Location';
  stscDateDeletedS               = 'Date Deleted';
  stscFileS                      = 'File';
  stscInvalidFolderS             = 'Invalid folder';
  stscFolderReadOnlyS            = 'Cannot create folder: Parent folder is read-only';
  stscInvalidSortDirS            = 'Invalid sort direction';

  stscInsufficientDataS          = 'FileName cannot be empty when RunParameters is specified';

  stscCreateFileFailedS          = 'CreateFile failed';
  stscFileMappingFailedS         = 'CreateFileMapping failed';
  stscCreateViewFailedS          = 'MapViewOfFile failed';
  stscBadOriginS                 = 'Bad origin parameter for call to Seek';
  stscGetSizeFailedS             = 'Error reading size of existing file';

  stscNilStreamS                 = 'Buffered/text stream: Attempted to read, write, or seek and underlying stream is nil';
  stscNoSeekForReadS             = 'Buffered/text stream: Could not seek to the correct position in the underlying stream (for read request)';
  stscNoSeekForWriteS            = 'Buffered/text stream: Could not seek to the correct position in the underlying stream (for write request)';
  stscCannotWriteS               = 'Buffered/text stream: Could not write the entire buffer to the underlying stream';
  stscBadTerminatorS             = 'Text stream: Case statement was used with a bad value of LineTerminator';
  stscBadLineLengthS             = 'Text stream: Length of a fixed line must be between 1 and 4096 bytes';
  stscCannotSetSizeS             = 'Buffered/text stream: Cannot set the size of the underlying stream (needs OnSetStreamSize event)';

  stscUnknownErrorS              = 'Unknown error creating a pattern token';
  stscExpandingClassS            = 'Problem in expanding character class';
  stscAlternationFollowsClosureS = 'Alternation cannot immediately follow a closure marker';
  stscUnbalancedParensS          = 'Unbalanced nesting parentheses';
  stscFollowingClosureS          = 'Closure cannot immediately follow BegOfLine, EndOfLine or another closure';
  stscPatternErrorS              = 'Error detected near end of pattern';
  stscUnbalancedTagS             = 'Unbalanced tag marker';
  stscNoPatternsS                = 'No Match, Replace, or SelAvoid Patterns defined';
  stscPatternTooLargeS           = 'Pattern exceeds MaxPatLen';
  stscStreamsNilS                = 'Input and/or output stream is not assigned';
  stscInTextStreamErrorS         = 'Error creating internal input text stream';
  stscOutTextStreamErrorS        = 'Error creating internal output text stream';
  stscClosureMaybeEmptyS         = 'A * or + operand could be empty';
  stscOutFileDeleteS             = 'Error deleting old previous file';
  stscInFileNotFoundS            = 'Input file not found';
  stscREInFileErrorS             = 'Error creating internal text stream';
  stscOutFileCreateS             = 'Error creating output file';


  stscNetNoManualCreateS          = 'Can''t manually create an object of this type';
  stscNetUnknownErrorS            = 'Unknown network error';
  stscNetGroupNotSpecifiedS       = 'Local or global group not specified';
  stscNetDateSpecifiedOutOfRangeS = 'Date specified out or range';
  stscNetInvalidParameterS        = 'Invalid parameter';
  stscNetInvalidItemTypeS         = 'Invalid item type for this method';

  stscStatBadCountS               = 'Unequal or bad counts of array elements';
  stscStatBadParamS               = 'Invalid parameter';
  stscStatBadDataS                = 'Invalid data point in array';
  stscStatNoConvergeS             = 'no convergence in numerical routine';

  stscWordDelimitersS  = '219';
  stscInvalidSLEntryS  = '220';
  stscBadStreamS       = '221';

  stscMoneyIdxOutOfRangeS           = 'Index out of range (%s)';
  stscMoneyNilResultS               = 'Nil result parameter';
  stscMoneyNilParameterS            = 'Nil parameter to operation';
  stscMoneyCurrenciesNotMatchS      = 'Currencies do not match';
  stscMoneyNoExchangeRatesAvailS    = 'No Exchange Rates Available';
  stscMoneyInvalidExchangeParamsS   = 'Invalid exchange parameters';
  stscMoneyInvalidTriangleExchangeS = 'Invalid triangle exchange';
  stscMoneyNoSuchExchangeS          = 'No exchange rate for %s->%s available';
  stscMoneyMissingIntermediateRateS = 'Intermediate exchange rate for %s->%s missing';
  stscMoneyInvalidExchRateS         = 'Exchange rate is missing a property value';
  stscMoneyTriExchUsesTriExchS      = 'Triangular exchange rate is using triangular exchange rates';

  stscDecMathRoundPlacesS           = 'Decimal math: the number of decimal places to round to must be betwen 0 and 16';
  stscDecMathAsIntOverflowS         = 'Decimal math: current value overflows an integer';
  stscDecMathConversionS            = 'Decimal math: string value not a valid number';
  stscDecMathDivByZeroS             = 'Decimal math: division by zero attempted';
  stscDecMathNegExpS                = 'Decimal math: cannot raise to a negative power';
  stscDecMathMultOverflowS          = 'Decimal math: result overflowed during multiplication';
  stscDecMathDivOverflowS           = 'Decimal math: result overflowed during division';

  stscTxtDatNoSuchFieldS            = 'No such field';
  stscTxtDatUniqueNameRequiredS     = 'Field name must be unique';
  stscTxtDatUnhandledVariantS       = 'Unhandled Variant Type';
  stscTxtDatInvalidSchemaS          = 'Invalid Schema';
  stscTxtDatRecordSetOpenS          = 'Cannot perform this operation on an open record set';

  stscPRNGDegFreedomS               = 'StRandom: the number of degrees of freedom should be greater than zero';
  stscPRNGBetaShapeS                = 'StRandom: the Beta distribution shape values should be greater than zero';
  stscPRNGMeanS                     = 'StRandom: the mean must be greater than zero';
  stscPRNGGammaShapeS               = 'StRandom: the Gamma distribution shape must be greater than zero';
  stscPRNGGammaScaleS               = 'StRandom: the Gamma distribution scale must be greater than zero';
  stscPRNGStdDevS                   = 'StRandom: the standard deviation must be greater than zero';
  stscPRNGWeibullShapeS             = 'StRandom: the Weibull distribution shape must be greater than zero';
  stscPRNGWeibullScaleS             = 'StRandom: the Weibull distribution scale must be greater than zero';
  stscPRNGLimitS                    = 'StRandom: the limit must be greater than zero';
  stscPRNGUpperLimitS               = 'StRandom: the upper limit must be greater than the lower limit';
  stscPRNGErlangOrderS              = 'StRandom: the Erlang distribution''s order must be greater than zero';


type
  StStrRec = record
    ID: Integer;
    Str: string;
  end;

const
  SysToolsStrArray : array [0..174] of StStrRec = (

  {string table constants for STREGINI}
 (ID: stscFalseString; Str: stscFalseStringS),
 (ID: stscTrueString; Str: stscTrueStringS),
 (ID: stscNoFileKey; Str: stscNoFileKeyS),
 (ID: stscInvalidPKey; Str: stscInvalidPKeyS),
 (ID: stscNoWin32S; Str: stscNoWin32SS),
 (ID: stscCreateKeyFail; Str: stscCreateKeyFailS),
 (ID: stscOpenKeyFail; Str: stscOpenKeyFailS),
 (ID: stscIniWriteFail; Str: stscIniWriteFailS),
 (ID: stscRegWriteFail; Str: stscRegWriteFailS),
 (ID: stscNoKeyName; Str: stscNoKeyNameS),
 (ID: stscQueryKeyFail; Str: stscQueryKeyFailS),
 (ID: stscEnumKeyFail; Str: stscEnumKeyFailS),
 (ID: stscEnumValueFail; Str: stscEnumValueFailS),
 (ID: stscIniDeleteFail; Str: stscIniDeleteFailS),
 (ID: stscKeyHasSubKeys; Str: stscKeyHasSubKeysS),
 (ID: stscDeleteKeyFail; Str: stscDeleteKeyFailS),
 (ID: stscIniDelValueFail; Str: stscIniDelValueFailS),
 (ID: stscRegDelValueFail; Str: stscRegDelValueFailS),
 (ID: stscOutputFileExists; Str: stscOutputFileExistsS),
 (ID: stscFileHasExtension; Str: stscFileHasExtensionS),
 (ID: stscSaveKeyFail; Str: stscSaveKeyFailS),
 (ID: stscNo16bitSupport; Str: stscNo16bitSupportS),
 (ID: stscCantFindInputFile; Str: stscCantFindInputFileS),
 (ID: stscLoadKeyFail; Str: stscLoadKeyFailS),
 (ID: stscUnloadKeyFail; Str: stscUnloadKeyFailS),
 (ID: stscNotWinNTPlatform; Str: stscNotWinNTPlatformS),
 (ID: stscBadOptionsKeyCombo; Str: stscBadOptionsKeyComboS),
 (ID: stscRestoreKeyFail; Str: stscRestoreKeyFailS),
 (ID: stscReplaceKeyFail; Str: stscReplaceKeyFailS),
 (ID: stscNoIniFileSupport; Str: stscNoIniFileSupportS),
 (ID: stscRemoteKeyIsOpen; Str: stscRemoteKeyIsOpenS),
 (ID: stscConnectRemoteKeyFail; Str: stscConnectRemoteKeyFailS),
 (ID: stscCloseRemoteKeyFail; Str: stscCloseRemoteKeyFailS),
 (ID: stscFlushKeyFail; Str: stscFlushKeyFailS),
 (ID: stscBufferDataSizesDif; Str: stscBufferDataSizesDifS),
 (ID: stscKeyIsEmptyNotExists; Str: stscKeyIsEmptyNotExistsS),
 (ID: stscGetSecurityFail; Str: stscGetSecurityFailS),
 (ID: stscSetSecurityFail; Str: stscSetSecurityFailS),
 (ID: stscByteArrayTooLarge; Str: stscByteArrayTooLargeS),
 (ID: stscQueryValueFail; Str: stscQueryValueFailS),
 (ID: stscNoValueNameSpecified; Str: stscNoValueNameSpecifiedS),

  {string table constants for container classes}
 (ID: stscNoCompare; Str: stscNoCompareS), {Compare property must be set}
 (ID: stscBadType; Str: stscBadTypeS), {an incompatible class is passed to a method}
 (ID: stscBadSize; Str: stscBadSizeS), {bad size for TStDictionary, TStBits, TStCollection}
 (ID: stscDupNode; Str: stscDupNodeS), {attempt to add duplicate node to TStTree}
 (ID: stscBadIndex; Str: stscBadIndexS), {bad index passed to TStBits or large array}
 (ID: stscBadWinMode; Str: stscBadWinModeS), {requires enhanced mode operation}
 (ID: stscUnknownClass; Str: stscUnknownClassS), {container class name not registered}
 (ID: stscUnknownNodeClass; Str: stscUnknownNodeClassS), {container node class name not registered}
 (ID: stscNoStoreData; Str: stscNoStoreDataS), {container has no store data routine}
 (ID: stscNoLoadData; Str: stscNoLoadDataS), {container has no load data routine}
 (ID: stscWrongClass; Str: stscWrongClassS), {container class and streamed class not equal}
 (ID: stscWrongNodeClass; Str: stscWrongNodeClassS), {container node class and streamed class not equal}
 (ID: stscBadCompare; Str: stscBadCompareS), {invalid compare function or unable to assign now}
 (ID: stscTooManyCols; Str: stscTooManyColsS), {assign a matrix with >1 col to array}
 (ID: stscBadColCount; Str: stscBadColCountS), {assign a matrix with wrong col count to virtual matrix}
 (ID: stscBadElSize; Str: stscBadElSizeS), {assign a matrix with wrong elem size to virtual matrix}
 (ID: stscBadDups; Str: stscBadDupsS), {setting Dups to False in a non-empty sorted collection}

  {string table constants for sorting unit}
 (ID: stscTooManyFiles; Str: stscTooManyFilesS), {too many merge files in TStSorter}
 (ID: stscFileCreate; Str: stscFileCreateS), {error creating file in TStSorter}
 (ID: stscFileOpen; Str: stscFileOpenS), {error opening file in TStSorter}
 (ID: stscFileWrite; Str: stscFileWriteS), {error writing file in TStSorter}
 (ID: stscFileRead; Str: stscFileReadS), {error reading file in TStSorter}
 (ID: stscBadState; Str: stscBadStateS), {TStSorter in wrong state}

  {string table constants for Bcd unit}
 (ID: stscBcdBadFormat; Str: stscBcdBadFormatS), {bad BCD format}
 (ID: stscBcdOverflow; Str: stscBcdOverflowS), {BCD larger than 10**64}
 (ID: stscBcdDivByZero; Str: stscBcdDivByZeroS), {BCD divide by zero}
 (ID: stscBcdBadInput; Str: stscBcdBadInputS), {BCD negative input to sqrt, ln, or power}
 (ID: stscBcdBufOverflow; Str: stscBcdBufOverflowS), {buffer overflow in FormatBcd}
 (ID: stscNoVerInfo; Str: stscNoVerInfoS), {no version info in file}
 (ID: stscVerInfoFail; Str: stscVerInfoFailS), {error reading version info}

(*
  {shell string constants}
 (ID: stscShellVersionError; Str: stscShellVersionErrorS), {not available in this version of Shell32.dll}
 (ID: stscShellFileOpSrcError; Str: stscShellFileOpSrcErrorS), {no source files specified}
 (ID: stscShellFileOpDstError; Str: stscShellFileOpDstErrorS), {no destination files specified}
 (ID: stscShellFileOpMapError; Str: stscShellFileOpMapErrorS), {mapping incomplete}
 (ID: stscShellFormatError; Str: stscShellFormatErrorS), {format error}
 (ID: stscShellFormatCancel; Str: stscShellFormatCancelS), {format cancelled}
 (ID: stscShellFormatNoFormat; Str: stscShellFormatNoFormatS), {drive cannot be formatted}
 (ID: stscShellFormatBadDrive; Str: stscShellFormatBadDriveS), {not removable drive}
 (ID: stscTrayIconInvalidOS; Str: stscTrayIconInvalidOSS), {bad OS (NT 3.51)}
 (ID: stscTrayIconCantAdd; Str: stscTrayIconCantAddS), {can't add icon to the tray}
 (ID: stscTrayIconCantDelete; Str: stscTrayIconCantDeleteS), {can't delete icon from the tray}
 (ID: stscTrayIconError; Str: stscTrayIconErrorS), {general tray icon error}
 (ID: stscBadDropTarget; Str: stscBadDropTargetS), {drop target is not TWinControl}
 (ID: stscCOMInitFailed; Str: stscCOMInitFailedS), {COInitialize failed}
 (ID: stscNoPathSpecified; Str: stscNoPathSpecifiedS), {No destination path for shortcut}
 (ID: stscIShellLinkError; Str: stscIShellLinkErrorS), {Error creating IShellLink}
 (ID: stscNotShortcut; Str: stscNotShortcutS), {File is not a shortcut}
 (ID: stscTrayIconClose; Str: stscTrayIconCloseS), {Close}
 (ID: stscTrayIconRestore; Str: stscTrayIconRestoreS), {Restore}
 (ID: stscInvalidTargetFile; Str: stscInvalidTargetFileS), {Shortcut target file not found}
 (ID: stscShellFileOpDelete; Str: stscShellFileOpDeleteS), {Can't use file mappings with delete op}
 (ID: stscShellFileNotFound; Str: stscShellFileNotFoundS), {One or more source files is missing}
 (ID: stscTrayIconDuplicate; Str: stscTrayIconDuplicateS), {Cant' have more than one tray icon}
 (ID: stscBadVerInfoKey; Str: stscBadVerInfoKeyS), {User-defined key not found in ver info}
 (ID: stscImageListInvalid; Str: stscImageListInvalidS), {No image list assigned.}
*)
 (ID: stscBadVerInfoKey; Str: stscBadVerInfoKeyS), {User-defined key not found in ver info}

  {barcode errors}
 (ID: stscInvalidUPCACodeLen; Str: stscInvalidUPCACodeLenS),
 (ID: stscInvalidCharacter; Str: stscInvalidCharacterS),
 (ID: stscInvalidCheckCharacter; Str: stscInvalidCheckCharacterS),
 (ID: stscInvalidUPCECodeLen; Str: stscInvalidUPCECodeLenS),
 (ID: stscInvalidEAN8CodeLen; Str: stscInvalidEAN8CodeLenS),
 (ID: stscInvalidEAN13CodeLen; Str: stscInvalidEAN13CodeLenS),
 (ID: stscInvalidSupCodeLen; Str: stscInvalidSupCodeLenS),

  {stexpr errors}
 (ID: stscExprEmpty; Str: stscExprEmptyS),  {empty expression}
 (ID: stscExprBadNum; Str: stscExprBadNumS),  {error in floating point number}
 (ID: stscExprBadChar; Str: stscExprBadCharS),  {unknown character}
 (ID: stscExprOpndExp; Str: stscExprOpndExpS),  {expected function, number, sign, or (}
 (ID: stscExprNumeric; Str: stscExprNumericS),  {numeric error}
 (ID: stscExprBadExp; Str: stscExprBadExpS),  {invalid expression}
 (ID: stscExprOpndOvfl; Str: stscExprOpndOvflS),  {operand stack overflow}
 (ID: stscExprUnkFunc; Str: stscExprUnkFuncS),  {unknown function identifier}
 (ID: stscExprLParExp; Str: stscExprLParExpS),  {left parenthesis expected}
 (ID: stscExprRParExp; Str: stscExprRParExpS),  {right parenthesis expected}
 (ID: stscExprCommExp; Str: stscExprCommExpS),  {list separator (comma) expected}
 (ID: stscExprDupIdent; Str: stscExprDupIdentS),  {duplicate identifier}

  {ststat errors}
 (ID: stscStatBadCount; Str: stscStatBadCountS),  {unequal or bad counts of array elements}
 (ID: stscStatBadParam; Str: stscStatBadParamS),  {invalid parameter}
 (ID: stscStatBadData; Str: stscStatBadDataS),  {invalid data point in array}
 (ID: stscStatNoConverge; Str: stscStatNoConvergeS),  {no convergence in numerical routine}

  {stfin errors}
 (ID: stscFinBadArg; Str: stscFinBadArgS),
 (ID: stscFinNoConverge; Str: stscFinNoConvergeS),

  {stmime errors}
 (ID: stscBadEncodeFmt; Str: stscBadEncodeFmtS),
 (ID: stscBadAttachment; Str: stscBadAttachmentS),
 (ID: stscDupeString; Str: stscDupeStringS),
 (ID: stscInStream; Str: stscInStreamS),

  {ststring errors}
 (ID: stscOutOfBounds; Str: stscOutOfBoundsS),  {Index out of string bounds}


  {stBarPN errors}
 (ID: stscInvalidLength; Str: stscInvalidLengthS),

  {StHTML errors}
 (ID: stscNoInputFile; Str: stscNoInputFileS),
 (ID: stscNoOutputFile; Str: stscNoOutputFileS),
 (ID: stscInFileError; Str: stscInFileErrorS),
 (ID: stscOutFileError; Str: stscOutFileErrorS),
 (ID: stscWordDelimiters; Str: stscWordDelimitersS),
 (ID: stscInvalidSLEntry; Str: stscInvalidSLEntryS),
 (ID: stscBadStream; Str: stscBadStreamS),

  {StShlCtl constansts}
 (ID: stscName; Str: stscNameS),
 (ID: stscSize; Str: stscSizeS),
 (ID: stscType; Str: stscTypeS),
 (ID: stscModified; Str: stscModifiedS),
 (ID: stscAttributes; Str: stscAttributesS),
 (ID: stscFileFolder; Str: stscFileFolderS),
 (ID: stscSystemFolder; Str: stscSystemFolderS),
 (ID: stscOriginalLoc; Str: stscOriginalLocS),
 (ID: stscDateDeleted; Str: stscDateDeletedS),
 (ID: stscFile; Str: stscFileS),
 (ID: stscInvalidFolder; Str: stscInvalidFolderS),
 (ID: stscFolderReadOnly; Str: stscFolderReadOnlyS),

  {StSpawnApplication errors}
  (ID: stscInsufficientData; Str: stscInsufficientDataS),

  {StMemoryMappedFile errors}
 (ID: stscCreateFileFailed; Str: stscCreateFileFailedS),
 (ID: stscFileMappingFailed; Str: stscFileMappingFailedS),
 (ID: stscCreateViewFailed; Str: stscCreateViewFailedS),
 (ID: stscBadOrigin; Str: stscBadOriginS),
 (ID: stscGetSizeFailed; Str: stscGetSizeFailedS),

  {buffered stream errors}
 (ID: stscNilStream; Str: stscNilStreamS),
 (ID: stscNoSeekForRead; Str: stscNoSeekForReadS),
 (ID: stscNoSeekForWrite; Str: stscNoSeekForWriteS),
 (ID: stscCannotWrite; Str: stscCannotWriteS),
 (ID: stscBadTerminator; Str: stscBadTerminatorS),
 (ID: stscBadLineLength; Str: stscBadLineLengthS),
 (ID: stscCannotSetSize; Str: stscCannotSetSizeS),

  {RegEx errors}
 (ID: stscUnknownError; Str: stscUnknownErrorS),
 (ID: stscExpandingClass; Str: stscExpandingClassS),
 (ID: stscAlternationFollowsClosure; Str: stscAlternationFollowsClosureS),
 (ID: stscUnbalancedParens; Str: stscUnbalancedParensS),
 (ID: stscFollowingClosure; Str: stscFollowingClosureS),
 (ID: stscPatternError; Str: stscPatternErrorS),
 (ID: stscUnbalancedTag; Str: stscUnbalancedTagS),
 (ID: stscNoPatterns; Str: stscNoPatternsS),
 (ID: stscPatternTooLarge; Str: stscPatternTooLargeS),
 (ID: stscStreamsNil; Str: stscStreamsNilS),
 (ID: stscInTextStreamError; Str: stscInTextStreamErrorS),
 (ID: stscOutTextStreamError; Str: stscOutTextStreamErrorS),
 (ID: stscClosureMaybeEmpty; Str: stscClosureMaybeEmptyS),
 (ID: stscInFileNotFound; Str: stscInFileNotFoundS),
 (ID: stscREInFileError; Str: stscREInFileErrorS),
 (ID: stscOutFileDelete; Str: stscOutFileDeleteS),
 (ID: stscOutFileCreate; Str: stscOutFileCreateS),


  {StNet errors 320-339}
 (ID: stscNetNoManualCreate; Str: stscNetNoManualCreateS),
 (ID: stscNetUnknownError; Str: stscNetUnknownErrorS),
 (ID: stscNetGroupNotSpecified; Str: stscNetGroupNotSpecifiedS),
 (ID: stscNetDateSpecifiedOutOfRange; Str: stscNetDateSpecifiedOutOfRangeS),
 (ID: stscNetInvalidParameter; Str: stscNetInvalidParameterS),
 (ID: stscNetInvalidItemType; Str: stscNetInvalidItemTypeS),

 { StMoney errors }
// (ID: stscMoneyIdxOutOfRange; Str:           stscMoneyIdxOutOfRangeS),
 (ID: stscMoneyNilResult; Str:               stscMoneyNilResultS),
 (ID: stscMoneyNilParameter; Str:            stscMoneyNilParameterS),
 (ID: stscMoneyCurrenciesNotMatch; Str:      stscMoneyCurrenciesNotMatchS),
 (ID: stscMoneyNoExchangeRatesAvail; Str:    stscMoneyNoExchangeRatesAvailS),
 (ID: stscMoneyInvalidExchangeParams; Str:   stscMoneyInvalidExchangeParamsS),
 (ID: stscMoneyInvalidTriangleExchange; Str: stscMoneyInvalidTriangleExchangeS),
 (ID: stscMoneyNoSuchExchange; Str:          stscMoneyNoSuchExchangeS),
 (ID: stscMoneyMissingIntermediateRate; Str: stscMoneyMissingIntermediateRateS),
 (ID: stscMoneyInvalidExchRate; Str: stscMoneyInvalidExchRateS),
 (ID: stscMoneyTriExchUsesTriExch; Str: stscMoneyTriExchUsesTriExchS),
 (ID: stscDecMathMultOverflow; Str: stscDecMathMultOverflowS),
 (ID: stscDecMathDivOverflow; Str: stscDecMathDivOverflowS),

 (ID: stscTxtDatNoSuchField; Str:        stscTxtDatNoSuchFieldS),
 (ID: stscTxtDatUniqueNameRequired; Str: stscTxtDatUniqueNameRequiredS),
 (ID: stscTxtDatUnhandledVariant; Str:   stscTxtDatUnhandledVariantS),
 (ID: stscTxtDatInvalidSchema; Str:      stscTxtDatInvalidSchemaS),
 (ID: stscTxtDatRecordSetOpen; Str:      stscTxtDatRecordSetOpenS)
 );

function SysToolsStr(Index : Integer) : string;

implementation

function SysToolsStr(Index : Integer) : string;
var
  i : Integer;
begin
  for i := Low(SysToolsStrArray) to High(SysToolsStrArray) do
    if SysToolsStrArray[i].ID = Index then
      Result := SysToolsStrArray[i].Str;
end;


initialization

end.
