unit SysTools_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision: 1.1.1.1 $
// File generated on 5/10/2000 2:46:28 PM from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: F:\TPS\SYSTOOLS\COM\SysTools.tlb (1)
// IID\LCID: {AC5A1156-9F49-49CB-8827-2C37F08BF054}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SysToolsMajorVersion = 1;
  SysToolsMinorVersion = 0;

  LIBID_SysTools: TGUID = '{AC5A1156-9F49-49CB-8827-2C37F08BF054}';

  IID_IStDate: TGUID = '{06A34C32-CF9D-40DA-B943-B7D5D178AE4D}';
  CLASS_StDate: TGUID = '{C77513BB-8AB2-4EB8-A585-8D3F8486F4E3}';
  IID_IStString: TGUID = '{B622F701-6E3F-4875-A574-201F0DB002F3}';
  CLASS_StString: TGUID = '{D152991C-854C-40DD-AB93-912311F62E2E}';
  IID_IStToHTML: TGUID = '{9F64CFCF-E50F-4807-A1AA-1E319B0A1D63}';
  DIID_IStToHTMLEvents: TGUID = '{71DAEE82-3292-4122-8E4A-E1BB58D8D162}';
  CLASS_StToHTML: TGUID = '{807E3064-CA6F-4DEF-B6F6-BA4DBF4F51DD}';
  IID_IStStringList: TGUID = '{7A005B00-EE90-4034-B05A-64579C0A7837}';
  DIID_IStStringListEvents: TGUID = '{2457A45F-311D-4E0F-A370-AAD85C6C922E}';
  CLASS_StStringList: TGUID = '{1689CC1F-4D2E-409E-AFFD-1EBAC808C569}';
  IID_IStRegEx: TGUID = '{6498218A-2A04-4B0F-AD99-AD1D551997E8}';
  DIID_IStRegExEvents: TGUID = '{25513E60-B981-49F8-AD17-E18E7358163A}';
  CLASS_StRegEx: TGUID = '{E894200A-226E-4B27-8FB5-320BF9430046}';
  IID_IStExpr: TGUID = '{C4753F38-E937-4AFE-8D35-2210305B6D19}';
  CLASS_StExpr: TGUID = '{D0200AD9-8DE6-40C4-AACB-CC12964084FE}';
  IID_IStMime: TGUID = '{96547B4C-ED9E-4844-8F74-5C9E0268F460}';
  DIID_IStMimeEvents: TGUID = '{0D42F0C6-844D-4541-8BAD-E2BFB6BF6C25}';
  CLASS_StMime: TGUID = '{0EF7D4FF-E095-4C8A-A25C-CED37FF02398}';
  IID_IStRegINI: TGUID = '{37198FBE-3932-4548-A6B1-31229E83F87B}';
  CLASS_StRegINI: TGUID = '{856EC866-10AE-4AC7-90FF-6C7D5B153064}';
  IID_IStFin: TGUID = '{821B289E-492C-4115-8484-A6545E281BB3}';
  CLASS_StFin: TGUID = '{8CD51FD5-514E-4CB0-9121-9B4B21B3A320}';
  IID_IStRegINIQueryKeyInfo: TGUID = '{DF09E023-FC82-4BC0-BEA8-AA5D5C0E9F2A}';
  CLASS_StRegINIQueryKeyInfo: TGUID = '{33B067CF-275F-444C-A63B-E98ADEE1D1B2}';
  IID_IStRegINISubKeys: TGUID = '{43B798DA-D2CA-4263-854D-E63FBB2F7C64}';
  CLASS_StRegINISubKeys: TGUID = '{A4FD4C76-1147-46C9-862B-4C3A5D246D38}';
  IID_IStRegINIValue: TGUID = '{A3E5FB23-CD5C-471B-8214-5075375DE8DF}';
  CLASS_StRegINIValue: TGUID = '{C5B752C4-113B-4887-92CC-5C30B158E502}';
  IID_IStRegINIValues: TGUID = '{FC9E57F0-ABC0-4B69-80A0-06CDACB61AEB}';
  CLASS_StRegINIValues: TGUID = '{50210282-7730-48FC-A9EC-6BBAD8B3915A}';
  IID_IStRegINISubKey: TGUID = '{BBA14CDE-6BDE-4E40-86DD-E04697595AF3}';
  CLASS_StRegINISubKey: TGUID = '{0C2EECA2-08E5-47C2-AABF-F091F71427CC}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TStBondDateType
type
  TStBondDateType = TOleEnum;
const
  bdtActual = $00000000;
  bdt30E360 = $00000001;
  bdt30360 = $00000002;

// Constants for enum TStDayType
type
  TStDayType = TOleEnum;
const
  Sunday = $00000000;
  Monday = $00000001;
  Tuesday = $00000002;
  Wednesday = $00000003;
  Thursday = $00000004;
  Friday = $00000005;
  Saturday = $00000006;

// Constants for enum TStConvertState
type
  TStConvertState = TOleEnum;
const
  csStarted = $00000000;
  csProgressing = $00000001;
  csFinished = $00000002;

// Constants for enum TStLineTerminator
type
  TStLineTerminator = TOleEnum;
const
  ltNone = $00000000;
  ltCR = $00000001;
  ltLF = $00000002;
  ltCRLF = $00000003;
  ltOther = $00000004;

// Constants for enum TStOutputOption
type
  TStOutputOption = TOleEnum;
const
  ooUnSelected = $00000001;
  ooModified = $00000002;
  ooCountOnly = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IStDate = interface;
  IStDateDisp = dispinterface;
  IStString = interface;
  IStStringDisp = dispinterface;
  IStToHTML = interface;
  IStToHTMLDisp = dispinterface;
  IStToHTMLEvents = dispinterface;
  IStStringList = interface;
  IStStringListDisp = dispinterface;
  IStStringListEvents = dispinterface;
  IStRegEx = interface;
  IStRegExDisp = dispinterface;
  IStRegExEvents = dispinterface;
  IStExpr = interface;
  IStExprDisp = dispinterface;
  IStMime = interface;
  IStMimeDisp = dispinterface;
  IStMimeEvents = dispinterface;
  IStRegINI = interface;
  IStRegINIDisp = dispinterface;
  IStFin = interface;
  IStFinDisp = dispinterface;
  IStRegINIQueryKeyInfo = interface;
  IStRegINIQueryKeyInfoDisp = dispinterface;
  IStRegINISubKeys = interface;
  IStRegINISubKeysDisp = dispinterface;
  IStRegINIValue = interface;
  IStRegINIValueDisp = dispinterface;
  IStRegINIValues = interface;
  IStRegINIValuesDisp = dispinterface;
  IStRegINISubKey = interface;
  IStRegINISubKeyDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  StDate = IStDate;
  StString = IStString;
  StToHTML = IStToHTML;
  StStringList = IStStringList;
  StRegEx = IStRegEx;
  StExpr = IStExpr;
  StMime = IStMime;
  StRegINI = IStRegINI;
  StFin = IStFin;
  StRegINIQueryKeyInfo = IStRegINIQueryKeyInfo;
  StRegINISubKeys = IStRegINISubKeys;
  StRegINIValue = IStRegINIValue;
  StRegINIValues = IStRegINIValues;
  StRegINISubKey = IStRegINISubKey;


// *********************************************************************//
// Interface: IStDate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {06A34C32-CF9D-40DA-B943-B7D5D178AE4D}
// *********************************************************************//
  IStDate = interface(IDispatch)
    ['{06A34C32-CF9D-40DA-B943-B7D5D178AE4D}']
    function  AstJulianDate(Julian: TDateTime): Double; safecall;
    function  AstJulianDatePrim(Year: Integer; Month: Integer; Day: Integer; UT: TDateTime): Double; safecall;
    function  AstJulianDateToStDate(AstJD: Double; Truncate: WordBool): TDateTime; safecall;
    function  BondDateDiff(Date1: TDateTime; Date2: TDateTime; DayBasis: TStBondDateType): TDateTime; safecall;
    function  CurrentDate: TDateTime; safecall;
    function  CurrentDateString(const Picture: WideString; Pack: WordBool): WideString; safecall;
    function  CurrentTime: TDateTime; safecall;
    function  CurrentTimeString(const Picture: WideString; Pack: WordBool): WideString; safecall;
    procedure DateDiff(Date1: TDateTime; Date2: TDateTime; var Days: Integer; var Months: Integer; 
                       var Years: Integer); safecall;
    function  DateStringHMStoAstJD(const Picture: WideString; const DS: WideString; Hours: Integer; 
                                   Minutes: Integer; Seconds: Integer; Epoch: Integer): Double; safecall;
    function  DateStringToDMY(const Picture: WideString; const S: WideString; var Day: Integer; 
                              var Month: Integer; var Year: Integer; Epoch: Integer): WordBool; safecall;
    function  DateStringToStDate(const Picture: WideString; const S: WideString; Epoch: Integer): TDateTime; safecall;
    procedure DateTimeDiff(DT1: TDateTime; DT2: TDateTime; var Days: Integer; var Seconds: Integer); safecall;
    function  DayOfWeek(Julian: TDateTime): TStDayType; safecall;
    function  DayOfWeekDMY(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TStDayType; safecall;
    function  DayOfWeekToString(WeekDay: TStDayType): WideString; safecall;
    function  DaysInMonth(Month: Integer; Year: Integer; Epoch: Integer): Integer; safecall;
    function  DecTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime; safecall;
    function  DMYtoStDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TDateTime; safecall;
    function  DMYtoDateString(const Picture: WideString; Day: Integer; Month: Integer; 
                              Year: Integer; Epoch: Integer; Pack: WordBool): WideString; safecall;
    function  HMStoStTime(Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime; safecall;
    function  IncDate(Julian: TDateTime; Days: Integer; Months: Integer; Years: Integer): TDateTime; safecall;
    procedure IncDateTime(DT1: TDateTime; var DT2: TDateTime; Days: Integer; Seconds: Integer); safecall;
    function  IncDateTrunc(Julian: TDateTime; Months: Integer; Years: Integer): TDateTime; safecall;
    function  IncTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime; safecall;
    function  InternationalDate(ForceCentury: WordBool): WideString; safecall;
    function  InternationalLongDate(ShortNames: WordBool; ExcludeDOW: WordBool): WideString; safecall;
    function  InternationalTime(ShowSeconds: WordBool): WideString; safecall;
    function  IsLeapYear(Year: Integer): WordBool; safecall;
    function  MonthToString(Month: Integer): WideString; safecall;
    function  RoundToNearestHour(T: TDateTime; Truncate: WordBool): TDateTime; safecall;
    function  RoundToNearestMinute(T: TDateTime; Truncate: WordBool): TDateTime; safecall;
    function  StDateToDateString(const Picture: WideString; Julian: TDateTime; Pack: WordBool): WideString; safecall;
    procedure StDateToDMY(Julian: TDateTime; var Day: Integer; var Month: Integer; var Year: Integer); safecall;
    function  StTimeToAmPmString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString; safecall;
    procedure StTimeToHMS(T: TDateTime; var Hours: Byte; var Minutes: Byte; var Seconds: Byte); safecall;
    function  StTimeToTimeString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString; safecall;
    procedure TimeDiff(T1: TDateTime; T2: TDateTime; var Hours: Byte; var Minutes: Byte; 
                       var Seconds: Byte); safecall;
    function  TimeStringToHMS(const Picture: WideString; const TS: WideString; var Hours: Integer; 
                              var Minutes: Integer; var Seconds: Integer): WordBool; safecall;
    function  TimeStringToStTime(const Picture: WideString; const S: WideString): TDateTime; safecall;
    function  ValidDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): WordBool; safecall;
    function  ValidTime(Hours: Integer; Minutes: Integer; Seconds: Integer): WordBool; safecall;
    function  WeekOfYear(Julian: TDateTime): Byte; safecall;
    function  License(const Key: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IStDateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {06A34C32-CF9D-40DA-B943-B7D5D178AE4D}
// *********************************************************************//
  IStDateDisp = dispinterface
    ['{06A34C32-CF9D-40DA-B943-B7D5D178AE4D}']
    function  AstJulianDate(Julian: TDateTime): Double; dispid 1;
    function  AstJulianDatePrim(Year: Integer; Month: Integer; Day: Integer; UT: TDateTime): Double; dispid 2;
    function  AstJulianDateToStDate(AstJD: Double; Truncate: WordBool): TDateTime; dispid 3;
    function  BondDateDiff(Date1: TDateTime; Date2: TDateTime; DayBasis: TStBondDateType): TDateTime; dispid 4;
    function  CurrentDate: TDateTime; dispid 5;
    function  CurrentDateString(const Picture: WideString; Pack: WordBool): WideString; dispid 6;
    function  CurrentTime: TDateTime; dispid 7;
    function  CurrentTimeString(const Picture: WideString; Pack: WordBool): WideString; dispid 8;
    procedure DateDiff(Date1: TDateTime; Date2: TDateTime; var Days: Integer; var Months: Integer; 
                       var Years: Integer); dispid 9;
    function  DateStringHMStoAstJD(const Picture: WideString; const DS: WideString; Hours: Integer; 
                                   Minutes: Integer; Seconds: Integer; Epoch: Integer): Double; dispid 10;
    function  DateStringToDMY(const Picture: WideString; const S: WideString; var Day: Integer; 
                              var Month: Integer; var Year: Integer; Epoch: Integer): WordBool; dispid 11;
    function  DateStringToStDate(const Picture: WideString; const S: WideString; Epoch: Integer): TDateTime; dispid 12;
    procedure DateTimeDiff(DT1: TDateTime; DT2: TDateTime; var Days: Integer; var Seconds: Integer); dispid 13;
    function  DayOfWeek(Julian: TDateTime): TStDayType; dispid 14;
    function  DayOfWeekDMY(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TStDayType; dispid 15;
    function  DayOfWeekToString(WeekDay: TStDayType): WideString; dispid 16;
    function  DaysInMonth(Month: Integer; Year: Integer; Epoch: Integer): Integer; dispid 17;
    function  DecTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime; dispid 18;
    function  DMYtoStDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TDateTime; dispid 19;
    function  DMYtoDateString(const Picture: WideString; Day: Integer; Month: Integer; 
                              Year: Integer; Epoch: Integer; Pack: WordBool): WideString; dispid 20;
    function  HMStoStTime(Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime; dispid 21;
    function  IncDate(Julian: TDateTime; Days: Integer; Months: Integer; Years: Integer): TDateTime; dispid 22;
    procedure IncDateTime(DT1: TDateTime; var DT2: TDateTime; Days: Integer; Seconds: Integer); dispid 23;
    function  IncDateTrunc(Julian: TDateTime; Months: Integer; Years: Integer): TDateTime; dispid 24;
    function  IncTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime; dispid 25;
    function  InternationalDate(ForceCentury: WordBool): WideString; dispid 26;
    function  InternationalLongDate(ShortNames: WordBool; ExcludeDOW: WordBool): WideString; dispid 27;
    function  InternationalTime(ShowSeconds: WordBool): WideString; dispid 28;
    function  IsLeapYear(Year: Integer): WordBool; dispid 29;
    function  MonthToString(Month: Integer): WideString; dispid 30;
    function  RoundToNearestHour(T: TDateTime; Truncate: WordBool): TDateTime; dispid 31;
    function  RoundToNearestMinute(T: TDateTime; Truncate: WordBool): TDateTime; dispid 32;
    function  StDateToDateString(const Picture: WideString; Julian: TDateTime; Pack: WordBool): WideString; dispid 33;
    procedure StDateToDMY(Julian: TDateTime; var Day: Integer; var Month: Integer; var Year: Integer); dispid 34;
    function  StTimeToAmPmString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString; dispid 35;
    procedure StTimeToHMS(T: TDateTime; var Hours: Byte; var Minutes: Byte; var Seconds: Byte); dispid 36;
    function  StTimeToTimeString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString; dispid 37;
    procedure TimeDiff(T1: TDateTime; T2: TDateTime; var Hours: Byte; var Minutes: Byte; 
                       var Seconds: Byte); dispid 38;
    function  TimeStringToHMS(const Picture: WideString; const TS: WideString; var Hours: Integer; 
                              var Minutes: Integer; var Seconds: Integer): WordBool; dispid 39;
    function  TimeStringToStTime(const Picture: WideString; const S: WideString): TDateTime; dispid 40;
    function  ValidDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): WordBool; dispid 41;
    function  ValidTime(Hours: Integer; Minutes: Integer; Seconds: Integer): WordBool; dispid 42;
    function  WeekOfYear(Julian: TDateTime): Byte; dispid 43;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// Interface: IStString
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B622F701-6E3F-4875-A574-201F0DB002F3}
// *********************************************************************//
  IStString = interface(IDispatch)
    ['{B622F701-6E3F-4875-A574-201F0DB002F3}']
    function  AddBackSlash(const DirName: WideString): WideString; safecall;
    function  AsciiCount(const S: WideString; const WordDelims: WideString; const Quote: WideString): Integer; safecall;
    function  AsciiPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                            const Quote: WideString; var Pos: Integer): WordBool; safecall;
    function  BinaryB(B: Byte): WideString; safecall;
    function  BinaryW(W: Integer): WideString; safecall;
    function  BinaryL(L: Integer): WideString; safecall;
    function  Center(const S: WideString; Len: Integer): WideString; safecall;
    function  CenterCh(const S: WideString; const C: WideString; Len: Integer): WideString; safecall;
    function  CharCount(const S: WideString; const C: WideString): Integer; safecall;
    function  CharExists(const S: WideString; const C: WideString): WordBool; safecall;
    function  CharStr(const C: WideString; Len: Integer): WideString; safecall;
    function  CleanPathName(const PathName: WideString): WideString; safecall;
    function  Commaize(L: Integer): WideString; safecall;
    function  CommaizeCh(L: Integer; const Ch: WideString): WideString; safecall;
    function  CompString(const S1: WideString; const S2: WideString): Integer; safecall;
    function  CompUCString(const S1: WideString; const S2: WideString): Integer; safecall;
    function  ContainsOnly(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool; safecall;
    function  ContainsOtherThan(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool; safecall;
    function  CopyFromNthWord(const S: WideString; const WordDelims: WideString; 
                              const AWord: WideString; N: Integer; var SubString: WideString): WordBool; safecall;
    function  CopyFromToWord(const S: WideString; const WordDelims: WideString; 
                             const Word1: WideString; const Word2: WideString; N1: Integer; 
                             N2: Integer; var SubString: WideString): WordBool; safecall;
    function  CopyLeft(const S: WideString; Len: Integer): WideString; safecall;
    function  CopyMid(const S: WideString; First: Integer; Len: Integer): WideString; safecall;
    function  CopyRight(const S: WideString; First: Integer): WideString; safecall;
    function  CopyWithin(const S: WideString; const Delimiter: WideString; Strip: WordBool): WideString; safecall;
    function  DefaultExtension(const Name: WideString; const Ext: WideString): WideString; safecall;
    function  DeleteFromNthWord(const S: WideString; const WordDelims: WideString; 
                                const AWord: WideString; N: Integer; var SubString: WideString): WordBool; safecall;
    function  DeleteFromToWord(const S: WideString; const WordDelims: WideString; 
                               const Word1: WideString; const Word2: WideString; N1: Integer; 
                               N2: Integer; var SubString: WideString): WordBool; safecall;
    function  DeleteWithin(const S: WideString; const Delimeter: WideString): WideString; safecall;
    function  Detab(const S: WideString; TabSize: Byte): WideString; safecall;
    function  Entab(const S: WideString; TabSize: Byte): WideString; safecall;
    function  Ext2Str(R: OleVariant; Width: Byte; Places: Byte): WideString; safecall;
    function  ExtractAscii(N: Integer; const S: WideString; const WordDelims: WideString; 
                           const Quote: WideString): WideString; safecall;
    function  ExtractTokens(const S: WideString; const Delims: WideString; 
                            const QuoteChar: WideString; AllowNulls: WordBool; 
                            out Tokens: IStStringList): Integer; safecall;
    function  ExtractWord(N: Integer; const S: WideString; const WordDelims: WideString): WideString; safecall;
    function  Filter(const S: WideString; const Filters: WideString): WideString; safecall;
    function  FloatForm(const Mask: WideString; R: Double; L: Integer; const LtCurr: WideString; 
                        const RtCurr: WideString; const Sep: WideString; const DecPt: WideString): WideString; safecall;
    function  ForceExtension(const Name: WideString; const Ext: WideString): WideString; safecall;
    function  HasExtension(const Name: WideString; var DotPos: Integer): WordBool; safecall;
    function  HexB(B: Byte): WideString; safecall;
    function  HexW(W: Integer): WideString; safecall;
    function  HexL(L: Integer): WideString; safecall;
    function  IsChAlpha(const C: WideString): WordBool; safecall;
    function  IsChAlphaNumeric(const C: WideString; const Numbers: WideString): WordBool; safecall;
    function  IsChNumeric(const C: WideString; const Numbers: WideString): WordBool; safecall;
    function  IsStrAlpha(const S: WideString): WordBool; safecall;
    function  IsStrAlphaNumeric(const S: WideString; const Numbers: WideString): WordBool; safecall;
    function  IsStrNumeric(const S: WideString; const Numbers: WideString): WordBool; safecall;
    function  JustExtension(const Name: WideString): WideString; safecall;
    function  JustFilename(const PathName: WideString): WideString; safecall;
    function  JustName(const PathName: WideString): WideString; safecall;
    function  JustPathname(const PathName: WideString): WideString; safecall;
    function  KeepChars(const S: WideString; const Chars: WideString): WideString; safecall;
    function  LastString(const S: WideString; const AString: WideString; var Position: Integer): WordBool; safecall;
    function  LastWord(const S: WideString; const WordDelims: WideString; const AWord: WideString; 
                       var Position: Integer): WordBool; safecall;
    function  LastWordAbs(const S: WideString; const WordDelims: WideString; var Position: Integer): WordBool; safecall;
    function  LeftPad(const S: WideString; Len: Integer): WideString; safecall;
    function  LeftPadCh(const S: WideString; const C: WideString; Len: Integer): WideString; safecall;
    function  LeftTrimChars(const S: WideString; const Chars: WideString): WideString; safecall;
    function  Long2Str(L: Integer): WideString; safecall;
    function  LongIntForm(const Mask: WideString; L: Integer; const LtCurr: WideString; 
                          const RtCurr: WideString; const Sep: WideString): WideString; safecall;
    function  OctalB(B: Byte): WideString; safecall;
    function  OctalW(W: Integer): WideString; safecall;
    function  OctalL(L: Integer): WideString; safecall;
    function  Pad(const S: WideString; Len: Integer): WideString; safecall;
    function  PadCh(const S: WideString; const C: WideString; Len: Integer): WideString; safecall;
    function  Real2Str(R: Double; Width: Byte; Places: Byte): WideString; safecall;
    function  RepeatString(const S: WideString; var Repetitions: Integer; MaxLen: Integer): WideString; safecall;
    function  ReplaceWord(const S: WideString; const WordDelims: WideString; 
                          const OldWord: WideString; const NewWord: WideString; N: Integer; 
                          var Replacements: Integer): WideString; safecall;
    function  ReplaceWordAll(const S: WideString; const WordDelims: WideString; 
                             const OldWord: WideString; const NewWord: WideString; 
                             var Replacements: Integer): WideString; safecall;
    function  ReplaceString(const S: WideString; const OldString: WideString; 
                            const NewString: WideString; N: Integer; var Replacements: Integer): WideString; safecall;
    function  ReplaceStringAll(const S: WideString; const OldString: WideString; 
                               const NewString: WideString; var Replacements: Integer): WideString; safecall;
    function  RightTrimChars(const S: WideString; const Chars: WideString): WideString; safecall;
    function  Scramble(const S: WideString; const Key: WideString): WideString; safecall;
    function  Str2Ext(const S: WideString; var R: OleVariant): WordBool; safecall;
    function  Str2Int16(const S: WideString; var I: Smallint): WordBool; safecall;
    function  Str2Long(const S: WideString; var I: Integer): WordBool; safecall;
    function  Str2Real(const S: WideString; var R: Double): WordBool; safecall;
    function  Str2Word(const S: WideString; var W: Integer): WordBool; safecall;
    function  StrChDelete(const S: WideString; Pos: Integer): WideString; safecall;
    function  StrChInsert(const S: WideString; const C: WideString; Pos: Integer): WideString; safecall;
    function  StrChPos(const P: WideString; const C: WideString; var Pos: Integer): WordBool; safecall;
    function  StrStCopy(const S: WideString; Pos: Integer; Count: Integer): WideString; safecall;
    function  StrStDelete(const S: WideString; Pos: Integer; Count: Integer): WideString; safecall;
    function  StrStInsert(const S1: WideString; const S2: WideString; Pos: Integer): WideString; safecall;
    function  StrStPos(const P: WideString; const S: WideString; var Pos: Integer): WordBool; safecall;
    function  StrWithin(const S: WideString; const SearchStr: WideString; Start: Integer; 
                        var Position: Integer): WordBool; safecall;
    function  Substitute(const S: WideString; const FromStr: WideString; const ToStr: WideString): WideString; safecall;
    function  Trim(const S: WideString): WideString; safecall;
    function  TrimChars(const S: WideString; const Chars: WideString): WideString; safecall;
    function  TrimLead(const S: WideString): WideString; safecall;
    function  TrimSpaces(const S: WideString): WideString; safecall;
    function  TrimTrail(const S: WideString): WideString; safecall;
    function  ValPrep(const S: WideString): WideString; safecall;
    function  WordCount(const S: WideString; const WordDelims: WideString): Integer; safecall;
    function  WordPos(const S: WideString; const WordDelims: WideString; const AWord: WideString; 
                      N: Integer; var Position: Integer): WordBool; safecall;
    function  WordPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                           var Position: Integer): WordBool; safecall;
    procedure WordWrap(const InSt: WideString; var OutSt: WideString; var Overlap: WideString; 
                       Margin: Integer; PadToMArgin: WordBool); safecall;
    function  License(const Key: WideString): WordBool; safecall;
    function  Soundex(const S: WideString): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IStStringDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B622F701-6E3F-4875-A574-201F0DB002F3}
// *********************************************************************//
  IStStringDisp = dispinterface
    ['{B622F701-6E3F-4875-A574-201F0DB002F3}']
    function  AddBackSlash(const DirName: WideString): WideString; dispid 1;
    function  AsciiCount(const S: WideString; const WordDelims: WideString; const Quote: WideString): Integer; dispid 2;
    function  AsciiPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                            const Quote: WideString; var Pos: Integer): WordBool; dispid 3;
    function  BinaryB(B: Byte): WideString; dispid 4;
    function  BinaryW(W: Integer): WideString; dispid 5;
    function  BinaryL(L: Integer): WideString; dispid 6;
    function  Center(const S: WideString; Len: Integer): WideString; dispid 7;
    function  CenterCh(const S: WideString; const C: WideString; Len: Integer): WideString; dispid 8;
    function  CharCount(const S: WideString; const C: WideString): Integer; dispid 9;
    function  CharExists(const S: WideString; const C: WideString): WordBool; dispid 10;
    function  CharStr(const C: WideString; Len: Integer): WideString; dispid 11;
    function  CleanPathName(const PathName: WideString): WideString; dispid 12;
    function  Commaize(L: Integer): WideString; dispid 13;
    function  CommaizeCh(L: Integer; const Ch: WideString): WideString; dispid 14;
    function  CompString(const S1: WideString; const S2: WideString): Integer; dispid 15;
    function  CompUCString(const S1: WideString; const S2: WideString): Integer; dispid 16;
    function  ContainsOnly(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool; dispid 17;
    function  ContainsOtherThan(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool; dispid 18;
    function  CopyFromNthWord(const S: WideString; const WordDelims: WideString; 
                              const AWord: WideString; N: Integer; var SubString: WideString): WordBool; dispid 19;
    function  CopyFromToWord(const S: WideString; const WordDelims: WideString; 
                             const Word1: WideString; const Word2: WideString; N1: Integer; 
                             N2: Integer; var SubString: WideString): WordBool; dispid 20;
    function  CopyLeft(const S: WideString; Len: Integer): WideString; dispid 21;
    function  CopyMid(const S: WideString; First: Integer; Len: Integer): WideString; dispid 22;
    function  CopyRight(const S: WideString; First: Integer): WideString; dispid 23;
    function  CopyWithin(const S: WideString; const Delimiter: WideString; Strip: WordBool): WideString; dispid 24;
    function  DefaultExtension(const Name: WideString; const Ext: WideString): WideString; dispid 25;
    function  DeleteFromNthWord(const S: WideString; const WordDelims: WideString; 
                                const AWord: WideString; N: Integer; var SubString: WideString): WordBool; dispid 26;
    function  DeleteFromToWord(const S: WideString; const WordDelims: WideString; 
                               const Word1: WideString; const Word2: WideString; N1: Integer; 
                               N2: Integer; var SubString: WideString): WordBool; dispid 27;
    function  DeleteWithin(const S: WideString; const Delimeter: WideString): WideString; dispid 28;
    function  Detab(const S: WideString; TabSize: Byte): WideString; dispid 29;
    function  Entab(const S: WideString; TabSize: Byte): WideString; dispid 30;
    function  Ext2Str(R: OleVariant; Width: Byte; Places: Byte): WideString; dispid 31;
    function  ExtractAscii(N: Integer; const S: WideString; const WordDelims: WideString; 
                           const Quote: WideString): WideString; dispid 32;
    function  ExtractTokens(const S: WideString; const Delims: WideString; 
                            const QuoteChar: WideString; AllowNulls: WordBool; 
                            out Tokens: IStStringList): Integer; dispid 33;
    function  ExtractWord(N: Integer; const S: WideString; const WordDelims: WideString): WideString; dispid 34;
    function  Filter(const S: WideString; const Filters: WideString): WideString; dispid 35;
    function  FloatForm(const Mask: WideString; R: Double; L: Integer; const LtCurr: WideString; 
                        const RtCurr: WideString; const Sep: WideString; const DecPt: WideString): WideString; dispid 36;
    function  ForceExtension(const Name: WideString; const Ext: WideString): WideString; dispid 37;
    function  HasExtension(const Name: WideString; var DotPos: Integer): WordBool; dispid 38;
    function  HexB(B: Byte): WideString; dispid 39;
    function  HexW(W: Integer): WideString; dispid 40;
    function  HexL(L: Integer): WideString; dispid 41;
    function  IsChAlpha(const C: WideString): WordBool; dispid 42;
    function  IsChAlphaNumeric(const C: WideString; const Numbers: WideString): WordBool; dispid 43;
    function  IsChNumeric(const C: WideString; const Numbers: WideString): WordBool; dispid 44;
    function  IsStrAlpha(const S: WideString): WordBool; dispid 45;
    function  IsStrAlphaNumeric(const S: WideString; const Numbers: WideString): WordBool; dispid 46;
    function  IsStrNumeric(const S: WideString; const Numbers: WideString): WordBool; dispid 47;
    function  JustExtension(const Name: WideString): WideString; dispid 48;
    function  JustFilename(const PathName: WideString): WideString; dispid 49;
    function  JustName(const PathName: WideString): WideString; dispid 50;
    function  JustPathname(const PathName: WideString): WideString; dispid 51;
    function  KeepChars(const S: WideString; const Chars: WideString): WideString; dispid 52;
    function  LastString(const S: WideString; const AString: WideString; var Position: Integer): WordBool; dispid 53;
    function  LastWord(const S: WideString; const WordDelims: WideString; const AWord: WideString; 
                       var Position: Integer): WordBool; dispid 54;
    function  LastWordAbs(const S: WideString; const WordDelims: WideString; var Position: Integer): WordBool; dispid 55;
    function  LeftPad(const S: WideString; Len: Integer): WideString; dispid 56;
    function  LeftPadCh(const S: WideString; const C: WideString; Len: Integer): WideString; dispid 57;
    function  LeftTrimChars(const S: WideString; const Chars: WideString): WideString; dispid 58;
    function  Long2Str(L: Integer): WideString; dispid 59;
    function  LongIntForm(const Mask: WideString; L: Integer; const LtCurr: WideString; 
                          const RtCurr: WideString; const Sep: WideString): WideString; dispid 60;
    function  OctalB(B: Byte): WideString; dispid 61;
    function  OctalW(W: Integer): WideString; dispid 62;
    function  OctalL(L: Integer): WideString; dispid 63;
    function  Pad(const S: WideString; Len: Integer): WideString; dispid 64;
    function  PadCh(const S: WideString; const C: WideString; Len: Integer): WideString; dispid 65;
    function  Real2Str(R: Double; Width: Byte; Places: Byte): WideString; dispid 66;
    function  RepeatString(const S: WideString; var Repetitions: Integer; MaxLen: Integer): WideString; dispid 67;
    function  ReplaceWord(const S: WideString; const WordDelims: WideString; 
                          const OldWord: WideString; const NewWord: WideString; N: Integer; 
                          var Replacements: Integer): WideString; dispid 68;
    function  ReplaceWordAll(const S: WideString; const WordDelims: WideString; 
                             const OldWord: WideString; const NewWord: WideString; 
                             var Replacements: Integer): WideString; dispid 69;
    function  ReplaceString(const S: WideString; const OldString: WideString; 
                            const NewString: WideString; N: Integer; var Replacements: Integer): WideString; dispid 70;
    function  ReplaceStringAll(const S: WideString; const OldString: WideString; 
                               const NewString: WideString; var Replacements: Integer): WideString; dispid 71;
    function  RightTrimChars(const S: WideString; const Chars: WideString): WideString; dispid 72;
    function  Scramble(const S: WideString; const Key: WideString): WideString; dispid 73;
    function  Str2Ext(const S: WideString; var R: OleVariant): WordBool; dispid 74;
    function  Str2Int16(const S: WideString; var I: Smallint): WordBool; dispid 75;
    function  Str2Long(const S: WideString; var I: Integer): WordBool; dispid 76;
    function  Str2Real(const S: WideString; var R: Double): WordBool; dispid 77;
    function  Str2Word(const S: WideString; var W: Integer): WordBool; dispid 78;
    function  StrChDelete(const S: WideString; Pos: Integer): WideString; dispid 79;
    function  StrChInsert(const S: WideString; const C: WideString; Pos: Integer): WideString; dispid 80;
    function  StrChPos(const P: WideString; const C: WideString; var Pos: Integer): WordBool; dispid 81;
    function  StrStCopy(const S: WideString; Pos: Integer; Count: Integer): WideString; dispid 82;
    function  StrStDelete(const S: WideString; Pos: Integer; Count: Integer): WideString; dispid 83;
    function  StrStInsert(const S1: WideString; const S2: WideString; Pos: Integer): WideString; dispid 84;
    function  StrStPos(const P: WideString; const S: WideString; var Pos: Integer): WordBool; dispid 85;
    function  StrWithin(const S: WideString; const SearchStr: WideString; Start: Integer; 
                        var Position: Integer): WordBool; dispid 86;
    function  Substitute(const S: WideString; const FromStr: WideString; const ToStr: WideString): WideString; dispid 87;
    function  Trim(const S: WideString): WideString; dispid 88;
    function  TrimChars(const S: WideString; const Chars: WideString): WideString; dispid 89;
    function  TrimLead(const S: WideString): WideString; dispid 90;
    function  TrimSpaces(const S: WideString): WideString; dispid 91;
    function  TrimTrail(const S: WideString): WideString; dispid 92;
    function  ValPrep(const S: WideString): WideString; dispid 93;
    function  WordCount(const S: WideString; const WordDelims: WideString): Integer; dispid 94;
    function  WordPos(const S: WideString; const WordDelims: WideString; const AWord: WideString; 
                      N: Integer; var Position: Integer): WordBool; dispid 95;
    function  WordPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                           var Position: Integer): WordBool; dispid 96;
    procedure WordWrap(const InSt: WideString; var OutSt: WideString; var Overlap: WideString; 
                       Margin: Integer; PadToMArgin: WordBool); dispid 97;
    function  License(const Key: WideString): WordBool; dispid 200;
    function  Soundex(const S: WideString): WideString; dispid 98;
  end;

// *********************************************************************//
// Interface: IStToHTML
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F64CFCF-E50F-4807-A1AA-1E319B0A1D63}
// *********************************************************************//
  IStToHTML = interface(IDispatch)
    ['{9F64CFCF-E50F-4807-A1AA-1E319B0A1D63}']
    function  Get_CaseSensitve: WordBool; safecall;
    procedure Set_CaseSensitve(Value: WordBool); safecall;
    function  Get_CommentMarkers: IStStringList; safecall;
    procedure Set_CommentMarkers(const Value: IStStringList); safecall;
    function  Get_EmbeddedHTML: IStStringList; safecall;
    procedure Set_EmbeddedHTML(const Value: IStStringList); safecall;
    function  Get_FixedLineLength: Integer; safecall;
    procedure Set_FixedLineLength(Value: Integer); safecall;
    function  Get_Keywords: IStStringList; safecall;
    procedure Set_Keywords(const Value: IStStringList); safecall;
    function  Get_LineTermChar: WideString; safecall;
    procedure Set_LineTermChar(const Value: WideString); safecall;
    function  Get_LineTerminator: TStLineTerminator; safecall;
    procedure Set_LineTerminator(Value: TStLineTerminator); safecall;
    function  Get_PageFooter: IStStringList; safecall;
    procedure Set_PageFooter(const Value: IStStringList); safecall;
    function  Get_PageHeader: IStStringList; safecall;
    procedure Set_PageHeader(const Value: IStStringList); safecall;
    function  Get_StringMarkers: IStStringList; safecall;
    procedure Set_StringMarkers(const Value: IStStringList); safecall;
    function  Get_WordDelimeters: WideString; safecall;
    procedure Set_WordDelimeters(const Value: WideString); safecall;
    procedure GenerateHTML; safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    function  Get_Stream: OleVariant; safecall;
    procedure Set_Stream(Value: OleVariant); safecall;
    procedure Clear; safecall;
    function  License(const Key: WideString): WordBool; safecall;
    property CaseSensitve: WordBool read Get_CaseSensitve write Set_CaseSensitve;
    property CommentMarkers: IStStringList read Get_CommentMarkers write Set_CommentMarkers;
    property EmbeddedHTML: IStStringList read Get_EmbeddedHTML write Set_EmbeddedHTML;
    property FixedLineLength: Integer read Get_FixedLineLength write Set_FixedLineLength;
    property Keywords: IStStringList read Get_Keywords write Set_Keywords;
    property LineTermChar: WideString read Get_LineTermChar write Set_LineTermChar;
    property LineTerminator: TStLineTerminator read Get_LineTerminator write Set_LineTerminator;
    property PageFooter: IStStringList read Get_PageFooter write Set_PageFooter;
    property PageHeader: IStStringList read Get_PageHeader write Set_PageHeader;
    property StringMarkers: IStStringList read Get_StringMarkers write Set_StringMarkers;
    property WordDelimeters: WideString read Get_WordDelimeters write Set_WordDelimeters;
    property Stream: OleVariant read Get_Stream write Set_Stream;
  end;

// *********************************************************************//
// DispIntf:  IStToHTMLDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F64CFCF-E50F-4807-A1AA-1E319B0A1D63}
// *********************************************************************//
  IStToHTMLDisp = dispinterface
    ['{9F64CFCF-E50F-4807-A1AA-1E319B0A1D63}']
    property CaseSensitve: WordBool dispid 1;
    property CommentMarkers: IStStringList dispid 2;
    property EmbeddedHTML: IStStringList dispid 3;
    property FixedLineLength: Integer dispid 4;
    property Keywords: IStStringList dispid 5;
    property LineTermChar: WideString dispid 6;
    property LineTerminator: TStLineTerminator dispid 7;
    property PageFooter: IStStringList dispid 8;
    property PageHeader: IStStringList dispid 9;
    property StringMarkers: IStStringList dispid 10;
    property WordDelimeters: WideString dispid 11;
    procedure GenerateHTML; dispid 12;
    procedure LoadFromFile(const FileName: WideString); dispid 13;
    procedure SaveToFile(const FileName: WideString); dispid 15;
    property Stream: OleVariant dispid 17;
    procedure Clear; dispid 14;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// DispIntf:  IStToHTMLEvents
// Flags:     (4096) Dispatchable
// GUID:      {71DAEE82-3292-4122-8E4A-E1BB58D8D162}
// *********************************************************************//
  IStToHTMLEvents = dispinterface
    ['{71DAEE82-3292-4122-8E4A-E1BB58D8D162}']
    procedure OnProgress(Percent: Integer); dispid 1;
  end;

// *********************************************************************//
// Interface: IStStringList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7A005B00-EE90-4034-B05A-64579C0A7837}
// *********************************************************************//
  IStStringList = interface(IDispatch)
    ['{7A005B00-EE90-4034-B05A-64579C0A7837}']
    function  Get__NewEnum: IUnknown; safecall;
    function  Get_Item(Index: Integer): WideString; safecall;
    procedure Set_Item(Index: Integer; const Value: WideString); safecall;
    function  Get_CommaText: WideString; safecall;
    procedure Set_CommaText(const Value: WideString); safecall;
    function  Get_Count: Integer; safecall;
    function  Get_Duplicates: Integer; safecall;
    procedure Set_Duplicates(Value: Integer); safecall;
    function  Get_Names(Index: Integer): WideString; safecall;
    function  Get_Sorted: WordBool; safecall;
    procedure Set_Sorted(Value: WordBool); safecall;
    function  Get_Strings(Index: Integer): WideString; safecall;
    procedure Set_Strings(Index: Integer; const Value: WideString); safecall;
    function  Get_Text: WideString; safecall;
    procedure Set_Text(const Value: WideString); safecall;
    function  Get_Values(const Name: WideString): WideString; safecall;
    procedure Set_Values(const Name: WideString; const Value: WideString); safecall;
    function  Add(const S: WideString): Integer; safecall;
    procedure Append(const S: WideString); safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    function  Equals(const Strings: IStStringList): WordBool; safecall;
    procedure Exchange(Index1: Integer; Index2: Integer); safecall;
    function  Find(const S: WideString; var Index: Integer): WordBool; safecall;
    function  IndexOf(const S: WideString): Integer; safecall;
    function  IndexOfName(const Name: WideString): Integer; safecall;
    procedure Insert(Index: Integer; const S: WideString); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure Move(CurIndex: Integer; NewIndex: Integer); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    procedure Sort; safecall;
    function  Get_Stream: OleVariant; safecall;
    procedure Set_Stream(Value: OleVariant); safecall;
    function  License(const Key: WideString): WordBool; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: Integer]: WideString read Get_Item write Set_Item; default;
    property CommaText: WideString read Get_CommaText write Set_CommaText;
    property Count: Integer read Get_Count;
    property Duplicates: Integer read Get_Duplicates write Set_Duplicates;
    property Names[Index: Integer]: WideString read Get_Names;
    property Sorted: WordBool read Get_Sorted write Set_Sorted;
    property Strings[Index: Integer]: WideString read Get_Strings write Set_Strings;
    property Text: WideString read Get_Text write Set_Text;
    property Values[const Name: WideString]: WideString read Get_Values write Set_Values;
    property Stream: OleVariant read Get_Stream write Set_Stream;
  end;

// *********************************************************************//
// DispIntf:  IStStringListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7A005B00-EE90-4034-B05A-64579C0A7837}
// *********************************************************************//
  IStStringListDisp = dispinterface
    ['{7A005B00-EE90-4034-B05A-64579C0A7837}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Index: Integer]: WideString dispid 0; default;
    property CommaText: WideString dispid 1;
    property Count: Integer readonly dispid 2;
    property Duplicates: Integer dispid 3;
    property Names[Index: Integer]: WideString readonly dispid 4;
    property Sorted: WordBool dispid 6;
    property Strings[Index: Integer]: WideString dispid 7;
    property Text: WideString dispid 8;
    property Values[const Name: WideString]: WideString dispid 9;
    function  Add(const S: WideString): Integer; dispid 10;
    procedure Append(const S: WideString); dispid 12;
    procedure Clear; dispid 13;
    procedure Delete(Index: Integer); dispid 15;
    function  Equals(const Strings: IStStringList): WordBool; dispid 16;
    procedure Exchange(Index1: Integer; Index2: Integer); dispid 17;
    function  Find(const S: WideString; var Index: Integer): WordBool; dispid 18;
    function  IndexOf(const S: WideString): Integer; dispid 19;
    function  IndexOfName(const Name: WideString): Integer; dispid 20;
    procedure Insert(Index: Integer; const S: WideString); dispid 22;
    procedure LoadFromFile(const FileName: WideString); dispid 24;
    procedure Move(CurIndex: Integer; NewIndex: Integer); dispid 26;
    procedure SaveToFile(const FileName: WideString); dispid 27;
    procedure Sort; dispid 29;
    property Stream: OleVariant dispid 21;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// DispIntf:  IStStringListEvents
// Flags:     (4096) Dispatchable
// GUID:      {2457A45F-311D-4E0F-A370-AAD85C6C922E}
// *********************************************************************//
  IStStringListEvents = dispinterface
    ['{2457A45F-311D-4E0F-A370-AAD85C6C922E}']
    procedure OnChange; dispid 1;
    procedure OnChanging; dispid 2;
  end;

// *********************************************************************//
// Interface: IStRegEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6498218A-2A04-4B0F-AD99-AD1D551997E8}
// *********************************************************************//
  IStRegEx = interface(IDispatch)
    ['{6498218A-2A04-4B0F-AD99-AD1D551997E8}']
    function  Get_Avoid: WordBool; safecall;
    procedure Set_Avoid(Value: WordBool); safecall;
    function  Get_IgnoreCase: WordBool; safecall;
    procedure Set_IgnoreCase(Value: WordBool); safecall;
    function  Get_InFixedLineLength: Integer; safecall;
    procedure Set_InFixedLineLength(Value: Integer); safecall;
    function  Get_InLineTermChar: WideString; safecall;
    procedure Set_InLineTermChar(const Value: WideString); safecall;
    function  Get_InLineTerminator: TStLineTerminator; safecall;
    procedure Set_InLineTerminator(Value: TStLineTerminator); safecall;
    function  Get_LineCount: Integer; safecall;
    function  Get_LineNumbers: WordBool; safecall;
    procedure Set_LineNumbers(Value: WordBool); safecall;
    function  Get_LinesMatched: Integer; safecall;
    function  Get_LinesPerSecond: Integer; safecall;
    function  Get_LinesReplaced: Integer; safecall;
    function  Get_LinesSelected: Integer; safecall;
    function  Get_MatchPattern: IStStringList; safecall;
    procedure Set_MatchPattern(const Value: IStStringList); safecall;
    function  Get_OutFixedLineLength: Integer; safecall;
    procedure Set_OutFixedLineLength(Value: Integer); safecall;
    function  Get_OutLineTermChar: WideString; safecall;
    procedure Set_OutLineTermChar(const Value: WideString); safecall;
    function  Get_OutLineTerminitor: Integer; safecall;
    procedure Set_OutLineTerminitor(Value: Integer); safecall;
    function  Get_OutputOptions: TStOutputOption; safecall;
    procedure Set_OutputOptions(Value: TStOutputOption); safecall;
    function  Get_ReplacePattern: IStStringList; safecall;
    procedure Set_ReplacePattern(const Value: IStStringList); safecall;
    function  Get_SelAvoidPattern: IStStringList; safecall;
    procedure Set_SelAvoidPattern(const Value: IStStringList); safecall;
    function  CheckString(const S: WideString; var StartPos: Integer; var EndPos: Integer; 
                          var Length: Integer): WordBool; safecall;
    function  DOSMaskToRegEx(const Masks: WideString): WordBool; safecall;
    function  Execute: WordBool; safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    function  Get_Stream: OleVariant; safecall;
    procedure Set_Stream(Value: OleVariant); safecall;
    function  License(const Key: WideString): WordBool; safecall;
    property Avoid: WordBool read Get_Avoid write Set_Avoid;
    property IgnoreCase: WordBool read Get_IgnoreCase write Set_IgnoreCase;
    property InFixedLineLength: Integer read Get_InFixedLineLength write Set_InFixedLineLength;
    property InLineTermChar: WideString read Get_InLineTermChar write Set_InLineTermChar;
    property InLineTerminator: TStLineTerminator read Get_InLineTerminator write Set_InLineTerminator;
    property LineCount: Integer read Get_LineCount;
    property LineNumbers: WordBool read Get_LineNumbers write Set_LineNumbers;
    property LinesMatched: Integer read Get_LinesMatched;
    property LinesPerSecond: Integer read Get_LinesPerSecond;
    property LinesReplaced: Integer read Get_LinesReplaced;
    property LinesSelected: Integer read Get_LinesSelected;
    property MatchPattern: IStStringList read Get_MatchPattern write Set_MatchPattern;
    property OutFixedLineLength: Integer read Get_OutFixedLineLength write Set_OutFixedLineLength;
    property OutLineTermChar: WideString read Get_OutLineTermChar write Set_OutLineTermChar;
    property OutLineTerminitor: Integer read Get_OutLineTerminitor write Set_OutLineTerminitor;
    property OutputOptions: TStOutputOption read Get_OutputOptions write Set_OutputOptions;
    property ReplacePattern: IStStringList read Get_ReplacePattern write Set_ReplacePattern;
    property SelAvoidPattern: IStStringList read Get_SelAvoidPattern write Set_SelAvoidPattern;
    property Stream: OleVariant read Get_Stream write Set_Stream;
  end;

// *********************************************************************//
// DispIntf:  IStRegExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6498218A-2A04-4B0F-AD99-AD1D551997E8}
// *********************************************************************//
  IStRegExDisp = dispinterface
    ['{6498218A-2A04-4B0F-AD99-AD1D551997E8}']
    property Avoid: WordBool dispid 1;
    property IgnoreCase: WordBool dispid 2;
    property InFixedLineLength: Integer dispid 3;
    property InLineTermChar: WideString dispid 4;
    property InLineTerminator: TStLineTerminator dispid 5;
    property LineCount: Integer readonly dispid 6;
    property LineNumbers: WordBool dispid 7;
    property LinesMatched: Integer readonly dispid 8;
    property LinesPerSecond: Integer readonly dispid 9;
    property LinesReplaced: Integer readonly dispid 10;
    property LinesSelected: Integer readonly dispid 11;
    property MatchPattern: IStStringList dispid 12;
    property OutFixedLineLength: Integer dispid 13;
    property OutLineTermChar: WideString dispid 14;
    property OutLineTerminitor: Integer dispid 15;
    property OutputOptions: TStOutputOption dispid 16;
    property ReplacePattern: IStStringList dispid 17;
    property SelAvoidPattern: IStStringList dispid 18;
    function  CheckString(const S: WideString; var StartPos: Integer; var EndPos: Integer; 
                          var Length: Integer): WordBool; dispid 19;
    function  DOSMaskToRegEx(const Masks: WideString): WordBool; dispid 20;
    function  Execute: WordBool; dispid 21;
    procedure LoadFromFile(const FileName: WideString); dispid 22;
    procedure SaveToFile(const FileName: WideString); dispid 24;
    property Stream: OleVariant dispid 23;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// DispIntf:  IStRegExEvents
// Flags:     (4096) Dispatchable
// GUID:      {25513E60-B981-49F8-AD17-E18E7358163A}
// *********************************************************************//
  IStRegExEvents = dispinterface
    ['{25513E60-B981-49F8-AD17-E18E7358163A}']
    procedure OnProgress(Percent: Integer); dispid 1;
  end;

// *********************************************************************//
// Interface: IStExpr
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4753F38-E937-4AFE-8D35-2210305B6D19}
// *********************************************************************//
  IStExpr = interface(IDispatch)
    ['{C4753F38-E937-4AFE-8D35-2210305B6D19}']
    function  AnalyzeExpr(const Expr: WideString): Double; safecall;
    function  License(const Key: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IStExprDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4753F38-E937-4AFE-8D35-2210305B6D19}
// *********************************************************************//
  IStExprDisp = dispinterface
    ['{C4753F38-E937-4AFE-8D35-2210305B6D19}']
    function  AnalyzeExpr(const Expr: WideString): Double; dispid 1;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// Interface: IStMime
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {96547B4C-ED9E-4844-8F74-5C9E0268F460}
// *********************************************************************//
  IStMime = interface(IDispatch)
    ['{96547B4C-ED9E-4844-8F74-5C9E0268F460}']
    function  Get_Attachments: IStStringList; safecall;
    function  Get_Boundry: WideString; safecall;
    procedure Set_Boundry(const Value: WideString); safecall;
    function  Get_Encoding: WideString; safecall;
    procedure Set_Encoding(const Value: WideString); safecall;
    function  Get_ContentDescription: WideString; safecall;
    procedure Set_ContentDescription(const Value: WideString); safecall;
    function  Get_ContentDisposition: WideString; safecall;
    procedure Set_ContentDisposition(const Value: WideString); safecall;
    function  Get_ContentType: WideString; safecall;
    procedure Set_ContentType(const Value: WideString); safecall;
    function  Get_Directory: WideString; safecall;
    procedure Set_Directory(const Value: WideString); safecall;
    function  Get_MimeHeaders: WordBool; safecall;
    procedure Set_MimeHeaders(Value: WordBool); safecall;
    function  Get_Stream: OleVariant; safecall;
    procedure Set_Stream(Value: OleVariant); safecall;
    procedure AddFileAttachment(const FileName: WideString); safecall;
    procedure AddStreamAttachment(Stream: OleVariant; const FileName: WideString); safecall;
    procedure ExtractAttachment(const Attachment: WideString); safecall;
    procedure ExtractAttachmentIndex(Index: Integer); safecall;
    procedure ExtractToStream(Index: Integer; Stream: OleVariant); safecall;
    procedure ExtractAttachments; safecall;
    procedure GetTag(const Description: WideString); safecall;
    function  License(const Key: WideString): WordBool; safecall;
    property Attachments: IStStringList read Get_Attachments;
    property Boundry: WideString read Get_Boundry write Set_Boundry;
    property Encoding: WideString read Get_Encoding write Set_Encoding;
    property ContentDescription: WideString read Get_ContentDescription write Set_ContentDescription;
    property ContentDisposition: WideString read Get_ContentDisposition write Set_ContentDisposition;
    property ContentType: WideString read Get_ContentType write Set_ContentType;
    property Directory: WideString read Get_Directory write Set_Directory;
    property MimeHeaders: WordBool read Get_MimeHeaders write Set_MimeHeaders;
    property Stream: OleVariant read Get_Stream write Set_Stream;
  end;

// *********************************************************************//
// DispIntf:  IStMimeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {96547B4C-ED9E-4844-8F74-5C9E0268F460}
// *********************************************************************//
  IStMimeDisp = dispinterface
    ['{96547B4C-ED9E-4844-8F74-5C9E0268F460}']
    property Attachments: IStStringList readonly dispid 3;
    property Boundry: WideString dispid 4;
    property Encoding: WideString dispid 5;
    property ContentDescription: WideString dispid 6;
    property ContentDisposition: WideString dispid 7;
    property ContentType: WideString dispid 8;
    property Directory: WideString dispid 9;
    property MimeHeaders: WordBool dispid 10;
    property Stream: OleVariant dispid 11;
    procedure AddFileAttachment(const FileName: WideString); dispid 12;
    procedure AddStreamAttachment(Stream: OleVariant; const FileName: WideString); dispid 13;
    procedure ExtractAttachment(const Attachment: WideString); dispid 14;
    procedure ExtractAttachmentIndex(Index: Integer); dispid 15;
    procedure ExtractToStream(Index: Integer; Stream: OleVariant); dispid 16;
    procedure ExtractAttachments; dispid 17;
    procedure GetTag(const Description: WideString); dispid 18;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// DispIntf:  IStMimeEvents
// Flags:     (4096) Dispatchable
// GUID:      {0D42F0C6-844D-4541-8BAD-E2BFB6BF6C25}
// *********************************************************************//
  IStMimeEvents = dispinterface
    ['{0D42F0C6-844D-4541-8BAD-E2BFB6BF6C25}']
    procedure OnProgress(Status: TStConvertState; PercentDone: Byte); dispid 1;
    procedure OnSaveAs(var FileName: WideString); dispid 2;
  end;

// *********************************************************************//
// Interface: IStRegINI
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {37198FBE-3932-4548-A6B1-31229E83F87B}
// *********************************************************************//
  IStRegINI = interface(IDispatch)
    ['{37198FBE-3932-4548-A6B1-31229E83F87B}']
    procedure Open(const RootName: WideString; IsIniFile: WordBool); safecall;
    function  Get_CurrentSubKey: WideString; safecall;
    procedure Set_CurrentSubKey(const Value: WideString); safecall;
    function  Get_IsIniFile: WordBool; safecall;
    procedure CreateKey(const KeyName: WideString); safecall;
    procedure DeleteKey(const KeyName: WideString; DeleteSubKeys: WordBool); safecall;
    procedure DeleteValue(const ValueName: WideString); safecall;
    function  Get_Primary: WideString; safecall;
    procedure Set_Primary(const Value: WideString); safecall;
    function  Get_SubKeys: IStRegINISubKeys; safecall;
    function  Get_Values: IStRegINIValues; safecall;
    function  KeyExists(const KeyName: WideString): WordBool; safecall;
    function  QueryKey: IStRegINIQueryKeyInfo; safecall;
    function  ReadBoolean(const ValueName: WideString; Default: WordBool): WordBool; safecall;
    function  ReadDate(const ValueName: WideString; Default: TDateTime): TDateTime; safecall;
    function  ReadDateTime(const ValueName: WideString; Default: TDateTime): TDateTime; safecall;
    function  ReadInteger(const ValueName: WideString; Default: Integer): Integer; safecall;
    function  ReadString(const ValueName: WideString; const Default: WideString): WideString; safecall;
    function  ReadTime(const ValueName: WideString; Default: TDateTime): TDateTime; safecall;
    procedure WriteBoolean(const ValueName: WideString; Value: WordBool); safecall;
    procedure WriteDate(const ValueName: WideString; Value: TDateTime); safecall;
    procedure WriteDateTime(const ValueName: WideString; Value: TDateTime); safecall;
    procedure WriteInteger(const ValueName: WideString; Value: Integer); safecall;
    procedure WriteString(const ValueName: WideString; const Value: WideString); safecall;
    procedure WriteTime(const ValueName: WideString; Value: TDateTime); safecall;
    function  License(const Key: WideString): WordBool; safecall;
    property CurrentSubKey: WideString read Get_CurrentSubKey write Set_CurrentSubKey;
    property IsIniFile: WordBool read Get_IsIniFile;
    property Primary: WideString read Get_Primary write Set_Primary;
    property SubKeys: IStRegINISubKeys read Get_SubKeys;
    property Values: IStRegINIValues read Get_Values;
  end;

// *********************************************************************//
// DispIntf:  IStRegINIDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {37198FBE-3932-4548-A6B1-31229E83F87B}
// *********************************************************************//
  IStRegINIDisp = dispinterface
    ['{37198FBE-3932-4548-A6B1-31229E83F87B}']
    procedure Open(const RootName: WideString; IsIniFile: WordBool); dispid 1;
    property CurrentSubKey: WideString dispid 2;
    property IsIniFile: WordBool readonly dispid 3;
    procedure CreateKey(const KeyName: WideString); dispid 6;
    procedure DeleteKey(const KeyName: WideString; DeleteSubKeys: WordBool); dispid 8;
    procedure DeleteValue(const ValueName: WideString); dispid 9;
    property Primary: WideString dispid 10;
    property SubKeys: IStRegINISubKeys readonly dispid 11;
    property Values: IStRegINIValues readonly dispid 12;
    function  KeyExists(const KeyName: WideString): WordBool; dispid 13;
    function  QueryKey: IStRegINIQueryKeyInfo; dispid 14;
    function  ReadBoolean(const ValueName: WideString; Default: WordBool): WordBool; dispid 16;
    function  ReadDate(const ValueName: WideString; Default: TDateTime): TDateTime; dispid 17;
    function  ReadDateTime(const ValueName: WideString; Default: TDateTime): TDateTime; dispid 18;
    function  ReadInteger(const ValueName: WideString; Default: Integer): Integer; dispid 19;
    function  ReadString(const ValueName: WideString; const Default: WideString): WideString; dispid 21;
    function  ReadTime(const ValueName: WideString; Default: TDateTime): TDateTime; dispid 22;
    procedure WriteBoolean(const ValueName: WideString; Value: WordBool); dispid 24;
    procedure WriteDate(const ValueName: WideString; Value: TDateTime); dispid 25;
    procedure WriteDateTime(const ValueName: WideString; Value: TDateTime); dispid 26;
    procedure WriteInteger(const ValueName: WideString; Value: Integer); dispid 28;
    procedure WriteString(const ValueName: WideString; const Value: WideString); dispid 29;
    procedure WriteTime(const ValueName: WideString; Value: TDateTime); dispid 30;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// Interface: IStFin
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {821B289E-492C-4115-8484-A6545E281BB3}
// *********************************************************************//
  IStFin = interface(IDispatch)
    ['{821B289E-492C-4115-8484-A6545E281BB3}']
    function  IsCardValid(const S: WideString): WordBool; safecall;
    function  License(const Key: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IStFinDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {821B289E-492C-4115-8484-A6545E281BB3}
// *********************************************************************//
  IStFinDisp = dispinterface
    ['{821B289E-492C-4115-8484-A6545E281BB3}']
    function  IsCardValid(const S: WideString): WordBool; dispid 1;
    function  License(const Key: WideString): WordBool; dispid 200;
  end;

// *********************************************************************//
// Interface: IStRegINIQueryKeyInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DF09E023-FC82-4BC0-BEA8-AA5D5C0E9F2A}
// *********************************************************************//
  IStRegINIQueryKeyInfo = interface(IDispatch)
    ['{DF09E023-FC82-4BC0-BEA8-AA5D5C0E9F2A}']
    function  Get_QIClassName: WideString; safecall;
    function  Get_QIFileTime: TDateTime; safecall;
    function  Get_QIKey: Integer; safecall;
    function  Get_QIMaxCNLen: Integer; safecall;
    function  Get_QIMaxDataLen: Integer; safecall;
    function  Get_QIMaxSKNLen: Integer; safecall;
    function  Get_QIMaxVNLen: Integer; safecall;
    function  Get_QINumSubKeys: Integer; safecall;
    function  Get_QINumValues: Integer; safecall;
    function  Get_QISDescLen: Integer; safecall;
    property QIClassName: WideString read Get_QIClassName;
    property QIFileTime: TDateTime read Get_QIFileTime;
    property QIKey: Integer read Get_QIKey;
    property QIMaxCNLen: Integer read Get_QIMaxCNLen;
    property QIMaxDataLen: Integer read Get_QIMaxDataLen;
    property QIMaxSKNLen: Integer read Get_QIMaxSKNLen;
    property QIMaxVNLen: Integer read Get_QIMaxVNLen;
    property QINumSubKeys: Integer read Get_QINumSubKeys;
    property QINumValues: Integer read Get_QINumValues;
    property QISDescLen: Integer read Get_QISDescLen;
  end;

// *********************************************************************//
// DispIntf:  IStRegINIQueryKeyInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DF09E023-FC82-4BC0-BEA8-AA5D5C0E9F2A}
// *********************************************************************//
  IStRegINIQueryKeyInfoDisp = dispinterface
    ['{DF09E023-FC82-4BC0-BEA8-AA5D5C0E9F2A}']
    property QIClassName: WideString readonly dispid 1;
    property QIFileTime: TDateTime readonly dispid 2;
    property QIKey: Integer readonly dispid 3;
    property QIMaxCNLen: Integer readonly dispid 4;
    property QIMaxDataLen: Integer readonly dispid 5;
    property QIMaxSKNLen: Integer readonly dispid 6;
    property QIMaxVNLen: Integer readonly dispid 7;
    property QINumSubKeys: Integer readonly dispid 8;
    property QINumValues: Integer readonly dispid 9;
    property QISDescLen: Integer readonly dispid 10;
  end;

// *********************************************************************//
// Interface: IStRegINISubKeys
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {43B798DA-D2CA-4263-854D-E63FBB2F7C64}
// *********************************************************************//
  IStRegINISubKeys = interface(IDispatch)
    ['{43B798DA-D2CA-4263-854D-E63FBB2F7C64}']
    function  Get__NewEnum: IUnknown; safecall;
    function  Get_Item(Index: Integer): IStRegINISubKey; safecall;
    function  Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: Integer]: IStRegINISubKey read Get_Item; default;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IStRegINISubKeysDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {43B798DA-D2CA-4263-854D-E63FBB2F7C64}
// *********************************************************************//
  IStRegINISubKeysDisp = dispinterface
    ['{43B798DA-D2CA-4263-854D-E63FBB2F7C64}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Index: Integer]: IStRegINISubKey readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IStRegINIValue
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A3E5FB23-CD5C-471B-8214-5075375DE8DF}
// *********************************************************************//
  IStRegINIValue = interface(IDispatch)
    ['{A3E5FB23-CD5C-471B-8214-5075375DE8DF}']
    function  Get_Value: WideString; safecall;
    property Value: WideString read Get_Value;
  end;

// *********************************************************************//
// DispIntf:  IStRegINIValueDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A3E5FB23-CD5C-471B-8214-5075375DE8DF}
// *********************************************************************//
  IStRegINIValueDisp = dispinterface
    ['{A3E5FB23-CD5C-471B-8214-5075375DE8DF}']
    property Value: WideString readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IStRegINIValues
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FC9E57F0-ABC0-4B69-80A0-06CDACB61AEB}
// *********************************************************************//
  IStRegINIValues = interface(IDispatch)
    ['{FC9E57F0-ABC0-4B69-80A0-06CDACB61AEB}']
    function  Get__NewEnum: IUnknown; safecall;
    function  Get_Item(Index: Integer): IStRegINIValue; safecall;
    function  Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: Integer]: IStRegINIValue read Get_Item; default;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IStRegINIValuesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FC9E57F0-ABC0-4B69-80A0-06CDACB61AEB}
// *********************************************************************//
  IStRegINIValuesDisp = dispinterface
    ['{FC9E57F0-ABC0-4B69-80A0-06CDACB61AEB}']
    property _NewEnum: IUnknown readonly dispid -4;
    property Item[Index: Integer]: IStRegINIValue readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
  end;

// *********************************************************************//
// Interface: IStRegINISubKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BBA14CDE-6BDE-4E40-86DD-E04697595AF3}
// *********************************************************************//
  IStRegINISubKey = interface(IDispatch)
    ['{BBA14CDE-6BDE-4E40-86DD-E04697595AF3}']
    function  Get_Value: WideString; safecall;
    property Value: WideString read Get_Value;
  end;

// *********************************************************************//
// DispIntf:  IStRegINISubKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BBA14CDE-6BDE-4E40-86DD-E04697595AF3}
// *********************************************************************//
  IStRegINISubKeyDisp = dispinterface
    ['{BBA14CDE-6BDE-4E40-86DD-E04697595AF3}']
    property Value: WideString readonly dispid 0;
  end;

// *********************************************************************//
// The Class CoStDate provides a Create and CreateRemote method to          
// create instances of the default interface IStDate exposed by              
// the CoClass StDate. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStDate = class
    class function Create: IStDate;
    class function CreateRemote(const MachineName: string): IStDate;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStDate
// Help String      : StDate Object
// Default Interface: IStDate
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStDateProperties= class;
{$ENDIF}
  TStDate = class(TOleServer)
  private
    FIntf:        IStDate;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStDateProperties;
    function      GetServerProperties: TStDateProperties;
{$ENDIF}
    function      GetDefaultInterface: IStDate;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStDate);
    procedure Disconnect; override;
    function  AstJulianDate(Julian: TDateTime): Double;
    function  AstJulianDatePrim(Year: Integer; Month: Integer; Day: Integer; UT: TDateTime): Double;
    function  AstJulianDateToStDate(AstJD: Double; Truncate: WordBool): TDateTime;
    function  BondDateDiff(Date1: TDateTime; Date2: TDateTime; DayBasis: TStBondDateType): TDateTime;
    function  CurrentDate: TDateTime;
    function  CurrentDateString(const Picture: WideString; Pack: WordBool): WideString;
    function  CurrentTime: TDateTime;
    function  CurrentTimeString(const Picture: WideString; Pack: WordBool): WideString;
    procedure DateDiff(Date1: TDateTime; Date2: TDateTime; var Days: Integer; var Months: Integer; 
                       var Years: Integer);
    function  DateStringHMStoAstJD(const Picture: WideString; const DS: WideString; Hours: Integer; 
                                   Minutes: Integer; Seconds: Integer; Epoch: Integer): Double;
    function  DateStringToDMY(const Picture: WideString; const S: WideString; var Day: Integer; 
                              var Month: Integer; var Year: Integer; Epoch: Integer): WordBool;
    function  DateStringToStDate(const Picture: WideString; const S: WideString; Epoch: Integer): TDateTime;
    procedure DateTimeDiff(DT1: TDateTime; DT2: TDateTime; var Days: Integer; var Seconds: Integer);
    function  DayOfWeek(Julian: TDateTime): TStDayType;
    function  DayOfWeekDMY(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TStDayType;
    function  DayOfWeekToString(WeekDay: TStDayType): WideString;
    function  DaysInMonth(Month: Integer; Year: Integer; Epoch: Integer): Integer;
    function  DecTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime;
    function  DMYtoStDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TDateTime;
    function  DMYtoDateString(const Picture: WideString; Day: Integer; Month: Integer; 
                              Year: Integer; Epoch: Integer; Pack: WordBool): WideString;
    function  HMStoStTime(Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime;
    function  IncDate(Julian: TDateTime; Days: Integer; Months: Integer; Years: Integer): TDateTime;
    procedure IncDateTime(DT1: TDateTime; var DT2: TDateTime; Days: Integer; Seconds: Integer);
    function  IncDateTrunc(Julian: TDateTime; Months: Integer; Years: Integer): TDateTime;
    function  IncTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime;
    function  InternationalDate(ForceCentury: WordBool): WideString;
    function  InternationalLongDate(ShortNames: WordBool; ExcludeDOW: WordBool): WideString;
    function  InternationalTime(ShowSeconds: WordBool): WideString;
    function  IsLeapYear(Year: Integer): WordBool;
    function  MonthToString(Month: Integer): WideString;
    function  RoundToNearestHour(T: TDateTime; Truncate: WordBool): TDateTime;
    function  RoundToNearestMinute(T: TDateTime; Truncate: WordBool): TDateTime;
    function  StDateToDateString(const Picture: WideString; Julian: TDateTime; Pack: WordBool): WideString;
    procedure StDateToDMY(Julian: TDateTime; var Day: Integer; var Month: Integer; var Year: Integer);
    function  StTimeToAmPmString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString;
    procedure StTimeToHMS(T: TDateTime; var Hours: Byte; var Minutes: Byte; var Seconds: Byte);
    function  StTimeToTimeString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString;
    procedure TimeDiff(T1: TDateTime; T2: TDateTime; var Hours: Byte; var Minutes: Byte; 
                       var Seconds: Byte);
    function  TimeStringToHMS(const Picture: WideString; const TS: WideString; var Hours: Integer; 
                              var Minutes: Integer; var Seconds: Integer): WordBool;
    function  TimeStringToStTime(const Picture: WideString; const S: WideString): TDateTime;
    function  ValidDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): WordBool;
    function  ValidTime(Hours: Integer; Minutes: Integer; Seconds: Integer): WordBool;
    function  WeekOfYear(Julian: TDateTime): Byte;
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStDate read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStDateProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStDate
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStDateProperties = class(TPersistent)
  private
    FServer:    TStDate;
    function    GetDefaultInterface: IStDate;
    constructor Create(AServer: TStDate);
  protected
  public
    property DefaultInterface: IStDate read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStString provides a Create and CreateRemote method to          
// create instances of the default interface IStString exposed by              
// the CoClass StString. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStString = class
    class function Create: IStString;
    class function CreateRemote(const MachineName: string): IStString;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStString
// Help String      : StString Object
// Default Interface: IStString
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStStringProperties= class;
{$ENDIF}
  TStString = class(TOleServer)
  private
    FIntf:        IStString;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStStringProperties;
    function      GetServerProperties: TStStringProperties;
{$ENDIF}
    function      GetDefaultInterface: IStString;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStString);
    procedure Disconnect; override;
    function  AddBackSlash(const DirName: WideString): WideString;
    function  AsciiCount(const S: WideString; const WordDelims: WideString; const Quote: WideString): Integer;
    function  AsciiPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                            const Quote: WideString; var Pos: Integer): WordBool;
    function  BinaryB(B: Byte): WideString;
    function  BinaryW(W: Integer): WideString;
    function  BinaryL(L: Integer): WideString;
    function  Center(const S: WideString; Len: Integer): WideString;
    function  CenterCh(const S: WideString; const C: WideString; Len: Integer): WideString;
    function  CharCount(const S: WideString; const C: WideString): Integer;
    function  CharExists(const S: WideString; const C: WideString): WordBool;
    function  CharStr(const C: WideString; Len: Integer): WideString;
    function  CleanPathName(const PathName: WideString): WideString;
    function  Commaize(L: Integer): WideString;
    function  CommaizeCh(L: Integer; const Ch: WideString): WideString;
    function  CompString(const S1: WideString; const S2: WideString): Integer;
    function  CompUCString(const S1: WideString; const S2: WideString): Integer;
    function  ContainsOnly(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool;
    function  ContainsOtherThan(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool;
    function  CopyFromNthWord(const S: WideString; const WordDelims: WideString; 
                              const AWord: WideString; N: Integer; var SubString: WideString): WordBool;
    function  CopyFromToWord(const S: WideString; const WordDelims: WideString; 
                             const Word1: WideString; const Word2: WideString; N1: Integer; 
                             N2: Integer; var SubString: WideString): WordBool;
    function  CopyLeft(const S: WideString; Len: Integer): WideString;
    function  CopyMid(const S: WideString; First: Integer; Len: Integer): WideString;
    function  CopyRight(const S: WideString; First: Integer): WideString;
    function  CopyWithin(const S: WideString; const Delimiter: WideString; Strip: WordBool): WideString;
    function  DefaultExtension(const Name: WideString; const Ext: WideString): WideString;
    function  DeleteFromNthWord(const S: WideString; const WordDelims: WideString; 
                                const AWord: WideString; N: Integer; var SubString: WideString): WordBool;
    function  DeleteFromToWord(const S: WideString; const WordDelims: WideString; 
                               const Word1: WideString; const Word2: WideString; N1: Integer; 
                               N2: Integer; var SubString: WideString): WordBool;
    function  DeleteWithin(const S: WideString; const Delimeter: WideString): WideString;
    function  Detab(const S: WideString; TabSize: Byte): WideString;
    function  Entab(const S: WideString; TabSize: Byte): WideString;
    function  Ext2Str(R: OleVariant; Width: Byte; Places: Byte): WideString;
    function  ExtractAscii(N: Integer; const S: WideString; const WordDelims: WideString; 
                           const Quote: WideString): WideString;
    function  ExtractTokens(const S: WideString; const Delims: WideString; 
                            const QuoteChar: WideString; AllowNulls: WordBool; 
                            out Tokens: IStStringList): Integer;
    function  ExtractWord(N: Integer; const S: WideString; const WordDelims: WideString): WideString;
    function  Filter(const S: WideString; const Filters: WideString): WideString;
    function  FloatForm(const Mask: WideString; R: Double; L: Integer; const LtCurr: WideString; 
                        const RtCurr: WideString; const Sep: WideString; const DecPt: WideString): WideString;
    function  ForceExtension(const Name: WideString; const Ext: WideString): WideString;
    function  HasExtension(const Name: WideString; var DotPos: Integer): WordBool;
    function  HexB(B: Byte): WideString;
    function  HexW(W: Integer): WideString;
    function  HexL(L: Integer): WideString;
    function  IsChAlpha(const C: WideString): WordBool;
    function  IsChAlphaNumeric(const C: WideString; const Numbers: WideString): WordBool;
    function  IsChNumeric(const C: WideString; const Numbers: WideString): WordBool;
    function  IsStrAlpha(const S: WideString): WordBool;
    function  IsStrAlphaNumeric(const S: WideString; const Numbers: WideString): WordBool;
    function  IsStrNumeric(const S: WideString; const Numbers: WideString): WordBool;
    function  JustExtension(const Name: WideString): WideString;
    function  JustFilename(const PathName: WideString): WideString;
    function  JustName(const PathName: WideString): WideString;
    function  JustPathname(const PathName: WideString): WideString;
    function  KeepChars(const S: WideString; const Chars: WideString): WideString;
    function  LastString(const S: WideString; const AString: WideString; var Position: Integer): WordBool;
    function  LastWord(const S: WideString; const WordDelims: WideString; const AWord: WideString; 
                       var Position: Integer): WordBool;
    function  LastWordAbs(const S: WideString; const WordDelims: WideString; var Position: Integer): WordBool;
    function  LeftPad(const S: WideString; Len: Integer): WideString;
    function  LeftPadCh(const S: WideString; const C: WideString; Len: Integer): WideString;
    function  LeftTrimChars(const S: WideString; const Chars: WideString): WideString;
    function  Long2Str(L: Integer): WideString;
    function  LongIntForm(const Mask: WideString; L: Integer; const LtCurr: WideString; 
                          const RtCurr: WideString; const Sep: WideString): WideString;
    function  OctalB(B: Byte): WideString;
    function  OctalW(W: Integer): WideString;
    function  OctalL(L: Integer): WideString;
    function  Pad(const S: WideString; Len: Integer): WideString;
    function  PadCh(const S: WideString; const C: WideString; Len: Integer): WideString;
    function  Real2Str(R: Double; Width: Byte; Places: Byte): WideString;
    function  RepeatString(const S: WideString; var Repetitions: Integer; MaxLen: Integer): WideString;
    function  ReplaceWord(const S: WideString; const WordDelims: WideString; 
                          const OldWord: WideString; const NewWord: WideString; N: Integer; 
                          var Replacements: Integer): WideString;
    function  ReplaceWordAll(const S: WideString; const WordDelims: WideString; 
                             const OldWord: WideString; const NewWord: WideString; 
                             var Replacements: Integer): WideString;
    function  ReplaceString(const S: WideString; const OldString: WideString; 
                            const NewString: WideString; N: Integer; var Replacements: Integer): WideString;
    function  ReplaceStringAll(const S: WideString; const OldString: WideString; 
                               const NewString: WideString; var Replacements: Integer): WideString;
    function  RightTrimChars(const S: WideString; const Chars: WideString): WideString;
    function  Scramble(const S: WideString; const Key: WideString): WideString;
    function  Str2Ext(const S: WideString; var R: OleVariant): WordBool;
    function  Str2Int16(const S: WideString; var I: Smallint): WordBool;
    function  Str2Long(const S: WideString; var I: Integer): WordBool;
    function  Str2Real(const S: WideString; var R: Double): WordBool;
    function  Str2Word(const S: WideString; var W: Integer): WordBool;
    function  StrChDelete(const S: WideString; Pos: Integer): WideString;
    function  StrChInsert(const S: WideString; const C: WideString; Pos: Integer): WideString;
    function  StrChPos(const P: WideString; const C: WideString; var Pos: Integer): WordBool;
    function  StrStCopy(const S: WideString; Pos: Integer; Count: Integer): WideString;
    function  StrStDelete(const S: WideString; Pos: Integer; Count: Integer): WideString;
    function  StrStInsert(const S1: WideString; const S2: WideString; Pos: Integer): WideString;
    function  StrStPos(const P: WideString; const S: WideString; var Pos: Integer): WordBool;
    function  StrWithin(const S: WideString; const SearchStr: WideString; Start: Integer; 
                        var Position: Integer): WordBool;
    function  Substitute(const S: WideString; const FromStr: WideString; const ToStr: WideString): WideString;
    function  Trim(const S: WideString): WideString;
    function  TrimChars(const S: WideString; const Chars: WideString): WideString;
    function  TrimLead(const S: WideString): WideString;
    function  TrimSpaces(const S: WideString): WideString;
    function  TrimTrail(const S: WideString): WideString;
    function  ValPrep(const S: WideString): WideString;
    function  WordCount(const S: WideString; const WordDelims: WideString): Integer;
    function  WordPos(const S: WideString; const WordDelims: WideString; const AWord: WideString; 
                      N: Integer; var Position: Integer): WordBool;
    function  WordPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                           var Position: Integer): WordBool;
    procedure WordWrap(const InSt: WideString; var OutSt: WideString; var Overlap: WideString; 
                       Margin: Integer; PadToMArgin: WordBool);
    function  License(const Key: WideString): WordBool;
    function  Soundex(const S: WideString): WideString;
    property  DefaultInterface: IStString read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStStringProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStString
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStStringProperties = class(TPersistent)
  private
    FServer:    TStString;
    function    GetDefaultInterface: IStString;
    constructor Create(AServer: TStString);
  protected
  public
    property DefaultInterface: IStString read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStToHTML provides a Create and CreateRemote method to          
// create instances of the default interface IStToHTML exposed by              
// the CoClass StToHTML. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStToHTML = class
    class function Create: IStToHTML;
    class function CreateRemote(const MachineName: string): IStToHTML;
  end;

  TStToHTMLOnProgress = procedure(Sender: TObject; Percent: Integer) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStToHTML
// Help String      : StToHTML Object
// Default Interface: IStToHTML
// Def. Intf. DISP? : No
// Event   Interface: IStToHTMLEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStToHTMLProperties= class;
{$ENDIF}
  TStToHTML = class(TOleServer)
  private
    FOnProgress: TStToHTMLOnProgress;
    FIntf:        IStToHTML;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStToHTMLProperties;
    function      GetServerProperties: TStToHTMLProperties;
{$ENDIF}
    function      GetDefaultInterface: IStToHTML;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_CaseSensitve: WordBool;
    procedure Set_CaseSensitve(Value: WordBool);
    function  Get_CommentMarkers: IStStringList;
    procedure Set_CommentMarkers(const Value: IStStringList);
    function  Get_EmbeddedHTML: IStStringList;
    procedure Set_EmbeddedHTML(const Value: IStStringList);
    function  Get_FixedLineLength: Integer;
    procedure Set_FixedLineLength(Value: Integer);
    function  Get_Keywords: IStStringList;
    procedure Set_Keywords(const Value: IStStringList);
    function  Get_LineTermChar: WideString;
    procedure Set_LineTermChar(const Value: WideString);
    function  Get_LineTerminator: TStLineTerminator;
    procedure Set_LineTerminator(Value: TStLineTerminator);
    function  Get_PageFooter: IStStringList;
    procedure Set_PageFooter(const Value: IStStringList);
    function  Get_PageHeader: IStStringList;
    procedure Set_PageHeader(const Value: IStStringList);
    function  Get_StringMarkers: IStStringList;
    procedure Set_StringMarkers(const Value: IStStringList);
    function  Get_WordDelimeters: WideString;
    procedure Set_WordDelimeters(const Value: WideString);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStToHTML);
    procedure Disconnect; override;
    procedure GenerateHTML;
    procedure LoadFromFile(const FileName: WideString);
    procedure SaveToFile(const FileName: WideString);
    procedure Clear;
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStToHTML read GetDefaultInterface;
    property Stream: OleVariant read Get_Stream write Set_Stream;
    property CaseSensitve: WordBool read Get_CaseSensitve write Set_CaseSensitve;
    property CommentMarkers: IStStringList read Get_CommentMarkers write Set_CommentMarkers;
    property EmbeddedHTML: IStStringList read Get_EmbeddedHTML write Set_EmbeddedHTML;
    property FixedLineLength: Integer read Get_FixedLineLength write Set_FixedLineLength;
    property Keywords: IStStringList read Get_Keywords write Set_Keywords;
    property LineTermChar: WideString read Get_LineTermChar write Set_LineTermChar;
    property LineTerminator: TStLineTerminator read Get_LineTerminator write Set_LineTerminator;
    property PageFooter: IStStringList read Get_PageFooter write Set_PageFooter;
    property PageHeader: IStStringList read Get_PageHeader write Set_PageHeader;
    property StringMarkers: IStStringList read Get_StringMarkers write Set_StringMarkers;
    property WordDelimeters: WideString read Get_WordDelimeters write Set_WordDelimeters;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStToHTMLProperties read GetServerProperties;
{$ENDIF}
    property OnProgress: TStToHTMLOnProgress read FOnProgress write FOnProgress;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStToHTML
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStToHTMLProperties = class(TPersistent)
  private
    FServer:    TStToHTML;
    function    GetDefaultInterface: IStToHTML;
    constructor Create(AServer: TStToHTML);
  protected
    function  Get_CaseSensitve: WordBool;
    procedure Set_CaseSensitve(Value: WordBool);
    function  Get_CommentMarkers: IStStringList;
    procedure Set_CommentMarkers(const Value: IStStringList);
    function  Get_EmbeddedHTML: IStStringList;
    procedure Set_EmbeddedHTML(const Value: IStStringList);
    function  Get_FixedLineLength: Integer;
    procedure Set_FixedLineLength(Value: Integer);
    function  Get_Keywords: IStStringList;
    procedure Set_Keywords(const Value: IStStringList);
    function  Get_LineTermChar: WideString;
    procedure Set_LineTermChar(const Value: WideString);
    function  Get_LineTerminator: TStLineTerminator;
    procedure Set_LineTerminator(Value: TStLineTerminator);
    function  Get_PageFooter: IStStringList;
    procedure Set_PageFooter(const Value: IStStringList);
    function  Get_PageHeader: IStStringList;
    procedure Set_PageHeader(const Value: IStStringList);
    function  Get_StringMarkers: IStStringList;
    procedure Set_StringMarkers(const Value: IStStringList);
    function  Get_WordDelimeters: WideString;
    procedure Set_WordDelimeters(const Value: WideString);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    property DefaultInterface: IStToHTML read GetDefaultInterface;
  published
    property CaseSensitve: WordBool read Get_CaseSensitve write Set_CaseSensitve;
    property CommentMarkers: IStStringList read Get_CommentMarkers write Set_CommentMarkers;
    property EmbeddedHTML: IStStringList read Get_EmbeddedHTML write Set_EmbeddedHTML;
    property FixedLineLength: Integer read Get_FixedLineLength write Set_FixedLineLength;
    property Keywords: IStStringList read Get_Keywords write Set_Keywords;
    property LineTermChar: WideString read Get_LineTermChar write Set_LineTermChar;
    property LineTerminator: TStLineTerminator read Get_LineTerminator write Set_LineTerminator;
    property PageFooter: IStStringList read Get_PageFooter write Set_PageFooter;
    property PageHeader: IStStringList read Get_PageHeader write Set_PageHeader;
    property StringMarkers: IStStringList read Get_StringMarkers write Set_StringMarkers;
    property WordDelimeters: WideString read Get_WordDelimeters write Set_WordDelimeters;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStStringList provides a Create and CreateRemote method to          
// create instances of the default interface IStStringList exposed by              
// the CoClass StStringList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStStringList = class
    class function Create: IStStringList;
    class function CreateRemote(const MachineName: string): IStStringList;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStStringList
// Help String      : StStringList Object
// Default Interface: IStStringList
// Def. Intf. DISP? : No
// Event   Interface: IStStringListEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStStringListProperties= class;
{$ENDIF}
  TStStringList = class(TOleServer)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FIntf:        IStStringList;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStStringListProperties;
    function      GetServerProperties: TStStringListProperties;
{$ENDIF}
    function      GetDefaultInterface: IStStringList;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get__NewEnum: IUnknown;
    function  Get_Item(Index: Integer): WideString;
    procedure Set_Item(Index: Integer; const Value: WideString);
    function  Get_CommaText: WideString;
    procedure Set_CommaText(const Value: WideString);
    function  Get_Count: Integer;
    function  Get_Duplicates: Integer;
    procedure Set_Duplicates(Value: Integer);
    function  Get_Names(Index: Integer): WideString;
    function  Get_Sorted: WordBool;
    procedure Set_Sorted(Value: WordBool);
    function  Get_Strings(Index: Integer): WideString;
    procedure Set_Strings(Index: Integer; const Value: WideString);
    function  Get_Text: WideString;
    procedure Set_Text(const Value: WideString);
    function  Get_Values(const Name: WideString): WideString;
    procedure Set_Values(const Name: WideString; const Value: WideString);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStStringList);
    procedure Disconnect; override;
    function  Add(const S: WideString): Integer;
    procedure Append(const S: WideString);
    procedure Clear;
    procedure Delete(Index: Integer);
    function  Equals(const Strings: IStStringList): WordBool;
    procedure Exchange(Index1: Integer; Index2: Integer);
    function  Find(const S: WideString; var Index: Integer): WordBool;
    function  IndexOf(const S: WideString): Integer;
    function  IndexOfName(const Name: WideString): Integer;
    procedure Insert(Index: Integer; const S: WideString);
    procedure LoadFromFile(const FileName: WideString);
    procedure Move(CurIndex: Integer; NewIndex: Integer);
    procedure SaveToFile(const FileName: WideString);
    procedure Sort;
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStStringList read GetDefaultInterface;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: Integer]: WideString read Get_Item write Set_Item; default;
    property Count: Integer read Get_Count;
    property Names[Index: Integer]: WideString read Get_Names;
    property Strings[Index: Integer]: WideString read Get_Strings write Set_Strings;
    property Values[const Name: WideString]: WideString read Get_Values write Set_Values;
    property Stream: OleVariant read Get_Stream write Set_Stream;
    property CommaText: WideString read Get_CommaText write Set_CommaText;
    property Duplicates: Integer read Get_Duplicates write Set_Duplicates;
    property Sorted: WordBool read Get_Sorted write Set_Sorted;
    property Text: WideString read Get_Text write Set_Text;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStStringListProperties read GetServerProperties;
{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStStringList
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStStringListProperties = class(TPersistent)
  private
    FServer:    TStStringList;
    function    GetDefaultInterface: IStStringList;
    constructor Create(AServer: TStStringList);
  protected
    function  Get__NewEnum: IUnknown;
    function  Get_Item(Index: Integer): WideString;
    procedure Set_Item(Index: Integer; const Value: WideString);
    function  Get_CommaText: WideString;
    procedure Set_CommaText(const Value: WideString);
    function  Get_Count: Integer;
    function  Get_Duplicates: Integer;
    procedure Set_Duplicates(Value: Integer);
    function  Get_Names(Index: Integer): WideString;
    function  Get_Sorted: WordBool;
    procedure Set_Sorted(Value: WordBool);
    function  Get_Strings(Index: Integer): WideString;
    procedure Set_Strings(Index: Integer; const Value: WideString);
    function  Get_Text: WideString;
    procedure Set_Text(const Value: WideString);
    function  Get_Values(const Name: WideString): WideString;
    procedure Set_Values(const Name: WideString; const Value: WideString);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    property DefaultInterface: IStStringList read GetDefaultInterface;
  published
    property CommaText: WideString read Get_CommaText write Set_CommaText;
    property Duplicates: Integer read Get_Duplicates write Set_Duplicates;
    property Sorted: WordBool read Get_Sorted write Set_Sorted;
    property Text: WideString read Get_Text write Set_Text;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegEx provides a Create and CreateRemote method to          
// create instances of the default interface IStRegEx exposed by              
// the CoClass StRegEx. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegEx = class
    class function Create: IStRegEx;
    class function CreateRemote(const MachineName: string): IStRegEx;
  end;

  TStRegExOnProgress = procedure(Sender: TObject; Percent: Integer) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegEx
// Help String      : StRegEx Object
// Default Interface: IStRegEx
// Def. Intf. DISP? : No
// Event   Interface: IStRegExEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegExProperties= class;
{$ENDIF}
  TStRegEx = class(TOleServer)
  private
    FOnProgress: TStRegExOnProgress;
    FIntf:        IStRegEx;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegExProperties;
    function      GetServerProperties: TStRegExProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegEx;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_Avoid: WordBool;
    procedure Set_Avoid(Value: WordBool);
    function  Get_IgnoreCase: WordBool;
    procedure Set_IgnoreCase(Value: WordBool);
    function  Get_InFixedLineLength: Integer;
    procedure Set_InFixedLineLength(Value: Integer);
    function  Get_InLineTermChar: WideString;
    procedure Set_InLineTermChar(const Value: WideString);
    function  Get_InLineTerminator: TStLineTerminator;
    procedure Set_InLineTerminator(Value: TStLineTerminator);
    function  Get_LineCount: Integer;
    function  Get_LineNumbers: WordBool;
    procedure Set_LineNumbers(Value: WordBool);
    function  Get_LinesMatched: Integer;
    function  Get_LinesPerSecond: Integer;
    function  Get_LinesReplaced: Integer;
    function  Get_LinesSelected: Integer;
    function  Get_MatchPattern: IStStringList;
    procedure Set_MatchPattern(const Value: IStStringList);
    function  Get_OutFixedLineLength: Integer;
    procedure Set_OutFixedLineLength(Value: Integer);
    function  Get_OutLineTermChar: WideString;
    procedure Set_OutLineTermChar(const Value: WideString);
    function  Get_OutLineTerminitor: Integer;
    procedure Set_OutLineTerminitor(Value: Integer);
    function  Get_OutputOptions: TStOutputOption;
    procedure Set_OutputOptions(Value: TStOutputOption);
    function  Get_ReplacePattern: IStStringList;
    procedure Set_ReplacePattern(const Value: IStStringList);
    function  Get_SelAvoidPattern: IStStringList;
    procedure Set_SelAvoidPattern(const Value: IStStringList);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegEx);
    procedure Disconnect; override;
    function  CheckString(const S: WideString; var StartPos: Integer; var EndPos: Integer; 
                          var Length: Integer): WordBool;
    function  DOSMaskToRegEx(const Masks: WideString): WordBool;
    function  Execute: WordBool;
    procedure LoadFromFile(const FileName: WideString);
    procedure SaveToFile(const FileName: WideString);
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStRegEx read GetDefaultInterface;
    property LineCount: Integer read Get_LineCount;
    property LinesMatched: Integer read Get_LinesMatched;
    property LinesPerSecond: Integer read Get_LinesPerSecond;
    property LinesReplaced: Integer read Get_LinesReplaced;
    property LinesSelected: Integer read Get_LinesSelected;
    property Stream: OleVariant read Get_Stream write Set_Stream;
    property Avoid: WordBool read Get_Avoid write Set_Avoid;
    property IgnoreCase: WordBool read Get_IgnoreCase write Set_IgnoreCase;
    property InFixedLineLength: Integer read Get_InFixedLineLength write Set_InFixedLineLength;
    property InLineTermChar: WideString read Get_InLineTermChar write Set_InLineTermChar;
    property InLineTerminator: TStLineTerminator read Get_InLineTerminator write Set_InLineTerminator;
    property LineNumbers: WordBool read Get_LineNumbers write Set_LineNumbers;
    property MatchPattern: IStStringList read Get_MatchPattern write Set_MatchPattern;
    property OutFixedLineLength: Integer read Get_OutFixedLineLength write Set_OutFixedLineLength;
    property OutLineTermChar: WideString read Get_OutLineTermChar write Set_OutLineTermChar;
    property OutLineTerminitor: Integer read Get_OutLineTerminitor write Set_OutLineTerminitor;
    property OutputOptions: TStOutputOption read Get_OutputOptions write Set_OutputOptions;
    property ReplacePattern: IStStringList read Get_ReplacePattern write Set_ReplacePattern;
    property SelAvoidPattern: IStStringList read Get_SelAvoidPattern write Set_SelAvoidPattern;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegExProperties read GetServerProperties;
{$ENDIF}
    property OnProgress: TStRegExOnProgress read FOnProgress write FOnProgress;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegEx
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegExProperties = class(TPersistent)
  private
    FServer:    TStRegEx;
    function    GetDefaultInterface: IStRegEx;
    constructor Create(AServer: TStRegEx);
  protected
    function  Get_Avoid: WordBool;
    procedure Set_Avoid(Value: WordBool);
    function  Get_IgnoreCase: WordBool;
    procedure Set_IgnoreCase(Value: WordBool);
    function  Get_InFixedLineLength: Integer;
    procedure Set_InFixedLineLength(Value: Integer);
    function  Get_InLineTermChar: WideString;
    procedure Set_InLineTermChar(const Value: WideString);
    function  Get_InLineTerminator: TStLineTerminator;
    procedure Set_InLineTerminator(Value: TStLineTerminator);
    function  Get_LineCount: Integer;
    function  Get_LineNumbers: WordBool;
    procedure Set_LineNumbers(Value: WordBool);
    function  Get_LinesMatched: Integer;
    function  Get_LinesPerSecond: Integer;
    function  Get_LinesReplaced: Integer;
    function  Get_LinesSelected: Integer;
    function  Get_MatchPattern: IStStringList;
    procedure Set_MatchPattern(const Value: IStStringList);
    function  Get_OutFixedLineLength: Integer;
    procedure Set_OutFixedLineLength(Value: Integer);
    function  Get_OutLineTermChar: WideString;
    procedure Set_OutLineTermChar(const Value: WideString);
    function  Get_OutLineTerminitor: Integer;
    procedure Set_OutLineTerminitor(Value: Integer);
    function  Get_OutputOptions: TStOutputOption;
    procedure Set_OutputOptions(Value: TStOutputOption);
    function  Get_ReplacePattern: IStStringList;
    procedure Set_ReplacePattern(const Value: IStStringList);
    function  Get_SelAvoidPattern: IStStringList;
    procedure Set_SelAvoidPattern(const Value: IStStringList);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    property DefaultInterface: IStRegEx read GetDefaultInterface;
  published
    property Avoid: WordBool read Get_Avoid write Set_Avoid;
    property IgnoreCase: WordBool read Get_IgnoreCase write Set_IgnoreCase;
    property InFixedLineLength: Integer read Get_InFixedLineLength write Set_InFixedLineLength;
    property InLineTermChar: WideString read Get_InLineTermChar write Set_InLineTermChar;
    property InLineTerminator: TStLineTerminator read Get_InLineTerminator write Set_InLineTerminator;
    property LineNumbers: WordBool read Get_LineNumbers write Set_LineNumbers;
    property MatchPattern: IStStringList read Get_MatchPattern write Set_MatchPattern;
    property OutFixedLineLength: Integer read Get_OutFixedLineLength write Set_OutFixedLineLength;
    property OutLineTermChar: WideString read Get_OutLineTermChar write Set_OutLineTermChar;
    property OutLineTerminitor: Integer read Get_OutLineTerminitor write Set_OutLineTerminitor;
    property OutputOptions: TStOutputOption read Get_OutputOptions write Set_OutputOptions;
    property ReplacePattern: IStStringList read Get_ReplacePattern write Set_ReplacePattern;
    property SelAvoidPattern: IStStringList read Get_SelAvoidPattern write Set_SelAvoidPattern;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStExpr provides a Create and CreateRemote method to          
// create instances of the default interface IStExpr exposed by              
// the CoClass StExpr. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStExpr = class
    class function Create: IStExpr;
    class function CreateRemote(const MachineName: string): IStExpr;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStExpr
// Help String      : StExpr Object
// Default Interface: IStExpr
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStExprProperties= class;
{$ENDIF}
  TStExpr = class(TOleServer)
  private
    FIntf:        IStExpr;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStExprProperties;
    function      GetServerProperties: TStExprProperties;
{$ENDIF}
    function      GetDefaultInterface: IStExpr;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStExpr);
    procedure Disconnect; override;
    function  AnalyzeExpr(const Expr: WideString): Double;
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStExpr read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStExprProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStExpr
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStExprProperties = class(TPersistent)
  private
    FServer:    TStExpr;
    function    GetDefaultInterface: IStExpr;
    constructor Create(AServer: TStExpr);
  protected
  public
    property DefaultInterface: IStExpr read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStMime provides a Create and CreateRemote method to          
// create instances of the default interface IStMime exposed by              
// the CoClass StMime. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStMime = class
    class function Create: IStMime;
    class function CreateRemote(const MachineName: string): IStMime;
  end;

  TStMimeOnProgress = procedure(Sender: TObject; Status: TStConvertState; PercentDone: Byte) of object;
  TStMimeOnSaveAs = procedure(Sender: TObject; var FileName: OleVariant) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStMime
// Help String      : StMime Object
// Default Interface: IStMime
// Def. Intf. DISP? : No
// Event   Interface: IStMimeEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStMimeProperties= class;
{$ENDIF}
  TStMime = class(TOleServer)
  private
    FOnProgress: TStMimeOnProgress;
    FOnSaveAs: TStMimeOnSaveAs;
    FIntf:        IStMime;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStMimeProperties;
    function      GetServerProperties: TStMimeProperties;
{$ENDIF}
    function      GetDefaultInterface: IStMime;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_Attachments: IStStringList;
    function  Get_Boundry: WideString;
    procedure Set_Boundry(const Value: WideString);
    function  Get_Encoding: WideString;
    procedure Set_Encoding(const Value: WideString);
    function  Get_ContentDescription: WideString;
    procedure Set_ContentDescription(const Value: WideString);
    function  Get_ContentDisposition: WideString;
    procedure Set_ContentDisposition(const Value: WideString);
    function  Get_ContentType: WideString;
    procedure Set_ContentType(const Value: WideString);
    function  Get_Directory: WideString;
    procedure Set_Directory(const Value: WideString);
    function  Get_MimeHeaders: WordBool;
    procedure Set_MimeHeaders(Value: WordBool);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStMime);
    procedure Disconnect; override;
    procedure AddFileAttachment(const FileName: WideString);
    procedure AddStreamAttachment(Stream: OleVariant; const FileName: WideString);
    procedure ExtractAttachment(const Attachment: WideString);
    procedure ExtractAttachmentIndex(Index: Integer);
    procedure ExtractToStream(Index: Integer; Stream: OleVariant);
    procedure ExtractAttachments;
    procedure GetTag(const Description: WideString);
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStMime read GetDefaultInterface;
    property Attachments: IStStringList read Get_Attachments;
    property Stream: OleVariant read Get_Stream write Set_Stream;
    property Boundry: WideString read Get_Boundry write Set_Boundry;
    property Encoding: WideString read Get_Encoding write Set_Encoding;
    property ContentDescription: WideString read Get_ContentDescription write Set_ContentDescription;
    property ContentDisposition: WideString read Get_ContentDisposition write Set_ContentDisposition;
    property ContentType: WideString read Get_ContentType write Set_ContentType;
    property Directory: WideString read Get_Directory write Set_Directory;
    property MimeHeaders: WordBool read Get_MimeHeaders write Set_MimeHeaders;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStMimeProperties read GetServerProperties;
{$ENDIF}
    property OnProgress: TStMimeOnProgress read FOnProgress write FOnProgress;
    property OnSaveAs: TStMimeOnSaveAs read FOnSaveAs write FOnSaveAs;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStMime
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStMimeProperties = class(TPersistent)
  private
    FServer:    TStMime;
    function    GetDefaultInterface: IStMime;
    constructor Create(AServer: TStMime);
  protected
    function  Get_Attachments: IStStringList;
    function  Get_Boundry: WideString;
    procedure Set_Boundry(const Value: WideString);
    function  Get_Encoding: WideString;
    procedure Set_Encoding(const Value: WideString);
    function  Get_ContentDescription: WideString;
    procedure Set_ContentDescription(const Value: WideString);
    function  Get_ContentDisposition: WideString;
    procedure Set_ContentDisposition(const Value: WideString);
    function  Get_ContentType: WideString;
    procedure Set_ContentType(const Value: WideString);
    function  Get_Directory: WideString;
    procedure Set_Directory(const Value: WideString);
    function  Get_MimeHeaders: WordBool;
    procedure Set_MimeHeaders(Value: WordBool);
    function  Get_Stream: OleVariant;
    procedure Set_Stream(Value: OleVariant);
  public
    property DefaultInterface: IStMime read GetDefaultInterface;
  published
    property Boundry: WideString read Get_Boundry write Set_Boundry;
    property Encoding: WideString read Get_Encoding write Set_Encoding;
    property ContentDescription: WideString read Get_ContentDescription write Set_ContentDescription;
    property ContentDisposition: WideString read Get_ContentDisposition write Set_ContentDisposition;
    property ContentType: WideString read Get_ContentType write Set_ContentType;
    property Directory: WideString read Get_Directory write Set_Directory;
    property MimeHeaders: WordBool read Get_MimeHeaders write Set_MimeHeaders;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegINI provides a Create and CreateRemote method to          
// create instances of the default interface IStRegINI exposed by              
// the CoClass StRegINI. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegINI = class
    class function Create: IStRegINI;
    class function CreateRemote(const MachineName: string): IStRegINI;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegINI
// Help String      : StRegINI Object
// Default Interface: IStRegINI
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegINIProperties= class;
{$ENDIF}
  TStRegINI = class(TOleServer)
  private
    FIntf:        IStRegINI;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegINIProperties;
    function      GetServerProperties: TStRegINIProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegINI;
  protected
    procedure InitServerData; override;
    function  Get_CurrentSubKey: WideString;
    procedure Set_CurrentSubKey(const Value: WideString);
    function  Get_IsIniFile: WordBool;
    function  Get_Primary: WideString;
    procedure Set_Primary(const Value: WideString);
    function  Get_SubKeys: IStRegINISubKeys;
    function  Get_Values: IStRegINIValues;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegINI);
    procedure Disconnect; override;
    procedure Open(const RootName: WideString; IsIniFile: WordBool);
    procedure CreateKey(const KeyName: WideString);
    procedure DeleteKey(const KeyName: WideString; DeleteSubKeys: WordBool);
    procedure DeleteValue(const ValueName: WideString);
    function  KeyExists(const KeyName: WideString): WordBool;
    function  QueryKey: IStRegINIQueryKeyInfo;
    function  ReadBoolean(const ValueName: WideString; Default: WordBool): WordBool;
    function  ReadDate(const ValueName: WideString; Default: TDateTime): TDateTime;
    function  ReadDateTime(const ValueName: WideString; Default: TDateTime): TDateTime;
    function  ReadInteger(const ValueName: WideString; Default: Integer): Integer;
    function  ReadString(const ValueName: WideString; const Default: WideString): WideString;
    function  ReadTime(const ValueName: WideString; Default: TDateTime): TDateTime;
    procedure WriteBoolean(const ValueName: WideString; Value: WordBool);
    procedure WriteDate(const ValueName: WideString; Value: TDateTime);
    procedure WriteDateTime(const ValueName: WideString; Value: TDateTime);
    procedure WriteInteger(const ValueName: WideString; Value: Integer);
    procedure WriteString(const ValueName: WideString; const Value: WideString);
    procedure WriteTime(const ValueName: WideString; Value: TDateTime);
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStRegINI read GetDefaultInterface;
    property IsIniFile: WordBool read Get_IsIniFile;
    property SubKeys: IStRegINISubKeys read Get_SubKeys;
    property Values: IStRegINIValues read Get_Values;
    property CurrentSubKey: WideString read Get_CurrentSubKey write Set_CurrentSubKey;
    property Primary: WideString read Get_Primary write Set_Primary;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegINIProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegINI
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegINIProperties = class(TPersistent)
  private
    FServer:    TStRegINI;
    function    GetDefaultInterface: IStRegINI;
    constructor Create(AServer: TStRegINI);
  protected
    function  Get_CurrentSubKey: WideString;
    procedure Set_CurrentSubKey(const Value: WideString);
    function  Get_IsIniFile: WordBool;
    function  Get_Primary: WideString;
    procedure Set_Primary(const Value: WideString);
    function  Get_SubKeys: IStRegINISubKeys;
    function  Get_Values: IStRegINIValues;
  public
    property DefaultInterface: IStRegINI read GetDefaultInterface;
  published
    property CurrentSubKey: WideString read Get_CurrentSubKey write Set_CurrentSubKey;
    property Primary: WideString read Get_Primary write Set_Primary;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStFin provides a Create and CreateRemote method to          
// create instances of the default interface IStFin exposed by              
// the CoClass StFin. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStFin = class
    class function Create: IStFin;
    class function CreateRemote(const MachineName: string): IStFin;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStFin
// Help String      : StFin Object
// Default Interface: IStFin
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStFinProperties= class;
{$ENDIF}
  TStFin = class(TOleServer)
  private
    FIntf:        IStFin;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStFinProperties;
    function      GetServerProperties: TStFinProperties;
{$ENDIF}
    function      GetDefaultInterface: IStFin;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStFin);
    procedure Disconnect; override;
    function  IsCardValid(const S: WideString): WordBool;
    function  License(const Key: WideString): WordBool;
    property  DefaultInterface: IStFin read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStFinProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStFin
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStFinProperties = class(TPersistent)
  private
    FServer:    TStFin;
    function    GetDefaultInterface: IStFin;
    constructor Create(AServer: TStFin);
  protected
  public
    property DefaultInterface: IStFin read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegINIQueryKeyInfo provides a Create and CreateRemote method to          
// create instances of the default interface IStRegINIQueryKeyInfo exposed by              
// the CoClass StRegINIQueryKeyInfo. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegINIQueryKeyInfo = class
    class function Create: IStRegINIQueryKeyInfo;
    class function CreateRemote(const MachineName: string): IStRegINIQueryKeyInfo;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegINIQueryKeyInfo
// Help String      : StRegINIQueryKeyInfo Object
// Default Interface: IStRegINIQueryKeyInfo
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegINIQueryKeyInfoProperties= class;
{$ENDIF}
  TStRegINIQueryKeyInfo = class(TOleServer)
  private
    FIntf:        IStRegINIQueryKeyInfo;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegINIQueryKeyInfoProperties;
    function      GetServerProperties: TStRegINIQueryKeyInfoProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegINIQueryKeyInfo;
  protected
    procedure InitServerData; override;
    function  Get_QIClassName: WideString;
    function  Get_QIFileTime: TDateTime;
    function  Get_QIKey: Integer;
    function  Get_QIMaxCNLen: Integer;
    function  Get_QIMaxDataLen: Integer;
    function  Get_QIMaxSKNLen: Integer;
    function  Get_QIMaxVNLen: Integer;
    function  Get_QINumSubKeys: Integer;
    function  Get_QINumValues: Integer;
    function  Get_QISDescLen: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegINIQueryKeyInfo);
    procedure Disconnect; override;
    property  DefaultInterface: IStRegINIQueryKeyInfo read GetDefaultInterface;
    property QIClassName: WideString read Get_QIClassName;
    property QIFileTime: TDateTime read Get_QIFileTime;
    property QIKey: Integer read Get_QIKey;
    property QIMaxCNLen: Integer read Get_QIMaxCNLen;
    property QIMaxDataLen: Integer read Get_QIMaxDataLen;
    property QIMaxSKNLen: Integer read Get_QIMaxSKNLen;
    property QIMaxVNLen: Integer read Get_QIMaxVNLen;
    property QINumSubKeys: Integer read Get_QINumSubKeys;
    property QINumValues: Integer read Get_QINumValues;
    property QISDescLen: Integer read Get_QISDescLen;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegINIQueryKeyInfoProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegINIQueryKeyInfo
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegINIQueryKeyInfoProperties = class(TPersistent)
  private
    FServer:    TStRegINIQueryKeyInfo;
    function    GetDefaultInterface: IStRegINIQueryKeyInfo;
    constructor Create(AServer: TStRegINIQueryKeyInfo);
  protected
    function  Get_QIClassName: WideString;
    function  Get_QIFileTime: TDateTime;
    function  Get_QIKey: Integer;
    function  Get_QIMaxCNLen: Integer;
    function  Get_QIMaxDataLen: Integer;
    function  Get_QIMaxSKNLen: Integer;
    function  Get_QIMaxVNLen: Integer;
    function  Get_QINumSubKeys: Integer;
    function  Get_QINumValues: Integer;
    function  Get_QISDescLen: Integer;
  public
    property DefaultInterface: IStRegINIQueryKeyInfo read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegINISubKeys provides a Create and CreateRemote method to          
// create instances of the default interface IStRegINISubKeys exposed by              
// the CoClass StRegINISubKeys. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegINISubKeys = class
    class function Create: IStRegINISubKeys;
    class function CreateRemote(const MachineName: string): IStRegINISubKeys;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegINISubKeys
// Help String      : StRegINISubKeys Object
// Default Interface: IStRegINISubKeys
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegINISubKeysProperties= class;
{$ENDIF}
  TStRegINISubKeys = class(TOleServer)
  private
    FIntf:        IStRegINISubKeys;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegINISubKeysProperties;
    function      GetServerProperties: TStRegINISubKeysProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegINISubKeys;
  protected
    procedure InitServerData; override;
    function  Get__NewEnum: IUnknown;
    function  Get_Item(Index: Integer): IStRegINISubKey;
    function  Get_Count: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegINISubKeys);
    procedure Disconnect; override;
    property  DefaultInterface: IStRegINISubKeys read GetDefaultInterface;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: Integer]: IStRegINISubKey read Get_Item; default;
    property Count: Integer read Get_Count;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegINISubKeysProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegINISubKeys
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegINISubKeysProperties = class(TPersistent)
  private
    FServer:    TStRegINISubKeys;
    function    GetDefaultInterface: IStRegINISubKeys;
    constructor Create(AServer: TStRegINISubKeys);
  protected
    function  Get__NewEnum: IUnknown;
    function  Get_Item(Index: Integer): IStRegINISubKey;
    function  Get_Count: Integer;
  public
    property DefaultInterface: IStRegINISubKeys read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegINIValue provides a Create and CreateRemote method to          
// create instances of the default interface IStRegINIValue exposed by              
// the CoClass StRegINIValue. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegINIValue = class
    class function Create: IStRegINIValue;
    class function CreateRemote(const MachineName: string): IStRegINIValue;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegINIValue
// Help String      : StRegIniValue Object
// Default Interface: IStRegINIValue
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegINIValueProperties= class;
{$ENDIF}
  TStRegINIValue = class(TOleServer)
  private
    FIntf:        IStRegINIValue;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegINIValueProperties;
    function      GetServerProperties: TStRegINIValueProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegINIValue;
  protected
    procedure InitServerData; override;
    function  Get_Value: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegINIValue);
    procedure Disconnect; override;
    property  DefaultInterface: IStRegINIValue read GetDefaultInterface;
    property Value: WideString read Get_Value;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegINIValueProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegINIValue
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegINIValueProperties = class(TPersistent)
  private
    FServer:    TStRegINIValue;
    function    GetDefaultInterface: IStRegINIValue;
    constructor Create(AServer: TStRegINIValue);
  protected
    function  Get_Value: WideString;
  public
    property DefaultInterface: IStRegINIValue read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegINIValues provides a Create and CreateRemote method to          
// create instances of the default interface IStRegINIValues exposed by              
// the CoClass StRegINIValues. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegINIValues = class
    class function Create: IStRegINIValues;
    class function CreateRemote(const MachineName: string): IStRegINIValues;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegINIValues
// Help String      : StRegIniValues Object
// Default Interface: IStRegINIValues
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegINIValuesProperties= class;
{$ENDIF}
  TStRegINIValues = class(TOleServer)
  private
    FIntf:        IStRegINIValues;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegINIValuesProperties;
    function      GetServerProperties: TStRegINIValuesProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegINIValues;
  protected
    procedure InitServerData; override;
    function  Get__NewEnum: IUnknown;
    function  Get_Item(Index: Integer): IStRegINIValue;
    function  Get_Count: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegINIValues);
    procedure Disconnect; override;
    property  DefaultInterface: IStRegINIValues read GetDefaultInterface;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: Integer]: IStRegINIValue read Get_Item; default;
    property Count: Integer read Get_Count;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegINIValuesProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegINIValues
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegINIValuesProperties = class(TPersistent)
  private
    FServer:    TStRegINIValues;
    function    GetDefaultInterface: IStRegINIValues;
    constructor Create(AServer: TStRegINIValues);
  protected
    function  Get__NewEnum: IUnknown;
    function  Get_Item(Index: Integer): IStRegINIValue;
    function  Get_Count: Integer;
  public
    property DefaultInterface: IStRegINIValues read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStRegINISubKey provides a Create and CreateRemote method to          
// create instances of the default interface IStRegINISubKey exposed by              
// the CoClass StRegINISubKey. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStRegINISubKey = class
    class function Create: IStRegINISubKey;
    class function CreateRemote(const MachineName: string): IStRegINISubKey;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStRegINISubKey
// Help String      : StRegINISubKey Object
// Default Interface: IStRegINISubKey
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStRegINISubKeyProperties= class;
{$ENDIF}
  TStRegINISubKey = class(TOleServer)
  private
    FIntf:        IStRegINISubKey;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TStRegINISubKeyProperties;
    function      GetServerProperties: TStRegINISubKeyProperties;
{$ENDIF}
    function      GetDefaultInterface: IStRegINISubKey;
  protected
    procedure InitServerData; override;
    function  Get_Value: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStRegINISubKey);
    procedure Disconnect; override;
    property  DefaultInterface: IStRegINISubKey read GetDefaultInterface;
    property Value: WideString read Get_Value;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStRegINISubKeyProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStRegINISubKey
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStRegINISubKeyProperties = class(TPersistent)
  private
    FServer:    TStRegINISubKey;
    function    GetDefaultInterface: IStRegINISubKey;
    constructor Create(AServer: TStRegINISubKey);
  protected
    function  Get_Value: WideString;
  public
    property DefaultInterface: IStRegINISubKey read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoStDate.Create: IStDate;
begin
  Result := CreateComObject(CLASS_StDate) as IStDate;
end;

class function CoStDate.CreateRemote(const MachineName: string): IStDate;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StDate) as IStDate;
end;

procedure TStDate.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{C77513BB-8AB2-4EB8-A585-8D3F8486F4E3}';
    IntfIID:   '{06A34C32-CF9D-40DA-B943-B7D5D178AE4D}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStDate.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStDate;
  end;
end;

procedure TStDate.ConnectTo(svrIntf: IStDate);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStDate.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStDate.GetDefaultInterface: IStDate;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStDate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStDateProperties.Create(Self);
{$ENDIF}
end;

destructor TStDate.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStDate.GetServerProperties: TStDateProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStDate.AstJulianDate(Julian: TDateTime): Double;
begin
  Result := DefaultInterface.AstJulianDate(Julian);
end;

function  TStDate.AstJulianDatePrim(Year: Integer; Month: Integer; Day: Integer; UT: TDateTime): Double;
begin
  Result := DefaultInterface.AstJulianDatePrim(Year, Month, Day, UT);
end;

function  TStDate.AstJulianDateToStDate(AstJD: Double; Truncate: WordBool): TDateTime;
begin
  Result := DefaultInterface.AstJulianDateToStDate(AstJD, Truncate);
end;

function  TStDate.BondDateDiff(Date1: TDateTime; Date2: TDateTime; DayBasis: TStBondDateType): TDateTime;
begin
  Result := DefaultInterface.BondDateDiff(Date1, Date2, DayBasis);
end;

function  TStDate.CurrentDate: TDateTime;
begin
  Result := DefaultInterface.CurrentDate;
end;

function  TStDate.CurrentDateString(const Picture: WideString; Pack: WordBool): WideString;
begin
  Result := DefaultInterface.CurrentDateString(Picture, Pack);
end;

function  TStDate.CurrentTime: TDateTime;
begin
  Result := DefaultInterface.CurrentTime;
end;

function  TStDate.CurrentTimeString(const Picture: WideString; Pack: WordBool): WideString;
begin
  Result := DefaultInterface.CurrentTimeString(Picture, Pack);
end;

procedure TStDate.DateDiff(Date1: TDateTime; Date2: TDateTime; var Days: Integer; 
                           var Months: Integer; var Years: Integer);
begin
  DefaultInterface.DateDiff(Date1, Date2, Days, Months, Years);
end;

function  TStDate.DateStringHMStoAstJD(const Picture: WideString; const DS: WideString; 
                                       Hours: Integer; Minutes: Integer; Seconds: Integer; 
                                       Epoch: Integer): Double;
begin
  Result := DefaultInterface.DateStringHMStoAstJD(Picture, DS, Hours, Minutes, Seconds, Epoch);
end;

function  TStDate.DateStringToDMY(const Picture: WideString; const S: WideString; var Day: Integer; 
                                  var Month: Integer; var Year: Integer; Epoch: Integer): WordBool;
begin
  Result := DefaultInterface.DateStringToDMY(Picture, S, Day, Month, Year, Epoch);
end;

function  TStDate.DateStringToStDate(const Picture: WideString; const S: WideString; Epoch: Integer): TDateTime;
begin
  Result := DefaultInterface.DateStringToStDate(Picture, S, Epoch);
end;

procedure TStDate.DateTimeDiff(DT1: TDateTime; DT2: TDateTime; var Days: Integer; 
                               var Seconds: Integer);
begin
  DefaultInterface.DateTimeDiff(DT1, DT2, Days, Seconds);
end;

function  TStDate.DayOfWeek(Julian: TDateTime): TStDayType;
begin
  Result := DefaultInterface.DayOfWeek(Julian);
end;

function  TStDate.DayOfWeekDMY(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TStDayType;
begin
  Result := DefaultInterface.DayOfWeekDMY(Day, Month, Year, Epoch);
end;

function  TStDate.DayOfWeekToString(WeekDay: TStDayType): WideString;
begin
  Result := DefaultInterface.DayOfWeekToString(WeekDay);
end;

function  TStDate.DaysInMonth(Month: Integer; Year: Integer; Epoch: Integer): Integer;
begin
  Result := DefaultInterface.DaysInMonth(Month, Year, Epoch);
end;

function  TStDate.DecTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime;
begin
  Result := DefaultInterface.DecTime(T, Hours, Minutes, Seconds);
end;

function  TStDate.DMYtoStDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): TDateTime;
begin
  Result := DefaultInterface.DMYtoStDate(Day, Month, Year, Epoch);
end;

function  TStDate.DMYtoDateString(const Picture: WideString; Day: Integer; Month: Integer; 
                                  Year: Integer; Epoch: Integer; Pack: WordBool): WideString;
begin
  Result := DefaultInterface.DMYtoDateString(Picture, Day, Month, Year, Epoch, Pack);
end;

function  TStDate.HMStoStTime(Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime;
begin
  Result := DefaultInterface.HMStoStTime(Hours, Minutes, Seconds);
end;

function  TStDate.IncDate(Julian: TDateTime; Days: Integer; Months: Integer; Years: Integer): TDateTime;
begin
  Result := DefaultInterface.IncDate(Julian, Days, Months, Years);
end;

procedure TStDate.IncDateTime(DT1: TDateTime; var DT2: TDateTime; Days: Integer; Seconds: Integer);
begin
  DefaultInterface.IncDateTime(DT1, DT2, Days, Seconds);
end;

function  TStDate.IncDateTrunc(Julian: TDateTime; Months: Integer; Years: Integer): TDateTime;
begin
  Result := DefaultInterface.IncDateTrunc(Julian, Months, Years);
end;

function  TStDate.IncTime(T: TDateTime; Hours: Byte; Minutes: Byte; Seconds: Byte): TDateTime;
begin
  Result := DefaultInterface.IncTime(T, Hours, Minutes, Seconds);
end;

function  TStDate.InternationalDate(ForceCentury: WordBool): WideString;
begin
  Result := DefaultInterface.InternationalDate(ForceCentury);
end;

function  TStDate.InternationalLongDate(ShortNames: WordBool; ExcludeDOW: WordBool): WideString;
begin
  Result := DefaultInterface.InternationalLongDate(ShortNames, ExcludeDOW);
end;

function  TStDate.InternationalTime(ShowSeconds: WordBool): WideString;
begin
  Result := DefaultInterface.InternationalTime(ShowSeconds);
end;

function  TStDate.IsLeapYear(Year: Integer): WordBool;
begin
  Result := DefaultInterface.IsLeapYear(Year);
end;

function  TStDate.MonthToString(Month: Integer): WideString;
begin
  Result := DefaultInterface.MonthToString(Month);
end;

function  TStDate.RoundToNearestHour(T: TDateTime; Truncate: WordBool): TDateTime;
begin
  Result := DefaultInterface.RoundToNearestHour(T, Truncate);
end;

function  TStDate.RoundToNearestMinute(T: TDateTime; Truncate: WordBool): TDateTime;
begin
  Result := DefaultInterface.RoundToNearestMinute(T, Truncate);
end;

function  TStDate.StDateToDateString(const Picture: WideString; Julian: TDateTime; Pack: WordBool): WideString;
begin
  Result := DefaultInterface.StDateToDateString(Picture, Julian, Pack);
end;

procedure TStDate.StDateToDMY(Julian: TDateTime; var Day: Integer; var Month: Integer; 
                              var Year: Integer);
begin
  DefaultInterface.StDateToDMY(Julian, Day, Month, Year);
end;

function  TStDate.StTimeToAmPmString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString;
begin
  Result := DefaultInterface.StTimeToAmPmString(Picture, T, Pack);
end;

procedure TStDate.StTimeToHMS(T: TDateTime; var Hours: Byte; var Minutes: Byte; var Seconds: Byte);
begin
  DefaultInterface.StTimeToHMS(T, Hours, Minutes, Seconds);
end;

function  TStDate.StTimeToTimeString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString;
begin
  Result := DefaultInterface.StTimeToTimeString(Picture, T, Pack);
end;

procedure TStDate.TimeDiff(T1: TDateTime; T2: TDateTime; var Hours: Byte; var Minutes: Byte; 
                           var Seconds: Byte);
begin
  DefaultInterface.TimeDiff(T1, T2, Hours, Minutes, Seconds);
end;

function  TStDate.TimeStringToHMS(const Picture: WideString; const TS: WideString; 
                                  var Hours: Integer; var Minutes: Integer; var Seconds: Integer): WordBool;
begin
  Result := DefaultInterface.TimeStringToHMS(Picture, TS, Hours, Minutes, Seconds);
end;

function  TStDate.TimeStringToStTime(const Picture: WideString; const S: WideString): TDateTime;
begin
  Result := DefaultInterface.TimeStringToStTime(Picture, S);
end;

function  TStDate.ValidDate(Day: Integer; Month: Integer; Year: Integer; Epoch: Integer): WordBool;
begin
  Result := DefaultInterface.ValidDate(Day, Month, Year, Epoch);
end;

function  TStDate.ValidTime(Hours: Integer; Minutes: Integer; Seconds: Integer): WordBool;
begin
  Result := DefaultInterface.ValidTime(Hours, Minutes, Seconds);
end;

function  TStDate.WeekOfYear(Julian: TDateTime): Byte;
begin
  Result := DefaultInterface.WeekOfYear(Julian);
end;

function  TStDate.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStDateProperties.Create(AServer: TStDate);
begin
  inherited Create;
  FServer := AServer;
end;

function TStDateProperties.GetDefaultInterface: IStDate;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoStString.Create: IStString;
begin
  Result := CreateComObject(CLASS_StString) as IStString;
end;

class function CoStString.CreateRemote(const MachineName: string): IStString;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StString) as IStString;
end;

procedure TStString.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D152991C-854C-40DD-AB93-912311F62E2E}';
    IntfIID:   '{B622F701-6E3F-4875-A574-201F0DB002F3}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStString.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStString;
  end;
end;

procedure TStString.ConnectTo(svrIntf: IStString);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStString.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStString.GetDefaultInterface: IStString;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStString.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStStringProperties.Create(Self);
{$ENDIF}
end;

destructor TStString.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStString.GetServerProperties: TStStringProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStString.AddBackSlash(const DirName: WideString): WideString;
begin
  Result := DefaultInterface.AddBackSlash(DirName);
end;

function  TStString.AsciiCount(const S: WideString; const WordDelims: WideString; 
                               const Quote: WideString): Integer;
begin
  Result := DefaultInterface.AsciiCount(S, WordDelims, Quote);
end;

function  TStString.AsciiPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                                  const Quote: WideString; var Pos: Integer): WordBool;
begin
  Result := DefaultInterface.AsciiPosition(N, S, WordDelims, Quote, Pos);
end;

function  TStString.BinaryB(B: Byte): WideString;
begin
  Result := DefaultInterface.BinaryB(B);
end;

function  TStString.BinaryW(W: Integer): WideString;
begin
  Result := DefaultInterface.BinaryW(W);
end;

function  TStString.BinaryL(L: Integer): WideString;
begin
  Result := DefaultInterface.BinaryL(L);
end;

function  TStString.Center(const S: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.Center(S, Len);
end;

function  TStString.CenterCh(const S: WideString; const C: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.CenterCh(S, C, Len);
end;

function  TStString.CharCount(const S: WideString; const C: WideString): Integer;
begin
  Result := DefaultInterface.CharCount(S, C);
end;

function  TStString.CharExists(const S: WideString; const C: WideString): WordBool;
begin
  Result := DefaultInterface.CharExists(S, C);
end;

function  TStString.CharStr(const C: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.CharStr(C, Len);
end;

function  TStString.CleanPathName(const PathName: WideString): WideString;
begin
  Result := DefaultInterface.CleanPathName(PathName);
end;

function  TStString.Commaize(L: Integer): WideString;
begin
  Result := DefaultInterface.Commaize(L);
end;

function  TStString.CommaizeCh(L: Integer; const Ch: WideString): WideString;
begin
  Result := DefaultInterface.CommaizeCh(L, Ch);
end;

function  TStString.CompString(const S1: WideString; const S2: WideString): Integer;
begin
  Result := DefaultInterface.CompString(S1, S2);
end;

function  TStString.CompUCString(const S1: WideString; const S2: WideString): Integer;
begin
  Result := DefaultInterface.CompUCString(S1, S2);
end;

function  TStString.ContainsOnly(const S: WideString; const Chars: WideString; var BadPos: Integer): WordBool;
begin
  Result := DefaultInterface.ContainsOnly(S, Chars, BadPos);
end;

function  TStString.ContainsOtherThan(const S: WideString; const Chars: WideString; 
                                      var BadPos: Integer): WordBool;
begin
  Result := DefaultInterface.ContainsOtherThan(S, Chars, BadPos);
end;

function  TStString.CopyFromNthWord(const S: WideString; const WordDelims: WideString; 
                                    const AWord: WideString; N: Integer; var SubString: WideString): WordBool;
begin
  Result := DefaultInterface.CopyFromNthWord(S, WordDelims, AWord, N, SubString);
end;

function  TStString.CopyFromToWord(const S: WideString; const WordDelims: WideString; 
                                   const Word1: WideString; const Word2: WideString; N1: Integer; 
                                   N2: Integer; var SubString: WideString): WordBool;
begin
  Result := DefaultInterface.CopyFromToWord(S, WordDelims, Word1, Word2, N1, N2, SubString);
end;

function  TStString.CopyLeft(const S: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.CopyLeft(S, Len);
end;

function  TStString.CopyMid(const S: WideString; First: Integer; Len: Integer): WideString;
begin
  Result := DefaultInterface.CopyMid(S, First, Len);
end;

function  TStString.CopyRight(const S: WideString; First: Integer): WideString;
begin
  Result := DefaultInterface.CopyRight(S, First);
end;

function  TStString.CopyWithin(const S: WideString; const Delimiter: WideString; Strip: WordBool): WideString;
begin
  Result := DefaultInterface.CopyWithin(S, Delimiter, Strip);
end;

function  TStString.DefaultExtension(const Name: WideString; const Ext: WideString): WideString;
begin
  Result := DefaultInterface.DefaultExtension(Name, Ext);
end;

function  TStString.DeleteFromNthWord(const S: WideString; const WordDelims: WideString; 
                                      const AWord: WideString; N: Integer; var SubString: WideString): WordBool;
begin
  Result := DefaultInterface.DeleteFromNthWord(S, WordDelims, AWord, N, SubString);
end;

function  TStString.DeleteFromToWord(const S: WideString; const WordDelims: WideString; 
                                     const Word1: WideString; const Word2: WideString; N1: Integer; 
                                     N2: Integer; var SubString: WideString): WordBool;
begin
  Result := DefaultInterface.DeleteFromToWord(S, WordDelims, Word1, Word2, N1, N2, SubString);
end;

function  TStString.DeleteWithin(const S: WideString; const Delimeter: WideString): WideString;
begin
  Result := DefaultInterface.DeleteWithin(S, Delimeter);
end;

function  TStString.Detab(const S: WideString; TabSize: Byte): WideString;
begin
  Result := DefaultInterface.Detab(S, TabSize);
end;

function  TStString.Entab(const S: WideString; TabSize: Byte): WideString;
begin
  Result := DefaultInterface.Entab(S, TabSize);
end;

function  TStString.Ext2Str(R: OleVariant; Width: Byte; Places: Byte): WideString;
begin
  Result := DefaultInterface.Ext2Str(R, Width, Places);
end;

function  TStString.ExtractAscii(N: Integer; const S: WideString; const WordDelims: WideString; 
                                 const Quote: WideString): WideString;
begin
  Result := DefaultInterface.ExtractAscii(N, S, WordDelims, Quote);
end;

function  TStString.ExtractTokens(const S: WideString; const Delims: WideString; 
                                  const QuoteChar: WideString; AllowNulls: WordBool; 
                                  out Tokens: IStStringList): Integer;
begin
  Result := DefaultInterface.ExtractTokens(S, Delims, QuoteChar, AllowNulls, Tokens);
end;

function  TStString.ExtractWord(N: Integer; const S: WideString; const WordDelims: WideString): WideString;
begin
  Result := DefaultInterface.ExtractWord(N, S, WordDelims);
end;

function  TStString.Filter(const S: WideString; const Filters: WideString): WideString;
begin
  Result := DefaultInterface.Filter(S, Filters);
end;

function  TStString.FloatForm(const Mask: WideString; R: Double; L: Integer; 
                              const LtCurr: WideString; const RtCurr: WideString; 
                              const Sep: WideString; const DecPt: WideString): WideString;
begin
  Result := DefaultInterface.FloatForm(Mask, R, L, LtCurr, RtCurr, Sep, DecPt);
end;

function  TStString.ForceExtension(const Name: WideString; const Ext: WideString): WideString;
begin
  Result := DefaultInterface.ForceExtension(Name, Ext);
end;

function  TStString.HasExtension(const Name: WideString; var DotPos: Integer): WordBool;
begin
  Result := DefaultInterface.HasExtension(Name, DotPos);
end;

function  TStString.HexB(B: Byte): WideString;
begin
  Result := DefaultInterface.HexB(B);
end;

function  TStString.HexW(W: Integer): WideString;
begin
  Result := DefaultInterface.HexW(W);
end;

function  TStString.HexL(L: Integer): WideString;
begin
  Result := DefaultInterface.HexL(L);
end;

function  TStString.IsChAlpha(const C: WideString): WordBool;
begin
  Result := DefaultInterface.IsChAlpha(C);
end;

function  TStString.IsChAlphaNumeric(const C: WideString; const Numbers: WideString): WordBool;
begin
  Result := DefaultInterface.IsChAlphaNumeric(C, Numbers);
end;

function  TStString.IsChNumeric(const C: WideString; const Numbers: WideString): WordBool;
begin
  Result := DefaultInterface.IsChNumeric(C, Numbers);
end;

function  TStString.IsStrAlpha(const S: WideString): WordBool;
begin
  Result := DefaultInterface.IsStrAlpha(S);
end;

function  TStString.IsStrAlphaNumeric(const S: WideString; const Numbers: WideString): WordBool;
begin
  Result := DefaultInterface.IsStrAlphaNumeric(S, Numbers);
end;

function  TStString.IsStrNumeric(const S: WideString; const Numbers: WideString): WordBool;
begin
  Result := DefaultInterface.IsStrNumeric(S, Numbers);
end;

function  TStString.JustExtension(const Name: WideString): WideString;
begin
  Result := DefaultInterface.JustExtension(Name);
end;

function  TStString.JustFilename(const PathName: WideString): WideString;
begin
  Result := DefaultInterface.JustFilename(PathName);
end;

function  TStString.JustName(const PathName: WideString): WideString;
begin
  Result := DefaultInterface.JustName(PathName);
end;

function  TStString.JustPathname(const PathName: WideString): WideString;
begin
  Result := DefaultInterface.JustPathname(PathName);
end;

function  TStString.KeepChars(const S: WideString; const Chars: WideString): WideString;
begin
  Result := DefaultInterface.KeepChars(S, Chars);
end;

function  TStString.LastString(const S: WideString; const AString: WideString; var Position: Integer): WordBool;
begin
  Result := DefaultInterface.LastString(S, AString, Position);
end;

function  TStString.LastWord(const S: WideString; const WordDelims: WideString; 
                             const AWord: WideString; var Position: Integer): WordBool;
begin
  Result := DefaultInterface.LastWord(S, WordDelims, AWord, Position);
end;

function  TStString.LastWordAbs(const S: WideString; const WordDelims: WideString; 
                                var Position: Integer): WordBool;
begin
  Result := DefaultInterface.LastWordAbs(S, WordDelims, Position);
end;

function  TStString.LeftPad(const S: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.LeftPad(S, Len);
end;

function  TStString.LeftPadCh(const S: WideString; const C: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.LeftPadCh(S, C, Len);
end;

function  TStString.LeftTrimChars(const S: WideString; const Chars: WideString): WideString;
begin
  Result := DefaultInterface.LeftTrimChars(S, Chars);
end;

function  TStString.Long2Str(L: Integer): WideString;
begin
  Result := DefaultInterface.Long2Str(L);
end;

function  TStString.LongIntForm(const Mask: WideString; L: Integer; const LtCurr: WideString; 
                                const RtCurr: WideString; const Sep: WideString): WideString;
begin
  Result := DefaultInterface.LongIntForm(Mask, L, LtCurr, RtCurr, Sep);
end;

function  TStString.OctalB(B: Byte): WideString;
begin
  Result := DefaultInterface.OctalB(B);
end;

function  TStString.OctalW(W: Integer): WideString;
begin
  Result := DefaultInterface.OctalW(W);
end;

function  TStString.OctalL(L: Integer): WideString;
begin
  Result := DefaultInterface.OctalL(L);
end;

function  TStString.Pad(const S: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.Pad(S, Len);
end;

function  TStString.PadCh(const S: WideString; const C: WideString; Len: Integer): WideString;
begin
  Result := DefaultInterface.PadCh(S, C, Len);
end;

function  TStString.Real2Str(R: Double; Width: Byte; Places: Byte): WideString;
begin
  Result := DefaultInterface.Real2Str(R, Width, Places);
end;

function  TStString.RepeatString(const S: WideString; var Repetitions: Integer; MaxLen: Integer): WideString;
begin
  Result := DefaultInterface.RepeatString(S, Repetitions, MaxLen);
end;

function  TStString.ReplaceWord(const S: WideString; const WordDelims: WideString; 
                                const OldWord: WideString; const NewWord: WideString; N: Integer; 
                                var Replacements: Integer): WideString;
begin
  Result := DefaultInterface.ReplaceWord(S, WordDelims, OldWord, NewWord, N, Replacements);
end;

function  TStString.ReplaceWordAll(const S: WideString; const WordDelims: WideString; 
                                   const OldWord: WideString; const NewWord: WideString; 
                                   var Replacements: Integer): WideString;
begin
  Result := DefaultInterface.ReplaceWordAll(S, WordDelims, OldWord, NewWord, Replacements);
end;

function  TStString.ReplaceString(const S: WideString; const OldString: WideString; 
                                  const NewString: WideString; N: Integer; var Replacements: Integer): WideString;
begin
  Result := DefaultInterface.ReplaceString(S, OldString, NewString, N, Replacements);
end;

function  TStString.ReplaceStringAll(const S: WideString; const OldString: WideString; 
                                     const NewString: WideString; var Replacements: Integer): WideString;
begin
  Result := DefaultInterface.ReplaceStringAll(S, OldString, NewString, Replacements);
end;

function  TStString.RightTrimChars(const S: WideString; const Chars: WideString): WideString;
begin
  Result := DefaultInterface.RightTrimChars(S, Chars);
end;

function  TStString.Scramble(const S: WideString; const Key: WideString): WideString;
begin
  Result := DefaultInterface.Scramble(S, Key);
end;

function  TStString.Str2Ext(const S: WideString; var R: OleVariant): WordBool;
begin
  Result := DefaultInterface.Str2Ext(S, R);
end;

function  TStString.Str2Int16(const S: WideString; var I: Smallint): WordBool;
begin
  Result := DefaultInterface.Str2Int16(S, I);
end;

function  TStString.Str2Long(const S: WideString; var I: Integer): WordBool;
begin
  Result := DefaultInterface.Str2Long(S, I);
end;

function  TStString.Str2Real(const S: WideString; var R: Double): WordBool;
begin
  Result := DefaultInterface.Str2Real(S, R);
end;

function  TStString.Str2Word(const S: WideString; var W: Integer): WordBool;
begin
  Result := DefaultInterface.Str2Word(S, W);
end;

function  TStString.StrChDelete(const S: WideString; Pos: Integer): WideString;
begin
  Result := DefaultInterface.StrChDelete(S, Pos);
end;

function  TStString.StrChInsert(const S: WideString; const C: WideString; Pos: Integer): WideString;
begin
  Result := DefaultInterface.StrChInsert(S, C, Pos);
end;

function  TStString.StrChPos(const P: WideString; const C: WideString; var Pos: Integer): WordBool;
begin
  Result := DefaultInterface.StrChPos(P, C, Pos);
end;

function  TStString.StrStCopy(const S: WideString; Pos: Integer; Count: Integer): WideString;
begin
  Result := DefaultInterface.StrStCopy(S, Pos, Count);
end;

function  TStString.StrStDelete(const S: WideString; Pos: Integer; Count: Integer): WideString;
begin
  Result := DefaultInterface.StrStDelete(S, Pos, Count);
end;

function  TStString.StrStInsert(const S1: WideString; const S2: WideString; Pos: Integer): WideString;
begin
  Result := DefaultInterface.StrStInsert(S1, S2, Pos);
end;

function  TStString.StrStPos(const P: WideString; const S: WideString; var Pos: Integer): WordBool;
begin
  Result := DefaultInterface.StrStPos(P, S, Pos);
end;

function  TStString.StrWithin(const S: WideString; const SearchStr: WideString; Start: Integer; 
                              var Position: Integer): WordBool;
begin
  Result := DefaultInterface.StrWithin(S, SearchStr, Start, Position);
end;

function  TStString.Substitute(const S: WideString; const FromStr: WideString; 
                               const ToStr: WideString): WideString;
begin
  Result := DefaultInterface.Substitute(S, FromStr, ToStr);
end;

function  TStString.Trim(const S: WideString): WideString;
begin
  Result := DefaultInterface.Trim(S);
end;

function  TStString.TrimChars(const S: WideString; const Chars: WideString): WideString;
begin
  Result := DefaultInterface.TrimChars(S, Chars);
end;

function  TStString.TrimLead(const S: WideString): WideString;
begin
  Result := DefaultInterface.TrimLead(S);
end;

function  TStString.TrimSpaces(const S: WideString): WideString;
begin
  Result := DefaultInterface.TrimSpaces(S);
end;

function  TStString.TrimTrail(const S: WideString): WideString;
begin
  Result := DefaultInterface.TrimTrail(S);
end;

function  TStString.ValPrep(const S: WideString): WideString;
begin
  Result := DefaultInterface.ValPrep(S);
end;

function  TStString.WordCount(const S: WideString; const WordDelims: WideString): Integer;
begin
  Result := DefaultInterface.WordCount(S, WordDelims);
end;

function  TStString.WordPos(const S: WideString; const WordDelims: WideString; 
                            const AWord: WideString; N: Integer; var Position: Integer): WordBool;
begin
  Result := DefaultInterface.WordPos(S, WordDelims, AWord, N, Position);
end;

function  TStString.WordPosition(N: Integer; const S: WideString; const WordDelims: WideString; 
                                 var Position: Integer): WordBool;
begin
  Result := DefaultInterface.WordPosition(N, S, WordDelims, Position);
end;

procedure TStString.WordWrap(const InSt: WideString; var OutSt: WideString; 
                             var Overlap: WideString; Margin: Integer; PadToMArgin: WordBool);
begin
  DefaultInterface.WordWrap(InSt, OutSt, Overlap, Margin, PadToMArgin);
end;

function  TStString.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

function  TStString.Soundex(const S: WideString): WideString;
begin
  Result := DefaultInterface.Soundex(S);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStStringProperties.Create(AServer: TStString);
begin
  inherited Create;
  FServer := AServer;
end;

function TStStringProperties.GetDefaultInterface: IStString;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoStToHTML.Create: IStToHTML;
begin
  Result := CreateComObject(CLASS_StToHTML) as IStToHTML;
end;

class function CoStToHTML.CreateRemote(const MachineName: string): IStToHTML;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StToHTML) as IStToHTML;
end;

procedure TStToHTML.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{807E3064-CA6F-4DEF-B6F6-BA4DBF4F51DD}';
    IntfIID:   '{9F64CFCF-E50F-4807-A1AA-1E319B0A1D63}';
    EventIID:  '{71DAEE82-3292-4122-8E4A-E1BB58D8D162}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStToHTML.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IStToHTML;
  end;
end;

procedure TStToHTML.ConnectTo(svrIntf: IStToHTML);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TStToHTML.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TStToHTML.GetDefaultInterface: IStToHTML;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStToHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStToHTMLProperties.Create(Self);
{$ENDIF}
end;

destructor TStToHTML.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStToHTML.GetServerProperties: TStToHTMLProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TStToHTML.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
   1: if Assigned(FOnProgress) then
            FOnProgress(Self, Params[0] {Integer});
  end; {case DispID}
end;

function  TStToHTML.Get_CaseSensitve: WordBool;
begin
  Result := DefaultInterface.Get_CaseSensitve;
end;

procedure TStToHTML.Set_CaseSensitve(Value: WordBool);
begin
  DefaultInterface.Set_CaseSensitve(Value);
end;

function  TStToHTML.Get_CommentMarkers: IStStringList;
begin
  Result := DefaultInterface.Get_CommentMarkers;
end;

procedure TStToHTML.Set_CommentMarkers(const Value: IStStringList);
begin
  DefaultInterface.Set_CommentMarkers(Value);
end;

function  TStToHTML.Get_EmbeddedHTML: IStStringList;
begin
  Result := DefaultInterface.Get_EmbeddedHTML;
end;

procedure TStToHTML.Set_EmbeddedHTML(const Value: IStStringList);
begin
  DefaultInterface.Set_EmbeddedHTML(Value);
end;

function  TStToHTML.Get_FixedLineLength: Integer;
begin
  Result := DefaultInterface.Get_FixedLineLength;
end;

procedure TStToHTML.Set_FixedLineLength(Value: Integer);
begin
  DefaultInterface.Set_FixedLineLength(Value);
end;

function  TStToHTML.Get_Keywords: IStStringList;
begin
  Result := DefaultInterface.Get_Keywords;
end;

procedure TStToHTML.Set_Keywords(const Value: IStStringList);
begin
  DefaultInterface.Set_Keywords(Value);
end;

function  TStToHTML.Get_LineTermChar: WideString;
begin
  Result := DefaultInterface.Get_LineTermChar;
end;

procedure TStToHTML.Set_LineTermChar(const Value: WideString);
begin
  DefaultInterface.Set_LineTermChar(Value);
end;

function  TStToHTML.Get_LineTerminator: TStLineTerminator;
begin
  Result := DefaultInterface.Get_LineTerminator;
end;

procedure TStToHTML.Set_LineTerminator(Value: TStLineTerminator);
begin
  DefaultInterface.Set_LineTerminator(Value);
end;

function  TStToHTML.Get_PageFooter: IStStringList;
begin
  Result := DefaultInterface.Get_PageFooter;
end;

procedure TStToHTML.Set_PageFooter(const Value: IStStringList);
begin
  DefaultInterface.Set_PageFooter(Value);
end;

function  TStToHTML.Get_PageHeader: IStStringList;
begin
  Result := DefaultInterface.Get_PageHeader;
end;

procedure TStToHTML.Set_PageHeader(const Value: IStStringList);
begin
  DefaultInterface.Set_PageHeader(Value);
end;

function  TStToHTML.Get_StringMarkers: IStStringList;
begin
  Result := DefaultInterface.Get_StringMarkers;
end;

procedure TStToHTML.Set_StringMarkers(const Value: IStStringList);
begin
  DefaultInterface.Set_StringMarkers(Value);
end;

function  TStToHTML.Get_WordDelimeters: WideString;
begin
  Result := DefaultInterface.Get_WordDelimeters;
end;

procedure TStToHTML.Set_WordDelimeters(const Value: WideString);
begin
  DefaultInterface.Set_WordDelimeters(Value);
end;

function  TStToHTML.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStToHTML.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

procedure TStToHTML.GenerateHTML;
begin
  DefaultInterface.GenerateHTML;
end;

procedure TStToHTML.LoadFromFile(const FileName: WideString);
begin
  DefaultInterface.LoadFromFile(FileName);
end;

procedure TStToHTML.SaveToFile(const FileName: WideString);
begin
  DefaultInterface.SaveToFile(FileName);
end;

procedure TStToHTML.Clear;
begin
  DefaultInterface.Clear;
end;

function  TStToHTML.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStToHTMLProperties.Create(AServer: TStToHTML);
begin
  inherited Create;
  FServer := AServer;
end;

function TStToHTMLProperties.GetDefaultInterface: IStToHTML;
begin
  Result := FServer.DefaultInterface;
end;

function  TStToHTMLProperties.Get_CaseSensitve: WordBool;
begin
  Result := DefaultInterface.Get_CaseSensitve;
end;

procedure TStToHTMLProperties.Set_CaseSensitve(Value: WordBool);
begin
  DefaultInterface.Set_CaseSensitve(Value);
end;

function  TStToHTMLProperties.Get_CommentMarkers: IStStringList;
begin
  Result := DefaultInterface.Get_CommentMarkers;
end;

procedure TStToHTMLProperties.Set_CommentMarkers(const Value: IStStringList);
begin
  DefaultInterface.Set_CommentMarkers(Value);
end;

function  TStToHTMLProperties.Get_EmbeddedHTML: IStStringList;
begin
  Result := DefaultInterface.Get_EmbeddedHTML;
end;

procedure TStToHTMLProperties.Set_EmbeddedHTML(const Value: IStStringList);
begin
  DefaultInterface.Set_EmbeddedHTML(Value);
end;

function  TStToHTMLProperties.Get_FixedLineLength: Integer;
begin
  Result := DefaultInterface.Get_FixedLineLength;
end;

procedure TStToHTMLProperties.Set_FixedLineLength(Value: Integer);
begin
  DefaultInterface.Set_FixedLineLength(Value);
end;

function  TStToHTMLProperties.Get_Keywords: IStStringList;
begin
  Result := DefaultInterface.Get_Keywords;
end;

procedure TStToHTMLProperties.Set_Keywords(const Value: IStStringList);
begin
  DefaultInterface.Set_Keywords(Value);
end;

function  TStToHTMLProperties.Get_LineTermChar: WideString;
begin
  Result := DefaultInterface.Get_LineTermChar;
end;

procedure TStToHTMLProperties.Set_LineTermChar(const Value: WideString);
begin
  DefaultInterface.Set_LineTermChar(Value);
end;

function  TStToHTMLProperties.Get_LineTerminator: TStLineTerminator;
begin
  Result := DefaultInterface.Get_LineTerminator;
end;

procedure TStToHTMLProperties.Set_LineTerminator(Value: TStLineTerminator);
begin
  DefaultInterface.Set_LineTerminator(Value);
end;

function  TStToHTMLProperties.Get_PageFooter: IStStringList;
begin
  Result := DefaultInterface.Get_PageFooter;
end;

procedure TStToHTMLProperties.Set_PageFooter(const Value: IStStringList);
begin
  DefaultInterface.Set_PageFooter(Value);
end;

function  TStToHTMLProperties.Get_PageHeader: IStStringList;
begin
  Result := DefaultInterface.Get_PageHeader;
end;

procedure TStToHTMLProperties.Set_PageHeader(const Value: IStStringList);
begin
  DefaultInterface.Set_PageHeader(Value);
end;

function  TStToHTMLProperties.Get_StringMarkers: IStStringList;
begin
  Result := DefaultInterface.Get_StringMarkers;
end;

procedure TStToHTMLProperties.Set_StringMarkers(const Value: IStStringList);
begin
  DefaultInterface.Set_StringMarkers(Value);
end;

function  TStToHTMLProperties.Get_WordDelimeters: WideString;
begin
  Result := DefaultInterface.Get_WordDelimeters;
end;

procedure TStToHTMLProperties.Set_WordDelimeters(const Value: WideString);
begin
  DefaultInterface.Set_WordDelimeters(Value);
end;

function  TStToHTMLProperties.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStToHTMLProperties.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

{$ENDIF}

class function CoStStringList.Create: IStStringList;
begin
  Result := CreateComObject(CLASS_StStringList) as IStStringList;
end;

class function CoStStringList.CreateRemote(const MachineName: string): IStStringList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StStringList) as IStStringList;
end;

procedure TStStringList.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{1689CC1F-4D2E-409E-AFFD-1EBAC808C569}';
    IntfIID:   '{7A005B00-EE90-4034-B05A-64579C0A7837}';
    EventIID:  '{2457A45F-311D-4E0F-A370-AAD85C6C922E}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStStringList.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IStStringList;
  end;
end;

procedure TStStringList.ConnectTo(svrIntf: IStStringList);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TStStringList.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TStStringList.GetDefaultInterface: IStStringList;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStStringList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStStringListProperties.Create(Self);
{$ENDIF}
end;

destructor TStStringList.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStStringList.GetServerProperties: TStStringListProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TStStringList.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
   1: if Assigned(FOnChange) then
            FOnChange(Self);
   2: if Assigned(FOnChanging) then
            FOnChanging(Self);
  end; {case DispID}
end;

function  TStStringList.Get__NewEnum: IUnknown;
begin
  Result := DefaultInterface.Get__NewEnum;
end;

function  TStStringList.Get_Item(Index: Integer): WideString;
begin
  Result := DefaultInterface.Get_Item(Index);
end;

procedure TStStringList.Set_Item(Index: Integer; const Value: WideString);
begin
  DefaultInterface.Set_Item(Index, Value);
end;

function  TStStringList.Get_CommaText: WideString;
begin
  Result := DefaultInterface.Get_CommaText;
end;

procedure TStStringList.Set_CommaText(const Value: WideString);
begin
  DefaultInterface.Set_CommaText(Value);
end;

function  TStStringList.Get_Count: Integer;
begin
  Result := DefaultInterface.Get_Count;
end;

function  TStStringList.Get_Duplicates: Integer;
begin
  Result := DefaultInterface.Get_Duplicates;
end;

procedure TStStringList.Set_Duplicates(Value: Integer);
begin
  DefaultInterface.Set_Duplicates(Value);
end;

function  TStStringList.Get_Names(Index: Integer): WideString;
begin
  Result := DefaultInterface.Get_Names(Index);
end;

function  TStStringList.Get_Sorted: WordBool;
begin
  Result := DefaultInterface.Get_Sorted;
end;

procedure TStStringList.Set_Sorted(Value: WordBool);
begin
  DefaultInterface.Set_Sorted(Value);
end;

function  TStStringList.Get_Strings(Index: Integer): WideString;
begin
  Result := DefaultInterface.Get_Strings(Index);
end;

procedure TStStringList.Set_Strings(Index: Integer; const Value: WideString);
begin
  DefaultInterface.Set_Strings(Index, Value);
end;

function  TStStringList.Get_Text: WideString;
begin
  Result := DefaultInterface.Get_Text;
end;

procedure TStStringList.Set_Text(const Value: WideString);
begin
  DefaultInterface.Set_Text(Value);
end;

function  TStStringList.Get_Values(const Name: WideString): WideString;
begin
  Result := DefaultInterface.Get_Values(Name);
end;

procedure TStStringList.Set_Values(const Name: WideString; const Value: WideString);
begin
  DefaultInterface.Set_Values(Name, Value);
end;

function  TStStringList.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStStringList.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

function  TStStringList.Add(const S: WideString): Integer;
begin
  Result := DefaultInterface.Add(S);
end;

procedure TStStringList.Append(const S: WideString);
begin
  DefaultInterface.Append(S);
end;

procedure TStStringList.Clear;
begin
  DefaultInterface.Clear;
end;

procedure TStStringList.Delete(Index: Integer);
begin
  DefaultInterface.Delete(Index);
end;

function  TStStringList.Equals(const Strings: IStStringList): WordBool;
begin
  Result := DefaultInterface.Equals(Strings);
end;

procedure TStStringList.Exchange(Index1: Integer; Index2: Integer);
begin
  DefaultInterface.Exchange(Index1, Index2);
end;

function  TStStringList.Find(const S: WideString; var Index: Integer): WordBool;
begin
  Result := DefaultInterface.Find(S, Index);
end;

function  TStStringList.IndexOf(const S: WideString): Integer;
begin
  Result := DefaultInterface.IndexOf(S);
end;

function  TStStringList.IndexOfName(const Name: WideString): Integer;
begin
  Result := DefaultInterface.IndexOfName(Name);
end;

procedure TStStringList.Insert(Index: Integer; const S: WideString);
begin
  DefaultInterface.Insert(Index, S);
end;

procedure TStStringList.LoadFromFile(const FileName: WideString);
begin
  DefaultInterface.LoadFromFile(FileName);
end;

procedure TStStringList.Move(CurIndex: Integer; NewIndex: Integer);
begin
  DefaultInterface.Move(CurIndex, NewIndex);
end;

procedure TStStringList.SaveToFile(const FileName: WideString);
begin
  DefaultInterface.SaveToFile(FileName);
end;

procedure TStStringList.Sort;
begin
  DefaultInterface.Sort;
end;

function  TStStringList.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStStringListProperties.Create(AServer: TStStringList);
begin
  inherited Create;
  FServer := AServer;
end;

function TStStringListProperties.GetDefaultInterface: IStStringList;
begin
  Result := FServer.DefaultInterface;
end;

function  TStStringListProperties.Get__NewEnum: IUnknown;
begin
  Result := DefaultInterface.Get__NewEnum;
end;

function  TStStringListProperties.Get_Item(Index: Integer): WideString;
begin
  Result := DefaultInterface.Get_Item(Index);
end;

procedure TStStringListProperties.Set_Item(Index: Integer; const Value: WideString);
begin
  DefaultInterface.Set_Item(Index, Value);
end;

function  TStStringListProperties.Get_CommaText: WideString;
begin
  Result := DefaultInterface.Get_CommaText;
end;

procedure TStStringListProperties.Set_CommaText(const Value: WideString);
begin
  DefaultInterface.Set_CommaText(Value);
end;

function  TStStringListProperties.Get_Count: Integer;
begin
  Result := DefaultInterface.Get_Count;
end;

function  TStStringListProperties.Get_Duplicates: Integer;
begin
  Result := DefaultInterface.Get_Duplicates;
end;

procedure TStStringListProperties.Set_Duplicates(Value: Integer);
begin
  DefaultInterface.Set_Duplicates(Value);
end;

function  TStStringListProperties.Get_Names(Index: Integer): WideString;
begin
  Result := DefaultInterface.Get_Names(Index);
end;

function  TStStringListProperties.Get_Sorted: WordBool;
begin
  Result := DefaultInterface.Get_Sorted;
end;

procedure TStStringListProperties.Set_Sorted(Value: WordBool);
begin
  DefaultInterface.Set_Sorted(Value);
end;

function  TStStringListProperties.Get_Strings(Index: Integer): WideString;
begin
  Result := DefaultInterface.Get_Strings(Index);
end;

procedure TStStringListProperties.Set_Strings(Index: Integer; const Value: WideString);
begin
  DefaultInterface.Set_Strings(Index, Value);
end;

function  TStStringListProperties.Get_Text: WideString;
begin
  Result := DefaultInterface.Get_Text;
end;

procedure TStStringListProperties.Set_Text(const Value: WideString);
begin
  DefaultInterface.Set_Text(Value);
end;

function  TStStringListProperties.Get_Values(const Name: WideString): WideString;
begin
  Result := DefaultInterface.Get_Values(Name);
end;

procedure TStStringListProperties.Set_Values(const Name: WideString; const Value: WideString);
begin
  DefaultInterface.Set_Values(Name, Value);
end;

function  TStStringListProperties.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStStringListProperties.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

{$ENDIF}

class function CoStRegEx.Create: IStRegEx;
begin
  Result := CreateComObject(CLASS_StRegEx) as IStRegEx;
end;

class function CoStRegEx.CreateRemote(const MachineName: string): IStRegEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegEx) as IStRegEx;
end;

procedure TStRegEx.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{E894200A-226E-4B27-8FB5-320BF9430046}';
    IntfIID:   '{6498218A-2A04-4B0F-AD99-AD1D551997E8}';
    EventIID:  '{25513E60-B981-49F8-AD17-E18E7358163A}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegEx.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IStRegEx;
  end;
end;

procedure TStRegEx.ConnectTo(svrIntf: IStRegEx);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TStRegEx.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TStRegEx.GetDefaultInterface: IStRegEx;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegExProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegEx.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegEx.GetServerProperties: TStRegExProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TStRegEx.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
   1: if Assigned(FOnProgress) then
            FOnProgress(Self, Params[0] {Integer});
  end; {case DispID}
end;

function  TStRegEx.Get_Avoid: WordBool;
begin
  Result := DefaultInterface.Get_Avoid;
end;

procedure TStRegEx.Set_Avoid(Value: WordBool);
begin
  DefaultInterface.Set_Avoid(Value);
end;

function  TStRegEx.Get_IgnoreCase: WordBool;
begin
  Result := DefaultInterface.Get_IgnoreCase;
end;

procedure TStRegEx.Set_IgnoreCase(Value: WordBool);
begin
  DefaultInterface.Set_IgnoreCase(Value);
end;

function  TStRegEx.Get_InFixedLineLength: Integer;
begin
  Result := DefaultInterface.Get_InFixedLineLength;
end;

procedure TStRegEx.Set_InFixedLineLength(Value: Integer);
begin
  DefaultInterface.Set_InFixedLineLength(Value);
end;

function  TStRegEx.Get_InLineTermChar: WideString;
begin
  Result := DefaultInterface.Get_InLineTermChar;
end;

procedure TStRegEx.Set_InLineTermChar(const Value: WideString);
begin
  DefaultInterface.Set_InLineTermChar(Value);
end;

function  TStRegEx.Get_InLineTerminator: TStLineTerminator;
begin
  Result := DefaultInterface.Get_InLineTerminator;
end;

procedure TStRegEx.Set_InLineTerminator(Value: TStLineTerminator);
begin
  DefaultInterface.Set_InLineTerminator(Value);
end;

function  TStRegEx.Get_LineCount: Integer;
begin
  Result := DefaultInterface.Get_LineCount;
end;

function  TStRegEx.Get_LineNumbers: WordBool;
begin
  Result := DefaultInterface.Get_LineNumbers;
end;

procedure TStRegEx.Set_LineNumbers(Value: WordBool);
begin
  DefaultInterface.Set_LineNumbers(Value);
end;

function  TStRegEx.Get_LinesMatched: Integer;
begin
  Result := DefaultInterface.Get_LinesMatched;
end;

function  TStRegEx.Get_LinesPerSecond: Integer;
begin
  Result := DefaultInterface.Get_LinesPerSecond;
end;

function  TStRegEx.Get_LinesReplaced: Integer;
begin
  Result := DefaultInterface.Get_LinesReplaced;
end;

function  TStRegEx.Get_LinesSelected: Integer;
begin
  Result := DefaultInterface.Get_LinesSelected;
end;

function  TStRegEx.Get_MatchPattern: IStStringList;
begin
  Result := DefaultInterface.Get_MatchPattern;
end;

procedure TStRegEx.Set_MatchPattern(const Value: IStStringList);
begin
  DefaultInterface.Set_MatchPattern(Value);
end;

function  TStRegEx.Get_OutFixedLineLength: Integer;
begin
  Result := DefaultInterface.Get_OutFixedLineLength;
end;

procedure TStRegEx.Set_OutFixedLineLength(Value: Integer);
begin
  DefaultInterface.Set_OutFixedLineLength(Value);
end;

function  TStRegEx.Get_OutLineTermChar: WideString;
begin
  Result := DefaultInterface.Get_OutLineTermChar;
end;

procedure TStRegEx.Set_OutLineTermChar(const Value: WideString);
begin
  DefaultInterface.Set_OutLineTermChar(Value);
end;

function  TStRegEx.Get_OutLineTerminitor: Integer;
begin
  Result := DefaultInterface.Get_OutLineTerminitor;
end;

procedure TStRegEx.Set_OutLineTerminitor(Value: Integer);
begin
  DefaultInterface.Set_OutLineTerminitor(Value);
end;

function  TStRegEx.Get_OutputOptions: TStOutputOption;
begin
  Result := DefaultInterface.Get_OutputOptions;
end;

procedure TStRegEx.Set_OutputOptions(Value: TStOutputOption);
begin
  DefaultInterface.Set_OutputOptions(Value);
end;

function  TStRegEx.Get_ReplacePattern: IStStringList;
begin
  Result := DefaultInterface.Get_ReplacePattern;
end;

procedure TStRegEx.Set_ReplacePattern(const Value: IStStringList);
begin
  DefaultInterface.Set_ReplacePattern(Value);
end;

function  TStRegEx.Get_SelAvoidPattern: IStStringList;
begin
  Result := DefaultInterface.Get_SelAvoidPattern;
end;

procedure TStRegEx.Set_SelAvoidPattern(const Value: IStStringList);
begin
  DefaultInterface.Set_SelAvoidPattern(Value);
end;

function  TStRegEx.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStRegEx.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

function  TStRegEx.CheckString(const S: WideString; var StartPos: Integer; var EndPos: Integer; 
                               var Length: Integer): WordBool;
begin
  Result := DefaultInterface.CheckString(S, StartPos, EndPos, Length);
end;

function  TStRegEx.DOSMaskToRegEx(const Masks: WideString): WordBool;
begin
  Result := DefaultInterface.DOSMaskToRegEx(Masks);
end;

function  TStRegEx.Execute: WordBool;
begin
  Result := DefaultInterface.Execute;
end;

procedure TStRegEx.LoadFromFile(const FileName: WideString);
begin
  DefaultInterface.LoadFromFile(FileName);
end;

procedure TStRegEx.SaveToFile(const FileName: WideString);
begin
  DefaultInterface.SaveToFile(FileName);
end;

function  TStRegEx.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegExProperties.Create(AServer: TStRegEx);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegExProperties.GetDefaultInterface: IStRegEx;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegExProperties.Get_Avoid: WordBool;
begin
  Result := DefaultInterface.Get_Avoid;
end;

procedure TStRegExProperties.Set_Avoid(Value: WordBool);
begin
  DefaultInterface.Set_Avoid(Value);
end;

function  TStRegExProperties.Get_IgnoreCase: WordBool;
begin
  Result := DefaultInterface.Get_IgnoreCase;
end;

procedure TStRegExProperties.Set_IgnoreCase(Value: WordBool);
begin
  DefaultInterface.Set_IgnoreCase(Value);
end;

function  TStRegExProperties.Get_InFixedLineLength: Integer;
begin
  Result := DefaultInterface.Get_InFixedLineLength;
end;

procedure TStRegExProperties.Set_InFixedLineLength(Value: Integer);
begin
  DefaultInterface.Set_InFixedLineLength(Value);
end;

function  TStRegExProperties.Get_InLineTermChar: WideString;
begin
  Result := DefaultInterface.Get_InLineTermChar;
end;

procedure TStRegExProperties.Set_InLineTermChar(const Value: WideString);
begin
  DefaultInterface.Set_InLineTermChar(Value);
end;

function  TStRegExProperties.Get_InLineTerminator: TStLineTerminator;
begin
  Result := DefaultInterface.Get_InLineTerminator;
end;

procedure TStRegExProperties.Set_InLineTerminator(Value: TStLineTerminator);
begin
  DefaultInterface.Set_InLineTerminator(Value);
end;

function  TStRegExProperties.Get_LineCount: Integer;
begin
  Result := DefaultInterface.Get_LineCount;
end;

function  TStRegExProperties.Get_LineNumbers: WordBool;
begin
  Result := DefaultInterface.Get_LineNumbers;
end;

procedure TStRegExProperties.Set_LineNumbers(Value: WordBool);
begin
  DefaultInterface.Set_LineNumbers(Value);
end;

function  TStRegExProperties.Get_LinesMatched: Integer;
begin
  Result := DefaultInterface.Get_LinesMatched;
end;

function  TStRegExProperties.Get_LinesPerSecond: Integer;
begin
  Result := DefaultInterface.Get_LinesPerSecond;
end;

function  TStRegExProperties.Get_LinesReplaced: Integer;
begin
  Result := DefaultInterface.Get_LinesReplaced;
end;

function  TStRegExProperties.Get_LinesSelected: Integer;
begin
  Result := DefaultInterface.Get_LinesSelected;
end;

function  TStRegExProperties.Get_MatchPattern: IStStringList;
begin
  Result := DefaultInterface.Get_MatchPattern;
end;

procedure TStRegExProperties.Set_MatchPattern(const Value: IStStringList);
begin
  DefaultInterface.Set_MatchPattern(Value);
end;

function  TStRegExProperties.Get_OutFixedLineLength: Integer;
begin
  Result := DefaultInterface.Get_OutFixedLineLength;
end;

procedure TStRegExProperties.Set_OutFixedLineLength(Value: Integer);
begin
  DefaultInterface.Set_OutFixedLineLength(Value);
end;

function  TStRegExProperties.Get_OutLineTermChar: WideString;
begin
  Result := DefaultInterface.Get_OutLineTermChar;
end;

procedure TStRegExProperties.Set_OutLineTermChar(const Value: WideString);
begin
  DefaultInterface.Set_OutLineTermChar(Value);
end;

function  TStRegExProperties.Get_OutLineTerminitor: Integer;
begin
  Result := DefaultInterface.Get_OutLineTerminitor;
end;

procedure TStRegExProperties.Set_OutLineTerminitor(Value: Integer);
begin
  DefaultInterface.Set_OutLineTerminitor(Value);
end;

function  TStRegExProperties.Get_OutputOptions: TStOutputOption;
begin
  Result := DefaultInterface.Get_OutputOptions;
end;

procedure TStRegExProperties.Set_OutputOptions(Value: TStOutputOption);
begin
  DefaultInterface.Set_OutputOptions(Value);
end;

function  TStRegExProperties.Get_ReplacePattern: IStStringList;
begin
  Result := DefaultInterface.Get_ReplacePattern;
end;

procedure TStRegExProperties.Set_ReplacePattern(const Value: IStStringList);
begin
  DefaultInterface.Set_ReplacePattern(Value);
end;

function  TStRegExProperties.Get_SelAvoidPattern: IStStringList;
begin
  Result := DefaultInterface.Get_SelAvoidPattern;
end;

procedure TStRegExProperties.Set_SelAvoidPattern(const Value: IStStringList);
begin
  DefaultInterface.Set_SelAvoidPattern(Value);
end;

function  TStRegExProperties.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStRegExProperties.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

{$ENDIF}

class function CoStExpr.Create: IStExpr;
begin
  Result := CreateComObject(CLASS_StExpr) as IStExpr;
end;

class function CoStExpr.CreateRemote(const MachineName: string): IStExpr;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StExpr) as IStExpr;
end;

procedure TStExpr.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D0200AD9-8DE6-40C4-AACB-CC12964084FE}';
    IntfIID:   '{C4753F38-E937-4AFE-8D35-2210305B6D19}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStExpr.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStExpr;
  end;
end;

procedure TStExpr.ConnectTo(svrIntf: IStExpr);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStExpr.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStExpr.GetDefaultInterface: IStExpr;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStExpr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStExprProperties.Create(Self);
{$ENDIF}
end;

destructor TStExpr.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStExpr.GetServerProperties: TStExprProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStExpr.AnalyzeExpr(const Expr: WideString): Double;
begin
  Result := DefaultInterface.AnalyzeExpr(Expr);
end;

function  TStExpr.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStExprProperties.Create(AServer: TStExpr);
begin
  inherited Create;
  FServer := AServer;
end;

function TStExprProperties.GetDefaultInterface: IStExpr;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoStMime.Create: IStMime;
begin
  Result := CreateComObject(CLASS_StMime) as IStMime;
end;

class function CoStMime.CreateRemote(const MachineName: string): IStMime;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StMime) as IStMime;
end;

procedure TStMime.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{0EF7D4FF-E095-4C8A-A25C-CED37FF02398}';
    IntfIID:   '{96547B4C-ED9E-4844-8F74-5C9E0268F460}';
    EventIID:  '{0D42F0C6-844D-4541-8BAD-E2BFB6BF6C25}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStMime.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IStMime;
  end;
end;

procedure TStMime.ConnectTo(svrIntf: IStMime);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TStMime.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TStMime.GetDefaultInterface: IStMime;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStMime.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStMimeProperties.Create(Self);
{$ENDIF}
end;

destructor TStMime.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStMime.GetServerProperties: TStMimeProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TStMime.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
   1: if Assigned(FOnProgress) then
            FOnProgress(Self, Params[0] {TStConvertState}, Params[1] {Byte});
   2: if Assigned(FOnSaveAs) then
            FOnSaveAs(Self, Params[0] {var WideString});
  end; {case DispID}
end;

function  TStMime.Get_Attachments: IStStringList;
begin
  Result := DefaultInterface.Get_Attachments;
end;

function  TStMime.Get_Boundry: WideString;
begin
  Result := DefaultInterface.Get_Boundry;
end;

procedure TStMime.Set_Boundry(const Value: WideString);
begin
  DefaultInterface.Set_Boundry(Value);
end;

function  TStMime.Get_Encoding: WideString;
begin
  Result := DefaultInterface.Get_Encoding;
end;

procedure TStMime.Set_Encoding(const Value: WideString);
begin
  DefaultInterface.Set_Encoding(Value);
end;

function  TStMime.Get_ContentDescription: WideString;
begin
  Result := DefaultInterface.Get_ContentDescription;
end;

procedure TStMime.Set_ContentDescription(const Value: WideString);
begin
  DefaultInterface.Set_ContentDescription(Value);
end;

function  TStMime.Get_ContentDisposition: WideString;
begin
  Result := DefaultInterface.Get_ContentDisposition;
end;

procedure TStMime.Set_ContentDisposition(const Value: WideString);
begin
  DefaultInterface.Set_ContentDisposition(Value);
end;

function  TStMime.Get_ContentType: WideString;
begin
  Result := DefaultInterface.Get_ContentType;
end;

procedure TStMime.Set_ContentType(const Value: WideString);
begin
  DefaultInterface.Set_ContentType(Value);
end;

function  TStMime.Get_Directory: WideString;
begin
  Result := DefaultInterface.Get_Directory;
end;

procedure TStMime.Set_Directory(const Value: WideString);
begin
  DefaultInterface.Set_Directory(Value);
end;

function  TStMime.Get_MimeHeaders: WordBool;
begin
  Result := DefaultInterface.Get_MimeHeaders;
end;

procedure TStMime.Set_MimeHeaders(Value: WordBool);
begin
  DefaultInterface.Set_MimeHeaders(Value);
end;

function  TStMime.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStMime.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

procedure TStMime.AddFileAttachment(const FileName: WideString);
begin
  DefaultInterface.AddFileAttachment(FileName);
end;

procedure TStMime.AddStreamAttachment(Stream: OleVariant; const FileName: WideString);
begin
  DefaultInterface.AddStreamAttachment(Stream, FileName);
end;

procedure TStMime.ExtractAttachment(const Attachment: WideString);
begin
  DefaultInterface.ExtractAttachment(Attachment);
end;

procedure TStMime.ExtractAttachmentIndex(Index: Integer);
begin
  DefaultInterface.ExtractAttachmentIndex(Index);
end;

procedure TStMime.ExtractToStream(Index: Integer; Stream: OleVariant);
begin
  DefaultInterface.ExtractToStream(Index, Stream);
end;

procedure TStMime.ExtractAttachments;
begin
  DefaultInterface.ExtractAttachments;
end;

procedure TStMime.GetTag(const Description: WideString);
begin
  DefaultInterface.GetTag(Description);
end;

function  TStMime.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStMimeProperties.Create(AServer: TStMime);
begin
  inherited Create;
  FServer := AServer;
end;

function TStMimeProperties.GetDefaultInterface: IStMime;
begin
  Result := FServer.DefaultInterface;
end;

function  TStMimeProperties.Get_Attachments: IStStringList;
begin
  Result := DefaultInterface.Get_Attachments;
end;

function  TStMimeProperties.Get_Boundry: WideString;
begin
  Result := DefaultInterface.Get_Boundry;
end;

procedure TStMimeProperties.Set_Boundry(const Value: WideString);
begin
  DefaultInterface.Set_Boundry(Value);
end;

function  TStMimeProperties.Get_Encoding: WideString;
begin
  Result := DefaultInterface.Get_Encoding;
end;

procedure TStMimeProperties.Set_Encoding(const Value: WideString);
begin
  DefaultInterface.Set_Encoding(Value);
end;

function  TStMimeProperties.Get_ContentDescription: WideString;
begin
  Result := DefaultInterface.Get_ContentDescription;
end;

procedure TStMimeProperties.Set_ContentDescription(const Value: WideString);
begin
  DefaultInterface.Set_ContentDescription(Value);
end;

function  TStMimeProperties.Get_ContentDisposition: WideString;
begin
  Result := DefaultInterface.Get_ContentDisposition;
end;

procedure TStMimeProperties.Set_ContentDisposition(const Value: WideString);
begin
  DefaultInterface.Set_ContentDisposition(Value);
end;

function  TStMimeProperties.Get_ContentType: WideString;
begin
  Result := DefaultInterface.Get_ContentType;
end;

procedure TStMimeProperties.Set_ContentType(const Value: WideString);
begin
  DefaultInterface.Set_ContentType(Value);
end;

function  TStMimeProperties.Get_Directory: WideString;
begin
  Result := DefaultInterface.Get_Directory;
end;

procedure TStMimeProperties.Set_Directory(const Value: WideString);
begin
  DefaultInterface.Set_Directory(Value);
end;

function  TStMimeProperties.Get_MimeHeaders: WordBool;
begin
  Result := DefaultInterface.Get_MimeHeaders;
end;

procedure TStMimeProperties.Set_MimeHeaders(Value: WordBool);
begin
  DefaultInterface.Set_MimeHeaders(Value);
end;

function  TStMimeProperties.Get_Stream: OleVariant;
begin
  Result := DefaultInterface.Get_Stream;
end;

procedure TStMimeProperties.Set_Stream(Value: OleVariant);
begin
  DefaultInterface.Set_Stream(Value);
end;

{$ENDIF}

class function CoStRegINI.Create: IStRegINI;
begin
  Result := CreateComObject(CLASS_StRegINI) as IStRegINI;
end;

class function CoStRegINI.CreateRemote(const MachineName: string): IStRegINI;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegINI) as IStRegINI;
end;

procedure TStRegINI.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{856EC866-10AE-4AC7-90FF-6C7D5B153064}';
    IntfIID:   '{37198FBE-3932-4548-A6B1-31229E83F87B}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegINI.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStRegINI;
  end;
end;

procedure TStRegINI.ConnectTo(svrIntf: IStRegINI);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStRegINI.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStRegINI.GetDefaultInterface: IStRegINI;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegINI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegINIProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegINI.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegINI.GetServerProperties: TStRegINIProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStRegINI.Get_CurrentSubKey: WideString;
begin
  Result := DefaultInterface.Get_CurrentSubKey;
end;

procedure TStRegINI.Set_CurrentSubKey(const Value: WideString);
begin
  DefaultInterface.Set_CurrentSubKey(Value);
end;

function  TStRegINI.Get_IsIniFile: WordBool;
begin
  Result := DefaultInterface.Get_IsIniFile;
end;

function  TStRegINI.Get_Primary: WideString;
begin
  Result := DefaultInterface.Get_Primary;
end;

procedure TStRegINI.Set_Primary(const Value: WideString);
begin
  DefaultInterface.Set_Primary(Value);
end;

function  TStRegINI.Get_SubKeys: IStRegINISubKeys;
begin
  Result := DefaultInterface.Get_SubKeys;
end;

function  TStRegINI.Get_Values: IStRegINIValues;
begin
  Result := DefaultInterface.Get_Values;
end;

procedure TStRegINI.Open(const RootName: WideString; IsIniFile: WordBool);
begin
  DefaultInterface.Open(RootName, IsIniFile);
end;

procedure TStRegINI.CreateKey(const KeyName: WideString);
begin
  DefaultInterface.CreateKey(KeyName);
end;

procedure TStRegINI.DeleteKey(const KeyName: WideString; DeleteSubKeys: WordBool);
begin
  DefaultInterface.DeleteKey(KeyName, DeleteSubKeys);
end;

procedure TStRegINI.DeleteValue(const ValueName: WideString);
begin
  DefaultInterface.DeleteValue(ValueName);
end;

function  TStRegINI.KeyExists(const KeyName: WideString): WordBool;
begin
  Result := DefaultInterface.KeyExists(KeyName);
end;

function  TStRegINI.QueryKey: IStRegINIQueryKeyInfo;
begin
  Result := DefaultInterface.QueryKey;
end;

function  TStRegINI.ReadBoolean(const ValueName: WideString; Default: WordBool): WordBool;
begin
  Result := DefaultInterface.ReadBoolean(ValueName, Default);
end;

function  TStRegINI.ReadDate(const ValueName: WideString; Default: TDateTime): TDateTime;
begin
  Result := DefaultInterface.ReadDate(ValueName, Default);
end;

function  TStRegINI.ReadDateTime(const ValueName: WideString; Default: TDateTime): TDateTime;
begin
  Result := DefaultInterface.ReadDateTime(ValueName, Default);
end;

function  TStRegINI.ReadInteger(const ValueName: WideString; Default: Integer): Integer;
begin
  Result := DefaultInterface.ReadInteger(ValueName, Default);
end;

function  TStRegINI.ReadString(const ValueName: WideString; const Default: WideString): WideString;
begin
  Result := DefaultInterface.ReadString(ValueName, Default);
end;

function  TStRegINI.ReadTime(const ValueName: WideString; Default: TDateTime): TDateTime;
begin
  Result := DefaultInterface.ReadTime(ValueName, Default);
end;

procedure TStRegINI.WriteBoolean(const ValueName: WideString; Value: WordBool);
begin
  DefaultInterface.WriteBoolean(ValueName, Value);
end;

procedure TStRegINI.WriteDate(const ValueName: WideString; Value: TDateTime);
begin
  DefaultInterface.WriteDate(ValueName, Value);
end;

procedure TStRegINI.WriteDateTime(const ValueName: WideString; Value: TDateTime);
begin
  DefaultInterface.WriteDateTime(ValueName, Value);
end;

procedure TStRegINI.WriteInteger(const ValueName: WideString; Value: Integer);
begin
  DefaultInterface.WriteInteger(ValueName, Value);
end;

procedure TStRegINI.WriteString(const ValueName: WideString; const Value: WideString);
begin
  DefaultInterface.WriteString(ValueName, Value);
end;

procedure TStRegINI.WriteTime(const ValueName: WideString; Value: TDateTime);
begin
  DefaultInterface.WriteTime(ValueName, Value);
end;

function  TStRegINI.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegINIProperties.Create(AServer: TStRegINI);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegINIProperties.GetDefaultInterface: IStRegINI;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegINIProperties.Get_CurrentSubKey: WideString;
begin
  Result := DefaultInterface.Get_CurrentSubKey;
end;

procedure TStRegINIProperties.Set_CurrentSubKey(const Value: WideString);
begin
  DefaultInterface.Set_CurrentSubKey(Value);
end;

function  TStRegINIProperties.Get_IsIniFile: WordBool;
begin
  Result := DefaultInterface.Get_IsIniFile;
end;

function  TStRegINIProperties.Get_Primary: WideString;
begin
  Result := DefaultInterface.Get_Primary;
end;

procedure TStRegINIProperties.Set_Primary(const Value: WideString);
begin
  DefaultInterface.Set_Primary(Value);
end;

function  TStRegINIProperties.Get_SubKeys: IStRegINISubKeys;
begin
  Result := DefaultInterface.Get_SubKeys;
end;

function  TStRegINIProperties.Get_Values: IStRegINIValues;
begin
  Result := DefaultInterface.Get_Values;
end;

{$ENDIF}

class function CoStFin.Create: IStFin;
begin
  Result := CreateComObject(CLASS_StFin) as IStFin;
end;

class function CoStFin.CreateRemote(const MachineName: string): IStFin;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StFin) as IStFin;
end;

procedure TStFin.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{8CD51FD5-514E-4CB0-9121-9B4B21B3A320}';
    IntfIID:   '{821B289E-492C-4115-8484-A6545E281BB3}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStFin.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStFin;
  end;
end;

procedure TStFin.ConnectTo(svrIntf: IStFin);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStFin.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStFin.GetDefaultInterface: IStFin;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStFin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStFinProperties.Create(Self);
{$ENDIF}
end;

destructor TStFin.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStFin.GetServerProperties: TStFinProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStFin.IsCardValid(const S: WideString): WordBool;
begin
  Result := DefaultInterface.IsCardValid(S);
end;

function  TStFin.License(const Key: WideString): WordBool;
begin
  Result := DefaultInterface.License(Key);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStFinProperties.Create(AServer: TStFin);
begin
  inherited Create;
  FServer := AServer;
end;

function TStFinProperties.GetDefaultInterface: IStFin;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoStRegINIQueryKeyInfo.Create: IStRegINIQueryKeyInfo;
begin
  Result := CreateComObject(CLASS_StRegINIQueryKeyInfo) as IStRegINIQueryKeyInfo;
end;

class function CoStRegINIQueryKeyInfo.CreateRemote(const MachineName: string): IStRegINIQueryKeyInfo;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegINIQueryKeyInfo) as IStRegINIQueryKeyInfo;
end;

procedure TStRegINIQueryKeyInfo.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{33B067CF-275F-444C-A63B-E98ADEE1D1B2}';
    IntfIID:   '{DF09E023-FC82-4BC0-BEA8-AA5D5C0E9F2A}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegINIQueryKeyInfo.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStRegINIQueryKeyInfo;
  end;
end;

procedure TStRegINIQueryKeyInfo.ConnectTo(svrIntf: IStRegINIQueryKeyInfo);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStRegINIQueryKeyInfo.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStRegINIQueryKeyInfo.GetDefaultInterface: IStRegINIQueryKeyInfo;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegINIQueryKeyInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegINIQueryKeyInfoProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegINIQueryKeyInfo.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegINIQueryKeyInfo.GetServerProperties: TStRegINIQueryKeyInfoProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStRegINIQueryKeyInfo.Get_QIClassName: WideString;
begin
  Result := DefaultInterface.Get_QIClassName;
end;

function  TStRegINIQueryKeyInfo.Get_QIFileTime: TDateTime;
begin
  Result := DefaultInterface.Get_QIFileTime;
end;

function  TStRegINIQueryKeyInfo.Get_QIKey: Integer;
begin
  Result := DefaultInterface.Get_QIKey;
end;

function  TStRegINIQueryKeyInfo.Get_QIMaxCNLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxCNLen;
end;

function  TStRegINIQueryKeyInfo.Get_QIMaxDataLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxDataLen;
end;

function  TStRegINIQueryKeyInfo.Get_QIMaxSKNLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxSKNLen;
end;

function  TStRegINIQueryKeyInfo.Get_QIMaxVNLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxVNLen;
end;

function  TStRegINIQueryKeyInfo.Get_QINumSubKeys: Integer;
begin
  Result := DefaultInterface.Get_QINumSubKeys;
end;

function  TStRegINIQueryKeyInfo.Get_QINumValues: Integer;
begin
  Result := DefaultInterface.Get_QINumValues;
end;

function  TStRegINIQueryKeyInfo.Get_QISDescLen: Integer;
begin
  Result := DefaultInterface.Get_QISDescLen;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegINIQueryKeyInfoProperties.Create(AServer: TStRegINIQueryKeyInfo);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegINIQueryKeyInfoProperties.GetDefaultInterface: IStRegINIQueryKeyInfo;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIClassName: WideString;
begin
  Result := DefaultInterface.Get_QIClassName;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIFileTime: TDateTime;
begin
  Result := DefaultInterface.Get_QIFileTime;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIKey: Integer;
begin
  Result := DefaultInterface.Get_QIKey;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIMaxCNLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxCNLen;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIMaxDataLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxDataLen;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIMaxSKNLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxSKNLen;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QIMaxVNLen: Integer;
begin
  Result := DefaultInterface.Get_QIMaxVNLen;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QINumSubKeys: Integer;
begin
  Result := DefaultInterface.Get_QINumSubKeys;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QINumValues: Integer;
begin
  Result := DefaultInterface.Get_QINumValues;
end;

function  TStRegINIQueryKeyInfoProperties.Get_QISDescLen: Integer;
begin
  Result := DefaultInterface.Get_QISDescLen;
end;

{$ENDIF}

class function CoStRegINISubKeys.Create: IStRegINISubKeys;
begin
  Result := CreateComObject(CLASS_StRegINISubKeys) as IStRegINISubKeys;
end;

class function CoStRegINISubKeys.CreateRemote(const MachineName: string): IStRegINISubKeys;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegINISubKeys) as IStRegINISubKeys;
end;

procedure TStRegINISubKeys.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{A4FD4C76-1147-46C9-862B-4C3A5D246D38}';
    IntfIID:   '{43B798DA-D2CA-4263-854D-E63FBB2F7C64}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegINISubKeys.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStRegINISubKeys;
  end;
end;

procedure TStRegINISubKeys.ConnectTo(svrIntf: IStRegINISubKeys);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStRegINISubKeys.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStRegINISubKeys.GetDefaultInterface: IStRegINISubKeys;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegINISubKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegINISubKeysProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegINISubKeys.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegINISubKeys.GetServerProperties: TStRegINISubKeysProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStRegINISubKeys.Get__NewEnum: IUnknown;
begin
  Result := DefaultInterface.Get__NewEnum;
end;

function  TStRegINISubKeys.Get_Item(Index: Integer): IStRegINISubKey;
begin
  Result := DefaultInterface.Get_Item(Index);
end;

function  TStRegINISubKeys.Get_Count: Integer;
begin
  Result := DefaultInterface.Get_Count;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegINISubKeysProperties.Create(AServer: TStRegINISubKeys);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegINISubKeysProperties.GetDefaultInterface: IStRegINISubKeys;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegINISubKeysProperties.Get__NewEnum: IUnknown;
begin
  Result := DefaultInterface.Get__NewEnum;
end;

function  TStRegINISubKeysProperties.Get_Item(Index: Integer): IStRegINISubKey;
begin
  Result := DefaultInterface.Get_Item(Index);
end;

function  TStRegINISubKeysProperties.Get_Count: Integer;
begin
  Result := DefaultInterface.Get_Count;
end;

{$ENDIF}

class function CoStRegINIValue.Create: IStRegINIValue;
begin
  Result := CreateComObject(CLASS_StRegINIValue) as IStRegINIValue;
end;

class function CoStRegINIValue.CreateRemote(const MachineName: string): IStRegINIValue;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegINIValue) as IStRegINIValue;
end;

procedure TStRegINIValue.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{C5B752C4-113B-4887-92CC-5C30B158E502}';
    IntfIID:   '{A3E5FB23-CD5C-471B-8214-5075375DE8DF}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegINIValue.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStRegINIValue;
  end;
end;

procedure TStRegINIValue.ConnectTo(svrIntf: IStRegINIValue);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStRegINIValue.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStRegINIValue.GetDefaultInterface: IStRegINIValue;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegINIValue.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegINIValueProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegINIValue.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegINIValue.GetServerProperties: TStRegINIValueProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStRegINIValue.Get_Value: WideString;
begin
  Result := DefaultInterface.Get_Value;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegINIValueProperties.Create(AServer: TStRegINIValue);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegINIValueProperties.GetDefaultInterface: IStRegINIValue;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegINIValueProperties.Get_Value: WideString;
begin
  Result := DefaultInterface.Get_Value;
end;

{$ENDIF}

class function CoStRegINIValues.Create: IStRegINIValues;
begin
  Result := CreateComObject(CLASS_StRegINIValues) as IStRegINIValues;
end;

class function CoStRegINIValues.CreateRemote(const MachineName: string): IStRegINIValues;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegINIValues) as IStRegINIValues;
end;

procedure TStRegINIValues.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{50210282-7730-48FC-A9EC-6BBAD8B3915A}';
    IntfIID:   '{FC9E57F0-ABC0-4B69-80A0-06CDACB61AEB}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegINIValues.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStRegINIValues;
  end;
end;

procedure TStRegINIValues.ConnectTo(svrIntf: IStRegINIValues);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStRegINIValues.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStRegINIValues.GetDefaultInterface: IStRegINIValues;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegINIValues.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegINIValuesProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegINIValues.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegINIValues.GetServerProperties: TStRegINIValuesProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStRegINIValues.Get__NewEnum: IUnknown;
begin
  Result := DefaultInterface.Get__NewEnum;
end;

function  TStRegINIValues.Get_Item(Index: Integer): IStRegINIValue;
begin
  Result := DefaultInterface.Get_Item(Index);
end;

function  TStRegINIValues.Get_Count: Integer;
begin
  Result := DefaultInterface.Get_Count;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegINIValuesProperties.Create(AServer: TStRegINIValues);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegINIValuesProperties.GetDefaultInterface: IStRegINIValues;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegINIValuesProperties.Get__NewEnum: IUnknown;
begin
  Result := DefaultInterface.Get__NewEnum;
end;

function  TStRegINIValuesProperties.Get_Item(Index: Integer): IStRegINIValue;
begin
  Result := DefaultInterface.Get_Item(Index);
end;

function  TStRegINIValuesProperties.Get_Count: Integer;
begin
  Result := DefaultInterface.Get_Count;
end;

{$ENDIF}

class function CoStRegINISubKey.Create: IStRegINISubKey;
begin
  Result := CreateComObject(CLASS_StRegINISubKey) as IStRegINISubKey;
end;

class function CoStRegINISubKey.CreateRemote(const MachineName: string): IStRegINISubKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StRegINISubKey) as IStRegINISubKey;
end;

procedure TStRegINISubKey.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{0C2EECA2-08E5-47C2-AABF-F091F71427CC}';
    IntfIID:   '{BBA14CDE-6BDE-4E40-86DD-E04697595AF3}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStRegINISubKey.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStRegINISubKey;
  end;
end;

procedure TStRegINISubKey.ConnectTo(svrIntf: IStRegINISubKey);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStRegINISubKey.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStRegINISubKey.GetDefaultInterface: IStRegINISubKey;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TStRegINISubKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStRegINISubKeyProperties.Create(Self);
{$ENDIF}
end;

destructor TStRegINISubKey.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStRegINISubKey.GetServerProperties: TStRegINISubKeyProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TStRegINISubKey.Get_Value: WideString;
begin
  Result := DefaultInterface.Get_Value;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStRegINISubKeyProperties.Create(AServer: TStRegINISubKey);
begin
  inherited Create;
  FServer := AServer;
end;

function TStRegINISubKeyProperties.GetDefaultInterface: IStRegINISubKey;
begin
  Result := FServer.DefaultInterface;
end;

function  TStRegINISubKeyProperties.Get_Value: WideString;
begin
  Result := DefaultInterface.Get_Value;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Servers',[TStDate, TStString, TStToHTML, TStStringList, 
    TStRegEx, TStExpr, TStMime, TStRegINI, TStFin, 
    TStRegINIQueryKeyInfo, TStRegINISubKeys, TStRegINIValue, TStRegINIValues, TStRegINISubKey]);
end;

end.
