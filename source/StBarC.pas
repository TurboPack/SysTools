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
 * This rework is by Michael Geddes (at Striven) (c) 2025..2026
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StBarC.pas 4.04                             *}
{*********************************************************}
{* SysTools: bar code components                         *}
{*********************************************************}

{$I StDefine.inc}

unit StBarC;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Types,
  Vcl.Controls;

// STBase ------
type
  EStException = class(Exception)     {ancestor to all SysTools exceptions}
    protected {private}
      FErrorCode : Longint;
    public
      constructor CreateResTP(Ident : LongInt; Dummy : Word);
      constructor CreateResFmtTP(Ident : Longint; const Args : array of const;
                                 Dummy : Word);
      property ErrorCode : LongInt
        read FErrorCode
        write FErrorCode;
  end;
  EStExceptionClass = class of EStException;
  EStBarCodeError = class(EStException);     {BarCode exception}

  {: Simple stack-based record based array container.
    Designed to easily add/remove without a whole heap of
    extra functionality.
  }
  TArrayOf<T> = record
  public
  type
    TArrType = TArray<T>;
    PArray = ^TArrType;
  private
    FArray : TArrType;
    FCount : integer;

    procedure Grow(ANewCount : integer);
    function GetValue(AIdx : Integer): T;
    procedure SetValue(AIdx : Integer; const NewVal: T);
    const _GrowNumber = 10;
  public
    constructor Create(ACap : integer);
    class operator Initialize(out AArray : TArrayOf<T>);

    function Append(const AVal : T) : integer; overload;
    function Append(const AArr : TArrayOf<T>) : integer; overload;

    function Add(const AVal : T) : integer; inline;

    function AsArray : TArray<T>;
    property Value[AIdx : Integer] : T read GetValue write SetValue; default;
    property Count : integer read FCount;

    procedure Clear;

    procedure Delete(AIdx : integer);

  type
    TEnumerator = record
    private
      FPArray: PArray;
      FPCount : PInteger;
      FIndex: Integer;
      function DoGetCurrent: T;
      constructor Create(AArray : PArray; ACount : PInteger);
    public
      property Current: T read DoGetCurrent;
      function MoveNext: Boolean;
    end;

    function GetEnumerator: TEnumerator;
  end;


// -------------

// StConst -----
const
  {barcode errors}
  stscInvalidUPCACodeLen    = 140;
  stscInvalidCharacter      = 141;
  stscInvalidCheckCharacter = 142;
  stscInvalidUPCECodeLen    = 143;
  stscInvalidEAN8CodeLen    = 144;
  stscInvalidEAN13CodeLen   = 145;
  stscInvalidSupCodeLen     = 146;
resourcestring
  stscInvalidUPCACodeLenS        = 'Invalid code length (must be 11 or 12)';
  stscInvalidCharacterS          = 'Invalid character';
  stscInvalidCheckCharacterS     = 'Invalid check character';
  stscInvalidUPCECodeLenS        = 'Invalid code length (must be 6, 7 or 8)';
  stscInvalidEAN8CodeLenS        = 'Invalid code length (must be 7 or 8)';
  stscInvalidEAN13CodeLenS       = 'Invalid code length (must be 12 or 13)';
  stscInvalidSupCodeLenS         = 'Invalid supplemental code length (must be 2 or 5)';
type
  StStrRec = record
    ID: Integer;
    Str: string;
  end;
const
  SysToolsStrArray : array [0..6] of StStrRec = (
  {barcode errors}
 (ID: stscInvalidUPCACodeLen; Str: stscInvalidUPCACodeLenS),
 (ID: stscInvalidCharacter; Str: stscInvalidCharacterS),
 (ID: stscInvalidCheckCharacter; Str: stscInvalidCheckCharacterS),
 (ID: stscInvalidUPCECodeLen; Str: stscInvalidUPCECodeLenS),
 (ID: stscInvalidEAN8CodeLen; Str: stscInvalidEAN8CodeLenS),
 (ID: stscInvalidEAN13CodeLen; Str: stscInvalidEAN13CodeLenS),
 (ID: stscInvalidSupCodeLen; Str: stscInvalidSupCodeLenS)
 );
// -------------


// Files
type
  TStBarKind = (bkSpace, bkBar, bkThreeQuarterBar, bkHalfBar, bkGuard, bkSupplement, bkBlankSpace);
  TStBarKindSet = set of TStBarKind;
  TStDigitArray = record
  private
    FBytes : TArrayOf<Byte>;
    function rItem(AIdx : integer): Byte;
    procedure wItem(AIdx : integer; NewVal: Byte);
  public
    property Item[AIdx : integer] : Byte read rItem write wItem; default;
  end;


  TStBarData = record
  public
    Kind    : TStBarKindSet;
    Modules : Integer;
  end;

{$SCOPEDENUMS ON}
  TStTextSize = ( Small, Normal);
{$SCOPEDENUMS OFF}
  TStTextData = record
    Text : String;
    BarIdx : integer;
    Size : TStTextSize;
  end;

  TStBarCodeInfo = record
  private
    FBars       : TArrayOf<TStBarData>;
    FText       : TArrayOf<TStTextData>;

    function GetBars(Index : Integer) : TStBarData;
    function GetCount : Integer;
    function GetTextData(Index : integer): TStTextData;
    function GetTextItemCount: integer;

  public

    procedure Add(ModuleCount : Integer; BarKind : TStBarKindSet);
    procedure AddText(AText : String;ASize : TStTextSize = TStTextSize.Normal);
    procedure AddDigit(ADigit : word;ASize : TStTextSize = TStTextSize.Normal);
    procedure Clear;

    property Bars[Index : Integer] : TStBarData read GetBars; default;

    property Count : Integer read GetCount;

    property TextItems[Index : integer] : TStTextData read GetTextData;
    property TextItemCount : integer read GetTextItemCount;
  end;

  {$SCOPEDENUMS ON}
  TStBarCodeType = (UPC_A, UPC_E, EAN_8, EAN_13,
                    Interleaved2of5, Codabar, Code11,
                    Code39, Code93, Code128, Code128C, Code128A, Code128B);
  TStBarCodeTypes = set of TStBarCodeType;

  TStBarCodeTypeHelper = record helper for TStBarCodeType
  public
    type
      TDesc = record
        C, N : String;
      end;
    const
      _desc : Array[TStBarCodeType] of TDesc = (
        (C:'UPCA'; N: 'UPC-A';),
        (C:'UPCE'; N: 'UPC-E';),
        (C:'EAN8'; N: 'EAN-8';),
        (C:'EAN13';N: 'EAN-13';),
        (C:'I25';  N: 'interleaved 2 of 5';),
        (C:'CBR';  N: 'codabar';),
        (C:'11';  N: 'code 11';),
        (C:'39';  N: 'code 39';),
        (C:'93';  N: 'code 93';),
        (C:'128';  N: 'Code 128';),
        (C:'128C';  N: 'Code 128-C';),
        (C:'128A';  N: 'Code 128-A';),
        (C:'128B';  N: 'Code 128-B';)
        );
    function AsCode : String;
    function AsDesc : String;

    class function AsEnumFromCode(const ACode : String; var AVal : TStBarCodeType) : boolean; static;
    class function AsEnumFromDesc(const ACode : String; var AVal : TStBarCodeType) : boolean; static;
  end;
  TStAutoSelect = (WithoutCheck, WithCheck, Both);

  TStCheckResult = (None, Match, Added, Mismatch, Invalid);
  {$SCOPEDENUMS OFF}


  // Compat with old.
  TBarCodeType = (
    bcAny,     { choose best-fit }
    bcEAN,
    bcUPC,        { upc = 12-digit ean }
    bcISBN,       { isbn numbers (still EAN13) }
    bc39,         { code 39 }
    bc128,        { code 128 (a,b,c: autoselection) }
    bc128C,       { code 128 (compact form for digits) }
    bc128B,       { code 128, full printable ascii }
    bcI25,        { interleaved 2 of 5 (only digits) }
    bcCBR,        { Codabar  }
    bc93,         { code 93  }
    bcEANCS,
    bcUPCCS,
    bcISBNCS,
    bcCBRCS,
    bcWithChecksum
  );
  TBarCodeTypes = set of TBarCodeType;


  // Canvas - output in mm.
  TStBarcodeCanvas  = class
  public
    // procedure SetPenWidth( width: Double); virtual; abstract;
    procedure DrawLine( X0, Y0, X1, Y1 : Double); virtual; abstract;
    procedure SetFontSize( Size : Double); virtual; abstract;
    procedure TextOut( X, Y : Double ; txt : string ); virtual; abstract;
    function GetTextExtents(AText : String) : TPointF; virtual; abstract;
  end;

  TStBarcode = class(TGraphicControl)
  protected
  const
    bcMaxBarCodeLen = 255;
    bcGuardBarAbove = False;
    bcGuardBarBelow = True;
    bcDefNarrowToWideRatio = 2;
    _MMPerInch = 25.4;
    _PointPerMM = 0.352778;

    _Code128 : TStBarCodeTypes = [TStBarcodeType.Code128, TStBarcodeType.Code128A, TStBarcodeType.Code128B, TStBarcodeType.Code128C];
    _FontSizeMM : array[TStTextSize] of double = ( 2.7, 3.5);

  var
    {property variables}
    FAddCheckChar     : Boolean;
    FBarCodeType      : TStBarCodeType;
    FBarToSpaceRatio  : Double;
    FBarNarrowToWideRatio : Integer;
    FBarWidth         : Double;         {in mils}
    FBearerBars       : Boolean;
    FShowCode         : Boolean;
    FShowGuardChars   : Boolean;
    FSupplementalCode : string;
    FTallGuardBars    : Boolean;
    FExtendedSyntax   : Boolean;
    FText : string;

    {internal variables}
    bcBarInfo        : TStBarCodeInfo;
    bcBarModWidth    : double; {width of single bar}
    bcCheckK         : Integer; {"K" check character for use by Code11}
    bcDigits         : TStDigitArray;
    bcDigitCount     : Integer;
    bcSpaceModWidth  : Double; {width of empty space between bars}
    bcNormalWidth    : double;
    bcSpaceWidth     : double;
    bcSupplementWidth: double;
    bcDirty          : boolean;

    {property methods}
    function GetCode : string;
    procedure SetAddCheckChar(Value : Boolean);
    procedure SetBarCodeType(Value : TStBarCodeType);
    procedure SetBarToSpaceRatio(Value : Double);
    procedure SetBarNarrowToWideRatio(Value: Integer);
    procedure SetBarWidth(Value : Double);
    procedure SetBearerBars(Value : Boolean);
    procedure SetCode(const Value : string);
    procedure SetExtendedSyntax (const v : Boolean);
    procedure SetShowCode(Value : Boolean);
    procedure SetShowGuardChars(Value : Boolean);
    procedure SetSupplementalCode(const Value : string);
    procedure SetTallGuardBars(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure CalcBarCode;
    procedure CalcBarCodeWidth;
    procedure IncrementBarPosn( const AData : TStBarData; var normalWidth, spaceWidth, supplementWidth : double);
    // procedure DrawBarCode(const R : TRect);
    function GetDigits(Characters : string) : Integer; overload;
    function GetDigits(AType : TStBarCodeType; Characters : string; var ADigits : TStDigitArray) : Integer; overload;
    class function GetDigits(AType : TStBarCodeType; Characters : string; AExtendedSyntax : boolean; var ADigits : TStDigitArray) : Integer; overload; static;
    function SmallestLineWidth: Double;
    class function DoGetCheckCharacters(AType : TStBarCodeType; const ADigits : TStDigitArray; ALen : integer; var C, K : integer) : TStCheckResult; static;
  {$SCOPEDENUMS ON}
    type
      TEanMode = (Standard, Complement);
  {$SCOPEDENUMS OFF}
    class function EANChecksum(const ADigits : TStDigitArray; ALen : Integer; AMode : TEanMode) : integer; static;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;

    procedure Invalidate; override;

    function GetCheckCharacters(const S : string; var C, K : Integer) : TStCheckResult; overload;
    function GetCheckCharacters(AType : TStBarCodeType; const S : string; var C, K : Integer) : TStCheckResult; overload;
    class function GetCheckCharacters(AType : TStBarCodeType; const S : string; var C, K : Integer; AExtendedSyntax : boolean = false) : TStCheckResult; overload; static;
    function GetCheckCharacters(var C, K : Integer) : TStCheckResult; overload;

    class function ValidateCheckDigits(AType : TStBarCodeType; AExtSyntax : boolean; Const ABarcode : String) : boolean; static;
    function GetBarCodeWidth(AScale : Double = 1) : Double; overload;
    function GetBarCodeWidth(var ACanCompress : boolean; AScale : Double = 1) : Double; overload;
    function Validate(DisplayError : Boolean) : Boolean;

    procedure DrawBarCode(ACanvas : TStBarcodeCanvas; const R : TRectF; AScale : Double = 1;ACenter : boolean = true);

    function SelectBarcodeType(AType : TStBarCodeTypes; ACSMode: TStAutoSelect) : boolean; overload;

    function SelectBarcodeType( ATypes :  TBarCodeTypes) : boolean; overload;

  public
    {properties}

    property AddCheckChar : Boolean read FAddCheckChar write SetAddCheckChar;

    property BarCodeType : TStBarCodeType read FBarCodeType write SetBarCodeType;

    property BarToSpaceRatio : Double read FBarToSpaceRatio write SetBarToSpaceRatio;

    property BarNarrowToWideRatio : Integer read FBarNarrowToWideRatio write SetBarNarrowToWideRatio default bcDefNarrowToWideRatio;

    //: The width of a barcode line element.
    property BarWidth : Double read FBarWidth write SetBarWidth;

    property BearerBars : Boolean read FBearerBars write SetBearerBars;

    property Code : string read GetCode write SetCode;

    property ExtendedSyntax : Boolean read FExtendedSyntax write SetExtendedSyntax default False;

    property ShowCode : Boolean read FShowCode write SetShowCode;

    property ShowGuardChars : Boolean read FShowGuardChars write SetShowGuardChars;

    property SupplementalCode : string read FSupplementalCode write SetSupplementalCode;

    property TallGuardBars : Boolean read FTallGuardBars write SetTallGuardBars;

  end;

implementation

uses
  System.Math;

// StBase ------
procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : LongInt);
var
  E : EStException;
begin
  E := ExceptionClass.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;
function SysToolsStr(Index : Integer) : string;
var
  i : Integer;
begin
  for i := Low(SysToolsStrArray) to High(SysToolsStrArray) do
    if SysToolsStrArray[i].ID = Index then
      Result := SysToolsStrArray[i].Str;
end;
constructor EStException.CreateResTP(Ident : LongInt; Dummy : Word);
begin
  inherited Create(SysToolsStr(Ident));
end;
constructor EStException.CreateResFmtTP(Ident : Longint; const Args : array of const; Dummy : Word);
begin
  inherited CreateFmt(SysToolsStr(Ident), Args);
end;
// -------------

const
  {left and right codes for UPC_A}
  UPC_A_LeftHand : array[0..9] of string =
    ('0001101', {0}
     '0011001', {1}
     '0010011', {2}
     '0111101', {3}
     '0100011', {4}
     '0110001', {5}
     '0101111', {6}
     '0111011', {7}
     '0110111', {8}
     '0001011'  {9} );

  UPC_A_RightHand : array[0..9] of string =
    ('1110010', {0}
     '1100110', {1}
     '1101100', {2}
     '1000010', {3}
     '1011100', {4}
     '1001110', {5}
     '1010000', {6}
     '1000100', {7}
     '1001000', {8}
     '1110100'  {9} );

const
  UPC_E_OddParity : array[0..9] of string =
    ('0001101', {0}
     '0011001', {1}
     '0010011', {2}
     '0111101', {3}
     '0100011', {4}
     '0110001', {5}
     '0101111', {6}
     '0111011', {7}
     '0110111', {8}
     '0001011'  {9} );

  UPC_E_EvenParity : array[0..9] of string =
    ('0100111', {0}
     '0110011', {1}
     '0011011', {2}
     '0100001', {3}
     '0011101', {4}
     '0111001', {5}
     '0000101', {6}
     '0010001', {7}
     '0001001', {8}
     '0010111'  {9} );

const
  EAN_LeftHandA : array[0..9] of string =
    ('0001101', {0}
     '0011001', {1}
     '0010011', {2}
     '0111101', {3}
     '0100011', {4}
     '0110001', {5}
     '0101111', {6}
     '0111011', {7}
     '0110111', {8}
     '0001011'  {9} );

  EAN_LeftHandB : array[0..9] of string =
    ('0100111', {0}
     '0110011', {1}
     '0011011', {2}
     '0100001', {3}
     '0011101', {4}
     '0111001', {5}
     '0000101', {6}
     '0010001', {7}
     '0001001', {8}
     '0010111'  {9} );

const
  Interleaved_2of5 : array[0..9] of string =
    ('00110', {0}
     '10001', {1}
     '01001', {2}
     '11000', {3}
     '00101', {4}
     '10100', {5}
     '01100', {6}
     '00011', {7}
     '10010', {8}
     '01010'  {9} );

const
  Codabar : array[0..19] of string =
     {BSBSBSB}      {bar-space-bar-space-bar...}
    ('0000011', {0}
     '0000110', {1}
     '0001001', {2}
     '1100000', {3}
     '0010010', {4}
     '1000010', {5}
     '0100001', {6}
     '0100100', {7}
     '0110000', {8}
     '1001000', {9}
     '0001100', {-}
     '0011000', { $}
     '1000101', {:}
     '1010001', {/}
     '1010100', {.}
     '0010101', {+}
     '0011010', {A}
     '0101001', {B}
     '0001011', {C}
     '0001110'  {D});

const
  Code11 :  array[0..11] of string =
     {BSBSB}    {bar-space-bar-space-bar...} {0-narrow, 1-wide}
    ('00001',   {0}
     '10001',   {1}
     '01001',   {2}
     '11000',   {3}
     '00101',   {4}
     '10100',   {5}
     '01100',   {6}
     '00011',   {7}
     '10010',   {8}
     '10000',   {9}
     '00100',   {-}
     '00110');  {stop character}

const
  Code39 : array[0..43] of string =
     {BSBSBSBSB}      {bar-space-bar-space-bar...} {0-narrow, 1-wide}
    ('000110100',   {0}
     '100100001',   {1}
     '001100001',   {2}
     '101100000',   {3}
     '000110001',   {4}
     '100110000',   {5}
     '001110000',   {6}
     '000100101',   {7}
     '100100100',   {8}
     '001100100',   {9}
     '100001001',   {A}
     '001001001',   {B}
     '101001000',   {C}
     '000011001',   {D}
     '100011000',   {E}
     '001011000',   {F}
     '000001101',   {G}
     '100001100',   {H}
     '001001100',   {I}
     '000011100',   {J}
     '100000011',   {K}
     '001000011',   {L}
     '101000010',   {M}
     '000010011',   {N}
     '100010010',   {O}
     '001010010',   {P}
     '000000111',   {Q}
     '100000110',   {R}
     '001000110',   {S}
     '000010110',   {T}
     '110000001',   {U}
     '011000001',   {V}
     '111000000',   {W}
     '010010001',   {X}
     '110010000',   {Y}
     '011010000',   {Z}
     '010000101',   {-}
     '110000100',   {.}
     '011000100',   {SPACE}
     '010101000',   { $}
     '010100010',   {/}
     '010001010',   {+}
     '000101010',   {%}
     '010010100');  {*}

const
  Code93 : array[0..46] of string =
     {BSBSBS}    {bar-space-bar-space-bar...} {0-narrow, 1-wide}
    ('131112',   {0}
     '111213',   {1}
     '111312',   {2}
     '111411',   {3}
     '121113',   {4}
     '121212',   {5}
     '121311',   {6}
     '111114',   {7}
     '131211',   {8}
     '141111',   {9}
     '211113',   {A}
     '211212',   {B}
     '211311',   {C}
     '221112',   {D}
     '221211',   {E}
     '231111',   {F}
     '112113',   {G}
     '112212',   {H}
     '112311',   {I}
     '122112',   {J}
     '132111',   {K}
     '111123',   {L}
     '111222',   {M}
     '111321',   {N}
     '121122',   {O}
     '131121',   {P}
     '212112',   {Q}
     '212211',   {R}
     '211122',   {S}
     '211221',   {T}
     '221121',   {U}
     '222111',   {V}
     '112122',   {W}
     '112221',   {X}
     '122121',   {Y}
     '123111',   {Z}
     '121131',   {-}
     '311112',   {.}
     '311211',   {SPACE}
     '321111',   { $}
     '112131',   {/}
     '113121',   {+}
     '211131',   {%}
     '121221',   {($)}
     '312111',   {(%)}
     '311121',   {(/)}
     '122211');  {(+)}

  Code93Map : array[#0..#127] of string =
  {Circle Code}   {ASCII Code 93 }
    ('%U',        {NL     (%)U   }
     '$A',        {SH     ($)A   }
     '$B',        {SX     ($)B   }
     '$C',        {EX     ($)C   }
     '$D',        {ET     ($)D   }
     '$E',        {EQ     ($)E   }
     '$F',        {AK     ($)F   }
     '$G',        {BL     ($)G   }
     '$H',        {BS     ($)H   }
     '$I',        {HT     ($)I   }
     '$J',        {LF     ($)J   }
     '$K',        {VT     ($)K   }
     '$L',        {FF     ($)L   }
     '$M',        {CR     ($)M   }
     '$N',        {SO     ($)N   }
     '$O',        {SI     ($)O   }
     '$P',        {DL     ($)P   }
     '$Q',        {D1     ($)Q   }
     '$R',        {D2     ($)R   }
     '$S',        {D3     ($)S   }
     '$T',        {D4     ($)T   }
     '$U',        {NK     ($)U   }
     '$V',        {SY     ($)V   }
     '$W',        {EB     ($)W   }
     '$X',        {CN     ($)X   }
     '$Y',        {EM     ($)Y   }
     '$Z',        {SB     ($)Z   }
     '%A',        {EC     (%)A   }
     '%B',        {FS     (%)B   }
     '%C',        {GS     (%)C   }
     '%D',        {RS     (%)D   }
     '%E',        {US     (%)E   }
      ' ',        {Space   Space }
     '/A',        {!      (/)A   }
     '/B',        {"      (/)B   }
     '/C',        {#      (/)C   }
      '$',        { $   (/)D or $}
      '%',        {%    (/)E or %}
     '/F',        {&      (/)F   }
     '/G',        {'      (/)G   }
     '/H',        {(      (/)H   }
     '/I',        {)      (/)I   }
     '/J',        {*      (/)J   }
     ' +',        {+    (/)K or +}
     '/L',        {,      (/)L   }
      '-',        {-    (/)M or -}
      '.',        {.    (/)N or .}
      '/',        {/    (/)O or /}
      '0',        {0    (/)P or 0}
      '1',        {1    (/)Q or 1}
      '2',        {2    (/)R or 2}
      '3',        {3    (/)S or 3}
      '4',        {4    (/)T or 4}
      '5',        {5    (/)U or 5}
      '6',        {6    (/)V or 6}
      '7',        {7    (/)W or 7}
      '8',        {8    (/)X or 8}
      '9',        {9    (/)Y or 9}
     '/Z',        {:      (/)Z   }
     '%F',        {;      (%)F   }
     '%G',        {<      (%)G   }
     '%H',        {=      (%)H   }
     '%I',        {>      (%)I   }
     '%J',        {?      (%)J   }
     '%V',        {       (%)V   }
      'A',        {A   	    A    }
      'B',        {B	    B    }
      'C',        {C	    C    }
      'D',        {D	    D    }
      'E',        {E	    E    }
      'F',        {F	    F    }
      'G',        {G	    G    }
      'H',        {H	    H    }
      'I',        {I	    I    }
      'J',        {J	    J    }
      'K',        {K	    K    }
      'L',        {L	    L    }
      'M',        {M	    M    }
      'N',        {N	    N    }
      'O',        {O	    O    }
      'P',        {P	    P    }
      'Q',        {Q	    Q    }
      'R',        {R	    R    }
      'S',        {S	    S    }
      'T',        {T        T    }
      'U',        {U	    U    }
      'V',        {V	    V    }
      'W',        {W	    W    }
      'X',        {X	    X    }
      'Y',        {Y	    Y    }
      'Z',        {Z	    Z    }
     '%K',        {[  	   (%)K  }
     '%L',        {\  	   (%)L  }
     '%M',        {]  	   (%)M  }
     '%N',        {^  	   (%)N  }
     '%O',        {_  	   (%)O  }
     '%W',        {`       (%)W  }
     '+A',        {a  	   (+)A  }
     '+B',        {b  	   (+)B  }
     '+C',        {c  	   (+)C  }
     '+D',        {d  	   (+)D  }
     '+E',        {e  	   (+)E  }
     '+F',        {f  	   (+)F  }
     '+G',        {g  	   (+)G  }
     '+H',        {h  	   (+)H  }
     '+I',        {i  	   (+)I  }
     '+J',        {j  	   (+)J  }
     '+K',        {k  	   (+)K  }
     '+L',        {l  	   (+)L  }
     '+M',        {m  	   (+)M  }
     '+N',        {n  	   (+)N  }
     '+O',        {o  	   (+)O  }
     '+P',        {p  	   (+)P  }
     '+Q',        {q  	   (+)Q  }
     '+R',        {r  	   (+)R  }
     '+S',        {s  	   (+)S  }
     '+T',        {t  	   (+)T  }
     '+U',        {u  	   (+)U  }
     '+V',        {v  	   (+)V  }
     '+W',        {w  	   (+)W  }
     '+X',        {x  	   (+)X  }
     '+Y',        {y  	   (+)Y  }
     '+Z',        {z  	   (+)Z  }
     '%P',        {{  	   (%)P  }
     '%Q',        {|  	   (%)Q  }
     '%R',        {}{	   (%)R  }
     '%S',        {~  	   (%)S  }
     '%T');       { DEL    (%)T  }

const
  Code128 : array[0..106] of string =
     {BSBSBS}   {Value  CodeA  CodeB   CodeC}
    ('212222',  {0	SPACE	SPACE	00}
     '222122',  {1	!	!	01}
     '222221',  {2	"	"	02}
     '121223',  {3	#	#	03}
     '121322',  {4	$	$	04}
     '131222',  {5	%	%	05}
     '122213',  {6	&	&	06}
     '122312',  {7	'	'	07}
     '132212',  {8	(	(	08}
     '221213',  {9	)	)	09}
     '221312',  {10	* 	*	10}
     '231212',  {11	+	+	11}
     '112232',  {12	,	,	12}
     '122132',  {13	-	-	13}
     '122231',  {14	.	.	14}
     '113222',  {15	/	/	15}
     '123122',  {16	0	0	16}
     '123221',  {17	1	1	17}
     '223211',  {18	2	2	18}
     '221132',  {19	3	3	19}
     '221231',  {20	4	4	20}
     '213212',  {21	5	5	21}
     '223112',  {22	6	6	22}
     '312131',  {23	7	7	23}
     '311222',  {24	8	8	24}
     '321122',  {25	9	9	25}
     '321221',  {26	:	:	26}
     '312212',  {27	;	;	27}
     '322112',  {28	<	<	28}
     '322211',  {29	= 	= 	29}
     '212123',  {30	>	>	30}
     '212321',  {31	?	?	31}
     '232121',  {32	@	@	32}
     '111323',  {33	A	A	33}
     '131123',  {34	B	B	34}
     '131321',  {35	C	C	35}
     '112313',  {36	D	D	36}
     '132113',  {37	E	E	37}
     '132311',  {38	F	F	38}
     '211313',  {39	G	G	39}
     '231113',  {40	H	H	40}
     '231311',  {41	I	I	41}
     '112133',  {42	J	J	42}
     '112331',  {43	K	K	43}
     '132131',  {44	L	L	44}
     '113123',  {45	M	M	45}
     '113321',  {46	N	N	46}
     '133121',  {47	O	O	47}
     '313121',  {48	P	P	48}
     '211331',  {49	Q	Q	49}
     '231131',  {50	R	R	50}
     '213113',  {51	S	S	51}
     '213311',  {52	T	T	52}
     '213131',  {53	U	U	53}
     '311123',  {54	V	V	54}
     '311321',  {55	W	W	55}
     '331121',  {56	X	X	56}
     '312113',  {57	Y	Y	57}
     '312311',  {58	Z	Z	58}
     '332111',  {59	[	[	59}
     '314111',  {60	\	\	60}
     '221411',  {61	]	]	61}
     '431111',  {62	^	^	62}
     '111224',  {63	_ 	_ 	63}
     '111422',  {64	NU	`	64}
     '121124',  {65	SH	a	65}
     '121421',  {66	SX	b	66}
     '141122',  {67	EX	c	67}
     '141221',  {68	ET	d	68}
     '112214',  {69	EQ	e	69}
     '112412',  {70	AK	f	70}
     '122114',  {71	BL	g	71}
     '122411',  {72	BS	h	72}
     '142112',  {73	HT	i	73}
     '142211',  {74	LF	j	74}
     '241211',  {75	VT	k	75}
     '221114',  {76	FF	l	76}
     '413111',  {77	CR	m	77}
     '241112',  {78	SO	n	78}
     '134111',  {79	SI	o	79}
     '111242',  {80	DL	p	80}
     '121142',  {81	D1	q	81}
     '121241',  {82	D2	r	82}
     '114212',  {83	D3	s	83}
     '124112',  {84	D4	t	84}
     '124211',  {85	NK	u	85}
     '411212',  {86	SY	v	86}
     '421112',  {87	EB	w	87}
     '421211',  {88	CN	x	88}
     '212141',  {89	EM	y	89}
     '214121',  {90	SB	z	90}
     '412121',  (*91	EC	{	91*)
     '111143',  {92	FS		92}
     '111341',  (*93	GS	}	93*)
     '131141',  {94	RS	~	94}
     '114113',  {95	US	DEL	95}
     '114311',  {96	FNC 3	FNC 3	96}      {use #132}
     '411113',  {97	FNC 2	FNC 2	97}      {use #131}
     '411311',  {98	SHIFT	SHIFT	98}      {use #130}
     '113141',  {99	CODE C	CODE C	99}      {use #135}
     '114131',  {100	CODE B	FNC 4	CODE B}  {use #134}
     '311141',  {101	FNC 4	CODE A	CODE A}  {use #133}
     '411131',  {102	FNC 1	FNC 1	FNC 1 }  {use #130}
     '211412',  {103	CODE A}                  {use #136}
     '211214',  {104	CODE B}                  {use #137}
     '211232',  {105	CODE C}                  {use #138}
     '2331112');{106    STOP}                    {use #139}

  CUPCE_Mirror0 : array[0..9] of string = (
    'EEEOOO', 'EEOEOO', 'EEOOEO', 'EEOOOE', 'EOEEOO',
    'EOOEEO', 'EOOOEE', 'EOEOEO', 'EOEOOE', 'EOOEOE'
  );
  CUPCE_Mirror1 : array[0..9] of string = (
    'OOOEEE', 'OOEOEE', 'OOEEOE', 'OOEEEO', 'OEOOEE',
    'OEEOOE', 'OEEEOO', 'OEOEOE', 'OEOEEO', 'OEEOEO'
  );
  CEAN_Handedness : array[0..9] of string = (
    {EAN refers to this as the 13th digit - counting from the right}
    'AAAAAA', 'AABABB', 'AABBAB', 'AABBBA', 'ABAABB',
    'ABBAAB', 'ABBBAA', 'ABABAB', 'ABABBA', 'ABBABA'
  );
  CEAN_SupCodeParitySmall: array[0..3] of string = (
    'OO', 'OE', 'EO', 'EE'
  );

  CEAN_SupCodeParity : array[0..9] of string = (
   'EEOOO', 'EOEOO', 'EOOEO', 'EOOOE', 'OEEOO',
   'OOEEO', 'OOOEE', 'OEOEO', 'OEOOE', 'OOEOE'
  );

{*** helper routines ***}

function RectWidth(const R : TRect) : Integer;
begin
  Result := R.Right-R.Left;
end;

function RectHeight(const R : TRect) : Integer;
begin
  Result := R.Bottom-R.Top;
end;

function TStDigitArray.rItem(AIdx : integer): Byte;
begin
  if AIdx > FBytes.Count then
    result := 0
  else
    result := FBytes[AIdx-1];
end;

procedure TStDigitArray.wItem(AIdx : integer; NewVal: Byte);
begin
  Dec(AIdx);
  while AIdx >= FBytes.Count do
    FBytes.Add(0);
  FBytes[AIdx] := newVal;
end;


{*** TStBarCodeInfo ***}

procedure TStBarCodeInfo.Add(ModuleCount : Integer; BarKind : TStBarKindSet);
var
  Bar : TStBarData;
begin
  Bar.Modules := ModuleCount;
  Bar.Kind := BarKind;
  FBars.Add(Bar);
end;

procedure TStBarCodeInfo.AddText(AText : String;ASize : TStTextSize = TStTextSize.Normal);
var
  txt : TStTextData;
begin
  txt.Text := AText;
  txt.BarIdx := FBars.Count;
  txt.Size := ASize;
  FText.Add(txt);
end;

procedure TStBarCodeInfo.AddDigit(ADigit : word;ASize : TStTextSize = TStTextSize.Normal);
begin
  AddText(char(ord('0')+ADigit), ASize);
end;

procedure TStBarCodeInfo.Clear;
begin
  FBars.Clear;
  FText.Clear;
end;

function TStBarCodeInfo.GetBars(Index : Integer) : TStBarData;
begin
  Result := FBars[Index];
end;

function TStBarCodeInfo.GetCount : Integer;
begin
  Result := FBars.Count;
end;

function TStBarCodeInfo.GetTextData(Index : integer): TStTextData;
begin
  result := FText[Index];
end;

function TStBarCodeInfo.GetTextItemCount: integer;
begin
  result := FText.Count;
end;

function ChToInt(AChar : Char) : Integer; inline;
begin
  result := Ord(AChar) - ord('0');
  if (result < 0) or (result > 9) then
    raise EConvertError.CreateFmt('''%s'' is not a valid integer', [AChar]);
end;
function ChMatches(AChar : Char; AVal : integer) : boolean; inline;
begin
  result := (Ord(AChar) - ord('0')) = AVal;
end;

{*** TStBarcode ***}

procedure TStBarcode.CalcBarCode;
var
  I, J, X : Integer;
  CheckC  : Integer;
  CheckK  : Integer;
  CSP     : string;
  C       : string;
  C1, C2  : string;

  procedure AddCode(const S : string; AKind : TStBarKindSet);
  var
    I : Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] = '0' then
        bcBarInfo.Add(1, AKind - [bkBar, bkThreeQuarterBar, bkHalfBar] + [bkSpace])
      else
        bcBarInfo.Add(ChToInt(S[I]), AKind);
  end;

  procedure AddECode(const Parity : string);
  var
    I : Integer;
  begin
    for I := 1 to Length(Parity) do
    begin
      var digit := bcDigits[I+1];
      if FShowCode then
        bcBarInfo.AddDigit(digit);
      if Parity[I] = 'E' then
        AddCode(UPC_E_EvenParity[digit], [bkBar])
      else
        AddCode(UPC_E_OddParity[digit], [bkBar]);
    end;
  end;

  procedure AddSupCode(const Parity : string);
  var
    I : Integer;
  begin
    for I := 1 to Length(Parity) do
    begin
      if Parity[I] = 'E' then
        AddCode(UPC_E_EvenParity[bcDigits[I]], [bkThreeQuarterBar, bkSupplement])
      else
        AddCode(UPC_E_OddParity[bcDigits[I]], [bkThreeQuarterBar, bkSupplement]);
      if I < Length(Parity) then
        AddCode('01', [bkThreeQuarterBar, bkSupplement]);
    end;
  end;

  procedure AddCodeModules(const S : string);
  var
    K : Integer;
  begin
    for K := 1 to Length(S) do
    begin
      if Odd(K) then
        bcBarInfo.Add(ChToInt(S[K]), [bkBar])
      else
        bcBarInfo.Add(ChToInt(S[K]), [bkSpace]);
    end;
  end;

  procedure AddCodeWideNarrow(const S : string);
  var
    K : Integer;
  begin
    for K := 1 to Length(S) do
    begin
      case S[K] of
        '0' : if Odd(K) then
                bcBarInfo.Add(1, [bkBar])
              else
                bcBarInfo.Add(1, [bkSpace]);
        '1' : if Odd(K) then
                bcBarInfo.Add(FBarNarrowToWideRatio, [bkBar])
              else
                bcBarInfo.Add(FBarNarrowToWideRatio, [bkSpace]);
      end;
    end;
  end;

const
  _TextSpace = 8;
begin

  if FBarcodeType = TStBarcodeType.Code128 then
    SelectBarcodeType([TStBarcodeType.Code128], TStAutoSelect.WithoutCheck);

  bcBarInfo.Clear;
  if Code = '' then
    Exit;

  {get copy of code}
  C := Code;

  {get digits}
  case FBarCodeType of
    TStBarcodeType.UPC_A, TStBarcodeType.UPC_E,
    TStBarcodeType.EAN_8, TStBarcodeType.EAN_13,
    TStBarcodeType.Codabar, TStBarcodeType.Code11, TStBarcodeType.Code93:
      begin
        bcDigitCount := GetDigits(C);
      end;
    TStBarcodeType.Interleaved2of5 :
      begin
        {adjust odd length code}
        if FAddCheckChar then
        begin
          if not Odd(Length(C)) then
            C := '0' + C;
        end
        else if Odd(Length(C)) then
          C := '0' + C;
        bcDigitCount := GetDigits(C);
      end;
    TStBarcodeType.Code39 :
      begin
        {add guard characters}
        if C[1] <> '*' then
          C := '*' + C;
        if C[Length(C)] <> '*' then
          C := C + '*';
        bcDigitCount := GetDigits(C);
      end;
    TStBarcodeType.Code128A,
    TStBarcodeType.Code128B,
    TStBarcodeType.Code128C,
    TStBarcodeType.Code128 :
      begin
        {add start code}
        case C[1] of
          #136, #137, #138: ;
        else
          case FBarcodeType of
          TStBarcodeType.Code128,
          TStBarcodeType.Code128A: C := #136 + C;
          TStBarcodeType.Code128B: C := #137 + C;
          TStBarcodeType.Code128C: C := #138 + C;
          end;
        end;
        bcDigitCount := GetDigits(C);
      end;
  end;

  case FBarCodeType of
    TStBarcodeType.UPC_A :
      begin
        {get check digit}
        if Length(C) = 11 then
          GetCheckCharacters(C, CheckC, CheckK)
        else
          CheckC := bcDigits[12];

        if FShowCode then
        begin
          bcBarInfo.AddDigit(bcDigits[1], TStTextSize.Small);
          bcBarInfo.Add(_TextSpace,[bkSpace,bkBlankSpace]);
        end;
        {encode left hand guard bars}
        AddCode('101', [bkGuard, bkBar]);

        {first six characters as left hand characters}
        AddCode(UPC_A_LeftHand[bcDigits[1]], [bkBar, bkGuard]);
        for I := 2 to 6 do
        begin
          if FShowCode then
            bcBarInfo.AddDigit(bcDigits[I]);
          AddCode(UPC_A_LeftHand[bcDigits[I]], [bkBar]);
        end;

        {center guard pattern}
        AddCode('01010', [bkGuard, bkBar]);

        {last five data characters as right hand characters}
        for I := 7 to 11 do
        begin
          if FShowCode then
            bcBarInfo.AddDigit(bcDigits[I]);
          AddCode(UPC_A_RightHand[bcDigits[I]], [bkBar]);
        end;

        {check character}
        AddCode(UPC_A_RightHand[CheckC], [bkBar]);

        {encode right hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
        if FShowCode then
        begin
          bcBarInfo.AddDigit(CheckC, TStTextSize.Small);
          bcBarInfo.Add(_TextSpace,[bkSpace,bkBlankSpace]);
        end;
      end;
    TStBarcodeType.UPC_E :
      begin
        {encode left hand guard bars, 101}
        if FShowCode then
        begin
          bcBarInfo.AddDigit(bcDigits[1], TStTextSize.Small);
          bcBarInfo.Add(_TextSpace,[bkSpace,bkBlankSpace]);
        end;
        AddCode('101', [bkGuard, bkBar]);
        GetCheckCharacters(C, CheckC, CheckK);

        if bcDigits[1] = 0 then
          AddECode(CUPCE_Mirror0[CheckC])
        else
          AddECode(CUPCE_Mirror1[CheckC]);

        {encode right hand guard bars}
        AddCode('010101', [bkGuard, bkBar]);
        if FShowCode then
        begin
          bcBarInfo.AddDigit(CheckC);
          bcBarInfo.Add(_TextSpace,[bkSpace,bkBlankSpace]);
        end;
      end;
    TStBarcodeType.EAN_8   :
      begin
        {get check digit}
        if Length(C) = 7 then
          GetCheckCharacters(C, CheckC, CheckK)
        else
          CheckC := bcDigits[8];

        {encode left hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
        {two flag two data characters, encoded as left hand A characters}
        for I := 1 to 4 do
        begin
          if FShowCode then
            bcBarInfo.AddDigit(bcDigits[I]);
          AddCode(EAN_LeftHandA[bcDigits[I]], [bkBar]);
        end;
        {encode center guard bars}
        AddCode('01010', [bkGuard, bkBar]);
        {last three data characters, encoded as right hand characters}
        for I := 5 to 7 do
        begin
          if FShowCode then
            bcBarInfo.AddDigit(bcDigits[I]);
          AddCode(UPC_A_RightHand[bcDigits[I]], [bkBar]);
        end;
        {check character}
        if FShowCode then
          bcBarInfo.AddDigit(CheckC, TStTextSize.Small);
        AddCode(UPC_A_RightHand[CheckC], [bkBar]);
        {encode right hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
      end;
    TStBarcodeType.EAN_13  :
      begin
        {get check digit}
        if Length(C) = 12 then
          GetCheckCharacters(C, CheckC, CheckK)
        else
          CheckC := bcDigits[13];

        {determine which left hand table to use based on first flag character}
        {EAN refers to this as the 13th digit - counting from the right}
        CSP := CEAN_Handedness[bcDigits[1]];

        if FShowCode then
        begin
          bcBarInfo.AddDigit(bcDigits[1]);
          bcBarInfo.Add(_TextSpace,[bkSpace,bkBlankSpace]);
        end;

        {encode left hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
        {start with second flag character and next five data characters}
        for I := 2 to 7 do
        begin
          if FShowCode then
            bcBarInfo.AddDigit(bcDigits[I]);
          if CSP[I-1] = 'A' then
            AddCode(EAN_LeftHandA[bcDigits[I]], [bkBar])
          else
            AddCode(EAN_LeftHandB[bcDigits[I]], [bkBar]);
        end;
        {encode center guard bars}
        AddCode('01010', [bkGuard, bkBar]);
        {encode last five data characters}
        for I := 8 to 12 do
        begin
          if FShowCode then
            bcBarInfo.AddDigit(bcDigits[I]);
          AddCode(UPC_A_RightHand[bcDigits[I]], [bkBar]);
        end;

        if FShowCode then
          bcBarInfo.AddDigit(CheckC);

        {check character}
        AddCode(UPC_A_RightHand[CheckC], [bkBar]);
        {encode right hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
      end;
    TStBarcodeType.Interleaved2of5 :
      begin
        {add check character}
        if FAddCheckChar then
        begin
          {get check digit}
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
        end;

        {encode left guard pattern}
        bcBarInfo.Add(1, [bkGuard, bkBar]);
        bcBarInfo.Add(1, [bkGuard, bkSpace]);
        bcBarInfo.Add(1, [bkGuard, bkBar]);
        bcBarInfo.Add(1, [bkGuard, bkSpace]);

        I := 1;
        while I < bcDigitCount do
        begin
          {take two characters at a time - odd as bars, even as spaces}
          C1 := Interleaved_2of5[bcDigits[I]];
          C2 := Interleaved_2of5[bcDigits[I+1]];
          {interleave data}
          for J := 1 to 5 do
          begin
            if FShowCode then
            begin
              if J = 1 then
                bcBarInfo.AddDigit(bcDigits[i])
              else if  j = 3 then
                bcBarInfo.AddDigit(bcDigits[i+1]);
            end;
            if C1[J] = '1' then
              bcBarInfo.Add(FBarNarrowToWideRatio, [bkBar]) {wide bar}
            else
              bcBarInfo.Add(1, [bkBar]);   {narrow bar}
            if C2[J] = '1' then
              bcBarInfo.Add(FBarNarrowToWideRatio, [bkSpace]){wide space}
            else
              bcBarInfo.Add(1, [bkSpace]); {narrow space}
          end;
          Inc(I, 2);
        end;

        {encode right guard pattern}
        bcBarInfo.Add(FBarNarrowToWideRatio,
          [bkGuard, bkBar]); {double-width bar}
        bcBarInfo.Add(1, [bkGuard, bkSpace]);
        bcBarInfo.Add(1, [bkGuard, bkBar]);
      end;
    TStBarcodeType.Codabar :
      begin
        for I := 1 to bcDigitCount do
        begin
          bcBarInfo.AddText(Code[i]);
          AddCodeWideNarrow(Codabar[bcDigits[I]]);
          if I < bcDigitCount then
            bcBarInfo.Add(1, [bkSpace]);
        end;
      end;
    TStBarcodeType.Code11 :
      begin
        AddCodeWideNarrow(Code11[11]);  {start}
        bcBarInfo.Add(1, [bkSpace]);
        {add check characters}
        if FAddCheckChar then
        begin
          {get check digits}
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckK;
        end;

        for I := 1 to bcDigitCount do
        begin
          bcBarInfo.AddText(Code[i]);
          AddCodeWideNarrow(Code11[bcDigits[I]]);
          bcBarInfo.Add(1, [bkSpace]);
        end;
        AddCodeWideNarrow(Code11[11]);  {stop}
      end;
    TStBarcodeType.Code39 :
      begin
        for I := 1 to bcDigitCount do
        begin
          bcBarInfo.AddText(Code[i]);
          C1 := Code39[bcDigits[I]];
          for J := 1 to Length(C1) do
          begin
            case C1[J] of
              '0' : if Odd(J) then
                      bcBarInfo.Add(1, [bkBar])
                    else
                      bcBarInfo.Add(1, [bkSpace]);
              '1' : if Odd(J) then
                      bcBarInfo.Add(2, [bkBar])
                    else
                      bcBarInfo.Add(2, [bkSpace]);
            end;
          end;
          bcBarInfo.Add(1, [bkSpace]);
        end;
      end;
    TStBarcodeType.Code93 :
      begin;
        {start character}
        AddCodeModules('111141');
        {add check characters}
        if FAddCheckChar then
        begin
          {get check digits}
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckK;
        end;
        for I := 1 to bcDigitCount do
        begin
          if FShowCode then
            bcBarInfo.AddText(Code[i]);
          AddCodeModules(Code93[bcDigits[I]]);
        end;
        {stop character}
        AddCodeModules('1111411');
      end;
    TStBarcodeType.Code128A,
    TStBarcodeType.Code128B,
    TStBarcodeType.Code128C,
    TStBarcodeType.Code128 :
      begin
        {add check character}
        if FAddCheckChar then
        begin
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
        end;
        {add stop code}
        Inc(bcDigitCount);
        bcDigits[bcDigitCount] := 106;
        for I  := 1 to bcDigitCount do
        begin
          AddCodeModules(Code128[bcDigits[I]]);
        end;
      end;
  end;

  if FBarCodeType in [TStBarcodeType.UPC_A, TStBarcodeType.UPC_E, TStBarcodeType.EAN_8, TStBarcodeType.EAN_13] then
  begin
    {add supplemental encodings if requested}
    if Length(FSupplementalCode) in [2, 5] then
    begin
      {get digits}
      bcDigitCount := GetDigits(FSupplementalCode);
      {7 spaces after primary code - 0000000}
      AddCode('0000000', [bkThreeQuarterBar, bkBlankSpace]);
      {encode left hand guard bars, 1011}
      AddCode('1011', [bkThreeQuarterBar, bkSupplement]);

      if bcDigitCount = 2 then
      begin
        {two digit supplement}
        {determine parity table to use for each of the two characters}
        X := bcDigits[1] * 10 + bcDigits[2];
        AddSupCode(CEAN_SupCodeParitySmall[X mod 4]);
      end
      else
      begin
        {five digit supplement}
        {determine the parity pattern to use for each of the five}
        X :=  ((bcDigits[1] + bcDigits[3] + bcDigits[5])*3 + (bcDigits[2] + bcDigits[4])*9) mod 10;
        AddSupCode(CEAN_SupCodeParity[X]);
      end;
    end;
  end;
  bcDirty := false;
end;

procedure incf(var AVal :double; AIncBy : Double);
begin
  AVal := AVal + AIncBy;
end;
procedure decf(var AVal :double; AIncBy : Double);
begin
  AVal := AVal - AIncBy;
end;

procedure TStBarcode.IncrementBarPosn( const AData : TStBarData; var normalWidth, spaceWidth, supplementWidth : double);
begin
  //
  if bkSpace in AData.Kind then
  begin
    if bkBlankSpace in AData.Kind then
      Incf(spaceWidth, bcSpaceModWidth*AData.Modules)
    else if bkSupplement in AData.Kind then
      Incf(supplementWidth, bcSpaceModWidth*AData.Modules)
    else
      Incf(normalWidth, bcSpaceModWidth*AData.Modules)
  end
  else
  begin
    if bkBlankSpace in AData.Kind then
      Incf(spaceWidth, bcBarModWidth*AData.Modules)
    else if bkSupplement in AData.Kind then
      Incf(supplementWidth, bcBarModWidth*AData.Modules)
    else
      Incf(normalWidth, bcBarModWidth*AData.Modules)
  end;
end;

procedure TStBarcode.CalcBarCodeWidth;
var
  I : Integer;
begin
  if bcDirty or (FBarcodeType = TStBarcodeType.Code128) then
    CalcBarCode;

  bcNormalWidth := 0;
  bcSpaceWidth := 0;
  bcSupplementWidth := 0;
  for I := 0 to bcBarInfo.Count-1 do
    IncrementBarPosn(bcBarInfo[I], bcNormalWidth, bcSpaceWidth, bcSupplementWidth);
end;

constructor TStBarcode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Code := '';
end;

procedure TStBarcode.AfterConstruction;
begin
  inherited;
  FAddCheckChar := True;
  FBarToSpaceRatio := 1;
  FBarNarrowToWideRatio := bcDefNarrowToWideRatio;
  FBarWidth := 0.38; // 0.35 normal minimum
  FShowCode := True;
  FShowGuardChars := False;
  FTallGuardBars := true;
  FExtendedSyntax := False;
end;

function TStBarcode.SelectBarcodeType( ATypes :  TBarCodeTypes) : boolean;
begin
  if bcAny in ATypes then
    ATypes := ATypes + [bcEAN,bcUPC, bc128];
  if bcWithChecksum in ATypes then
  begin
    if bcEAN in ATypes then
    begin
      Exclude(ATypes, bcEAN);
      Include(Atypes, bcEANCS);
    end;
    if bcUPC in ATypes then
    begin
      Exclude(ATypes, bcUPC);
      Include(Atypes, bcUPCCS);
    end;
    if bcISBN in ATypes then
    begin
      Exclude(ATypes, bcISBN);
      Include(Atypes, bcISBNCS);
    end;
    if bcCBR in ATypes then
    begin
      Exclude(ATypes, bcCBR);
      Include(Atypes, bcCBRCS);
    end;
  end;

  // Check for matching checksums
  var bcTypes :  TStBarCodeTypes := [];
  for var bc in ATypes do
  begin
    case bc of
      bcEANCS: bcTypes := bcTypes + [TStBarcodeType.EAN_8,TStBarcodeType.EAN_13];
      bcUPCCS: bcTypes := bcTypes + [TStBarcodeType.UPC_A, TStBarcodeType.UPC_E];
      bcISBNCS:Include(bcTypes, TStBarcodeType.EAN_13);
      bcCBRCS: Include(bcTypes, TStBarcodeType.Codabar);
    end;
  end;
  if bcTypes <> [] then
  begin
    if SelectBarcodeType(bctypes, TStAutoSelect.WithCheck) then
      exit(true);
  end;

  // Check for length where checksum can be added.
  bcTypes := [];
  for var bc in ATypes do
  begin
    case bc of
      bcEAN: bcTypes := bcTypes + [TStBarcodeType.EAN_8,TStBarcodeType.EAN_13];
      bcUPC: bcTypes := bcTypes + [TStBarcodeType.UPC_A, TStBarcodeType.UPC_E];
      bcISBN:Include(bcTypes, TStBarcodeType.EAN_13);
      bcCBR: Include(bcTypes, TStBarcodeType.Codabar);
    end;
  end;
  (*
  if (TStBarCodeType.UPC_A in bcTypes) and (TStBarCodeType.EAN_13 in bcTypes) then
    Exclude(bcTypes, TStBarCodeType.UPC_A);
  *)

  if bcTypes <> [] then
  begin
    if SelectBarcodeType(bctypes, TStAutoSelect.WithoutCheck) then
      exit(true);
  end;

  // Fallback to those without checksum.
  bcTypes := [];
  for var bc in ATypes do
  begin
    case bc of
      bc39:  Include(bcTypes, TStBarcodeType.Code39);
      bc128: Include(bcTypes, TStBarcodeType.Code128);
      bc128C:Include(bcTypes, TStBarcodeType.Code128C);
      bc128B:Include(bcTypes, TStBarcodeType.Code128B);
      bcI25: Include(bcTypes, TStBarcodeType.Interleaved2of5);
      bc93:  Include(bcTypes, TStBarcodeType.Code93);
    end;
  end;

  if bcTypes <> [] then
  begin
    if SelectBarcodeType(bctypes, TStAutoSelect.Both) then
      exit(true);
  end;
  result := false;
end;

function TStBarcode.SelectBarcodeType(AType : TStBarCodeTypes; ACSMode: TStAutoSelect) : boolean;
var
  digits : TStDigitArray;
  c, k: integer;

begin
  var hasDigits : boolean := false;
  var hasAlpha : boolean := false;
  var hasOther : boolean := false;
  var hasSpace : boolean := false;
  for var ch in Code do
  begin
    case ch of
      '0'..'9': hasDigits := true;
      'a'..'z',
      'A'..'Z': hasAlpha := true;
      ' ': hasSpace := true;
    else hasOther := true;
    end;
  end;
  if (not hasDigits) or hasAlpha or hasOther or hasSpace then
  begin
    AType := AType - [TStBarcodeType.EAN_8, TStBarcodeType.EAN_13, TStBarcodeType.UPC_A, TStBarcodeType.UPC_E];
  end;
  case Length(Code) of
    7:{ean8-cs,upce-cs}
    begin
      if (TStBarcodeType.EAN_8 in AType) and (ACSMode <> TStAutoSelect.WithCheck) then
      begin
        try
          GetDigits(TStBarcodeType.EAN_8, Code, digits);
          SetBarCodeType(TStBarcodeType.EAN_8);
          exit(true);
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.EAN_8);
        end;
      end;
      if (TStBarcodeType.UPC_E in AType) and (ACSMode <> TStAutoSelect.WithCheck) then
      begin
        try
          var len := GetDigits(TStBarcodeType.UPC_E, Code, digits);
          DoGetCheckCharacters(TStBarcodeType.UPC_E, digits, len, C, K);
          SetBarCodeType(TStBarcodeType.UPC_E);
          exit(true);
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.UPC_E);
        end;
      end;

    end;
    8:{ean8+cs, upce+cs}
    begin
      if (TStBarcodeType.EAN_8 in AType) and (ACSMode <> TStAutoSelect.WithoutCheck) then
      begin
        try
          var len := GetDigits(TStBarcodeType.EAN_8, Code, digits);
          DoGetCheckCharacters(TStBarcodeType.EAN_8, digits, len, C, K);
          if digits[8] = C then
          begin
            SetBarCodeType(TStBarcodeType.EAN_8);
            exit(true);
          end;

        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.EAN_8);
        end;
      end;
      if (TStBarcodeType.UPC_E in AType) and (ACSMode <> TStAutoSelect.WithoutCheck) then
      begin
        try
          var len := GetDigits(TStBarcodeType.UPC_E, Code, digits);
          DoGetCheckCharacters(TStBarcodeType.UPC_E, digits, len, C, K);
          if digits[8] = C then
          begin
            SetBarCodeType(TStBarcodeType.UPC_E);
            exit(true);
          end;
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.UPC_E);
        end;
      end;
    end;
    11:{upcA-cs}
    begin
      if (TStBarcodeType.UPC_A in AType) and (ACSMode <> TStAutoSelect.WithCheck) then
      begin
        try
          GetDigits(TStBarcodeType.UPC_A, Code, digits);
          SetBarCodeType(TStBarcodeType.UPC_A);
          exit(true);
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.UPC_A);
        end;
      end;
    end;
    12:{ean13-cs}{upca+cs}
    begin
      if (TStBarcodeType.UPC_A in AType) and (ACSMode <> TStAutoSelect.WithoutCheck) then
      begin
        try
          var len := GetDigits(TStBarcodeType.UPC_A, Code, digits);
          DoGetCheckCharacters(TStBarcodeType.UPC_A, digits, len, C, K);
          if digits[12] <> C then
            exclude(AType, TStBarcodeType.UPC_A)
          else
          begin
            SetBarCodeType( TStBarcodeType.UPC_A);
            exit(true);
          end;
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.UPC_A);
        end;
      end;

      if (TStBarcodeType.EAN_13 in AType) and (ACSMode <> TStAutoSelect.WithCheck) then
      begin
        try
          GetDigits(TStBarcodeType.EAN_13, Code, digits);
          SetBarCodeType( TStBarcodeType.EAN_13);
          exit(true);
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.EAN_13);
        end;
      end;
    end;
    13:{ean13+cs}
    begin
      if (TStBarcodeType.EAN_13 in AType) and (ACSMode <> TStAutoSelect.WithoutCheck) then
      begin
        try
          var len := GetDigits(TStBarcodeType.EAN_13, Code, digits);
          DoGetCheckCharacters(TStBarcodeType.EAN_13, digits, len, C, K);
          if not ChMatches(Code.Chars[12], C) then
            exclude(AType, TStBarcodeType.EAN_13)
          else
          begin
            SetBarCodeType( TStBarcodeType.EAN_13);
            exit(true);
          end;
        except
          on E : EStBarCodeError do
            Exclude(AType, TStBarcodeType.EAN_13);
        end;
      end;
    end;
  end;
  if TStBarcodeType.Code128 in AType then
    AType := AType + _Code128 - [TStBarcodeType.Code128]; // Expand Code128

  for var bctype in (AType- [TStBarcodeType.EAN_13, TStBarcodeType.EAN_8, TStBarcodeType.UPC_A, TStBarcodeType.UPC_E]) do
  begin
    try
      GetDigits(bcType, Code, digits);
      SetBarCodeType( bcType);
      exit(true);
    except
      on E : EStBarCodeError do
        ;
    end;
  end;
  result := false;
end;

procedure TStBarcode.Invalidate;
begin
  bcDirty := true;
end;

procedure TStBarcode.DrawBarCode(ACanvas : TStBarcodeCanvas; const R : TRectF; AScale : Double = 1;ACenter : boolean = true);

  procedure StripCode128NonPrintable(var ACode : String);
  begin
    {remove non-printable characters}
    var escaped := false;
    var outpos := 1;
    for var i := 1 to Length(ACode) do
    begin
      var ch := ACode[I];
      var doout := true;
      case ch of
        #0..pred(' '):
          begin
            doOut := false;
            escaped := false;
          end;
        '\':
          if ExtendedSyntax then
          begin
            if escaped then
              escaped := false
            else
            begin
              escaped := true;
              doout := false;
            end;
          end;
        'A', 'B', 'C', 'a', 'b', 'c':
          if escaped then
          begin
            ch := ' ';
            escaped := false;
          end;
      else
        escaped := false;
      end;
      if doout then
      begin
        ACode[outpos] := ch;
        inc(outpos);
      end;
    end;
    if escaped then
    begin
      ACode[outpos] := '\';
      inc(outpos);
    end;
    SetLength(ACode, outpos-1);
  end;
  function DrawBar(XPos, YPos, AWidth, AHeight : double) : double;
  begin
    ACanvas.DrawLine(XPos, YPos, XPos+AWidth, YPos+AHeight);
    Result := XPos + AWidth;
  end;
var
  I              : Integer;
  X, Y           : Double;
  TH, GA, GAB,
  TQ, BB         : Double;
  BarCodeHeight  : double;
  BarCodeWidth   : double;
  SmallestWidth  : Double;
  C              : string;
begin
  if bcDirty or (FBarcodeType = TStBarcodeType.Code128) then
    CalcBarCode;

  {determine narrowest line width}
  SmallestWidth := SmallestLineWidth;

  {find sizes for the BarCode elements}
  bcBarModWidth := FBarWidth * AScale;

  if bcBarModWidth < SmallestWidth then
    bcBarModWidth := SmallestWidth;
  bcSpaceModWidth := bcBarModWidth / FBarToSpaceRatio;

  {total width of BarCode and position within rect}
  CalcBarCodeWidth;
  BarCodeWidth := bcNormalWidth + bcSpaceWidth + bcSupplementWidth;
  BarCodeHeight := R.Height;
  if ACenter and (BarCodeWidth < R.Width) then
    X := R.Left + (R.Width-BarCodeWidth) / 2
  else
    X := R.Left;
  Y := R.Top;

  {guard bar adjustment}
  GA := (BarCodeHeight*10) / 100; {10% of bar height}
  GAB := GA;

  {three quarter height bar adjustment}
  TQ := BarCodeHeight / 4;

  {draw the text}
  if FShowCode and (Code > '') then
  begin

    {get text height}
    ACanvas.SetFontSize( _FontSizeMM[TStTextSize.Normal] );
    TH := ACanvas.GetTextExtents('Yg0').Y;

    Decf(BarCodeHeight, TH + (0.1{mm}*AScale));
    {guard bar adjustment}
    GAB := TH / 2;

    {three quarter height bar adjustment}
    TQ := BarCodeHeight / 4;

    var lastpos := 0;
    var XTxt : double := X + (0.6{mm} * AScale);
    var YTxt : double := Y + BarCodeHeight + (0.3{mm} * AScale);

    if bcBarInfo.TextItemCount = 0 then
    begin
      C := Code;
      case FBarCodeType of
        TStBarcodeType.Interleaved2of5 :
          begin
            if Odd(Length(C)) then
              C := '0' + C;
          end;
        TStBarcodeType.Codabar :
          begin
            if not FShowGuardChars then
              {strip leading and trailing characters}
              C := Copy(C, 2, Length(C)-2);
          end;
        TStBarcodeType.Code39 :
          begin
            {add guard characters}
            if C[1] <> '*' then
              C := '*' + C;
            if C[Length(C)] <> '*' then
              C := C + '*';
            if not FShowGuardChars then
              {strip leading and trailing characters}
              C := Copy(C, 2, Length(C)-2);
          end;
        TStBarcodeType.Code93 :
          begin
            {remove non-printable characters}
            for I := 1 to Length(C) do
              if C[I] < ' ' then
                C[I] := ' ';
          end;
        TStBarcodeType.Code128A,
        TStBarcodeType.Code128B,
        TStBarcodeType.Code128C,
        TStBarcodeType.Code128 :
          StripCode128NonPrintable(C);

      end;
      var tw := ACanvas.GetTextExtents(C).x;
      var xoffset := (BarCodeWidth - tw) / 2;
      ACanvas.TextOut(X+xoffset, YTxt, C);
    end
    else
    begin
      var fontSize := TStTextSize.Normal; // Already set to this.
      var topAdjust : double := 0;

      for I := 0 to bcBarInfo.TextItemCount-1 do
      begin
        var ti := bcBarInfo.TextItems[I];
        if fontSize <> ti.Size then
        begin
          fontSize := ti.Size;
          ACanvas.SetFontSize(_FontSizeMM[fontSize]);
          if fontSize = TStTextSize.Normal then
            topAdjust := 0
          else
          begin
            var oth : double := ACanvas.GetTextExtents('Yg0').Y;
            topAdjust := TH-oth;
          end;
        end;

        // Position output under corresponding bar section
        for var J := lastPos to Min(ti.BarIdx, bcBarInfo.Count)-1 do
          IncrementBarPosn(bcBarInfo[J], XTxt, XTxt, XTxt);
        lastPos := ti.BarIdx;
        ACanvas.TextOut(XTxt, YTxt + topAdjust, ti.Text);
      end;
    end;

  end;

  if (FBarCodeType = TStBarcodeType.Interleaved2of5) and FBearerBars then
  begin
    BB := 3 * bcBarModWidth;
    {reduce height to allow for bearer bars}
    DecF(BarCodeHeight, BB * 2);
    {draw the bearer bars}
    DrawBar(X-bcBarModWidth, Y,
                     X+BarCodeWidth+bcBarModWidth, Y+BB);
    DrawBar(X-bcBarModWidth, Y+BarCodeHeight+BB,
                     X+BarCodeWidth+bcBarModWidth, Y+BarCodeHeight+BB*2);
    {adjust top of BarCode}
    IncF(Y, BB);
  end;

  {draw the bar code}

  // Guard bar attributes
  var ygstart : Double := Y;
  var ygHeight : Double := BarCodeHeight;
  if FTallGuardBars then
  begin
    if bcGuardBarAbove then
    begin
      DecF(ygstart, GA);
      IncF(ygHeight, GA);
    end;
    if bcGuardBarBelow then
      IncF(ygHeight, GAB);
  end;

  for I := 0 to bcBarInfo.Count-1 do
  begin
    if bkSpace in bcBarInfo[I].Kind then
      IncF(X, bcSpaceModWidth*bcBarInfo[I].Modules)
    else if (bkGuard in bcBarInfo[I].Kind) then
      X := DrawBar(X, ygStart, bcBarModWidth*bcBarInfo[I].Modules, ygHeight)
    else if (bkBar in bcBarInfo[I].Kind) then
      X := DrawBar(X, Y, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight)
    else if (bkThreeQuarterBar in bcBarInfo[I].Kind) then
      X := DrawBar(X, Y+TQ, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight-TQ);
  end;
end;

 {added}
function TStBarcode.GetBarCodeWidth(AScale : Double = 1) : Double;
begin
  var ign_cmp : boolean;
  result := GetbarcodeWidth(ign_cmp, AScale);
end;

function TStBarcode.GetBarCodeWidth(var ACanCompress : boolean; AScale : Double = 1) : Double;
var
  SmallestWidth  : Double;
begin
  if bcDirty or (FBarcodeType = TStBarcodeType.Code128) then
    CalcBarCode;

  {determine narrowest line width}
  SmallestWidth := SmallestLineWidth;

  {find sizes for the BarCode elements}
  bcBarModWidth := FBarWidth * AScale;

  ACanCompress := bcBarModWidth > SmallestWidth;
  if not ACanCompress then
    bcBarModWidth := SmallestWidth;
  bcSpaceModWidth := bcBarModWidth / FBarToSpaceRatio;

  CalcBarcodeWidth;

  {width in pixels (not counting text printed to left or right of code)}
  Result := bcNormalWidth + bcSpaceWidth + bcSupplementWidth;
end;

function TStBarcode.GetCheckCharacters(const S : string; var C, K : Integer) : TStCheckResult;
begin
  var len := GetDigits(FBarCodeType, S, bcDigits);
  bcDigitCount := len;
  result := DoGetCheckCharacters(FBarcodeType, bcDigits, len, C, K);
end;

function TStBarcode.GetCheckCharacters(var C, K : Integer) : TStCheckResult;
begin
  if bcDirty or (FBarcodeType = TStBarcodeType.Code128) then
    result := GetCheckCharacters(Code, C, K)
  else
    result := DoGetCheckCharacters(FBarcodeType, bcDigits, bcDigitCount, C, K);
end;

class function TStBarcode.ValidateCheckDigits(AType : TStBarCodeType; AExtSyntax : boolean; Const ABarcode : String) : boolean;
var
  digits : TStDigitArray;
  c, K : integer;
begin
  try
    var len := GetDigits(AType, ABarcode, AExtSyntax, digits);
    DoGetCheckCharacters(AType, digits, len, c, k);
    case AType of
      TStBarcodeType.UPC_A,
      TStBarcodeType.UPC_E,
      TStBarcodeType.EAN_8,
      TStBarcodeType.EAN_13,
      TStBarcodeType.Codabar,
      TStBarcodeType.Code39:
      result := ChMatches(ABarcode[high(ABarcode)], C);

      TStBarcodeType.Code11,
      TStBarcodeType.Code93:
        result := (ChMatches(ABarcode[pred(high(ABarcode))], C))
              and (ChMatches(ABarcode[high(ABarcode)], K));
    else
      result := true;
    end;
  except
    on E : EStBarCodeError do
      result := false;
  end;
end;

function TStBarcode.GetCheckCharacters(AType : TStBarCodeType; const S : string; var C, K : Integer) : TStCheckResult;
var
  digits : TStDigitArray;
begin
  var len := GetDigits(AType, S, digits);
  result := DoGetCheckCharacters(AType, digits, len, C, K);
end;

class function TStBarcode.GetCheckCharacters(AType : TStBarCodeType; const S : string; var C, K : Integer; AExtendedSyntax : boolean = false) : TStCheckResult;
var
  digits : TStDigitArray;
begin
  var len := GetDigits(AType, S, AExtendedSyntax, digits);
  result := DoGetCheckCharacters(AType, digits, len, C, K);
end;

class function TStBarcode.EANChecksum(const ADigits : TStDigitArray; ALen : Integer; AMode : TEanMode) : integer;
var
  esum, osum, sum : integer;
  even : boolean;
begin
  esum := 0;
  osum := 0;
  even := true; // Last char even.
  for var idx :=  ALen downto 1 do
  begin
    if even then
    begin
      even := false;
      inc(esum, ADigits[idx]);
    end
    else
    begin
      even := true;
      inc(osum, ADigits[idx]);
    end;
  end;
  if AMode = TEanMode.Standard then
  begin
    sum := ((3*esum) + osum);
    result := (10 - (sum mod 10)) mod 10;
  end
  else
  begin
    sum := (3*esum) + (9*osum);
    result := sum mod 10
  end;
end;

const
  // UPCE Checksum tool.
  // Duplicating for convenience.
  // Since the checksum is done on the expanded UPCE->UPCA version... this short-cuts that.
  // + = Even Total
  // - = Odd Total
  // _ = Skipped
  CUPCE_Check : array[0..9] of string =
  (
    {N}
    // SabcdeNX -> SabN0000cdeX (0<= N <= 2)
    {0}'+-++-+-',
    {1}'+-++-+-',
    {2}'+-++-+-',
    // Sabcde3X -> Sabc00000deX ( N = 3 )
    {3}'+-+--+_',
    // Sabcde4X -> Sabcd00000eX ( N = 4 )
    {4}'+-+-++_',
    // SabcdeNX -> Sabcde0000NX ( 5 <= N <= 9 )
    {5}'+-+-+-+',
    {6}'+-+-+-+',
    {7}'+-+-+-+',
    {8}'+-+-+-+',
    {9}'+-+-+-+'
  );

class function TStBarcode.DoGetCheckCharacters(AType : TStBarCodeType; const ADigits : TStDigitArray; ALen : integer; var C, K : integer) : TStCheckResult;
var
  I  : Integer;
  C1 : Integer;
  C2 : Integer;
begin
  C := -1;
  K := -1;

  case AType of
    TStBarcodeType.UPC_A :
      if ALen < 11 then
        result := TStCheckResult.Invalid
      else
      begin
        C := EANChecksum(ADigits, 11, TEanMode.Standard);
        if ALen = 11 then
          result := TStCheckResult.Added
        else if ADigits[12] = C then
          result := TStCheckResult.Match
        else
          result := TStCheckResult.Mismatch;
      end;
    TStBarcodeType.UPC_E:
      if ALen < 7 then
        result := TStCheckResult.Invalid
      else
      begin
        {determine check character}
        c1 := 0;
        c2 := 0;
        // UPC_E checksum is calculated on its expanded UPCA version.
        var pattern := CUPCE_Check[ADigits[7]];
        for var idx := 1 to length(pattern) do
        begin
          case pattern[idx] of
            '+': inc(c1, ADigits[idx]);
            '-': inc(c2, ADigits[idx]);
          end;
        end;
        C := 10 - (((c1*3) +c2) mod 10);
        if C = 10 then
          C := 0;
        if ALen = 7 then
          result := TStCheckResult.Added
        else if ADigits[8] = C then
          result := TStCheckResult.Match
        else
          result := TStCheckResult.Mismatch;
      end;
    TStBarcodeType.EAN_8 :
      if ALen < 7 then
        result := TStCheckResult.Invalid
      else
      begin
        C := EANChecksum(ADigits, 7, TEanMode.Standard);
        if ALen = 7 then
          result := TStCheckResult.Added
        else if ADigits[8] = C then
          result := TStCheckResult.Match
        else
          result := TStCheckResult.Mismatch;
      end;
    TStBarcodeType.EAN_13 :
      if ALen < 12 then
        result := TStCheckResult.Invalid
      else
      begin
        C := EANChecksum(ADigits, 12, TEanMode.Standard);
        if ALen = 12 then
          result := TStCheckResult.Added
        else if ADigits[13] = C then
          result := TStCheckResult.Match
        else
          result := TStCheckResult.Mismatch;
      end;
    TStBarcodeType.Interleaved2of5 :
      begin
        {get digits}
        C1 := 0;
        C2 := 0;
        for I := 1 to ALen do
          if Odd(I) then
            C1 := C1 + ADigits[I]  {odd digits}
          else
            C2 := C2 + ADigits[I]; {even digits}
        C2 := C2 * 3;

        C := 10 - ((C1 + C2) mod 10);
        if C = 10 then
          C := 0;
        result := TStCheckResult.Added
      end;
    TStBarcodeType.Codabar :
      begin
        {get digits}
        C1 := 0;
        for I := 1 to ALen do
          C1 := C1 + ADigits[I];

        C := 16 - (C1 mod 16);
        if C = 16 then
          C := 0;
        result := TStCheckResult.Added
      end;
    TStBarcodeType.Code11 :
      begin
        {get digits}
        C1 := 0;
        for I := ALen downto 1 do
          C1 := C1 + ADigits[I]*(ALen-I+1);
        C1 := C1 mod 11; {the "C" check character}
        C2 := C1;
        for I := ALen downto 1 do
          C2 := C2 + ADigits[I]*(ALen-I+2);
        C2 := C2 mod 11; {the "K" check character}
        K := C2;
        C := C1;
        result := TStCheckResult.Added
      end;
    TStBarcodeType.Code39 :
      begin
        {get digits}
        C1 := 0;
        for I := 1 to ALen do
          C1 := C1 + ADigits[I];

        C := 43 - (C1 mod 43);
        if C = 43 then
          C := 0;
        result := TStCheckResult.Added
      end;
    TStBarcodeType.Code93 :
      begin
        {get digits}
        C1 := 0;
        for I := ALen downto 1 do
          C1 := C1 + ADigits[I]*(ALen-I+1);
        C1 := C1 mod 47; {the "C" check character}
        C2 := C1;
        for I := ALen downto 1 do
          C2 := C2 + ADigits[I]*(ALen-I+2);
        C2 := C2 mod 47; {the "K" check character}
        K := C2;
        C := C1;
        result := TStCheckResult.Added
      end;
    TStBarcodeType.Code128A,
    TStBarcodeType.Code128B,
    TStBarcodeType.Code128C,
    TStBarcodeType.Code128 :
      begin
        {get digits}
        C1 := ADigits[1];
        for I := 2 to ALen do
          C1 := C1 + ADigits[I]*(I-1);

        C := C1 mod 103;
        if C = 103 then
          C := 0;
        result := TStCheckResult.Added;
      end;
  else
    result := TStCheckResult.None;
  end;
end;

function TStBarcode.GetCode : string;
begin
  Result := FText;
end;

function TStBarcode.GetDigits(Characters : string) : Integer;
begin
  result := GetDigits(FBarCodeType, Characters, bcDigits);
  bcDigitCount := result;
end;

function TStBarcode.GetDigits(AType : TStBarCodeType; Characters : string; var ADigits : TStDigitArray) : Integer;
begin
  result := GetDigits(AType, Characters, ExtendedSyntax, ADigits);
end;

class function TStBarcode.GetDigits(AType : TStBarCodeType; Characters : string; AExtendedSyntax : boolean; var ADigits : TStDigitArray) : Integer;

  procedure GetACode128CDigit (c : Char; var Index : Integer; var bcDigitPos : Integer);
  var
    J : Integer;
  begin
    case (c) of
      #130     : ADigits[bcDigitPos + 1] := 98;  {rest are manufactured characters}
      #131     : ADigits[bcDigitPos + 1] := 97;
      #132     : ADigits[bcDigitPos + 1] := 96;
      #133     : ADigits[bcDigitPos + 1] := 98;
      #134     : ADigits[bcDigitPos + 1] := 100;
      #135     : ADigits[bcDigitPos + 1] := 99;
      #136     : ADigits[bcDigitPos + 1] := 103;
      #137     : ADigits[bcDigitPos + 1] := 104;
      #138     : ADigits[bcDigitPos + 1] := 105;
      #139     : ADigits[bcDigitPos + 1] := 106;
    else
      try
        J := StrToInt(Copy (Characters, Index, 2));
        ADigits[bcDigitPos + 1] := J;
        Inc (Index);
      except
        RaiseStError(EStBarCodeError, stscInvalidCharacter);
      end;
    end;
    Inc (Index);
    Inc (bcDigitPos);
  end;

  procedure GetACode128ABDigit (c : Char; var Index : Integer;
                                var bcDigitPos : Integer);
  begin
    case c of
      ' '      : ADigits[bcDigitPos + 1] := 0;
      '!'      : ADigits[bcDigitPos + 1] := 1;
      '"'      : ADigits[bcDigitPos + 1] := 2;
      '#'      : ADigits[bcDigitPos + 1] := 3;
      '$'      : ADigits[bcDigitPos + 1] := 4;
      '%'      : ADigits[bcDigitPos + 1] := 5;
      '&'      : ADigits[bcDigitPos + 1] := 6;
      ''''     : ADigits[bcDigitPos + 1] := 7;
      '('      : ADigits[bcDigitPos + 1] := 8;
      ')'      : ADigits[bcDigitPos + 1] := 9;
      '*'      : ADigits[bcDigitPos + 1] := 10;
      '+'      : ADigits[bcDigitPos + 1] := 11;
      ','      : ADigits[bcDigitPos + 1] := 12;
      '-'      : ADigits[bcDigitPos + 1] := 13;
      '.'      : ADigits[bcDigitPos + 1] := 14;
      '/'      : ADigits[bcDigitPos + 1] := 15;
      '0'..'9' : ADigits[bcDigitPos + 1] := 16 + Ord(c)-Ord('0');
      ':'      : ADigits[bcDigitPos + 1] := 26;
      ';'      : ADigits[bcDigitPos + 1] := 27;
      '<'      : ADigits[bcDigitPos + 1] := 28;
      '='      : ADigits[bcDigitPos + 1] := 29;
      '>'      : ADigits[bcDigitPos + 1] := 30;
      '?'      : ADigits[bcDigitPos + 1] := 31;
      '@'      : ADigits[bcDigitPos + 1] := 32;
      'A'..'Z' : ADigits[bcDigitPos + 1] := 33 + Ord(c)-Ord('A');
      '['      : ADigits[bcDigitPos + 1] := 59;
      '\'      : ADigits[bcDigitPos + 1] := 60;
      ']'      : ADigits[bcDigitPos + 1] := 61;
      '^'      : ADigits[bcDigitPos + 1] := 62;
      '_'      : ADigits[bcDigitPos + 1] := 63;
      #0, #31  : ADigits[bcDigitPos + 1] := 64 + Ord(c);  {control characters}
      '`'      : ADigits[bcDigitPos + 1] := 64;
      'a'..'z' : ADigits[bcDigitPos + 1] := 65 + Ord(c)-Ord('a');
      '{'      : ADigits[bcDigitPos + 1] := 91;
      '|'      : ADigits[bcDigitPos + 1] := 92;
      '}'      : ADigits[bcDigitPos + 1] := 93;
      '~'      : ADigits[bcDigitPos + 1] := 94;
      #130     : ADigits[bcDigitPos + 1] := 98; {rest are manufactured characters}
      #131     : ADigits[bcDigitPos + 1] := 97;
      #132     : ADigits[bcDigitPos + 1] := 96;
      #133     : ADigits[bcDigitPos + 1] := 98;
      #134     : ADigits[bcDigitPos + 1] := 100;
      #135     : ADigits[bcDigitPos + 1] := 99;
      #136     : ADigits[bcDigitPos + 1] := 103;
      #137     : ADigits[bcDigitPos + 1] := 104;
      #138     : ADigits[bcDigitPos + 1] := 105;
      #139     : ADigits[bcDigitPos + 1] := 106;
    else
      RaiseStError(EStBarCodeError, stscInvalidCharacter);
    end;
    Inc (Index);
    Inc (bcDigitPos);
  end;

  function CountCode128Digits (Index : Integer) : Integer;
  begin
    Result := 0;
    while (Index <= Length (Characters)) and
          (Characters[Index] >= '0') and (Characters[Index] <= '9') do
    begin
      Inc (Result);
      Inc (Index);
    end;
  end;

  function CheckCode128Digits (Index : Integer; CharsLen : Integer) : Boolean;
  var
    NumDigits : Integer;
  begin
    Result := False;
    NumDigits := CountCode128Digits (Index);
    if Odd(NumDigits) then
    begin
      Characters := Copy (Characters, 1, Index - 1) +
                    '0' + Copy (Characters, Index, CharsLen - Index + 1);
      Result := True;
    end;
  end;

  function GetCode128Digits : Integer;
  type
    TStCode128CodeSubset = (csCodeA, csCodeB, csCodeC);
  var
    I             : Integer;
    RLen          : Integer;
    CurMode       : TStCode128CodeSubset;
    NeedCharCount : Boolean;
    Skip          : Boolean;

  begin
    I := 1;
    Result := Length (Characters);
    RLen := 0;
    CurMode := csCodeC;
    case Atype of
      TStBarcodeType.Code128A: CurMode := csCodeA;
      TStBarcodeType.Code128B: CurMode := csCodeB;
      TStBarcodeType.Code128,
      TStBarcodeType.Code128C: CurMode := csCodeC;
    end;
    NeedCharCount := CurMode = csCodeC;

    while I <= Result do
    begin
      if (NeedCharCount) and
         (Characters[I] >= '0') and (Characters[I] <= '9') then
      begin
        NeedCharCount := False;
        if CheckCode128Digits (I, Result) then
          Inc (Result);
      end;

      Skip := False;
      if (AExtendedSyntax) and (Characters[I] = '\')  and
         (I < Result) then
      begin
        if ((Characters[I + 1] = 'A') or (Characters[I + 1] = 'a')) and
           (CurMode <> csCodeA) then
        begin
          Inc (RLen);
          ADigits[RLen] := 101;
          CurMode := csCodeA;
          Skip := True;
        end
        else if ((Characters[I + 1] = 'B') or (Characters[I + 1] = 'b')) and
                    (CurMode <> csCodeB) then
        begin
          Inc (RLen);
          ADigits[RLen] := 100;
          CurMode :=csCodeB;
          Skip := True;
        end
        else if ((Characters[I + 1] = 'C') or (Characters[I + 1] = 'c')) and
                    (CurMode <> csCodeC) then
        begin
          NeedCharCount := True;
          Inc (RLen);
          ADigits[RLen] := 99;
          CurMode := csCodeC;
          Skip := True;
        end
        else if (Characters[I + 1] = '\') then
        begin
          GetACode128ABDigit ('\', I, RLen);
          Skip := True;
        end;
        Inc (I);
      end;

      if not Skip then
        case CurMode of
          csCodeC :
            GetACode128CDigit (Characters[I], I, RLen);
          else
            GetACode128ABDigit (Characters[I], I, RLen);
        end
      else
        Inc (I);
    end;
    Result := RLen;
  end;

var
  I, J : Integer;
  S    : string;
begin
  FillChar(ADigits, SizeOf(ADigits), #0);
  Result := 0;

  case AType of
    TStBarcodeType.UPC_A, TStBarcodeType.UPC_E, TStBarcodeType.EAN_8, TStBarcodeType.EAN_13, TStBarcodeType.Interleaved2of5 :
      begin
        Result := Length(Characters);
        for I := 1 to Result do
          ADigits[I] := ChToInt(Characters[I]);
      end;
    TStBarcodeType.Codabar :
      begin
        Result := Length(Characters);
        for I := 1 to Result do
        begin
          case Characters[I] of
            '0'..'9' : ADigits[I] := ChToInt(Characters[I]);
            '-'      : ADigits[I] := 10;
            '$'      : ADigits[I] := 11;
            ':'      : ADigits[I] := 12;
            '/'      : ADigits[I] := 13;
            '.'      : ADigits[I] := 14;
            '+'      : ADigits[I] := 15;
            'A', 'a' : ADigits[I] := 16;
            'B', 'b' : ADigits[I] := 17;
            'C', 'c' : ADigits[I] := 18;
            'D', 'd' : ADigits[I] := 19;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      end;
    TStBarcodeType.Code11 :
      begin
        Result := Length(Characters);
        for I := 1 to Result do
        begin
          case Characters[I] of
            '0'..'9' : ADigits[I] := ChToInt(Characters[I]);
            '-'      : ADigits[I] := 10;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      end;
    TStBarcodeType.Code39 :
      begin
        Result := Length(Characters);
        for I := 1 to Result do
        begin
          case Characters[I] of
            '0'..'9' : ADigits[I] := ChToInt(Characters[I]);
            'A'..'Z' : ADigits[I] := Ord(Characters[I]) - Ord('A') + 10;
            '-'      : ADigits[I] := 36;
            '.'      : ADigits[I] := 37;
            ' '      : ADigits[I] := 38;
            '$'      : ADigits[I] := 39;
            '/'      : ADigits[I] := 40;
            '+'      : ADigits[I] := 41;
            '%'      : ADigits[I] := 42;
            '*'      : ADigits[I] := 43;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      end;
    TStBarcodeType.Code93 :
      begin
        Result := Length(Characters);
        J := 1;
        I := 1;
        while I <= Result do
        begin
          S := Code93Map[Characters[I]];
          if Length(S) > 1 then
          begin
            case S[1] of
              '$' : ADigits[J] := 43; {(+)}
              '%' : ADigits[J] := 44; {(%)}
              '/' : ADigits[J] := 45; {(/)}
              '+' : ADigits[J] := 46; {(+)}
            else
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
            end;
            Inc(J);
            S := S[2];
          end;

          case S[1] of
            '0'..'9' : ADigits[J] := Ord(S[1])-Ord('0');
            'A'..'Z' : ADigits[J] := 10 + Ord(S[1])-Ord('A');
            '-'      : ADigits[J] := 36;
            '.'      : ADigits[J] := 37;
            ' '      : ADigits[J] := 38;
            '$'      : ADigits[J] := 39;
            '/'      : ADigits[J] := 40;
            '+'      : ADigits[J] := 41;
            '%'      : ADigits[J] := 42;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
          Inc(I);
          Inc(J);
        end;
        Result := J;
      end;
    TStBarcodeType.Code128A,
    TStBarcodeType.Code128B,
    TStBarcodeType.Code128C,
    TStBarcodeType.Code128 :
      Result := GetCode128Digits;
  end;
end;

procedure TStBarcode.SetAddCheckChar(Value : Boolean);
begin
  if Value <> FAddCheckChar then
  begin
    FAddCheckChar := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetBarCodeType(Value : TStBarCodeType);
begin
  if Value <> FBarCodeType then
  begin
    FBarCodeType := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetBarToSpaceRatio(Value : Double);
begin
  {always uses a bar to space ratio of 1}

  if FBarCodeType in [TStBarcodeType.Interleaved2of5, TStBarcodeType.Code11, TStBarcodeType.Code39, TStBarcodeType.Code93, TStBarcodeType.Code128, TStBarcodeType.Code128A, TStBarcodeType.Code128B, TStBarcodeType.Code128C] then
    Value := 1;

  if Value <> FBarToSpaceRatio then
  begin
    FBarToSpaceRatio := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetBarNarrowToWideRatio(Value : Integer);
begin
  if Value <> FBarNarrowToWideRatio then
  begin
    FBarNarrowToWideRatio := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetBarWidth(Value : Double);
begin
  if Value <> FBarWidth then
  begin
    FBarWidth := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetBearerBars(Value : Boolean);
begin
  if Value <> FBearerBars then
  begin
    FBearerBars := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetCode(const Value : string);
begin
  var newText : String;
  if FBarCodeType in [TStBarcodeType.Code39] then
    newText := UpperCase(Value)
  else if FBarCodeType in [TStBarcodeType.Codabar] then
    newText := LowerCase(Value)
  else
    newText := Value;
  if newText <> FText then
  begin
    FText := newText;
    bcDirty := true;
  end;
end;

procedure TStBarcode.SetExtendedSyntax (const v : Boolean);
begin
  if v <> FExtendedSyntax then
  begin
    FExtendedSyntax := v;
    Invalidate;
  end;
end;

procedure TStBarcode.SetShowCode(Value : Boolean);
begin
  if Value <> FShowCode then
  begin
    FShowCode := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetShowGuardChars(Value : Boolean);
begin
  if Value <> FShowGuardChars then
  begin
    FShowGuardChars := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetSupplementalCode(const Value : string);
begin
  if Value <> FSupplementalCode then
  begin
    FSupplementalCode := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetTallGuardBars(Value : Boolean);
begin
  if Value <> FTallGuardBars then
  begin
    FTallGuardBars := Value;
    Invalidate;
  end;
end;

procedure TStBarcode.SetVersion(const Value : string);
begin
end;

function TStBarcode.SmallestLineWidth : Double;
begin
  case FBarCodeType of
    TStBarCodeType.UPC_E,
    TStBarCodeType.EAN_8:result := 0.33;
    TStBarCodeType.UPC_A,
    TStBarCodeType.EAN_13: result := 0.26;
    //TStBarCodeType.Interleaved2of5: ;
    //TStBarCodeType.Codabar: ;
    //TStBarCodeType.Code11: ;
    //TStBarCodeType.Code39: ;
    //TStBarCodeType.Code93: ;
    TStBarCodeType.Code128,
    TStBarCodeType.Code128C,
    TStBarCodeType.Code128A,
    TStBarCodeType.Code128B: result := 0.19;
  else
    Result := 0.265;
  end;
end;

function TStBarcode.Validate(DisplayError : Boolean) : Boolean;
var
  I      : Integer;
  CheckC : Integer;
  CheckK : Integer;
begin
  Result := True;
  try
    case FBarCodeType of
      TStBarcodeType.UPC_A :
        begin
          {11 or 12 characters}
          if not (Length(Code) in [11, 12]) then
            RaiseStError(EStBarCodeError, stscInvalidUPCACodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;

          DoGetCheckCharacters(FBarCodeType, bcDigits, bcDigitCount, CheckC, CheckK);
          if (Length(Code) = 12) and (CheckC <> bcDigits[12]) then
            RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
        end;
      TStBarcodeType.UPC_E :
        begin
          if not (Length(Code) in [7..8]) then
            RaiseStError(EStBarCodeError, stscInvalidUPCECodeLen);
          try
            GetDigits(Code);
            if Length(Code) = 8 then
            begin
              DoGetCheckCharacters(FBarCodeType, bcDigits, bcDigitCount, CheckC, CheckK);
              if (Length(Code) = 8) and (CheckC <> bcDigits[8]) then
                RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
            end;
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
          if not (bcDigits[1] in [0,1]) then
            RaiseStError(EStBarCodeError, stscInvalidUPCECodeLen);
        end;
      TStBarcodeType.EAN_8 :
        begin
          {7 or 8 characters}
          if not (Length(Code) in [7, 8]) then
            RaiseStError(EStBarCodeError, stscInvalidEAN8CodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;

          DoGetCheckCharacters(FBarCodeType, bcDigits, bcDigitCount, CheckC, CheckK);
          if (Length(Code) = 8) and (CheckC <> bcDigits[8]) then
            RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
        end;
      TStBarcodeType.EAN_13 :
        begin
          {12 or 13 characters}
          if not (Length(Code) in [12, 13]) then
            RaiseStError(EStBarCodeError, stscInvalidEAN13CodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;

          DoGetCheckCharacters(FBarCodeType, bcDigits, bcDigitCount, CheckC, CheckK);
          if (Length(Code) = 13) and (CheckC <> bcDigits[13]) then
            RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
        end;
      TStBarcodeType.Interleaved2of5 :
        begin
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      TStBarcodeType.Codabar :
        begin
          for I := 1 to Length(Code) do
          begin
            case Code[I] of
              '0'..'9', '-', '$', ':', '/', '.', '+', 'a'..'d', 'A'..'D': ;
            else
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
            end;
          end;
        end;
      TStBarcodeType.Code11 :
        begin
          for I := 1 to Length(Code) do
          begin
            case Code[I] of
              '0'..'9', '-': ;
            else
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
            end;
          end;
          {test check characters}
          if not FAddCheckChar then
          begin
            GetCheckCharacters(Code, CheckC, CheckK);
            if (ChToInt(Code[Length(Code)-1]) <> CheckC) or
               (ChToInt(Code[Length(Code)]) <> CheckK) then
              RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
          end;
        end;
      TStBarcodeType.Code39 :
        begin
          for I := 1 to Length(Code) do
          begin
            case Code[I] of
              '0'..'9', 'A'..'Z', 'a'..'z',
              '-', '.', ' ', '$', '/', '+', '%', '*': ;
            else
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
            end;
          end;
          {check for embedded guard character}
          for I := 2 to Length(Code)-1 do
            if Code[I] = '*' then
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
        end;
      TStBarcodeType.Code93 :
        begin
          try
            GetCheckCharacters(Code, CheckC, CheckK);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      TStBarcodeType.Code128A,
      TStBarcodeType.Code128B,
      TStBarcodeType.Code128C,
      TStBarcodeType.Code128 :
        begin
          try
            GetCheckCharacters(Code, CheckC, CheckK);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
    end;
    {check supplemental code}
    if FSupplementalCode > '' then
      if not (Length(FSupplementalCode) in [2, 5]) then
        RaiseStError(EStBarCodeError, stscInvalidSupCodeLen);
  except
    Result := False;
    if DisplayError then
      raise;
  end;
end;

function TStBarCodeTypeHelper.AsCode : String;
begin
  result := _Desc[self].C
end;

function TStBarCodeTypeHelper.AsDesc : String;
begin
  result := _Desc[self].N
end;

class function TStBarCodeTypeHelper.AsEnumFromCode(const ACode : String; var AVal : TStBarCodeType) : boolean;
begin
  result := false;
  for var idx := low(TStBarCodeType) to high(TStBarCodeType) do
  begin
    if SameText(_Desc[idx].C, ACode) then
    begin
      AVal := idx;
      exit(true);
    end;
  end;
end;

class function TStBarCodeTypeHelper.AsEnumFromDesc(const ACode : String; var AVal : TStBarCodeType) : boolean;
begin
  result := false;
  for var idx := low(TStBarCodeType) to high(TStBarCodeType) do
  begin
    if SameText(_Desc[idx].N, ACode) then
    begin
      AVal := idx;
      exit(true);
    end;
  end;
end;

procedure TArrayOf<T>.Grow(ANewCount : integer);
begin
  SetLength(Farray, System.Math.Max(length(FArray)+ _GrowNumber, ANewCount));
end;

function TArrayOf<T>.GetValue(AIdx : Integer): T;
begin
  if (AIdx < 0) or (AIDX >= FCount) then
    Raise Exception.CreateFmt('Index %d out of array bounds', [AIDX]);
  result := FArray[Aidx];
end;

procedure TArrayOf<T>.SetValue(AIdx : Integer; const NewVal: T);
begin
  if (AIdx < 0) or (AIDX >= FCount) then
    Raise Exception.CreateFmt('Index %d out of array bounds', [AIDX]);
  FArray[AIDx] := newVal;
end;

// public definitions

constructor TArrayOf<T>.Create(ACap : integer);
begin
  if ACap = 0 then
    ACap := _GrowNumber;
  SetLength(FArray, ACap);
  FCount := 0;
end;

procedure TArrayOf<T>.Delete(AIdx: integer);
begin
  if (AIdx >= 0) and (AIdx < FCount)  then
  begin
    for var idx := AIdx to FCount-2 do
      FArray[idx] := FArray[idx+1];
    Dec(FCount);
  end;

end;

class operator TArrayOf<T>.Initialize(out AArray : TArrayOf<T>);
begin
  AArray.FCount := 0;
end;

function TArrayOf<T>.Append(const AVal : T) : integer;
begin
  result := FCount;
  inc(FCount);
  if result >= length(FArray) then
    Grow(result);
  FArray[result] := AVal;
end;

function TArrayOf<T>.Append(const AArr : TArrayOf<T>) : integer;
begin
  Grow(FCount + AArr.FCount);
  for var idx := 0 to AArr.FCount-1 do
  begin
    FArray[FCount] := AArr.FArray[idx];
    inc(FCount);
  end;
  result := FCount;
end;

function TArrayOf<T>.Add(const AVal : T) : integer;
begin
  result := Append(AVal);
end;

function TArrayOf<T>.AsArray : TArray<T>;
begin
  SetLength(FArray, FCount);
  result := FArray;
end;

// protected definitions

function TArrayOf<T>.TEnumerator.DoGetCurrent: T;
begin
  result := FPArray^[FIndex];
end;

function TArrayOf<T>.TEnumerator.MoveNext: Boolean;
begin
  if FIndex < FPCount^ then
    Inc(FIndex);
  result := FIndex < FPCount^;
end;

constructor TArrayOf<T>.TEnumerator.Create(AArray : PArray; ACount : PInteger);
begin
  inherited;
  FPArray := AArray;
  FIndex := -1;
  FPCount := ACount;
end;


function TArrayOf<T>.GetEnumerator: TEnumerator;
begin
  result.Create(@FArray, @FCount);
end;

procedure TArrayOf<T>.Clear;
begin
  FCount := 0;
end;


end.
