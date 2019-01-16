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
{* SysTools: St2DBarC.pas 4.04                           *}
{*********************************************************}
{* SysTools: Two-Dimensional Barcodes                    *}
{*********************************************************}

{$I StDefine.inc}

unit St2DBarC;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Graphics,
  StdCtrls,
  Math,
  ClipBrd,
  StConst;

resourcestring

  StEBadBarHeight        = 'Bar Height cannot be less than one';
  StEBadBarHeightToWidth = 'BarHeightToWidth cannot be less than one';
  StEBadBarWidth         = 'Bar Width cannot be less than one';
  StEBadCountryCode      = 'Invalid Country Code';
  StEBadNumCols          = 'Invalid Number of columns';
  StEBadNumRows          = 'Invalid number of rows';
  StEBadPostalCode       = 'Invalid Postal Code';
  StEBadServiceClass     = 'Invalid Service Class';
  StEBadQuietZone        = 'Invalid Quiet Zone';
  StECodeTooLarge        = 'Code too large for barcode';
  StEGLIOutOfRange       = 'GLI value out of range';
  StEInvalidCodeword     = 'Invalid Codeword';
  StENeedBarHeight       = 'Either BarHeight or BarHeightToWidth is required';
  StENeedHorz            = 'Horizontal size needs to be specified';
  StENeedVert            = 'Vertical size needs to be specified';

type
  { Generic 2D barcode types and constants }

  TStDataMode = (dmBinary, dmText, dmNumeric);

  { PDF417 types and constants }

  TStPDF417CodewordList = array [0..2700] of Word;
  TStPDF417ECCLevels = (ecAuto, ecLevel0, ecLevel1, ecLevel2, ecLevel3,
                        ecLevel4, ecLevel5, ecLevel6, ecLevel7, ecLevel8);

  { MaxiCode types and constants }

  TStMaxiCodeMode = (cmMode2, cmMode3, cmMode4, cmMode5, cmMode6);

const
  StMaxiCodeGaloisField    = 64;  { Galois field to work in }
  StMaxiCodeECCPoly        = 67;  { Primary polynomial - }
  StMaxMaxiCodeECCDataSize = 144; { Max amount of data }

type
  TStMaxiCodeECCData = array [0..StMaxMaxiCodeECCDataSize] of Byte;
  TStMaxiCodeECCPoly = (epPrimary, epStandard, epEnhanced);
  TStMaxiCodeECCInterleave = (imNone, imEven, imOdd);

  { E2DBarcodeError }

  E2DBarcodeError = class (Exception);

  { TStCustom2DBarcode }

  TStCustom2DBarcode = class (TGraphicControl)
    protected { private }
      FCode              : string;
      FBarWidth          : Integer;
      FBackgroundColor   : TColor;
      FCaption           : string;
      FECCLevel          : Integer;
      FExtendedSyntax    : Boolean;
      FRelativeBarHeight : Boolean;
      FBarHeightToWidth  : Integer;
      FBarHeight         : Integer;

      FQuietZone         : Integer;
      FAlignment         : TAlignment;
      FCaptionLayout     : TTextLayout;
      FBarCodeRect       : TRect;
      FUsedCodewords     : Integer;
      FFreeCodewords     : Integer;
      FUsedECCCodewords  : Integer;
      FTotalCodewords    : Integer;

    { protected }
      FBitmap            : TBitmap;

      function  CalculateBarCodeWidth (PaintableWidth : Integer) : Integer;
                virtual; abstract;
      function  CalculateBarCodeHeight (PaintableHeight : Integer) : Integer;
                virtual; abstract;
      procedure DrawBarcode; virtual; abstract;
      procedure GenerateBarcodeBitmap (BCWidth  : Integer;
                                       BCHeight : Integer);
      procedure GenerateCodewords; virtual; abstract;
      function  GetBarCodeHeight : Integer;
      function  GetBarCodeWidth : Integer;
      procedure GetCurrentResolution (var ResX : Integer; var ResY : Integer);
      function GetVersion : string;
      procedure Paint; override;
      procedure SetAlignment (const v : TAlignment);
      procedure SetBackgroundColor (const v : TColor);
      procedure SetBarHeight (const v : Integer); virtual;
      procedure SetBarHeightToWidth (const v : Integer); virtual;
      procedure SetBarWidth (const v : Integer); virtual;
      procedure SetBitmap (const v : TBitmap);
      procedure SetCaption (const v : string);
      procedure SetCaptionLayout (const v : TTextLayout);
      procedure SetCode (const v : string);
      procedure SetECCLevel (const v : Integer);
      procedure SetExtendedSyntax (const v : Boolean);
      procedure SetRelativeBarHeight (const v : Boolean); virtual;
      procedure SetQuietZone (const v : Integer);
      procedure SetVersion(const Value : string);

    public
      constructor Create (AOwner : TComponent); override;
      destructor Destroy; override;

      procedure CopyToClipboard;
      procedure CopyToClipboardRes (ResX : Integer; ResY : Integer);
      procedure PaintToCanvas (ACanvas : TCanvas; Position : TPoint);
      procedure PaintToCanvasRes (ACanvas : TCanvas; Position : TPoint;
                                  ResX : Integer; ResY : Integer);
      procedure PaintToCanvasSize (ACanvas : TCanvas; X, Y, H : Double);
      procedure PaintToDC (DC : hDC; Position : TPoint);
      procedure PaintToDCRes (DC : hDC; Position : TPoint;
                              ResX : Integer; ResY : Integer);
      procedure PaintToPrinterCanvas (ACanvas : TCanvas; Position : TPoint);
      procedure PaintToPrinterCanvasRes (ACanvas : TCanvas; Position : TPoint;
                                         ResX : Integer; ResY : Integer);
      procedure PaintToPrinterCanvasSize (ACanvas : TCanvas; X, Y, H : Double);
      procedure PaintToPrinterDC (DC : hDC; Position : TPoint);
      procedure PaintToPrinterDCRes (DC : hDC; Position : TPoint;
                                     ResX : Integer; ResY : Integer);                                     
      procedure RenderToResolution (var OutBitmap : TBitmap;
                                    ResX : Integer;
                                    ResY : Integer;
                                    var SizeX : Integer;
                                    var SizeY : Integer); virtual; abstract;
      procedure SaveToFile (const FileName : string);
      procedure SaveToFileRes (const FileName : string;
                               ResX : Integer; ResY : Integer);

      property Alignment : TAlignment read FAlignment write SetAlignment
               default taCenter;
      property BackgroundColor : TColor
               read FBackgroundColor write SetBackgroundColor default clWhite;
      property BarCodeHeight : Integer read GetBarCodeHeight;
      property BarCodeRect : TRect read FBarCodeRect;
      property BarCodeWidth : Integer read GetBarCodeWidth;
      property BarHeight : Integer read FBarHeight write SetBarHeight
               default 2;
      property BarHeightToWidth : Integer
               read FBarHeightToWidth write SetBarHeightToWidth default 4;
      property BarWidth : Integer read FBarWidth write SetBarWidth default 2;
      property Bitmap : TBitmap read FBitmap write SetBitmap stored False;
      property Caption : string read FCaption write SetCaption;
      property CaptionLayout : TTextLayout
               read FCaptionLayout write SetCaptionLayout
               default tlBottom;
      property Code : string read FCode write SetCode;
      property ECCLevel : Integer read FECCLevel write SetECCLevel default 0;
      property ExtendedSyntax : Boolean
               read FExtendedSyntax write SetExtendedSyntax default True;
      property FreeCodewords : Integer read FFreeCodewords;
      property RelativeBarHeight : Boolean
               read FRelativeBarHeight write SetRelativeBarHeight
               default False;
      property QuietZone : Integer read FQuietZone write SetQuietZone
               default 8;
      property TotalCodewords : Integer read FTotalCodewords;
      property UsedCodewords : Integer read FUsedCodewords;
      property UsedECCCodewords : Integer read FUsedECCCodewords;

      property Color default clBlack;

    published
      property Version : string read GetVersion write SetVersion stored False;
    
      { Properties }
      property Align;
      property Cursor;
      property Enabled;
      property Font;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property ShowHint;
      property Visible;

      { Events }
      property OnClick;
      property OnDblClick;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
  end;

  { TStPDF417Barcode }

  TStPDF417Barcode = class (TStCustom2DBarcode)
    private
      FTruncated       : Boolean;
      FCodewords       : TStPDF417CodewordList;
      FNumCodewords    : Integer;
      FNewTextCodeword : Boolean;
      FHighlight       : Boolean;
      FNumRows         : Integer;
      FNumColumns      : Integer;

    protected
      procedure AddCodeword (Value : Word);
      function  CalculateBarCodeWidth (PaintableWidth : Integer) : Integer;
                override;
      function  CalculateBarCodeHeight (PaintableHeight : Integer) : Integer;
                override;
      procedure CalculateECC (NumCodewords : Integer; ECCLen : Integer);
      procedure CalculateSize (var XSize : Integer;
                               var YSize : Integer);
      function  CodewordToBitmask (RowNumber : Integer;
                                   Codeword : Integer) : DWord;
      procedure ConvertBytesToBase900 (const S : array of byte;
                                       var A : array of integer);
      procedure ConvertToBase900 (const S : string;
                                  var A : array of integer;
                                  var LenA : integer);
      procedure DrawBarcode; override;
      procedure DrawCodeword (RowNumber : Integer;
                              ColNumber : Integer;
                              WorkBarHeight : Integer;
                              Pattern   : string); 
      procedure DrawCodewordBitmask (RowNumber : Integer;
                                     ColNumber : Integer;
                                     WorkBarHeight : Integer;
                                     Bitmask   : DWord);
      procedure DrawLeftRowIndicator (RowNumber : Integer;
                                      WorkBarHeight : Integer;
                                      NumRows : Integer;
                                      NumCols : Integer);
      procedure DrawRightRowIndicator (RowNumber : Integer;
                                       ColNumber : Integer;
                                       WorkBarHeight : Integer;
                                       NumRows : Integer;
                                       NumCols : Integer);
      procedure DrawStartPattern (RowNumber : Integer;
                                  WorkBarHeight : Integer);
      procedure DrawStopPattern (RowNumber : Integer;
                                 ColNumber : Integer;
                                 WorkBarHeight : Integer);
      procedure EncodeBinary (var Position : Integer; CodeLen : Integer);
      procedure EncodeNumeric (var Position : Integer; CodeLen : Integer);
      procedure EncodeText (var Position : Integer; CodeLen : Integer);
      procedure GenerateCodewords; override;
      procedure GetNextCharacter (var NewChar  : Integer;
                                  var Codeword : Boolean;
                                  var Position : Integer;
                                  CodeLen      : Integer);
      function  GetPDF417ECCLevel : TStPDF417ECCLevels;
      function  GetRealErrorLevel : Integer;
      function  GoodForNumericCompaction (Position : Integer;
                                          CodeLen : Integer;
                                          var Count : Integer) : Boolean;
      function  GoodForTextCompaction (Position : Integer;
                                       CodeLen : Integer;
                                       var Count : Integer) : Boolean;
      function  IsNumericString (const S : string) : boolean;
      procedure SetBarHeight (const v : Integer); override;
      procedure SetBarHeightToWidth (const v : Integer); override;
      procedure SetBarWidth (const v : Integer); override;
      procedure SetNumColumns (const v : Integer);
      procedure SetNumRows (const v : Integer);
      procedure SetPDF417ECCLevel (const v : TStPDF417ECCLevels);
      procedure SetRelativeBarHeight (const v : Boolean); override;
      procedure SetTruncated (const v : Boolean);
      procedure TextToCodewords;

    public
      constructor Create (AOwner : TComponent); override;

      procedure RenderToResolution (var OutBitmap : TBitmap;
                                    ResX : Integer;
                                    ResY : Integer;
                                    var SizeX : Integer;
                                    var SizeY : Integer); override;

    published
      property ECCLevel : TStPDF417ECCLevels
               read GetPDF417ECCLevel write SetPDF417ECCLevel default ecAuto;
      property NumColumns : Integer read FNumColumns write SetNumColumns
               default 0;
      property NumRows : Integer read FNumRows write SetNumRows
               default 0;
      property Truncated : Boolean read FTruncated write SetTruncated
               default False;

      property Alignment;
      property BackgroundColor;
      property BarCodeHeight;
      property BarCodeWidth;
      property BarHeight;
      property BarHeightToWidth;
      property BarWidth;
      property Bitmap;
      property CaptionLayout;
      property Code;
      property ExtendedSyntax;
      property Height default 81;
      property RelativeBarHeight;
      property QuietZone;
      property Width default 273;

      property Caption;
      property Color;
      property Font;
  end;

  { TStMaxiCodeBarcode }

  TStMaxiCodeBarcode = class (TStCustom2DBarcode)
    private
      FMode                : TStMaxiCodeMode;
      FCodewords           : TStMaxiCodeECCData;
      FNumCodewords        : Integer;
      FHighlight           : Boolean;
      FShowCodewords       : Boolean;
      FShowAll             : Boolean;
      FMessage             : TStMaxiCodeECCData;
      FCarrierCountryCode  : Integer;
      FCarrierPostalCode   : string;
      FCarrierServiceClass : Integer;
      FAutoScale           : Boolean;
      FHorPixelsPerMM      : Extended;
      FVerPixelsPerMM      : Extended;
      FMaxiHexWidth        : Extended;
      FMaxiHexHeight       : Extended;
      FMaxiHexVOffset      : Extended;
      FMaxiHexHOffset      : Extended;

      { Log and AnitLog data for Galois field arithmetic }
      FLog     : array [0..StMaxiCodeGaloisField] of Integer;
      FAntiLog : array [0..StMaxiCodeGaloisField] of Integer;
      
    protected
      procedure AddCodeword (Value : Integer);
      function  CalculateBarCodeWidth (PaintableWidth : Integer) : Integer;
                override;
      function  CalculateBarCodeHeight (PaintableHeight : Integer) : Integer;
                override;
      procedure DrawBarcode; override;
      procedure DrawFinder;
      procedure DrawHex (XPos, YPos : Integer);
      procedure GenerateCodewords; override;
      procedure GenerateECC;
      procedure GetNextCharacter (var NewChar  : Integer;
                                  var Codeword : Boolean;
                                  var Position : Integer;
                                  CodeLen      : Integer);
      procedure GetSizes;
      procedure GetSizesEx (ResX : Integer; ResY : Integer);
      procedure PlotCell (Row : Integer; Col : Integer);
      procedure SetAutoScale (const v : Boolean);
      procedure SetBarHeight (const v : Integer); override;
      procedure SetBarWidth (const v : Integer); override;
      procedure SetCarrierCountryCode (const v : Integer);
      procedure SetCarrierPostalCode (const v : string);
      procedure SetCarrierServiceClass (const v : Integer);
      procedure SetMode (const v : TStMaxiCodeMode);
      procedure SetHorPixelsPerMM (const v : Extended);
      procedure SetVerPixelsPerMM (const v : Extended);
      procedure TextToCodewords;

    public
      constructor Create (AOwner : TComponent); override;

      procedure RenderToResolution (var OutBitmap : TBitmap;
                                    ResX          : Integer;
                                    ResY          : Integer;
                                    var SizeX     : Integer;
                                    var SizeY     : Integer); override;

    published
      property AutoScale : Boolean
               read FAutoScale write SetAutoScale default True;
      property CarrierCountryCode : Integer
               read FCarrierCountryCode write SetCarrierCountryCode default 0;
      property CarrierPostalCode : string
               read FCarrierPostalCode write SetCarrierPostalCode;
      property CarrierServiceClass : Integer
               read FCarrierServiceClass write SetCarrierServiceClass
               default 0;
      property HorPixelsPerMM : Extended
               read FHorPixelsPerMM write SetHorPixelsPerMM;

      property Mode : TStMaxiCodeMode
               read FMode write SetMode default cmMode4;
      property VerPixelsPerMM : Extended
               read FVerPixelsPerMM write SetVerPixelsPerMM;

      property Alignment;
      property BackgroundColor;
      property BarCodeHeight;
      property BarCodeWidth;
      property BarHeight default 0;
      property BarWidth default 0;
      property Bitmap;
      property CaptionLayout;
      property Code;
      property ExtendedSyntax;
      property Height default 129;
      property QuietZone;
      property Width default 121;

      property Caption;
      property Color;
      property Font;
  end;


implementation

uses
  System.Types, System.UITypes;

  { PDF417 types and constants }

type
  TStPDF417CodewordArray = array [0..2] of array [0..928] of Integer;

const

  StPDF417CellWidth = 17;

  StPDF417Codewords : TstPDF417CodewordArray =
  (($1d5c0, $1eaf0, $1f57c, $1d4e0, $1ea78, $1f53e, $1a8c0, $1d470, $1a860,
    $15040, $1a830, $15020, $1adc0, $1d6f0, $1eb7c, $1ace0, $1d678, $1eb3e,
    $158c0, $1ac70, $15860, $15dc0, $1aef0, $1d77c, $15ce0, $1ae78, $1d73e,
    $15c70, $1ae3c, $15ef0, $1af7c, $15e78, $1af3e, $15f7c, $1f5fa, $1d2e0,
    $1e978, $1f4be, $1a4c0, $1d270, $1e93c, $1a460, $1d238, $14840, $1a430,
    $1d21c, $14820, $1a418, $14810, $1a6e0, $1d378, $1e9be, $14cc0, $1a670,
    $1d33c, $14c60, $1a638, $1d31e, $14c30, $1a61c, $14ee0, $1a778, $1d3be,
    $14e70, $1a73c, $14e38, $1a71e, $14f78, $1a7be, $14f3c, $14f1e, $1a2c0,
    $1d170, $1e8bc, $1a260, $1d138, $1e89e, $14440, $1a230, $1d11c, $14420,
    $1a218, $14410, $14408, $146c0, $1a370, $1d1bc, $14660, $1a338, $1d19e,
    $14630, $1a31c, $14618, $1460c, $14770, $1a3bc, $14738, $1a39e, $1471c,
    $147bc, $1a160, $1d0b8, $1e85e, $14240, $1a130, $1d09c, $14220, $1a118,
    $1d08e, $14210, $1a10c, $14208, $1a106, $14360, $1a1b8, $1d0de, $14330,
    $1a19c, $14318, $1a18e, $1430c, $14306, $1a1de, $1438e, $14140, $1a0b0,
    $1d05c, $14120, $1a098, $1d04e, $14110, $1a08c, $14108, $1a086, $14104,
    $141b0, $14198, $1418c, $140a0, $1d02e, $1a04c, $1a046, $14082, $1cae0,
    $1e578, $1f2be, $194c0, $1ca70, $1e53c, $19460, $1ca38, $1e51e, $12840,
    $19430, $12820, $196e0, $1cb78, $1e5be, $12cc0, $19670, $1cb3c, $12c60,
    $19638, $12c30, $12c18, $12ee0, $19778, $1cbbe, $12e70, $1973c, $12e38,
    $12e1c, $12f78, $197be, $12f3c, $12fbe, $1dac0, $1ed70, $1f6bc, $1da60,
    $1ed38, $1f69e, $1b440, $1da30, $1ed1c, $1b420, $1da18, $1ed0e, $1b410,
    $1da0c, $192c0, $1c970, $1e4bc, $1b6c0, $19260, $1c938, $1e49e, $1b660,
    $1db38, $1ed9e, $16c40, $12420, $19218, $1c90e, $16c20, $1b618, $16c10,
    $126c0, $19370, $1c9bc, $16ec0, $12660, $19338, $1c99e, $16e60, $1b738,
    $1db9e, $16e30, $12618, $16e18, $12770, $193bc, $16f70, $12738, $1939e,
    $16f38, $1b79e, $16f1c, $127bc, $16fbc, $1279e, $16f9e, $1d960, $1ecb8,
    $1f65e, $1b240, $1d930, $1ec9c, $1b220, $1d918, $1ec8e, $1b210, $1d90c,
    $1b208, $1b204, $19160, $1c8b8, $1e45e, $1b360, $19130, $1c89c, $16640,
    $12220, $1d99c, $1c88e, $16620, $12210, $1910c, $16610, $1b30c, $19106,
    $12204, $12360, $191b8, $1c8de, $16760, $12330, $1919c, $16730, $1b39c,
    $1918e, $16718, $1230c, $12306, $123b8, $191de, $167b8, $1239c, $1679c,
    $1238e, $1678e, $167de, $1b140, $1d8b0, $1ec5c, $1b120, $1d898, $1ec4e,
    $1b110, $1d88c, $1b108, $1d886, $1b104, $1b102, $12140, $190b0, $1c85c,
    $16340, $12120, $19098, $1c84e, $16320, $1b198, $1d8ce, $16310, $12108,
    $19086, $16308, $1b186, $16304, $121b0, $190dc, $163b0, $12198, $190ce,
    $16398, $1b1ce, $1638c, $12186, $16386, $163dc, $163ce, $1b0a0, $1d858,
    $1ec2e, $1b090, $1d84c, $1b088, $1d846, $1b084, $1b082, $120a0, $19058,
    $1c82e, $161a0, $12090, $1904c, $16190, $1b0cc, $19046, $16188, $12084,
    $16184, $12082, $120d8, $161d8, $161cc, $161c6, $1d82c, $1d826, $1b042,
    $1902c, $12048, $160c8, $160c4, $160c2, $18ac0, $1c570, $1e2bc, $18a60,
    $1c538, $11440, $18a30, $1c51c, $11420, $18a18, $11410, $11408, $116c0,
    $18b70, $1c5bc, $11660, $18b38, $1c59e, $11630, $18b1c, $11618, $1160c,
    $11770, $18bbc, $11738, $18b9e, $1171c, $117bc, $1179e, $1cd60, $1e6b8,
    $1f35e, $19a40, $1cd30, $1e69c, $19a20, $1cd18, $1e68e, $19a10, $1cd0c,
    $19a08, $1cd06, $18960, $1c4b8, $1e25e, $19b60, $18930, $1c49c, $13640,
    $11220, $1cd9c, $1c48e, $13620, $19b18, $1890c, $13610, $11208, $13608,
    $11360, $189b8, $1c4de, $13760, $11330, $1cdde, $13730, $19b9c, $1898e,
    $13718, $1130c, $1370c, $113b8, $189de, $137b8, $1139c, $1379c, $1138e,
    $113de, $137de, $1dd40, $1eeb0, $1f75c, $1dd20, $1ee98, $1f74e, $1dd10,
    $1ee8c, $1dd08, $1ee86, $1dd04, $19940, $1ccb0, $1e65c, $1bb40, $19920,
    $1eedc, $1e64e, $1bb20, $1dd98, $1eece, $1bb10, $19908, $1cc86, $1bb08,
    $1dd86, $19902, $11140, $188b0, $1c45c, $13340, $11120, $18898, $1c44e,
    $17740, $13320, $19998, $1ccce, $17720, $1bb98, $1ddce, $18886, $17710,
    $13308, $19986, $17708, $11102, $111b0, $188dc, $133b0, $11198, $188ce,
    $177b0, $13398, $199ce, $17798, $1bbce, $11186, $13386, $111dc, $133dc,
    $111ce, $177dc, $133ce, $1dca0, $1ee58, $1f72e, $1dc90, $1ee4c, $1dc88,
    $1ee46, $1dc84, $1dc82, $198a0, $1cc58, $1e62e, $1b9a0, $19890, $1ee6e,
    $1b990, $1dccc, $1cc46, $1b988, $19884, $1b984, $19882, $1b982, $110a0,
    $18858, $1c42e, $131a0, $11090, $1884c, $173a0, $13190, $198cc, $18846,
    $17390, $1b9cc, $11084, $17388, $13184, $11082, $13182, $110d8, $1886e,
    $131d8, $110cc, $173d8, $131cc, $110c6, $173cc, $131c6, $110ee, $173ee,
    $1dc50, $1ee2c, $1dc48, $1ee26, $1dc44, $1dc42, $19850, $1cc2c, $1b8d0,
    $19848, $1cc26, $1b8c8, $1dc66, $1b8c4, $19842, $1b8c2, $11050, $1882c,
    $130d0, $11048, $18826, $171d0, $130c8, $19866, $171c8, $1b8e6, $11042,
    $171c4, $130c2, $171c2, $130ec, $171ec, $171e6, $1ee16, $1dc22, $1cc16,
    $19824, $19822, $11028, $13068, $170e8, $11022, $13062, $18560, $10a40,
    $18530, $10a20, $18518, $1c28e, $10a10, $1850c, $10a08, $18506, $10b60,
    $185b8, $1c2de, $10b30, $1859c, $10b18, $1858e, $10b0c, $10b06, $10bb8,
    $185de, $10b9c, $10b8e, $10bde, $18d40, $1c6b0, $1e35c, $18d20, $1c698,
    $18d10, $1c68c, $18d08, $1c686, $18d04, $10940, $184b0, $1c25c, $11b40,
    $10920, $1c6dc, $1c24e, $11b20, $18d98, $1c6ce, $11b10, $10908, $18486,
    $11b08, $18d86, $10902, $109b0, $184dc, $11bb0, $10998, $184ce, $11b98,
    $18dce, $11b8c, $10986, $109dc, $11bdc, $109ce, $11bce, $1cea0, $1e758,
    $1f3ae, $1ce90, $1e74c, $1ce88, $1e746, $1ce84, $1ce82, $18ca0, $1c658,
    $19da0, $18c90, $1c64c, $19d90, $1cecc, $1c646, $19d88, $18c84, $19d84,
    $18c82, $19d82, $108a0, $18458, $119a0, $10890, $1c66e, $13ba0, $11990,
    $18ccc, $18446, $13b90, $19dcc, $10884, $13b88, $11984, $10882, $11982,
    $108d8, $1846e, $119d8, $108cc, $13bd8, $119cc, $108c6, $13bcc, $119c6,
    $108ee, $119ee, $13bee, $1ef50, $1f7ac, $1ef48, $1f7a6, $1ef44, $1ef42,
    $1ce50, $1e72c, $1ded0, $1ef6c, $1e726, $1dec8, $1ef66, $1dec4, $1ce42,
    $1dec2, $18c50, $1c62c, $19cd0, $18c48, $1c626, $1bdd0, $19cc8, $1ce66,
    $1bdc8, $1dee6, $18c42, $1bdc4, $19cc2, $1bdc2, $10850, $1842c, $118d0,
    $10848, $18426, $139d0, $118c8, $18c66, $17bd0, $139c8, $19ce6, $10842,
    $17bc8, $1bde6, $118c2, $17bc4, $1086c, $118ec, $10866, $139ec, $118e6,
    $17bec, $139e6, $17be6, $1ef28, $1f796, $1ef24, $1ef22, $1ce28, $1e716,
    $1de68, $1ef36, $1de64, $1ce22, $1de62, $18c28, $1c616, $19c68, $18c24,
    $1bce8, $19c64, $18c22, $1bce4, $19c62, $1bce2, $10828, $18416, $11868,
    $18c36, $138e8, $11864, $10822, $179e8, $138e4, $11862, $179e4, $138e2,
    $179e2, $11876, $179f6, $1ef12, $1de34, $1de32, $19c34, $1bc74, $1bc72,
    $11834, $13874, $178f4, $178f2, $10540, $10520, $18298, $10510, $10508,
    $10504, $105b0, $10598, $1058c, $10586, $105dc, $105ce, $186a0, $18690,
    $1c34c, $18688, $1c346, $18684, $18682, $104a0, $18258, $10da0, $186d8,
    $1824c, $10d90, $186cc, $10d88, $186c6, $10d84, $10482, $10d82, $104d8,
    $1826e, $10dd8, $186ee, $10dcc, $104c6, $10dc6, $104ee, $10dee, $1c750,
    $1c748, $1c744, $1c742, $18650, $18ed0, $1c76c, $1c326, $18ec8, $1c766,
    $18ec4, $18642, $18ec2, $10450, $10cd0, $10448, $18226, $11dd0, $10cc8,
    $10444, $11dc8, $10cc4, $10442, $11dc4, $10cc2, $1046c, $10cec, $10466,
    $11dec, $10ce6, $11de6, $1e7a8, $1e7a4, $1e7a2, $1c728, $1cf68, $1e7b6,
    $1cf64, $1c722, $1cf62, $18628, $1c316, $18e68, $1c736, $19ee8, $18e64,
    $18622, $19ee4, $18e62, $19ee2, $10428, $18216, $10c68, $18636, $11ce8,
    $10c64, $10422, $13de8, $11ce4, $10c62, $13de4, $11ce2, $10436, $10c76,
    $11cf6, $13df6, $1f7d4, $1f7d2, $1e794, $1efb4, $1e792, $1efb2, $1c714,
    $1cf34, $1c712, $1df74, $1cf32, $1df72, $18614, $18e34, $18612, $19e74,
    $18e32, $1bef4),
   ($1f560, $1fab8, $1ea40, $1f530, $1fa9c, $1ea20, $1f518, $1fa8e, $1ea10,
    $1f50c, $1ea08, $1f506, $1ea04, $1eb60, $1f5b8, $1fade, $1d640, $1eb30,
    $1f59c, $1d620, $1eb18, $1f58e, $1d610, $1eb0c, $1d608, $1eb06, $1d604,
    $1d760, $1ebb8, $1f5de, $1ae40, $1d730, $1eb9c, $1ae20, $1d718, $1eb8e,
    $1ae10, $1d70c, $1ae08, $1d706, $1ae04, $1af60, $1d7b8, $1ebde, $15e40,
    $1af30, $1d79c, $15e20, $1af18, $1d78e, $15e10, $1af0c, $15e08, $1af06,
    $15f60, $1afb8, $1d7de, $15f30, $1af9c, $15f18, $1af8e, $15f0c, $15fb8,
    $1afde, $15f9c, $15f8e, $1e940, $1f4b0, $1fa5c, $1e920, $1f498, $1fa4e,
    $1e910, $1f48c, $1e908, $1f486, $1e904, $1e902, $1d340, $1e9b0, $1f4dc,
    $1d320, $1e998, $1f4ce, $1d310, $1e98c, $1d308, $1e986, $1d304, $1d302,
    $1a740, $1d3b0, $1e9dc, $1a720, $1d398, $1e9ce, $1a710, $1d38c, $1a708,
    $1d386, $1a704, $1a702, $14f40, $1a7b0, $1d3dc, $14f20, $1a798, $1d3ce,
    $14f10, $1a78c, $14f08, $1a786, $14f04, $14fb0, $1a7dc, $14f98, $1a7ce,
    $14f8c, $14f86, $14fdc, $14fce, $1e8a0, $1f458, $1fa2e, $1e890, $1f44c,
    $1e888, $1f446, $1e884, $1e882, $1d1a0, $1e8d8, $1f46e, $1d190, $1e8cc,
    $1d188, $1e8c6, $1d184, $1d182, $1a3a0, $1d1d8, $1e8ee, $1a390, $1d1cc,
    $1a388, $1d1c6, $1a384, $1a382, $147a0, $1a3d8, $1d1ee, $14790, $1a3cc,
    $14788, $1a3c6, $14784, $14782, $147d8, $1a3ee, $147cc, $147c6, $147ee,
    $1e850, $1f42c, $1e848, $1f426, $1e844, $1e842, $1d0d0, $1e86c, $1d0c8,
    $1e866, $1d0c4, $1d0c2, $1a1d0, $1d0ec, $1a1c8, $1d0e6, $1a1c4, $1a1c2,
    $143d0, $1a1ec, $143c8, $1a1e6, $143c4, $143c2, $143ec, $143e6, $1e828,
    $1f416, $1e824, $1e822, $1d068, $1e836, $1d064, $1d062, $1a0e8, $1d076,
    $1a0e4, $1a0e2, $141e8, $1a0f6, $141e4, $141e2, $1e814, $1e812, $1d034,
    $1d032, $1a074, $1a072, $1e540, $1f2b0, $1f95c, $1e520, $1f298, $1f94e,
    $1e510, $1f28c, $1e508, $1f286, $1e504, $1e502, $1cb40, $1e5b0, $1f2dc,
    $1cb20, $1e598, $1f2ce, $1cb10, $1e58c, $1cb08, $1e586, $1cb04, $1cb02,
    $19740, $1cbb0, $1e5dc, $19720, $1cb98, $1e5ce, $19710, $1cb8c, $19708,
    $1cb86, $19704, $19702, $12f40, $197b0, $1cbdc, $12f20, $19798, $1cbce,
    $12f10, $1978c, $12f08, $19786, $12f04, $12fb0, $197dc, $12f98, $197ce,
    $12f8c, $12f86, $12fdc, $12fce, $1f6a0, $1fb58, $16bf0, $1f690, $1fb4c,
    $169f8, $1f688, $1fb46, $168fc, $1f684, $1f682, $1e4a0, $1f258, $1f92e,
    $1eda0, $1e490, $1fb6e, $1ed90, $1f6cc, $1f246, $1ed88, $1e484, $1ed84,
    $1e482, $1ed82, $1c9a0, $1e4d8, $1f26e, $1dba0, $1c990, $1e4cc, $1db90,
    $1edcc, $1e4c6, $1db88, $1c984, $1db84, $1c982, $1db82, $193a0, $1c9d8,
    $1e4ee, $1b7a0, $19390, $1c9cc, $1b790, $1dbcc, $1c9c6, $1b788, $19384,
    $1b784, $19382, $1b782, $127a0, $193d8, $1c9ee, $16fa0, $12790, $193cc,
    $16f90, $1b7cc, $193c6, $16f88, $12784, $16f84, $12782, $127d8, $193ee,
    $16fd8, $127cc, $16fcc, $127c6, $16fc6, $127ee, $1f650, $1fb2c, $165f8,
    $1f648, $1fb26, $164fc, $1f644, $1647e, $1f642, $1e450, $1f22c, $1ecd0,
    $1e448, $1f226, $1ecc8, $1f666, $1ecc4, $1e442, $1ecc2, $1c8d0, $1e46c,
    $1d9d0, $1c8c8, $1e466, $1d9c8, $1ece6, $1d9c4, $1c8c2, $1d9c2, $191d0,
    $1c8ec, $1b3d0, $191c8, $1c8e6, $1b3c8, $1d9e6, $1b3c4, $191c2, $1b3c2,
    $123d0, $191ec, $167d0, $123c8, $191e6, $167c8, $1b3e6, $167c4, $123c2,
    $167c2, $123ec, $167ec, $123e6, $167e6, $1f628, $1fb16, $162fc, $1f624,
    $1627e, $1f622, $1e428, $1f216, $1ec68, $1f636, $1ec64, $1e422, $1ec62,
    $1c868, $1e436, $1d8e8, $1c864, $1d8e4, $1c862, $1d8e2, $190e8, $1c876,
    $1b1e8, $1d8f6, $1b1e4, $190e2, $1b1e2, $121e8, $190f6, $163e8, $121e4,
    $163e4, $121e2, $163e2, $121f6, $163f6, $1f614, $1617e, $1f612, $1e414,
    $1ec34, $1e412, $1ec32, $1c834, $1d874, $1c832, $1d872, $19074, $1b0f4,
    $19072, $1b0f2, $120f4, $161f4, $120f2, $161f2, $1f60a, $1e40a, $1ec1a,
    $1c81a, $1d83a, $1903a, $1b07a, $1e2a0, $1f158, $1f8ae, $1e290, $1f14c,
    $1e288, $1f146, $1e284, $1e282, $1c5a0, $1e2d8, $1f16e, $1c590, $1e2cc,
    $1c588, $1e2c6, $1c584, $1c582, $18ba0, $1c5d8, $1e2ee, $18b90, $1c5cc,
    $18b88, $1c5c6, $18b84, $18b82, $117a0, $18bd8, $1c5ee, $11790, $18bcc,
    $11788, $18bc6, $11784, $11782, $117d8, $18bee, $117cc, $117c6, $117ee,
    $1f350, $1f9ac, $135f8, $1f348, $1f9a6, $134fc, $1f344, $1347e, $1f342,
    $1e250, $1f12c, $1e6d0, $1e248, $1f126, $1e6c8, $1f366, $1e6c4, $1e242,
    $1e6c2, $1c4d0, $1e26c, $1cdd0, $1c4c8, $1e266, $1cdc8, $1e6e6, $1cdc4,
    $1c4c2, $1cdc2, $189d0, $1c4ec, $19bd0, $189c8, $1c4e6, $19bc8, $1cde6,
    $19bc4, $189c2, $19bc2, $113d0, $189ec, $137d0, $113c8, $189e6, $137c8,
    $19be6, $137c4, $113c2, $137c2, $113ec, $137ec, $113e6, $137e6, $1fba8,
    $175f0, $1bafc, $1fba4, $174f8, $1ba7e, $1fba2, $1747c, $1743e, $1f328,
    $1f996, $132fc, $1f768, $1fbb6, $176fc, $1327e, $1f764, $1f322, $1767e,
    $1f762, $1e228, $1f116, $1e668, $1e224, $1eee8, $1f776, $1e222, $1eee4,
    $1e662, $1eee2, $1c468, $1e236, $1cce8, $1c464, $1dde8, $1cce4, $1c462,
    $1dde4, $1cce2, $1dde2, $188e8, $1c476, $199e8, $188e4, $1bbe8, $199e4,
    $188e2, $1bbe4, $199e2, $1bbe2, $111e8, $188f6, $133e8, $111e4, $177e8,
    $133e4, $111e2, $177e4, $133e2, $177e2, $111f6, $133f6, $1fb94, $172f8,
    $1b97e, $1fb92, $1727c, $1723e, $1f314, $1317e, $1f734, $1f312, $1737e,
    $1f732, $1e214, $1e634, $1e212, $1ee74, $1e632, $1ee72, $1c434, $1cc74,
    $1c432, $1dcf4, $1cc72, $1dcf2, $18874, $198f4, $18872, $1b9f4, $198f2,
    $1b9f2, $110f4, $131f4, $110f2, $173f4, $131f2, $173f2, $1fb8a, $1717c,
    $1713e, $1f30a, $1f71a, $1e20a, $1e61a, $1ee3a, $1c41a, $1cc3a, $1dc7a,
    $1883a, $1987a, $1b8fa, $1107a, $130fa, $171fa, $170be, $1e150, $1f0ac,
    $1e148, $1f0a6, $1e144, $1e142, $1c2d0, $1e16c, $1c2c8, $1e166, $1c2c4,
    $1c2c2, $185d0, $1c2ec, $185c8, $1c2e6, $185c4, $185c2, $10bd0, $185ec,
    $10bc8, $185e6, $10bc4, $10bc2, $10bec, $10be6, $1f1a8, $1f8d6, $11afc,
    $1f1a4, $11a7e, $1f1a2, $1e128, $1f096, $1e368, $1e124, $1e364, $1e122,
    $1e362, $1c268, $1e136, $1c6e8, $1c264, $1c6e4, $1c262, $1c6e2, $184e8,
    $1c276, $18de8, $184e4, $18de4, $184e2, $18de2, $109e8, $184f6, $11be8,
    $109e4, $11be4, $109e2, $11be2, $109f6, $11bf6, $1f9d4, $13af8, $19d7e,
    $1f9d2, $13a7c, $13a3e, $1f194, $1197e, $1f3b4, $1f192, $13b7e, $1f3b2,
    $1e114, $1e334, $1e112, $1e774, $1e332, $1e772, $1c234, $1c674, $1c232,
    $1cef4, $1c672, $1cef2, $18474, $18cf4, $18472, $19df4, $18cf2, $19df2,
    $108f4, $119f4, $108f2, $13bf4, $119f2, $13bf2, $17af0, $1bd7c, $17a78,
    $1bd3e, $17a3c, $17a1e, $1f9ca, $1397c, $1fbda, $17b7c, $1393e, $17b3e,
    $1f18a, $1f39a, $1f7ba, $1e10a, $1e31a, $1e73a, $1ef7a, $1c21a, $1c63a,
    $1ce7a, $1defa, $1843a, $18c7a, $19cfa, $1bdfa, $1087a, $118fa, $139fa,
    $17978, $1bcbe, $1793c, $1791e, $138be, $179be, $178bc, $1789e, $1785e,
    $1e0a8, $1e0a4, $1e0a2, $1c168, $1e0b6, $1c164, $1c162, $182e8, $1c176,
    $182e4, $182e2, $105e8, $182f6, $105e4, $105e2, $105f6, $1f0d4, $10d7e,
    $1f0d2, $1e094, $1e1b4, $1e092, $1e1b2, $1c134, $1c374, $1c132, $1c372,
    $18274, $186f4, $18272, $186f2, $104f4, $10df4, $104f2, $10df2, $1f8ea,
    $11d7c, $11d3e, $1f0ca, $1f1da, $1e08a, $1e19a, $1e3ba, $1c11a, $1c33a,
    $1c77a, $1823a, $1867a, $18efa, $1047a, $10cfa, $11dfa, $13d78, $19ebe,
    $13d3c, $13d1e, $11cbe, $13dbe, $17d70, $1bebc, $17d38, $1be9e, $17d1c,
    $17d0e, $13cbc, $17dbc, $13c9e, $17d9e, $17cb8, $1be5e, $17c9c, $17c8e,
    $13c5e, $17cde, $17c5c, $17c4e, $17c2e, $1c0b4, $1c0b2, $18174, $18172,
    $102f4, $102f2, $1e0da, $1c09a, $1c1ba, $1813a, $1837a, $1027a, $106fa,
    $10ebe, $11ebc, $11e9e, $13eb8, $19f5e, $13e9c, $13e8e, $11e5e, $13ede,
    $17eb0, $1bf5c, $17e98, $1bf4e, $17e8c, $17e86, $13e5c, $17edc, $13e4e,
    $17ece, $17e58, $1bf2e, $17e4c, $17e46, $13e2e, $17e6e, $17e2c, $17e26,
    $10f5e, $11f5c, $11f4e, $13f58, $19fae, $13f4c, $13f46, $11f2e, $13f6e,
    $13f2c, $13f26),
   ($1abe0, $1d5f8, $153c0, $1a9f0, $1d4fc, $151e0, $1a8f8, $1d47e, $150f0,
    $1a87c, $15078, $1fad0, $15be0, $1adf8, $1fac8, $159f0, $1acfc, $1fac4,
    $158f8, $1ac7e, $1fac2, $1587c, $1f5d0, $1faec, $15df8, $1f5c8, $1fae6,
    $15cfc, $1f5c4, $15c7e, $1f5c2, $1ebd0, $1f5ec, $1ebc8, $1f5e6, $1ebc4,
    $1ebc2, $1d7d0, $1ebec, $1d7c8, $1ebe6, $1d7c4, $1d7c2, $1afd0, $1d7ec,
    $1afc8, $1d7e6, $1afc4, $14bc0, $1a5f0, $1d2fc, $149e0, $1a4f8, $1d27e,
    $148f0, $1a47c, $14878, $1a43e, $1483c, $1fa68, $14df0, $1a6fc, $1fa64,
    $14cf8, $1a67e, $1fa62, $14c7c, $14c3e, $1f4e8, $1fa76, $14efc, $1f4e4,
    $14e7e, $1f4e2, $1e9e8, $1f4f6, $1e9e4, $1e9e2, $1d3e8, $1e9f6, $1d3e4,
    $1d3e2, $1a7e8, $1d3f6, $1a7e4, $1a7e2, $145e0, $1a2f8, $1d17e, $144f0,
    $1a27c, $14478, $1a23e, $1443c, $1441e, $1fa34, $146f8, $1a37e, $1fa32,
    $1467c, $1463e, $1f474, $1477e, $1f472, $1e8f4, $1e8f2, $1d1f4, $1d1f2,
    $1a3f4, $1a3f2, $142f0, $1a17c, $14278, $1a13e, $1423c, $1421e, $1fa1a,
    $1437c, $1433e, $1f43a, $1e87a, $1d0fa, $14178, $1a0be, $1413c, $1411e,
    $141be, $140bc, $1409e, $12bc0, $195f0, $1cafc, $129e0, $194f8, $1ca7e,
    $128f0, $1947c, $12878, $1943e, $1283c, $1f968, $12df0, $196fc, $1f964,
    $12cf8, $1967e, $1f962, $12c7c, $12c3e, $1f2e8, $1f976, $12efc, $1f2e4,
    $12e7e, $1f2e2, $1e5e8, $1f2f6, $1e5e4, $1e5e2, $1cbe8, $1e5f6, $1cbe4,
    $1cbe2, $197e8, $1cbf6, $197e4, $197e2, $1b5e0, $1daf8, $1ed7e, $169c0,
    $1b4f0, $1da7c, $168e0, $1b478, $1da3e, $16870, $1b43c, $16838, $1b41e,
    $1681c, $125e0, $192f8, $1c97e, $16de0, $124f0, $1927c, $16cf0, $1b67c,
    $1923e, $16c78, $1243c, $16c3c, $1241e, $16c1e, $1f934, $126f8, $1937e,
    $1fb74, $1f932, $16ef8, $1267c, $1fb72, $16e7c, $1263e, $16e3e, $1f274,
    $1277e, $1f6f4, $1f272, $16f7e, $1f6f2, $1e4f4, $1edf4, $1e4f2, $1edf2,
    $1c9f4, $1dbf4, $1c9f2, $1dbf2, $193f4, $193f2, $165c0, $1b2f0, $1d97c,
    $164e0, $1b278, $1d93e, $16470, $1b23c, $16438, $1b21e, $1641c, $1640e,
    $122f0, $1917c, $166f0, $12278, $1913e, $16678, $1b33e, $1663c, $1221e,
    $1661e, $1f91a, $1237c, $1fb3a, $1677c, $1233e, $1673e, $1f23a, $1f67a,
    $1e47a, $1ecfa, $1c8fa, $1d9fa, $191fa, $162e0, $1b178, $1d8be, $16270,
    $1b13c, $16238, $1b11e, $1621c, $1620e, $12178, $190be, $16378, $1213c,
    $1633c, $1211e, $1631e, $121be, $163be, $16170, $1b0bc, $16138, $1b09e,
    $1611c, $1610e, $120bc, $161bc, $1209e, $1619e, $160b8, $1b05e, $1609c,
    $1608e, $1205e, $160de, $1605c, $1604e, $115e0, $18af8, $1c57e, $114f0,
    $18a7c, $11478, $18a3e, $1143c, $1141e, $1f8b4, $116f8, $18b7e, $1f8b2,
    $1167c, $1163e, $1f174, $1177e, $1f172, $1e2f4, $1e2f2, $1c5f4, $1c5f2,
    $18bf4, $18bf2, $135c0, $19af0, $1cd7c, $134e0, $19a78, $1cd3e, $13470,
    $19a3c, $13438, $19a1e, $1341c, $1340e, $112f0, $1897c, $136f0, $11278,
    $1893e, $13678, $19b3e, $1363c, $1121e, $1361e, $1f89a, $1137c, $1f9ba,
    $1377c, $1133e, $1373e, $1f13a, $1f37a, $1e27a, $1e6fa, $1c4fa, $1cdfa,
    $189fa, $1bae0, $1dd78, $1eebe, $174c0, $1ba70, $1dd3c, $17460, $1ba38,
    $1dd1e, $17430, $1ba1c, $17418, $1ba0e, $1740c, $132e0, $19978, $1ccbe,
    $176e0, $13270, $1993c, $17670, $1bb3c, $1991e, $17638, $1321c, $1761c,
    $1320e, $1760e, $11178, $188be, $13378, $1113c, $17778, $1333c, $1111e,
    $1773c, $1331e, $1771e, $111be, $133be, $177be, $172c0, $1b970, $1dcbc,
    $17260, $1b938, $1dc9e, $17230, $1b91c, $17218, $1b90e, $1720c, $17206,
    $13170, $198bc, $17370, $13138, $1989e, $17338, $1b99e, $1731c, $1310e,
    $1730e, $110bc, $131bc, $1109e, $173bc, $1319e, $1739e, $17160, $1b8b8,
    $1dc5e, $17130, $1b89c, $17118, $1b88e, $1710c, $17106, $130b8, $1985e,
    $171b8, $1309c, $1719c, $1308e, $1718e, $1105e, $130de, $171de, $170b0,
    $1b85c, $17098, $1b84e, $1708c, $17086, $1305c, $170dc, $1304e, $170ce,
    $17058, $1b82e, $1704c, $17046, $1302e, $1706e, $1702c, $17026, $10af0,
    $1857c, $10a78, $1853e, $10a3c, $10a1e, $10b7c, $10b3e, $1f0ba, $1e17a,
    $1c2fa, $185fa, $11ae0, $18d78, $1c6be, $11a70, $18d3c, $11a38, $18d1e,
    $11a1c, $11a0e, $10978, $184be, $11b78, $1093c, $11b3c, $1091e, $11b1e,
    $109be, $11bbe, $13ac0, $19d70, $1cebc, $13a60, $19d38, $1ce9e, $13a30,
    $19d1c, $13a18, $19d0e, $13a0c, $13a06, $11970, $18cbc, $13b70, $11938,
    $18c9e, $13b38, $1191c, $13b1c, $1190e, $13b0e, $108bc, $119bc, $1089e,
    $13bbc, $1199e, $13b9e, $1bd60, $1deb8, $1ef5e, $17a40, $1bd30, $1de9c,
    $17a20, $1bd18, $1de8e, $17a10, $1bd0c, $17a08, $1bd06, $17a04, $13960,
    $19cb8, $1ce5e, $17b60, $13930, $19c9c, $17b30, $1bd9c, $19c8e, $17b18,
    $1390c, $17b0c, $13906, $17b06, $118b8, $18c5e, $139b8, $1189c, $17bb8,
    $1399c, $1188e, $17b9c, $1398e, $17b8e, $1085e, $118de, $139de, $17bde,
    $17940, $1bcb0, $1de5c, $17920, $1bc98, $1de4e, $17910, $1bc8c, $17908,
    $1bc86, $17904, $17902, $138b0, $19c5c, $179b0, $13898, $19c4e, $17998,
    $1bcce, $1798c, $13886, $17986, $1185c, $138dc, $1184e, $179dc, $138ce,
    $179ce, $178a0, $1bc58, $1de2e, $17890, $1bc4c, $17888, $1bc46, $17884,
    $17882, $13858, $19c2e, $178d8, $1384c, $178cc, $13846, $178c6, $1182e,
    $1386e, $178ee, $17850, $1bc2c, $17848, $1bc26, $17844, $17842, $1382c,
    $1786c, $13826, $17866, $17828, $1bc16, $17824, $17822, $13816, $17836,
    $10578, $182be, $1053c, $1051e, $105be, $10d70, $186bc, $10d38, $1869e,
    $10d1c, $10d0e, $104bc, $10dbc, $1049e, $10d9e, $11d60, $18eb8, $1c75e,
    $11d30, $18e9c, $11d18, $18e8e, $11d0c, $11d06, $10cb8, $1865e, $11db8,
    $10c9c, $11d9c, $10c8e, $11d8e, $1045e, $10cde, $11dde, $13d40, $19eb0,
    $1cf5c, $13d20, $19e98, $1cf4e, $13d10, $19e8c, $13d08, $19e86, $13d04,
    $13d02, $11cb0, $18e5c, $13db0, $11c98, $18e4e, $13d98, $19ece, $13d8c,
    $11c86, $13d86, $10c5c, $11cdc, $10c4e, $13ddc, $11cce, $13dce, $1bea0,
    $1df58, $1efae, $1be90, $1df4c, $1be88, $1df46, $1be84, $1be82, $13ca0,
    $19e58, $1cf2e, $17da0, $13c90, $19e4c, $17d90, $1becc, $19e46, $17d88,
    $13c84, $17d84, $13c82, $17d82, $11c58, $18e2e, $13cd8, $11c4c, $17dd8,
    $13ccc, $11c46, $17dcc, $13cc6, $17dc6, $10c2e, $11c6e, $13cee, $17dee,
    $1be50, $1df2c, $1be48, $1df26, $1be44, $1be42, $13c50, $19e2c, $17cd0,
    $13c48, $19e26, $17cc8, $1be66, $17cc4, $13c42, $17cc2, $11c2c, $13c6c,
    $11c26, $17cec, $13c66, $17ce6, $1be28, $1df16, $1be24, $1be22, $13c28,
    $19e16, $17c68, $13c24, $17c64, $13c22, $17c62, $11c16, $13c36, $17c76,
    $1be14, $1be12, $13c14, $17c34, $13c12, $17c32, $102bc, $1029e, $106b8,
    $1835e, $1069c, $1068e, $1025e, $106de, $10eb0, $1875c, $10e98, $1874e,
    $10e8c, $10e86, $1065c, $10edc, $1064e, $10ece, $11ea0, $18f58, $1c7ae,
    $11e90, $18f4c, $11e88, $18f46, $11e84, $11e82, $10e58, $1872e, $11ed8,
    $18f6e, $11ecc, $10e46, $11ec6, $1062e, $10e6e, $11eee, $19f50, $1cfac,
    $19f48, $1cfa6, $19f44, $19f42, $11e50, $18f2c, $13ed0, $19f6c, $18f26,
    $13ec8, $11e44, $13ec4, $11e42, $13ec2, $10e2c, $11e6c, $10e26, $13eec,
    $11e66, $13ee6, $1dfa8, $1efd6, $1dfa4, $1dfa2, $19f28, $1cf96, $1bf68,
    $19f24, $1bf64, $19f22, $1bf62, $11e28, $18f16, $13e68, $11e24, $17ee8,
    $13e64, $11e22, $17ee4, $13e62, $17ee2, $10e16, $11e36, $13e76, $17ef6,
    $1df94, $1df92, $19f14, $1bf34, $19f12, $1bf32, $11e14, $13e34, $11e12,
    $17e74, $13e32, $17e72, $1df8a, $19f0a, $1bf1a, $11e0a, $13e1a, $17e3a,
    $1035c, $1034e, $10758, $183ae, $1074c, $10746, $1032e, $1076e, $10f50,
    $187ac, $10f48, $187a6, $10f44, $10f42, $1072c, $10f6c, $10726, $10f66,
    $18fa8, $1c7d6, $18fa4, $18fa2, $10f28, $18796, $11f68, $18fb6, $11f64,
    $10f22, $11f62, $10716, $10f36, $11f76, $1cfd4, $1cfd2, $18f94, $19fb4,
    $18f92, $19fb2, $10f14, $11f34, $10f12, $13f74, $11f32, $13f72, $1cfca,
    $18f8a, $19f9a, $10f0a, $11f1a, $13f3a, $103ac, $103a6, $107a8, $183d6,
    $107a4, $107a2, $10396, $107b6, $187d4, $187d2, $10794, $10fb4, $10792,
    $10fb2, $1c7ea));

type
  TStPDF417TextCompactionMode = (cmAlpha, cmLower, cmMixed, cmPunctuation,
                                 cmNone);
  TStPDF417TextCompactionModes = set of TStPDF417TextCompactionMode;

  TStPDF417TextCompactionData = record
    Value : Integer;
    Mode : TStPDF417TextCompactionModes;
  end;

const
  TStPDF417TextCompaction : array [0..127] of TStPDF417TextCompactionData =
    ((Value : -1; Mode : []),                                  { 000 }
     (Value : -1; Mode : []),                                  { 001 }
     (Value : -1; Mode : []),                                  { 002 }
     (Value : -1; Mode : []),                                  { 003 }
     (Value : -1; Mode : []),                                  { 004 }
     (Value : -1; Mode : []),                                  { 005 }
     (Value : -1; Mode : []),                                  { 006 }
     (Value : -1; Mode : []),                                  { 007 }
     (Value : -1; Mode : []),                                  { 008 }
     (Value : 12; Mode : [cmMixed, cmPunctuation]),            { 009 }
     (Value : 15; Mode : [cmPunctuation]),                     { 010 }
     (Value : -1; Mode : []),                                  { 011 }
     (Value : -1; Mode : []),                                  { 012 }
     (Value : 11; Mode : [cmMixed, cmPunctuation]),            { 013 }
     (Value : -1; Mode : []),                                  { 014 }
     (Value : -1; Mode : []),                                  { 015 }
     (Value : -1; Mode : []),                                  { 016 }
     (Value : -1; Mode : []),                                  { 017 }
     (Value : -1; Mode : []),                                  { 018 }
     (Value : -1; Mode : []),                                  { 019 }
     (Value : -1; Mode : []),                                  { 020 }
     (Value : -1; Mode : []),                                  { 021 }
     (Value : -1; Mode : []),                                  { 022 }
     (Value : -1; Mode : []),                                  { 023 }
     (Value : -1; Mode : []),                                  { 024 }
     (Value : -1; Mode : []),                                  { 025 }
     (Value : -1; Mode : []),                                  { 026 }
     (Value : -1; Mode : []),                                  { 027 }
     (Value : -1; Mode : []),                                  { 028 }
     (Value : -1; Mode : []),                                  { 029 }
     (Value : -1; Mode : []),                                  { 030 }
     (Value : -1; Mode : []),                                  { 031 }
     (Value : 26; Mode : [cmAlpha, cmLower, cmMixed]),         { 032 }
     (Value : 10; Mode : [cmPunctuation]),                     { 033 }
     (Value : 20; Mode : [cmPunctuation]),                     { 034 }
     (Value : 15; Mode : [cmMixed]),                           { 035 }
     (Value : 18; Mode : [cmMixed, cmPunctuation]),            { 036 }
     (Value : 21; Mode : [cmMixed]),                           { 037 }
     (Value : 10; Mode : [cmMixed]),                           { 038 }
     (Value : 28; Mode : [cmPunctuation]),                     { 039 }
     (Value : 23; Mode : [cmPunctuation]),                     { 040 }
     (Value : 24; Mode : [cmPunctuation]),                     { 041 }
     (Value : 22; Mode : [cmMixed, cmPunctuation]),            { 042 }
     (Value : 20; Mode : [cmMixed]),                           { 043 }
     (Value : 13; Mode : [cmMixed, cmPunctuation]),            { 044 }
     (Value : 16; Mode : [cmMixed, cmPunctuation]),            { 045 }
     (Value : 17; Mode : [cmMixed, cmPunctuation]),            { 046 }
     (Value : 19; Mode : [cmMixed, cmPunctuation]),            { 047 }
     (Value :  0; Mode : [cmMixed]),                           { 048 }
     (Value :  1; Mode : [cmMixed]),                           { 049 }
     (Value :  2; Mode : [cmMixed]),                           { 050 }
     (Value :  3; Mode : [cmMixed]),                           { 051 }
     (Value :  4; Mode : [cmMixed]),                           { 052 }
     (Value :  5; Mode : [cmMixed]),                           { 053 }
     (Value :  6; Mode : [cmMixed]),                           { 054 }
     (Value :  7; Mode : [cmMixed]),                           { 055 }
     (Value :  8; Mode : [cmMixed]),                           { 056 }
     (Value :  9; Mode : [cmMixed]),                           { 057 }
     (Value : 14; Mode : [cmMixed, cmPunctuation]),            { 058 }
     (Value :  0; Mode : [cmPunctuation]),                     { 059 }
     (Value :  1; Mode : [cmPunctuation]),                     { 060 }
     (Value : 23; Mode : [cmMixed]),                           { 061 }
     (Value :  2; Mode : [cmPunctuation]),                     { 062 }
     (Value : 25; Mode : [cmPunctuation]),                     { 063 }
     (Value :  3; Mode : [cmPunctuation]),                     { 064 }
     (Value :  0; Mode : [cmAlpha]),                           { 065 }
     (Value :  1; Mode : [cmAlpha]),                           { 066 }
     (Value :  2; Mode : [cmAlpha]),                           { 067 }
     (Value :  3; Mode : [cmAlpha]),                           { 068 }
     (Value :  4; Mode : [cmAlpha]),                           { 069 }
     (Value :  5; Mode : [cmAlpha]),                           { 070 }
     (Value :  6; Mode : [cmAlpha]),                           { 071 }
     (Value :  7; Mode : [cmAlpha]),                           { 072 }
     (Value :  8; Mode : [cmAlpha]),                           { 073 }
     (Value :  9; Mode : [cmAlpha]),                           { 074 }
     (Value : 10; Mode : [cmAlpha]),                           { 075 }
     (Value : 11; Mode : [cmAlpha]),                           { 076 }
     (Value : 12; Mode : [cmAlpha]),                           { 077 }
     (Value : 13; Mode : [cmAlpha]),                           { 078 }
     (Value : 14; Mode : [cmAlpha]),                           { 079 }
     (Value : 15; Mode : [cmAlpha]),                           { 080 }
     (Value : 16; Mode : [cmAlpha]),                           { 081 }
     (Value : 17; Mode : [cmAlpha]),                           { 082 }
     (Value : 18; Mode : [cmAlpha]),                           { 083 }
     (Value : 19; Mode : [cmAlpha]),                           { 084 }
     (Value : 20; Mode : [cmAlpha]),                           { 085 }
     (Value : 21; Mode : [cmAlpha]),                           { 086 }
     (Value : 22; Mode : [cmAlpha]),                           { 087 }
     (Value : 23; Mode : [cmAlpha]),                           { 088 }
     (Value : 24; Mode : [cmAlpha]),                           { 089 }
     (Value : 25; Mode : [cmAlpha]),                           { 090 }
     (Value :  4; Mode : [cmPunctuation]),                     { 091 }
     (Value :  5; Mode : [cmPunctuation]),                     { 092 }
     (Value :  6; Mode : [cmPunctuation]),                     { 093 }
     (Value : 24; Mode : [cmMixed]),                           { 094 }
     (Value :  7; Mode : [cmPunctuation]),                     { 095 }
     (Value :  8; Mode : [cmPunctuation]),                     { 096 }
     (Value :  0; Mode : [cmLower]),                           { 097 }
     (Value :  1; Mode : [cmLower]),                           { 098 }
     (Value :  2; Mode : [cmLower]),                           { 099 }
     (Value :  3; Mode : [cmLower]),                           { 100 }
     (Value :  4; Mode : [cmLower]),                           { 101 }
     (Value :  5; Mode : [cmLower]),                           { 102 }
     (Value :  6; Mode : [cmLower]),                           { 103 }
     (Value :  7; Mode : [cmLower]),                           { 104 }
     (Value :  8; Mode : [cmLower]),                           { 105 }
     (Value :  9; Mode : [cmLower]),                           { 106 }
     (Value : 10; Mode : [cmLower]),                           { 107 }
     (Value : 11; Mode : [cmLower]),                           { 108 }
     (Value : 12; Mode : [cmLower]),                           { 109 }
     (Value : 13; Mode : [cmLower]),                           { 110 }
     (Value : 14; Mode : [cmLower]),                           { 111 }
     (Value : 15; Mode : [cmLower]),                           { 112 }
     (Value : 16; Mode : [cmLower]),                           { 113 }
     (Value : 17; Mode : [cmLower]),                           { 114 }
     (Value : 18; Mode : [cmLower]),                           { 115 }
     (Value : 19; Mode : [cmLower]),                           { 116 }
     (Value : 20; Mode : [cmLower]),                           { 117 }
     (Value : 21; Mode : [cmLower]),                           { 118 }
     (Value : 22; Mode : [cmLower]),                           { 119 }
     (Value : 23; Mode : [cmLower]),                           { 120 }
     (Value : 24; Mode : [cmLower]),                           { 121 }
     (Value : 25; Mode : [cmLower]),                           { 122 }
     (Value : 26; Mode : [cmPunctuation]),                     { 123 }
     (Value : 21; Mode : [cmPunctuation]),                     { 124 }
     (Value : 27; Mode : [cmPunctuation]),                     { 125 }
     (Value :  9; Mode : [cmPunctuation]),                     { 126 }
     (Value : -1; Mode : []));                                 { 127 }

{ TStMaxiCode types and constants }

type
  TStMaxiCodeCodeSet = (csCodeSetA, csCodeSetB, csCodeSetC, csCodeSetD,
                        csCodeSetE, csNone);

const
  StMaxiCodeCodeSets : array [csCodeSetA..csCodeSetE] of
                       array [0..255] of ShortInt =
    { csCodeSetA }
      {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}
    ((-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {000}
      -1, -1, -1,  0, -1, -1, -1, -1, -1, -1, {010}
      -1, -1, -1, -1, -1, -1, -1, -1, 28, 29, {020}
      30, -1, 32, 33, 34, 35, 36, 37, 38, 39, {030}
      40, 41, 42, 43, 44, 45, 46, 47, 48, 49, {040}
      50, 51, 52, 53, 54, 55, 56, 57, 58, -1, {050}
      -1, -1, -1, -1, -1,  1,  2,  3,  4,  5, {060}
       6,  7,  8,  9, 10, 11, 12, 13, 14, 15, {070}
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, {080}
      26, -1, -1, -1, -1, -1, -1, -1, -1, -1, {090}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {100}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {110}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {120}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {130}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {140}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {150}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {160}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {170}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {180}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {190}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {200}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {210}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {220}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {230}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {240}
      -1, -1, -1, -1, -1, -1),                {250}
     { csCodeSetB }
      {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {000}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {010}
      -1, -1, -1, -1, -1, -1, -1, -1, 28, 29, {020}
      30, -1, 47, 53, -1, -1, -1, -1, -1, -1, {030}
      -1, -1, -1, -1, 48, 49, 50, -1, -1, -1, {040}
      -1, -1, -1, -1, -1, -1, -1, -1, 51, 37, {050}
      38, 39, 40, 41, 52, -1, -1, -1, -1, -1, {060}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {070}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {080}
      -1, 42, 43, 44, 45, 46,  0,  1,  2,  3, {090}
       4,  5,  6,  7,  8,  9, 10, 11, 12, 13, {100}
      14, 15, 16, 17, 18, 19, 20, 21, 22, 23, {110}
      24, 25, 26, 32, 54, 34, 35, 36, -1, -1, {120}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {130}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {140}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {150}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {160}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {170}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {180}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {190}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {200}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {210}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {220}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {230}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {240}
      -1, -1, -1, -1, -1, -1),                {250}
     { csCodeSetC }
      {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {000}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {010}
      -1, -1, -1, -1, -1, -1, -1, -1, 28, 29, {020}
      30, -1, 59, -1, -1, -1, -1, -1, -1, -1, {030}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {040}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {050}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {060}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {070}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {080}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {090}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {100}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {110}
      -1, -1, -1, -1, -1, -1, -1, -1, 48, 49, {120}
      50, 51, 52, 53, 54, 55, 56, 57, -1, -1, {130}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {140}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {150}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {160}
      37, -1, 38, -1, -1, -1, -1, 39, 40, 41, {170}
      -1, 42, -1, -1, -1, 43, 44, -1, 45, 46, {180}
      47, -1,  0,  1,  2,  3,  4,  5,  6,  7, {190}
       8,  9, 10, 11, 12, 13, 14, 15, 16, 17, {200}
      18, 19, 20, 21, 22, 23, 24, 25, 26, 32, {210}
      33, 34, 35, 36, -1, -1, -1, -1, -1, -1, {220}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {230}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {240}
      -1, -1, -1, -1, -1, -1),                {250}
     { csCodeSetD }
      {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}
     (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {000}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {010}
      -1, -1, -1, -1, -1, -1, -1, -1, 28, 29, {020}
      30, -1, 59, -1, -1, -1, -1, -1, -1, -1, {030}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {040}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {050}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {060}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {070}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {080}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {090}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {100}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {110}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {120}
      -1, -1, -1, -1, -1, -1, -1, -1, 47, 48, {130}
      49, 50, 51, 52, 53, 54, 55, 56, 57, -1, {140}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {150}
      -1, 37, -1, -1, -1, -1, -1, -1, 38, -1, {160}
      -1, 39, -1, -1, -1, 40, 41, -1, -1, -1, {170}
      42, -1, -1, 43, 44, -1, -1, 45, -1, -1, {180}
      -1, 46, -1, -1, -1, -1, -1, -1, -1, -1, {190}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {200}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {210}
      -1, -1, -1, -1,  0,  1,  2,  3,  4,  5, {220}
       6,  7,  8,  9, 10, 11, 12, 13, 14, 15, {230}
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, {240}
      26, 32, 33, 34, 35, 36),                {250}
     { csCodeSetE }
      {0} {1} {2} {3} {4} {5} {6} {7} {8} {9}
     ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, {000}
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, {010}
      20, 21, 22, 23, 24, 25, 26, 30, 32, 33, {020}
      34, 35, 59, -1, -1, -1, -1, -1, -1, -1, {030}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {040}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {050}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {060}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {070}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {080}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {090}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {100}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {110}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {120}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {130}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, 48, {140}
      49, 50, 51, 52, 53, 54, 55, 56, 57, 36, {150}
      37, -1, 38, 39, 40, 41, 42, 43, -1, 44, {160}
      -1, -1, -1, 45, 46, -1, -1, -1, -1, -1, {170}
      -1, -1, 47, -1, -1, -1, -1, -1, -1, -1, {180}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {190}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {200}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {210}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {220}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {230}
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, {240}
      -1, -1, -1, -1, -1, -1));               {250}

{ TStCustom2DBarcode }

constructor TStCustom2DBarcode.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  FBitmap := TBitmap.Create;

  FBarWidth          := 2;
  FBarHeight         := 0;
  FBackgroundColor   := clWhite;
  FExtendedSyntax    := True;
  FQuietZone         := 8;
  FAlignment         := taCenter;
  FCaptionLayout     := tlBottom;
  Width              := 329;
  Height             := 50;
  Color              := clBlack;
  FECCLevel          := 2;
  FRelativeBarHeight := False;
  FBarHeightToWidth  := 4;
  FBarHeight         := 2;
  FCaption           := '';
  FCode              := '';
  FECCLevel          := 0;
end;

destructor TStCustom2DBarcode.Destroy;
begin
  FBitmap.Free;

  inherited Destroy;
end;

procedure TStCustom2DBarcode.CopyToClipboard;
begin
  CopyToClipboardRes (0, 0);
end;

procedure TStCustom2DBarcode.CopyToClipboardRes (ResX : Integer;
                                                 ResY : Integer);
var
  MetaFile       : TMetaFile;
  MetaFileCanvas : TMetaFileCanvas;
  RenderBMP      : TBitmap;
  SizeX          : Integer;
  SizeY          : Integer;

begin
  Clipboard.Clear;
  Clipboard.Open;
  try
    RenderBmp := TBitmap.Create;
    try
      RenderToResolution (RenderBmp, ResX, ResY, SizeX, SizeY);
      Clipboard.Assign (RenderBmp);

      {metafile}
      MetaFile := TMetaFile.Create;
      try
        MetaFileCanvas := TMetaFileCanvas.Create (MetaFile, 0);
        try
          MetaFile.Enhanced := True;
          MetaFile.Width := ClientWidth;
          MetaFile.Height := ClientHeight;
          MetaFileCanvas.Draw (0, 0, RenderBmp);
        finally
          MetaFileCanvas.Free;
        end;
        Clipboard.Assign (MetaFile);
      finally
        MetaFile.Free;
      end;
    finally
      RenderBmp.Free;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TStCustom2DBarcode.GenerateBarcodeBitmap (BCWidth  : Integer;
                                                    BCHeight : Integer);
var
  TextHeight    : Integer;
  TextWidth     : Integer;
  XPos          : Integer;
  YPos          : Integer;
  TopOffset     : Integer;
  BottomOffset  : Integer;
  BarCodeHeight : Integer;
  BarCodeWidth  : Integer;
  RWidthOffset  : Integer;
  LWidthOffset  : Integer;
  PaintHeight   : Integer;

begin
  XPos := 0;
  YPos := 0;

  { Initialize the canvas }
  FBitmap.Width := BCWidth;
  FBitmap.Height := BCHeight;
  FBitmap.Canvas.Pen.Color := Color;
  FBitmap.Canvas.Brush.Color := BackgroundColor;
  FBitmap.Canvas.FillRect (Rect (0, 0, BCWidth, BCHeight));
  FBitmap.Canvas.Brush.Color := Color;

  { Calculate the size of the caption }
  FBitmap.Canvas.Font.Assign (Font);
  TextHeight := FBitmap.Canvas.TextHeight ('Yg0');
  TextWidth := FBitmap.Canvas.TextWidth (Caption);

  { determine x position of the caption }
  case FAlignment of
    taLeftJustify :
      XPos := 0;
    taRightJustify :
      if BCWidth - TextWidth > 0 then
        XPos := BCWidth - TextWidth
      else
        XPos := 0;
    taCenter :
      if BCWidth - TextWidth > 0 then
        XPos := BCWidth div 2 - TextWidth div 2
      else
        XPos := 0;
  end;

  { determine the y position of the caption.  In addition, determine offsets
    for the barcode painting. }
  TopOffset := 0;
  BottomOffset := 0;
  case FCaptionLayout of
    tlBottom :
      begin
        if BCHeight - 2 - TextHeight > 0 then
          YPos := BCHeight - 2 - TextHeight
        else
          YPos := 0;
        if Caption <> '' then
          BottomOffset := TextHeight + 4;
      end;
    tlTop :
      begin
        YPos := 0;
        if Caption <> '' then
          TopOffset := TextHeight + 4;
      end;
    tlCenter :
      if BCHeight - TextHeight > 0 then
        YPos := BCHeight div 2 - TextHeight div 2
      else
        YPos := 0;
  end;

  { determine the size of the barcode and calculate the rectangle the
    barcode should be painted in.  Take into account the size of the
    caption (and it's existance), and the quiet zone.}
  PaintHeight := BCHeight - QuietZone * 2 - BottomOffset - TopOffset;
  BarCodeHeight := CalculateBarCodeHeight (PaintHeight);
  BarCodeWidth := CalculateBarCodeWidth (BCWidth);
  if BarCodeHeight < PaintHeight then begin
    Inc (BottomOffset, (PaintHeight - BarCodeHeight) div 2);
    Inc (TopOffset, (PaintHeight - BarCodeHeight) div 2);
  end;

  { Position the barcode horizontally }
  LWidthOffset := QuietZone;
  RWidthOffset := QuietZone;
  if BarCodeWidth < BCWidth - QuietZone * 2 then
    case Alignment of
      taLeftJustify :
        begin
          LWidthOffset := QuietZone;
          RWidthOffset := BCWidth - BarCodeWidth - QuietZone;
        end;
      taRightJustify :
        begin
          RWidthOffset := QuietZone;
          LWidthOffset := BCWidth - BarCodeWidth - QuietZone;
        end;
      taCenter :
        begin
          LWidthOffset := (BCWidth - BarCodeWidth) div 2;
          RWidthOffset := (BCWidth - BarCodeWidth) div 2;
        end;
    end;

  { Save the barcode rectangle }
  FBarCodeRect := Rect (LWidthOffset,
                        QuietZone + TopOffset,
                        BCWidth - RWidthOffset,
                        BCHeight - QuietZone - BottomOffset);
  { Draw the barcode }
  DrawBarcode;

  FBitmap.Canvas.Brush.Color := BackgroundColor;
  { Draw the caption }
  FBitmap.Canvas.TextOut (XPos, YPos, Caption);
end;

function TStCustom2DBarcode.GetBarCodeHeight : Integer;
begin
  Result := CalculateBarCodeHeight (Height);
end;

function TStCustom2DBarcode.GetBarCodeWidth : Integer;
begin
  Result := CalculateBarCodeWidth (Width);
end;

procedure TStCustom2DBarcode.GetCurrentResolution (var ResX : Integer;
                                                   var ResY : Integer);
begin
  ResX := GetDeviceCaps (FBitmap.Canvas.Handle, LOGPIXELSX);
  ResY := GetDeviceCaps (FBitmap.Canvas.Handle, LOGPIXELSY);
end;

function TStCustom2DBarcode.GetVersion : string;
begin
  Result := StVersionStr;
end;

procedure TStCustom2DBarcode.Paint;
begin
  GenerateBarcodeBitmap (Width, Height);
  Canvas.CopyRect (Rect (0, 0, Width, Height),
                   FBitmap.Canvas,
                   Rect (0, 0, Width, Height));
end;

procedure TStCustom2DBarcode.PaintToCanvas (ACanvas  : TCanvas;
                                            Position : TPoint);
begin
  PaintToDC (ACanvas.Handle, Position);
end;

procedure TStCustom2DBarcode.PaintToCanvasRes (ACanvas  : TCanvas;
                                               Position : TPoint;
                                               ResX     : Integer;
                                               ResY     : Integer);
begin
  PaintToDCRes (ACanvas.Handle, Position, ResX, ResY);
end;

procedure TStCustom2DBarcode.PaintToCanvasSize (ACanvas : TCanvas;
                                                X, Y, H : Double);
var
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;

begin
  {get some information about this device context}
  PixelsPerInchX := GetDeviceCaps (ACanvas.Handle, LOGPIXELSX);
  PixelsPerInchY := GetDeviceCaps (ACanvas.Handle, LOGPIXELSY);

  PaintToCanvasRes (ACanvas,
                    Point (Round (PixelsPerInchX * X),
                           Round (PixelsPerInchY * Y)),
                    Round (PixelsPerInchX * H),
                    Round (PixelsPerInchY * H));
end;

procedure TStCustom2DBarcode.PaintToDC (DC : hDC; Position : TPoint);
var
  NewResX : Integer;
  NewResY : Integer;
  
begin
  NewResX := GetDeviceCaps (DC, LOGPIXELSX);
  NewResY := GetDeviceCaps (DC, LOGPIXELSY);
  PaintToDCRes (DC, Position, NewResX, NewResY);
end;

procedure TStCustom2DBarcode.PaintToDCRes (DC : hDC; Position : TPoint;
                                           ResX : Integer; ResY : Integer);
var
  ACanvas   : TCanvas;
  R1        : TRect;
  R2        : TRect;
  RenderBmp : TBitmap;
  SizeX     : Integer;
  SizeY     : Integer;

begin
  ACanvas := TCanvas.Create;
  ACanvas.Handle := DC;
  try
    RenderBmp := TBitmap.Create;
    try
      {this is necessary because of a Delphi buglet}
      RenderBmp.Canvas.Font.PixelsPerInch := ResY;
      {use our font}
      RenderBmp.Canvas.Font := Font;

      RenderToResolution (RenderBmp, ResX, ResY, SizeX, SizeY);
      R1 := Rect (0, 0, RenderBmp.Width, RenderBmp.Height);
      R2 := Rect (Position.X, Position.Y,
                  RenderBmp.Width + Position.X,
                  RenderBmp.Height + Position.Y);

      ACanvas.CopyRect (R2, RenderBmp.Canvas, R1);
    finally
      RenderBmp.Free;
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TStCustom2DBarcode.PaintToPrinterCanvas (ACanvas  : TCanvas;
                                                   Position : TPoint);
begin
  PaintToPrinterDC (ACanvas.Handle, Position);
end;

procedure TStCustom2DBarcode.PaintToPrinterCanvasRes (ACanvas  : TCanvas;
                                                      Position : TPoint;
                                                      ResX     : Integer;
                                                      ResY     : Integer);
begin
  PaintToPrinterDCRes (ACanvas.Handle, Position, ResX, ResY);
end;

procedure TStCustom2DBarcode.PaintToPrinterCanvasSize (ACanvas : TCanvas;
                                                       X, Y, H : Double);
var
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;

begin
  {get some information about this device context}
  PixelsPerInchX := GetDeviceCaps (ACanvas.Handle, LOGPIXELSX);
  PixelsPerInchY := GetDeviceCaps (ACanvas.Handle, LOGPIXELSY);

  PaintToPrinterCanvasRes (ACanvas,
                           Point (Round (PixelsPerInchX * X),
                                  Round (PixelsPerInchY * Y)),
                           Round (PixelsPerInchX * H),
                           Round (PixelsPerInchY * H));
end;

procedure TStCustom2DBarcode.PaintToPrinterDC (DC : hDC; Position : TPoint);
var
  NewResX : Integer;
  NewResY : Integer;

begin
  NewResX := GetDeviceCaps (DC, LOGPIXELSX);
  NewResY := GetDeviceCaps (DC, LOGPIXELSY);
  PaintToPrinterDCRes (DC, Position, NewResX, NewResY);
end;

procedure TStCustom2DBarcode.PaintToPrinterDCRes (DC       : hDC;
                                                  Position : TPoint;
                                                  ResX     : Integer;
                                                  ResY     : Integer);
var
  ACanvas   : TCanvas;
  R2        : TRect;
  Info      : PBitMapInfo;
  InfoSize  : DWORD;
  ImageSize : DWORD;
  Image     : Pointer;
  RenderBmp : TBitmap;
  SizeX     : Integer;
  SizeY     : Integer;

begin
  ACanvas := TCanvas.Create;
  ACanvas.Handle := DC;
  try
    RenderBmp := TBitmap.Create;
    try
      {this is necessary because of a Delphi buglet}
      RenderBmp.Canvas.Font.PixelsPerInch := ResY;
      {use our font}
      RenderBmp.Canvas.Font.Assign (Font);

      RenderToResolution (RenderBmp, ResX, ResY, SizeX, SizeY);
      R2 := Rect (Position.X, Position.Y,
                  SizeX + Position.X,
                  SizeY + Position.Y);

      {Delphi does not allow a simple Canvas.CopyRect to the printer Canvas}
      with RenderBmp do begin
        GetDIBSizes (Handle, InfoSize, ImageSize);
        GetMem (Info, InfoSize);
        try
          GetMem (Image, ImageSize);
          try
            GetDIB (Handle, Palette, Info^, Image^);
            with Info^.bmiHeader do begin
              StretchDIBits (ACanvas.Handle,
                             R2.Left, R2.Top, SizeX, SizeY,
                             0, 0, biWidth, biHeight,
                             Image, Info^, DIB_RGB_COLORS, SRCCOPY);
            end;
          finally
             FreeMem (Image, ImageSize)
          end;
        finally
          FreeMem (Info, InfoSize);
        end;
      end;
    finally
      RenderBmp.Free;
    end;
  finally
    ACanvas.Free;
  end;
end;
                                                  
procedure TStCustom2DBarcode.SaveToFile (const FileName : string);
begin
  GenerateBarcodeBitmap (Width, Height);
  FBitmap.SaveToFile (FileName);
end;

procedure TStCustom2DBarcode.SaveToFileRes (const FileName : string;
                                            ResX : Integer; ResY : Integer);
var
  RenderBmp : TBitmap;
  SizeX     : Integer;
  SizeY     : Integer;

begin
  RenderBmp := TBitmap.Create;
  try
    RenderToResolution (RenderBmp, ResX, ResY, SizeX, SizeY);
    RenderBmp.SaveToFile (FileName);
  finally
    RenderBmp.Free;
  end;
end;                         

procedure TStCustom2DBarcode.SetAlignment (const v : TAlignment);
var
  OldAlignment : TAlignment;

begin
  if v <> FAlignment then begin
    OldAlignment := FAlignment;
    try
      FAlignment := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FAlignment := OldAlignment;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetBackgroundColor (const v : TColor);
var
  OldBackgroundColor : TColor;

begin
  if v <> FBackgroundColor then begin
    OldBackgroundColor := FBackgroundColor;
    try
      FBackgroundColor := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FBackgroundColor := OldBackgroundColor;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetBarHeight (const v : Integer);
var
  OldBarHeight : Integer;

begin
  if v <> FBarHeight then begin
    OldBarHeight := FBarHeight;
    try
      FBarHeight := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FBarHeight := OldBarHeight;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetBarHeightToWidth (const v : Integer);
var
  OldBarHeightToWidth : Integer;

begin
  if v <> FBarHeightToWidth then begin
    if v < 0 then
      raise E2DBarcodeError.Create (StEBadBarHeightToWidth);
    OldBarHeightToWidth := FBarHeightToWidth;
    try
      FBarHeightToWidth := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FBarHeightToWidth := OldBarHeightToWidth;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetBarWidth (const v : Integer);
var
  OldBarWidth : Integer;

begin
  if v <> FBarWidth then begin
    OldBarWidth := FBarWidth;
    try
      FBarWidth := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FBarWidth := OldBarWidth;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetBitmap (const v : TBitmap);
begin
  FBitmap.Assign (v);
  Invalidate;
end;

procedure TStCustom2DBarcode.SetCaption (const v : string);
var
  OldCaption : string;

begin
  if v <> FCaption then begin
    OldCaption := FCaption;
    try
      FCaption := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FCaption := OldCaption;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetCaptionLayout (const v : TTextLayout);
var
  OldCaptionLayout : TTextLayout;

begin
  if v <> FCaptionLayout then begin
    OldCaptionLayout := FCaptionLayout;
    try
      FCaptionLayout := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FCaptionLayout := OldCaptionLayout;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetCode (const v : string);
var
  OldCode : string;

begin
  if v <> FCode then begin
    OldCode := FCode;
    try
      FCode := v;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FCode := OldCode;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetECCLevel (const v : Integer);
var
  OldECCLevel : Integer;

begin
  if v <> FECCLevel then begin
    OldECCLevel := FECCLevel;
    try
      FECCLevel := v;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FECCLevel := OldECCLevel;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetExtendedSyntax (const v : Boolean);
var
  OldExtendedSyntax : Boolean;

begin
  if v <> FExtendedSyntax then begin
    OldExtendedSyntax := FExtendedSyntax;
    try
      FExtendedSyntax := v;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FExtendedSyntax := OldExtendedSyntax;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetQuietZone (const v : Integer);
var
  OldQuietZone : Integer;

begin
  if v <> FQuietZone then begin
    if (v < 0) then
      raise E2DBarcodeError.Create (StEBadQuietZone);
    OldQuietZone := FQuietZone;
    try
      FQuietZone := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FQuietZone := OldQuietZone;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetRelativeBarHeight (const v : Boolean);
var
  OldRelativeBarHeight : Boolean;

begin
  if v <> FRelativeBarHeight then begin
    OldRelativeBarHeight := FRelativeBarHeight;
    try
      FRelativeBarHeight := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FRelativeBarHeight := OldRelativeBarHeight;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStCustom2DBarcode.SetVersion(const Value : string);
begin
end;

{ TStPDF417Barcode }

constructor TStPDF417Barcode.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  FNumCodewords     := 1;
  FTruncated        := False;
  FHighlight        := False;
  FECCLevel         := -1;
  FNumRows          := 0;
  FNumColumns       := 0;
  FTotalCodewords   := FNumRows * FNumColumns;
  FUsedCodewords    := 0;
  FUsedECCCodewords := 0;
  FFreeCodewords    := FTotalCodewords;
  Width             := 273;
  Height            := 81;

  GenerateCodewords;
  GenerateBarcodeBitmap (Width, Height);
end;

procedure TStPDF417Barcode.AddCodeword (Value : Word);
begin
  FCodewords[FNumCodewords] := Value;
  Inc (FNumCodewords);
end;

function TStPDF417Barcode.CalculateBarCodeWidth (
                                        PaintableWidth : Integer) : Integer;
var
  XSize : Integer;
  YSize : Integer;

begin
  CalculateSize (XSize, YSize);
  if Truncated then
    Result := (XSize + 2) * 17 * BarWidth + BarWidth
  else
    Result := (XSize + 4) * 17 * BarWidth + BarWidth;
end;

function TStPDF417Barcode.CalculateBarCodeHeight (
                                        PaintableHeight : Integer) : Integer;
var
  XSize : Integer;
  YSize : Integer;
  
begin
  CalculateSize (XSize, YSize);
  if RelativeBarHeight then
    Result := PaintableHeight
  else if BarHeightToWidth <> 0 then
    Result := (BarHeightToWidth * BarWidth) * YSize
  else
    Result := BarHeight * YSize;
end;

procedure TStPDF417Barcode.CalculateECC (NumCodewords : Integer;
                                         ECCLen       : Integer);

const
  StMods  : array [0..64] of array [0..64] of Integer = 
    ((  0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (917,  27,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (890, 351, 200,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (809, 723, 568, 522,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (566, 155, 460, 919, 427,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (766,  17, 803,  19, 285, 861,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (437, 691, 784, 597, 537, 925,  76,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (379, 428, 653, 646, 284, 436, 308, 237, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (205, 441, 501, 362, 289, 257, 622, 527, 
      567,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (612, 266, 691, 818, 841, 826, 244,  64, 
      457, 377,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (904, 602, 327,  68,  15, 213, 825, 708, 
      565,  45, 462,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (851,  69,   7, 388, 127, 347, 684, 646, 
      201, 757, 864, 597,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (692, 394, 184, 204, 678, 592, 322, 583, 
      606, 384, 342, 713, 764,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (215, 105, 833, 691, 915, 478, 354, 274, 
      286, 241, 187, 154, 677, 669,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (642, 868, 147, 575, 550,  74,  80,   5, 
      230, 664, 904, 109, 476, 829, 460,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     ( 65, 176,  42, 295, 428, 442, 116, 295, 
      132, 801, 524, 599, 755, 232, 562, 274, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (192,  70,  98,  55, 733, 916, 510, 163, 
      437, 843,  61, 259, 650, 430, 298, 115, 
      425,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (573, 760, 756, 233, 321, 560, 202, 312, 
      297, 120, 739, 275, 855,  37, 624, 315, 
      577, 279,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (787, 754, 821, 371,  17, 508, 201, 806, 
      177, 506, 407, 491, 249, 923, 181,  75, 
      170, 200, 250,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (500, 632, 880, 710, 375, 274, 258, 717, 
      176, 802, 109, 736, 540,  64,  45, 152, 
       12, 647, 448, 712,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (568, 259, 193, 165, 347, 691, 310, 610, 
      624, 693, 763, 716, 422, 553, 681, 425, 
      129, 534, 781, 519, 108,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (772,   6,  76, 519, 563, 875,  66, 678,
      578, 716, 927, 296, 633, 244, 155, 928, 
      432, 838,  95,  55,  78, 665,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (455, 538,  32, 581, 473, 772, 462, 194, 
      251, 503, 631,   1, 630, 247, 843, 101, 
      749, 457, 143, 597, 294,  93,  78,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (433, 747, 273, 806, 697, 585, 200, 249, 
      628, 555, 713,  54, 608, 322,  54, 135, 
      385, 701, 308, 238, 166, 128, 819, 142, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (367,  39, 208, 439, 454, 104, 608,  55, 
      916, 912, 314, 375, 760, 141, 169, 287, 
      765, 374, 492, 348, 251, 320, 732, 899, 
      847,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (169, 764, 847, 131, 858, 325, 454, 441, 
      245, 699, 893, 446, 830, 159, 121, 269, 
      608, 331, 760, 477,  93, 788, 544, 887, 
      284, 443,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (504, 710, 383, 531, 151, 694, 636, 175, 
      269,  93,  21, 463, 671, 438, 433, 857, 
      610, 560, 165, 531, 100, 357, 688, 114,
      149, 825, 694,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (580, 925, 461, 840, 560,  93, 427, 203, 
      563,  99, 586, 201, 557, 339, 277, 321, 
      712, 470, 920,  65, 509, 525, 879, 378, 
      452,  72, 222, 720,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (808, 318, 478,  42, 706, 500, 264,  14, 
      397, 261, 862,  33, 864,  62, 462, 305, 
      509, 231, 316, 800, 465, 452, 738, 126, 
      239,   9, 845, 241, 656,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (563, 235, 604, 915, 635, 324, 392, 364, 
      683, 541,  89, 655, 211, 194, 136, 453, 
      104,  12, 390, 487, 484, 794, 549, 471, 
       26, 910, 498, 383, 138, 926,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (757, 764, 673, 108, 706, 886,  76, 234, 
      695, 196,  66, 270,   8, 252, 612, 825, 
      660, 679, 860, 898, 204, 861, 371, 142, 
      358, 380, 528, 379, 120, 757, 347,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (410,  63, 330, 685, 390, 231, 133, 803, 
      320, 571, 800, 593, 147, 263, 494, 273, 
      517, 193, 284, 687, 742, 677, 742, 536, 
      321, 640, 586, 176, 525, 922, 575, 361, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (575, 871, 311, 454, 504, 870, 199, 768, 
      634, 362, 548, 855, 529, 384, 830, 923, 
      222,  85, 841,  59, 518, 590, 358, 110, 
      695, 864, 699, 581, 642, 175, 836, 855, 
      709, 274, 686, 244,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  5,  10, 156, 729, 684, 324,  60, 264, 
       99, 261,  89, 460, 742, 208, 699, 670, 
      512, 404, 726, 389, 492, 287, 894, 571, 
       41, 203, 353, 256, 243, 784, 385, 555, 
      595, 734, 714, 565, 205, 706, 316, 115, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (285,  82, 730, 339, 436, 572, 271, 103, 
      758, 231, 560,  31, 213, 272, 267, 569, 
      773,   3,  21, 446, 706, 413,  97, 376, 
       60, 714, 436, 417, 405, 632,  25, 109, 
      876, 470, 915, 157, 840, 764,  64, 678, 
      848, 659,  36, 476,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (669, 912, 896, 252, 338, 162, 414, 632, 
      626, 252, 869, 185, 444,  82, 920, 783, 
      565, 875, 126, 877, 524, 603, 189, 136, 
      373, 540, 649, 271, 836, 540, 199, 323, 
      888, 486,  92, 849, 162, 701, 178, 926, 
      498, 575, 765, 422, 450, 302, 354, 710, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (187,  57,  15, 317, 835, 593,   8, 158, 
       95, 145,  37, 659, 576, 386, 884, 913, 
      495, 869, 908, 296, 437, 215,  33, 883, 
      877, 477, 712, 578, 349,  13, 174, 839, 
      914, 107, 260,  40, 532, 210, 395, 905, 
      163, 785, 693, 627, 393, 687, 112, 481, 
      717, 297,  37, 483,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (163, 726, 626, 653, 414, 537, 467, 579, 
      729, 396, 142, 598, 860, 774, 518, 461, 
      136, 687, 827, 614, 841, 468, 207, 481, 
      649, 910, 497, 686, 186, 235, 845, 863, 
      821, 711, 663, 534, 393, 756, 467, 224, 
      442, 520, 210, 732, 864, 729, 433, 735, 
       70, 184, 278,  97, 492,  17,   2, 338, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0,
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     ( 77, 611, 467, 704, 555, 579, 802, 773, 
      303, 518, 560, 196, 314, 102,   5, 845, 
      248, 125, 836, 923,  88, 630, 886, 619, 
       37, 141, 409, 229,  77, 658, 450, 449, 
       93, 651, 276, 501, 166,  75, 630, 701, 
      388,  72, 830, 166, 187, 131, 711, 577, 
      834, 147, 361, 517,  76, 581,  45, 495, 
      366, 278, 781,  61,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (  0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0, 
        0,   0,   0,   0,   0,   0,   0,   0,   0),
     (543, 264, 623, 843, 381,   4, 629, 840, 
      771, 280,  97, 404,  83, 717, 733, 648, 
      502, 488, 201, 651, 158, 605, 352, 517, 
      535, 225, 594, 460,  31, 519,  35, 440, 
      184, 283, 762, 672, 400, 511, 376, 543, 
      822, 858, 609, 430, 172, 462, 476, 723, 
      612, 381, 877, 733, 505, 107, 287, 610, 
      106, 453, 771, 862,  93,   6, 422, 539,   0));

  StMods128 : array [0..127] of Integer =
    (521, 310, 864, 547, 858, 580, 296, 379,
      53, 779, 897, 444, 400, 925, 749, 415,
     822,  93, 217, 208, 928, 244, 583, 620,
     246, 148, 447, 631, 292, 908, 490, 704,
     516, 258, 457, 907, 594, 723, 674, 292,
     272,  96, 684, 432, 686, 606, 860, 569,
     193, 219, 129, 186, 236, 287, 192, 775,
     278, 173,  40, 379, 712, 463, 646, 776,
     171, 491, 297, 763, 156, 732,  95, 270,
     447,  90, 507,  48, 228, 821, 808, 898,
     784, 663, 627, 378, 382, 262, 380, 602,
     754, 336,  89, 614,  87, 432, 670, 616,
     157, 374, 242, 726, 600, 269, 375, 898,
     845, 454, 354, 130, 814, 587, 804,  34,
     211, 330, 539, 297, 827, 865,  37, 517,
     834, 315, 550,  86, 801,   4, 108, 539);

  StMods256 : array [0..255] of Integer =
    (524, 894,  75, 766, 882, 857,  74, 204,
      82, 586, 708, 250, 905, 786, 138, 720, 
     858, 194, 311, 913, 275, 190, 375, 850, 
     438, 733, 194, 280, 201, 280, 828, 757, 
     710, 814, 919,  89,  68, 569,  11, 204, 
     796, 605, 540, 913, 801, 700, 799, 137, 
     439, 418, 592, 668, 353, 859, 370, 694, 
     325, 240, 216, 257, 284, 549, 209, 884, 
     315,  70, 329, 793, 490, 274, 877, 162, 
     749, 812, 684, 461, 334, 376, 849, 521, 
     307, 291, 803, 712,  19, 358, 399, 908, 
     103, 511,  51,   8, 517, 225, 289, 470, 
     637, 731,  66, 255, 917, 269, 463, 830, 
     730, 433, 848, 585, 136, 538, 906,  90, 
       2, 290, 743, 199, 655, 903, 329,  49, 
     802, 580, 355, 588, 188, 462,  10, 134, 
     628, 320, 479, 130, 739,  71, 263, 318, 
     374, 601, 192, 605, 142, 673, 687, 234, 
     722, 384, 177, 752, 607, 640, 455, 193, 
     689, 707, 805, 641,  48,  60, 732, 621, 
     895, 544, 261, 852, 655, 309, 697, 755, 
     756,  60, 231, 773, 434, 421, 726, 528, 
     503, 118,  49, 795,  32, 144, 500, 238,
     836, 394, 280, 566, 319,   9, 647, 550, 
      73, 914, 342, 126,  32, 681, 331, 792, 
     620,  60, 609, 441, 180, 791, 893, 754, 
     605, 383, 228, 749, 760, 213,  54, 297, 
     134,  54, 834, 299, 922, 191, 910, 532, 
     609, 829, 189,  20, 167,  29, 872, 449, 
      83, 402,  41, 656, 505, 579, 481, 173, 
     404, 251, 688,  95, 497, 555, 642, 543, 
     307, 159, 924, 558, 648,  55, 497,  10);

  StMods512 : array [0..511] of Integer =
    (352,  77, 373, 504,  35, 599, 428, 207,
     409, 574, 118, 498, 285, 380, 350, 492,
     197, 265, 920, 155, 914, 299, 229, 643,
     294, 871, 306,  88,  87, 193, 352, 781,
     846,  75, 327, 520, 435, 543, 203, 666,
     249, 346, 781, 621, 640, 268, 794, 534,
     539, 781, 408, 390, 644, 102, 476, 499,
     290, 632, 545,  37, 858, 916, 552,  41,
     542, 289, 122, 272, 383, 800, 485,  98,
     752, 472, 761, 107, 784, 860, 658, 741,
     290, 204, 681, 407, 855,  85,  99,  62,
     482, 180,  20, 297, 451, 593, 913, 142,
     808, 684, 287, 536, 561,  76, 653, 899,
     729, 567, 744, 390, 513, 192, 516, 258,
     240, 518, 794, 395, 768, 848,  51, 610,
     384, 168, 190, 826, 328, 596, 786, 303,
     570, 381, 415, 641, 156, 237, 151, 429,
     531, 207, 676, 710,  89, 168, 304, 402,
      40, 708, 575, 162, 864, 229,  65, 861,
     841, 512, 164, 477, 221,  92, 358, 785,
     288, 357, 850, 836, 827, 736, 707,  94,
       8, 494, 114, 521,   2, 499, 851, 543, 
     152, 729, 771,  95, 248, 361, 578, 323, 
     856, 797, 289,  51, 684, 466, 533, 820, 
     669,  45, 902, 452, 167, 342, 244, 173, 
      35, 463, 651,  51, 699, 591, 452, 578, 
      37, 124, 298, 332, 552,  43, 427, 119, 
     662, 777, 475, 850, 764, 364, 578, 911, 
     283, 711, 472, 420, 245, 288, 594, 394, 
     511, 327, 589, 777, 699, 688,  43, 408, 
     842, 383, 721, 521, 560, 644, 714, 559, 
      62, 145, 873, 663, 713, 159, 672, 729,
     624,  59, 193, 417, 158, 209, 563, 564, 
     343, 693, 109, 608, 563, 365, 181, 772, 
     677, 310, 248, 353, 708, 410, 579, 870, 
     617, 841, 632, 860, 289, 536,  35, 777, 
     618, 586, 424, 833,  77, 597, 346, 269, 
     757, 632, 695, 751, 331, 247, 184,  45, 
     787, 680,  18,  66, 407, 369,  54, 492, 
     228, 613, 830, 922, 437, 519, 644, 905, 
     789, 420, 305, 441, 207, 300, 892, 827, 
     141, 537, 381, 662, 513,  56, 252, 341, 
     242, 797, 838, 837, 720, 224, 307, 631, 
      61,  87, 560, 310, 756, 665, 397, 808, 
     851, 309, 473, 795, 378,  31, 647, 915, 
     459, 806, 590, 731, 425, 216, 548, 249, 
     321, 881, 699, 535, 673, 782, 210, 815, 
     905, 303, 843, 922, 281,  73, 469, 791, 
     660, 162, 498, 308, 155, 422, 907, 817, 
     187,  62,  16, 425, 535, 336, 286, 437, 
     375, 273, 610, 296, 183, 923, 116, 667, 
     751, 353,  62, 366, 691, 379, 687, 842, 
      37, 357, 720, 742, 330,   5,  39, 923, 
     311, 424, 242, 749, 321,  54, 669, 316, 
     342, 299, 534, 105, 667, 488, 640, 672, 
     576, 540, 316, 486, 721, 610,  46, 656, 
     447, 171, 616, 464, 190, 531, 297, 321,
     762, 752, 533, 175, 134,  14, 381, 433,
     717,  45, 111,  20, 596, 284, 736, 138,
     646, 411, 877, 669, 141, 919,  45, 780,
     407, 164, 332, 899, 165, 726, 600, 325,
     498, 655, 357, 752, 768, 223, 849, 647,
      63, 310, 863, 251, 366, 304, 282, 738,
     675, 410, 389, 244,  31, 121, 303, 263);

var
  BaseReg  : array [0..800] of DWord;
  CoeffReg : array [0..800] of DWord;
  i        : Integer;
  j        : Integer;
  TInt     : Integer;
  Temp     : DWord;
  Wrap     : DWord;

begin
  if ECClen < 128 then
    for i := 0 to ECCLen - 1 do
      CoeffReg[i] := StMods[ECClen][i]
  else begin
    if ECClen = 128 then
      for i := 0 to ECCLen - 1 do
        CoeffReg[i] := StMods128[i]
    else if ECClen = 256 then
      for i := 0 to ECCLen - 1 do
        CoeffReg[i] := StMods256[i]
    else if ECClen = 512 then
      for i := 0 to ECCLen - 1 do
        CoeffReg[i] := StMods512[i];
  end;

  for i := 0 to ECCLen - 1 do
    BaseReg[i] := 0;

  for i := NumCodewords to NumCodewords + ECCLen - 1 do
    FCodewords[i] := 0;

  for i := 0 to NumCodewords - 1 do begin
    wrap := (BaseReg[ECClen - 1] + FCodewords[i]) mod 929;
    for j := ECCLen - 1 downto 1 do begin
      temp := (CoeffReg[ECClen - 1 - j] * wrap) mod 929;
      temp := (929 - temp) mod 929;
      BaseReg[j] := (BaseReg[j - 1] + temp) mod 929;
    end;
    temp := (CoeffReg[ECClen - 1] * wrap) mod 929;      
    temp := (929 - temp) mod 929;
    BaseReg[0]:= temp;
  end;

  for j := 0 to ECCLen - 1 do
    BaseReg[j] := (929 - BaseReg[j]) mod 929;

  for j := 0 to ECCLen - 1 do begin
    tint := BaseReg[ECClen - 1 - j];
    FCodewords [NumCodewords + j] := tint;
  end;
end;

procedure TStPDF417Barcode.CalculateSize (var XSize : Integer;
                                          var YSize : Integer);
var
  i                 : Integer;
  NumErrorCodewords : Integer;
  ErrorLevel        : Integer;
  j                 : Integer;
                                            
begin
  { Set the error correction level automatically if needed }
  ErrorLevel := GetRealErrorLevel;

  NumErrorCodewords := Trunc (Power (2, ErrorLevel + 1));

  XSize := NumColumns;
  YSize := NumRows;

  FTotalCodewords := XSize * YSize;

  { Adjust the size if necessary }
  if (NumRows <= 0) or (NumColumns <= 0) then begin
    if NumRows > 0 then begin
      i := 1;
      while i <= 30 do begin
        if i * NumRows - NumErrorCodewords > FNumCodewords then
          Break;
        Inc (i);
      end;
      FTotalCodewords := YSize * 30;
      XSize := i;
    end else if NumColumns > 0 then begin
      i := 3;
      while i <= 90 do begin
        if i * NumColumns - NumErrorCodewords > FNumCodewords then
          Break;
        Inc (i);
      end;
      YSize := i;
      FTotalCodewords := XSize * 90;
    end else begin
      i := 1;
      j := 3;
      while (i * j - NumErrorCodewords < FNumCodewords) do begin
        if j < 90 then
          Inc (j);
        if (i < 30) and (i * j - NumErrorCodewords < FNumCodewords) then
          Inc (i);
        if (j >= 90) and (i >= 30) then
          Break;
      end;
      XSize := i;
      YSize := J;
      FTotalCodewords := 900;
    end;
  end;
end;

function TStPDF417Barcode.CodewordToBitmask (RowNumber : Integer;
                                             Codeword  : Integer) : DWord;
begin
  if (Codeword < 0) or (CodeWord > 929) then
    raise E2DBarcodeError.Create (StEInvalidCodeword);
  Result := StPDF417Codewords[RowNumber mod 3][Codeword];
end;

procedure TStPDF417Barcode.ConvertBytesToBase900 (const S : array of Byte;
                                                  var A   : array of Integer);
var
  i        : Integer;
  D        : array [0..5] of Byte;
  Dividend : Integer;
  Digits   : array [0..4] of Integer;
  SP       : Integer;

begin
//  Assert(length(S) = 6,
//    'ConvertBytesToBase900: there should be 6 bytes in the input byte array');
//  Assert(length(A) = 5,
//    'ConvertBytesToBase900: there should be 5 elements in the output digit array');

  {copy the array of bytes}
  for i := 0 to 5 do
    D[i] := S[i];

  {loop until the entire base 256 value has been converted to an array
   of base 900 digits (6 base 256 digits will convert to 5 base 900
   digits)}
  SP := 0;
  while (SP < 5) do begin
    Dividend := 0;
    for i := 0 to 5 do begin
     {notes: at the start of the loop, Dividend will always be in the
               range 0..899--it starts out as zero and the final
               statement in the loop forces it into that range
             the first calculation sets Dividend to 0..230399
             the second calc sets D[i] to 0..255 (with no possibility
               of overflow)
             the third calc sets Dividend to 0..899 again}
      Dividend := (Dividend shl 8) + D[i];
      D[i] := Dividend div 900;
      Dividend := Dividend mod 900;
    end;

    Digits[SP] := Dividend;
    inc(SP);
  end;

  {pop the base 900 digits and enter them into the array of integers}
  i := 0;
  while (SP > 0) do begin
    dec(SP);
    A[i] := Digits[SP];
    inc(i);
  end;
end;

procedure TStPDF417Barcode.ConvertToBase900 (const S  : string;
                                             var A    : array of Integer;
                                             var LenA : Integer);
var
  D          : string;
  i          : Integer;
  LenD       : Integer;
  Dividend   : Integer;
  Rem        : Integer;
  Done       : Boolean;
  FirstDigit : Integer;
  Digits     : array [0..14] of Integer;
                             // 15 base 900 digits = 45 base 10 digits
  SP         : Integer;
  
begin
  {Assert: S must be non-empty
           it must contain just the ASCII characters '0' to '9' (so no
             leading/trailing spaces either)
           it must have a maximum length of 45}
  Assert(IsNumericString(S), 'ConvertToBase900: S should be a numeric string');

  {grab the string and calculate its length}
  D := S;
  LenD := length(D);

  {convert the string from ASCII characters into binary digits and in
   the process calculate the first non-zero digit}
  FirstDigit := 0;
  for i := LenD downto 1 do begin
    D[i] := char(ord(D[i]) - ord('0'));
    if (D[i] <> #0) then
      FirstDigit := i;
  end;

  {if the input string comprises just zero digits, return}
  if (FirstDigit = 0) then begin
    LenA := 0;
    Exit;
  end;

  {prepare the stack of base 900 digits}
  SP := 0;

  {loop until the entire base 10 string has been converted to an array
   of base 900 digits}
  Done := false;
  while not Done do begin

    {if we can switch to using standard integer arithmetic, do so}
    if ((LenD - FirstDigit) <= 8) then begin

      {convert the remaining digits to a binary integer}
      Dividend := 0;
      for i := FirstDigit to LenD do
        Dividend := (Dividend * 10) + ord(D[i]);

      {calculate the remaining base 900 digits using the standard
       radix conversion algorithm; push onto the digit stack}
      while (Dividend <> 0) do begin
        Digits[SP] := Dividend mod 900;
        inc(SP);
        Dividend := Dividend div 900;
      end;

      {we've finished}
      Done := true;
    end

    {otherwise operate directly on the base 10 string}
    else begin

      {calculate the remainder base 100}
      Rem := ord(D[LenD]);
      dec(LenD);
      Rem := Rem + (ord(D[LenD]) * 10);
      dec(LenD);

      {calculate the quotient and remainder of the remaining digits,
       dividing by 9}
      Dividend := 0;
      for i := FirstDigit to LenD do begin
        Dividend := (Dividend * 10) + ord(D[i]);
        D[i] := char(Dividend div 9);
        Dividend := Dividend mod 9;
      end;

      {push the base 900 digit onto the stack: it's the remainder base
       9 multiplied by 100, plus the remainder base 100}
      Digits[SP] := (Dividend * 100) + Rem;
      inc(SP);

      {if the first digit is now zero, advance the index to the first
       non-zero digit}
      if (D[FirstDigit] = '0') then
        inc(FirstDigit);
    end;
  end;

  {pop the base 900 digits and enter them into the array of integers}
  i := 0;
  while (SP > 0) do begin
    dec(SP);
    A[i] := Digits[SP];
    inc(i);
  end;
  LenA := i;
end;

procedure TStPDF417Barcode.DrawBarcode;
var
  XSize             : Integer;
  YSize             : Integer;
  i                 : Integer;
  j                 : Integer;
  WorkBarHeight     : Integer;
  CodewordPos       : Integer;
  ErrorLevel        : Integer;
  NumErrorCodewords : Integer;

const
  SymbolPadding = 900;

begin
  { Set the error correction level automatically if needed }
  ErrorLevel := GetRealErrorLevel;

  NumErrorCodewords := Trunc (Power (2, ErrorLevel + 1));

  CalculateSize (XSize, YSize);

  { The first codewords is always the length }
  if FNumCodewords +
     (XSize * YSize - FNumCodewords - NumErrorCodewords) < 0 then
    raise E2DBarcodeError.Create (StECodeTooLarge);
  FCodewords[0] := FNumCodewords +
                  (XSize * YSize - FNumCodewords - NumErrorCodewords);

  CodewordPos := 1; { The first codeword is always the length }

  WorkBarHeight := (BarCodeRect.Bottom - BarCodeRect.Top) div YSize;

  for i := 0 to YSize - 1 do begin
    if FHighlight then
      FBitmap.Canvas.Brush.Color := $ffbbff;
    DrawStartPattern (i, WorkBarHeight);
    if FHighlight then
      FBitmap.Canvas.Brush.Color := $ffffbb;
    DrawLeftRowIndicator (i, WorkBarHeight, YSize, XSize);
    for j := 0 to XSize - 1 do begin
      if (i = 0) and (j = 0) then begin
        if FHighlight then
          FBitmap.Canvas.Brush.Color := $bbffff;
        { Length }
        DrawCodeWordBitmask (i, j + 2, WorkBarHeight,
                             CodeWordToBitmask (i, FNumCodewords +
                      (XSize * YSize - FNumCodewords - NumErrorCodewords)))
      end else if CodewordPos < FNumCodewords then begin
        if FHighlight then
          FBitmap.Canvas.Brush.Color := $bbbbff; 
        { Data }
        DrawCodeWordBitmask (i, j + 2, WorkBarHeight,
                             CodewordToBitmask (i, FCodewords[CodewordPos]));
        Inc (CodewordPos);
      end else if CodewordPos >= XSize * YSize - NumErrorCodeWords then begin
        if FHighlight then
          FBitmap.Canvas.Brush.Color := $ffbbbb; 
        { Error Correction Codes }
        DrawCodeWordBitmask (i, j + 2, WorkBarHeight,
                             CodewordToBitmask (i, FCodewords[CodewordPos]));
        Inc (CodewordPos);
      end else begin
        if FHighlight then
          FBitmap.Canvas.Brush.Color := $bbffbb; 
        { Padding }
        DrawCodewordBitmask (i, j + 2, WorkBarHeight,
                             CodewordToBitmask (i, SymbolPadding));
        Inc (CodewordPos);
      end;
    end;
    if FHighlight then
      FBitmap.Canvas.Brush.Color := $bbddff;
    if Truncated then
      DrawStopPattern (i, XSize + 2, WorkBarHeight)
    else begin
      DrawRightRowIndicator (i, XSize + 2, WorkBarHeight, YSize, XSize);
      if FHighlight then
        FBitmap.Canvas.Brush.Color := $ddaaff; 
      DrawStopPattern (i, XSize + 3, WorkBarHeight);
    end;
  end;
end;

procedure TStPDF417Barcode.DrawCodeword (RowNumber     : Integer;
                                         ColNumber     : Integer;
                                         WorkBarHeight : Integer;
                                         Pattern       : string);

  function GetColumnPosition (ColNumber : Integer) : Integer;
  begin
    Result := ColNumber * StPDF417CellWidth * BarWidth;
  end;

var
  i         : Integer;
  CurPos    : Integer;
  NewPos    : Integer;
  DrawBlock : Boolean;

begin
  if FHighlight then begin
    FBitmap.Canvas.FillRect (
        Rect (BarCodeRect.Left + (GetColumnPosition (ColNumber)),
              BarCodeRect.Top + RowNumber * WorkBarHeight,
              BarCodeRect.Left + 17 * BarWidth + GetColumnPosition (ColNumber),
              BarCodeRect.Top + (RowNumber + 1) * WorkBarHeight));
    FBitmap.Canvas.Brush.Color := Color;
  end;

  CurPos := 0;
  DrawBlock := True;
  for i := 1 to Length (Pattern) do begin
    NewPos := StrToInt (Copy (Pattern, i, 1)) * BarWidth;
    if DrawBlock then
      FBitmap.Canvas.Rectangle (
          BarCodeRect.Left + CurPos + GetColumnPosition (ColNumber),
          BarCodeRect.Top + RowNumber * WorkBarHeight,
          BarCodeRect.Left + CurPos + NewPos + GetColumnPosition (ColNumber),
          BarCodeRect.Top + (RowNumber + 1) * WorkBarHeight);
    CurPos := CurPos + NewPos;
    DrawBlock := not DrawBlock;
  end;
end;

procedure TStPDF417Barcode.DrawCodewordBitmask (RowNumber     : Integer;
                                                ColNumber     : Integer;
                                                WorkBarHeight : Integer;
                                                Bitmask       : DWord);

  function GetColumnPosition (ColNumber : Integer) : Integer;
  begin
    Result := ColNumber * StPDF417CellWidth * BarWidth;
  end;

var
  i : Integer;

begin
  if FHighlight then begin
    FBitmap.Canvas.FillRect (
        Rect (BarCodeRect.Left + (GetColumnPosition (ColNumber)),
              BarCodeRect.Top + RowNumber * WorkBarHeight,
              BarCodeRect.Left + 17 * BarWidth + GetColumnPosition (ColNumber),
              BarCodeRect.Top + (RowNumber + 1) * WorkBarHeight));
    FBitmap.Canvas.Brush.Color := Color;
  end;

  for i := 16 downto 0 do
    if ((BitMask shr i) and $00001) <> 0 then
      FBitmap.Canvas.Rectangle (
          BarCodeRect.Left + (16 - i) * BarWidth +
          GetColumnPosition (ColNumber),
          BarCodeRect.Top + RowNumber * WorkBarHeight,
          BarCodeRect.Left + (17 - i) * BarWidth +
          GetColumnPosition (ColNumber),
          BarCodeRect.Top + (RowNumber + 1) * WorkBarHeight);
end;

procedure TStPDF417Barcode.DrawLeftRowIndicator (RowNumber     : Integer;
                                                 WorkBarHeight : Integer;
                                                 NumRows       : Integer;
                                                 NumCols       : Integer);
var
  CodeWord   : Integer;
  ErrorLevel : Integer;
  
begin
  ErrorLevel := GetRealErrorLevel;
  CodeWord := 0;
  if RowNumber mod 3 = 0 then
    CodeWord := ((RowNumber div 3) * 30) + ((NumRows - 1) div 3)
  else if RowNumber mod 3 = 1 then
    CodeWord := ((RowNumber div 3) * 30) + ((NumRows - 1) mod 3) +
                 (3 * ErrorLevel)
  else if RowNumber mod 3 = 2 then
    CodeWord := (( RowNumber div 3) * 30) + (NumCols - 1);    
  DrawCodeWordBitmask (RowNumber, 1, WorkBarHeight,
                       CodewordToBitmask (RowNumber, Codeword));
end;

procedure TStPDF417Barcode.DrawRightRowIndicator (RowNumber     : Integer;
                                                  ColNumber     : Integer;
                                                  WorkBarHeight : Integer;
                                                  NumRows       : Integer;
                                                  NumCols       : Integer);
var
  Codeword   : Integer;
  ErrorLevel : Integer;
  
begin
  ErrorLevel := GetRealErrorLevel;
  CodeWord := 0;
  if RowNumber mod 3 = 0 then
    Codeword := ((RowNumber div 3) * 30) + (NumCols - 1)  
  else if RowNumber mod 3 = 1 then
    Codeword := ((RowNumber div 3) * 30) + ((NumRows - 1) div 3)
  else if RowNumber mod 3 = 2 then
    Codeword := ((RowNumber div 3) * 30) + ((NumRows - 1) mod 3) +
                (3 * ErrorLevel);
  DrawCodeWordBitmask (RowNumber, ColNumber, WorkBarHeight,
                       CodewordToBitmask (RowNumber, Codeword));
end;

procedure TStPDF417Barcode.DrawStartPattern (RowNumber     : Integer;
                                             WorkBarHeight : Integer);
begin
  DrawCodeword (RowNumber, 0, WorkBarHeight, '81111113');
end;

procedure TStPDF417Barcode.DrawStopPattern (RowNumber     : Integer;
                                            ColNumber     : Integer;
                                            WorkBarHeight : Integer);
begin
  if Truncated then
    DrawCodeWord (RowNumber, ColNumber, WorkBarHeight, '1')
  else
    DrawCodeWord (RowNumber, ColNumber, WorkBarHeight, '711311121');
end;

procedure TStPDF417Barcode.EncodeBinary (var Position : Integer;
                                         CodeLen      : Integer);

  function CountBytes (Position : Integer; CodeLen : Integer) : Integer;
  var
    Done  : Boolean;
    Dummy : Integer;
    
  begin
    Result := 0;
    Done := False;
    while not done do begin
      if (Result < CodeLen) and 
         (not GoodForNumericCompaction (Position + Result, CodeLen, Dummy)) and
         (not GoodForTextCompaction (Position + Result, CodeLen, Dummy)) then
        Inc (Result)
      else
        Done := True;
    end;
  end;

var
  MultipleOfSix  : Boolean;
  BinaryDataSize : Integer;
  i              : Integer;
  j              : Integer;
  A              : array [0..6] of Integer; 

const
  Even6Bytes = 924;
  Odd6Bytes  = 901;

begin
  BinaryDataSize := CountBytes (Position, CodeLen);
  if BinaryDataSize mod 6 = 0 then
    MultipleOfSix := True
  else
    MultipleOfSix := False;
  if MultipleOfSix then
    AddCodeword (Even6Bytes)
  else
    AddCodeword (Odd6Bytes);

  i := 0;
  while i < BinaryDataSize do
    if BinaryDataSize - i < 6 then begin
      AddCodeword (Word (Code[Position + i]));
      Inc (i);
    end else begin
      ConvertBytesToBase900 ([Byte (Code[Position + i]),
                              Byte (Code[Position + i + 1]),
                              Byte (Code[Position + i + 2]),
                              Byte (Code[Position + i + 3]),
                              Byte (Code[Position + i + 4]),
                              Byte (Code[Position + i + 5])], A);
      for j := 1 to 5 do
        AddCodeword (A[j - 1]);                                        {!!.dg}
      Inc (i, 6);
    end;
  Inc (Position, BinaryDataSize);                                      {!!.dg}
end;

procedure TStPDF417Barcode.EncodeNumeric (var Position : Integer;
                                          CodeLen      : Integer);

  function CollectDigits (var Position : Integer;
                          CodeLen      : Integer) : string;
  var
    StartPos : Integer;

  const
    MaxDigitChunk = 44;

  begin
    Result := '';
    StartPos := Position;
    while (Position <= CodeLen) and (Position - StartPos < MaxDigitChunk) and
          (Code[Position] >= '0') and (Code[Position] <= '9') do begin
      Inc (Position);
    end;
    if Position - StartPos > 0 then
      Result := '1' + Copy (Code, StartPos, Position - StartPos); 
  end;

var
  NumericString : string;
  A             : array [0..44] of Integer; 
  LenA          : Integer;
  i             : Integer;
  
const
  NumericLatch = 902;

begin
  AddCodeword (NumericLatch);
  repeat
    NumericString := CollectDigits (Position, CodeLen);
    if NumericString <> '' then begin
      ConvertToBase900 (NumericString, A, LenA);
      for i := 0 to LenA do
        AddCodeword (A[i]);
    end;
  until NumericString = '';
end;

procedure TStPDF417Barcode.EncodeText (var Position : Integer;
                                       CodeLen      : Integer);

  function SelectBestTextMode (
        CurChar : TStPDF417TextCompactionData) : TStPDF417TextCompactionMode;
  begin
    if cmAlpha in CurChar.Mode then
      Result := cmAlpha
    else if cmLower in CurChar.Mode then
      Result := cmLower
    else if cmMixed in CurChar.Mode then
      Result := cmMixed
    else if cmPunctuation in CurChar.Mode then
      Result := cmPunctuation
    else
      Result := cmNone;
  end;

  procedure AddTextCharacter (Value : Word);
  begin
    if FNewTextCodeword then
      FCodewords[FNumCodewords] := 30 * Value
    else begin
      FCodewords[FNumCodewords] := FCodewords[FNumCodewords] + Value;
      Inc (FNumCodewords);
    end;
    FNewTextCodeword := not FNewTextCodeword;
  end;

  function ChangeTextSubmode (CurrentMode : TStPDF417TextCompactionMode;
                              NewMode : TStPDF417TextCompactionMode;
                              UseShift : Boolean) : TStPDF417TextCompactionMode;
  const
    LatchAlphaToLower       = 27;
    LatchAlphaToMixed       = 28;
    ShiftAlphaToPunctuation = 29;
    ShiftLowerToAlpha       = 27;
    LatchLowerToMixed       = 28;
    ShiftLowertoPunctuation = 29;
    LatchMixedToPunctuation = 25;
    LatchMixedToLower       = 27;
    LatchMixedToAlpha       = 28;
    ShiftMixedToPunctuation = 29;
    LatchPunctuationToAlpha = 29;
      
  begin
    if UseShift then
      Result := CurrentMode
    else
      Result := NewMode;
        
    case CurrentMode of
      cmAlpha :
        case NewMode of
          cmLower :
            begin
              { Alpha to Lower.  No shift }
              AddTextCharacter (LatchAlphaToLower);
              if UseShift then
                Result := NewMode;
             end;
          cmMixed :
            begin
              { Alpha to Numeric.  No shift }
              AddTextCharacter (LatchAlphaToMixed);
              if UseShift then
                Result := NewMode;
            end;
          cmPunctuation :
            { Alpha to Punctuation }
            if UseShift then
              AddTextCharacter (ShiftAlphaToPunctuation)
            else begin
              AddTextCharacter (LatchAlphaToMixed);
              AddTextCharacter (LatchMixedToPunctuation);
            end;
        end;

      cmLower :
        case NewMode of
          cmAlpha :
            { Lower to Alpha }
            if UseShift then
              AddTextCharacter (ShiftLowerToAlpha)
            else begin
              AddTextCharacter (LatchLowerToMixed);
              AddTextCharacter (LatchMixedToAlpha);
            end;
          cmMixed :
            begin
              { Lower to Mixed.  No shift }
              AddTextCharacter (LatchLowerToMixed);
              if UseShift then
                Result := NewMode;
            end;
          cmPunctuation :
            { Lower to Punctuation }
            if UseShift then
              AddTextCharacter (ShiftLowerToPunctuation)
            else begin
              AddTextCharacter (LatchLowerToMixed);
              AddTextCharacter (LatchMixedToPunctuation);
            end;
        end;
          
      cmMixed :
        case NewMode of
          cmAlpha :
            begin
              { Mixed to Alpha.  No shift }
              AddTextCharacter (LatchMixedToAlpha);
              if UseShift then
                Result := NewMode;
            end;
          cmLower :
            begin
              { Mixed to Lower.  No shift }
              AddTextCharacter (LatchMixedToLower);
              if UseShift then
                Result := NewMode;
            end;
          cmPunctuation :
            { Mixed to Punctuation }
            if UseShift then
              AddTextCharacter (ShiftMixedToPunctuation)
            else
              AddTextCharacter (LatchMixedToPunctuation);
        end;
      cmPunctuation :
        case NewMode of
          cmAlpha :
            begin
              { Punctuation to Alpha.  No shift }
              AddTextCharacter (LatchPunctuationToAlpha);
              if UseShift then
                Result := NewMode;
            end;
          cmLower :
            begin
              { Punctuation to Lower.  No shift }
              AddTextCharacter (LatchPunctuationToAlpha);
              AddTextCharacter (LatchAlphaToLower);
              if UseShift then
                Result := NewMode;
            end;
          cmMixed :
            begin
              { Punctuation to Mixed.  No shift }
              AddTextCharacter (LatchPunctuationToAlpha);
              AddTextCharacter (LatchAlphaToMixed);
              if UseShift then
                Result := NewMode;
            end;
        end;
    end;
  end;

var
  CurrentTextSubmode : TStPDF417TextCompactionMode;
  CurChar            : TStPDF417TextCompactionData;
  UseShift           : Boolean;
  Done               : Boolean;
  Dummy              : Integer;
  NewChar            : Integer;
  Codeword           : Boolean;

const
  EndingPadChar      = 29;

begin
  { Initialize and get the first character }
  FNewTextCodeword := True;
  CurrentTextSubmode := cmAlpha;
  Done := False;

  { get characters until it is necessary to step out of text mode }
  while (Position <= CodeLen) and (CurChar.Value >= 0) and
        (not Done) do begin
    if (Position <= CodeLen) then begin
      GetNextCharacter (NewChar, Codeword, Position, CodeLen);
      CurChar := TStPDF417TextCompaction[NewChar];
    end;

    if Codeword then begin
      { If the text contains an odd number of letters, follow it with a
        trailing 29 }
      if not FNewTextCodeword then
        AddTextCharacter (EndingPadChar);
      FNewTextCodeword := True;
      { Add the codeword }
      AddCodeword (NewChar)
    end else begin
      { Check if the text submode for the current character is different than
        the current text submode }
      if not (CurrentTextSubmode in CurChar.Mode) then begin
        { if the text submode is different, see if it remains different.  If
          it does, use a latch, otherwise just shift }
        if Position < CodeLen then begin
          if not (CurrentTextSubmode in
             TStPDF417TextCompaction[Integer (Code[Position + 1])].Mode) then
            UseShift := False
          else
            UseShift := True;
        end else
          UseShift := True;

        { Add the shift or latch to the text codewords }
        CurrentTextSubmode := ChangeTextSubmode (CurrentTextSubmode,
                                                 SelectBestTextMode (CurChar),
                                                 UseShift);
      end;

      { Add the character to the codeword array }
      AddTextCharacter (CurChar.Value);
    end;
    { If this is a digit and it looks like a good time to switch to
      numeric mode, do so }
    if GoodForNumericCompaction (Position, CodeLen, Dummy) then
      Done := True;
  end;

  { If the text contains an odd number of letters, follow it with a
    trailing 29 }
  if not FNewTextCodeword then
    AddTextCharacter (EndingPadChar);
end;

procedure TStPDF417Barcode.GenerateCodewords;
var
  ErrorLevel        : Integer;
  NumErrorCodewords : Integer;
  XSize             : Integer;
  YSize             : Integer;

begin
  TextToCodewords;

  ErrorLevel := GetRealErrorLevel;

  NumErrorCodewords := Trunc (Power (2, ErrorLevel + 1));

  CalculateSize (XSize, YSize);

  FUsedCodewords := FNumCodewords;
  FUsedECCCodewords := NumErrorCodewords;
  FFreeCodewords := FTotalCodewords - FUsedCodewords;
  
  { The first codewords is always the length }
  if FNumCodewords +
     (XSize * YSize - FNumCodewords - NumErrorCodewords) < 0 then
    raise E2DBarcodeError.Create (StECodeTooLarge);
  FCodewords[0] := FNumCodewords +
                  (XSize * YSize - FNumCodewords - NumErrorCodewords);

  if NumErrorCodeWords + FNumCodeWords <= XSize * YSize then
    CalculateECC (XSize * YSize - NumErrorCodeWords, NumErrorCodewords)
  else
    raise E2DBarcodeError.Create (StECodeTooLarge);
end;

procedure TStPDF417Barcode.GetNextCharacter (var NewChar  : Integer;
                                             var Codeword : Boolean;
                                             var Position : Integer;
                                             CodeLen      : Integer);
var
  WorkNum : Integer;
                                               
begin
  NewChar := 0;
  Codeword := False;

  if Position <= CodeLen then begin
    if (FCode[Position] = '\') and
       (Position < CodeLen) then begin 
      case FCode[Position + 1] of
        '0'..'9' : begin
          try
            NewChar := StrToInt (Copy (FCode, Position + 1, 3));
            Inc (Position, 4);
          except
            NewChar := 0;
            Inc (Position, 4);
          end;
        end;
        'C', 'c' : begin
          try
            Codeword := True;
            NewChar := StrToInt (Copy (FCode, Position + 2, 3));
            Inc (Position, 5);
          except
            NewChar := 0;
            Inc (Position, 5);
          end;
        end;
        'G', 'g' : begin
          WorkNum := StrToInt (Copy (FCode, Position + 1, 6));
          Inc (Position, 8);
          if (WorkNum >= 0) and (WorkNum <= 899) then begin
            AddCodeword (927);
            Codeword := True;
            NewChar := WorkNum;
          end else if (WorkNum >= 900) and (WorkNum < 810900) then begin
            AddCodeword (926);
            AddCodeword ((WorkNum div 900) - 1);
            Codeword := True;
            NewChar := WorkNum mod 900;
          end else if (WorkNum >= 810900) and (WorkNum < 811800) then begin
            AddCodeword (925);
            Codeword := True;
            NewChar := WorkNum;
          end else
            raise E2DBarcodeError.Create (StEGLIOutOfRange);
        end;
        'X', 'x' : begin
          try
            NewChar := StrToInt ('$' + Copy (FCode, Position + 2, 2));
            Inc (Position, 4);
          except
            NewChar := 0;
            Inc (Position, 4);
          end;
        end;
        '\' : begin
          NewChar := Byte (FCode[Position]);
          Inc (Position, 2);
        end;
        else begin
          NewChar := Byte (FCode[Position]);
          Inc (Position);
        end;
      end;   
    end else begin
      NewChar := Byte (FCode[Position]);
      Inc (Position);
    end;
  end;
end;

function TStPDF417Barcode.GetPDF417ECCLevel : TStPDF417ECCLevels;
begin
  case FECCLevel of
    0 : Result := ecLevel0;
    1 : Result := ecLevel1;
    2 : Result := ecLevel2;
    3 : Result := ecLevel3;
    4 : Result := ecLevel4;
    5 : Result := ecLevel5;
    6 : Result := ecLevel6;
    7 : Result := ecLevel7;
    8 : Result := ecLevel8;
  else
    Result := ecAuto;
  end;
end;

function TStPDF417Barcode.GetRealErrorLevel : Integer;
begin
  if (FECCLevel < 0) then begin
    if FNumCodeWords < 41 then
      Result := 2
    else if FNumCodeWords < 161 then
      Result := 3
    else if FNumCodeWords < 321 then
      Result := 4
    else
      Result := 5;
  end else
    Result := FECCLevel
end;

function TStPDF417Barcode.GoodForNumericCompaction (
                                               Position  : Integer;
                                               CodeLen   : Integer;
                                               var Count : Integer) : Boolean;
const
  BytesNeeded = 13;

begin
  Result := False;
  Count := 0;
  while (Position + Count < CodeLen) and
        (Code[Position + Count] >= '0') and
        (Code[Position + Count] <= '9') do
    Inc (Count);
  if Count > BytesNeeded then
    Result := True;
end;

function TStPDF417Barcode.GoodForTextCompaction (
                                             Position  : Integer;
                                             CodeLen   : Integer;
                                             var Count : Integer) : Boolean;

  function IsGoodTextValue (const v : Char) : Boolean;                 {!!.01}
  begin                                                                {!!.01}
    if v > #127 then                                                   {!!.01}
      Result := False                                                  {!!.01}
    else if TStPDF417TextCompaction[Integer (v)].Value >= 0 then       {!!.01}
      Result := True                                                   {!!.01}
    else                                                               {!!.01}
      Result := False;                                                 {!!.01}
  end;                                                                 {!!.01}

const
  BytesNeeded = 5;

begin
  Result := False;
  Count := 0;
  while (Position + Count < CodeLen) and                               {!!.01}
        (IsGoodTextValue (Code[Position + Count])) and                 {!!.01}
        (Count <= BytesNeeded) do                                      {!!.01}
    Inc (Count);
  if (Count > BytesNeeded) or
     ((Position + Count >= CodeLen) and (Count > 0)) then
    Result := True;
end;

procedure TStPDF417Barcode.RenderToResolution (var OutBitmap : TBitmap;
                                               ResX          : Integer;
                                               ResY          : Integer;
                                               var SizeX     : Integer;
                                               var SizeY     : Integer);
var
  OldBarWidth : Integer;
  OldWidth    : Integer;
  OldHeight   : Integer;
  CurResX     : Integer;
  CurResY     : Integer;
  MultX       : Extended;
  MultY       : Extended;

begin
  OldBarWidth := BarWidth;
  OldWidth := Width;
  OldHeight := Height;
  SizeX := Width;
  SizeY := Height;
  try
    if (ResX <> 0) and (ResY <> 0) then begin
      GetCurrentResolution (CurResX, CurResY);
      MultX := ResX / CurResX;
      MultY := ResY / CurResY;
      FBarWidth := Trunc (FBarWidth * MultX);
      FBitmap.Width := Trunc (FBitmap.Width * MultX);
      FBitmap.Height := Trunc (FBitmap.Height * MultY);
      SizeX := FBitmap.Width;
      SizeY := FBitmap.Height;
    end;
    FBitmap.Canvas.Font.PixelsPerInch := OutBitmap.Canvas.Font.PixelsPerInch;
    GenerateBarcodeBitmap (FBitmap.Width, FBitmap.Height);
    OutBitmap.Width := SizeX;
    OutBitmap.Height := SizeY;
    OutBitmap.Canvas.CopyRect (Rect (0, 0, SizeX, SizeY), FBitmap.Canvas,
                               Rect (0, 0, SizeX, SizeY));
  finally
    FBarWidth := OldBarWidth;
    FBitmap.Width := OldWidth;
    FBitmap.Height := OldHeight;
    GenerateBarcodeBitmap (Width, Height);
  end;
end;

procedure TStPDF417Barcode.SetBarHeight (const v : Integer);
begin
  if (v < 1) and (BarHeightToWidth = 0) and (not RelativeBarHeight) then
    raise E2DBarcodeError.Create (StENeedBarHeight);
  if v < 0 then
    raise E2DBarcodeError.Create (StEBadBarWidth);
  inherited SetBarHeight (v);
end;

procedure TStPDF417Barcode.SetBarHeightToWidth (const v : Integer);
begin
  if (v = 0) and (BarHeight = 0) and (not RelativeBarHeight) then
    raise E2DBarcodeError.Create (StENeedBarHeight);
  inherited SetBarHeightToWidth (v);
end;

procedure TStPDF417Barcode.SetBarWidth (const v : Integer);
begin
  if v < 1 then
    raise E2DBarcodeError.Create (StEBadBarHeight);
  inherited SetBarWidth (v);
end;

procedure TStPDF417Barcode.SetPDF417ECCLevel (const v : TStPDF417ECCLevels);
var
  NewLevel : Integer;
  OldLevel : Integer;

begin
  NewLevel := 0;

  case v of
    ecAuto   : NewLevel := -1;
    ecLevel0 : NewLevel := 0;
    ecLevel1 : NewLevel := 1;
    ecLevel2 : NewLevel := 2;
    ecLevel3 : NewLevel := 3;
    ecLevel4 : NewLevel := 4;
    ecLevel5 : NewLevel := 5;
    ecLevel6 : NewLevel := 6;
    ecLevel7 : NewLevel := 7;
    ecLevel8 : NewLevel := 8;
  end;

  if NewLevel <> FECCLevel then begin
    OldLevel := FECCLevel;
    try
      FECCLevel := NewLevel;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FECCLevel := OldLevel;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStPDF417Barcode.SetRelativeBarHeight (const v : Boolean); 
begin
  if (not v) and (BarHeightToWidth = 0) and (BarHeight = 0) then
    raise E2DBarcodeError.Create (StENeedBarHeight);
  inherited SetRelativeBarHeight (v);
end;

procedure TStPDF417Barcode.SetTruncated (const v : Boolean);
var
  OldTruncated : Boolean;

begin
  if v <> FTruncated then begin
    OldTruncated := FTruncated;
    try
      FTruncated := v;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FTruncated := OldTruncated;
        try
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

function TStPDF417Barcode.IsNumericString (const S : string) : boolean;
var
  i      : integer;
  LenS : integer;
  
begin
  {note: an assertion test for ConvertToBase900}
  Result := false;
  LenS := length(S);
  if (LenS = 0) or (LenS > 45) then
    Exit;
  for i := 1 to LenS do
    if not (('0' <= S[i]) and (S[i] <= '9')) then
      Exit;
  Result := true;
end;

procedure TStPDF417Barcode.SetNumColumns (const v : Integer);
var
  OldNumColumns : Integer;

begin
  if (v < 0) or (v > 30) then
    raise E2DBarcodeError.Create (StEBadNumCols);
  if v <> FNumColumns then begin
    OldNumColumns := FNumColumns;
    try
      if v < 0 then
        FNumColumns := 0
      else
        FNumColumns := v;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FNumColumns := OldNumColumns;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStPDF417Barcode.SetNumRows (const v : Integer);
var
  OldNumRows : Integer;

begin
  if (v < 0) or (v > 90) then
    raise E2DBarcodeError.Create (StEBadNumRows);
  if v <> FNumRows then begin
    OldNumRows := FNumRows;
    try
      if v < 0 then
        FNumRows := 0
      else
        FNumRows := v;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FNumRows := OldNumRows;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStPDF417Barcode.TextToCodewords;
var
  i                  : Integer;
  CodeLen            : Integer;
  CurrentMode        : TStDataMode;
  Count              : Integer;
  First              : Boolean;
  
const
  TextCompaction     = 900;
  PadCodeword        = 900;

begin
  First := True;
  for i := 0 to 2700 do
    FCodewords[i] := PadCodeword;
  FNumCodewords := 1; { There will always be a length codeword }
  i := 1;

  CodeLen := Length (Code); 
  if CodeLen = 0 then
    Exit;

  if GoodForNumericCompaction (i, CodeLen, Count) then
    CurrentMode := dmNumeric
  else if GoodForTextCompaction (i, CodeLen, Count) then
    CurrentMode := dmText
  else
    CurrentMode := dmBinary;

  while i < CodeLen do begin
    case CurrentMode of
      dmBinary :
        EncodeBinary (i, CodeLen);
      dmText :
        if First then
          EncodeText (i, CodeLen);
      dmNumeric :
        EncodeNumeric (i, CodeLen);
    end;

    if GoodForNumericCompaction (i, CodeLen, Count) then
      CurrentMode := dmNumeric
    else if GoodForTextCompaction (i, CodeLen, Count) then begin
      if not First then
        AddCodeword (TextCompaction); 
      CurrentMode := dmText;
      EncodeText (i, CodeLen);                                         {!!.01}
    end else
      CurrentMode := dmBinary;
    First := False;
  end;
end;

{ TStMaxiCodeBarcode }

constructor TStMaxiCodeBarcode.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  FMode                := cmMode4;
  FHighlight           := False;
  FShowCodewords       := False;
  FShowAll             := False;
  FAutoScale           := True;
  FBarWidth            := 0;
  FBarHeight           := 0;
  FHorPixelsPerMM      := 4;
  FVerPixelsPerMM      := 4;
  FMaxiHexWidth        := 9;
  FMaxiHexHeight       := 9;
  FMaxiHexVOffset      := -2;
  FMaxiHexHOffset      := 4;
  FCarrierCountryCode  := 0;
  FCarrierServiceClass := 0;
  FCarrierPostalCode   := '000000000';
  
  GetSizes;

  Width                := 121;
  Height               := 129;

  GenerateCodewords;
  GenerateBarcodeBitmap (Width, Height);
end;

procedure TStMaxiCodeBarcode.AddCodeword (Value : Integer);
begin
  if FNumCodewords <= 144 then
    FMessage[FNumCodewords] := Value;
  Inc (FNumCodewords);
end;

function TStMaxiCodeBarcode.CalculateBarCodeWidth (
                                        PaintableWidth : Integer) : Integer;
begin
  Result := Round (30 * FMaxiHexWidth + FMaxiHexHOffset);
end;

function TStMaxiCodeBarcode.CalculateBarCodeHeight (
                                        PaintableHeight : Integer) : Integer;
begin
  Result := Round (33 * FMaxiHexHeight + 33 * FMaxiHexVOffset);
end;

procedure TStMaxiCodeBarcode.DrawBarcode;

  function IsBitOn (Value : Byte; Bit : Byte) : Boolean;
  begin
    Result := ((Value shr Bit) and $01) <> $00;
  end;

const
  {
    The MaxBits array is arranged to match the hex layout of the MaxiCode
    Barcode.
    
    -2  identifies the (light) module at the center of the finder pattern,
    -1  identifies modules which are always dark,
     0  identifies modules which are always light, and
    Positive numbers indicate the bitnumber of the cell.
  }
  MaxBits : array [0..32] of array [0..29] of Integer =
(( 122,121,128,127,134,133,140,139,146,145,152,151,158,157,164,163,170,169,176,175,182,181,188,187,194,193,200,199, -1, -1   ),
 (   124,123,130,129,136,135,142,141,148,147,154,153,160,159,166,165,172,171,178,177,184,183,190,189,196,195,202,201,817,  0 ),
 ( 126,125,132,131,138,137,144,143,150,149,156,155,162,161,168,167,174,173,180,179,186,185,192,191,198,197,204,203,819,818   ),
 (   284,283,278,277,272,271,266,265,260,259,254,253,248,247,242,241,236,235,230,229,224,223,218,217,212,211,206,205,820,  0 ),
 ( 286,285,280,279,274,273,268,267,262,261,256,255,250,249,244,243,238,237,232,231,226,225,220,219,214,213,208,207,822,821   ),
 (   288,287,282,281,276,275,270,269,264,263,258,257,252,251,246,245,240,239,234,233,228,227,222,221,216,215,210,209,823,  0 ),
 ( 290,289,296,295,302,301,308,307,314,313,320,319,326,325,332,331,338,337,344,343,350,349,356,355,362,361,368,367,825,824   ),
 (   292,291,298,297,304,303,310,309,316,315,322,321,328,327,334,333,340,339,346,345,352,351,358,357,364,363,370,369,826,  0 ),
 ( 294,293,300,299,306,305,312,311,318,317,324,323,330,329,336,335,342,341,348,347,354,353,360,359,366,365,372,371,828,827   ),
 (   410,409,404,403,398,397,392,391, 80, 79, -1, -1, 14, 13, 38, 37,  3,  0, 45, 44,110,109,386,385,380,379,374,373,829,  0 ),
 ( 412,411,406,405,400,399,394,393, 82, 81, 41, -1, 16, 15, 40, 39,  4,  0,  0, 46,112,111,388,387,382,381,376,375,831,830   ),
 (   414,413,408,407,402,401,396,395, 84, 83, 42,  0,  0,  0,  0,  0,  6,  5, 48, 47,114,113,390,389,384,383,378,377,832,  0 ),
 ( 416,415,422,421,428,427,104,103, 56, 55, 17,  0,  0,  0,  0,  0,  0,  0, 21, 20, 86, 85,434,433,440,439,446,445,834,833   ),
 (   418,417,424,423,430,429,106,105, 58, 57,  0,  0,  0,  0,  0,  0,  0,  0, 23, 22, 88, 87,436,435,442,441,448,447,835,  0 ),
 ( 420,419,426,425,432,431,108,107, 60, 59,  0,  0,  0,  0,  0,  0,  0,  0,  0, 24, 90, 89,438,437,444,443,450,449,837,836   ),
 (   482,481,476,475,470,469, 49, -1, 31,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1, 54, 53,464,463,458,457,452,451,838,  0 ),
 ( 484,483,478,477,472,471, 50,  0, -1,  0,  0,  0,  0,  0, -2,  0,  0,  0,  0,  0, -1,  0,466,465,460,459,454,453,840,839   ),
 (   486,485,480,479,474,473, 52, 51, 32,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2, -1, 43,468,467,462,461,456,455,841,  0 ),
 ( 488,487,494,493,500,499, 98, 97, 62, 61,  0,  0,  0,  0,  0,  0,  0,  0,  0, 27, 92, 91,506,505,512,511,518,517,843,842   ),
 (   490,489,496,495,502,501,100, 99, 64, 63,  0,  0,  0,  0,  0,  0,  0,  0, 29, 28, 94, 93,508,507,514,513,520,519,844,  0 ),
 ( 492,491,498,497,504,503,102,101, 66, 65, 18,  0,  0,  0,  0,  0,  0,  0, 19, 30, 96, 95,510,509,516,515,522,521,846,845   ),
 (   560,559,554,553,548,547,542,541, 74, 73, 33,  0,  0,  0,  0,  0,  0, 11, 68, 67,116,115,536,535,530,529,524,523,847,  0 ),
 ( 562,561,556,555,550,549,544,543, 76, 75, -1,  0,  8,  7, 36, 35, 12, -1, 70, 69,118,117,538,537,532,531,526,525,849,848   ),
 (   564,563,558,557,552,551,546,545, 78, 77, -1, 34, 10,  9, 26, 25,  0, -1, 72, 71,120,119,540,539,534,533,528,527,850,  0 ),
 ( 566,565,572,571,578,577,584,583,590,589,596,595,602,601,608,607,614,613,620,619,626,625,632,631,638,637,644,643,852,851   ),
 (   568,567,574,573,580,579,586,585,592,591,598,597,604,603,610,609,616,615,622,621,628,627,634,633,640,639,646,645,853,  0 ),
 ( 570,569,576,575,582,581,588,587,594,593,600,599,606,605,612,611,618,617,624,623,630,629,636,635,642,641,648,647,855,854   ),
 (   728,727,722,721,716,715,710,709,704,703,698,697,692,691,686,685,680,679,674,673,668,667,662,661,656,655,650,649,856,  0 ),
 ( 730,729,724,723,718,717,712,711,706,705,700,699,694,693,688,687,682,681,676,675,670,669,664,663,658,657,652,651,858,857   ),
 (   732,731,726,725,720,719,714,713,708,707,702,701,696,695,690,689,684,683,678,677,672,671,666,665,660,659,654,653,859,  0 ),
 ( 734,733,740,739,746,745,752,751,758,757,764,763,770,769,776,775,782,781,788,787,794,793,800,799,806,805,812,811,861,860   ),
 (   736,735,742,741,748,747,754,753,760,759,766,765,772,771,778,777,784,783,790,789,796,795,802,801,808,807,814,813,862,  0 ),
 ( 738,737,744,743,750,749,756,755,762,761,768,767,774,773,780,779,786,785,792,791,798,797,804,803,810,809,816,815,864,863   ));

  ColorTable : array [0..11] of TColor = ($bbffff, $ffbbff, $ffffbb, $bbbbff,
                                          $bbffbb, $ffbbbb, $a0cbff, $a0ffcb,
                                          $ffa0cb, $cba0ff, $ffcba0, $cbffa0);
var
  i         : Integer;
  j         : Integer;
  XPos      : Integer;
  YPos      : Integer;
  RowOffset : Extended;
  ByteNum   : Integer;
  BitOffset : Integer;

begin

  FBitmap.Canvas.Brush.Color := Color;
  FBitmap.Canvas.Pen.Width := 1;

  YPos := 0;
  RowOffset := 0;

  for i := 0 to 32 do begin
    for j := 0 to 29 do begin
      XPos := Round (j * FMaxiHexWidth + RowOffset);
      if FHighlight then begin
        FBitmap.Canvas.Pen.Color := Color;
        FBitmap.Canvas.Brush.Color := Color;
      end;

      ByteNum := MaxBits[i, j];
      if ByteNum = -1 then begin
        if FHighlight then
          FBitmap.Canvas.Pen.Color := $505050;
        DrawHex (XPos, YPos);
        if FHighlight then
          FBitmap.Canvas.Pen.Color := Color;
      end else if ByteNum > 0 then begin
        BitOffset := ((ByteNum - 1) mod 6);
        ByteNum := (ByteNum - 1) div 6 {+ 1}; { Codeword 1 is the mode }
        if FHighlight then begin
          if not FShowCodewords then
            case ByteNum of
              0 : FBitmap.Canvas.Pen.Color := ColorTable[0];
              1..9 : FBitmap.Canvas.Pen.Color := ColorTable[1];
              10..19 : FBitmap.Canvas.Pen.Color := ColorTable[2];
              20..87 : FBitmap.Canvas.Pen.Color := ColorTable[3];
              89..103 : FBitmap.Canvas.Pen.Color := ColorTable[4];
              104..144 : FBitmap.Canvas.Pen.Color := ColorTable[5];
            end
          else
            FBitmap.Canvas.Pen.Color := ColorTable[ByteNum mod 12];
          FBitmap.Canvas.Brush.Color := FBitmap.Canvas.Pen.Color;
          DrawHex (XPos, YPos);
          FBitmap.Canvas.Pen.Color := Color;
          FBitmap.Canvas.Brush.Color := Color;
        end;
        if IsBitOn (FCodewords[ByteNum], 5 - BitOffset) then 
          DrawHex (XPos, YPos)
        else if FShowAll then
          DrawHex (XPos, YPos);
      end;
    end;
    RowOffset := FMaxiHexHOffset - RowOffset;
    YPos := Round (((i + 1) * FMaxiHexHeight) * 0.8660254);
  end;

  DrawFinder;
end;

procedure TStMaxiCodeBarcode.DrawFinder;
var
  CenterX : Integer;
  CenterY : Integer;

begin
  CenterX := Round (BarCodeRect.Left + 14.5 * FMaxiHexWidth);
  CenterY := BarCodeRect.Top + Round (16.5 * FMaxiHexHeight * 0.8660254);
  FBitmap.Canvas.Brush.Color := BackgroundColor;
  FBitmap.Canvas.Pen.Width := Round (FMaxiHexWidth + FMaxiHexVOffset);
  FBitmap.Canvas.Ellipse (
      CenterX - Round (FMaxiHexWidth)  * 4,
      CenterY - Round (FMaxiHexHeight) * 4,
      CenterX + Round (FMaxiHexWidth)  * 4,
      CenterY + Round (FMaxiHexHeight) * 4);
  FBitmap.Canvas.Ellipse (
      CenterX - Round (FMaxiHexWidth  * 2.5),
      CenterY - Round (FMaxiHexHeight * 2.5),
      CenterX + Round (FMaxiHexWidth  * 2.5),
      CenterY + Round (FMaxiHexHeight * 2.5));
  FBitmap.Canvas.Ellipse (
      CenterX - Round (FMaxiHexWidth),
      CenterY - Round (FMaxiHexHeight),
      CenterX + Round (FMaxiHexWidth),
      CenterY + Round (FMaxiHexHeight));

  if FHighlight then begin
    FBitmap.Canvas.Pen.Width := 1;
    FBitmap.Canvas.Pen.Color := clRed;
    FBitmap.Canvas.MoveTo (CenterX, 0);
    FBitmap.Canvas.LineTo (CenterX, Height);
    FBitmap.Canvas.MoveTo (0, CenterY);
    FBitmap.Canvas.LineTo (Width, CenterY);
  end;

  FBitmap.Canvas.Pen.Width := 1;
  FBitmap.Canvas.Brush.Color := Color;
end;

procedure TStMaxiCodeBarcode.DrawHex (XPos, YPos : Integer);
var
  XOffset   : Integer;
  YOffset   : Integer;
  HexWidth  : Integer;
  HexHeight : Integer;
  Border    : Extended;

begin
  XOffset := BarCodeRect.Left + XPos;
  YOffset := BarCodeRect.Top + YPos;
  Border := ((FMaxiHexWidth +  1) / 8) + 1;
  if FMaxiHexWidth >= 4 then begin
    XOffset := Round (XOffset + Border);
    YOffset := Round (YOffset + Border);
    HexWidth := Round (FMaxiHexWidth - Border);
    HexHeight := Round (FMaxiHexHeight - Border);
  end else begin
    HexWidth := Round (FMaxiHexWidth);
    HexHeight := Round (FMaxiHexHeight);
  end;

  if (HexWidth < 4) or (HexHeight < 4) then
    { Ellipses look better at poorer resolutions }
    FBitmap.Canvas.Ellipse (XOffset, YOffset,
                            XOffset + HexWidth, YOffset + HexHeight)
  else begin
    { Better resolution, draw a hex }
    FBitmap.Canvas.MoveTo (XOffset + HexWidth div 2,
                           YOffset);
    FBitmap.Canvas.LineTo (XOffset + HexWidth,
                           YOffset + HexHeight div 4);
    FBitmap.Canvas.LineTo (XOffset + HexWidth,
                           YOffset + HexHeight - (HexHeight div 4));
    FBitmap.Canvas.LineTo (XOffset + HexWidth div 2,
                           YOffset + HexHeight);
    FBitmap.Canvas.LineTo (XOffset,
                           YOffset + HexHeight - (HexHeight div 4));
    FBitmap.Canvas.LineTo (XOffset,
                           YOffset + HexHeight div 4);
    FBitmap.Canvas.LineTo (XOffset + HexWidth div 2,
                           YOffset);

    FBitmap.Canvas.FloodFill (XOffset + HexWidth div 2,
                              YOffset + HexWidth div 2,
                              BackgroundColor,
                              fsSurface);
  end;
end;

procedure TStMaxiCodeBarcode.GenerateCodewords;
begin
  TextToCodewords;
end;

procedure TStMaxiCodeBarcode.GenerateECC;
 { Calculate the ECC codes for MaxiCode }

  function GFSum (a : Integer; b : Integer) : Integer;
  { Sum of two numbers in Galois field arithmetic }
  begin
    Result := a xor b;
  end;

  function GfDifference (a : Integer; b : Integer) : Integer;
  { difference between two numbers in Galois field arithmetic (included for
    completeness) }
  begin
    Result := a xor b;
  end;

  function GFProduct (a : Integer; b : Integer) : Integer;
  { Product of two numbers in Galois field arithmetic }
  begin
    if (a = 0) or (b = 0) then
      Result := 0
    else
      Result := FAntiLog[(FLog[a] + FLog[b]) mod (StMaxiCodeGaloisField - 1)];
  end;

  function GFQuotient (a : Integer; b : Integer) : Integer;
  { Division of two numbers in Galois field arithmetic (included for
    completeness ) }
  begin
    if b = 0 then
      Result := 1 - StMaxiCodeGaloisField
    else if a = 0 then
      Result := 0
    else
      Result := FAntiLog[(FLog[a] - FLog[b] +
                (StMaxiCodeGaloisField - 1)) mod (StMaxiCodeGaloisField - 1)];
  end;

  procedure FillLogArrays (StMaxiCodeGaloisField : Integer;
                           StMaxiCodeECCPoly : Integer);
  { Populate the log and antilog tables for Galois field arithmetic }
  var
    i : Integer;

  begin
    FLog[0] := 1 - StMaxiCodeGaloisField;
    FAntiLog[0] := 1;
    for i := 1 to StMaxiCodeGaloisField - 1 do begin
      FAntiLog[i] := FAntiLog[i - 1] * 2;
      if FAntiLog[i] >= StMaxiCodeGaloisField then
        FAntiLog[i] := FAntiLog[i] xor StMaxiCodeECCPoly;
      FLog[FAntiLog[i]] := i;
    end;
  end;

  procedure CalculateECCCodes (var Data   : TStMaxiCodeECCData;
                               Polynomial : TStMaxiCodeECCPoly;
                               IStart     : TStMaxiCodeECCInterleave);
  { Calculate the Reed-Solomon error correcting codes (ECC) for MaxiCode.
    Basically, this is the equivalent of taking the Data as a series of
    coefficients to a polynomial (that has the lowest power the same as the
    highest power of the generating polynomial) and dividing it by the
    generating polynomial using Galois field arithmetic.  Get the remainder of
    this division and use that as the Reed Solomon error correcting codes }

  const
    { Generating polynomials }
    GPrimary : array [0..10] of Integer =
      (46, 44, 49, 3, 2, 57, 42, 39, 28, 31, 1);
    GEnhanced : array [0..28] of Integer =
      (28, 11, 20, 7, 43, 9, 41, 34, 49, 46, 37, 40, 55, 34, 45, 61, 13, 23,
       29, 22, 10, 35, 55, 41, 10, 53, 45, 22, 1);
    GStandard : array [0..20] of Integer =
      (59, 23, 19, 31, 33, 38, 17, 22, 48, 15, 36, 57, 37, 22, 8, 27, 33, 11,
       44, 23, 1);

  var
    BRegisters   : TStMaxiCodeECCData; { Works space for calculating RS ECC }
    DataPos      : Integer;            { Position for data read/writes }
    i            : Integer;
    j            : Integer;
    SumFromLast  : Integer;            { Result of input data + Last BReg }
    GenPolyMult  : Integer;            { Input data (SumFromLast) * gen poly }
    NumCodewords : Integer;            { Number of ECC codewords to generate }
    Interleaved  : Boolean;            { Read all data or alternate chars }
    StartingPos  : Integer;            { Where to start reading from }
    DataLength   : Integer;            { Amount of data to read }
    OutDataPos   : Integer;            { Where to write ECC to }

  begin
    { Intialize where to get data, write data, what poly to use, etc.. based
     from the Polynomial used and whether or not the even characters or
     odd characters are being encoded. }
    case Polynomial of
      epStandard :
        { Standard Error Correction }
        begin
          NumCodewords := 20;
          Interleaved := True;
          if IStart = imOdd then begin
            StartingPos := 20;
            OutDataPos := 104;
        end else begin
            StartingPos := 21;
            OutDataPos := 105;
          end;
          DataLength := 42;
        end;
      epEnhanced :
        begin
          { Enhanced Error Correction }
          NumCodewords := 28;
          Interleaved := True;
          if IStart = imOdd then begin
            StartingPos := 20;
            OutDataPos := 88;
          end else begin
            StartingPos := 21;
            OutDataPos := 89;
          end;
          DataLength := 34;
        end
      else begin
        { Primary Message }
        NumCodewords := 10;
        Interleaved := False;
        StartingPos := 0;
        OutDataPos := 10;
        DataLength := 10; 
      end;
    end;

    { Initialize all the BRegisters }
    for i := 0 to StMaxMaxiCodeECCDataSize do
      BRegisters[i] := 0;

    { Calculate the Log and AntiLog tables }
    FillLogArrays (StMaxiCodeGaloisField, StMaxiCodeECCPoly);

    DataPos := StartingPos;

    { Divide the polynomials and store the results in the BRegisters }
    for i := 0 to DataLength - 1 do begin
      SumFromLast := GFSum (BRegisters[NumCodewords - 1], Data[DataPos]);
      for j := NumCodewords - 1 downto 0 do begin
        case Polynomial of
          epStandard :
            GenPolyMult := GFProduct (SumFromLast, GStandard[j]);
          epEnhanced :
            GenPolyMult := GFProduct (SumFromLast, GEnhanced[j]);
        else
          GenPolyMult := GFProduct (SumFromLast, GPrimary[j]);
        end;
        if j > 0 then
          BRegisters[j] := GFSum (BRegisters[j - 1], GenPolyMult)
        else
          BRegisters[j] := GenPolyMult;
      end;
      if Interleaved then
        Inc (DataPos, 2)
      else
        Inc (DataPos);
    end;

    { Write the ECC values back into the data }
    DataPos := OutDataPos;
    for i := NumCodewords - 1 downto 0 do begin
      Data[DataPos] := BRegisters[i];
      if Interleaved then
        Inc (DataPos, 2)
      else
        Inc (DataPos);
    end;
  end;

begin
  { Calculate ECC codes for MaxiCode }

  CalculateECCCodes (FCodewords, epPrimary,  imNone); 
  if Mode = cmMode5 then begin
    CalculateECCCodes (FCodewords, epEnhanced, imEven);
    CalculateECCCodes (FCodewords, epEnhanced, imOdd);
  end else begin
    CalculateECCCodes (FCodewords, epStandard, imEven);
    CalculateECCCodes (FCodewords, epStandard, imOdd);
  end;
end;

procedure TStMaxiCodeBarcode.GetNextCharacter (var NewChar  : Integer;
                                               var Codeword : Boolean;
                                               var Position : Integer;
                                               CodeLen      : Integer);
var
  WorkNum : Integer;
                                                 
begin
  NewChar := 0;
  Codeword := False;

  if Position <= CodeLen then begin   
    if (FCode[Position] = '\') and
       (Position < CodeLen) then begin
      case FCode[Position + 1] of
        '0'..'9' : begin
          try
            NewChar := StrToInt (Copy (FCode, Position + 1, 3));
            Inc (Position, 4);
          except
            NewChar := 0;
            Inc (Position, 4);
          end;
        end;
        'C', 'c' : begin
          try
            Codeword := True;
            NewChar := StrToInt (Copy (FCode, Position + 2, 2));
            Inc (Position, 4);
          except
            NewChar := 0;
            Inc (Position, 4);
          end;
        end;
        'E', 'e' : begin
          if UpperCase (Copy (FCode, Position + 1, 3)) = 'EOT' then begin
            NewChar := 4;
            Inc (Position, 4);
          end else
            try
              WorkNum := StrToInt (Copy (FCode, Position + 1, 6));
              AddCodeword (27);
              Codeword := True;
              Inc (Position, 8);
              if (WorkNum >= 0) and (WorkNum <= 31) then begin
                NewChar := WorkNum;
              end else if (WorkNum >= 32) and (WorkNum <= 1023) then begin
                AddCodeword ($20 or (WorkNum div 64));
                NewChar := WorkNum mod 64;
              end else if (WorkNum >= 1024) and (WorkNum <= 32767) then begin
                AddCodeword ($30 or (WorkNum div 4096));
                WorkNum := WorkNum mod 4096;
                AddCodeword (WorkNum div 64);
                NewChar := WorkNum mod 64;
              end else if (WorkNum >= 32768) and (WorkNum <= 999999) then begin
                AddCodeword ($38 or (WorkNum div 262144));
                WorkNum := WorkNum mod 262144;
                AddCodeword (WorkNum div 64);
                WorkNum := WorkNum mod 4096;
                AddCodeword (WorkNum div 64);
                NewChar := WorkNum mod 64;
              end else
                raise E2DBarcodeError.Create (StEGLIOutOfRange);
            except
              on EConvertError do begin
                NewChar := Byte (FCode[Position]);
                Inc (Position);
              end;
            end;
        end;
        'F', 'f', 'G', 'g', 'N', 'n', 'R', 'r' : begin
          if Position < CodeLen - 1 then begin 
            if (FCode[Position + 2] = 'S') or
               (FCode[Position + 2] = 's') then begin
              case FCode[Position + 1] of
                'F', 'f' : NewChar := 28;
                'G', 'g' : NewChar := 29;
                'N', 'n' : begin
                  NewChar := 31;
                  Codeword := True;
                end;
                'R', 'r' : NewChar := 30;
              end;
              Inc (Position, 3);
            end else begin
              NewChar := Byte (FCode[Position]);
              Inc (Position);
            end;
          end else begin
            NewChar := Byte (FCode[Position]);
            Inc (Position);
          end;
        end;
        'X', 'x' : begin
          try
            NewChar := StrToInt ('$' + Copy (FCode, Position + 2, 2));
            Inc (Position, 4);
          except
            NewChar := 0;
            Inc (Position, 4);
          end;
        end;
        '\' : begin
          NewChar := Byte (FCode[Position]);
          Inc (Position, 2);
        end;
        else begin
          NewChar := Byte (FCode[Position]);
          Inc (Position);
        end;
      end;
    end else begin
      NewChar := Byte (FCode[Position]);
      Inc (Position);
    end;
  end;
end;

procedure TStMaxiCodeBarcode.GetSizes;
var
  ResX : Integer;
  ResY : Integer;

begin
  ResX := GetDeviceCaps (FBitmap.Canvas.Handle, LOGPIXELSX);
  ResY := GetDeviceCaps (FBitmap.Canvas.Handle, LOGPIXELSY);
  GetSizesEx (ResX, ResY);
end;

procedure TStMaxiCodeBarcode.GetSizesEx (ResX : Integer; ResY : Integer);
begin
  if FAutoScale then begin
    FMaxiHexWidth := (ResX * 1.003937) / 29;  { Width is 1.00" }
    FMaxiHexHeight := (ResY * 0.959449) /29;  { Height is 0.96" }
    FMaxiHexVOffset := -1 * (FMaxiHexHeight / 6);
    FMaxiHexHOffset := FMaxiHexWidth / 2;
  end else begin
    if BarWidth <> 0 then
      FMaxiHexWidth := BarWidth
    else
      FMaxiHexWidth :=   (FHorPixelsPerMM * 27) / 29;
    if BarHeight <> 0 then
      FMaxiHexHeight := BarHeight
    else
      FMaxiHexHeight := Round (FVerPixelsPerMM * 25) / 29;
    FMaxiHexVOffset := -1 * (FMaxiHexHeight / 6);
    FMaxiHexHOffset := FMaxiHexWidth / 2;
  end;
end;

procedure TStMaxiCodeBarcode.PlotCell (Row : Integer; Col : Integer);
var
  XPos : Integer;
  YPos : Integer;

begin
  YPos := Round (Row * FMaxiHexHeight + Row * FMaxiHexVOffset);
  if (Row mod 2) <> 0 then
    XPos := Round (FMaxiHexHOffset + FMaxiHexWidth * Col)
  else
    XPos := Round (FMaxiHexWidth * Col);
  DrawHex (XPos, YPos);
end;

procedure TStMaxiCodeBarcode.RenderToResolution (var OutBitmap : TBitmap;
                                                 ResX          : Integer;
                                                 ResY          : Integer;
                                                 var SizeX     : Integer;
                                                 var SizeY     : Integer);
var
  OldBarWidth       : Integer;
  OldBarHeight      : Integer;
  OldHorPixelsPerMM : Extended;
  OldVerPixelsPerMM : Extended;
  OldWidth          : Integer;
  OldHeight         : Integer;
  CurResX           : Integer;
  CurResY           : Integer;
  MultX             : Extended;
  MultY             : Extended;
  OldPPI            : Integer;

begin
  OldBarWidth       := BarWidth;
  OldBarHeight      := BarHeight;
  OldHorPixelsPerMM := FHorPixelsPerMM;
  OldVerPixelsPerMM := FVerPixelsPerMM;
  OldWidth          := Width;
  OldHeight         := Height;
  SizeX             := Width;
  SizeY             := Height;
  try
    if (ResX <> 0) and (ResY <> 0) then begin
      GetCurrentResolution (CurResX, CurResY);
      MultX           := ResX / CurResX;
      MultY           := ResY / CurResY;

      FBarWidth       := Trunc (FBarWidth * MultX);
      FBarHeight      := Trunc (FBarHeight * MultX);
      FHorPixelsPerMM := FHorPixelsPerMM * MultX;
      FVerPixelsPerMM := FVerPixelsPerMM * MultX;
      GetSizesEx (ResX, ResY);
      FBitmap.Width   := Trunc (FBitmap.Width * MultX);
      FBitmap.Height  := Trunc (FBitmap.Height * MultY);

      SizeX           := FBitmap.Width;
      SizeY           := FBitmap.Height;
    end;
    OldPPI            := FBitmap.Canvas.Font.PixelsPerInch;
    try
      FBitmap.Canvas.Font.PixelsPerInch := OutBitmap.Canvas.Font.PixelsPerInch;
      GenerateBarcodeBitmap (FBitmap.Width, FBitmap.Height);
    finally
      FBitmap.Canvas.Font.PixelsPerInch := OldPPI;
    end;
    OutBitmap.Width  := SizeX;
    OutBitmap.Height := SizeY;
    OutBitmap.Canvas.CopyRect (Rect (0, 0, SizeX, SizeY), FBitmap.Canvas,
                               Rect (0, 0, SizeX, SizeY));
  finally
    FBarWidth       := OldBarWidth;
    FBarHeight      := OldBarHeight;
    FHorPixelsPerMM := OldHorPixelsPerMM;
    FVerPixelsPerMM := OldVerPixelsPerMM;
    FBitmap.Width   := OldWidth;
    FBitmap.Height  := OldHeight;
    GetSizes;
    GenerateBarcodeBitmap (Width, Height);
  end;
end;

procedure TStMaxiCodeBarcode.SetAutoScale (const v : Boolean);
var
  OldAutoScale : Boolean;

begin
  if v <> FAutoScale then begin
    OldAutoScale := FAutoScale;
    try
      if (BarHeight = 0) and (HorPixelsPerMM = 0) and (not v) then
        raise E2DBarcodeError.Create (StENeedHorz);
      if (BarWidth = 0) and (VerPixelsPerMM = 0) and (not v) then
        raise E2DBarcodeError.Create (StENeedVert);
      FAutoScale := v;
      GetSizes;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FAutoScale := OldAutoScale;
        try
          GetSizes;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.SetBarHeight (const v : Integer);
begin
  if (v = 0) and (VerPixelsPerMM = 0) and (not AutoScale) then
    raise E2DBarcodeError.Create (StENeedVert);
  inherited SetBarHeight (v);
  GetSizes;
  GenerateBarcodeBitmap (Width, Height);
  Invalidate;
end;

procedure TStMaxiCodeBarcode.SetBarWidth (const v : Integer);
begin
  if (v = 0) and (HorPixelsPerMM = 0) and (not AutoScale) then
    raise E2DBarcodeError.Create (StENeedHorz);
  inherited SetBarWidth (v);
  GetSizes;
  GenerateBarcodeBitmap (Width, Height);
  Invalidate;
end;

procedure TStMaxiCodeBarcode.SetCarrierCountryCode (const v : Integer);
var
  OldCarrierCountryCode : Integer;

begin
  if v <> FCarrierCountryCode then begin
    OldCarrierCountryCode := FCarrierCountryCode;
    try
      FCarrierCountryCode := v;
      if (FMode = cmMode2) or (FMode = cmMode3) then begin
        GenerateCodewords;
        GenerateBarcodeBitmap (Width, Height);
        Invalidate;
      end;
    except
      on E2DBarcodeError do begin
        FCarrierCountryCode := OldCarrierCountryCode;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.SetCarrierPostalCode (const v : string);
var
  OldCarrierPostalCode : string;

begin
  if v <> FCarrierPostalCode then begin
    OldCarrierPostalCode := FCarrierPostalCode;
    try
      FCarrierPostalCode := v;
      if (FMode = cmMode2) or (FMode = cmMode3) then begin
        GenerateCodewords;
        GenerateBarcodeBitmap (Width, Height);
        Invalidate;
      end;
    except
      on E2DBarcodeError do begin
        FCarrierPostalCode := OldCarrierPostalCode;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.SetCarrierServiceClass (const v : Integer);
var
  OldCarrierServiceClass : Integer;

begin
  if v <> FCarrierServiceClass then begin
    OldCarrierServiceClass := FCarrierServiceClass;
    try
      FCarrierServiceClass := v;
      if (FMode = cmMode2) or (FMode = cmMode3) then begin
        GenerateCodewords;
        GenerateBarcodeBitmap (Width, Height);
        Invalidate;
      end;
    except
      on E2DBarcodeError do begin
        FCarrierServiceClass := OldCarrierServiceClass;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.SetHorPixelsPerMM (const v : Extended);
var
  OldHorPixelsPerMM : Extended;

begin
  if v <> FHorPixelsPerMM then begin
    if (v = 0) and (BarWidth = 0) and (not AutoScale) then
      raise E2DBarcodeError.Create (StENeedHorz);
    OldHorPixelsPerMM := FHorPixelsPerMM;
    try
      FHorPixelsPerMM := v;
      GetSizes;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FHorPixelsPerMM := OldHorPixelsPerMM;
        try
          GetSizes;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.SetMode (const v : TStMaxiCodeMode);
var
  OldMode : TStMaxiCodeMode;

begin
  if v <> FMode then begin
    OldMode := Mode;
    try
      FMode := v;
      GenerateCodewords;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FMode := OldMode;
        try
          GenerateCodewords;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.SetVerPixelsPerMM (const v : Extended);
var
  OldVerPixelsPerMM : Extended;

begin
  if v <> FVerPixelsPerMM then begin
    if (v = 0) and (BarHeight = 0) and (not AutoScale) then
      raise E2DBarcodeError.Create (StENeedVert);
    OldVerPixelsPerMM := FVerPixelsPerMM;
    try
      FVerPixelsPerMM := v;
      GetSizes;
      GenerateBarcodeBitmap (Width, Height);
      Invalidate;
    except
      on E2DBarcodeError do begin
        FVerPixelsPerMM := OldVerPixelsPerMM;
        try
          GetSizes;
          GenerateBarcodeBitmap (Width, Height);
          Invalidate;
        except
          on E2DBarcodeError do begin
          end;
        end;
        raise
      end;
    end;
  end;
end;

procedure TStMaxiCodeBarcode.TextToCodewords;


  function FindCodeSet (Value : Char) : TStMaxiCodeCodeSet;
  begin
    Result := csCodeSetA;
    while Result < csNone do begin
      if StMaxiCodeCodeSets[Result][Integer (Value)] <> -1 then
        Exit; 
      Inc (Result);
    end;
    Result := csNone;
  end;

  function ChangeCodeSet (CurrentMode   : TStMaxiCodeCodeSet;
                          NewMode       : TStMaxiCodeCodeSet;
                          Value         : Char;
                          UseShift      : Boolean;
                          UseTwoShift   : Boolean;
                          UseThreeShift : Boolean) : TStMaxiCodeCodeSet;
  const
    ShiftAB  = 59;
    ShiftAC  = 60;
    ShiftAD  = 61;
    ShiftAE  = 62;
    LatchAB  = 63;

    Shift2BA = 56;
    Shift3BA = 57;
    ShiftBA  = 59;
    ShiftBC  = 60;
    ShiftBD  = 61;
    ShiftBE  = 62;
    LatchBA  = 63;

    LatchCA  = 58;
    LockC    = 60;
    ShiftCD  = 61;
    ShiftCE  = 62;
    LatchCB  = 63;

    LatchDA  = 58;
    ShiftDC  = 60;
    LockD    = 61;
    ShiftDE  = 62;
    LatchDB  = 63;

    LatchEA  = 58;
    ShiftEC  = 60;
    ShiftED  = 61;
    LockE    = 62;
    LatchEB  = 63;

  begin
    if UseShift then
      Result := CurrentMode
    else
      Result := NewMode;
      
    case CurrentMode of
      csCodeSetA :
        case NewMode of
          csCodeSetB :
            { A -> B }
            if UseShift then
              AddCodeword (ShiftAB)
            else
              AddCodeword (LatchAB);
          csCodeSetC :
            { A -> C }
            begin
              AddCodeword (ShiftAC);
              if not UseShift then
                AddCodeword (LockC);
            end;
          csCodeSetD :
            { A -> D }
            begin
              AddCodeword (ShiftAD);
              if not UseShift then
                AddCodeword (LockD);
            end;
          csCodeSetE :
            { A -> E }
            begin
              AddCodeword (ShiftAE);
              if not UseShift then
                AddCodeword (LockE);
            end;
        end;

      csCodeSetB :
        case NewMode of
          csCodeSetA :
            { B -> A }
            if UseThreeShift then
              AddCodeword (Shift3BA)
            else if UseTwoShift then
              AddCodeword (Shift2BA)
            else if UseShift then
              AddCodeword (ShiftBA)
            else
              AddCodeword (LatchBA);
          csCodeSetC :
            { B -> C }
            begin
              AddCodeword (ShiftBC);
              if not UseShift then
                AddCodeword (LockC);
            end;
          csCodeSetD :
            { B -> D }
            begin
              AddCodeword (ShiftBD);
              if not UseShift then
                AddCodeword (LockD);
            end;
          csCodeSetE :
            { B -> E }
            begin
              AddCodeword (ShiftBE);
              if not UseShift then
                AddCodeword (LockE);
            end;
        end;

      csCodeSetC :
        case NewMode of
          csCodeSetA :
            { C -> A }
            begin
              AddCodeword (LatchCA);
              Result := NewMode;
            end;
          csCodeSetB :
            { C -> B }
            begin
              AddCodeword (LatchCB);
              Result := NewMode;
            end;
          csCodeSetD :
            { C -> D }
            begin
              AddCodeword (ShiftCD);
              if not UseShift then
                AddCodeword (LockD);
            end;
          csCodeSetE :
            { C -> E }
            begin
              AddCodeword (ShiftCE);
              if not UseShift then
                AddCodeword (LockE);
            end;
        end;

      csCodeSetD :
        case NewMode of
          csCodeSetA :
            { D -> A }
            begin
              AddCodeword (LatchDA);
              Result := NewMode;
            end;
          csCodeSetB :
            { D -> B }
            begin
              AddCodeword (LatchDB);
              Result := NewMode;
            end;
          csCodeSetC :
            { D -> C }
            begin
              AddCodeword (ShiftDC);
              if not UseShift then
                AddCodeword (LockC);
            end;
          csCodeSetE :
            { D -> E }
            begin
              AddCodeword (ShiftDE);
              if not UseShift then
                AddCodeword (LockE);
            end;
        end;

      csCodeSetE :
        case NewMode of
          csCodeSetA :
            { E -> A }
            begin
              AddCodeword (LatchEA);
              Result := NewMode;
            end;
          csCodeSetB :
            { E -> B }
            begin
              AddCodeword (LatchEB);
              Result := NewMode;
            end;
          csCodeSetC :
            { E -> C }
            begin
              AddCodeword (ShiftEC);
              if not UseShift then
                AddCodeword (LockC);
            end;
          csCodeSetD :
            { E -> D }
            begin
              AddCodeword (ShiftED);
              if not UseShift then
                AddCodeword (LockD);
            end;
        end;
    end;
  end;

  procedure GetMessageCodewords;
  var
    CodeLen     : Integer;
    CurrentMode : TStMaxiCodeCodeSet;
    UseShift    : Boolean;
    UseShift2   : Boolean;
    UseShift3   : Boolean;
    WorkMode    : TStMaxiCodeCodeSet;
    i           : Integer;
    Codeword    : Boolean;
    NewChar     : Integer;

  begin
    CodeLen := Length (Code);
    if CodeLen = 0 then begin
      for i := 0 to 144 do 
        AddCodeword (33);
      Exit;
    end;
    CurrentMode := csCodeSetA;
    i := 1;
    while i <= CodeLen do begin
      GetNextCharacter (NewChar, CodeWord, i, CodeLen);
      if CodeWord then
        AddCodeword (NewChar)
      else if StMaxiCodeCodeSets[CurrentMode][NewChar] = -1 then begin
        WorkMode := FindCodeSet (Char (NewChar));
        UseShift := False;
        UseShift2 := False;
        UseShift3 := False;
        if i < CodeLen then begin
          if StMaxiCodeCodeSets[CurrentMode][Integer (Code[i + 1])] <> -1 then
            UseShift := True;
        end;
        CurrentMode := ChangeCodeSet (CurrentMode, WorkMode, Char (NewChar),
                                      UseShift, UseShift2, UseShift3);
        AddCodeword (StMaxiCodeCodeSets[WorkMode][NewChar]);
      end else
        AddCodeword (StMaxiCodeCodeSets[CurrentMode][NewChar]);
    end;

    if (FNumCodewords > 68) and (FMode = cmMode5) then
      raise E2DBarcodeError.Create (StECodeTooLarge)
    else if FNumCodewords > 84 then
      raise E2DBarcodeError.Create (StECodeTooLarge);

    if CodeLen < 144 then begin
      if CurrentMode = csCodeSetC then begin
        AddCodeword (58);
        CurrentMode := csCodeSetA;
      end else if CurrentMode = csCodeSetD then begin
        AddCodeword (58);
        CurrentMode := csCodeSetA;
      end;
      for i := FNumCodewords to 144 do begin
        case CurrentMode of
          csCodeSetA :
            AddCodeword (33);
          csCodeSetB :                                    
            AddCodeword (33);
          csCodeSetE :
            AddCodeword (28);
        end;
      end;
    end;
  end;

  procedure MergeCodewords;
  begin
    case FMode of
      cmMode2 :
        System.Move (FMessage, FCodewords[20], 84);
      cmMode3 :
        System.Move (FMessage, FCodewords[20], 84);
      cmMode4 :
        begin
          System.Move (FMessage, FCodeWords[1], 9);
          System.Move (FMessage[9], FCodewords[20], 84);
        end;
      cmMode5 :
        begin
          System.Move (FMessage, FCodeWords[1], 9);
          System.Move (FMessage[9], FCodewords[20], 68);
        end;
      cmMode6 :
        System.Move (FMessage, FCodewords[20], 84);
    end;
  end;

  function IsNumericPostalCode : Boolean;
  var
    PostalLen : Integer;
    i         : Integer;
    
  begin
    Result := True;
    i := 1;
    PostalLen := Length (FCarrierPostalCode);
    while (i <= PostalLen) do
      if (FCarrierPostalCode[i] < '0') or
         (FCarrierPostalCode[i] > '9') then begin
        Result := False;
        i := PostalLen + 1;
      end else
        Inc (i);
  end;
  
  procedure EncodeCarrierInfo;

  { Encodation of the carrier information requires some fairly bizarre
    bit manipulation

     Codewords:-------->                                              111111
                111111222222333333444444555555666666777777888888999999000000 C
          012345012345012345012345012345012345012345012345012345012345012345 W
     num: ppMMMMppppppppppppppppppppppppllppppccllllccccccssssccssssssEEE...
      an: ppMMMMppppppppppppppppppppppppppppppccppppccccccssssccssssssEEE...
          123456789012345678901234567890123456789012345678901234567890123456 B
     Bits: ----->  111111111122222222223333333333444444444455555555556666666 i
                                                                             t
   MMMM = Mode
   pppp = Postal Code
   ll   = Postal Code Length
   cccc = Country Code
   ssss = Service Class
   EEEE = ECC Codes

   For pppp, ll, cccc and ssss, the MSB is on the right.
  }

  var
    WorkNum : Integer;
    WorkStr : string;
    i       : Integer;

  begin
    for WorkNum := 2 to 10 do 
      FCodewords[WorkNum] := 0;
    FCodewords[0] := FCodewords[0] and $0f;      
      
    if FCodewords[0] = $02 then begin
      { Format numeric postal code }
      { Format the postal code length }
      WorkNum := Length (FCarrierPostalCode);
      FCodewords[6] := (WorkNum and $3c) shr 2;
      FCodewords[5] := (WorkNum and $03) shl 4;
      { Format the postal code }
      try
        WorkNum := StrToInt (FCarrierPostalCode);
      except
        on EConvertError do
          raise E2DBarcodeError.Create (StEBadPostalCode);
      end;
      FCodewords[5] := FCodewords[5] or ((WorkNum shr 26) and $0f);
      FCodewords[4] := (WorkNum shr 20) and $3f;
      FCodewords[3] := (WorkNum shr 14) and $3f;
      FCodewords[2] := (WorkNum shr 8) and $3f;
      FCodewords[1] := (WorkNum shr 2) and $3f;
      FCodewords[0] := FCodewords[0] or ((WorkNum and $03) shl 4);
    end else begin
      { Format alphanumeric postal code }
      WorkStr := UpperCase (FCarrierPostalCode) + '      ';
      for i := 0 to 5 do begin
        WorkNum := StMaxiCodeCodeSets[csCodeSetA][Integer (WorkStr[6 - i])];
        if WorkNum < 0 then
          WorkNum := StMaxiCodeCodeSets[csCodeSetA][32]; { Use a space }
        FCodewords[i] := FCodewords[i] or ((WorkNum and $03) shl 4);
        FCodewords[i + 1] := FCodewords[i + 1] or ((WorkNum and $3c) shr 2);
      end;
    end;

    { Format country code }
    WorkNum := FCarrierCountryCode;
    FCodewords[8] := (WorkNum shr 8) and $03;
    FCodewords[7] := (WorkNum shr 2) and $3f;
    FCodewords[6] := FCodewords[6] or ((WorkNum and $03) shl 4);

    { Format service class }
    WorkNum := FCarrierServiceClass;
    FCodewords[9] := (WorkNum and $3f0) shr 4;
    FCodewords[8] := FCodewords[8] or ((WorkNum and $0f) shl 2);
  end;

var
  i : Integer;

begin
  for i := 0 to 144 do begin
    FCodewords[i] := 0;
    FMessage[i] := 0;
  end;

  FNumCodewords := 0;

  { Encode the primary message and set the FNumCodewords to the begining
    of the secondary message }

  case FMode of
    cmMode2, cmMode3 :
      if IsNumericPostalCode then
        FCodewords[0] := $02
      else
        FCodewords[0] := $03;
    cmMode4 :                
      FCodewords[0] := $04; 
    cmMode5 :
      FCodewords[0] := $05;
    cmMode6 :
      FCodewords[0] := $06;
  end;

  if (FMode = cmMode2) or (FMode = cmMode3) then
    EncodeCarrierInfo;

  GetMessageCodewords;
  MergeCodewords;
  GenerateECC;
  FNumCodewords := 144;

  FTotalCodewords := 144;
  if FMode = cmMode5 then
    FUsedECCCodewords := 66
  else
    FUsedECCCodewords := 50;
  case FMode of           
    cmMode2 :
      FUsedCodewords := FNumCodewords + 10;
    cmMode3 :
      FUsedCodewords := FNumCodewords + 10;
    cmMode4 :
      FUsedCodewords := FNumCodewords + 1;
    cmMode5 :
      FUsedCodewords := FNumCodewords + 1;
    cmMode6 :
      FUsedCodewords := FNumCodewords + 1;
  end;

  FFreeCodewords := FTotalCodewords - FUsedCodewords - FUsedECCCodewords;

end;

end.

