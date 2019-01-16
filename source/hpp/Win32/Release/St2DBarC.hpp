// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'St2DBarC.pas' rev: 33.00 (Windows)

#ifndef St2dbarcHPP
#define St2dbarcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.Math.hpp>
#include <Vcl.Clipbrd.hpp>
#include <StConst.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace St2dbarc
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS E2DBarcodeError;
class DELPHICLASS TStCustom2DBarcode;
class DELPHICLASS TStPDF417Barcode;
class DELPHICLASS TStMaxiCodeBarcode;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStDataMode : unsigned char { dmBinary, dmText, dmNumeric };

typedef System::StaticArray<System::Word, 2701> TStPDF417CodewordList;

enum DECLSPEC_DENUM TStPDF417ECCLevels : unsigned char { ecAuto, ecLevel0, ecLevel1, ecLevel2, ecLevel3, ecLevel4, ecLevel5, ecLevel6, ecLevel7, ecLevel8 };

enum DECLSPEC_DENUM TStMaxiCodeMode : unsigned char { cmMode2, cmMode3, cmMode4, cmMode5, cmMode6 };

typedef System::StaticArray<System::Byte, 145> TStMaxiCodeECCData;

enum DECLSPEC_DENUM TStMaxiCodeECCPoly : unsigned char { epPrimary, epStandard, epEnhanced };

enum DECLSPEC_DENUM TStMaxiCodeECCInterleave : unsigned char { imNone, imEven, imOdd };

#pragma pack(push,4)
class PASCALIMPLEMENTATION E2DBarcodeError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall E2DBarcodeError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall E2DBarcodeError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall E2DBarcodeError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall E2DBarcodeError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall E2DBarcodeError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall E2DBarcodeError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall E2DBarcodeError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall E2DBarcodeError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall E2DBarcodeError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall E2DBarcodeError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall E2DBarcodeError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall E2DBarcodeError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~E2DBarcodeError() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TStCustom2DBarcode : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
protected:
	System::UnicodeString FCode;
	int FBarWidth;
	System::Uitypes::TColor FBackgroundColor;
	System::UnicodeString FCaption;
	int FECCLevel;
	bool FExtendedSyntax;
	bool FRelativeBarHeight;
	int FBarHeightToWidth;
	int FBarHeight;
	int FQuietZone;
	System::Classes::TAlignment FAlignment;
	Vcl::Stdctrls::TTextLayout FCaptionLayout;
	System::Types::TRect FBarCodeRect;
	int FUsedCodewords;
	int FFreeCodewords;
	int FUsedECCCodewords;
	int FTotalCodewords;
	Vcl::Graphics::TBitmap* FBitmap;
	virtual int __fastcall CalculateBarCodeWidth(int PaintableWidth) = 0 ;
	virtual int __fastcall CalculateBarCodeHeight(int PaintableHeight) = 0 ;
	virtual void __fastcall DrawBarcode() = 0 ;
	void __fastcall GenerateBarcodeBitmap(int BCWidth, int BCHeight);
	virtual void __fastcall GenerateCodewords() = 0 ;
	int __fastcall GetBarCodeHeight();
	int __fastcall GetBarCodeWidth();
	void __fastcall GetCurrentResolution(int &ResX, int &ResY);
	System::UnicodeString __fastcall GetVersion();
	virtual void __fastcall Paint();
	void __fastcall SetAlignment(const System::Classes::TAlignment v);
	void __fastcall SetBackgroundColor(const System::Uitypes::TColor v);
	virtual void __fastcall SetBarHeight(const int v);
	virtual void __fastcall SetBarHeightToWidth(const int v);
	virtual void __fastcall SetBarWidth(const int v);
	void __fastcall SetBitmap(Vcl::Graphics::TBitmap* const v);
	void __fastcall SetCaption(const System::UnicodeString v);
	void __fastcall SetCaptionLayout(const Vcl::Stdctrls::TTextLayout v);
	void __fastcall SetCode(const System::UnicodeString v);
	void __fastcall SetECCLevel(const int v);
	void __fastcall SetExtendedSyntax(const bool v);
	virtual void __fastcall SetRelativeBarHeight(const bool v);
	void __fastcall SetQuietZone(const int v);
	void __fastcall SetVersion(const System::UnicodeString Value);
	
public:
	__fastcall virtual TStCustom2DBarcode(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStCustom2DBarcode();
	void __fastcall CopyToClipboard();
	void __fastcall CopyToClipboardRes(int ResX, int ResY);
	void __fastcall PaintToCanvas(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TPoint &Position);
	void __fastcall PaintToCanvasRes(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TPoint &Position, int ResX, int ResY);
	void __fastcall PaintToCanvasSize(Vcl::Graphics::TCanvas* ACanvas, double X, double Y, double H);
	void __fastcall PaintToDC(HDC DC, const System::Types::TPoint &Position);
	void __fastcall PaintToDCRes(HDC DC, const System::Types::TPoint &Position, int ResX, int ResY);
	void __fastcall PaintToPrinterCanvas(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TPoint &Position);
	void __fastcall PaintToPrinterCanvasRes(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TPoint &Position, int ResX, int ResY);
	void __fastcall PaintToPrinterCanvasSize(Vcl::Graphics::TCanvas* ACanvas, double X, double Y, double H);
	void __fastcall PaintToPrinterDC(HDC DC, const System::Types::TPoint &Position);
	void __fastcall PaintToPrinterDCRes(HDC DC, const System::Types::TPoint &Position, int ResX, int ResY);
	virtual void __fastcall RenderToResolution(Vcl::Graphics::TBitmap* &OutBitmap, int ResX, int ResY, int &SizeX, int &SizeY) = 0 ;
	void __fastcall SaveToFile(const System::UnicodeString FileName);
	void __fastcall SaveToFileRes(const System::UnicodeString FileName, int ResX, int ResY);
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=2};
	__property System::Uitypes::TColor BackgroundColor = {read=FBackgroundColor, write=SetBackgroundColor, default=16777215};
	__property int BarCodeHeight = {read=GetBarCodeHeight, nodefault};
	__property System::Types::TRect BarCodeRect = {read=FBarCodeRect};
	__property int BarCodeWidth = {read=GetBarCodeWidth, nodefault};
	__property int BarHeight = {read=FBarHeight, write=SetBarHeight, default=2};
	__property int BarHeightToWidth = {read=FBarHeightToWidth, write=SetBarHeightToWidth, default=4};
	__property int BarWidth = {read=FBarWidth, write=SetBarWidth, default=2};
	__property Vcl::Graphics::TBitmap* Bitmap = {read=FBitmap, write=SetBitmap, stored=false};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property Vcl::Stdctrls::TTextLayout CaptionLayout = {read=FCaptionLayout, write=SetCaptionLayout, default=2};
	__property System::UnicodeString Code = {read=FCode, write=SetCode};
	__property int ECCLevel = {read=FECCLevel, write=SetECCLevel, default=0};
	__property bool ExtendedSyntax = {read=FExtendedSyntax, write=SetExtendedSyntax, default=1};
	__property int FreeCodewords = {read=FFreeCodewords, nodefault};
	__property bool RelativeBarHeight = {read=FRelativeBarHeight, write=SetRelativeBarHeight, default=0};
	__property int QuietZone = {read=FQuietZone, write=SetQuietZone, default=8};
	__property int TotalCodewords = {read=FTotalCodewords, nodefault};
	__property int UsedCodewords = {read=FUsedCodewords, nodefault};
	__property int UsedECCCodewords = {read=FUsedECCCodewords, nodefault};
	__property Color = {default=0};
	
__published:
	__property System::UnicodeString Version = {read=GetVersion, write=SetVersion, stored=false};
	__property Align = {default=0};
	__property Cursor = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
};


class PASCALIMPLEMENTATION TStPDF417Barcode : public TStCustom2DBarcode
{
	typedef TStCustom2DBarcode inherited;
	
private:
	bool FTruncated;
	TStPDF417CodewordList FCodewords;
	int FNumCodewords;
	bool FNewTextCodeword;
	bool FHighlight;
	int FNumRows;
	int FNumColumns;
	
protected:
	void __fastcall AddCodeword(System::Word Value);
	virtual int __fastcall CalculateBarCodeWidth(int PaintableWidth);
	virtual int __fastcall CalculateBarCodeHeight(int PaintableHeight);
	void __fastcall CalculateECC(int NumCodewords, int ECCLen);
	void __fastcall CalculateSize(int &XSize, int &YSize);
	unsigned __fastcall CodewordToBitmask(int RowNumber, int Codeword);
	void __fastcall ConvertBytesToBase900(const System::Byte *S, const int S_High, int *A, const int A_High);
	void __fastcall ConvertToBase900(const System::UnicodeString S, int *A, const int A_High, int &LenA);
	virtual void __fastcall DrawBarcode();
	void __fastcall DrawCodeword(int RowNumber, int ColNumber, int WorkBarHeight, System::UnicodeString Pattern);
	void __fastcall DrawCodewordBitmask(int RowNumber, int ColNumber, int WorkBarHeight, unsigned Bitmask);
	void __fastcall DrawLeftRowIndicator(int RowNumber, int WorkBarHeight, int NumRows, int NumCols);
	void __fastcall DrawRightRowIndicator(int RowNumber, int ColNumber, int WorkBarHeight, int NumRows, int NumCols);
	void __fastcall DrawStartPattern(int RowNumber, int WorkBarHeight);
	void __fastcall DrawStopPattern(int RowNumber, int ColNumber, int WorkBarHeight);
	void __fastcall EncodeBinary(int &Position, int CodeLen);
	void __fastcall EncodeNumeric(int &Position, int CodeLen);
	void __fastcall EncodeText(int &Position, int CodeLen);
	virtual void __fastcall GenerateCodewords();
	void __fastcall GetNextCharacter(int &NewChar, bool &Codeword, int &Position, int CodeLen);
	TStPDF417ECCLevels __fastcall GetPDF417ECCLevel();
	int __fastcall GetRealErrorLevel();
	bool __fastcall GoodForNumericCompaction(int Position, int CodeLen, int &Count);
	bool __fastcall GoodForTextCompaction(int Position, int CodeLen, int &Count);
	bool __fastcall IsNumericString(const System::UnicodeString S);
	virtual void __fastcall SetBarHeight(const int v);
	virtual void __fastcall SetBarHeightToWidth(const int v);
	virtual void __fastcall SetBarWidth(const int v);
	void __fastcall SetNumColumns(const int v);
	void __fastcall SetNumRows(const int v);
	void __fastcall SetPDF417ECCLevel(const TStPDF417ECCLevels v);
	virtual void __fastcall SetRelativeBarHeight(const bool v);
	void __fastcall SetTruncated(const bool v);
	void __fastcall TextToCodewords();
	
public:
	__fastcall virtual TStPDF417Barcode(System::Classes::TComponent* AOwner);
	virtual void __fastcall RenderToResolution(Vcl::Graphics::TBitmap* &OutBitmap, int ResX, int ResY, int &SizeX, int &SizeY);
	
__published:
	__property TStPDF417ECCLevels ECCLevel = {read=GetPDF417ECCLevel, write=SetPDF417ECCLevel, default=0};
	__property int NumColumns = {read=FNumColumns, write=SetNumColumns, default=0};
	__property int NumRows = {read=FNumRows, write=SetNumRows, default=0};
	__property bool Truncated = {read=FTruncated, write=SetTruncated, default=0};
	__property Alignment = {default=2};
	__property BackgroundColor = {default=16777215};
	__property BarCodeHeight;
	__property BarCodeWidth;
	__property BarHeight = {default=2};
	__property BarHeightToWidth = {default=4};
	__property BarWidth = {default=2};
	__property Bitmap;
	__property CaptionLayout = {default=2};
	__property Code = {default=0};
	__property ExtendedSyntax = {default=1};
	__property Height = {default=81};
	__property RelativeBarHeight = {default=0};
	__property QuietZone = {default=8};
	__property Width = {default=273};
	__property Caption;
	__property Color = {default=0};
	__property Font;
public:
	/* TStCustom2DBarcode.Destroy */ inline __fastcall virtual ~TStPDF417Barcode() { }
	
};


class PASCALIMPLEMENTATION TStMaxiCodeBarcode : public TStCustom2DBarcode
{
	typedef TStCustom2DBarcode inherited;
	
private:
	TStMaxiCodeMode FMode;
	TStMaxiCodeECCData FCodewords;
	int FNumCodewords;
	bool FHighlight;
	bool FShowCodewords;
	bool FShowAll;
	TStMaxiCodeECCData FMessage;
	int FCarrierCountryCode;
	System::UnicodeString FCarrierPostalCode;
	int FCarrierServiceClass;
	bool FAutoScale;
	System::Extended FHorPixelsPerMM;
	System::Extended FVerPixelsPerMM;
	System::Extended FMaxiHexWidth;
	System::Extended FMaxiHexHeight;
	System::Extended FMaxiHexVOffset;
	System::Extended FMaxiHexHOffset;
	System::StaticArray<int, 65> FLog;
	System::StaticArray<int, 65> FAntiLog;
	
protected:
	void __fastcall AddCodeword(int Value);
	virtual int __fastcall CalculateBarCodeWidth(int PaintableWidth);
	virtual int __fastcall CalculateBarCodeHeight(int PaintableHeight);
	virtual void __fastcall DrawBarcode();
	void __fastcall DrawFinder();
	void __fastcall DrawHex(int XPos, int YPos);
	virtual void __fastcall GenerateCodewords();
	void __fastcall GenerateECC();
	void __fastcall GetNextCharacter(int &NewChar, bool &Codeword, int &Position, int CodeLen);
	void __fastcall GetSizes();
	void __fastcall GetSizesEx(int ResX, int ResY);
	void __fastcall PlotCell(int Row, int Col);
	void __fastcall SetAutoScale(const bool v);
	virtual void __fastcall SetBarHeight(const int v);
	virtual void __fastcall SetBarWidth(const int v);
	void __fastcall SetCarrierCountryCode(const int v);
	void __fastcall SetCarrierPostalCode(const System::UnicodeString v);
	void __fastcall SetCarrierServiceClass(const int v);
	void __fastcall SetMode(const TStMaxiCodeMode v);
	void __fastcall SetHorPixelsPerMM(const System::Extended v);
	void __fastcall SetVerPixelsPerMM(const System::Extended v);
	void __fastcall TextToCodewords();
	
public:
	__fastcall virtual TStMaxiCodeBarcode(System::Classes::TComponent* AOwner);
	virtual void __fastcall RenderToResolution(Vcl::Graphics::TBitmap* &OutBitmap, int ResX, int ResY, int &SizeX, int &SizeY);
	
__published:
	__property bool AutoScale = {read=FAutoScale, write=SetAutoScale, default=1};
	__property int CarrierCountryCode = {read=FCarrierCountryCode, write=SetCarrierCountryCode, default=0};
	__property System::UnicodeString CarrierPostalCode = {read=FCarrierPostalCode, write=SetCarrierPostalCode};
	__property int CarrierServiceClass = {read=FCarrierServiceClass, write=SetCarrierServiceClass, default=0};
	__property System::Extended HorPixelsPerMM = {read=FHorPixelsPerMM, write=SetHorPixelsPerMM};
	__property TStMaxiCodeMode Mode = {read=FMode, write=SetMode, default=2};
	__property System::Extended VerPixelsPerMM = {read=FVerPixelsPerMM, write=SetVerPixelsPerMM};
	__property Alignment = {default=2};
	__property BackgroundColor = {default=16777215};
	__property BarCodeHeight;
	__property BarCodeWidth;
	__property BarHeight = {default=0};
	__property BarWidth = {default=0};
	__property Bitmap;
	__property CaptionLayout = {default=2};
	__property Code = {default=0};
	__property ExtendedSyntax = {default=1};
	__property Height = {default=129};
	__property QuietZone = {default=8};
	__property Width = {default=121};
	__property Caption;
	__property Color = {default=0};
	__property Font;
public:
	/* TStCustom2DBarcode.Destroy */ inline __fastcall virtual ~TStMaxiCodeBarcode() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _StEBadBarHeight;
#define St2dbarc_StEBadBarHeight System::LoadResourceString(&St2dbarc::_StEBadBarHeight)
extern DELPHI_PACKAGE System::ResourceString _StEBadBarHeightToWidth;
#define St2dbarc_StEBadBarHeightToWidth System::LoadResourceString(&St2dbarc::_StEBadBarHeightToWidth)
extern DELPHI_PACKAGE System::ResourceString _StEBadBarWidth;
#define St2dbarc_StEBadBarWidth System::LoadResourceString(&St2dbarc::_StEBadBarWidth)
extern DELPHI_PACKAGE System::ResourceString _StEBadCountryCode;
#define St2dbarc_StEBadCountryCode System::LoadResourceString(&St2dbarc::_StEBadCountryCode)
extern DELPHI_PACKAGE System::ResourceString _StEBadNumCols;
#define St2dbarc_StEBadNumCols System::LoadResourceString(&St2dbarc::_StEBadNumCols)
extern DELPHI_PACKAGE System::ResourceString _StEBadNumRows;
#define St2dbarc_StEBadNumRows System::LoadResourceString(&St2dbarc::_StEBadNumRows)
extern DELPHI_PACKAGE System::ResourceString _StEBadPostalCode;
#define St2dbarc_StEBadPostalCode System::LoadResourceString(&St2dbarc::_StEBadPostalCode)
extern DELPHI_PACKAGE System::ResourceString _StEBadServiceClass;
#define St2dbarc_StEBadServiceClass System::LoadResourceString(&St2dbarc::_StEBadServiceClass)
extern DELPHI_PACKAGE System::ResourceString _StEBadQuietZone;
#define St2dbarc_StEBadQuietZone System::LoadResourceString(&St2dbarc::_StEBadQuietZone)
extern DELPHI_PACKAGE System::ResourceString _StECodeTooLarge;
#define St2dbarc_StECodeTooLarge System::LoadResourceString(&St2dbarc::_StECodeTooLarge)
extern DELPHI_PACKAGE System::ResourceString _StEGLIOutOfRange;
#define St2dbarc_StEGLIOutOfRange System::LoadResourceString(&St2dbarc::_StEGLIOutOfRange)
extern DELPHI_PACKAGE System::ResourceString _StEInvalidCodeword;
#define St2dbarc_StEInvalidCodeword System::LoadResourceString(&St2dbarc::_StEInvalidCodeword)
extern DELPHI_PACKAGE System::ResourceString _StENeedBarHeight;
#define St2dbarc_StENeedBarHeight System::LoadResourceString(&St2dbarc::_StENeedBarHeight)
extern DELPHI_PACKAGE System::ResourceString _StENeedHorz;
#define St2dbarc_StENeedHorz System::LoadResourceString(&St2dbarc::_StENeedHorz)
extern DELPHI_PACKAGE System::ResourceString _StENeedVert;
#define St2dbarc_StENeedVert System::LoadResourceString(&St2dbarc::_StENeedVert)
static const System::Int8 StMaxiCodeGaloisField = System::Int8(0x40);
static const System::Int8 StMaxiCodeECCPoly = System::Int8(0x43);
static const System::Byte StMaxMaxiCodeECCDataSize = System::Byte(0x90);
}	/* namespace St2dbarc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ST2DBARC)
using namespace St2dbarc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// St2dbarcHPP
