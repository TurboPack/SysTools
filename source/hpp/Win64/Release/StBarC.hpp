// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StBarC.pas' rev: 32.00 (Windows)

#ifndef StbarcHPP
#define StbarcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Clipbrd.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <StBase.hpp>
#include <StConst.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stbarc
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStBarData;
class DELPHICLASS TStBarCodeInfo;
class DELPHICLASS TStBarCode;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStBarKind : unsigned char { bkSpace, bkBar, bkThreeQuarterBar, bkHalfBar, bkGuard, bkSupplement, bkBlankSpace };

typedef System::Set<TStBarKind, TStBarKind::bkSpace, TStBarKind::bkBlankSpace> TStBarKindSet;

typedef System::StaticArray<System::Byte, 255> TStDigitArray;

class PASCALIMPLEMENTATION TStBarData : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TStBarKindSet FKind;
	int FModules;
	__property TStBarKindSet Kind = {read=FKind, write=FKind, nodefault};
	__property int Modules = {read=FModules, write=FModules, nodefault};
public:
	/* TObject.Create */ inline __fastcall TStBarData(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStBarData(void) { }
	
};


class PASCALIMPLEMENTATION TStBarCodeInfo : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TStBarData* operator[](int Index) { return this->Bars[Index]; }
	
private:
	System::Classes::TList* FBars;
	TStBarData* __fastcall GetBars(int Index);
	int __fastcall GetCount(void);
	
public:
	__fastcall virtual TStBarCodeInfo(void);
	__fastcall virtual ~TStBarCodeInfo(void);
	void __fastcall Add(int ModuleCount, TStBarKindSet BarKind);
	void __fastcall Clear(void);
	__property TStBarData* Bars[int Index] = {read=GetBars/*, default*/};
	__property int Count = {read=GetCount, nodefault};
};


enum DECLSPEC_DENUM TStBarCodeType : unsigned char { bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13, bcInterleaved2of5, bcCodabar, bcCode11, bcCode39, bcCode93, bcCode128 };

enum DECLSPEC_DENUM TStCode128CodeSubset : unsigned char { csCodeA, csCodeB, csCodeC };

class PASCALIMPLEMENTATION TStBarCode : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
protected:
	bool FAddCheckChar;
	TStBarCodeType FBarCodeType;
	System::Uitypes::TColor FBarColor;
	double FBarToSpaceRatio;
	int FBarNarrowToWideRatio;
	double FBarWidth;
	TStCode128CodeSubset FCode128Subset;
	bool FBearerBars;
	bool FShowCode;
	bool FShowGuardChars;
	System::UnicodeString FSupplementalCode;
	bool FTallGuardBars;
	bool FExtendedSyntax;
	TStBarCodeInfo* bcBarInfo;
	int bcBarModWidth;
	int bcCheckK;
	TStDigitArray bcDigits;
	int bcDigitCount;
	int bcSpaceModWidth;
	int bcNormalWidth;
	int bcSpaceWidth;
	int bcSupplementWidth;
	System::UnicodeString __fastcall GetCode(void);
	System::UnicodeString __fastcall GetVersion(void);
	void __fastcall SetAddCheckChar(bool Value);
	void __fastcall SetBarCodeType(TStBarCodeType Value);
	void __fastcall SetBarColor(System::Uitypes::TColor Value);
	void __fastcall SetBarToSpaceRatio(double Value);
	void __fastcall SetBarNarrowToWideRatio(int Value);
	void __fastcall SetBarWidth(double Value);
	void __fastcall SetBearerBars(bool Value);
	void __fastcall SetCode(const System::UnicodeString Value);
	void __fastcall SetCode128Subset(TStCode128CodeSubset Value);
	void __fastcall SetExtendedSyntax(const bool v);
	void __fastcall SetShowCode(bool Value);
	void __fastcall SetShowGuardChars(bool Value);
	void __fastcall SetSupplementalCode(const System::UnicodeString Value);
	void __fastcall SetTallGuardBars(bool Value);
	void __fastcall SetVersion(const System::UnicodeString Value);
	void __fastcall CalcBarCode(void);
	void __fastcall CalcBarCodeWidth(void);
	int __fastcall DrawBar(int XPos, int YPos, int AWidth, int AHeight);
	void __fastcall DrawBarCode(const System::Types::TRect &R);
	int __fastcall GetDigits(System::UnicodeString Characters);
	void __fastcall PaintPrim(const System::Types::TRect &R);
	double __fastcall SmallestLineWidth(int PixelsPerInch);
	MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Msg);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TStBarCode(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStBarCode(void);
	void __fastcall CopyToClipboard(void);
	void __fastcall GetCheckCharacters(const System::UnicodeString S, int &C, int &K);
	double __fastcall GetBarCodeWidth(Vcl::Graphics::TCanvas* ACanvas);
	void __fastcall PaintToCanvas(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect);
	void __fastcall PaintToCanvasSize(Vcl::Graphics::TCanvas* ACanvas, double X, double Y, double H);
	void __fastcall PaintToDC(HDC DC, const System::Types::TRect &ARect);
	void __fastcall PaintToDCSize(HDC DC, double X, double Y, double W, double H);
	void __fastcall SaveToFile(const System::UnicodeString FileName);
	bool __fastcall Validate(bool DisplayError);
	
__published:
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property Cursor = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property bool AddCheckChar = {read=FAddCheckChar, write=SetAddCheckChar, nodefault};
	__property TStBarCodeType BarCodeType = {read=FBarCodeType, write=SetBarCodeType, nodefault};
	__property System::Uitypes::TColor BarColor = {read=FBarColor, write=SetBarColor, nodefault};
	__property double BarToSpaceRatio = {read=FBarToSpaceRatio, write=SetBarToSpaceRatio};
	__property int BarNarrowToWideRatio = {read=FBarNarrowToWideRatio, write=SetBarNarrowToWideRatio, default=2};
	__property double BarWidth = {read=FBarWidth, write=SetBarWidth};
	__property bool BearerBars = {read=FBearerBars, write=SetBearerBars, nodefault};
	__property System::UnicodeString Code = {read=GetCode, write=SetCode};
	__property TStCode128CodeSubset Code128Subset = {read=FCode128Subset, write=SetCode128Subset, nodefault};
	__property bool ExtendedSyntax = {read=FExtendedSyntax, write=SetExtendedSyntax, default=0};
	__property bool ShowCode = {read=FShowCode, write=SetShowCode, nodefault};
	__property bool ShowGuardChars = {read=FShowGuardChars, write=SetShowGuardChars, nodefault};
	__property System::UnicodeString SupplementalCode = {read=FSupplementalCode, write=SetSupplementalCode};
	__property bool TallGuardBars = {read=FTallGuardBars, write=SetTallGuardBars, nodefault};
	__property System::UnicodeString Version = {read=GetVersion, write=SetVersion, stored=false};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
};


//-- var, const, procedure ---------------------------------------------------
static const System::Byte bcMaxBarCodeLen = System::Byte(0xff);
static const bool bcGuardBarAbove = true;
static const bool bcGuardBarBelow = true;
static const System::Int8 bcDefNarrowToWideRatio = System::Int8(0x2);
}	/* namespace Stbarc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STBARC)
using namespace Stbarc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StbarcHPP
