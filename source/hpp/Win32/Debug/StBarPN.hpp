// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StBarPN.pas' rev: 32.00 (Windows)

#ifndef StbarpnHPP
#define StbarpnHPP

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
#include <StStrL.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stbarpn
{
//-- forward type declarations -----------------------------------------------
struct TStPNBarCodeDims;
struct TStPNBarCodeRes;
class DELPHICLASS TStPNBarCode;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TStPNBarCodeDims
{
public:
	int PixPerBar;
	int PixPerSpace;
	int ShortBarHeight;
	int TallBarHeight;
	int Width;
	int Height;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TStPNBarCodeRes
{
public:
	int XRes;
	int YRes;
};
#pragma pack(pop)


class PASCALIMPLEMENTATION TStPNBarCode : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
protected:
	System::UnicodeString FPostalCode;
	int FCheckNumber;
	TStPNBarCodeDims pnbcDisplayDims;
	TStPNBarCodeRes pnbcDefRes;
	System::UnicodeString __fastcall GetVersion(void);
	void __fastcall SetPostalCode(System::UnicodeString Value);
	void __fastcall SetVersion(const System::UnicodeString v);
	int __fastcall DrawTallBar(Vcl::Graphics::TCanvas* C, const TStPNBarCodeDims &Dims, int XPos, bool AddSpace);
	int __fastcall DrawShortBar(Vcl::Graphics::TCanvas* C, const TStPNBarCodeDims &Dims, int XPos, bool AddSpace);
	int __fastcall DrawNumber(Vcl::Graphics::TCanvas* C, const TStPNBarCodeDims &Dims, int Value, int XPos, bool FrontGuard, bool EndGuard);
	void __fastcall DrawBarCode(Vcl::Graphics::TCanvas* C, const TStPNBarCodeDims &Dims);
	void __fastcall SetCheckNumber(void);
	virtual void __fastcall Loaded(void);
	virtual void __fastcall Paint(void);
	
public:
	__fastcall virtual TStPNBarCode(System::Classes::TComponent* AOwner);
	void __fastcall ComputeSizes(Vcl::Graphics::TCanvas* C, const TStPNBarCodeRes &Res, TStPNBarCodeDims &Dims);
	void __fastcall CopyToClipboard(void);
	void __fastcall PaintToCanvas(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TPoint &Position);
	void __fastcall PaintToDC(HDC DC, const System::Types::TPoint &Position);
	void __fastcall PaintToPrinterCanvas(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TPoint &Position);
	void __fastcall PaintToPrinterDC(HDC DC, const System::Types::TPoint &Position);
	void __fastcall SaveToFile(Vcl::Graphics::TCanvas* ACanvas, const System::UnicodeString FileName);
	void __fastcall SaveToFileRes(const TStPNBarCodeRes &Res, const System::UnicodeString FileName);
	
__published:
	__property Cursor = {default=0};
	__property Enabled = {default=1};
	__property Hint = {default=0};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property System::UnicodeString PostalCode = {read=FPostalCode, write=SetPostalCode};
	__property System::UnicodeString Version = {read=GetVersion, write=SetVersion, stored=false};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TGraphicControl.Destroy */ inline __fastcall virtual ~TStPNBarCode(void) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stbarpn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STBARPN)
using namespace Stbarpn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StbarpnHPP
