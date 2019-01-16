// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDb2DBC.pas' rev: 33.00 (Windows)

#ifndef Stdb2dbcHPP
#define Stdb2dbcHPP

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
#include <St2DBarC.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stdb2dbc
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStDbPDF417Barcode;
class DELPHICLASS TStDbMaxiCodeBarcode;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStDbPDF417Barcode : public St2dbarc::TStPDF417Barcode
{
	typedef St2dbarc::TStPDF417Barcode inherited;
	
protected:
	Vcl::Dbctrls::TFieldDataLink* FCaptionDataLink;
	Vcl::Dbctrls::TFieldDataLink* FCodeDataLink;
	void __fastcall CaptionDataChange(System::TObject* Sender);
	void __fastcall CodeDataChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetCaptionDataField();
	System::UnicodeString __fastcall GetCodeDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	void __fastcall SetCaptionDataField(const System::UnicodeString Value);
	void __fastcall SetCodeDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	
public:
	__fastcall virtual TStDbPDF417Barcode(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStDbPDF417Barcode();
	
__published:
	__property Code = {stored=false, default=0};
	__property Caption = {stored=false};
	__property System::UnicodeString CaptionDataField = {read=GetCaptionDataField, write=SetCaptionDataField};
	__property System::UnicodeString CodeDataField = {read=GetCodeDataField, write=SetCodeDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
};


class PASCALIMPLEMENTATION TStDbMaxiCodeBarcode : public St2dbarc::TStMaxiCodeBarcode
{
	typedef St2dbarc::TStMaxiCodeBarcode inherited;
	
protected:
	Vcl::Dbctrls::TFieldDataLink* FCaptionDataLink;
	Vcl::Dbctrls::TFieldDataLink* FCodeDataLink;
	Vcl::Dbctrls::TFieldDataLink* FCountryCodeDataLink;
	Vcl::Dbctrls::TFieldDataLink* FPostalCodeDataLink;
	Vcl::Dbctrls::TFieldDataLink* FServiceClassDataLink;
	void __fastcall CaptionDataChange(System::TObject* Sender);
	void __fastcall CodeDataChange(System::TObject* Sender);
	void __fastcall CountryCodeChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetCaptionDataField();
	System::UnicodeString __fastcall GetCodeDataField();
	System::UnicodeString __fastcall GetCountryCodeDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	System::UnicodeString __fastcall GetPostalCodeDataField();
	System::UnicodeString __fastcall GetServiceClassDataField();
	void __fastcall PostalCodeChange(System::TObject* Sender);
	void __fastcall ServiceClassChange(System::TObject* Sender);
	void __fastcall SetCaptionDataField(const System::UnicodeString Value);
	void __fastcall SetCodeDataField(const System::UnicodeString Value);
	void __fastcall SetCountryCodeDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetPostalCodeDataField(const System::UnicodeString Value);
	void __fastcall SetServiceClassDataField(const System::UnicodeString Value);
	
public:
	__fastcall virtual TStDbMaxiCodeBarcode(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStDbMaxiCodeBarcode();
	
__published:
	__property Code = {stored=false, default=0};
	__property Caption = {stored=false};
	__property System::UnicodeString CaptionDataField = {read=GetCaptionDataField, write=SetCaptionDataField};
	__property System::UnicodeString CarrierCountryCodeDataField = {read=GetCountryCodeDataField, write=SetCountryCodeDataField};
	__property System::UnicodeString CarrierPostalCodeDataField = {read=GetPostalCodeDataField, write=SetPostalCodeDataField};
	__property System::UnicodeString CarrierServiceClassDataField = {read=GetServiceClassDataField, write=SetServiceClassDataField};
	__property System::UnicodeString CodeDataField = {read=GetCodeDataField, write=SetCodeDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stdb2dbc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STDB2DBC)
using namespace Stdb2dbc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Stdb2dbcHPP
