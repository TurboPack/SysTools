// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDbPNBC.pas' rev: 31.00 (Windows)

#ifndef StdbpnbcHPP
#define StdbpnbcHPP

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
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <StConst.hpp>
#include <StBarPN.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stdbpnbc
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStDbPNBarCode;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStDbPNBarCode : public Stbarpn::TStPNBarCode
{
	typedef Stbarpn::TStPNBarCode inherited;
	
protected:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	void __fastcall DataChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetDataField(void);
	Data::Db::TDataSource* __fastcall GetDataSource(void);
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	
public:
	__fastcall virtual TStDbPNBarCode(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStDbPNBarCode(void);
	
__published:
	__property PostalCode = {stored=false, default=0};
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stdbpnbc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STDBPNBC)
using namespace Stdbpnbc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StdbpnbcHPP
