// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDbPNBC.pas' rev: 28.00 (Windows)

#ifndef StdbpnbcHPP
#define StdbpnbcHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <Vcl.Clipbrd.hpp>	// Pascal unit
#include <Vcl.Controls.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <Winapi.Messages.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <Data.DB.hpp>	// Pascal unit
#include <Vcl.DBCtrls.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit
#include <StBarPN.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stdbpnbc
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStDbPNBarCode;
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
