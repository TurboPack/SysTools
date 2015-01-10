// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StDbBarC.pas' rev: 29.00 (Windows)

#ifndef StdbbarcHPP
#define StdbbarcHPP

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
#include <StBarC.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stdbbarc
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStDbBarCode;
class PASCALIMPLEMENTATION TStDbBarCode : public Stbarc::TStBarCode
{
	typedef Stbarc::TStBarCode inherited;
	
protected:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	void __fastcall DataChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetDataField(void);
	Data::Db::TDataSource* __fastcall GetDataSource(void);
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	
public:
	__fastcall virtual TStDbBarCode(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStDbBarCode(void);
	
__published:
	__property Code = {stored=false, default=0};
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stdbbarc */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STDBBARC)
using namespace Stdbbarc;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StdbbarcHPP
