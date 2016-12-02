// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StExport.pas' rev: 31.00 (Windows)

#ifndef StexportHPP
#define StexportHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Data.DB.hpp>
#include <Data.DBConsts.hpp>
#include <StBase.hpp>
#include <StTxtDat.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stexport
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStDBtoCSVExport;
class DELPHICLASS TStDbSchemaGenerator;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TStExportProgressEvent)(System::TObject* Sender, int Index, bool &Abort);

class PASCALIMPLEMENTATION TStDBtoCSVExport : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Data::Db::TDataSet* FDataSet;
	System::WideChar FFieldDelimiter;
	bool FIncludeHeader;
	char FLineTermChar;
	Stbase::TStLineTerminator FLineTerminator;
	bool FQuoteAlways;
	System::WideChar FQuoteDelimiter;
	bool FQuoteIfSpaces;
	System::UnicodeString FDateFmt;
	System::UnicodeString FTimeFmt;
	System::UnicodeString FDateTimeFmt;
	TStExportProgressEvent FOnExportProgress;
	Sttxtdat::TStOnQuoteFieldEvent FOnQuoteField;
	
protected:
	System::UnicodeString __fastcall BuildCSVHeader(void);
	System::UnicodeString __fastcall BuildCSVRec(void);
	void __fastcall SetDataSet(Data::Db::TDataSet* const Value);
	void __fastcall SetFieldDelimiter(const System::WideChar Value);
	void __fastcall SetIncludeHeader(const bool Value);
	void __fastcall SetQuoteAlways(const bool Value);
	void __fastcall SetQuoteDelimiter(const System::WideChar Value);
	void __fastcall SetQuoteIfSpaces(const bool Value);
	
public:
	__fastcall TStDBtoCSVExport(void);
	virtual void __fastcall DoQuote(System::UnicodeString &Value);
	void __fastcall ExportToStream(System::Classes::TStream* AStream);
	void __fastcall ExportToFile(System::Sysutils::TFileName AFile);
	__property Data::Db::TDataSet* DataSet = {read=FDataSet, write=SetDataSet};
	__property System::WideChar FieldDelimiter = {read=FFieldDelimiter, write=SetFieldDelimiter, default=44};
	__property bool IncludeHeader = {read=FIncludeHeader, write=SetIncludeHeader, default=0};
	__property char LineTermChar = {read=FLineTermChar, write=FLineTermChar, default=0};
	__property Stbase::TStLineTerminator LineTerminator = {read=FLineTerminator, write=FLineTerminator, default=3};
	__property bool QuoteAlways = {read=FQuoteAlways, write=SetQuoteAlways, default=0};
	__property System::WideChar QuoteDelimiter = {read=FQuoteDelimiter, write=SetQuoteDelimiter, default=34};
	__property bool QuoteIfSpaces = {read=FQuoteIfSpaces, write=SetQuoteIfSpaces, default=0};
	__property System::UnicodeString DateFmt = {read=FDateFmt, write=FDateFmt};
	__property System::UnicodeString TimeFmt = {read=FTimeFmt, write=FTimeFmt};
	__property System::UnicodeString DateTimeFmt = {read=FDateTimeFmt, write=FDateTimeFmt};
	__property Sttxtdat::TStOnQuoteFieldEvent OnQuoteField = {read=FOnQuoteField, write=FOnQuoteField};
	__property TStExportProgressEvent OnExportProgress = {read=FOnExportProgress, write=FOnExportProgress};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStDBtoCSVExport(void) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStDbSchemaGenerator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Data::Db::TDataSet* FDataSet;
	Sttxtdat::TStTextDataSchema* FSchema;
	
protected:
	System::WideChar __fastcall GetFieldDelimiter(void);
	System::WideChar __fastcall GetQuoteDelimiter(void);
	System::UnicodeString __fastcall GetSchemaName(void);
	void __fastcall SetDataSet(Data::Db::TDataSet* const Value);
	void __fastcall SetFieldDelimiter(const System::WideChar Value);
	void __fastcall SetQuoteDelimiter(const System::WideChar Value);
	void __fastcall SetSchemaName(const System::UnicodeString Value);
	
public:
	__fastcall TStDbSchemaGenerator(void);
	__fastcall virtual ~TStDbSchemaGenerator(void);
	void __fastcall ExportToStream(System::Classes::TStream* AStream);
	void __fastcall ExportToFile(System::Sysutils::TFileName AFile);
	__property Data::Db::TDataSet* DataSet = {read=FDataSet, write=SetDataSet};
	__property System::WideChar FieldDelimiter = {read=GetFieldDelimiter, write=SetFieldDelimiter, default=44};
	__property System::WideChar QuoteDelimiter = {read=GetQuoteDelimiter, write=SetQuoteDelimiter, default=34};
	__property System::UnicodeString SchemaName = {read=GetSchemaName, write=SetSchemaName};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define DefaultDateFmt L"mm/dd/yyyy"
#define DefaultTimeFmt L"hh:mm:ss"
#define DefaultDateTimeFmt L"mm/dd/yyyy hh:mm:ss"
}	/* namespace Stexport */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STEXPORT)
using namespace Stexport;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StexportHPP
