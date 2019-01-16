// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StTxtDat.pas' rev: 32.00 (Windows)

#ifndef SttxtdatHPP
#define SttxtdatHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <StConst.hpp>
#include <StBase.hpp>
#include <StStrms.hpp>
#include <StStrL.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sttxtdat
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStDataField;
class DELPHICLASS TStDataFieldList;
class DELPHICLASS TStTextDataSchema;
class DELPHICLASS TStTextDataRecord;
class DELPHICLASS TStTextDataRecordSet;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStSchemaLayoutType : unsigned char { ltUnknown, ltFixed, ltVarying };

enum DECLSPEC_DENUM TStSchemaFieldType : unsigned char { sftUnknown, sftChar, sftFloat, sftNumber, sftBool, sftLongInt, sftDate, sftTime, sftTimeStamp };

typedef void __fastcall (__closure *TStOnQuoteFieldEvent)(System::TObject* Sender, System::UnicodeString &Field);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStDataField : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	int FFieldDecimals;
	int FFieldLen;
	System::UnicodeString FFieldName;
	int FFieldOffset;
	TStSchemaFieldType FFieldType;
	System::UnicodeString __fastcall GetAsString(void);
	void __fastcall SetFieldDecimals(const int Value);
	void __fastcall SetFieldLen(const int Value);
	void __fastcall SetFieldName(const System::UnicodeString Value);
	void __fastcall SetFieldOffset(const int Value);
	void __fastcall SetFieldType(const TStSchemaFieldType Value);
	
public:
	__property System::UnicodeString AsString = {read=GetAsString};
	__property int FieldDecimals = {read=FFieldDecimals, write=SetFieldDecimals, nodefault};
	__property int FieldLen = {read=FFieldLen, write=SetFieldLen, nodefault};
	__property System::UnicodeString FieldName = {read=FFieldName, write=SetFieldName};
	__property int FieldOffset = {read=FFieldOffset, write=SetFieldOffset, nodefault};
	__property TStSchemaFieldType FieldType = {read=FFieldType, write=SetFieldType, nodefault};
public:
	/* TObject.Create */ inline __fastcall TStDataField(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStDataField(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStDataFieldList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TStDataField* operator[](int Index) { return this->Fields[Index]; }
	
private:
	System::Classes::TStringList* FList;
	
protected:
	int __fastcall GetCount(void);
	TStDataField* __fastcall GetField(int Index);
	TStDataField* __fastcall GetFieldByName(const System::UnicodeString FieldName);
	void __fastcall SetField(int Index, TStDataField* const Value);
	void __fastcall SetFieldByName(const System::UnicodeString FieldName, TStDataField* const Value);
	
public:
	__fastcall TStDataFieldList(void);
	__fastcall virtual ~TStDataFieldList(void);
	void __fastcall AddField(const System::UnicodeString FieldName, TStSchemaFieldType FieldType, int FieldLen, int FieldDecimals, int FieldOffset);
	void __fastcall AddFieldStr(const System::UnicodeString FieldDef);
	void __fastcall Clear(void);
	void __fastcall RemoveField(const System::UnicodeString FieldName);
	__property int Count = {read=GetCount, nodefault};
	__property TStDataField* Fields[int Index] = {read=GetField, write=SetField/*, default*/};
	__property TStDataField* FieldByName[const System::UnicodeString FieldName] = {read=GetFieldByName, write=SetFieldByName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TStTextDataSchema : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TStDataField* operator[](int Index) { return this->Fields[Index]; }
	
private:
	System::WideChar FCommentDelimiter;
	System::WideChar FFieldDelimiter;
	TStSchemaLayoutType FLayoutType;
	System::WideChar FLineTermChar;
	Stbase::TStLineTerminator FLineTerminator;
	System::WideChar FQuoteDelimiter;
	System::WideChar FFixedSeparator;
	System::Classes::TStrings* FSchema;
	System::UnicodeString FSchemaName;
	TStDataFieldList* dsFieldList;
	
protected:
	System::Classes::TStrings* __fastcall GetCaptions(void);
	TStDataField* __fastcall GetField(int Index);
	TStDataField* __fastcall GetFieldByName(const System::UnicodeString FieldName);
	int __fastcall GetFieldCount(void);
	System::Classes::TStrings* __fastcall GetSchema(void);
	void __fastcall SetCommentDelimiter(const System::WideChar Value);
	void __fastcall SetField(int Index, TStDataField* const Value);
	void __fastcall SetFieldByName(const System::UnicodeString FieldName, TStDataField* const Value);
	void __fastcall SetFieldDelimiter(const System::WideChar Value);
	void __fastcall SetLayoutType(const TStSchemaLayoutType Value);
	void __fastcall SetQuoteDelimiter(const System::WideChar Value);
	void __fastcall SetFixedSeparator(const System::WideChar Value);
	void __fastcall SetSchema(System::Classes::TStrings* const Value);
	void __fastcall SetSchemaName(const System::UnicodeString Value);
	
public:
	__fastcall TStTextDataSchema(void);
	__fastcall virtual ~TStTextDataSchema(void);
	void __fastcall Assign(TStTextDataSchema* ASchema);
	void __fastcall AddField(const System::UnicodeString FieldName, TStSchemaFieldType FieldType, int FieldLen, int FieldDecimals);
	int __fastcall IndexOf(const System::UnicodeString FieldName);
	void __fastcall RemoveField(const System::UnicodeString FieldName);
	void __fastcall Update(System::Classes::TStrings* AList);
	void __fastcall ClearFields(void);
	void __fastcall BuildSchema(System::Classes::TStrings* AList);
	void __fastcall LoadFromFile(const System::Sysutils::TFileName AFileName);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToFile(const System::Sysutils::TFileName AFileName);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property System::Classes::TStrings* Captions = {read=GetCaptions};
	__property System::WideChar CommentDelimiter = {read=FCommentDelimiter, write=SetCommentDelimiter, default=59};
	__property TStDataField* FieldByName[const System::UnicodeString FieldName] = {read=GetFieldByName, write=SetFieldByName};
	__property int FieldCount = {read=GetFieldCount, nodefault};
	__property System::WideChar FieldDelimiter = {read=FFieldDelimiter, write=SetFieldDelimiter, default=44};
	__property TStDataField* Fields[int Index] = {read=GetField, write=SetField/*, default*/};
	__property TStSchemaLayoutType LayoutType = {read=FLayoutType, write=SetLayoutType, nodefault};
	__property System::WideChar LineTermChar = {read=FLineTermChar, write=FLineTermChar, default=0};
	__property Stbase::TStLineTerminator LineTerminator = {read=FLineTerminator, write=FLineTerminator, default=3};
	__property System::WideChar QuoteDelimiter = {read=FQuoteDelimiter, write=SetQuoteDelimiter, default=34};
	__property System::WideChar FixedSeparator = {read=FFixedSeparator, write=SetFixedSeparator, default=32};
	__property System::Classes::TStrings* Schema = {read=GetSchema, write=SetSchema};
	__property System::UnicodeString SchemaName = {read=FSchemaName, write=SetSchemaName};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TStTextDataRecord : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStrings* FFieldList;
	bool FQuoteAlways;
	bool FQuoteIfSpaces;
	TStTextDataSchema* FSchema;
	System::UnicodeString FValue;
	TStOnQuoteFieldEvent FOnQuoteField;
	
protected:
	System::UnicodeString __fastcall GetField(int Index);
	int __fastcall GetFieldCount(void);
	System::UnicodeString __fastcall GetFieldByName(const System::UnicodeString FieldName);
	System::Classes::TStrings* __fastcall GetFieldList(void);
	System::Classes::TStrings* __fastcall GetValues(void);
	void __fastcall SetField(int Index, const System::UnicodeString NewValue);
	void __fastcall SetFieldByName(const System::UnicodeString FieldName, const System::UnicodeString NewValue);
	void __fastcall SetQuoteAlways(const bool Value);
	void __fastcall SetQuoteIfSpaces(const bool Value);
	void __fastcall SetSchema(TStTextDataSchema* const Value);
	
public:
	__fastcall TStTextDataRecord(void);
	__fastcall virtual ~TStTextDataRecord(void);
	virtual void __fastcall BuildRecord(System::Classes::TStrings* Values, System::UnicodeString &NewRecord);
	System::UnicodeString __fastcall GetRecord(void);
	virtual void __fastcall DoQuote(System::UnicodeString &Value);
	void __fastcall FillRecordFromArray(System::TVarRec *Values, const int Values_High);
	void __fastcall FillRecordFromList(System::Classes::TStrings* Items);
	void __fastcall FillRecordFromValues(System::Classes::TStrings* Values);
	virtual void __fastcall MakeEmpty(void);
	__property System::UnicodeString AsString = {read=GetRecord};
	__property System::UnicodeString FieldByName[const System::UnicodeString FieldName] = {read=GetFieldByName, write=SetFieldByName};
	__property int FieldCount = {read=GetFieldCount, nodefault};
	__property System::Classes::TStrings* FieldList = {read=GetFieldList};
	__property System::UnicodeString Fields[int Index] = {read=GetField, write=SetField};
	__property bool QuoteAlways = {read=FQuoteAlways, write=SetQuoteAlways, default=0};
	__property bool QuoteIfSpaces = {read=FQuoteIfSpaces, write=SetQuoteIfSpaces, default=0};
	__property TStTextDataSchema* Schema = {read=FSchema, write=SetSchema};
	__property System::Classes::TStrings* Values = {read=GetValues};
	__property TStOnQuoteFieldEvent OnQuoteField = {read=FOnQuoteField, write=FOnQuoteField};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStTextDataRecordSet : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FActive;
	int FCurrentIndex;
	bool FIsDirty;
	System::Classes::TList* FRecords;
	TStTextDataSchema* FSchema;
	bool FAtEndOfFile;
	int FIgnoreStartingLines;
	
protected:
	int __fastcall GetCount(void);
	TStTextDataRecord* __fastcall GetCurrentRecord(void);
	TStTextDataRecord* __fastcall GetRecord(int Index);
	TStTextDataSchema* __fastcall GetSchema(void);
	void __fastcall SetActive(const bool Value);
	void __fastcall SetCurrentRecord(TStTextDataRecord* const Value);
	void __fastcall SetRecord(int Index, TStTextDataRecord* const Value);
	void __fastcall SetSchema(TStTextDataSchema* const Value);
	
public:
	__fastcall TStTextDataRecordSet(void);
	__fastcall virtual ~TStTextDataRecordSet(void);
	void __fastcall Append(void);
	void __fastcall AppendArray(System::TVarRec *Values, const int Values_High);
	void __fastcall AppendList(System::Classes::TStrings* Items);
	void __fastcall AppendValues(System::Classes::TStrings* Values);
	void __fastcall Clear(void);
	void __fastcall Delete(void);
	void __fastcall Insert(int Index);
	void __fastcall InsertArray(int Index, System::TVarRec *Values, const int Values_High);
	void __fastcall InsertList(int Index, System::Classes::TStrings* Items);
	void __fastcall InsertValues(int Index, System::Classes::TStrings* Values);
	bool __fastcall BOF(void);
	bool __fastcall EOF(void);
	void __fastcall First(void);
	void __fastcall Last(void);
	bool __fastcall Next(void);
	bool __fastcall Prior(void);
	void __fastcall LoadFromFile(const System::Sysutils::TFileName AFile);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToFile(const System::Sysutils::TFileName AFile);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property TStTextDataRecord* CurrentRecord = {read=GetCurrentRecord, write=SetCurrentRecord};
	__property bool IsDirty = {read=FIsDirty, nodefault};
	__property TStTextDataRecord* Records[int Index] = {read=GetRecord, write=SetRecord};
	__property TStTextDataSchema* Schema = {read=GetSchema, write=SetSchema};
	__property int IgnoreStartingLines = {read=FIgnoreStartingLines, write=FIgnoreStartingLines, default=0};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::WideChar StDefaultDelim = (System::WideChar)(0x2c);
static const System::WideChar StDefaultQuote = (System::WideChar)(0x22);
static const System::WideChar StDefaultComment = (System::WideChar)(0x3b);
static const System::WideChar StDefaultFixedSep = (System::WideChar)(0x20);
#define StDefaultLineTerm L"\r\n"
#define St_WhiteSpace L"\b\t\n\r "
extern DELPHI_PACKAGE void __fastcall StParseLine(const System::UnicodeString Data, TStTextDataSchema* Schema, System::Classes::TStrings* Result);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StFieldTypeToStr(TStSchemaFieldType FieldType);
extern DELPHI_PACKAGE TStSchemaFieldType __fastcall StStrToFieldType(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StTrimTrailingChars(const System::UnicodeString S, System::WideChar Trailer);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StDoEscape(System::WideChar Delim);
extern DELPHI_PACKAGE System::WideChar __fastcall StDeEscape(const System::UnicodeString EscStr);
}	/* namespace Sttxtdat */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STTXTDAT)
using namespace Sttxtdat;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SttxtdatHPP
