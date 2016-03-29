// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StMime.pas' rev: 31.00 (Windows)

#ifndef StmimeHPP
#define StmimeHPP

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
#include <StConst.hpp>
#include <StBase.hpp>
#include <StStrL.hpp>
#include <StOStr.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stmime
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStConvertStream;
class DELPHICLASS TStRawStream;
class DELPHICLASS TStQuotedStream;
class DELPHICLASS TStUUStream;
class DELPHICLASS TStBase64Stream;
class DELPHICLASS TStAttachment;
class DELPHICLASS TStMimeConverter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStConvertState : unsigned char { csStarted, csProgressing, csFinished };

typedef void __fastcall (__closure *TStProgressEvent)(System::TObject* Sender, TStConvertState Status, System::Byte PercentDone);

typedef void __fastcall (__closure *TStSaveAsEvent)(System::TObject* Sender, System::UnicodeString &FileName);

class PASCALIMPLEMENTATION TStConvertStream : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::UnicodeString FCurrentFile;
	TStMimeConverter* FOwner;
	TStProgressEvent FOnProgress;
	
public:
	__fastcall virtual TStConvertStream(TStMimeConverter* Owner);
	virtual void __fastcall DecodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream) = 0 ;
	virtual void __fastcall EncodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream) = 0 ;
	virtual void __fastcall Progress(TStConvertState Status, System::Byte PercentDone);
	__property System::UnicodeString CurrentFile = {read=FCurrentFile, write=FCurrentFile};
	__property TStProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStConvertStream(void) { }
	
};


class PASCALIMPLEMENTATION TStRawStream : public TStConvertStream
{
	typedef TStConvertStream inherited;
	
public:
	__fastcall virtual TStRawStream(TStMimeConverter* Owner);
	virtual void __fastcall DecodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	virtual void __fastcall EncodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStRawStream(void) { }
	
};


class PASCALIMPLEMENTATION TStQuotedStream : public TStConvertStream
{
	typedef TStConvertStream inherited;
	
public:
	__fastcall virtual TStQuotedStream(TStMimeConverter* Owner);
	virtual void __fastcall DecodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	virtual void __fastcall EncodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStQuotedStream(void) { }
	
};


class PASCALIMPLEMENTATION TStUUStream : public TStConvertStream
{
	typedef TStConvertStream inherited;
	
public:
	__fastcall virtual TStUUStream(TStMimeConverter* Owner);
	virtual void __fastcall DecodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	virtual void __fastcall EncodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStUUStream(void) { }
	
};


class PASCALIMPLEMENTATION TStBase64Stream : public TStConvertStream
{
	typedef TStConvertStream inherited;
	
public:
	__fastcall virtual TStBase64Stream(TStMimeConverter* Owner);
	virtual void __fastcall DecodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
	virtual void __fastcall EncodeToStream(System::Classes::TStream* InStream, System::Classes::TStream* OutStream);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TStBase64Stream(void) { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TStConverterClass);

class PASCALIMPLEMENTATION TStAttachment : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::UnicodeString FContentDescription;
	System::UnicodeString FContentDisposition;
	System::UnicodeString FContentType;
	System::UnicodeString FEncoding;
	System::UnicodeString FFileName;
	bool FOldStyle;
	int FSize;
	int FStreamOffset;
	
public:
	__property System::UnicodeString atContentDescription = {read=FContentDescription, write=FContentDescription};
	__property System::UnicodeString atContentDisposition = {read=FContentDisposition, write=FContentDisposition};
	__property System::UnicodeString atContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString atEncoding = {read=FEncoding, write=FEncoding};
	__property System::UnicodeString atFilename = {read=FFileName, write=FFileName};
	__property bool atOldStyle = {read=FOldStyle, write=FOldStyle, nodefault};
	__property int atSize = {read=FSize, write=FSize, nodefault};
	__property int atStreamOffset = {read=FStreamOffset, write=FStreamOffset, nodefault};
public:
	/* TObject.Create */ inline __fastcall TStAttachment(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStAttachment(void) { }
	
};


class PASCALIMPLEMENTATION TStMimeConverter : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	System::Classes::TStringList* FAttachments;
	System::UnicodeString FBoundary;
	bool FBoundaryUsed;
	System::UnicodeString FContentDescription;
	System::UnicodeString FContentDisposition;
	System::UnicodeString FContentType;
	TStConvertStream* FConverter;
	System::UnicodeString FDirectory;
	System::UnicodeString FEncoding;
	int FEndBoundaryOffset;
	bool FMimeHeaders;
	System::Classes::TStream* FStream;
	System::Classes::TMemoryStream* FInternalStream;
	TStProgressEvent FOnProgress;
	TStSaveAsEvent FOnSaveAs;
	void __fastcall AddMimeFooters(void);
	void __fastcall AddMimeHeaders(const System::UnicodeString AFileName);
	void __fastcall DeleteAttachments(void);
	void __fastcall ForceType(TStConverterClass ConverterType);
	System::UnicodeString __fastcall GetBoundary(void);
	System::Classes::TStream* __fastcall GetStream(void);
	void __fastcall InitConverter(void);
	void __fastcall SetBoundary(System::UnicodeString Value);
	void __fastcall SetConverter(TStConvertStream* Value);
	void __fastcall SetEncoding(System::UnicodeString Value);
	void __fastcall SetStream(System::Classes::TStream* Value);
	void __fastcall FindOldAttachment(void);
	DYNAMIC System::UnicodeString __fastcall GenerateBoundary(void);
	DYNAMIC void __fastcall PositionForExtract(TStAttachment* Att);
	DYNAMIC void __fastcall Progress(System::TObject* Sender, TStConvertState Status, System::Byte PercentDone);
	void __fastcall SaveAs(System::UnicodeString &FileName);
	void __fastcall ScanAttachments(void);
	
public:
	__fastcall TStMimeConverter(void);
	__fastcall virtual TStMimeConverter(System::Classes::TStream* AStream);
	__fastcall virtual ~TStMimeConverter(void);
	void __fastcall AddFileAttachment(const System::UnicodeString AFileName);
	DYNAMIC void __fastcall AddStreamAttachment(System::Classes::TStream* AStream, const System::UnicodeString AFileName);
	DYNAMIC void __fastcall ExtractAttachment(const System::UnicodeString Attachment);
	DYNAMIC void __fastcall ExtractAttachmentIndex(int Index);
	DYNAMIC void __fastcall ExtractToStream(int Index, System::Classes::TStream* AStream);
	void __fastcall ExtractAttachments(void);
	void __fastcall FillConverterList(System::Classes::TStrings* List);
	System::UnicodeString __fastcall GetTag(const System::UnicodeString Description);
	__classmethod void __fastcall RegisterConverter(const System::UnicodeString ATag, const System::UnicodeString ADesc, TStConverterClass AClass);
	__classmethod void __fastcall UnRegisterConverterClass(TStConverterClass AClass);
	__property System::Classes::TStringList* Attachments = {read=FAttachments};
	__property System::UnicodeString Boundary = {read=GetBoundary, write=SetBoundary};
	__property System::UnicodeString Encoding = {read=FEncoding, write=SetEncoding};
	__property System::UnicodeString ContentDescription = {read=FContentDescription, write=FContentDescription};
	__property System::UnicodeString ContentDisposition = {read=FContentDisposition, write=FContentDisposition};
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property TStConvertStream* Converter = {read=FConverter, write=SetConverter};
	__property System::UnicodeString Directory = {read=FDirectory, write=FDirectory};
	__property bool MimeHeaders = {read=FMimeHeaders, write=FMimeHeaders, default=1};
	__property System::Classes::TStream* Stream = {read=GetStream, write=SetStream};
	__property TStProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TStSaveAsEvent OnSaveAs = {read=FOnSaveAs, write=FOnSaveAs};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 AttachmentFileMode = System::Int8(0x20);
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 2> CRLFStr;
#define DefStContentDisposition L"attachment"
#define DefStContentType L"application/octet-stream"
#define DefStMimeEncoding L"base64"
static const System::Int8 ExtractFileMode = System::Int8(0x12);
static const System::Int8 MaxMimeLine = System::Int8(0x4e);
}	/* namespace Stmime */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STMIME)
using namespace Stmime;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StmimeHPP
