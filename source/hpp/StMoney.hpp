// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StMoney.pas' rev: 28.00 (Windows)

#ifndef StmoneyHPP
#define StmoneyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <StConst.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit
#include <StStrms.hpp>	// Pascal unit
#include <StDecMth.hpp>	// Pascal unit
#include <StIniStm.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stmoney
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStCurrency;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStCurrency : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FISOCode;
	System::UnicodeString FISOName;
	int FRatio;
	System::UnicodeString FUnitMajor;
	System::UnicodeString FUnitMinor;
	
public:
	void __fastcall LoadFromList(System::Classes::TStrings* List);
	void __fastcall SaveToList(System::Classes::TStrings* List);
	__property System::UnicodeString ISOCode = {read=FISOCode, write=FISOCode};
	__property System::UnicodeString ISOName = {read=FISOName, write=FISOName};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property int Ratio = {read=FRatio, write=FRatio, nodefault};
	__property System::UnicodeString UnitMajor = {read=FUnitMajor, write=FUnitMajor};
	__property System::UnicodeString UnitMinor = {read=FUnitMinor, write=FUnitMinor};
public:
	/* TObject.Create */ inline __fastcall TStCurrency(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TStCurrency(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TStCurrencyList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStCurrencyList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TStCurrency* operator[](int Index) { return Items[Index]; }
	
private:
	System::Classes::TStringList* FItems;
	
protected:
	int __fastcall GetCount(void);
	TStCurrency* __fastcall GetCurrency(const System::UnicodeString ISOName);
	TStCurrency* __fastcall GetItem(int Index);
	void __fastcall SetCurrency(const System::UnicodeString ISOName, TStCurrency* Value);
	void __fastcall SetItem(int Index, TStCurrency* Value);
	void __fastcall FreeCurrencyByIndex(int Index);
	
public:
	__fastcall TStCurrencyList(void);
	__fastcall virtual ~TStCurrencyList(void);
	void __fastcall Add(TStCurrency* ACurrency);
	void __fastcall Clear(void);
	bool __fastcall Contains(TStCurrency* ACurrency);
	bool __fastcall ContainsName(const System::UnicodeString ISOName);
	void __fastcall Delete(const System::UnicodeString ISOName);
	int __fastcall IndexOf(const System::UnicodeString ISOName);
	void __fastcall LoadFromFile(const System::Sysutils::TFileName AFileName);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToFile(const System::Sysutils::TFileName AFileName);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property int Count = {read=GetCount, nodefault};
	__property TStCurrency* Currencies[const System::UnicodeString ISOName] = {read=GetCurrency, write=SetCurrency};
	__property TStCurrency* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
};

#pragma pack(pop)

enum DECLSPEC_DENUM TStConversionType : unsigned char { ctUnknown, ctTriangular, ctMultiply, ctDivide };

typedef void __fastcall (__closure *TStGetRateUpdateEvent)(System::TObject* Sender, Stdecmth::TStDecimal* NewRate, System::TDateTime &NewDate);

class DELPHICLASS TStExchangeRate;
class PASCALIMPLEMENTATION TStExchangeRate : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Stdecmth::TStDecimal* FRate;
	System::UnicodeString FSource;
	System::UnicodeString FTarget;
	System::UnicodeString FIntermediate;
	TStConversionType FConversionType;
	System::TDateTime FDateUpdated;
	TStGetRateUpdateEvent FOnGetRateUpdate;
	void __fastcall SetRate(Stdecmth::TStDecimal* const Value);
	
public:
	__fastcall TStExchangeRate(void);
	__fastcall virtual ~TStExchangeRate(void);
	void __fastcall Assign(TStExchangeRate* ARate);
	void __fastcall Clear(void);
	void __fastcall Convert(Stdecmth::TStDecimal* Amount, Stdecmth::TStDecimal* Result);
	HIDESBASE bool __fastcall Equals(TStExchangeRate* aRate);
	bool __fastcall IsValid(void);
	bool __fastcall SameSourceAndTarget(TStExchangeRate* aRate);
	void __fastcall Update(void);
	void __fastcall LoadFromList(System::Classes::TStrings* List);
	void __fastcall SaveToList(System::Classes::TStrings* List);
	__property TStConversionType ConversionType = {read=FConversionType, write=FConversionType, nodefault};
	__property System::TDateTime DateUpdated = {read=FDateUpdated, write=FDateUpdated};
	__property System::UnicodeString Intermediate = {read=FIntermediate, write=FIntermediate};
	__property Stdecmth::TStDecimal* Rate = {read=FRate, write=SetRate};
	__property System::UnicodeString Source = {read=FSource, write=FSource};
	__property System::UnicodeString Target = {read=FTarget, write=FTarget};
	__property TStGetRateUpdateEvent OnGetRateUpdate = {read=FOnGetRateUpdate, write=FOnGetRateUpdate};
};


class DELPHICLASS TStExchangeRateList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStExchangeRateList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TStringList* FRates;
	
protected:
	void __fastcall DeleteRate(int Index);
	int __fastcall GetCount(void);
	TStExchangeRate* __fastcall GetRate(const System::UnicodeString Source, const System::UnicodeString Target);
	TStExchangeRate* __fastcall GetItem(int Index);
	virtual System::UnicodeString __fastcall MakeEntry(const System::UnicodeString Source, const System::UnicodeString Target);
	void __fastcall ConvertPrim(const System::UnicodeString aSource, const System::UnicodeString aTarget, Stdecmth::TStDecimal* aAmount, bool aAllowTriangular);
	
public:
	__fastcall TStExchangeRateList(void);
	__fastcall virtual ~TStExchangeRateList(void);
	void __fastcall Add(TStExchangeRate* ARate);
	void __fastcall AddByValues(const System::UnicodeString Source, const System::UnicodeString Target, const System::UnicodeString Intermediate, double Rate, TStConversionType ConversionType, System::TDateTime DateUpdated);
	void __fastcall Assign(TStExchangeRateList* AList);
	void __fastcall Clear(void);
	bool __fastcall Contains(TStExchangeRate* ARate);
	bool __fastcall ContainsByName(const System::UnicodeString Source, const System::UnicodeString Target);
	void __fastcall Convert(const System::UnicodeString Source, const System::UnicodeString Target, Stdecmth::TStDecimal* Amount, Stdecmth::TStDecimal* Result);
	void __fastcall Delete(TStExchangeRate* ARate);
	void __fastcall DeleteByName(const System::UnicodeString Source, const System::UnicodeString Target);
	void __fastcall UpdateRate(const System::UnicodeString Source, const System::UnicodeString Target, Stdecmth::TStDecimal* Rate);
	void __fastcall LoadFromFile(const System::Sysutils::TFileName AFileName);
	void __fastcall LoadFromStream(System::Classes::TStream* AStream);
	void __fastcall SaveToFile(const System::Sysutils::TFileName AFileName);
	void __fastcall SaveToStream(System::Classes::TStream* AStream);
	__property int Count = {read=GetCount, nodefault};
	__property TStExchangeRate* Items[int Index] = {read=GetItem};
	__property TStExchangeRate* Rates[const System::UnicodeString Source][const System::UnicodeString Target] = {read=GetRate};
};

#pragma pack(pop)

class DELPHICLASS TStMoney;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TStMoney : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Stdecmth::TStDecimal* FAmount;
	System::UnicodeString FCurrency;
	TStExchangeRateList* FExchangeRates;
	double __fastcall GetAsFloat(void);
	System::UnicodeString __fastcall GetAsString(void);
	void __fastcall SetAmount(Stdecmth::TStDecimal* const Value);
	void __fastcall SetAsFloat(const double Value);
	void __fastcall SetAsString(const System::UnicodeString Value);
	void __fastcall Validate(TStMoney* Source, TStMoney* Operand, TStMoney* Result);
	bool __fastcall ValidateCurrencies(TStMoney* Source, TStMoney* Dest);
	
public:
	__fastcall TStMoney(void);
	__fastcall virtual ~TStMoney(void);
	void __fastcall Assign(TStMoney* AMoney);
	void __fastcall Abs(TStMoney* Result);
	void __fastcall Add(TStMoney* Addend, TStMoney* Sum);
	void __fastcall Divide(double Divisor, TStMoney* Quotient);
	void __fastcall DivideByDecimal(Stdecmth::TStDecimal* Divisor, TStMoney* Quotient);
	void __fastcall Multiply(double Multiplier, TStMoney* Product);
	void __fastcall MultiplyByDecimal(Stdecmth::TStDecimal* Multiplier, TStMoney* Product);
	void __fastcall Negate(TStMoney* Result);
	void __fastcall Subtract(TStMoney* Subtrahend, TStMoney* Remainder);
	int __fastcall Compare(TStMoney* CompareTo);
	bool __fastcall IsEqual(TStMoney* AMoney);
	bool __fastcall IsGreaterThan(TStMoney* AMoney);
	bool __fastcall IsGreaterThanOrEqual(TStMoney* AMoney);
	bool __fastcall IsLessThan(TStMoney* AMoney);
	bool __fastcall IsLessThanOrEqual(TStMoney* AMoney);
	bool __fastcall IsNegative(void);
	bool __fastcall IsNotEqual(TStMoney* AMoney);
	bool __fastcall IsPositive(void);
	bool __fastcall IsZero(void);
	void __fastcall Convert(const System::UnicodeString Target, TStMoney* Result);
	void __fastcall Round(Stdecmth::TStRoundMethod Method, int Decimals, TStMoney* Result);
	__property Stdecmth::TStDecimal* Amount = {read=FAmount, write=SetAmount};
	__property double AsFloat = {read=GetAsFloat, write=SetAsFloat};
	__property System::UnicodeString AsString = {read=GetAsString, write=SetAsString};
	__property System::UnicodeString Currency = {read=FCurrency, write=FCurrency};
	__property TStExchangeRateList* ExchangeRates = {read=FExchangeRates, write=FExchangeRates};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stmoney */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STMONEY)
using namespace Stmoney;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StmoneyHPP
