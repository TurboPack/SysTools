// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StFIN.pas' rev: 32.00 (Windows)

#ifndef StfinHPP
#define StfinHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Math.hpp>
#include <System.SysUtils.hpp>
#include <StBase.hpp>
#include <StConst.hpp>
#include <StDate.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stfin
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStPaymentTime : unsigned char { ptEndOfPeriod, ptStartOfPeriod };

enum DECLSPEC_DENUM TStFrequency : unsigned char { fqAnnual, fqSemiAnnual, fqQuarterly, fqMonthly };

enum DECLSPEC_DENUM TStBasis : unsigned char { BasisNASD, BasisActAct, BasisAct360, BasisAct365, BasisEur30360 };

typedef System::StaticArray<int, 536870911> TStDateArray;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Extended StDelta;
extern DELPHI_PACKAGE System::Extended StEpsilon;
extern DELPHI_PACKAGE int StMaxIterations;
extern DELPHI_PACKAGE int __fastcall DayCount(int Day1, int Day2, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall AccruedInterestMaturity(int Issue, int Maturity, System::Extended Rate, System::Extended Par, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall AccruedInterestPeriodic(int Issue, int Settlement, int Maturity, System::Extended Rate, System::Extended Par, TStFrequency Frequency, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall BondDuration(int Settlement, int Maturity, System::Extended Rate, System::Extended Yield, TStFrequency Frequency, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall BondPrice(int Settlement, int Maturity, System::Extended Rate, System::Extended Yield, System::Extended Redemption, TStFrequency Frequency, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall CumulativeInterest(System::Extended Rate, int NPeriods, System::Extended PV, int StartPeriod, int EndPeriod, TStFrequency Frequency, TStPaymentTime Timing);
extern DELPHI_PACKAGE System::Extended __fastcall CumulativePrincipal(System::Extended Rate, int NPeriods, System::Extended PV, int StartPeriod, int EndPeriod, TStFrequency Frequency, TStPaymentTime Timing);
extern DELPHI_PACKAGE System::Extended __fastcall DecliningBalance(System::Extended Cost, System::Extended Salvage, int Life, int Period, int Month);
extern DELPHI_PACKAGE System::Extended __fastcall DiscountRate(int Settlement, int Maturity, System::Extended Price, System::Extended Redemption, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall DollarToDecimal(System::Extended FracDollar, int Fraction);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DollarToDecimalText(System::Extended DecDollar);
extern DELPHI_PACKAGE System::Extended __fastcall DollarToFraction(System::Extended DecDollar, int Fraction);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DollarToFractionStr(System::Extended FracDollar, int Fraction);
extern DELPHI_PACKAGE System::Extended __fastcall EffectiveInterestRate(System::Extended NominalRate, TStFrequency Frequency);
extern DELPHI_PACKAGE System::Extended __fastcall FutureValue(System::Extended Rate, int NPeriods, System::Extended Pmt, System::Extended PV, TStFrequency Frequency, TStPaymentTime Timing);
extern DELPHI_PACKAGE System::Extended __fastcall FutureValueSchedule(System::Extended Principal, const double *Schedule, const int Schedule_High);
extern DELPHI_PACKAGE System::Extended __fastcall FutureValueSchedule16(System::Extended Principal, const void *Schedule, int NRates);
extern DELPHI_PACKAGE System::Extended __fastcall InterestRate(int NPeriods, System::Extended Pmt, System::Extended PV, System::Extended FV, TStFrequency Frequency, TStPaymentTime Timing, System::Extended Guess);
extern DELPHI_PACKAGE System::Extended __fastcall InternalRateOfReturn(const double *Values, const int Values_High, System::Extended Guess);
extern DELPHI_PACKAGE System::Extended __fastcall InternalRateOfReturn16(const void *Values, int NValues, System::Extended Guess);
extern DELPHI_PACKAGE bool __fastcall IsCardValid(const System::UnicodeString S);
extern DELPHI_PACKAGE System::Extended __fastcall ModifiedDuration(int Settlement, int Maturity, System::Extended Rate, System::Extended Yield, TStFrequency Frequency, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall ModifiedIRR(const double *Values, const int Values_High, System::Extended FinanceRate, System::Extended ReinvestRate);
extern DELPHI_PACKAGE System::Extended __fastcall ModifiedIRR16(const void *Values, int NValues, System::Extended FinanceRate, System::Extended ReinvestRate);
extern DELPHI_PACKAGE System::Extended __fastcall NetPresentValue(System::Extended Rate, const double *Values, const int Values_High);
extern DELPHI_PACKAGE System::Extended __fastcall NetPresentValue16(System::Extended Rate, const void *Values, int NValues);
extern DELPHI_PACKAGE System::Extended __fastcall NominalInterestRate(System::Extended EffectRate, TStFrequency Frequency);
extern DELPHI_PACKAGE System::Extended __fastcall NonperiodicIRR(const double *Values, const int Values_High, const int *Dates, const int Dates_High, System::Extended Guess);
extern DELPHI_PACKAGE System::Extended __fastcall NonperiodicIRR16(const void *Values, const void *Dates, int NValues, System::Extended Guess);
extern DELPHI_PACKAGE System::Extended __fastcall NonperiodicNPV(System::Extended Rate, const double *Values, const int Values_High, const int *Dates, const int Dates_High);
extern DELPHI_PACKAGE System::Extended __fastcall NonperiodicNPV16(System::Extended Rate, const void *Values, const void *Dates, int NValues);
extern DELPHI_PACKAGE System::Extended __fastcall Payment(System::Extended Rate, int NPeriods, System::Extended PV, System::Extended FV, TStFrequency Frequency, TStPaymentTime Timing);
extern DELPHI_PACKAGE int __fastcall Periods(System::Extended Rate, System::Extended Pmt, System::Extended PV, System::Extended FV, TStFrequency Frequency, TStPaymentTime Timing);
extern DELPHI_PACKAGE System::Extended __fastcall PresentValue(System::Extended Rate, int NPeriods, System::Extended Pmt, System::Extended FV, TStFrequency Frequency, TStPaymentTime Timing);
extern DELPHI_PACKAGE System::Extended __fastcall ReceivedAtMaturity(int Settlement, int Maturity, System::Extended Investment, System::Extended Discount, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall RoundToDecimal(System::Extended Value, int Places, bool Bankers);
extern DELPHI_PACKAGE System::Extended __fastcall TBillEquivYield(int Settlement, int Maturity, System::Extended Discount);
extern DELPHI_PACKAGE System::Extended __fastcall TBillPrice(int Settlement, int Maturity, System::Extended Discount);
extern DELPHI_PACKAGE System::Extended __fastcall TBillYield(int Settlement, int Maturity, System::Extended Price);
extern DELPHI_PACKAGE System::Extended __fastcall VariableDecliningBalance(System::Extended Cost, System::Extended Salvage, int Life, System::Extended StartPeriod, System::Extended EndPeriod, System::Extended Factor, bool NoSwitch);
extern DELPHI_PACKAGE System::Extended __fastcall YieldDiscounted(int Settlement, int Maturity, System::Extended Price, System::Extended Redemption, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall YieldPeriodic(int Settlement, int Maturity, System::Extended Rate, System::Extended Price, System::Extended Redemption, TStFrequency Frequency, TStBasis Basis);
extern DELPHI_PACKAGE System::Extended __fastcall YieldMaturity(int Issue, int Settlement, int Maturity, System::Extended Rate, System::Extended Price, TStBasis Basis);
}	/* namespace Stfin */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STFIN)
using namespace Stfin;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StfinHPP
