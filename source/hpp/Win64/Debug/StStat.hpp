// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StStat.pas' rev: 29.00 (Windows)

#ifndef StstatHPP
#define StstatHPP

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
#include <StConst.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ststat
{
//-- forward type declarations -----------------------------------------------
struct TStLinEst;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TStLinEst
{
public:
	double B0;
	double B1;
	double seB0;
	double seB1;
	double R2;
	double sigma;
	double SSr;
	double SSe;
	double F0;
	int df;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE double __fastcall AveDev(double const *Data, const int Data_High);
extern DELPHI_PACKAGE double __fastcall AveDev16(const void *Data, int NData);
extern DELPHI_PACKAGE double __fastcall Confidence(double Alpha, double StandardDev, int Size);
extern DELPHI_PACKAGE double __fastcall Correlation(double const *Data1, const int Data1_High, double const *Data2, const int Data2_High);
extern DELPHI_PACKAGE double __fastcall Correlation16(const void *Data1, const void *Data2, int NData);
extern DELPHI_PACKAGE double __fastcall Covariance(double const *Data1, const int Data1_High, double const *Data2, const int Data2_High);
extern DELPHI_PACKAGE double __fastcall Covariance16(const void *Data1, const void *Data2, int NData);
extern DELPHI_PACKAGE double __fastcall DevSq(double const *Data, const int Data_High);
extern DELPHI_PACKAGE double __fastcall DevSq16(const void *Data, int NData);
extern DELPHI_PACKAGE void __fastcall Frequency(double const *Data, const int Data_High, double const *Bins, const int Bins_High, int *Counts, const int Counts_High);
extern DELPHI_PACKAGE void __fastcall Frequency16(const void *Data, int NData, const void *Bins, int NBins, void *Counts);
extern DELPHI_PACKAGE double __fastcall GeometricMean(double const *Data, const int Data_High);
extern DELPHI_PACKAGE double __fastcall GeometricMean16(const void *Data, int NData);
extern DELPHI_PACKAGE double __fastcall HarmonicMean(double const *Data, const int Data_High);
extern DELPHI_PACKAGE double __fastcall HarmonicMean16(const void *Data, int NData);
extern DELPHI_PACKAGE double __fastcall Largest(double const *Data, const int Data_High, int K);
extern DELPHI_PACKAGE double __fastcall Largest16(const void *Data, int NData, int K);
extern DELPHI_PACKAGE double __fastcall LargestSort(double const *Data, const int Data_High, int K);
extern DELPHI_PACKAGE double __fastcall Median(double const *Data, const int Data_High);
extern DELPHI_PACKAGE double __fastcall Median16(const void *Data, int NData);
extern DELPHI_PACKAGE double __fastcall Mode(double const *Data, const int Data_High);
extern DELPHI_PACKAGE double __fastcall Mode16(const void *Data, int NData);
extern DELPHI_PACKAGE double __fastcall Percentile(double const *Data, const int Data_High, double K);
extern DELPHI_PACKAGE double __fastcall Percentile16(const void *Data, int NData, double K);
extern DELPHI_PACKAGE double __fastcall PercentRank(double const *Data, const int Data_High, double X);
extern DELPHI_PACKAGE double __fastcall PercentRank16(const void *Data, int NData, double X);
extern DELPHI_PACKAGE float __fastcall GammaLn(float X);
extern DELPHI_PACKAGE System::Extended __fastcall Factorial(int N);
extern DELPHI_PACKAGE System::Extended __fastcall Permutations(int Number, int NumberChosen);
extern DELPHI_PACKAGE System::Extended __fastcall Combinations(int Number, int NumberChosen);
extern DELPHI_PACKAGE int __fastcall Rank(double Number, double const *Data, const int Data_High, bool Ascending);
extern DELPHI_PACKAGE int __fastcall Rank16(double Number, const void *Data, int NData, bool Ascending);
extern DELPHI_PACKAGE double __fastcall Smallest(double const *Data, const int Data_High, int K);
extern DELPHI_PACKAGE double __fastcall Smallest16(const void *Data, int NData, int K);
extern DELPHI_PACKAGE double __fastcall SmallestSort(double const *Data, const int Data_High, int K);
extern DELPHI_PACKAGE double __fastcall TrimMean(double const *Data, const int Data_High, double Percent);
extern DELPHI_PACKAGE double __fastcall TrimMean16(const void *Data, int NData, double Percent);
extern DELPHI_PACKAGE void __fastcall LinEst(double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High, TStLinEst &LF, bool ErrorStats);
extern DELPHI_PACKAGE void __fastcall LinEst16(const void *KnownY, const void *KnownX, int NData, TStLinEst &LF, bool ErrorStats);
extern DELPHI_PACKAGE void __fastcall LogEst(double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High, TStLinEst &LF, bool ErrorStats);
extern DELPHI_PACKAGE void __fastcall LogEst16(const void *KnownY, const void *KnownX, int NData, TStLinEst &LF, bool ErrorStats);
extern DELPHI_PACKAGE double __fastcall Forecast(double X, double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High);
extern DELPHI_PACKAGE double __fastcall Forecast16(double X, const void *KnownY, const void *KnownX, int NData);
extern DELPHI_PACKAGE double __fastcall ForecastExponential(double X, double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High);
extern DELPHI_PACKAGE double __fastcall ForecastExponential16(double X, const void *KnownY, const void *KnownX, int NData);
extern DELPHI_PACKAGE double __fastcall Intercept(double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High);
extern DELPHI_PACKAGE double __fastcall Intercept16(const void *KnownY, const void *KnownX, int NData);
extern DELPHI_PACKAGE double __fastcall RSquared(double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High);
extern DELPHI_PACKAGE double __fastcall RSquared16(const void *KnownY, const void *KnownX, int NData);
extern DELPHI_PACKAGE double __fastcall Slope(double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High);
extern DELPHI_PACKAGE double __fastcall Slope16(const void *KnownY, const void *KnownX, int NData);
extern DELPHI_PACKAGE double __fastcall StandardErrorY(double const *KnownY, const int KnownY_High, double const *KnownX, const int KnownX_High);
extern DELPHI_PACKAGE double __fastcall StandardErrorY16(const void *KnownY, const void *KnownX, int NData);
extern DELPHI_PACKAGE float __fastcall BetaDist(float X, float Alpha, float Beta, float A, float B);
extern DELPHI_PACKAGE float __fastcall BetaInv(float Probability, float Alpha, float Beta, float A, float B);
extern DELPHI_PACKAGE float __fastcall BinomDist(int NumberS, int Trials, float ProbabilityS, bool Cumulative);
extern DELPHI_PACKAGE int __fastcall CritBinom(int Trials, float ProbabilityS, float Alpha);
extern DELPHI_PACKAGE float __fastcall ChiDist(float X, int DegreesFreedom);
extern DELPHI_PACKAGE float __fastcall ChiInv(float Probability, int DegreesFreedom);
extern DELPHI_PACKAGE float __fastcall ExponDist(float X, float Lambda, bool Cumulative);
extern DELPHI_PACKAGE float __fastcall FDist(float X, int DegreesFreedom1, int DegreesFreedom2);
extern DELPHI_PACKAGE float __fastcall FInv(float Probability, int DegreesFreedom1, int DegreesFreedom2);
extern DELPHI_PACKAGE float __fastcall LogNormDist(float X, float Mean, float StandardDev);
extern DELPHI_PACKAGE float __fastcall LogInv(float Probability, float Mean, float StandardDev);
extern DELPHI_PACKAGE float __fastcall NormDist(float X, float Mean, float StandardDev, bool Cumulative);
extern DELPHI_PACKAGE float __fastcall NormInv(float Probability, float Mean, float StandardDev);
extern DELPHI_PACKAGE float __fastcall Erfc(float X);
extern DELPHI_PACKAGE float __fastcall NormSDist(float Z);
extern DELPHI_PACKAGE float __fastcall NormSInv(float Probability);
extern DELPHI_PACKAGE float __fastcall Poisson(int X, float Mean, bool Cumulative);
extern DELPHI_PACKAGE float __fastcall TDist(float X, int DegreesFreedom, bool TwoTails);
extern DELPHI_PACKAGE float __fastcall TInv(float Probability, int DegreesFreedom);
}	/* namespace Ststat */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSTAT)
using namespace Ststat;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StstatHPP
