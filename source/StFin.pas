// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StFIN.pas 4.04                              *}
{*********************************************************}
{* SysTools: Financial math functions modeled on         *}
{*           those in Excel                              *}
{*********************************************************}

{$I StDefine.inc}

unit StFIN;

interface

uses
  Windows,
  {$IFDEF UseMathUnit}
  Math,
  {$ELSE}
  StMath,
  {$ENDIF}
  SysUtils,
  StBase,
  StConst,
  StDate;

type
  TStPaymentTime = (ptEndOfPeriod, ptStartOfPeriod);
  TStFrequency = (fqAnnual, fqSemiAnnual, fqQuarterly, fqMonthly);
  TStBasis = (BasisNASD,             {US (NASD) 30/360}
              BasisActAct,           {Actual/actual}
              BasisAct360,           {Actual/360}
              BasisAct365,           {Actual/365}
              BasisEur30360);        {European 30/360}

  TStDateArray = array[0..(StMaxBlockSize div SizeOf(TStDate))-1] of TStDate;


const
  StDelta         : Extended = 0.00001;  {delta for difference equations}
  StEpsilon       : Extended = 0.00001;  {epsilon for difference equations}
  StMaxIterations : Integer  = 100;      {max attempts for convergence}


function AccruedInterestMaturity(Issue, Maturity : TStDate;
                                 Rate, Par : Extended;
                                 Basis : TStBasis) : Extended;
  {-Returns the accrued interest for a security that pays interest at maturity}

function AccruedInterestPeriodic(Issue, Settlement, Maturity : TStDate;
                                 Rate, Par : Extended;
                                 Frequency : TStFrequency;
                                 Basis : TStBasis) : Extended;
  {-Returns the accrued interest for a security that pays periodic interest}

function BondDuration(Settlement, Maturity : TStDate;
                      Rate, Yield : Extended;
                      Frequency : TStFrequency;
                      Basis : TStBasis) : Extended;
  {-Returns the Macauley duration for an assumed par value of $100}

function BondPrice(Settlement, Maturity : TStDate;
                   Rate, Yield, Redemption : Extended;
                   Frequency : TStFrequency;
                   Basis : TStBasis) : Extended;
  {-Returns the "clean" bond price per $100 face value of a security}

function CumulativeInterest(Rate : Extended;
                            NPeriods : Integer;
                            PV : Extended;
                            StartPeriod, EndPeriod : Integer;
                            Frequency : TStFrequency;
                            Timing : TStPaymentTime) : Extended;
  {-Returns the cumulative interest paid on a loan in specified periods}

function CumulativePrincipal(Rate : Extended;
                             NPeriods : Integer;
                             PV : Extended;
                             StartPeriod, EndPeriod : Integer;
                             Frequency : TStFrequency;
                             Timing : TStPaymentTime) : Extended;
  {-Returns the cumulative principal paid on a loan in specified periods}

function DayCount(Day1, Day2 : TStDate; Basis : TStBasis) : LongInt;
  {-Returns the number of days from Day1 to Day2 according to day count basis}

function DecliningBalance(Cost, Salvage : Extended;
                          Life, Period, Month : Integer) : Extended;
  {-Fixed rate declining balance depreciation}

function DiscountRate(Settlement, Maturity : TStDate;
                      Price, Redemption : Extended;
                      Basis : TStBasis) : Extended;
  {-Returns the discount Rate for a security}

function DollarToDecimal(FracDollar : Extended;
                         Fraction : Integer) : Extended;
  {-Converts a fractional dollar value to decimal dollar value}

function DollarToDecimalText(DecDollar : Extended) : string;
  {-Converts a decimal dollar value into an English text string}

function DollarToFraction(DecDollar : Extended;
                          Fraction : Integer) : Extended;
  {-Converts a decimal dollar value to fractional dollar value}

function DollarToFractionStr(FracDollar : Extended;
                             Fraction : Integer) : string;
  {-Converts a fractional dollar value to number string}

function EffectiveInterestRate(NominalRate : Extended;
                               Frequency : TStFrequency) : Extended;
  {-Converts nominal annual interest Rate to effective Rate}

function FutureValue(Rate : Extended;
                     NPeriods : Integer;
                     Pmt, PV : Extended;
                     Frequency : TStFrequency;
                     Timing: TStPaymentTime) : Extended;
  {-Returns the future value of an annuity}

  function FutureValueSchedule(Principal : Extended;
                               const Schedule : array of Double) : Extended;

function FutureValueSchedule16(Principal : Extended;
                               const Schedule; NRates : Integer) : Extended;
  {-Returns the future value of investment with variable interest rates}

function InterestRate(NPeriods : Integer;
                      Pmt, PV, FV : Extended;
                      Frequency : TStFrequency;
                      Timing : TStPaymentTime;
                      Guess : Extended) : Extended;
  {-Returns the interest Rate per period of an annuity}

  function InternalRateOfReturn(const Values : array of Double;
                                Guess : Extended) : Extended;

function InternalRateOfReturn16(const Values; NValues : Integer;
                                Guess : Extended) : Extended;
  {-Returns internal rate of return of a series of periodic cash flows}

function IsCardValid(const S : string) : Boolean;
  {-Checks for valid credit card number (MasterCard, Visa, AMEX, Discover)}

function ModifiedDuration(Settlement, Maturity : TStDate;
                          Rate, Yield : Extended;
                          Frequency : TStFrequency;
                          Basis : TStBasis) : Extended;
  {-Returns the modified duration for bond with an assumed par value of $100}

  function ModifiedIRR(const Values : array of Double;
                       FinanceRate, ReinvestRate : Extended) : Extended;

function ModifiedIRR16(const Values; NValues : Integer;
                       FinanceRate, ReinvestRate : Extended) : Extended;
  {-Returns the MIRR for a series of periodic cash flows}

  function NetPresentValue(Rate : Extended;
                           const Values : array of Double) : Extended;

function NetPresentValue16(Rate : Extended;
                           const Values; NValues : Integer) : Extended;
  {-Returns the net present value of a series of periodic cash flows}

function NominalInterestRate(EffectRate : Extended;
                             Frequency : TStFrequency) : Extended;
  {-Converts effective annual interest Rate to nominal Rate}

  function NonperiodicIRR(const Values : array of Double;
                          const Dates : array of TStDate;
                          Guess : Extended) : Extended;

function NonperiodicIRR16(const Values;
                          const Dates; NValues : Integer;
                          Guess : Extended) : Extended;
  {-Returns the IRR for a series of irregular cash flows}

  function NonperiodicNPV(Rate : Extended;
                          const Values : array of Double;
                          const Dates : array of TStDate) : Extended;

function NonperiodicNPV16(Rate : Extended;
                          const Values;
                          const Dates;
                          NValues : Integer) : Extended;
  {-Returns the net present value for a series of irregular cash flows}

function Payment(Rate : Extended;
                 NPeriods : Integer;
                 PV, FV : Extended;
                 Frequency : TStFrequency;
                 Timing : TStPaymentTime) : Extended;
  {-Returns the interest payment per period in an annuity}

function Periods(Rate : Extended;
                 Pmt, PV, FV : Extended;
                 Frequency : TStFrequency;
                 Timing: TStPaymentTime) : Integer;
  {-Returns the number of periods for an annuity}

function PresentValue(Rate : Extended;
                      NPeriods : Integer;
                      Pmt, FV : Extended;
                      Frequency : TStFrequency;
                      Timing : TStPaymentTime) : Extended;
  {-Returns present value of an annity}

function ReceivedAtMaturity(Settlement, Maturity : TStDate;
                            Investment, Discount : Extended;
                            Basis : TStBasis) : Extended;
  {-Returns the amount received at Maturity for a fully invested security}

function RoundToDecimal(Value : Extended;
                        Places : Integer;
                        Bankers : Boolean) : Extended;
  {-Rounds a real value to the specified number of decimal places}

function TBillEquivYield(Settlement, Maturity : TStDate;
                         Discount : Extended) : Extended;
  {-Returns the bond-equivalent yield for a treasury bill}

function TBillPrice(Settlement, Maturity : TStDate;
                    Discount : Extended) : Extended;
  {-Returns the price per $100 face value for a treasury bill}

function TBillYield(Settlement, Maturity : TStDate;
                    Price : Extended) : Extended;
  {-Returns the yield for a treasury bill}

function VariableDecliningBalance(Cost, Salvage : Extended;
                                  Life : Integer;
                                  StartPeriod, EndPeriod, Factor : Extended;
                                  NoSwitch : boolean) : Extended;
  {-Variable rate declining balance depreciation}

function YieldDiscounted(Settlement, Maturity : TStDate;
                         Price, Redemption : Extended;
                         Basis : TStBasis) : Extended;
  {-Returns the annual yield for a discounted security}

function YieldPeriodic(Settlement, Maturity : TStDate;
                       Rate, Price, Redemption : Extended;
                       Frequency : TStFrequency;
                       Basis : TStBasis) : Extended;
  {-Returns the yield on a security that pays periodicinterest}

function YieldMaturity(Issue, Settlement, Maturity : TStDate;
                       Rate, Price : Extended;
                       Basis : TStBasis) : Extended;
  {-Returns the annual yield of a security that pays interest at Maturity}


{========================================================================}

implementation

const
  PaymentType : array[TStPaymentTime] of Integer = (0, 1);
    {Used for converting Timing to integer 0 or 1}

  CouponsPerYear : array[TStFrequency] of Integer = (1, 2, 4, 12);
    {Used for converting Frequency to integer 1, 2, 4, or 12}

  CouponPeriod : array[TStFrequency] of Integer = (12, 6, 3, 1);
    {Used for converting Frequency to duration}

  DefaultGuess : Extended = 0.1;
     {Starting point for rate approximation routines}

var
  RecipLn10 : Extended;
     {Used for common log computation}


{=================  Local routines used by this unit ==================}

procedure RaiseStFinError(Code : Longint);
begin
  Raise EStFinError.CreateResTP(Code, 0);
end;

{-------------------------------------------------------}

function Exp10(Exponent : Extended) : Extended;
  {-Returns 10^Exponent}
begin
  Result := Power(10.0, Exponent);
end;

{-------------------------------------------------------}

function Log10(Value : Extended) : Extended;
  {-Returns common log of Value}
begin
  Result := Ln(Value) * RecipLn10;
end;

{-------------------------------------------------------}

function DayCount(Day1, Day2 : TStDate; Basis : TStBasis) : LongInt;
  {-The number of days from Day1 to Day2 according to day count basis}
var
  BDT : TStBondDateType;
begin
  case Basis of
    BasisNASD     : BDT := bdt30360PSA;
    BasisEur30360 : BDT := bdt30E360;
  else
    BDT := bdtActual;
  end;
  Result := Longint(BondDateDiff(Day1, Day2, BDT));
end;

{-------------------------------------------------------}

function LastCoupon(Settlement, Maturity : TStDate;
  Frequency : TStFrequency) : TStDate;
  {-The last coupon date prior to settlement}
var
  Last         : TStDate;
  Months       : Integer;
begin
  Last := Maturity;
  Months := 0;
  while (Last >= Settlement) do begin
    Months := Months + CouponPeriod[Frequency];
    Last := IncDateTrunc(Maturity, -Months, 0);
  end;
  Result := Last;
end;

{-------------------------------------------------------}

function NextCoupon(Settlement, Maturity : TStDate;
  Frequency : TStFrequency) : TStDate;
  {-The next coupon date after settlement}
var
  Next : TStDate;
begin
  Next := LastCoupon(Settlement, Maturity, Frequency);
  Result := IncDateTrunc(Next, CouponPeriod[Frequency], 0);
end;

{-------------------------------------------------------}

function CouponsToMaturity(Settlement, Maturity : TStDate;
  Frequency : TStFrequency) : Integer;
  {-The number of coupons remaining after settlement}
var
  CouponDate : TStDate;
  Months     : Integer;
  Coupons    : Integer;
begin
  CouponDate := Maturity;
  Coupons := 0;
  Months := 0;
  while (CouponDate > Settlement) do begin
    Months := Months + CouponPeriod[Frequency];
    CouponDate := IncDateTrunc(Maturity, -Months, 0);
    Coupons := Coupons + 1;
  end;
  Result := Coupons;
end;

{-------------------------------------------------------}

function DayCountFraction(Day1, Day2, Settlement, Maturity : TStDate;
                          Frequency : TStFrequency;
                          Basis : TStBasis) : Extended;
  {-The number of days from Day1 to Day2 divided by days/year
    except for Act/Act which uses actual coupon period x frequency}
var
  Last, Next : TStDate;
  DPY        : Integer;
begin
  if (Basis = BasisActAct) then begin
    Last := LastCoupon(Settlement, Maturity, Frequency);
    Next := NextCoupon(Settlement, Maturity, Frequency);
    DPY := DayCount(Last, Next, Basis) * CouponsPerYear[Frequency];
  end else if (Basis = BasisAct365) then
    DPY := 365
  else
    DPY := 360;
  Result := DayCount(Day1, Day2, Basis) / DPY;
end;

{-------------------------------------------------------}

function BondDirtyPrice(Settlement, Maturity : TStDate;
                        Rate, Yield, Redemption : Extended;
                        Frequency : TStFrequency;
                        Basis : TStBasis) : Extended;
  {-Bond Price including interest accrued in current coupon period}
var
  C, DCF, Yw : Extended;
  Vn, Vdcf   : Extended;
  Next       : TStDate;
  N, W       : Integer;
begin
  W := CouponsPerYear[Frequency];
  C := Redemption * (Rate / W);
  Yw := Yield / W;
  N := CouponsToMaturity(Settlement, Maturity, Frequency);
  Next := NextCoupon(Settlement, Maturity, Frequency);
  DCF := DayCountFraction(Settlement, Next, Settlement, Maturity,
    Frequency, Basis);
  Vdcf := Power(1.0 / (1.0 + Yw), DCF * W);
  Vn := Power(1.0 / (1.0 + Yw), N - 1.0);
  Result := Vdcf * (( C * (1.0 - Vn) / Yw) + Redemption * Vn + C);
end;



{======================  Public Routines  ============================}


function AccruedInterestMaturity(Issue, Maturity : TStDate;
                                 Rate, Par : Extended;
                                 Basis : TStBasis) : Extended;
var
  DCF : Extended;
begin
  If (Rate <= 0.0) or (Par <= 0.0) or (Issue >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Issue, Maturity, Issue, Maturity,
    fqAnnual, Basis);
  Result := Par * Rate * DCF;
end;

{-------------------------------------------------------}

function AccruedInterestPeriodic(Issue, Settlement, Maturity : TStDate;
                                 Rate, Par : Extended;
                                 Frequency : TStFrequency;
                                 Basis : TStBasis) : Extended;
var
  Last : TStDate;
  DCF  : Extended;
begin
  if (Rate <= 0.0) or (Par <= 0.0) or (Issue >= Settlement) then
    RaiseStFinError(stscFinBadArg);
  Last := LastCoupon(Settlement, Maturity, Frequency);
  if (Issue > Last) then
    Last := Issue;
  DCF := DayCountFraction(Last, Settlement, Settlement, Maturity,
    Frequency, Basis);
  Result := Par * Rate * DCF;
end;

{-------------------------------------------------------}

function BondDuration(Settlement,Maturity : TStDate;
                      Rate, Yield : Extended;
                      Frequency : TStFrequency;
                      Basis : TStBasis) : Extended;
var
  B, dB : Extended;
  Yw    : Extended;
begin
  if (Rate < 0.0) or (Yield < 0.0) or (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  Yw := Yield / CouponsPerYear[Frequency];
  B := BondDirtyPrice(Settlement, Maturity, Rate, Yield, 100.0,
         Frequency, Basis);
  if (B <> 0.0) then begin
    dB := BondDirtyPrice(Settlement, Maturity, Rate, Yield + StDelta, 100.0,
      Frequency, Basis) - B;
    Result := -((1.0 + Yw) / B) * (dB / StDelta);
  end else
    Result := 0;
end;

{-------------------------------------------------------}

function BondPrice(Settlement, Maturity : TStDate;
                   Rate, Yield, Redemption : Extended;
                   Frequency : TStFrequency;
                   Basis : TStBasis) : Extended;
var
  B, DCF : Extended;
  Last   : TStDate;
begin
  if (Yield < 0.0) or (Rate < 0.0) or (Redemption <= 0) or
     (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  B := BondDirtyPrice(Settlement, Maturity, Rate, Yield, Redemption,
    Frequency, Basis);
  Last := LastCoupon(Settlement, Maturity, Frequency);
  DCF := DayCountFraction(Last, Settlement, Settlement, Maturity,
    Frequency, Basis);
  Result := B - Redemption * Rate * DCF;
end;

{-------------------------------------------------------}

function CumulativeInterest(Rate : Extended;
                            NPeriods : Integer;
                            PV : Extended;
                            StartPeriod, EndPeriod : Integer;
                            Frequency : TStFrequency;
                            Timing : TStPaymentTime) : Extended;
var
  P, CP : Extended;
begin
  if (Rate <=0.0) or (NPeriods <= 0) or (PV <= 0.0) or (StartPeriod < 1) or
    (EndPeriod < 1) or (StartPeriod > EndPeriod) then
    RaiseStFinError(stscFinBadArg);
  P := Payment(Rate, NPeriods, PV, 0.0, Frequency, Timing);
  CP := CumulativePrincipal(Rate, NPeriods, PV, StartPeriod, EndPeriod,
    Frequency, Timing);
  Result := P * (EndPeriod - (StartPeriod - 1.0)) - CP;
end;

{-------------------------------------------------------}

function CumulativePrincipal(Rate : Extended;
                             NPeriods : Integer;
                             PV : Extended;
                             StartPeriod, EndPeriod : Integer;
                             Frequency : TStFrequency;
                             Timing : TStPaymentTime) : Extended;
var
  P : Extended;
begin
  if (Rate <=0.0) or (NPeriods <= 0) or (PV <= 0.0) or (StartPeriod < 1) or
    (EndPeriod < 1) or (StartPeriod > EndPeriod) then
    RaiseStFinError(stscFinBadArg);
  P := Payment(Rate, NPeriods, PV, 0.0, Frequency, Timing);
  Result := FutureValue(Rate, StartPeriod - 1, P, PV, Frequency, Timing) -
    FutureValue(Rate, EndPeriod, P, PV, Frequency, Timing);
end;

{-------------------------------------------------------}

function DecliningBalance(Cost, Salvage : Extended;
                          Life, Period, Month : Integer) : Extended;
var
  Rate : Extended;
  DPv  : Extended;
  TDPv : Extended;
  I    : Integer;
begin
  if (Cost <= 0.0) or (Cost < Salvage) or (Period < 1) or (Life < 2) or
    (Period > (Life + 1)) then
    RaiseStFinError(stscFinBadArg);
  DPv := 0.0;
  TDPv := 0.0;
  if (Salvage = 0) then                                              
    Salvage := 0.001;                                                
  if (Month = 0) then                                                
    Month := 12;                                                     
  Rate := RoundToDecimal(1.0 - Power(Salvage / Cost, 1.0 / Life), 3, false);
  for I := 1 to Period do begin
    if (I = 1) then
      DPv := (Cost * Rate * Month) / 12.0                   {1st Period}
    else if (I = (Life + 1)) then
      DPv := (Cost - TDPv) * Rate * (12.0 - Month) / 12.0   {Last Period}
    else
      DPv := (Cost - TDPv) * Rate;                          {All the rest}
    TDpv := TDpv + Dpv
  end;
  Result := RoundToDecimal(Dpv, 3, False);
end;

{-------------------------------------------------------}

function DiscountRate(Settlement, Maturity : TStDate;
                      Price, Redemption : Extended;
                      Basis : TStBasis) : Extended;
var
  DCF : Extended;
begin
  If (Price <= 0.0) or (Redemption <= 0.0) or (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, Basis);
  Result := (Redemption - Price) / (Redemption * DCF);
end;

  {-------------------------------------------------------}

function DollarToDecimal(FracDollar : Extended;
                         Fraction : Integer) : Extended;
var
  I, F, N : Extended;
begin
  if (Fraction < 1) then
    RaiseStFinError(stscFinBadArg);
  I := Int(FracDollar);                   {Integral part}
  N := Int(Log10(Fraction) + 1.0);        {Number of decimal places}
  F := Frac(FracDollar);                  {Fractional part}
  Result := I + (F * Exp10(N) / Fraction);
end;

{-------------------------------------------------------}

function DollarToDecimalText(DecDollar : Extended) : string;
var
  A, P  : Extended;
  N, I  : Integer;
  Str   : string;
  T     : Longint;
  CentVal : Integer;                                                 
const
  Orders : array[0..5] of string = ('', 'Thousand ', 'Million ',     
    'Billion ', 'Trillion ', 'Quadrillion ');                        

  function Text100(Num: Longint) : string;
    {formats an integer in the range 0 to 999}
  var
    I, J : Integer;
    A, T : Longint;
    S    : string;
  const
    Tens : array[0..9] of string =
             ('', '', 'Twenty', 'Thirty', 'Forty', 'Fifty',
              'Sixty', 'Seventy', 'Eighty', 'Ninety');
    Ones : array[0..19] of string =
             ('', 'One', 'Two', 'Three', 'Four', 'Five',
              'Six', 'Seven', 'Eight', 'Nine', 'Ten',
              'Eleven', 'Twelve', 'Thirteen', 'Fourteen', 'Fifteen',
              'Sixteen', 'Seventeen', 'Eighteen', 'Nineteen');
  begin
    S := '';
    I := 0;
    J := 0;
    Result := S;
    if (Num = 0) then
      Exit;
    A := Num;
    T := A div 100;
    if (T > 0) then begin
      I := T;                          {I = Hundreds digit}
      A := A - (T * 100);
    end;
    T := A div 10;
    if (T > 1) then begin
      J := T;                          {J = Tens digit}
      A := A - (T * 10);               {A = Ones digit}
    end;
    if (I > 0) then
      S := Ones[I] + ' Hundred';
    if (J > 0) then begin
      if (I > 0) then
        S := S + ' ' + Tens[J]
      else
        S := S + Tens[J];
    end;
    if (A > 0) then begin
      if (J > 0) then
        S := S + '-';
      if (I > 0) and (J = 0) then
        S := S + ' ' + Ones[A]
      else
        S := S + Ones[A];
    end;
    Result := S;
  end;

begin
  Str := '';
  if (DecDollar < 0) then                                            
    RaiseStFinError(stscFinBadArg);                                  
  if (DecDollar > 0) then begin                                      
    N := Trunc(Log10(DecDollar));
    if (N > 17) then  {DecDollar too large}
      RaiseStFinError(stscFinBadArg);
    A := DecDollar;
    for I := N downto 0 do begin
      P := Int(Exp10(I * 3));
      T := Trunc(A / P);
      if (T > 0) then
        Str := Str + {' ' +} Text100(T) + ' ' + Orders[I];
      A := A - (T * P);
    end;
  end;                                                               
  if (Str = '') then                                                 
    Str := 'Zero ';                                                  
  Str := Str + 'and ';                                               
  CentVal := Round(Frac(DecDollar) * 100);                           
  if (CentVal < 10) then                                             
    Str := Str + '0';                                                
  Result := Str + IntToStr(CentVal) + '/100';                        
end;

{-------------------------------------------------------}

function DollarToFraction(DecDollar : Extended;
                          Fraction : Integer) : Extended;
var
  I, F, N : Extended;
begin
  if (Fraction < 1) then
    RaiseStFinError(stscFinBadArg);
  I := Int(DecDollar);                  {Integral part}
  N := Int(Log10(Fraction) + 1.0);      {Number of decimal places}
  F := Frac(DecDollar);                 {Fractional part}
  Result := I + (F * Fraction / Exp10(N));
end;

{-------------------------------------------------------}

function DollarToFractionStr(FracDollar : Extended;
                             Fraction : Integer) : string;
var
  I, F, N : Extended;
begin
  Result := '';
  if (Fraction < 1) then
    RaiseStFinError(stscFinBadArg);
  I := Int(FracDollar);                 {Integral part}
  N := Int(Log10(Fraction) + 1.0);      {Number of decimal places}
  F := Frac(FracDollar) * Exp10(N);     {Fractional part}
  Result := IntToStr(Trunc(I));
  if (F > 0) then
    Result := Result  + ' ' + FloatToStrF(F, ffNumber, Trunc(N), 0) +
      '/' + IntToStr(Fraction);
end;

{-------------------------------------------------------}

function EffectiveInterestRate(NominalRate : Extended;
                               Frequency : TStFrequency) : Extended;
var
  W : Integer;
begin
  if (NominalRate <= 0.0) then
    RaiseStFinError(stscFinBadArg);
  W := CouponsPerYear[Frequency];
  Result := Power(1.0 + NominalRate / W, W) - 1.0;
end;

{-------------------------------------------------------}

function FutureValue(Rate : Extended;
                     NPeriods : Integer;
                     Pmt, PV : Extended;
                     Frequency : TStFrequency;
                     Timing: TStPaymentTime) : Extended;
var
  S, Rw  : Extended;
  PT     : Integer;

begin
  PT := PaymentType[Timing];
  Rw := Rate / CouponsPerYear[Frequency];
  S := Power(1.0 + Rw, NPeriods);
  Result := -((PV * S) + Pmt * (S - 1.0) * (1.0 + Rw * PT) / Rw);
end;

{-------------------------------------------------------}

  function FutureValueSchedule(Principal : Extended;
                               const Schedule : array of Double) : Extended;
  begin
    Result := FutureValueSchedule16(Principal, Schedule,
                                    High(Schedule) + 1);
  end;

function FutureValueSchedule16(Principal : Extended;
  const Schedule; NRates : Integer) : Extended;
var
  I : Integer;
begin
  Result := Principal;
  for I := 0 to (NRates - 1) do
    Result := Result * (1.0 + TDoubleArray(Schedule)[I]);
end;

{-------------------------------------------------------}

function InterestRate(NPeriods : Integer;
                      Pmt, PV, FV : Extended;
                      Frequency : TStFrequency;
                      Timing : TStPaymentTime;
                      Guess : Extended) : Extended;
var
  Rate     : Extended;
  NextRate : Extended;
  T, dT    : Extended;
  Count    : Integer;
begin
  Count := 0;
  NextRate := Guess;
  if (Guess = 0.0) then
    NextRate := DefaultGuess;
    {Solve FV(rate) = FV for rate by Newton's method}
  repeat
    Rate := NextRate;
    if (Rate <= - CouponsPerYear[Frequency]) then                    
      Rate := -0.999 * CouponsPerYear[Frequency];                    
     T := FutureValue(Rate, NPeriods, Pmt, PV, Frequency, Timing) - FV;
    dT := FutureValue(Rate + StDelta, NPeriods, Pmt, PV, Frequency,
      Timing) - FV - T;
    if (dT = 0.0) then
      Count := StMaxIterations
    else
      NextRate := Rate - StDelta * T / dT;
    Inc(Count);
  until (Abs(NextRate - Rate) < StEpsilon) or (Count > StMaxIterations);
  if (Count > StMaxIterations) then
    RaiseStFinError(stscFinNoConverge);
  Result := NextRate;
end;

{-------------------------------------------------------}

  function InternalRateOfReturn(const Values : array of Double;
                                Guess : Extended) : Extended;
  begin
    Result := InternalRateOfReturn16(Values, High(Values) + 1, Guess);
  end;

function InternalRateOfReturn16(const Values;
                                NValues : Integer;
                                Guess : Extended) : Extended;
var
  Rate     : Extended;
  NextRate : Extended;
  PV       : Extended;
  dPV      : Extended;
  Count    : Integer;
begin
  Count := 0;
  NextRate := Guess;
  if (Guess = 0.0) then
    NextRate := DefaultGuess;
    {Solve NPV(Rate) = 0 for rate by Newton's method}
  repeat
    Rate := NextRate;
    if (Rate <= -1.0) then                                           
      Rate := -0.999;                                                
    PV := NetPresentValue16(Rate, Values, NValues);
    dPV := NetPresentValue16(Rate + StDelta, Values, NValues) - PV;
    if (dPV = 0.0) then
      Count := StMaxIterations
    else
      NextRate := Rate - (StDelta * PV) / dPV;
    Inc(Count);
  until (Abs(NextRate - Rate) < StEpsilon) or (Count > StMaxIterations);
  if (Count > StMaxIterations) then
    RaiseStFinError(stscFinNoConverge);
  Result := NextRate;
end;

{-------------------------------------------------------}

function IsCardValid(const S : string) : Boolean;
const
  Ord0 = Ord('0');
var
  Temp    : string;
  I, J, K : Integer;
begin
  Result := False;
  Temp := '';
  for I := 1 to Length(S) do
    if (S[I] in ['0'..'9']) then
      Temp := Temp + S[I];
  if Temp = '' then
    Exit;
  K := 0;
  I := 1;
  if not Odd(Length(Temp)) then begin
    J := Ord(Temp[I]) - Ord0;
    J := J shl 1;
    if J > 9 then
      J := J - 9;
    K := K + J;
    Inc(I);
  end;
  while I <= Length(Temp) do begin
    K := K + Ord(Temp[I]) - Ord0;
    Inc(I);
    if I > Length(Temp) then
      Break;
    J := Ord(Temp[I]) - Ord0;
    J := J shl 1;
    if J > 9 then
      J := J - 9;
    K := K + J;
    Inc(I);
  end;
  Result := (K mod 10 = 0);
end;

{-------------------------------------------------------}

function ModifiedDuration(Settlement, Maturity : TStDate;
                          Rate, Yield : Extended;
                          Frequency : TStFrequency;
                          Basis : TStBasis) : Extended;
begin
  if (Rate < 0.0) or (Yield < 0.0) or (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  Result := BondDuration(Settlement, Maturity, Rate, Yield,
    Frequency, Basis)/ (1.0 + Yield / CouponsPerYear[Frequency]);
end;

{-------------------------------------------------------}

  function ModifiedIRR(const Values : array of Double;
                       FinanceRate, ReinvestRate : Extended) : Extended;
  begin
    Result := ModifiedIRR16(Values, High(Values) + 1, FinanceRate,
                            ReinvestRate);
  end;

function ModifiedIRR16(const Values;
                       NValues : Integer;
                       FinanceRate, ReinvestRate : Extended) : Extended;
var
  NPVPos : Extended;
  NPVNeg : Extended;
  Val    : Extended;
  Rn, Fn : Extended;
  I      : Integer;
begin
  NPVPos := 0.0;
  NPVNeg := 0.0;
  for I := 0 to (NValues - 1) do begin
    Val := TDoubleArray(Values)[I];
    if (Val > 0.0) then
      NPVPos := NPVPos + Val / Power(1.0 + ReinvestRate, I + 1.0)
    else
      NPVNeg := NPVNeg + Val / Power(1.0 + FinanceRate, I + 1.0);
  end;
  Rn := Power(1.0 + ReInvestRate, NValues);
  Fn := 1.0 + FinanceRate;
  Result := Power(-NPVPos * Rn / (NPVNeg * Fn), 1.0 / (NValues - 1.0)) - 1.0;
end;

{-------------------------------------------------------}

  function NetPresentValue(Rate : Extended;
                           const Values : array of Double) : Extended;
  begin
    Result := NetPresentValue16(Rate, Values, High(Values) + 1);
  end;

function NetPresentValue16(Rate : Extended;
                           const Values;
                           NValues : Integer) : Extended;
var
  I : Integer;
begin
  Result := 0;
  for I := 0 to (NValues - 1) do
    Result := Result + TDoubleArray(Values)[I] / Power(1.0 + Rate, I + 1.0);
end;

{-------------------------------------------------------}

function NominalInterestRate(EffectRate : Extended;
                             Frequency : TStFrequency) : Extended;
var
  W : Extended;
begin
  if (EffectRate <= 0.0) then
    RaiseStFinError(stscFinBadArg);
  W := CouponsPerYear[Frequency];
  Result := W * (Power(EffectRate + 1.0, 1.0 / W) - 1.0);
end;

{-------------------------------------------------------}

  function NonperiodicIRR(const Values : array of Double;
                          const Dates : array of TStDate;
                          Guess : Extended) : Extended;
  begin
    Result := NonPeriodicIRR16(Values, Dates, High(Values) + 1, Guess);
  end;

function NonperiodicIRR16(const Values;
                          const Dates;
                          NValues : Integer;
                          Guess : Extended) : Extended;
var
  Rate     : Extended;
  NextRate : Extended;
  PV, dPV  : Extended;
  Count    : Integer;
begin
  Count := 0;
  NextRate := Guess;
  if (Guess = 0.0) then
    NextRate := DefaultGuess;
    {Solve XNPV(Rate) = 0 for rate by Newton's method}
  repeat
    Rate := NextRate;
    if (Rate <= -1.0) then                                           
      Rate := -0.999;                                                
    PV := NonPeriodicNPV16(Rate, Values, Dates, NValues);
    dPV := NonPeriodicNPV16(Rate + StDelta, Values, Dates, NValues) - PV;
    if (dPV = 0.0) then
      Count := StMaxIterations
    else
      NextRate := Rate - (StDelta * PV) / dPV;
    Inc(Count);
  until (Abs(NextRate - Rate) < StEpsilon) or (Count > StMaxIterations);
  if (Count > StMaxIterations) then
    RaiseStFinError(stscFinNoConverge);
  Result := NextRate;
end;

{-------------------------------------------------------}

  function NonperiodicNPV(Rate : Extended;
                          const Values : array of Double;
                          const Dates : array of TStDate) : Extended;
  begin
    Result := NonperiodicNPV16(Rate, Values, Dates, High(Values) + 1);
  end;

function NonperiodicNPV16(Rate : Extended;
                          const Values;
                          const Dates;
                          NValues : Integer) : Extended;
var
  Day1 : TStDate;
  Diff : Double;
  I    : Integer;
begin
  Result := 0.0;
  Day1 := TStDateArray(Dates)[0];
  for I := 0 to (NValues - 1) do begin
    Diff := TStDateArray(Dates)[I] - Day1;
    if (Diff < 0) then
      RaiseStFinError(stscFinBadArg);
    Result := Result + TDoubleArray(Values)[I] / Power(1.0 + Rate, Diff / 365.0);
  end;
end;

{-------------------------------------------------------}

function Payment(Rate : Extended;
                 NPeriods : Integer;
                 PV, FV : Extended;
                 Frequency : TStFrequency;
                 Timing : TStPaymentTime) : Extended;
var
  PT, Rw, S : Extended;
begin
  PT := PaymentType[Timing];
  Rw := Rate / CouponsPerYear[Frequency];
  S := Power(1.0 + Rw, NPeriods);
  Result := Rw * (FV - PV * S) / ((S - 1.0) * (1.0 + Rw * PT));
end;

{-------------------------------------------------------}
function Periods(Rate : Extended;
                 Pmt, PV, FV : Extended;
                 Frequency : TStFrequency;
                 Timing: TStPaymentTime) : Integer;
var
  S, Rw  : Extended;

begin
  Rw := Rate / CouponsPerYear[Frequency];
  S := Pmt * (1.0 + Rw * PaymentType[Timing]);
  Result := Round(Ln((Rw*FV + S) / (Rw*PV + S)) / Ln(1.0 + Rw));
end;

{-------------------------------------------------------}

function PresentValue(Rate : Extended;
                      NPeriods : Integer;
                      Pmt, FV : Extended;
                      Frequency : TStFrequency;
                      Timing : TStPaymentTime) : Extended;
var
  PT, Rw, S  : Extended;
begin
  PT := PaymentType[Timing];
  Rw := Rate / CouponsPerYear[Frequency];
  S := Power(1.0 + Rw, -NPeriods);
  Result := (FV * S) + Pmt * (S - 1.0) * (1.0 + Rw * PT) / Rw;
end;

{-------------------------------------------------------}

function ReceivedAtMaturity(Settlement, Maturity : TStDate;
                            Investment, Discount : Extended;
                            Basis : TStBasis) : Extended;
var
  DCF : Extended;
begin
  if (Investment <= 0.0) or (Discount <= 0.0) or (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, Basis);
  Result := Investment / (1.0 - Discount * DCF);
end;

{-------------------------------------------------------}

 {revised}
function RoundToDecimal(Value : Extended;
                        Places : Integer;
                        Bankers : Boolean) : Extended;
var
  Val, IV, N, F : Extended;
  T             : Integer;
begin
  IV := 0;
  N := Exp10(Places);
  if (Places > 0) then
    IV := Int(Value);
  Val := (Value - IV) * N;
  T := Trunc(Val);
  F := (Val - T);
  if Bankers then
    Val := Round(Val) / N        {Delphi's Round does Bankers}
  else begin
    if Abs(Round(10.0 * F)) >= 5 then begin
      if (F > 0) then
        Val := (T + 1.0) / N
      else
        Val := (T - 1.0) / N;
    end else
      Val := T / N;
  end;
  Result := Val + IV;
end;

{-------------------------------------------------------}

function TBillEquivYield(Settlement, Maturity : TStDate;
                         Discount : Extended) : Extended;
var
  DCF : Extended;
begin
  if (Discount <= 0.0) or (Settlement > Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, BasisAct360);
  if (DCF > 1.0) then
    RaiseStFinError(stscFinBadArg);
  Result := (365.0 / 360.0) * Discount / (1.0 - Discount * DCF);
end;

{-------------------------------------------------------}

function TBillPrice(Settlement, Maturity : TStDate;
                    Discount : Extended) : Extended;
var
  DCF : Extended;
begin
  if (Discount <= 0.0) or (Settlement > Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, BasisAct360);
  if (DCF > 1.0) then
    RaiseStFinError(stscFinBadArg);
  Result := 100.0 * ( 1.0 - Discount * DCF);
end;

{-------------------------------------------------------}

function TBillYield(Settlement, Maturity : TStDate;
                    Price : Extended) : Extended;
var
  DCF : Extended;
begin
  if (Price <= 0.0) or (Settlement > Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, BasisAct360);
  if (DCF > 1.0) then
    RaiseStFinError(stscFinBadArg);
  Result := ((100.0 - Price) / Price) * (1.0 / DCF);
end;

{-------------------------------------------------------}

function VariableDecliningBalance(Cost, Salvage : Extended;
                                  Life : Integer;
                                  StartPeriod, EndPeriod, Factor : Extended;
                                  NoSwitch : Boolean) : Extended;
var
  VDB   : Extended;
  SLD   : Extended;
  Rate  : Extended;
begin
  if (Cost <= 0.0) or (Cost < Salvage) or (Life < 2) or (EndPeriod > Life) or
    (StartPeriod > EndPeriod) or (StartPeriod < 0) then
    RaiseStFinError(stscFinBadArg);
  if (Factor = 0.0) then
    Rate := 2.0 / Life
  else
    Rate := Factor / Life;
  SLD := (Cost - Salvage) * (EndPeriod - StartPeriod) / Life;
  VDB := Cost * (Power(1.0 - Rate, StartPeriod) - Power(1.0 - Rate, EndPeriod));
  if (not NoSwitch) and (SLD > VDB) then
    Result := SLD
  else
    Result := VDB;
end;

{-------------------------------------------------------}

function YieldDiscounted(Settlement, Maturity : TStDate;
                         Price, Redemption : Extended;
                         Basis : TStBasis) : Extended;
var
  DCF : Extended;
begin
  if (Price <= 0.0) or (Redemption <= 0.0) or (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCF := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, Basis);
  Result := (Redemption - Price) / (Price * DCF);
end;

{-------------------------------------------------------}

function YieldPeriodic(Settlement, Maturity : TStDate;
                       Rate, Price, Redemption : Extended;
                       Frequency : TStFrequency;
                       Basis : TStBasis) : Extended;
var
  Yield     : Extended;
  NextYield : Extended;
  P, dP     : Extended;
  Count     : Integer;
begin
  if (Price <= 0.0) or (Rate < 0.0) or (Redemption <= 0.0) or
    (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  Count := 0;
  NextYield := Rate;
  repeat  {Solve B = BondPrice(yield) - Price = 0 by Newton's method}
    if (NextYield > 0) then                                          
      Yield := NextYield                                             
    else                                                             
      Yield := 0.001;                                                
    P := BondPrice(Settlement, Maturity, Rate, Yield, Redemption,
      Frequency, Basis) - Price;
    dP := BondPrice(Settlement, Maturity, Rate, Yield + StDelta,
      Redemption, Frequency, Basis) - Price - P;
    if (dP = 0.0) then
      Count := StMaxIterations
    else
      NextYield := Yield - StDelta * P / dP;
    Inc(Count);
  until (Abs(NextYield - Yield) < StEpsilon) or (Count > StMaxIterations);
  if (Count > StMaxIterations) then
    RaiseStFinError(stscFinNoConverge);
  Result := NextYield;
end;

{-------------------------------------------------------}

function YieldMaturity(Issue, Settlement, Maturity : TStDate;
                       Rate, Price : Extended;
                       Basis : TStBasis) : Extended;
var
  DCFim, DCFsm, DCFis : Extended;
begin
  if (Price <= 0.0) or (Rate < 0.0) or (Settlement < Issue) or
    (Settlement >= Maturity) then
    RaiseStFinError(stscFinBadArg);
  DCFim := DayCountFraction(Issue, Maturity, Settlement, Maturity,
    fqAnnual, Basis);
  DCFsm := DayCountFraction(Settlement, Maturity, Settlement, Maturity,
    fqAnnual, Basis);
  DCFis := DayCountFraction(Issue, Settlement, Settlement, Maturity,
    fqAnnual, Basis);
  Result := 100.0 * (1.0 + Rate * DCFim);
  Result := Result / (Price + 100.0 * Rate * DCFis);
  Result := (Result - 1.0) / DCFsm;
end;



initialization
  RecipLn10 := 1.0 / Ln(10.0);
end.

