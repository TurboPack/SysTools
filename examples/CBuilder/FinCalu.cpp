// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower SysTools
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1996-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop

#include "FinCalu.h"
//---------------------------------------------------------------------------
#pragma link "Grids"
#pragma link "StFin"
#pragma link "StDate"
#pragma link "StDateSt"
#pragma resource "*.dfm"
TFinCalForm *FinCalForm;
//---------------------------------------------------------------------------
__fastcall TFinCalForm::TFinCalForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::FT(TWinControl* F, short T, String S)
{
  TEdit* edit;
  edit = (TEdit*) F;
  edit->Enabled = true;
  edit->Color = clYellow;
  edit->TabOrder = T;
  edit->Hint = S;
  edit->ShowHint = true;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupAccruedInterestPeriodic()
{
  FT(D1Edit, 1, "Issue");
  FT(D2Edit, 2, "Settlement");
  FT(D3Edit, 3, "Maturity");
  FT(E1Edit, 4, "Rate");
  FT(E2Edit, 5, "Par");
  FT(FreqEdit, 6, "Frequency");
  FT(BasEdit, 7, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupAccruedInterestMaturity()
{
  FT(D1Edit, 1, "Issue");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Rate");
  FT(E2Edit, 4, "Par");
  FT(BasEdit, 5, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupCumulativeInterest()
{
  FT(E1Edit, 1, "Rate");
  FT(I1Edit, 2, "NPeriods");
  FT(E2Edit, 3, "Present Value");
  FT(I2Edit, 4, "Start Period");
  FT(I3Edit, 5, "End Period");
  FT(FreqEdit, 6, "Frequency");
  FT(TimEdit, 7, "Timing");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupCumulativePrincipal()
{
  SetupCumulativeInterest();
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupDiscountRate()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Price");
  FT(E2Edit, 4, "Redemption");
  FT(BasEdit, 5, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupEffectiveInterestRate()
{
  FT(E1Edit, 1, "Nominal Rate");
  FT(FreqEdit, 2, "Frequency");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupNominalInterestRate()
{
  FT(E1Edit, 1, "Effective Rate");
  FT(FreqEdit, 2, "Frequency");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupInterestRate()
{
  FT(I1Edit, 1, "NPeriods");
  FT(E1Edit, 2, "Payment");
  FT(E2Edit, 3, "Present Value");
  FT(E3Edit, 4, "Future Value");
  FT(FreqEdit, 5, "Frequency");
  FT(TimEdit, 6, "Timing");
  FT(E4Edit, 7, "Guess");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupReceivedAtMaturity()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Investment");
  FT(E2Edit, 4, "Discount");
  FT(BasEdit, 5, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupYieldPeriodic()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Rate");
  FT(E2Edit, 4, "Price");
  FT(E3Edit, 5, "Redemption");
  FT(FreqEdit, 6, "Frequency");
  FT(BasEdit, 7, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupYieldDiscounted()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Price");
  FT(E2Edit, 4, "Redemption");
  FT(BasEdit, 5, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupYieldMaturity()
{
  FT(D1Edit, 1, "Issue");
  FT(D2Edit, 2, "Settlement");
  FT(D3Edit, 3, "Maturity");
  FT(E1Edit, 4, "Rate");
  FT(E2Edit, 5, "Price");
  FT(BasEdit, 6, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupTBillEquivYield()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Discount");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupTBillPrice()
{
  SetupTBillEquivYield();
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupTBillYield()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Price");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupBondDuration()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Rate");
  FT(E2Edit, 4, "Yield");
  FT(FreqEdit, 5, "Frequency");
  FT(BasEdit, 6, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupModifiedDuration()
{
  SetupBondDuration();
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupFutureValueSchedule()
{
  FT(E1Edit, 1, "Principal");
  FT(VAEdit, 2, "Schedule");
  FT(I1Edit, 3, "NRates");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupModifiedIRR()
{
  FT(VAEdit, 1, "Values");
  FT(I1Edit, 2, "NValues");
  FT(E1Edit, 3, "Finance Rate");
  FT(E2Edit, 4, "Reinvest Rate");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupNonperiodicIRR()
{
  FT(I1Edit, 1, "NValues");
  FT(VAEdit, 2, "Values");
  FT(DAEdit, 3, "Dates");
  FT(E1Edit, 4, "Guess");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupNonperiodicNPV()
{
  FT(I1Edit, 1, "NValues");
  FT(E1Edit, 2, "Rate");
  FT(VAEdit, 3, "Values");
  FT(DAEdit, 4, "Dates");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupDecliningBalance()
{
  FT(E1Edit, 1, "Cost");
  FT(E2Edit, 2, "Salvage");
  FT(I1Edit, 3, "Life");
  FT(I2Edit, 4, "Period");
  FT(I3Edit, 5, "Month");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupVariableDecliningBalance()
{
  FT(E1Edit, 1, "Cost");
  FT(E2Edit, 2, "Salvage");
  FT(I1Edit, 3, "Life");
  FT(E3Edit, 4, "Start");
  FT(E4Edit, 5, "End");
  FT(E5Edit, 6, "Factor");
  FT(BoolEdit, 7, "No Switch");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupDollarToDecimal()
{
  FT(E1Edit, 1, "Fractional Dollar");
  FT(I1Edit, 2, "Fraction");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupDollarToFraction()
{
  FT(E1Edit, 1, "Decimal Dollar");
  FT(I1Edit, 2, "Fraction");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupDollarToDecimalText()
{
  FT(E1Edit, 1, "Decimal Dollar");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupDollarToFractionStr()
{
  FT(E1Edit, 1, "Fractional Dollar");
  FT(I1Edit, 2, "Fraction");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupRoundToDecimal()
{
  FT(E1Edit, 1, "Value");
  FT(I1Edit, 2, "Places");
  FT(BoolEdit, 3, "Bankers");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupIsCardValid()
{
  FT(StrEdit, 1, "Card Number");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupNetPresentValue()
{
  FT(E1Edit, 1, "Rate");
  FT(I1Edit, 2, "NValues");
  FT(VAEdit, 3, "Values");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupFutureValue()
{
  FT(E1Edit, 1, "Rate");
  FT(I1Edit, 2, "NPeriods");
  FT(E2Edit, 3, "Payment");
  FT(E3Edit, 4, "Present Value");
  FT(FreqEdit, 5, "Frequency");
  FT(TimEdit, 6, "Timing");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupPresentValue()
{
  FT(E1Edit, 1, "Rate");
  FT(I1Edit, 2, "NPeriods");
  FT(E2Edit, 3, "Payment");
  FT(E3Edit, 4, "Future Value");
  FT(FreqEdit, 5, "Frequency");
  FT(TimEdit, 6, "Timing");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupBondPrice()
{
  FT(D1Edit, 1, "Settlement");
  FT(D2Edit, 2, "Maturity");
  FT(E1Edit, 3, "Rate");
  FT(E2Edit, 4, "Yield");
  FT(E3Edit, 5, "Redemption");
  FT(FreqEdit, 6, "Frequency");
  FT(BasEdit, 7, "Basis");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupPayment()
{
  FT(E1Edit, 1, "Rate");
  FT(I1Edit, 2, "NPeriods");
  FT(E2Edit, 3, "Present Value");
  FT(E3Edit, 4, "Future Value");
  FT(FreqEdit, 5, "Frequency");
  FT(TimEdit, 6, "Timing");
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::SetupInternalRateOfReturn()
{
  FT(I1Edit, 1, "NValues");
  FT(VAEdit, 2, "Values");
  FT(E1Edit, 3, "Guess");
}
//---------------------------------------------------------------------------



void __fastcall TFinCalForm::GoBtnClick(TObject *Sender)
{
  long double FR = 0;
  String SR = "";
  switch (Functions->ItemIndex) {
    case 0  : FR = AccruedInterestMaturity(DateVar[0], DateVar[1], ExtVar[0],
                 ExtVar[1], BasVar);
              break;
    case 1  : FR = AccruedInterestPeriodic(DateVar[0], DateVar[1], DateVar[2],
                 ExtVar[0], ExtVar[1], FreqVar, BasVar);
              break;
    case 2  : FR = BondDuration(DateVar[0], DateVar[1], ExtVar[0], ExtVar[1],
                 FreqVar, BasVar);
              break;
    case 3  : FR = BondPrice(DateVar[0], DateVar[1], ExtVar[0], ExtVar[1],
                 ExtVar[2], FreqVar, BasVar);
              break;
    case 4  : FR = CumulativeInterest(ExtVar[0], IntVar[0], ExtVar[1], IntVar[1],
                 IntVar[2], FreqVar, TimVar);
              break;
    case 5  : FR = CumulativePrincipal(ExtVar[0], IntVar[0], ExtVar[1], IntVar[1],
                 IntVar[2], FreqVar, TimVar);
              break;
    case 6  : FR = DecliningBalance(ExtVar[0], ExtVar[1], IntVar[0], IntVar[1],
                 IntVar[2]);
              break;
    case 7  : FR = DiscountRate(DateVar[0], DateVar[1], ExtVar[0], ExtVar[1],
                 BasVar);
              break;
    case 8  : FR = DollarToDecimal(ExtVar[0], IntVar[0]);

              break;
    case 9  : SR = DollarToDecimalText(ExtVar[0]);

              break;
    case 10 : FR = DollarToFraction(ExtVar[0], IntVar[0]);

              break;
    case 11 : SR = DollarToFractionStr(ExtVar[0], IntVar[0]);

              break;
    case 12 : FR = EffectiveInterestRate(ExtVar[0], FreqVar);

              break;
    case 13 : FR = FutureValue(ExtVar[0], IntVar[0], ExtVar[1], ExtVar[2],
                 FreqVar, TimVar);
              break;
    case 14 : FR = FutureValueSchedule16(ExtVar[0], ValArray, IntVar[0]);

              break;
    case 15 : FR = InterestRate(IntVar[0], ExtVar[0], ExtVar[1], ExtVar[2],
                 FreqVar, TimVar, ExtVar[3]);
              break;
    case 16 : FR = InternalRateOfReturn16(ValArray, IntVar[0], ExtVar[0]);

              break;
    case 17 : if (IsCardValid(StrVar))
                SR = "Valid card number";
              else
                SR = "Invalid card number";
              break;
    case 18 : FR = ModifiedDuration(DateVar[0], DateVar[1], ExtVar[2],
                 ExtVar[1], FreqVar, BasVar);
              break;
    case 19 : FR = ModifiedIRR16(ValArray, IntVar[0], ExtVar[0], ExtVar[1]);

              break;
    case 20 : FR = NetPresentValue16(ExtVar[0], ValArray, IntVar[0]);

              break;
    case 21 : FR = NominalInterestRate(ExtVar[0], FreqVar);

              break;
    case 22 : FR = NonperiodicIRR16(ValArray, DateArray, IntVar[0], ExtVar[0]);

              break;
    case 23 : FR = NonperiodicNPV16(ExtVar[0], ValArray, DateArray, IntVar[0]);

              break;
    case 24 : FR = Payment(ExtVar[0], IntVar[0], ExtVar[1], ExtVar[2],
                 FreqVar, TimVar);
              break;
    case 25 : FR = PresentValue(ExtVar[0], IntVar[0], ExtVar[1], ExtVar[2],
                 FreqVar, TimVar);
              break;
    case 26 : FR = ReceivedAtMaturity(DateVar[0], DateVar[1], ExtVar[0],
                 ExtVar[1], BasVar);
              break;
    case 27 : FR = RoundToDecimal(ExtVar[0], IntVar[0], BoolVar);

              break;
    case 28 : FR = TBillEquivYield(DateVar[0], DateVar[1], ExtVar[0]);

              break;
    case 29 : FR = TBillPrice(DateVar[0], DateVar[1], ExtVar[0]);

              break;
    case 30 : FR = TBillYield(DateVar[0], DateVar[1], ExtVar[0]);

              break;
    case 31 : FR = VariableDecliningBalance(ExtVar[0], ExtVar[1], IntVar[0],
                 ExtVar[2], ExtVar[3], ExtVar[4], BoolVar);
              break;
    case 32 : FR = YieldPeriodic(DateVar[0], DateVar[1], ExtVar[0], ExtVar[1],
                 ExtVar[2], FreqVar, BasVar);
              break;
    case 33 : FR = YieldDiscounted(DateVar[0], DateVar[1], ExtVar[0],
                 ExtVar[1], BasVar);
              break;
    case 34 : FR = YieldMaturity(DateVar[0], DateVar[1], DateVar[2], ExtVar[0],
                 ExtVar[1], BasVar);
              break;
  }
  FResult->Text = FloatToStr(FR);
  SResult->Text = SR;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::FunctionsClick(TObject *Sender)
{
  DisableFields();
  switch (Functions->ItemIndex) {
    case 0  : SetupAccruedInterestMaturity();  break;
    case 1  : SetupAccruedInterestPeriodic();  break;
    case 2  : SetupBondDuration();  break;
    case 3  : SetupBondPrice();  break;
    case 4  : SetupCumulativeInterest();  break;
    case 5  : SetupCumulativePrincipal();  break;
    case 6  : SetupDecliningBalance();  break;
    case 7  : SetupDiscountRate();  break;
    case 8  : SetupDollarToDecimal();  break;
    case 9  : SetupDollarToDecimalText();  break;
    case 10 : SetupDollarToFraction();  break;
    case 11 : SetupDollarToFractionStr();  break;
    case 12 : SetupEffectiveInterestRate();  break;
    case 13 : SetupFutureValue();  break;
    case 14 : SetupFutureValueSchedule();  break;
    case 15 : SetupInterestRate();  break;
    case 16 : SetupInternalRateOfReturn();  break;
    case 17 : SetupIsCardValid();  break;
    case 18 : SetupModifiedDuration();  break;
    case 19 : SetupModifiedIRR();  break;
    case 20 : SetupNetPresentValue();  break;
    case 21 : SetupNominalInterestRate();  break;
    case 22 : SetupNonperiodicIRR();  break;
    case 23 : SetupNonperiodicNPV();  break;
    case 24 : SetupPayment();  break;
    case 25 : SetupPresentValue();  break;
    case 26 : SetupReceivedAtMaturity();  break;
    case 27 : SetupRoundToDecimal();  break;
    case 28 : SetupTBillEquivYield();  break;
    case 29 : SetupTBillPrice();  break;
    case 30 : SetupTBillYield();  break;
    case 31 : SetupVariableDecliningBalance();  break;
    case 32 : SetupYieldPeriodic();  break;
    case 33 : SetupYieldDiscounted();  break;
    case 34 : SetupYieldMaturity();  break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::DisableFields()
{
  E1Edit->Enabled = false;
  E1Edit->Color = clWhite;
  E2Edit->Enabled = false;
  E2Edit->Color = clWhite;
  E3Edit->Enabled = false;
  E3Edit->Color = clWhite;
  E4Edit->Enabled = false;
  E4Edit->Color = clWhite;
  E5Edit->Enabled = false;
  E5Edit->Color = clWhite;
  I1Edit->Enabled = false;
  I1Edit->Color = clWhite;
  I2Edit->Enabled = false;
  I2Edit->Color = clWhite;
  I3Edit->Enabled = false;
  I3Edit->Color = clWhite;
  I4Edit->Enabled = false;
  I4Edit->Color = clWhite;
  I5Edit->Enabled = false;
  I5Edit->Color = clWhite;
  D1Edit->Enabled = false;
  D1Edit->Color = clWhite;
  D2Edit->Enabled = false;
  D2Edit->Color = clWhite;
  D3Edit->Enabled = false;
  D3Edit->Color = clWhite;
  D4Edit->Enabled = false;
  D4Edit->Color = clWhite;
  D5Edit->Enabled = false;
  D5Edit->Color = clWhite;
  DAEdit->Enabled = false;
  DAEdit->Color = clWhite;
  VAEdit->Enabled = false;
  VAEdit->Color = clWhite;
  StrEdit->Enabled = false;
  StrEdit->Color = clWhite;
  TimEdit->Enabled = False;
  TimEdit->Color = clWhite;
  BasEdit->Enabled = False;
  BasEdit->Color = clWhite;
  FreqEdit->Enabled = False;
  FreqEdit->Color = clWhite;
  BoolEdit->Enabled = false;
  BoolEdit->Color = clWhite;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::ExtEditExit(TObject *Sender)
{
  TEdit* edit = (TEdit*) Sender;
  ExtVar[edit->Tag] = edit->Text.ToDouble();
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::IntEditExit(TObject *Sender)
{
  TEdit* edit = (TEdit*) Sender;
  IntVar[edit->Tag] = edit->Text.ToInt();
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::DateEditExit(TObject *Sender)
{
  TEdit* edit = (TEdit*) Sender;
  DateVar[edit->Tag] = DateStringToStDate("mm/dd/yy", edit->Text, 1920);
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::StrEditExit(TObject *Sender)
{
  TEdit* edit = (TEdit*) Sender;
  StrVar = edit->Text;
}
//---------------------------------------------------------------------------

void __fastcall TFinCalForm::DAEditExit(TObject *Sender)
{
  for (int i=0; i<IntVar[0]; i++)
    DateArray[i] = DateStringToStDate("mm/dd/yy", DAEdit->Cells[0][i], 1950);
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::DAEditEnter(TObject *Sender)
{
  DAEdit->Row = 0;
  DAEdit->Col = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::VAEditEnter(TObject *Sender)
{
  VAEdit->Row = 0;
  VAEdit->Col = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::VAEditExit(TObject *Sender)
{
  for (int i=0; i<IntVar[0]; i++)
    ValArray[i] = VAEdit->Cells[0][i].ToDouble();
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::FreqEditExit(TObject *Sender)
{
  switch (FreqEdit->ItemIndex) {
    case 0: FreqVar = fqAnnual;  break;
    case 1: FreqVar = fqSemiAnnual;  break;
    case 2: FreqVar = fqQuarterly;  break;
    case 3: FreqVar = fqMonthly;  break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::BasEditExit(TObject *Sender)
{
  switch(BasEdit->ItemIndex) {
    case 0: BasVar = BasisNASD;  break;
    case 1: BasVar = BasisActAct;  break;
    case 2: BasVar = BasisAct360;  break;
    case 3: BasVar = BasisAct365;  break;
    case 4: BasVar = BasisEur30360;  break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::TimEditExit(TObject *Sender)
{
  switch (TimEdit->ItemIndex) {
    case 0: TimVar = ptEndOfPeriod;  break;
    case 1: TimVar = ptStartOfPeriod;  break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::BoolEditExit(TObject *Sender)
{
  BoolVar = BoolEdit->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::FormCreate(TObject *Sender)
{
  for (int i=0; i<5; i++) {
    ExtVar[i] = 0;
    DateVar[i] = CurrentDate();
    IntVar[i] = 0;
  }
  E1Edit->Text = FloatToStr(ExtVar[0]);
  E2Edit->Text = FloatToStr(ExtVar[1]);
  E3Edit->Text = FloatToStr(ExtVar[2]);
  E4Edit->Text = FloatToStr(ExtVar[3]);
  E5Edit->Text = FloatToStr(ExtVar[4]);
  I1Edit->Text = IntToStr(IntVar[0]);
  I2Edit->Text = IntToStr(IntVar[1]);
  I3Edit->Text = IntToStr(IntVar[2]);
  I4Edit->Text = IntToStr(IntVar[3]);
  I5Edit->Text = IntToStr(IntVar[4]);
  D1Edit->Text = StDateToDateString("mm/dd/yy", DateVar[0], true);
  D2Edit->Text = StDateToDateString("mm/dd/yy", DateVar[1], true);
  D3Edit->Text = StDateToDateString("mm/dd/yy", DateVar[2], true);
  D4Edit->Text = StDateToDateString("mm/dd/yy", DateVar[3], true);
  D5Edit->Text = StDateToDateString("mm/dd/yy", DateVar[4], true);
  DisableFields();
  for (int i=0; i<30; i++) {
    ValArray[i] = 0;
    DateArray[i] = CurrentDate();
  }
  TimEdit->ItemIndex = 0;
  TimVar = ptEndOfPeriod;
  BasEdit->ItemIndex = 0;
  BasVar = BasisNASD;
  FreqEdit->ItemIndex = 0;
  FreqVar = fqAnnual;
  BoolEdit->Checked = false;
  BoolVar = false;
  Application->HintPause = 250;
  Application->HintColor = clAqua;
}
//---------------------------------------------------------------------------
void __fastcall TFinCalForm::BitBtn1Click(TObject *Sender)
{
  Close();	
}
//---------------------------------------------------------------------------
