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
#ifndef FinCaluH
#define FinCaluH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Buttons.hpp>
#include "StFin.hpp"
#include "StDate.hpp"
#include "StDateSt.hpp"
#include "Grids.hpp"
//---------------------------------------------------------------------------
class TFinCalForm : public TForm
{
__published:	// IDE-managed Components
	TRadioGroup *Functions;
	TGroupBox *GroupBox1;
	TLabel *Label2;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label3;
	TLabel *Label1;
	TLabel *Label20;
	TLabel *Label10;
	TLabel *Label13;
	TEdit *StrEdit;
	TEdit *E1Edit;
	TEdit *E2Edit;
	TEdit *E3Edit;
	TEdit *E4Edit;
	TEdit *I1Edit;
	TEdit *I2Edit;
	TEdit *I3Edit;
	TEdit *I4Edit;
	TEdit *D1Edit;
	TEdit *D2Edit;
	TEdit *D3Edit;
	TEdit *D4Edit;
	TComboBox *FreqEdit;
	TComboBox *BasEdit;
	TComboBox *TimEdit;
	TEdit *E5Edit;
	TEdit *I5Edit;
	TEdit *D5Edit;
	TCheckBox *BoolEdit;
	TGroupBox *Arrays;
	TLabel *Label8;
	TLabel *Label9;
	TStringGrid *VAEdit;
	TStringGrid *DAEdit;
	TPanel *Panel1;
	TButton *GoBtn;
	TEdit *SResult;
	TBitBtn *BitBtn1;
	TEdit *FResult;

	void __fastcall GoBtnClick(TObject *Sender);
	void __fastcall FunctionsClick(TObject *Sender);
	void __fastcall ExtEditExit(TObject *Sender);
	void __fastcall IntEditExit(TObject *Sender);
	void __fastcall DateEditExit(TObject *Sender);
	void __fastcall StrEditExit(TObject *Sender);
	void __fastcall DAEditExit(TObject *Sender);
	void __fastcall DAEditEnter(TObject *Sender);
	void __fastcall VAEditEnter(TObject *Sender);
	void __fastcall VAEditExit(TObject *Sender);
	void __fastcall FreqEditExit(TObject *Sender);
	void __fastcall BasEditExit(TObject *Sender);
	void __fastcall TimEditExit(TObject *Sender);
	void __fastcall BoolEditExit(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall BitBtn1Click(TObject *Sender);
private:	// User declarations
    long double    ExtVar[5];
    int            IntVar[5];
    TStDate        DateVar[5];
    TStFrequency   FreqVar;
    TStBasis       BasVar;
    TStPaymentTime TimVar;
    String         StrVar;
    bool           BoolVar;
    TStDate        DateArray[30];
    double         ValArray[30];
    void __fastcall FT(TWinControl* F, short T, String S);
    void __fastcall SetupAccruedInterestPeriodic();
    void __fastcall SetupAccruedInterestMaturity();
    void __fastcall SetupCumulativeInterest();
    void __fastcall SetupCumulativePrincipal();
    void __fastcall SetupDiscountRate();
    void __fastcall SetupEffectiveInterestRate();
    void __fastcall SetupNominalInterestRate();
    void __fastcall SetupInterestRate();
    void __fastcall SetupReceivedAtMaturity();
    void __fastcall SetupYieldPeriodic();
    void __fastcall SetupYieldDiscounted();
    void __fastcall SetupYieldMaturity();
    void __fastcall SetupTBillEquivYield();
    void __fastcall SetupTBillPrice();
    void __fastcall SetupTBillYield();
    void __fastcall SetupBondDuration();
    void __fastcall SetupModifiedDuration();
    void __fastcall SetupFutureValueSchedule();
    void __fastcall SetupModifiedIRR();
    void __fastcall SetupNonperiodicIRR();
    void __fastcall SetupNonperiodicNPV();
    void __fastcall SetupDecliningBalance();
    void __fastcall SetupVariableDecliningBalance();
    void __fastcall SetupDollarToDecimal();
    void __fastcall SetupDollarToFraction();
    void __fastcall SetupDollarToDecimalText();
    void __fastcall SetupDollarToFractionStr();
    void __fastcall SetupRoundToDecimal();
    void __fastcall SetupIsCardValid();
    void __fastcall SetupNetPresentValue();
    void __fastcall SetupFutureValue();
    void __fastcall SetupPresentValue();
    void __fastcall SetupBondPrice();
    void __fastcall SetupPayment();
    void __fastcall SetupInternalRateOfReturn();
    void __fastcall DisableFields();
public:		// User declarations
	__fastcall TFinCalForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
 extern TFinCalForm *FinCalForm;
//---------------------------------------------------------------------------
#endif
