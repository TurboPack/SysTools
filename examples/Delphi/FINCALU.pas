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

unit Fincalu;

interface

uses
  Windows, Messages, SysUtils, Controls, Classes, Forms, StdCtrls, Buttons,
  ExtCtrls, Grids, Graphics,

  StFin, StDate, StDateSt;

type
  TFinCalForm = class(TForm)
    Functions: TRadioGroup;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label20: TLabel;
    Arrays: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    StrEdit: TEdit;
    Panel1: TPanel;
    GoBtn: TButton;
    SResult: TEdit;
    BitBtn1: TBitBtn;
    E1Edit: TEdit;
    E2Edit: TEdit;
    E3Edit: TEdit;
    E4Edit: TEdit;
    I1Edit: TEdit;
    I2Edit: TEdit;
    I3Edit: TEdit;
    I4Edit: TEdit;
    FResult: TEdit;
    VAEdit: TStringGrid;
    DAEdit: TStringGrid;
    D1Edit: TEdit;
    D2Edit: TEdit;
    D3Edit: TEdit;
    D4Edit: TEdit;
    Label10: TLabel;
    FreqEdit: TComboBox;
    BasEdit: TComboBox;
    TimEdit: TComboBox;
    E5Edit: TEdit;
    I5Edit: TEdit;
    D5Edit: TEdit;
    Label13: TLabel;
    BoolEdit: TCheckBox;

    procedure SetupAccruedInterestPeriodic;
    procedure SetupAccruedInterestMaturity;
    procedure SetupCumulativeInterest;
    procedure SetupCumulativePrincipal;
    procedure SetupDiscountRate;
    procedure SetupEffectiveInterestRate;
    procedure SetupNominalInterestRate;
    procedure SetupInterestRate;
    procedure SetupReceivedAtMaturity;
    procedure SetupYieldPeriodic;
    procedure SetupYieldDiscounted;
    procedure SetupYieldMaturity;
    procedure SetupTBillEquivYield;
    procedure SetupTBillPrice;
    procedure SetupTBillYield;
    procedure SetupBondDuration;
    procedure SetupModifiedDuration;
    procedure SetupFutureValueSCHEDULE;
    procedure SetupModifiedIRR;
    procedure SetupNonperiodicIRR;
    procedure SetupNonperiodicNPV;
    procedure SetupDecliningBalance;
    procedure SetupVariableDecliningBalance;
    procedure SetupDollarToDecimal;
    procedure SetupDollarToFraction;
    procedure SetupDollarToDecimalText;
    procedure SetupDollarToFractionStr;
    procedure SetupRoundToDecimal;
    procedure SetupIsCardValid;
    procedure SetupNetPresentValue;
    procedure SetupFutureValue;
    procedure SetupPresentValue;
    procedure SetupBondPrice;
    procedure SetupPayment;
    procedure SetupInternalRateOfReturn;
    procedure ExtEditExit(Sender: TObject);
    procedure IntEditExit(Sender: TObject);
    procedure DisableFields;
    procedure FunctionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure FT(F : TWinControl; T: Integer; S: string);
    procedure DateEditExit(Sender: TObject);
    procedure VAEditExit(Sender: TObject);
    procedure DAEditExit(Sender: TObject);
    procedure FreqEditExit(Sender: TObject);
    procedure BasEditExit(Sender: TObject);
    procedure TimEditExit(Sender: TObject);
    procedure StrEditExit(Sender: TObject);
    procedure VAEditEnter(Sender: TObject);
    procedure DAEditEnter(Sender: TObject);
    procedure BoolEditExit(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FinCalForm: TFinCalForm;

implementation

{$R *.DFM}


var
  ExtVar     : array[1..5] of Extended;
  IntVar     : array[1..5] of Integer;
  DateVar    : array[1..5] of TStDate;
  FreqVar    : TStFrequency;
  BasVar     : TStBasis;
  TimVar     : TStPaymentTime;
  StrVar     : String;
  BoolVar    : Boolean;
  DateArray  : array[0..29] of TStDate;
  ValArray   : array[0..29] of Double;


{------ Function parameters ------}

procedure TFinCalForm.FT(F : TWinControl; T: Integer; S: string);
begin
  F.Enabled := true;
  TEdit(F).Color := clYellow;
  F.TabOrder := T;
  F.Hint := S;
  F.ShowHint := true;
end;

procedure TFinCalForm.SetupAccruedInterestPeriodic;
begin
  FT(D1Edit, 1, 'Issue');
  FT(D2Edit, 2, 'Settlement');
  FT(D3Edit, 3, 'Maturity');
  FT(E1Edit, 4, 'Rate');
  FT(E2Edit, 5, 'Par');
  FT(FreqEdit, 6, 'Frequency');
  FT(BasEdit, 7, 'Basis');
end;

procedure TFinCalForm.SetupAccruedInterestMaturity;
begin
  FT(D1Edit, 1, 'Issue');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Rate');
  FT(E2Edit, 4, 'Par');
  FT(BasEdit, 5, 'Basis');
end;

procedure TFinCalForm.SetupCumulativeInterest;
begin
  FT(E1Edit, 1, 'Rate');
  FT(I1Edit, 2, 'NPeriods');
  FT(E2Edit, 3, 'Present Value');
  FT(I2Edit, 4, 'Start Period');
  FT(I3Edit, 5, 'End Period');
  FT(FreqEdit, 6, 'Frequency');
  FT(TimEdit, 7, 'Timing');
end;

procedure TFinCalForm.SetupCumulativePrincipal;
begin
  SetupCumulativeInterest;
end;

procedure TFinCalForm.SetupDiscountRate;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Price');
  FT(E2Edit, 4, 'Redemption');
  FT(BasEdit, 5, 'Basis');
end;

procedure TFinCalForm.SetupEffectiveInterestRate;
begin
  FT(E1Edit, 1, 'Nominal Rate');
  FT(FreqEdit, 2, 'Frequency');
end;

procedure TFinCalForm.SetupNominalInterestRate;
begin
  FT(E1Edit, 1, 'Effective Rate');
  FT(FreqEdit, 2, 'Frequency');
end;

procedure TFinCalForm.SetupInterestRate;
begin
  FT(I1Edit, 1, 'NPeriods');
  FT(E1Edit, 2, 'Payment');
  FT(E2Edit, 3, 'Present Value');
  FT(E3Edit, 4, 'Future Value');
  FT(FreqEdit, 5, 'Frequency');
  FT(TimEdit, 6, 'Timing');
  FT(E4Edit, 7, 'Guess');
end;

procedure TFinCalForm.SetupReceivedAtMaturity;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Investment');
  FT(E2Edit, 4, 'Discount');
  FT(BasEdit, 5, 'Basis');
end;

procedure TFinCalForm.SetupYieldPeriodic;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Rate');
  FT(E2Edit, 4, 'Price');
  FT(E3Edit, 5, 'Redemption');
  FT(FreqEdit, 6, 'Frequency');
  FT(BasEdit, 7, 'Basis');
end;

procedure TFinCalForm.SetupYieldDiscounted;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Price');
  FT(E2Edit, 4, 'Redemption');
  FT(BasEdit, 5, 'Basis');
end;

procedure TFinCalForm.SetupYieldMaturity;
begin
  FT(D1Edit, 1, 'Issue');
  FT(D2Edit, 2, 'Settlement');
  FT(D3Edit, 3, 'Maturity');
  FT(E1Edit, 4, 'Rate');
  FT(E2Edit, 5, 'Price');
  FT(BasEdit, 6, 'Basis');
end;

procedure TFinCalForm.SetupTBillEquivYield;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Discount');
end;

procedure TFinCalForm.SetupTBillPrice;
begin
  SetupTBillEquivYield;
end;

procedure TFinCalForm.SetupTBillYield;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Price');
end;

procedure TFinCalForm.SetupBondDuration;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Rate');
  FT(E2Edit, 4, 'Yield');
  FT(FreqEdit, 5, 'Frequency');
  FT(BasEdit, 6, 'Basis');
end;

procedure TFinCalForm.SetupModifiedDuration;
begin
  SetupBondDuration;
end;

procedure TFinCalForm.SetupFutureValueSCHEDULE;
begin
  FT(E1Edit, 1, 'Principal');
  FT(VAEdit, 2, 'Schedule');
  FT(I1Edit, 3, 'NRates');
end;

procedure TFinCalForm.SetupModifiedIRR;
begin
  FT(VAEdit, 1, 'Values');
  FT(I1Edit, 2, 'NValues');
  FT(E1Edit, 3, 'Finance Rate');
  FT(E2Edit, 4, 'Reinvest Rate');
end;

procedure TFinCalForm.SetupNonperiodicIRR;
begin
  FT(I1Edit, 1, 'NValues');
  FT(VAEdit, 2, 'Values');
  FT(DAEdit, 3, 'Dates');
  FT(E1Edit, 4, 'Guess');
end;

procedure TFinCalForm.SetupNonperiodicNPV;
begin
  FT(I1Edit, 1, 'NValues');
  FT(E1Edit, 2, 'Rate');
  FT(VAEdit, 3, 'Values');
  FT(DAEdit, 4, 'Dates');
end;

procedure TFinCalForm.SetupDecliningBalance;
begin
  FT(E1Edit, 1, 'Cost');
  FT(E2Edit, 2, 'Salvage');
  FT(I1Edit, 3, 'Life');
  FT(I2Edit, 4, 'Period');
  FT(I3Edit, 5, 'Month');
end;

procedure TFinCalForm.SetupVariableDecliningBalance;
begin
  FT(E1Edit, 1, 'Cost');
  FT(E2Edit, 2, 'Salvage');
  FT(I1Edit, 3, 'Life');
  FT(E3Edit, 4, 'Start');
  FT(E4Edit, 5, 'End');
  FT(E5Edit, 6, 'Factor');
  FT(BoolEdit, 7, 'No Switch');
end;

procedure TFinCalForm.SetupDollarToDecimal;
begin
  FT(E1Edit, 1, 'Fractional Dollar');
  FT(I1Edit, 2, 'Fraction');
end;

procedure TFinCalForm.SetupDollarToFraction;
begin
  FT(E1Edit, 1, 'Decimal Dollar');
  FT(I1Edit, 2, 'Fraction');
end;

procedure TFinCalForm.SetupDollarToDecimalText;
begin
  FT(E1Edit, 1, 'Decimal Dollar');
end;

procedure TFinCalForm.SetupDollarToFractionStr;
begin
  FT(E1Edit, 1, 'Fractional Dollar');
  FT(I1Edit, 2, 'Fraction');
end;

procedure TFinCalForm.SetupRoundToDecimal;
begin
  FT(E1Edit, 1, 'Value');
  FT(I1Edit, 2, 'Places');
  FT(BoolEdit, 3, 'Bankers');
end;

procedure TFinCalForm.SetupIsCardValid;
begin
  FT(StrEdit, 1, 'Card Number');
end;

procedure TFinCalForm.SetupNetPresentValue;
begin
  FT(E1Edit, 1, 'Rate');
  FT(I1Edit, 2, 'NValues');
  FT(VAEdit, 3, 'Values');
end;

procedure TFinCalForm.SetupFutureValue;
begin
  FT(E1Edit, 1, 'Rate');
  FT(I1Edit, 2, 'NPeriods');
  FT(E2Edit, 3, 'Payment');
  FT(E3Edit, 4, 'Present Value');
  FT(FreqEdit, 5, 'Frequency');
  FT(TimEdit, 6, 'Timing');
end;

procedure TFinCalForm.SetupPresentValue;
begin
  FT(E1Edit, 1, 'Rate');
  FT(I1Edit, 2, 'NPeriods');
  FT(E2Edit, 3, 'Payment');
  FT(E3Edit, 4, 'Future Value');
  FT(FreqEdit, 5, 'Frequency');
  FT(TimEdit, 6, 'Timing');
end;

procedure TFinCalForm.SetupBondPrice;
begin
  FT(D1Edit, 1, 'Settlement');
  FT(D2Edit, 2, 'Maturity');
  FT(E1Edit, 3, 'Rate');
  FT(E2Edit, 4, 'Yield');
  FT(E3Edit, 5, 'Redemption');
  FT(FreqEdit, 6, 'Frequency');
  FT(BasEdit, 7, 'Basis');
end;

procedure TFinCalForm.SetupPayment;
begin
  FT(E1Edit, 1, 'Rate');
  FT(I1Edit, 2, 'NPeriods');
  FT(E2Edit, 3, 'Present Value');
  FT(E3Edit, 4, 'Future Value');
  FT(FreqEdit, 5, 'Frequency');
  FT(TimEdit, 6, 'Timing');
end;

procedure TFinCalForm.SetupInternalRateOfReturn;
begin
  FT(I1Edit, 1, 'NValues');
  FT(VAEdit, 2, 'Values');
  FT(E1Edit, 3, 'Guess');
end;



{------- Function selection -------}

procedure TFinCalForm.GoBtnClick(Sender: TObject);
var
  FR : Extended;
  SR : String;
begin
  FR := 0;
  SR := '';
  case Functions.ItemIndex of
    0  : FR := AccruedInterestMaturity(DateVar[1], DateVar[2], ExtVar[1],
                 ExtVar[2], BasVar);
    1  : FR := AccruedInterestPeriodic(DateVar[1], DateVar[2], DateVar[3],
                 ExtVar[1], ExtVar[2], FreqVar, BasVar);
    2  : FR := BondDuration(DateVar[1], DateVar[2], ExtVar[1], ExtVar[2],
                 FreqVar, BasVar);
    3  : FR := BondPrice(DateVar[1], DateVar[2], ExtVar[1], ExtVar[2],
                 ExtVar[3], FreqVar, BasVar);
    4  : FR := CumulativeInterest(ExtVar[1], IntVar[1], ExtVar[2], IntVar[2],
                 IntVar[3], FreqVar, TimVar);
    5  : FR := CumulativePrincipal(ExtVar[1], IntVar[1], ExtVar[2], IntVar[2],
                 IntVar[3], FreqVar, TimVar);
    6  : FR := DecliningBalance(ExtVar[1], ExtVar[2], IntVar[1], IntVar[2],
                 IntVar[3]);
    7  : FR := DiscountRate(DateVar[1], DateVar[2], ExtVar[1], ExtVar[2],
                 BasVar);
    8  : FR := DollarToDecimal(ExtVar[1], IntVar[1]);

    9  : SR := DollarToDecimalText(ExtVar[1]);

    10 : FR := DollarToFraction(ExtVar[1], IntVar[1]);

    11 : SR := DollarToFractionStr(ExtVar[1], IntVar[1]);

    12 : FR := EffectiveInterestRate(ExtVar[1], FreqVar);

    13 : FR := FutureValue(ExtVar[1], IntVar[1], ExtVar[2], ExtVar[3],
                 FreqVar, TimVar);
    14 : FR := FutureValueSchedule16(ExtVar[1], ValArray, IntVar[1]);

    15 : FR := InterestRate(IntVar[1], ExtVar[1], ExtVar[2], ExtVar[3],
                 FreqVar, TimVar, ExtVar[4]);
    16 : FR := InternalRateOfReturn16(ValArray, IntVar[1], ExtVar[1]);

    17 : if IsCardValid(StrVar) then
           SR := 'Valid card number'
         else
           SR := 'Invalid card number';
    18 : FR := ModifiedDuration(DateVar[1], DateVar[2], ExtVar[1],
                 ExtVar[2], FreqVar, BasVar);
    19 : FR := ModifiedIRR16(ValArray, IntVar[1], ExtVar[1], ExtVar[2]);

    20 : FR := NetPresentValue16(ExtVar[1], ValArray, IntVar[1]);

    21 : FR := NominalInterestRate(ExtVar[1], FreqVar);

    22 : FR := NonPeriodicIRR16(ValArray, DateArray, IntVar[1], ExtVar[1]);

    23 : FR := NonPeriodicNPV16(ExtVar[1], ValArray, DateArray, IntVar[1]);

    24 : FR := Payment(ExtVar[1], IntVar[1], ExtVar[2], ExtVar[3],
                 FreqVar, TimVar);
    25 : FR := PresentValue(ExtVar[1], IntVar[1], ExtVar[2], ExtVar[3],
                 FreqVar, TimVar);
    26 : FR := ReceivedAtMaturity(DateVar[1], DateVar[2], ExtVar[1],
                 ExtVar[2], BasVar);
    27 : FR := RoundToDecimal(ExtVar[1], IntVar[1], BoolVar);

    28 : FR := TBillEquivYield(DateVar[1], DateVar[2], ExtVar[1]);

    29 : FR := TBillPrice(DateVar[1], DateVar[2], ExtVar[1]);

    30 : FR := TBillYield(DateVar[1], DateVar[2], ExtVar[1]);

    31 : FR := VariableDecliningBalance(ExtVar[1], ExtVar[2], IntVar[1],
                 ExtVar[3], ExtVar[4], ExtVar[5], BoolVar);
    32 : FR := YieldPeriodic(DateVar[1], DateVar[2], ExtVar[1], ExtVar[2],
                 ExtVar[3], FreqVar, BasVar);
    33 : FR := YieldDiscounted(DateVar[1], DateVar[2], ExtVar[1],
                 ExtVar[2], BasVar);
    34 : FR := YieldMaturity(DateVar[1], DateVar[2], DateVar[3], ExtVar[1],
                 ExtVar[2], BasVar);
  end;
  FResult.Text := FloatToStr(FR);
  SResult.Text := SR;
end;

procedure TFinCalForm.FunctionsClick(Sender: TObject);
begin
  DisableFields;
  case Functions.ItemIndex of
    0  : SetupAccruedInterestMaturity;
    1  : SetupAccruedInterestPeriodic;
    2  : SetupBondDuration;
    3  : SetupBondPrice;
    4  : SetupCumulativeInterest;
    5  : SetupCumulativePrincipal;
    6  : SetupDecliningBalance;
    7  : SetupDiscountRate;
    8  : SetupDollarToDecimal;
    9  : SetupDollarToDecimalText;
    10 : SetupDollarToFraction;
    11 : SetupDollarToFractionStr;
    12 : SetupEffectiveInterestRate;
    13 : SetupFutureValue;
    14 : SetupFutureValueSchedule;
    15 : SetupInterestRate;
    16 : SetupInternalRateOfReturn;
    17 : SetupIsCardValid;
    18 : SetupModifiedDuration;
    19 : SetupModifiedIRR;
    20 : SetupNetPresentValue;
    21 : SetupNominalInterestRate;
    22 : SetupNonperiodicIRR;
    23 : SetupNonperiodicNPV;
    24 : SetupPayment;
    25 : SetupPresentValue;
    26 : SetupReceivedAtMaturity;
    27 : SetupRoundToDecimal;
    28 : SetupTBillEquivYield;
    29 : SetupTBillPrice;
    30 : SetupTBillYield;
    31 : SetupVariableDecliningBalance;
    32 : SetupYieldPeriodic;
    33 : SetupYieldDiscounted;
    34 : SetupYieldMaturity;
  end;
end;

{------- Misc utilities and set up -------}

procedure TFinCalForm.DisableFields;
begin
  E1Edit.Enabled := false;
  E1Edit.Color := clwhite;
  E2Edit.Enabled := false;
  E2Edit.Color := clwhite;
  E3Edit.Enabled := false;
  E3Edit.Color := clwhite;
  E4Edit.Enabled := false;
  E4Edit.Color := clwhite;
  E5Edit.Enabled := false;
  E5Edit.Color := clwhite;
  I1Edit.Enabled := false;
  I1Edit.Color := clwhite;
  I2Edit.Enabled := false;
  I2Edit.Color := clwhite;
  I3Edit.Enabled := false;
  I3Edit.Color := clwhite;
  I4Edit.Enabled := false;
  I4Edit.Color := clwhite;
  I5Edit.Enabled := false;
  I5Edit.Color := clwhite;
  D1Edit.Enabled := false;
  D1Edit.Color := clwhite;
  D2Edit.Enabled := false;
  D2Edit.Color := clwhite;
  D3Edit.Enabled := false;
  D3Edit.Color := clwhite;
  D4Edit.Enabled := false;
  D4Edit.Color := clwhite;
  D5Edit.Enabled := false;
  D5Edit.Color := clwhite;
  DAEdit.Enabled := false;
  DAEdit.Color := clwhite;
  VAEdit.Enabled := false;
  VAEdit.Color := clwhite;
  StrEdit.Enabled := false;
  StrEdit.Color := clwhite;
  TimEdit.Enabled := False;
  TimEdit.Color := clWhite;
  BasEdit.Enabled := False;
  BasEdit.Color := clWhite;
  FreqEdit.Enabled := False;
  FreqEdit.Color := clWhite;
  BoolEdit.Enabled := false;
  BoolEdit.Color := clWhite;
end;

procedure TFinCalForm.ExtEditExit(Sender: TObject);
begin
  with (Sender as TEdit) do
    ExtVar[Tag] := StrToFloat(Text);
end;

procedure TFinCalForm.IntEditExit(Sender: TObject);
begin
  with (Sender as TEdit) do
    IntVar[Tag] := StrToInt(Text);
end;

procedure TFinCalForm.DateEditExit(Sender: TObject);
begin
  with (Sender as TEdit) do
    DateVar[Tag] := DateStringToStDate('mm/dd/yy', Text, 1920);
end;

procedure TFinCalForm.StrEditExit(Sender: TObject);
begin
  StrVar := TEdit(Sender).Text;
end;

procedure TFinCalForm.DAEditEnter(Sender: TObject);
begin
  DAEdit.Row := 0;
  DAEdit.Col := 0;
end;

procedure TFinCalForm.DAEditExit(Sender: TObject);
var
  I : Integer;
begin
  for I := 0 to IntVar[1] - 1 do
    DateArray[I] := DateStringToStDate('mm/dd/yy', DAEdit.Cells[0,I], 1950);
end;

procedure TFinCalForm.VAEditEnter(Sender: TObject);
begin
  VAEdit.Row := 0;
  VAEdit.Col := 0;
end;

procedure TFinCalForm.VAEditExit(Sender: TObject);
var
  I : Integer;
begin
  for I := 0 to IntVar[1] - 1 do
    ValArray[I] := StrToFloat(VAEdit.Cells[0,I]);
end;

procedure TFinCalForm.FreqEditExit(Sender: TObject);
begin
  case FreqEdit.ItemIndex of
    0: FreqVar := fqAnnual;
    1: FreqVar := fqSemiAnnual;
    2: FreqVar := fqQuarterly;
    3: FreqVar := fqMonthly;
  end;
end;

procedure TFinCalForm.BasEditExit(Sender: TObject);
begin
  case BasEdit.ItemIndex of
    0: BasVar := BasisNASD;
    1: BasVar := BasisActAct;
    2: BasVar := BasisAct360;
    3: BasVar := BasisAct365;
    4: BasVar := BasisEur30360;
  end;
end;

procedure TFinCalForm.TimEditExit(Sender: TObject);
begin
  case TimEdit.ItemIndex of
    0: TimVar := ptEndOfPeriod;
    1: TimVar := ptStartOfPeriod;
  end;
end;

procedure TFinCalForm.BoolEditExit(Sender: TObject);
begin
  BoolVar := BoolEdit.Checked;
end;

procedure TFinCalForm.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  for I := 1 to 5 do begin
    ExtVar[I] := 0;
    DateVar[I] := CurrentDate;
    IntVar[I] := 0;
  end;
  E1Edit.Text := FloatToStr(ExtVar[1]);
  E2Edit.Text := FloatToStr(ExtVar[2]);
  E3Edit.Text := FloatToStr(ExtVar[3]);
  E4Edit.Text := FloatToStr(ExtVar[4]);
  E5Edit.Text := FloatToStr(ExtVar[5]);
  I1Edit.Text := IntToStr(IntVar[1]);
  I2Edit.Text := IntToStr(IntVar[2]);
  I3Edit.Text := IntToStr(IntVar[3]);
  I4Edit.Text := IntToStr(IntVar[4]);
  I5Edit.Text := IntToStr(IntVar[5]);
  D1Edit.Text := DateToStr(DateVar[1]);
  D2Edit.Text := DateToStr(DateVar[2]);
  D3Edit.Text := DateToStr(DateVar[3]);
  D4Edit.Text := DateToStr(DateVar[4]);
  D5Edit.Text := DateToStr(DateVar[5]);
  DisableFields;
  for I := 0 to 29 do begin
    ValArray[I] := 0;
    DateArray[I] := CurrentDate;
  end;
  TimEdit.ItemIndex := 0;
  TimVar := ptEndOfPeriod;
  BasEdit.ItemIndex := 0;
  BasVar := BasisNASD;
  FreqEdit.ItemIndex := 0;
  FreqVar := fqAnnual;
  BoolEdit.Checked := false;
  BoolVar := false;
  Application.HintPause := 250;
  Application.HintColor := clAqua;
end;

procedure TFinCalForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

end.
