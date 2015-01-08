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

#include "BcdCalU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StBcd"
#pragma link "StStrL"

#pragma resource "*.dfm"
TBCDCalcDlg *BCDCalcDlg;
//---------------------------------------------------------------------------
__fastcall TBCDCalcDlg::TBCDCalcDlg(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::FormCreate(TObject *Sender)
{
  Memo1->Lines->Strings[0] = "";
  PendOp = 0;
  DFHold = 0;
  XBuffer = "0";
  ClearOnNext = false;
}
//---------------------------------------------------------------------------
extern System::AnsiString __fastcall BytesToString(char * Value, Cardinal Size)
{
  Cardinal Index;
  String S, Res;

  Res.SetLength(2*Size);

  for (Cardinal I = 1; I <= Size; I++) {
    Index = I*2;
    S = HexBL(Value[I-1]);

    Res[(Index)-1] = S[1];
    Res[Index] = S[2];
  };

  return Res;
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::FormKeyPress(TObject *Sender, char &Key)
{
  char HldOp;
  int L;
  TBcd BCD1;
  String S;

  if (Key == 13) {
    if (XBuffer == "0") {
      XBuffer = Memo1->Lines->Strings[0];
    }
    else {
      EqualBtnClick(Sender);
      XBuffer = "0";
    };
    Key = 0;
    ClearOnNext = true;
  };

  if (BCDChar.Contains(Key)) {
    if (Key == 'p') {
      S = Memo1->Lines->Strings[0];
      if (S[1] != '-') {
        S.Insert('-', 1);
      }
      else {
        S.Delete(1, 1);
      }
      Memo1->Lines->Strings[0] = S;
      ValBcd_C(S, BCD1);
      BCDString->Text = BytesToString((char *)BCD1, sizeof(BCD1));
      Key = 0;
    }
    else {
      if (ClearOnNext) {
        Memo1->Lines->Strings[0] = "";
        ClearOnNext = false;
      };
    };
  };

  if (BCDOper.Contains(Key)) {
    if ((Key != 's') && (Key != 'e') && (Key != 'l')) {
      if (Memo1->Lines->Strings[0] == "") {
        Memo1->Lines->Strings[0] = "0";
      };
      if (XBuffer != "0") {
        EqualBtnClick(Sender);
      }
      XBuffer = Memo1->Lines->Strings[0];
      ValBcd_C(XBuffer, BCD1);
      BCDString->Text = BytesToString((char *)BCD1, sizeof(BCD1));
      PendOp = Key;
      Key = 0;
      ClearOnNext = true;
    }
    else {
      HldOp = PendOp;
      PendOp = Key;
      EqualBtnClick(Sender);
      PendOp = HldOp;
      Key = 0;
    };
  };

  if (BCDChar.Contains(Key)) {
    S = Memo1->Lines->Strings[0];
    L = S.Length();
    if (L < Memo1->MaxLength) {
      Memo1->Lines->Strings[0] = S + Key;
    };
    Key = 0;
  };
  Memo1->SetFocus();
  Memo1->SelStart = Memo1->Text.Length();
  Memo1->SelLength = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::ClearBtnClick(TObject *Sender)
{
  XBuffer = "0";
  Memo1->Lines->Strings[0] = "0";
  BCDString->Text = "";
  PendOp = 0;
  ClearOnNext = true;
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::ClearEntryBtnClick(TObject *Sender)
{
  Memo1->Lines->Strings[0] = '0';
  ClearOnNext = true;
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::SevenKeyClick(TObject *Sender)
{
  char c = '7';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::EightKeyClick(TObject *Sender)
{
  char c = '8';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::NineKeyClick(TObject *Sender)
{
  char c = '9';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::FourKeyClick(TObject *Sender)
{
  char c = '4';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::FiveKeyClick(TObject *Sender)
{
  char c = '5';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::SixKeyClick(TObject *Sender)
{
  char c = '6';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::OneKeyClick(TObject *Sender)
{
  char c = '1';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::TwoKeyClick(TObject *Sender)
{
  char c = '2';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::ThreeKeyClick(TObject *Sender)
{
  char c = '3';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::ZeroBtnClick(TObject *Sender)
{
  char c = '0';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::DecKeyClick(TObject *Sender)
{
  FormKeyPress(Sender, DecimalSeparator);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::PlusMinusBtnClick(TObject *Sender)
{
  char c = 'p';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::AddBtnClick(TObject *Sender)
{
  char c = '+';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::SubBtnClick(TObject *Sender)
{
  char c = '-';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::MulBtnClick(TObject *Sender)
{
  char c = '*';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::DivBtnClick(TObject *Sender)
{
  char c = '/';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::SqrtBtnClick(TObject *Sender)
{
  char c = 's';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::ExpBtnClick(TObject *Sender)
{
  char c = 'e';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::LnBtnClick(TObject *Sender)
{
  char c = 'l';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::XtoYBtnClick(TObject *Sender)
{
  char c = '^';
  FormKeyPress(Sender, c);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::EqualBtnClick(TObject *Sender)
{
  Extended RV;
  String S;
  TBcd BCD;

  if (PendOp != 0) {
    S = Memo1->Lines->Strings[0];
    if (S == "") {
      MessageBeep(0);
      return;
    };
    
    switch (PendOp) {
      case '+' : {
        RV = StrToFloat(XBuffer) + StrToFloat(S);
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
      case '-' : {
        RV = StrToFloat(XBuffer) - StrToFloat(S);
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
      case '*' : {
        RV = StrToFloat(XBuffer) * StrToFloat(S);
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
      case '/' : {
        RV = StrToFloat(S);
        if (RV == 0) {
          Memo1->Lines->Strings[0] = "Divide by zero error";
          PendOp = 0;
          ClearOnNext = false;
        }
        else {
          RV = StrToFloat(XBuffer) / StrToFloat(S);
          Memo1->Lines->Strings[0] = FloatToStr(RV);
          ValBcd_C(Memo1->Lines->Strings[0], BCD);
          BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        };
        break;
      };
      case 's' : {
        RV = sqrt(StrToFloat(S));
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
      case 'e' : {
        RV = exp(StrToFloat(S));
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
      case 'l' : {
        RV = log(StrToFloat(S));
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
      case '^' : {
        RV = exp(log(StrToFloat(XBuffer)) * StrToFloat(S));
        Memo1->Lines->Strings[0] = FloatToStr(RV);
        ValBcd_C(Memo1->Lines->Strings[0], BCD);
        BCDString->Text = BytesToString((char *)BCD, sizeof(BCD));
        break;
      };
    };
  };

  PendOp = 0;
  ClearOnNext = true;
  Memo1->SetFocus();
  Memo1->SelStart = 0;
  Memo1->SelLength = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::Copy1Click(TObject *Sender)
{
  Memo1->SelectAll();
  Memo1->CopyToClipboard();
  Memo1->SelStart = 0;
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::Paste1Click(TObject *Sender)
{
  String S = "";
  String Nums = "";
  bool IsNeg = false;
  S = Clipboard()->AsText;
  Nums = "0123456789";
  Nums = Nums + Sysutils::DecimalSeparator;
  if (S[1] == '-') {
    IsNeg = true;
    S = S.SubString(2, S.Length() - 1);
  }

  if (IsStrNumericL(S, Nums)) {
    if (IsNeg) {
     S = "-" + S;
    }
    Memo1->Lines->Strings[0] = S;
  }
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::BSBtnClick(TObject *Sender)
{
  Memo1->Lines->Strings[0] = Memo1->Lines->Strings[0].SubString(1, Memo1->Lines->Strings[0].Length() - 1);
  if (Memo1->Lines->Strings[0].Length() == 0)
    ClearBtnClick(ClearBtn);
}
//---------------------------------------------------------------------------
void __fastcall TBCDCalcDlg::FormShow(TObject *Sender)
{
  BCDChar << '0' << '1' << '2' << '3' << '4' << '5' << '6' << '7' << '8' << '9' << DecimalSeparator << 'p';
  BCDOper << '+' << '-' << '/' << '*' << '^' << 'e' << 'l' << 's' << '=';
  
}
//---------------------------------------------------------------------------

