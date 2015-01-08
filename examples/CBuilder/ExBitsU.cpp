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

#include "ExBitsU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StBits"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TForm1::UpdateButtons(bool BitsOK)
{
  IsBitSetBtn->Enabled   = BitsOK;
  ControlBitBtn->Enabled = BitsOK;
  SetAllBtn->Enabled     = BitsOK;
  InvertAllBtn->Enabled  = BitsOK;
  ClearAllBtn->Enabled   = BitsOK;
  ToggleBitBtn->Enabled  = BitsOK;
  SetBitBtn->Enabled     = BitsOK;
  ClearBitBtn->Enabled   = BitsOK;
  SaveBtn->Enabled       = BitsOK;
};
//---------------------------------------------------------------------------
bool TForm1::CheckValue(String S, long &N)
{
  if (S == "") {
    ShowMessage("No value entered");
    return false;
  };

  N = StrToInt(S);
  if ((N < 0) || (N > MyBits->Max)) {
    ShowMessage("Number out of range");
    return false;
  };
  return true;
};
//---------------------------------------------------------------------------
String TForm1::GetTFString(long N)
{
  if (MyBits->BitIsSet(N))
    return "TRUE";
  else
    return "FALSE";
};
//---------------------------------------------------------------------------
void __fastcall TForm1::FormActivate(TObject *Sender)
{
  IsBitSetValue->Text = "-1";
  ToggleBitValue->Text = "-1";
  SetBitValue->Text = "-1";
  ControlBitValue->Text = "-1";
  ClearBitValue->Text = "-1";

  Msg1->Lines->Clear();
  Msg1->Lines->Add("BitSet not created");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RegisterClassAlias(__classid(TStBits), "TStBits");
  UpdateButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete MyBits;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateBtnClick(TObject *Sender)
{
  long MaxBits;

  Msg1->Lines->Clear();

  if (NumElemsValue->Text == "")
    NumElemsValue->Text = "50";

  MaxBits = StrToInt(NumElemsValue->Text);
  if ((MaxBits < 1) || (MaxBits > 9999)) {
    ShowMessage("Value out of range (1 - 9999)");
    return;
  };

  Msg1->Lines->Clear();

  if (!MyBits)
    delete MyBits;

  UpdateButtons(false);
  MyBits = new TStBits(MaxBits);

  Label1->Caption = "In entry fields below, enter a value from 0 to "
                  + IntToStr((int)MaxBits);
  Label2->Caption = "Elements in BitSet: " + IntToStr(MyBits->Max+1);

  IsBitSetValue->Text = "0";
  ToggleBitValue->Text = "0";
  SetBitValue->Text = "0";
  ControlBitValue->Text = "0";
  ClearBitValue->Text = "0";

  Msg1->Lines->Add("BitSet created");
  Msg1->Lines->Add(IntToStr(MyBits->Count));
  UpdateButtons(true);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::IsBitSetBtnClick(TObject *Sender)
{
  long BitNum;

  if (!CheckValue(IsBitSetValue->Text, BitNum))
    return;

  Msg1->Lines->Clear();
  if (MyBits->BitIsSet(BitNum)) 
    Msg1->Lines->Add("Bit is set");
  else
    Msg1->Lines->Add( "Bit not set");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ControlBitBtnClick(TObject *Sender)
{
  long BitNum;
  String WasStr, NowStr;

  if (!CheckValue(ControlBitValue->Text, BitNum))
    return;

  WasStr = GetTFString(BitNum);
  MyBits->ControlBit(BitNum,BitOnCB->Checked);
  NowStr = GetTFString(BitNum);

  Msg1->Lines->Clear();
  Msg1->Lines->Add("Bit was: " + WasStr);
  Msg1->Lines->Add("Bit is now: " + NowStr);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SetAllBtnClick(TObject *Sender)
{
  Msg1->Lines->Clear();
  MyBits->SetBits();
  Msg1->Lines->Add("Bits Set");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::InvertAllBtnClick(TObject *Sender)
{
  Msg1->Lines->Clear();
  MyBits->InvertBits();
  Msg1->Lines->Add("Bits Inverted");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearAllBtnClick(TObject *Sender)
{
  Msg1->Lines->Clear();
  MyBits->Clear();
  Msg1->Lines->Add("Bits Cleared");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ToggleBitBtnClick(TObject *Sender)
{
  long BitNum;
  String WasStr, NowStr;

  if (!CheckValue(ToggleBitValue->Text, BitNum))
    return;

  WasStr = GetTFString(BitNum);
  MyBits->ToggleBit(BitNum);
  NowStr = GetTFString(BitNum);

  Msg1->Lines->Clear();
  Msg1->Lines->Add("Bit was: " + WasStr);
  Msg1->Lines->Add("Bit is now: " + NowStr);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SetBitBtnClick(TObject *Sender)
{
  long BitNum;
  String WasStr, NowStr;

  if (!CheckValue(SetBitValue->Text, BitNum))
    return;

  WasStr = GetTFString(BitNum);
  MyBits->SetBit(BitNum);
  NowStr = GetTFString(BitNum);

  Msg1->Lines->Clear();
  Msg1->Lines->Add("Bit was: " + WasStr);
  Msg1->Lines->Add("Bit is now: " + NowStr);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearBitBtnClick(TObject *Sender)
{
  long BitNum;
  String WasStr, NowStr;

  if (!CheckValue(ClearBitValue->Text, BitNum))
    return;

  WasStr = GetTFString(BitNum);
  MyBits->ClearBit(BitNum);
  NowStr = GetTFString(BitNum);

  Msg1->Lines->Clear();
  Msg1->Lines->Add("Bit was: " + WasStr);
  Msg1->Lines->Add("Bit is now: " + NowStr);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadBtnClick(TObject *Sender)
{
  if (OD1->Execute()) {
    if (!MyBits) {
      //create a minimum sized bitset - load will resize it
      MyBits = new TStBits(1);
      if (!MyBits) {
        Msg1->Lines->Add("BitSet Create Failed");
        UpdateButtons(false);
        return;
      };
    };

    MyBits->Clear();
    MyBits->LoadFromFile(OD1->FileName);

    Label1->Caption = "In entry fields below, enter a value from 0 to "
                    + IntToStr(MyBits->Max);
    Label2->Caption = "Elements in BitSet: " + IntToStr(MyBits->Max+1);

    IsBitSetValue->Text = "0";
    ToggleBitValue->Text = "0";
    SetBitValue->Text = "0";
    ControlBitValue->Text = "0";
    ClearBitValue->Text = "0";

    Msg1->Clear();
    Msg1->Lines->Add("BitSet loaded");
    UpdateButtons(true);
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  if (SD1->Execute()) {
    MyBits->StoreToFile(SD1->FileName);
    Msg1->Clear();
    Msg1->Lines->Add("BitSet saved");
  };
}
//---------------------------------------------------------------------------
   