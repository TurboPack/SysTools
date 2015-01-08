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

#include <stdlib.h>
#include "Ex1DArrU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StLArr"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  //RegisterClass should be used, but it is not there in C++Builder 1.0
  RegisterClassAlias(__classid(TStLArray), "TStLArray");
  UpdateButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete MyLArray;
}
//---------------------------------------------------------------------------
void TForm1::SetBusy(Boolean B)
{
  if (B) {
    Screen->Cursor = crHourGlass;
  }
  else {
    Screen->Cursor = crDefault;
  }
};
//---------------------------------------------------------------------------
int __fastcall MyArraySort(const void* El1, const void* El2)
{
  ARecord R1, R2;
  int Res;

  R1 = *(ARecord*) El1;
  R2 = *(ARecord*) El2;

  Res = (R1.X - R2.X);
  if (Res == 0) {
    Res = (R1.Y - R2.Y);
    if (Res == 0) {
      Res = floor(R1.Mag - R2.Mag);
      if (Res == 0) {
        Res = CompareText(R1.Name1, R2.Name1);
        if (Res == 0)
          Res = CompareText(R1.Name2, R2.Name2);
      }
    }
  }
  return Res;
};
//---------------------------------------------------------------------------
void TForm1::FillControls()
{
  Edit1->Text = ARec.Name1;
  Edit2->Text = ARec.Name2;
  Edit3->Text = IntToStr((int)ARec.X);
  Edit4->Text = IntToStr((int)ARec.Y);
  Edit5->Text = FloatToStr(ARec.Mag);
};
//---------------------------------------------------------------------------
void TForm1::FillListBox()
{
  int Step;

  SetBusy(true);
  LB1->Clear();
  LB1->Perform(WM_SETREDRAW, 0, 0);

  for (Step = 0; Step < MyLArray->Count; Step++) {
    MyLArray->Get(Step, &ARec);
    LB1->Items->Add(IntToStr((int)ARec.X) + ", " + IntToStr((int)ARec.Y));
  };

  LB1->Perform(WM_SETREDRAW,1,0);
  LB1->Update();
  MyLArray->Get(0, &ARec);
  SetBusy(false);
};
//---------------------------------------------------------------------------
bool TForm1::CheckControls(ARecord& AR)
{
  if ((Edit1->Text == "") || (Edit2->Text == "") ||
     (Edit3->Text == "") ||
     (Edit4->Text == "") ||
     (Edit5->Text == ""))
    return false;

  AR.Name1 = Edit1->Text;
  AR.Name2 = Edit2->Text;
  AR.X = StrToInt(Edit3->Text);
  AR.Y = StrToInt(Edit4->Text);
  AR.Mag = StrToFloat(Edit5->Text);

  return true;
};
//---------------------------------------------------------------------------
void TForm1::UpdateButtons(bool AOK)
{
  ClearBtn->Enabled = AOK;
  FillBtn->Enabled  = AOK;
  GetBtn->Enabled   = AOK;
  PutBtn->Enabled   = AOK;
  SortBtn->Enabled  = AOK;
  SaveBtn->Enabled  = AOK;
};
//---------------------------------------------------------------------------
void __fastcall TForm1::LB1DblClick(TObject *Sender)
{
  MyLArray->Get(LB1->ItemIndex, &ARec);
  ElemNum->Text = IntToStr(LB1->ItemIndex);
  FillControls();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateBtnClick(TObject *Sender)
{
  int Step;
  int I;
  int Value;

  LB1->Clear();
  SetBusy(true);

  if (MyLArray)
    delete MyLArray;

  UpdateButtons(false);
  MyLArray = new TStLArray(5000, sizeof(ARec));

  MyLArray->ElementsStorable = true;
  MyLArray->Clear();

  Randomize();
  LB1->Perform(WM_SETREDRAW,0,0);
  Value = MyLArray->Count;

  for (Step = 0; Step < Value-1; Step++) {
    ARec.Name1 = "";
    ARec.Name2 = "";
    for (I = 1; I <= 10; I++) {
      ARec.Name1 = ARec.Name1 + (char)(random(26) + 'A');
      ARec.Name2 = ARec.Name2 + (char)(random(26) + 'A');
    };

    ARec.X = floor(random(1000));
    ARec.Y = floor(random(1000));
    ARec.Mag = sqrt(random(25000));

    MyLArray->Put(Step, &ARec);
    LB1->Items->Add(IntToStr((int)ARec.X) + ", " + IntToStr((int)ARec.Y));
  };
  LB1->Perform(WM_SETREDRAW,1,0);
  LB1->Update();

  ElemNum->Text = "0";
  MyLArray->Get(0, &ARec);
  FillControls();
  UpdateButtons(true);
  SetBusy(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearBtnClick(TObject *Sender)
{
  MyLArray->Clear();
  LB1->Clear();

  ElemNum->Text = "0";
  MyLArray->Get(0, &ARec);
  FillControls();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FillBtnClick(TObject *Sender)
{
  if (!CheckControls(ARec)) {
    ShowMessage("One or more invalid entries");
    return;
  };

  MyLArray->Fill(&ARec);

  FillListBox();
  ElemNum->Text = "0";
  MyLArray->Get(0, &ARec);
  FillControls();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GetBtnClick(TObject *Sender)
{
  long E;

  if (ElemNum->Text == "")
    ElemNum->Text = "0";

  E = StrToInt(ElemNum->Text);
  MyLArray->Get(E, &ARec);

  FillControls();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PutBtnClick(TObject *Sender)
{
  long E;

  if (ElemNum->Text == "")
    ElemNum->Text = "0";

  if (!CheckControls(ARec)) {
    ShowMessage("One or more invalid entries");
    return;
  };

  E = StrToInt(ElemNum->Text);
  MyLArray->Put(E, &ARec);
  LB1->Items->Strings[E] = (IntToStr((int)ARec.X) + ", " + IntToStr((int)ARec.Y));

  MyLArray->Get(E, &ARec);
  FillControls();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SortBtnClick(TObject *Sender)
{
  SetBusy(true);
  MyLArray->Sort(MyArraySort);
  SetBusy(false);

  FillListBox();
  FillControls();
  SetBusy(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadBtnClick(TObject *Sender)
{
  if (OD1->Execute()) {
    if (!MyLArray) {
      UpdateButtons(false);
      MyLArray = new TStLArray(5000, sizeof(ARec));
      MyLArray->ElementsStorable = true;
    };

    MyLArray->Clear();
    MyLArray->LoadFromFile(OD1->FileName);

    FillListBox();
    FillControls();
    UpdateButtons(true);
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  if (SD1->Execute())
    MyLArray->StoreToFile(SD1->FileName);
}
//---------------------------------------------------------------------------