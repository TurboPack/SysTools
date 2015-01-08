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
#include "ExDictU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StDict"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
bool __fastcall DDWalker(TStContainer* Container, TStNode* Node, void * OtherData)
{
  TStDictNode* N = (TStDictNode*) Node;
  ShortString *S = (ShortString*) N->Data;
  Form1->LB1->Items->Add(((TStDictNode*)Node)->Name + " = " + *S);
  return true;
}
//---------------------------------------------------------------------------
void * __fastcall MyLoadData(Classes::TReader* Reader)
{
  ShortString * S;
  S = new ShortString;
  *S = Reader->ReadString();
  return (void *) S;
}
//---------------------------------------------------------------------------
void __fastcall MyStoreData(Classes::TWriter* Writer, void * Data)
{
  ShortString *S = (ShortString*) Data;
  Writer->WriteString(*S);
}
//---------------------------------------------------------------------------
void TForm1::FillListBox(void)
{
  LB1->Clear();
  LB1->Perform(WM_SETREDRAW, 0, 0);

  MyDD->Iterate(DDWalker, 0);

  LB1->Perform(WM_SETREDRAW, 1, 0);
  LB1->Update();
}
//---------------------------------------------------------------------------
void TForm1::UpdateButtons(bool DOK)
{
  ClearBtn->Enabled  = DOK;
  AddBtn->Enabled    = DOK;
  UpdateBtn->Enabled = DOK;
  ExistsBtn->Enabled = DOK;
  DelBtn->Enabled    = DOK;
  SaveBtn->Enabled   = DOK;
  LB1->Enabled       = DOK;
}
//---------------------------------------------------------------------------
String TForm1::RandomData(void)
{
  int Len = random(100)+1;
  String S = "";
  for (int I = 1; I <= Len; I++)
    S = S + (char)(random(26) + 'A');
  return S;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RegisterClassAlias(__classid(TStDictionary), "TStDictionary");
  RegisterClassAlias(__classid(TStDictNode), "TStDictNode");
  UpdateButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete MyDD;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LB1Click(TObject *Sender)
{
  String S1, S2;
  int P, Len;

  S1 = LB1->Items->Strings[LB1->ItemIndex];
  S2 = S1;
  Len = S1.Length();
  P = S1.Pos("=");

  S1.Delete(P-1, Len-P+2);
  Edit1->Text = S1;

  S2.Delete(1, P+1);
  Edit2->Text = S2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LB1DblClick(TObject *Sender)
{
  void * P;

  if (MyDD->Exists(Edit1->Text, P)) {
    MyDD->Delete(Edit1->Text);
    FillListBox();
  }
  LB1->ItemIndex = 0;
  LB1Click(LB1);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateBtnClick(TObject *Sender)
{
  ShortString *S;

  randomize;
  if (MyDD)
    delete MyDD;

  UpdateButtons(false);
  MyDD = new TStDictionary(127);
  MyDD->LoadData = MyLoadData;
  MyDD->StoreData = MyStoreData;
  MyDD->Hash = AnsiELFHashText;

  for (int I = 1; I <= 100; I++) {
    S = new ShortString;
    *S = RandomData();
    MyDD->Add("Item" + IntToStr(I), S);
  }
  FillListBox();
  UpdateButtons(true);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearBtnClick(TObject *Sender)
{
  LB1->Clear();
  MyDD->Clear();
  Edit1->Clear();
  Edit2->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AddBtnClick(TObject *Sender)
{
  ShortString Name;
  ShortString *PS;

  if ((Edit1->Text == "") || (Edit2->Text == "")) {
    ShowMessage("Name and/or data missing");
    return;
  }

  PS = new ShortString;
  *PS = Edit2->Text;
  Name = Edit1->Text;

  MyDD->Add(Name, PS);

  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::UpdateBtnClick(TObject *Sender)
{
  void * P;

  if ((Edit1->Text == "") || (Edit2->Text == "")) {
    ShowMessage("Name and/or data missing");
    return;
  }

  if (MyDD->Exists(Edit1->Text, P)) {
    *((ShortString*)P) = Edit2->Text;
    MyDD->Update(Edit1->Text, P);
  } else {
    ShowMessage(Edit1->Text + " not found");
    return;
  }
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ExistsBtnClick(TObject *Sender)
{
  void *S;

  if (Edit1->Text == "") {
    ShowMessage("No name entry");
    return;
  }

  if (MyDD->Exists(Edit1->Text, S)) {
    Edit2->Clear();
    Edit2->Text = *((ShortString*)S);
    Edit2->Update();
  } else
    ShowMessage("No matching entry found");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DelBtnClick(TObject *Sender)
{
  void * P;

  if (Edit1->Text == "") {
    ShowMessage("No name entered");
    return;
  }

  if (MyDD->Exists(Edit1->Text, P)) {
    MyDD->Delete(Edit1->Text);
    FillListBox();
  } else
    ShowMessage("Entry not found");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadBtnClick(TObject *Sender)
{
  if (OD1->Execute()) {
    if (!MyDD) {
      UpdateButtons(false);
      MyDD = new TStDictionary(127);
      MyDD->LoadData = MyLoadData;
      MyDD->StoreData = MyStoreData;
    }

    MyDD->Clear();
    MyDD->LoadFromFile(OD1->FileName);

    FillListBox();
    UpdateButtons(true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  if (SD1->Execute())
    MyDD->StoreToFile(SD1->FileName);
}
//---------------------------------------------------------------------------