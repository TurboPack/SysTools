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
#include "ExCollU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StColl"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall MyDelNodeData(void * Data)
{
  delete (ARecord*)Data;
}
//---------------------------------------------------------------------------
bool __fastcall MatchCollString(TStContainer* Container, void * Data, void * OtherData)
{
  ARecord R1 = *(ARecord*) Data;
  ARecord R2 = *(ARecord*) OtherData;

  return
    (R1.First != R2.First) || (R1.Last != R2.Last);
}
//---------------------------------------------------------------------------
bool __fastcall CollWalker(TStContainer* Container, void * Data, void * OtherData)
//this function makes no comparison and always returns true
//so it will visit all nodes in the collection
{
  ARecord R1 = *(ARecord*) Data;

  Form1->LB1->Items->Add(R1.First + ' ' + R1.Last + ', ' + IntToStr(R1.Age));
  return true;
}
//---------------------------------------------------------------------------
void __fastcall MyStoreData(Classes::TWriter* Writer, void * Data)
{
  ARecord R1 = *(ARecord*) Data;
  Writer->WriteString(R1.First);
  Writer->WriteString(R1.Last);
  Writer->WriteInteger(R1.Age);
}
//---------------------------------------------------------------------------
void * __fastcall MyLoadData(Classes::TReader* Reader)
{
  ARecord* Res = new ARecord;
  Res->First = Reader->ReadString();
  Res->Last  = Reader->ReadString();
  Res->Age   = Reader->ReadInteger();
  return Res;
}
//---------------------------------------------------------------------------
void TForm1::UpdateButtons(bool COK)
{
  ClearBtn->Enabled = COK;
  PackBtn->Enabled  = COK;
  AtBtn->Enabled    = COK;
  AtInsBtn->Enabled = COK;
  AtPutBtn->Enabled = COK;
  DelBtn->Enabled   = COK;
  AtDelBtn->Enabled = COK;
  InsBtn->Enabled   = COK;
  EffBtn->Enabled   = COK;
  SaveBtn->Enabled  = COK;
}
//---------------------------------------------------------------------------
void TForm1::SetBusy(bool B)
{
  if (B)
    Screen->Cursor = crHourGlass;
  else
    Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
Boolean TForm1::CheckControls(ARecord &AR)
{
  if ((Edit3->Text == "") || (Edit4->Text == "") || (Edit5->Text == ""))
    return false;

  AR.First = Edit3->Text;
  AR.Last = Edit4->Text;

  AR.Age = StrToInt(Edit5->Text);
  return true;
}
//---------------------------------------------------------------------------
void TForm1::FillControls(ARecord AR)
{
  Edit3->Text = AR.First;
  Edit4->Text = AR.Last;
  Edit5->Text = IntToStr(AR.Age);
}
//---------------------------------------------------------------------------
void TForm1::FillListBox(void)
{
  LB1->Clear();
  LB1->Perform(WM_SETREDRAW,0,0);

  SetBusy(true);

  MyCollection->Iterate(CollWalker, true, 0);

  LB1->Perform(WM_SETREDRAW,1,0);
  LB1->Update();
  LB1->ItemIndex = 0;
  Edit2->Text = "0";

  SetBusy(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RegisterClassAlias(__classid(TStCollection), "TStCollection");
  UpdateButtons(false);

  FirstA[0] = "Fred";
  FirstA[1] = "Robert";
  FirstA[2] = "Barney";
  FirstA[3] = "Horatio";
  FirstA[4] = "Kent";
  FirstA[5] = "Arthur";
  FirstA[6] = "Lee";
  FirstA[7] = "John Q. ";

  LastA[0] = "Flintstone";
  LastA[1] = "Java";
  LastA[2] = "Rubble";
  LastA[3] = "Hornblower";
  LastA[4] = "C++Builder";
  LastA[5] = "Miller";
  LastA[6] = "Delphi";
  LastA[7] = "Public";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete MyCollection;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateBtnClick(TObject *Sender)
{
  int I;
  ARecord* AR;

  if (MyCollection)
    delete MyCollection;

  UpdateButtons(false);
  MyCollection = new TStCollection(100);

  MyCollection->DisposeData = MyDelNodeData;
  MyCollection->LoadData = MyLoadData;
  MyCollection->StoreData = MyStoreData;

  randomize;
  LB1->Clear();
  LB1->Perform(WM_SETREDRAW,0,0);
  SetBusy(true);

  for (I = 0; I < MaxElem; I++) {
    AR = new ARecord;
    AR->First = FirstA[random(8)];
    AR->Last = LastA[random(8)];
    AR->Age = random(100);

    MyCollection->Insert(AR);
    LB1->Items->Add(AR->First + " " + AR->Last + ", " + IntToStr(AR->Age));
  }
  LB1->Perform(WM_SETREDRAW,1,0);
  LB1->Update();

  MyCollection->Pack();
  Edit1->Text = IntToStr(MyCollection->Efficiency);
  UpdateButtons(true);
  SetBusy(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearBtnClick(TObject *Sender)
{
  MyCollection->Clear();
  LB1->Clear();
  Edit1->Text = IntToStr(MyCollection->Efficiency);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PackBtnClick(TObject *Sender)
{
  MyCollection->Pack();
  Edit1->Text = IntToStr(MyCollection->Efficiency);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AtBtnClick(TObject *Sender)
{
  void * Data;
  long E;

  if (Edit2->Text == "")
    Edit2->Text = "0";
  E = StrToInt(Edit2->Text);
  if ((E > MyCollection->Count-1) || (E < 0)) {
    ShowMessage("Element value out of range (0.." + IntToStr(MyCollection->Count) + ")");
    Edit2->Text = "0";
    return;
  }

  Data = MyCollection->At(E);
  ARecord R1 = *(ARecord*) Data;
  FillControls(R1);;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AtInsBtnClick(TObject *Sender)
{
  long E;
  ARecord* PAR;

  PAR = new ARecord;
  if (!CheckControls(*PAR)) {
    ShowMessage("One or more data controls invalid");
    delete PAR;
    return;
  }

  if (Edit2->Text == "")
    Edit2->Text = "0";
  E = StrToInt(Edit2->Text);
  if ((E > MyCollection->Count-1) || (E < 0)) {
    ShowMessage("Element value out of range (0.." + IntToStr(MyCollection->Count) + ")");
    Edit2->Text = "0";
    return;
  }

  MyCollection->AtInsert(E, PAR);
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AtPutBtnClick(TObject *Sender)
{
  long E;
  void * Data;
  ARecord AR;

  if (!CheckControls(AR)) {
    ShowMessage("One or more data controls invalid");
    return;
  }

  if (Edit2->Text == "")
    Edit2->Text = "0";
  E = StrToInt(Edit2->Text);
  if ((E > MyCollection->Count-1) || (E < 0)) {
    ShowMessage("Element value out of range (0.." + IntToStr(MyCollection->Count) + ")");
    Edit2->Text = "0";
    return;
  }

  Data = MyCollection->At(E);
  if (Data) {
    *(ARecord*) Data = AR;
    MyCollection->AtPut(E, Data);
    FillListBox();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DelBtnClick(TObject *Sender)
{
  ARecord AR;
  void * PN;

  if (!CheckControls(AR)) {
    ShowMessage("One or more data entry fields invalid");
    return;
  }

  PN = MyCollection->Iterate(MatchCollString, true, &AR);
  if (!PN) {
    MyCollection->Delete(PN);
    FillListBox();
  } else
    ShowMessage("Data not found");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AtDelBtnClick(TObject *Sender)
{
  long E;

  if (Edit2->Text == "")
    E = 0;
  else
    E = StrToInt(Edit2->Text);
  if ((E > MyCollection->Count-1) || (E < 0)) {
    ShowMessage("Element value out of range (0.." + IntToStr(MyCollection->Count) + ")");
    Edit2->Text = "0";
    return;
  }
  MyCollection->AtDelete(E);
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::InsBtnClick(TObject *Sender)
{
  int E;
  ARecord* AR;

  if (Edit2->Text == "")
    E = 0;
  else
    E = StrToInt(Edit2->Text);

  if ((E > MyCollection->Count-1) || (E < 0)) {
    ShowMessage("Element value out of range (0.." + IntToStr(MyCollection->Count) + ")");
    Edit2->Text = "0";
    return;
  }

  AR = new ARecord;
  if (!CheckControls(*AR)) {
    ShowMessage("One or more data entry fields invalid");
    delete AR;
    return;
  }

  MyCollection->Insert(AR);
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EffBtnClick(TObject *Sender)
{
  Edit1->Text = IntToStr(MyCollection->Efficiency);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadBtnClick(TObject *Sender)
{
  if (OD1->Execute()) {
    if (!MyCollection) {
      UpdateButtons(false);
      MyCollection = new TStCollection(100);
      MyCollection->DisposeData = MyDelNodeData;
      MyCollection->LoadData = MyLoadData;
      MyCollection->StoreData = MyStoreData;
    }

    LB1->Clear();
    MyCollection->Clear();

    SetBusy(true);
    MyCollection->LoadFromFile(OD1->FileName);
    MyCollection->Pack();
    SetBusy(false);

    FillListBox();
    UpdateButtons(true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  if (SD1->Execute()) {
    SetBusy(true);
    MyCollection->StoreToFile(SD1->FileName);
    SetBusy(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LB1Click(TObject *Sender)
{
  Edit2->Text = IntToStr(LB1->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LB1DblClick(TObject *Sender)
{
  MyCollection->AtDelete(LB1->ItemIndex);
  FillListBox();
  Edit2->Text = '0';
}
//---------------------------------------------------------------------------