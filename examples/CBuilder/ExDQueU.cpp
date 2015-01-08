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

#include "ExDQueU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StDQue"

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
  delete (String*)Data;
}
//---------------------------------------------------------------------------
void * __fastcall MyLoadData(Classes::TReader* Reader)
{
  String *S;
  S = new String;
  *S = Reader->ReadString();
  return S;
}
//---------------------------------------------------------------------------
void __fastcall MyStoreData(Classes::TWriter* Writer, void * Data)
{
  Writer->WriteString( (*(String*)Data) );
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RegisterClassAlias(__classid(TStDQue), "TStDQue");
  RegisterClassAlias(__classid(TStListNode), "TStListNode");
  UpdateButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete MyDQue;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateBtnClick(TObject *Sender)
{
  String *S;

  if (MyDQue)
    delete MyDQue;

  UpdateButtons(false);
  MyDQue = new TStDQue(__classid(TStListNode));

  MyDQue->DisposeData = MyDelNodeData;
  MyDQue->LoadData = MyLoadData;
  MyDQue->StoreData = MyStoreData;

  for (int I = 1; I <= 100; I++) {
    S = new String;
    *S = "Item" + IntToStr(I);
    MyDQue->Append(S);
  }
  FillListBox();
  UpdateButtons(true);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearBtnClick(TObject *Sender)
{
  MyDQue->Clear();
  Edit1->Text = "";
  FillListBox();
  UpdateButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PushHeadBtnClick(TObject *Sender)
{
  String *NewString;

  if (Edit1->Text == "") {
    ShowMessage("No value entered");
    return;
  }

  NewString = new String;
  *NewString = Edit1->Text;
  MyDQue->PushHead(NewString);
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PopHeadBtnClick(TObject *Sender)
{
  MyDQue->PopHead();
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PushTailBtnClick(TObject *Sender)
{
  String *NewString;

  if (Edit1->Text == "") {
    ShowMessage("No value entered");
    return;
  }

  NewString = new String;
  *NewString = Edit1->Text;
  MyDQue->PushTail(NewString);
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PopTailBtnClick(TObject *Sender)
{
  MyDQue->PopTail();
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::HeadBtnClick(TObject *Sender)
{
  void *Data;

  MyDQue->PeekHead(Data);
  Edit1->Text = *((String*)Data);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::TailBtnClick(TObject *Sender)
{
  void *Data;

  MyDQue->PeekTail(Data);
  Edit1->Text = *((String*)Data);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadBtnClick(TObject *Sender)
{
  if (OD1->Execute()) {
    if (!MyDQue) {
      UpdateButtons(false);
      MyDQue = new TStDQue(__classid(TStListNode));
      MyDQue->DisposeData = MyDelNodeData;
      MyDQue->LoadData = MyLoadData;
      MyDQue->StoreData = MyStoreData;
    }
    MyDQue->LoadFromFile(OD1->FileName);
    FillListBox();
    UpdateButtons(true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  if (SD1->Execute())
    MyDQue->StoreToFile(SD1->FileName);
}
//---------------------------------------------------------------------------
void TForm1::FillListBox(void)
{
  TStListNode *PN;

  LB1->Clear();
  LB1->Perform(WM_SETREDRAW, 0, 0);
  PN = MyDQue->Head;
  while (PN) {
    LB1->Items->Add(*((String*)(PN->Data)));
    PN = MyDQue->Next(PN);
  }
  LB1->Perform(WM_SETREDRAW, 1, 0);
  LB1->Update();
}
//---------------------------------------------------------------------------
void TForm1::UpdateButtons(bool QueOK)
{
  ClearBtn->Enabled = QueOK;
  PushHeadBtn->Enabled  = QueOK;
  PopHeadBtn->Enabled  = QueOK;
  PushTailBtn->Enabled  = QueOK;
  PopTailBtn->Enabled  = QueOK;
  HeadBtn->Enabled  = QueOK;
  TailBtn->Enabled  = QueOK;
  SaveBtn->Enabled  = QueOK;
}
//---------------------------------------------------------------------------
