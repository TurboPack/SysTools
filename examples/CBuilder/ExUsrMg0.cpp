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
#include <vcl.h>
#pragma hdrstop

#include "ExUsrMg0.h"
#include "ExUsrMg1.h"
#include "ExUsrMg2.h"
#include "ExUsrMg3.h"
#include "ExUsrMg4.h"
#pragma link "stnet"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::SelectServer1Click(TObject *Sender)
{
  String SelectedServer;
  if (InputQuery("Server name", "Select the server name to use", SelectedServer)) {
    CS = StNetwork->Server[SelectedServer];

    if (CS) {

      // load the users into the listview
      ListView1->Items->Clear();
      UL = CS->Users;
      for (int i=0;i<UL->Count;i++) {
        TStNetUserItem* U = (TStNetUserItem*)UL->Objects[i];
        if ((U->ItemType == nitLocalUser) || (U->ItemType == nitGlobalUser)) {
          TListItem* LI = ListView1->Items->Add();
          LI->Caption = U->Name;
          LI->SubItems->Add(U->FullName);
          LI->SubItems->Add(U->Comment);
        }
      }

      // load the groups into the listview
      ListView2->Items->Clear();
      GL = CS->Groups;
      for (int i=0;i<GL->Count;i++) {
        TStNetGroupItem* G = (TStNetGroupItem*)GL->Objects[i];
        if ((G->ItemType == nitLocalGroup) || (G->ItemType == nitGlobalGroup)) {
          TListItem* LI = ListView2->Items->Add();
          LI->Caption = G->Name;
          LI->SubItems->Add(G->Comment);
        }
      }

      // check for GloableGroup creation on domain controllers
      if (CS->ServerType.Contains(nsvtDomainCtrl) || CS->ServerType.Contains(nsvtDomainBackupCtrl))
        NewGlobalGroup1->Enabled = true;
      else
        NewGlobalGroup1->Enabled = false;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::About1Click(TObject *Sender)
{
  String AdditionalText = "An network example program from the TurboPower SysTools Library";
  String Caption = MainForm->Caption;
  Application->MessageBox(AdditionalText.c_str(), Caption.c_str(), MB_OK | MB_ICONINFORMATION);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Properties1Click(TObject *Sender)
{
  TListItem* LI;
  // Are we dealing with a user or group?
  if (ListView1->Focused() && ListView1->SelCount > 0) {
    LI = ListView1->Selected;

    TUserPropertiesForm* UPF = new TUserPropertiesForm(this);
    UPF->U = (TStNetUserItem*)UL->Objects[UL->IndexOf(LI->Caption)];
    if (UPF->ShowModal() == mrOk) {
      // update the data
      UPF->U->Name = UPF->UserNameEdit->Text;
      UPF->U->FullName = UPF->FullNameEdit->Text;
      UPF->U->Comment = UPF->CommentEdit->Text;

      if ((UPF->Password1Edit->Text != "NO PASSWORD CHANGE"))
        UPF->U->Password = UPF->Password1Edit->Text;

      LI->Caption = UPF->U->Name;
      LI->SubItems->Strings[0] = UPF->U->FullName;
      LI->SubItems->Strings[1] = UPF->U->Comment;
    }
    delete UPF;
  } else if (ListView2->Focused() && ListView2->SelCount > 0) {
    LI = ListView2->Selected;
    TGroupPropertiesForm* GPF = new TGroupPropertiesForm(this);
    GPF->G = (TStNetGroupItem*)GL->Objects[GL->IndexOf(LI->Caption)];
    if (GPF->ShowModal()) {
      // update the data
      GPF->G->Name = GPF->GroupNameEdit->Text;
      GPF->G->Comment = GPF->CommentEdit->Text;

      LI->Caption = GPF->G->Name;
      LI->SubItems->Strings[0] = GPF->G->Comment;
    }
    delete GPF;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Delete1Click(TObject *Sender)
{
  TListItem* LI;
  // Are we dealing with a user or group?
  if (ListView1->Focused() && ListView1->SelCount > 0) {
    LI = ListView1->Selected;
    TStNetUserItem* U = (TStNetUserItem*)UL->Objects[UL->IndexOf(LI->Caption)];

    if (MessageDlg("Are you sure you want to delete the user - [" + U->Name + "]?",
      mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes) {
      U->Delete();
      LI->Delete();
    }
  } else if (ListView2->Focused() && ListView2->SelCount > 0) {
    LI = ListView2->Selected;
    TStNetGroupItem* G = (TStNetGroupItem*)GL->Objects[GL->IndexOf(LI->Caption)];
    if (MessageDlg("Are you sure you want to delete the group - [" + G->Name + "]?",
      mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0) == mrYes) {
      G->Delete();
      LI->Delete();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ListViewKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
  if (Key == VK_RETURN) {
    Properties1Click(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::NewUser1Click(TObject *Sender)
{
  if (!CS)
    return;
  // Create a new user
  TNewUserForm* NU = new TNewUserForm(this);
  NU->Caption = CS->Name + " - New User Account";
  if (NU->ShowModal() == mrOk) {
    TStNetUserItem* U = CS->AddUser(NU->UserNameEdit->Text, NU->Password1Edit->Text, true);
    U->FullName = NU->FullNameEdit->Text;
    U->Comment  = NU->CommentEdit->Text;

    TListItem* LI = ListView1->Items->Add();
    LI->Caption = U->Name;
    LI->SubItems->Add(U->FullName);
    LI->SubItems->Add(U->Comment);

    UL = CS->Users;
  }
  delete NU;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::NewGlobalGroup1Click(TObject *Sender)
{
  if (!CS)
    return;
  // Create a new global group
  TNewGroupForm* NG = new TNewGroupForm(this);
  NG->Caption = CS->Name + " - New Global Group";
  if (NG->ShowModal() == mrOk) {
    TStNetGroupItem* G = CS->AddGroup(NG->GroupNameEdit->Text, NG->CommentEdit->Text, true);

    TListItem* LI = ListView2->Items->Add();
    LI->Caption = G->Name;
    LI->SubItems->Add(G->Comment);

    GL = CS->Groups;
  }
  delete NG;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::NewLocalGroup1Click(TObject *Sender)
{
  if (!CS)
    return;
  // create a new local group
  TNewGroupForm* NG = new TNewGroupForm(this);
  NG->Caption = CS->Name + " - New Local Group";
  if (NG->ShowModal() == mrOk) {
    TStNetGroupItem* G = CS->AddGroup(NG->GroupNameEdit->Text, NG->CommentEdit->Text, false);

    TListItem* LI = ListView2->Items->Add();
    LI->Caption = G->Name;
    LI->SubItems->Add(G->Comment);

    GL = CS->Groups;
  }
  delete NG;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Exit1Click(TObject *Sender)
{
  Close();    
}
//---------------------------------------------------------------------------
