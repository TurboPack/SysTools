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

#include "ExUsrMg2.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TGroupPropertiesForm *GroupPropertiesForm;
//---------------------------------------------------------------------------
__fastcall TGroupPropertiesForm::TGroupPropertiesForm(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TGroupPropertiesForm::FormShow(TObject *Sender)
{
  Caption = "Group Properties" + G->Name;
  GroupNameEdit->Text = G->Name;
  CommentEdit->Text = G->Comment;

  ML = G->Items;
  for (int i=0;i<ML->Count;i++) {
    TStNetItem* MI = (TStNetItem*)ML->Objects[i];
    TListItem* LI = ListView1->Items->Add();
    LI->Caption = MI->Name;
    LI->SubItems->Add(MI->Comment);
  }
}
//---------------------------------------------------------------------------
