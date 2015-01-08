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

#include "ex2htmlu.h"
//---------------------------------------------------------------------------
#pragma link "StToHTML"
#pragma link "StBase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StFileToHTML1Progress(TObject *Sender, WORD Percent)
{
  Label1->Caption = "Completed: " + IntToStr(Percent) + "%";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GetFileClick(TObject *Sender)
{
  if (OpenDialog1->Execute()) {
    if (Sender == SpeedButton1)
      Edit1->Text = OpenDialog1->FileName;
    else if (Sender == SpeedButton2)
      Edit2->Text = OpenDialog1->FileName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  if (! FileExists(Edit1->Text)) {
    ShowMessage("Input file does not exist");
    return;
  }

  StFileToHTML1->InFileName = Edit1->Text;
  StFileToHTML1->OutFileName = Edit2->Text;

  StFileToHTML1->Execute();
  Label1->Caption = "Waiting";
  ShowMessage("Done");
}
//---------------------------------------------------------------------------