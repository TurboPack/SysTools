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
#include <vcl\forms.hpp>
#pragma hdrstop

#include "exregeu1.h"
#include "exregeu2.h"
#include "ststrs.hpp"
//---------------------------------------------------------------------------
#pragma link "StRegEx"
#pragma link "cgauges"
#pragma link "StBase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::bntSelAvoidClick(TObject *Sender)
{
  TForm2* Form2 = new TForm2(this);

  Form2->Memo1->Clear();
  Form2->Memo1->Lines->Assign(StRegEx1->SelAvoidPattern);
  if (Form2->ShowModal() == mrOk) {
    StRegEx1->SelAvoidPattern->Clear();
    StRegEx1->SelAvoidPattern->Assign(Form2->Memo1->Lines);
  }

  delete Form2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnMatchClick(TObject *Sender)
{
  TForm2* Form2 = new TForm2(this);

  Form2->Memo1->Clear();
  Form2->Memo1->Lines->Assign(StRegEx1->MatchPattern);
  if (Form2->ShowModal() == mrOk) {
    StRegEx1->MatchPattern->Clear();
    StRegEx1->MatchPattern->Assign(Form2->Memo1->Lines);
  }

  delete Form2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnReplaceClick(TObject *Sender)
{
  TForm2* Form2 = new TForm2(this);

  Form2->Memo1->Clear();
  Form2->Memo1->Lines->Assign(StRegEx1->ReplacePattern);
  if (Form2->ShowModal() == mrOk) {
    StRegEx1->ReplacePattern->Clear();
    StRegEx1->ReplacePattern->Assign(Form2->Memo1->Lines);
  }

  delete Form2;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  ACount = 0;

  if (cbxModOnly->Checked)
    StRegEx1->OutputOptions = StRegEx1->OutputOptions << ooModified;
  else
    StRegEx1->OutputOptions = StRegEx1->OutputOptions >> ooModified;

  if (cbxCountOnly->Checked)
    StRegEx1->OutputOptions = StRegEx1->OutputOptions << ooCountOnly;
  else
    StRegEx1->OutputOptions = StRegEx1->OutputOptions >> ooCountOnly;

  if ((Trim(edtSourceFile->Text) == "") ||
     (Trim(edtDestFile->Text) == "") &&
     (!StRegEx1->OutputOptions.Contains(ooCountOnly))) {
    ShowMessage("Source and/or Destination file cannot be blank");
    return;
  }

  if (!FileExists(Trim(edtSourceFile->Text))) {
    ShowMessage("Source file not found");
    return;
  }

  if ((StRegEx1->SelAvoidPattern->Count == 0) &&
     (StRegEx1->MatchPattern->Count == 0)) {
    ShowMessage("You must specify a SelAvoid or Match Pattern");
    return;
  }

  StRegEx1->IgnoreCase  = cbIgnoreCase->Checked;
  StRegEx1->Avoid       = !cbSelect->Checked;
  StRegEx1->LineNumbers = cbLineNumbers->Checked;
  StRegEx1->InputFile   = TrimS(edtSourceFile->Text);
  StRegEx1->OutputFile  = edtDestFile->Text;

  lblSelAvoid->Caption = "Sel/Avoid: 0";
  lblMatch->Caption    = "Match: 0";
  lblReplace->Caption  = "ReplaceL 0";
  lblLPS->Caption      = "Lines/Sec: 0";

  Screen->Cursor = crHourGlass;
  StRegEx1->Execute();
  Screen->Cursor = crDefault;


  Memo1->Clear();
  if (!StRegEx1->OutputOptions.Contains(ooCountOnly))
    Memo1->Lines->LoadFromFile(edtDestFile->Text);

  lblSelAvoid->Caption = "Sel/Avoid: " + IntToStr(StRegEx1->LinesSelected);
  lblMatch->Caption    = "Match: " + IntToStr(StRegEx1->LinesMatched);
  lblReplace->Caption  = "Replace: " + IntToStr(StRegEx1->LinesReplaced);
  lblLPS->Caption      = "Lines/Sec: " + IntToStr(StRegEx1->LinesPerSecond);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::SelectFile(TObject *Sender)
{
  if (Sender == sbSource) {
    OpenDialog1->Title = "Source File";
    OpenDialog1->Options = OpenDialog1->Options << ofFileMustExist;
    if (OpenDialog1->Execute())
      edtSourceFile->Text = OpenDialog1->FileName;
  }
  else {
    OpenDialog1->Title = "Destination File";
    OpenDialog1->Options = OpenDialog1->Options >> ofFileMustExist;
    if (OpenDialog1->Execute())
      edtDestFile->Text = OpenDialog1->FileName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StRegEx1Match(TObject *Sender,
	TMatchPosition &REPosition)
{
  ACount++;
  Caption = IntToStr(REPosition.LineNum);
  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StRegEx1Progress(TObject *Sender, WORD Percent)
{
  if (((Percent % 2) == 0) && (Gauge1->Progress != Percent)) {
    Gauge1->Progress = Percent;
  }
}
//---------------------------------------------------------------------------

