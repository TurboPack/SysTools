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

#include "ExVInfoU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StVInfo"
#pragma link "StBase"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::Button1Click(TObject *Sender)
{
  if (OpenDialog1->Execute()) {
    VerInfo->FileName = OpenDialog1->FileName;
    ShowVersionInfo();
  }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
  ShowVersionInfo();
}
//---------------------------------------------------------------------------
void TMainForm::ShowVersionInfo()
{
  Memo1->Clear();
  Memo1->Lines->Add("Comments: " + VerInfo->Comments);
  Memo1->Lines->Add("Company Name: " + VerInfo->CompanyName);
  Memo1->Lines->Add("File Description: " + VerInfo->FileDescription);
  Memo1->Lines->Add("File Version: " + VerInfo->FileVersion);
  Memo1->Lines->Add("Internal Name: " + VerInfo->InternalName);
  Memo1->Lines->Add("Legal Copyright: " + VerInfo->LegalCopyright);
  Memo1->Lines->Add("Legal Trademark: " + VerInfo->LegalTrademark);
  Memo1->Lines->Add("Original Filename: " + VerInfo->OriginalFilename);
  Memo1->Lines->Add("Product Name: " + VerInfo->ProductName);
  Memo1->Lines->Add("Product Version: " + VerInfo->ProductVersion);
  if (UpperCase(ExtractFileName(VerInfo->FileName))
     == UpperCase("exvrinfo.exe")) {
   Memo1->Lines->Add("Extra Info 1: " + VerInfo->GetKeyValue("ExtraInfo1"));
   Memo1->Lines->Add("Extra Info 2: " + VerInfo->GetKeyValue("ExtraInfo2"));
  }
  Memo1->Lines->Add("Language: " + VerInfo->LanguageName);
  if ((int)VerInfo->FileDate != 0)
    Memo1->Lines->Add("File Date: " + DateToStr(VerInfo->FileDate));
}
