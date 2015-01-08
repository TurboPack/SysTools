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

#include "exmmfcu.h"
#pragma link "ststrms"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnOpenMMFClick(TObject *Sender)
{
  try {
    AMMFile = new TStMemoryMappedFile("", 100, false, true);
    btnOpenMMF->Enabled  = false;
    btnCloseMMF->Enabled = true;
    Timer1->Enabled      = true;
    Label1->Caption      = "Waiting: ";
  }
  catch (...) {
    ShowMessage("Unable to open file");
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnCloseMMFClick(TObject *Sender)
{
  delete AMMFile;
  AMMFile = NULL;

  btnOpenMMF->Enabled  = true;
  btnCloseMMF->Enabled = false;
  Timer1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  Label1->Caption = "Seeking: " + IntToStr(AMMFile->DataSize);
  Timer1->Enabled = false;
  AMMFile->Seek(0, soFromBeginning);
  AMMFile->Read(Buf, AMMFile->DataSize);
  Edit1->Text = StrPas(Buf);
  Timer1->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete AMMFile;
  AMMFile = NULL;
}
//---------------------------------------------------------------------------