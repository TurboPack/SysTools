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

#include "exspawnu.h"
//---------------------------------------------------------------------------
#pragma link "StSpawn"
#pragma link "StBase"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::EnableControls(bool B)
{
  RG1->Enabled = B;
  RG2->Enabled = B;
  cbNotify->Enabled = B;
  cbTimeout->Enabled = B;
  btnSpawn->Enabled = B;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StSpawnApplication1Completed(TObject *Sender)
{
  EnableControls(true);
  ShowMessage("Done");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StSpawnApplication1SpawnError(TObject *Sender,
	WORD Error)
{
  EnableControls(true);
  ShowMessage(IntToStr(Error));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StSpawnApplication1TimeOut(TObject *Sender)
{
  EnableControls(true);
  ShowMessage("Timed Out");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSpawnClick(TObject *Sender)
{
  if (OpenDialog1->Execute()) {
    StSpawnApplication1->FileName = OpenDialog1->FileName;
    StSpawnApplication1->SpawnCommand = TStSpawnCommand(RG1->ItemIndex);
    StSpawnApplication1->NotifyWhenDone = cbNotify->Checked;
    if (RG2->ItemIndex == 0)
      StSpawnApplication1->ShowState = ssMinimized;
    else
      StSpawnApplication1->ShowState = ssNormal;
    if (cbTimeout->Checked)
      StSpawnApplication1->TimeOut = 15;
    else
      StSpawnApplication1->TimeOut = 0;
    EnableControls(StSpawnApplication1->TimeOut == 0);
    StSpawnApplication1->Execute();
  }
}
//---------------------------------------------------------------------------