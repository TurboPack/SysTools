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

#include "gridfil0.h"
#pragma link "sttxtdat"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearGrid(bool ClearCaptions)
{
  if (ClearCaptions) {
    StringGrid1->Rows[0]->Clear();
  }
  for (int i = 1; i < StringGrid1->RowCount; i++) {
    StringGrid1->Rows[i]->Clear();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FillCaptions(void)
{
  StringGrid1->ColCount = Schema->Captions->Count;
  StringGrid1->Rows[0]->Assign(Schema->Captions);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FillCells(void)
{
  StringGrid1->RowCount = DataSet->Count;
  int i = 1;
  DataSet->First();
  while (!DataSet->EOF()) {
    StringGrid1->Rows[i]->Assign(DataSet->CurrentRecord->Values);
    DataSet->Next();
    i++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  delete Schema;
  delete DataSet;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  if (OpenDialog1->Execute()) {
    ClearGrid(true);
    delete Schema;
    Schema = new TStTextDataSchema;
    Schema->LoadFromFile(OpenDialog1->FileName);
    FillCaptions();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  if (OpenDialog2->Execute()) {
    ClearGrid(false);
    delete DataSet;
    DataSet = new TStTextDataRecordSet;
    DataSet->Schema = Schema;
    DataSet->LoadFromFile(OpenDialog2->FileName);
    FillCells();
  }
}
//---------------------------------------------------------------------------
