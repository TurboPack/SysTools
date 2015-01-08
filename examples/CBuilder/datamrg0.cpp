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

#include "datamrg0.h"
#pragma link "stmerge"
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
void __fastcall TForm1::DoUnknownTag(TObject *Sender, AnsiString Tag, AnsiString &Value, bool &Discard)
{
  if (Tag == "TIME") {
    Value = FormatDateTime("hh:mm:ss", Now());
  }
  else {
    Value = InputBox("Unknown Tag", "Value for " + Tag, "");
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DisableButtons(void)
{
  SpeedButton1->Enabled = false;
  SpeedButton2->Enabled = false;
  SpeedButton3->Enabled = false;
  SpeedButton4->Enabled = false;
}
//---------------------------------------------------------------------------
String __fastcall TForm1::NextFile(void)
{
  TVarRec FmtParam[] = { MergeNo };
  String S = ChangeFileExt(ExtractFileName(TemplateName),
     Format(".M%.2d", FmtParam, ARRAYSIZE(FmtParam) - 1));
  MergeNo++;
  return S;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::UpdateButtons(void)
{
  if (DataSet->Active) {

    SpeedButton1->Enabled = true;
    SpeedButton2->Enabled = true;
    SpeedButton3->Enabled = true;
    SpeedButton4->Enabled = true;

    if (DataSet->BOF()) {
      SpeedButton1->Enabled = false;
      SpeedButton2->Enabled = false;
    }

    if (DataSet->EOF()) {
      SpeedButton3->Enabled = false;
      SpeedButton4->Enabled = false;
    }

  } else {
  DisableButtons();
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::UpdateTagDisplay(void)
{
  Memo2->Lines->Assign(DataSet->CurrentRecord->FieldList);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Merger = new TStTextMerge;
  Merger->DefaultTags->Add("FIRST_NAME=Sir or Madam");
  Merger->DefaultTags->Add("CITY=ANYTOWN");
  Merger->DefaultTags->Add("COLOR=BLUE");
  Merger->OnGotUnknownTag = DoUnknownTag;
  DisableButtons();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  delete Schema;
  delete DataSet;
  delete Merger;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearMemo(TObject *Sender)
{
  dynamic_cast<TMemo *>(Sender)->Lines->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::NavClick(TObject *Sender)
{
  if (Sender == SpeedButton1) {DataSet->First();};
  if (Sender == SpeedButton2) {DataSet->Prior();};
  if (Sender == SpeedButton3) {DataSet->Next();};
  if (Sender == SpeedButton4) {DataSet->Last();};

  UpdateButtons();
  UpdateTagDisplay();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  if (OpenDialog1->Execute()) {
    TemplateName = OpenDialog1->FileName;
    MergeNo = 1;
    Merger->LoadTemplateFromFile(TemplateName);
    Memo3->Lines->Assign(Merger->Template);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  if (OpenDialog2->Execute() && OpenDialog3->Execute()) {
    delete Schema;
    Schema = new TStTextDataSchema;
    Schema->LoadFromFile(OpenDialog2->FileName);

    delete DataSet;
    DataSet = new TStTextDataRecordSet;
    DataSet->Schema = Schema;
    DataSet->LoadFromFile(OpenDialog3->FileName);
    DataSet->First();

    UpdateButtons();
    UpdateTagDisplay();

  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
  Merger->MergeTags->Assign(Memo2->Lines);
  Merger->Merge();
  Memo1->Lines->Assign(Merger->MergedText);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button4Click(TObject *Sender)
{
  SaveDialog1->FileName = NextFile();
  if (SaveDialog1->Execute()) {
    Memo3->Lines->SaveToFile(SaveDialog1->FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button5Click(TObject *Sender)
{
  SaveDialog2->FileName = TemplateName;
  if (SaveDialog2->Execute()) {
    TemplateName = SaveDialog2->FileName;
    Memo3->Lines->SaveToFile(TemplateName);
    Merger->Template->Assign(Memo3->Lines);
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button6Click(TObject *Sender)
{
  if (DataSet && DataSet->Active) {
    DataSet->Active = False;
    delete DataSet;
    DataSet = NULL;
    Memo2->Lines->Clear();
    DisableButtons();
  }
}
//---------------------------------------------------------------------------
