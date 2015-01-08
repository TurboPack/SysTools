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

#include "dbexprt0.h"
#pragma link "stexport"
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
void __fastcall TForm1::SetCaption(String FN)
{
  Caption = DefaultCaption;
  if (FN != "") {
    Caption = Caption + " - [" + FN + "]";
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  SchemaMaker = new TStDbSchemaGenerator;

  try {
    SchemaMaker->FieldDelimiter = StDeEscape(Edit1->Text);

    SchemaMaker->DataSet = Table1;

    SaveDialog1->FileName = ChangeFileExt(ExtractFileName(CurrentDb), ".sch");
    SaveDialog1->DefaultExt = "sch";
    SaveDialog1->Filter = "Schema Files /*.sch)|*.sch|All Files /*.*/|*.*";
    SaveDialog1->Title = "Save Schema";
    if (SaveDialog1->Execute()) {
      SchemaMaker->SchemaName = ChangeFileExt(ExtractFileName(SaveDialog1->FileName), "");
      SchemaMaker->ExportToFile(SaveDialog1->FileName);
      Memo1->Lines->LoadFromFile(SaveDialog1->FileName);
    }
  }
  __finally {
    delete SchemaMaker;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  if (OpenDialog1->Execute()) {
    Table1->Close();
    CurrentDb = OpenDialog1->FileName;
    Table1->TableName = CurrentDb;
    Table1->Open();
    SetCaption(CurrentDb);
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button3Click(TObject *Sender)
{
  Exporter = new TStDBtoCSVExport;

  try {
    Exporter->DataSet = Table1;
    Exporter->IncludeHeader = CheckBox1->Checked;
    Exporter->FieldDelimiter = StDeEscape(Edit1->Text);

    SaveDialog1->FileName = ChangeFileExt(ExtractFileName(CurrentDb), ".csv");
    SaveDialog1->DefaultExt = "csv";
    SaveDialog1->Filter = "CSV Files /*.csv)|*.csv|All Files /*.*/|*.*";
    SaveDialog1->Title = "Save Table Data";
    if (SaveDialog1->Execute()) {
      Exporter->ExportToFile(SaveDialog1->FileName);
      Memo1->Lines->LoadFromFile(SaveDialog1->FileName);
    }
  }
  __finally {
    delete Exporter;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button4Click(TObject *Sender)
{
  CurrentDb = "";
  Table1->Close();
  Table1->TableName = CurrentDb;
  Memo1->Lines->Clear();
  SetCaption(CurrentDb);
}
//---------------------------------------------------------------------------

