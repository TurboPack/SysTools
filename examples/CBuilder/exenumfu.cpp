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

#include "exenumfu.h"
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
bool __fastcall ProcessAll(const Sysutils::TSearchRec &SR, bool ForInclusion,
  bool &Abort)
{
  Abort = false;
  if (StopIt) {
    Abort = true;
    return  false;
  }
  else {
    Application->ProcessMessages();
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TextFilesOnly(const Sysutils::TSearchRec &SR, bool ForInclusion,
  bool &Abort)
{
  bool Rslt;
  Abort = false;
  if (StopIt) {     
    Abort = true;
    return false;
  }
  else {
    Application->ProcessMessages();

    if (ForInclusion) {
      Rslt = false;
      String S = SR.Name;
      if ((UpperCase(ExtractFileExt(S)) == ".TXT") ||
         ((SR.Attr && faDirectory) > 0)) {
        Rslt = true;
      }
      return Rslt;
    }
    else {
      return  true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  StopIt = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  AList = new TStringList;
  Edit1->Text = "C:\\";
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormDestroy(TObject *Sender)
{
  delete AList;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  AList->Clear();
  ListBox1->Clear();

  Screen->Cursor = crHourGlass;
  StopIt = false;

  if (!CheckBox2->Checked) {
    EnumerateFiles(Edit1->Text, AList, CheckBox1->Checked, ProcessAll);
    ListBox1->Items = AList;
  } else {
    EnumerateFiles(Edit1->Text, AList, CheckBox1->Checked, TextFilesOnly);
    /*by its nature, EnumerateFiles also returns directory names*/
    /*remove those from the list - leaving only the text files*/
    for (long int I = 0; I < AList->Count; I++) {
      if (UpperCase(ExtractFileExt(AList->Strings[I])) == ".TXT") {
        ListBox1->Items->Add(AList->Strings[I]);
      }  
    }
  }

  Screen->Cursor = crDefault;
  ListBox1->Update();

}
//---------------------------------------------------------------------------
 