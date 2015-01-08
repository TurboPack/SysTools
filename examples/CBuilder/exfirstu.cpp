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
#include <stfirst.hpp>
#include <ststrl.hpp>
#pragma hdrstop

#include "exfirstu.h"
//---------------------------------------------------------------------------
#pragma link "StwmDCpy"
#pragma link "StBase"
#pragma link "StStrL"
#pragma link "StWmDCpy"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::StWMDataCopy1DataReceived(TObject *Sender,
	tagCOPYDATASTRUCT &CopyData)
{
  AnsiString S;
  unsigned I;

  S = (char*)CopyData.lpData;
  I = 0;
  if (StrChPosL(S, ' ', I))
    S = StrStCopyL(S, I+1, S.Length()-I);
  else {
    if (StrChPosL(S, 0x9, I))
      S = StrStCopyL(S, I+1, S.Length()-I);
  }
  if (I > 0)
    DoFileOpen(S);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnLoadFileClick(TObject *Sender)
{
  if (OpenDialog1->Execute())
    Memo1->Lines->LoadFromFile(OpenDialog1->FileName);	
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  if (ParamCount > 0)
    DoFileOpen(ParamStr(1));
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DoFileOpen(ShortString FN)
{
  Memo1->Clear();
  if (FileExists(FN))
    Memo1->Lines->LoadFromFile(FN);
}