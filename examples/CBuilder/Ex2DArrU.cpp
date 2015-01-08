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

#include <stdlib.h>
#include "Ex2DArrU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StLArr"
#pragma link "StUtils"

#pragma resource "*.dfm"


TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
int __fastcall MyArraySort(const void* E1, const void* E2)
{
  return *(long*)E1 - *(long*)E2;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  //RegisterClass should be used, but it is not there in C++Builder 1.0
  RegisterClassAlias(__classid(TStLMatrix), "TStLMatrix");
  UpdateButtons(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete MyLMatrix;
}
//---------------------------------------------------------------------------
void TForm1::UpdateButtons(bool AOK)
{
  ClearBtn->Enabled  = AOK;
  FillBtn->Enabled   = AOK;
  PutBtn->Enabled    = AOK;
  PutRowBtn->Enabled = AOK;
  GetBtn->Enabled    = AOK;
  GetRowBtn->Enabled = AOK;
  SortBtn->Enabled   = AOK;
  SaveBtn->Enabled   = AOK;
}
//---------------------------------------------------------------------------
void TForm1::SetBusy(bool B)
{
  if (B)
    Screen->Cursor = crHourGlass;
  else
    Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void TForm1::FillListBox()
{
  long Value;

  SetBusy(true);
  ArrayLB->Clear();
  ArrayLB->Perform(WM_SETREDRAW, 0, 0);

  for (Cardinal Row = 0; Row < MyLMatrix->Rows; Row++) {
    for (Cardinal Col = 0; Col < MyLMatrix->Cols; Col++) {
      MyLMatrix->Get(Row, Col, &Value);
      ArrayLB->Items->Add(IntToStr(Row) + "," +
                        IntToStr(Col) + " = " + IntToStr((int)Value));
    };
  };

  ArrayLB->Perform(WM_SETREDRAW,1,0);
  ArrayLB->Update();
  SetBusy(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CreateBtnClick(TObject *Sender)
{
  long Value;

  ArrayLB->Clear();

  if (MyLMatrix)
    delete MyLMatrix;

  UpdateButtons(false);
  MyLMatrix = new TStLMatrix(MAXROWS, MAXCOLS, sizeof(long));
  MyLMatrix->ElementsStorable = true;

  SetBusy(true);
  for (Cardinal Row = 0; Row < MAXROWS; Row++) {
    for (Cardinal Col = 0; Col < MAXCOLS; Col++) {
      Value = floor(random(10000));
      MyLMatrix->Put(Row, Col, &Value);
    };
  };
  SetBusy(false);

  FillListBox();
  UpdateButtons(true);

  LMRow->Text = "0";
  LMCol->Text = "0";
  MyLMatrix->Get(0, 0, &Value);
  LMValue->Text = IntToStr((int)Value);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearBtnClick(TObject *Sender)
{
  long Value;

  MyLMatrix->Clear();
  ArrayLB->Clear();

  LMRow->Text = "0";
  LMCol->Text = "0";
  MyLMatrix->Get(0, 0, &Value);
  LMValue->Text = IntToStr((int)Value);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FillBtnClick(TObject *Sender)
{
  long Row, Col, Value;

  if (LMValue->Text == "") {
    ShowMessage("No value entered");
    return;
  };

  Value = StrToInt(LMValue->Text);
  MyLMatrix->Fill(&Value);

  FillListBox();

  Row = 0;
  Col = 0;
  LMRow->Text = IntToStr((int)Row);
  LMCol->Text = IntToStr((int)Col);

  MyLMatrix->Get(Row, Col, &Value);
  LMValue->Text = IntToStr((int)Value);

  SetBusy(false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PutBtnClick(TObject *Sender)
{
  long LBE, Row, Col, Value;

  if (LMValue->Text == "") {
    ShowMessage("No value entered");
    return;
  };

  if (LMRow->Text == "")
    LMRow->Text = "0";
  if (LMCol->Text == "")
    LMCol->Text = "0";

  Value = StrToInt(LMValue->Text);
  Row = StrToInt(LMRow->Text);
  Col = StrToInt(LMCol->Text);
  MyLMatrix->Put(Row, Col, &Value);

  LBE = (Row * MAXROWS) + Col;
  ArrayLB->Items->Strings[LBE] = IntToStr((int)Row) + "," +
                        IntToStr((int)Col) + " = " + IntToStr((int)Value);

  Row = StrToInt(LMRow->Text);
  Col = StrToInt(LMCol->Text);
  MyLMatrix->Get(Row, Col, &Value);
  LMValue->Text = IntToStr((int)Value);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PutRowBtnClick(TObject *Sender)
{
  long Row, Col, Value;

  if (LMValue->Text == "") {
    ShowMessage("No value entered");
    return;
  };

  if (LMRow->Text == "")
    LMRow->Text = "0";

  Value = StrToInt(LMValue->Text);
  Row = StrToInt(LMRow->Text);

  FillStruct(&LIArray, MAXCOLS, &Value, sizeof(Value));

  MyLMatrix->PutRow(Row, &LIArray);
  FillListBox();

  Row = StrToInt(LMRow->Text);
  Col = 0;
  MyLMatrix->Get(Row, Col, &Value);

  LMValue->Text = IntToStr((int)Value);
  LMCol->Text = "0";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GetBtnClick(TObject *Sender)
{
  long LBE, Row, Col, Value;

  if (LMValue->Text == "") {
    ShowMessage("No value entered");
    return;
  };

  if (LMRow->Text == "")
    LMRow->Text = "0";
  if (LMCol->Text == "")
    LMCol->Text = "0";

  Value = StrToInt(LMValue->Text);
  Row = StrToInt(LMRow->Text);
  Col = StrToInt(LMCol->Text);
  MyLMatrix->Get(Row, Col, &Value);

  LMRow->Text = IntToStr((int)Row);
  LMCol->Text = IntToStr((int)Col);
  LMValue->Text = IntToStr((int)Value);

  LBE = (Row * MAXCOLS) + Col;
  ArrayLB->ItemIndex = LBE;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GetRowBtnClick(TObject *Sender)
{
  long LIV;

  if (LMRow->Text == "")
    LMRow->Text = "0";

  LIV = 0;
  FillStruct(&LIArray, MAXCOLS, &LIV, sizeof(LIV));
  MyLMatrix->GetRow(0, &LIArray);

  ArrayLB->Clear();
  ArrayLB->Perform(WM_SETREDRAW,0,0);

  for (long Step = 0; Step < MAXCOLS; Step++)
    ArrayLB->Items->Add("Col" + IntToStr((int)Step) + ": " + IntToStr((int)LIArray[Step]));

  ArrayLB->Perform(WM_SETREDRAW, 1, 0);
  ArrayLB->Update();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SortBtnClick(TObject *Sender)
{
  MyLMatrix->SortRows(0, MyArraySort);
  FillListBox();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::LoadBtnClick(TObject *Sender)
{
  if (OD1->Execute()) {
    if (!MyLMatrix) {
      UpdateButtons(false);
      MyLMatrix = new TStLMatrix(MAXROWS, MAXCOLS, sizeof(long));
      MyLMatrix->ElementsStorable = true;
    };
    MyLMatrix->LoadFromFile(OD1->FileName);
    FillListBox();
    UpdateButtons(true);
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  if (SD1->Execute())
    MyLMatrix->StoreToFile(SD1->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ArrayLBDblClick(TObject *Sender)
{
  long Row, Col, I, Value;

  I = ArrayLB->ItemIndex;
  Row = I / MAXCOLS;
  Col = I % MAXCOLS;

  MyLMatrix->Get(Row, Col, &Value);
  LMRow->Text = IntToStr((int)Row);
  LMCol->Text = IntToStr((int)Col);
  LMValue->Text = IntToStr((int)Value);
}
//---------------------------------------------------------------------------
