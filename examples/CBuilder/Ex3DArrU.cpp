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

#include "Ex3DArrU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StLArr"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TMy3D::TMy3D(Cardinal X, Cardinal Y, Cardinal Z)
        : TStLArray(Z, 4/*sizeof(TStLMatrix)*/)
{
  long row, col, up, Value;
  TStLMatrix* A;

  XMax = X;
  YMax = Y;
  ZMax = Z;

  for (up = 0; up < ZMax; up++) {
    A = new TStLMatrix(XMax, YMax, sizeof(long));
    for (row = 0; row < YMax; row++) {
      for (col = 0; col < XMax; col++) {
        Value = up + 100 * col + 10000 * row;
        A->Put(row, col, &Value);
      };
    };
    Put(up, &A);
  };
}
//---------------------------------------------------------------------------
__fastcall TMy3D::~TMy3D(void)
{
  long Up;
  TStLMatrix* A;

  for (Up = 0; Up < ZMax; Up++) {
    Get(Up, &A);
    if (A)
      delete A;
  };
}
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  My3D = new TMy3D(50, 50, 50);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormClose(TObject *Sender, TCloseAction &Action)
{
  delete My3D;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  long XV, YV, ZV, Value;
  TStLMatrix *Z;

  XV = StrToInt(Edit1->Text);
  YV = StrToInt(Edit2->Text);
  ZV = StrToInt(Edit3->Text);
  if ((XV < 0) || (XV >= 50)) {
    Edit1->Text = "0";
    XV = StrToInt(Edit1->Text);
  };
  if ((YV < 0) || (YV >= 50)) {
    Edit2->Text = "0";
    YV = StrToInt(Edit2->Text);
  };
  if ((ZV < 0) || (ZV >= 50)) {
    Edit3->Text = "0";
    ZV = StrToInt(Edit3->Text);
  };

  My3D->Get(ZV, &Z);
  Z->Get(XV, YV, &Value);
  Edit4->Text = IntToStr((int)Value);
}
//---------------------------------------------------------------------------