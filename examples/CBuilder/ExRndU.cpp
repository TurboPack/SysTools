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

#include "ExRndU.h"
//---------------------------------------------------------------------------
#pragma link "strandom"
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;

const
  AnsiString DistNames[12] = {
    "Beta", "Cauchy", "ChiSquared", "Erlang", "Exponential",
    "F", "Gamma", "LogNormal", "Normal", "Student's t",
    "Uniform", "Weibull"};


//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  int i;
  int UniformInx;

  cboDist->Items->Clear();
  UniformInx = -1;
  for (i = 0; i < 12; i++) {
    cboDist->Items->Add(DistNames[i]);
    if (DistNames[i].AnsiCompare("Uniform") == 0)
      UniformInx = i;
  }
  cboDist->ItemIndex = UniformInx;
  cboDistChange(this);
  PRNG = new TStRandomSystem(0);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormDestroy(TObject *Sender)
{
   delete PRNG;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::cboDistChange(TObject *Sender)
{
  switch (cboDist->ItemIndex) {
    case 0 : PrepForBeta(); break;
    case 1 : PrepForCauchy(); break;
    case 2 : PrepForChiSquared(); break;
    case 3 : PrepForErlang(); break;
    case 4 : PrepForExponential(); break;
    case 5 : PrepForF(); break;
    case 6 : PrepForGamma(); break;
    case 7 : PrepForLogNormal(); break;
    case 8 : PrepForNormal(); break;
    case 9 : PrepForT(); break;
    case 10: PrepForUniform(); break;
    case 11: PrepForWeibull(); break;
  }
  updRightClick(this, btNext);
  updLeftClick(this, btNext);
  edtParm1->Text = FloatToStr(Value1);
  edtParm2->Text = FloatToStr(Value2);

}
//---------------------------------------------------------------------------
void __fastcall TForm1::updRightClick(TObject *Sender, TUDBtnType Button)
{
  lblRight->Caption = IntToStr(updRight->Position);
  GraphRight = updRight->Position;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::updLeftClick(TObject *Sender, TUDBtnType Button)
{
  lblLeft->Caption = IntToStr(updLeft->Position);
  GraphLeft = updLeft->Position;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::PrepForBeta()
{
  lblParm1->Caption = "Shape 1:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Shape 2:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = 0;
  updRight->Position = 1;
  Value1 = 2.0;
  Value2 = 4.0;
  GetRandom = GetBeta;
}

void __fastcall TForm1::PrepForCauchy()
{
  lblParm1->Caption = "(none)";
  lblParm1->Visible = true;
  lblParm2->Visible = false;
  edtParm1->Visible = false;
  edtParm1->Enabled = false;
  edtParm2->Visible = false;
  edtParm2->Enabled = false;
  updLeft->Position = -5;
  updRight->Position = 5;
  Value1 = 0.0;
  Value2 = 0.0;
  GetRandom = GetCauchy;
}

void __fastcall TForm1::PrepForChiSquared()
{
  lblParm1->Caption = "Degrees of freedom:";
  lblParm1->Visible = true;
  lblParm2->Visible = false;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = false;
  edtParm2->Enabled = false;
  updLeft->Position = 0;
  updRight->Position = 20;
  Value1 = 5.0;
  Value2 = 0.0;
  GetRandom = GetChiSquared;
}

void __fastcall TForm1::PrepForErlang()
{
  lblParm1->Caption = "Mean:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Order:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = 0;
  updRight->Position = 5;
  Value1 = 1.0;
  Value2 = 4.0;
  GetRandom = GetErlang;
}

void __fastcall TForm1::PrepForExponential()
{
  lblParm1->Caption = "Mean:";
  lblParm1->Visible = true;
  lblParm2->Visible = false;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = false;
  edtParm2->Enabled = false;
  updLeft->Position = 0;
  updRight->Position = 10;
  Value1 = 1.0;
  Value2 = 0.0;
  GetRandom = GetExponential;
}

void __fastcall TForm1::PrepForF()
{
  lblParm1->Caption = "Degrees of freedom 1:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Degrees of freedom 2:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = 0;
  updRight->Position = 20;
  Value1 = 10.0;
  Value2 = 5.0;
  GetRandom = GetF;
}

void __fastcall TForm1::PrepForGamma()
{
  lblParm1->Caption = "Shape:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Scale:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = 0;
  updRight->Position = 10;
  Value1 = 2.0;
  Value2 = 1.0;
  GetRandom = GetGamma;
}

void __fastcall TForm1::PrepForLogNormal()
{
  lblParm1->Caption = "Mean:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Standard deviation:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = 0;
  updRight->Position = 10;
  Value1 = 0.0;
  Value2 = 1.0;
  GetRandom = GetLogNormal;
}

void __fastcall TForm1::PrepForNormal()
{
  lblParm1->Caption = "Mean:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Standard deviation:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = -5;
  updRight->Position = 5;
  Value1 = 0.0;
  Value2 = 1.0;
  GetRandom = GetNormal;
}

void __fastcall TForm1::PrepForT()
{
  lblParm1->Caption = "Degrees of freedom:";
  lblParm1->Visible = true;
  lblParm2->Visible = false;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = false;
  edtParm2->Enabled = false;
  updLeft->Position = -10;
  updRight->Position = 10;
  Value1 = 10.0;
  Value2 = 0.0;
  GetRandom = GetT;
}

void __fastcall TForm1::PrepForUniform()
{
  lblParm1->Caption = "(none)";
  lblParm1->Visible = true;
  lblParm2->Visible = false;
  edtParm1->Visible = false;
  edtParm1->Enabled = false;
  edtParm2->Visible = false;
  edtParm2->Enabled = false;
  updLeft->Position = 0;
  updRight->Position = 1;
  Value1 = 0.0;
  Value2 = 0.0;
  GetRandom = GetUniform;
}

void __fastcall TForm1::PrepForWeibull()
{
  lblParm1->Caption = "Shape:";
  lblParm1->Visible = true;
  lblParm2->Caption = "Scale:";
  lblParm2->Visible = true;
  edtParm1->Visible = true;
  edtParm1->Enabled = true;
  edtParm2->Visible = true;
  edtParm2->Enabled = true;
  updLeft->Position = 0;
  updRight->Position = 10;
  Value1 = 2.0;
  Value2 = 3.0;
  GetRandom = GetWeibull;
}

double __fastcall TForm1::GetBeta()
{
  return PRNG->AsBeta(Value1, Value2);
}

double __fastcall TForm1::GetCauchy()
{
  return PRNG->AsCauchy();
}

double __fastcall TForm1::GetChiSquared()
{
  if (Value1 > 65535.0)
    throw(Exception(
      "TForm1.GetChiSquared: the degrees of freedom value 1 is too large for this example program"));

  return PRNG->AsChiSquared(INT(Value1));
}

double __fastcall TForm1::GetErlang()
{
  return PRNG->AsErlang(Value1, Value2);
}

double __fastcall TForm1::GetExponential()
{
  return PRNG->AsExponential(Value1);
}

double __fastcall TForm1::GetF()
{
  if (Value1 > 65535.0)
    throw(Exception(
      "TForm1.GetF: the degrees of freedom value 1 is too large for this example program"));
  if (Value2 > 65535.0)
    throw(Exception(
      "TForm1.GetF: the degrees of freedom value 2 is too large for this example program"));
  return PRNG->AsF(INT(Value1), INT(Value2));
}

double __fastcall TForm1::GetGamma()
{
  return PRNG->AsGamma(Value1, Value2);
}

double __fastcall TForm1::GetLogNormal()
{
  return PRNG->AsLogNormal(Value1, Value2);
}

double __fastcall TForm1::GetNormal()
{
  return PRNG->AsNormal(Value1, Value2);
}

double __fastcall TForm1::GetT()
{
  if (Value1 > 65535.0)
    throw(Exception(
      "TForm1.GetT: the degrees of freedom value is too large for this example program"));
  return PRNG->AsT(INT(Value1));
}

double __fastcall TForm1::GetUniform()
{
  return PRNG->AsFloat();
}

double __fastcall TForm1::GetWeibull()
{
  return PRNG->AsWeibull(Value1, Value2);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnGenerateClick(TObject *Sender)
{
  if (edtParm1->Text.IsEmpty())
    Value1 = 0.0;
  else
    Value1 = StrToFloat(edtParm1->Text);
  if (edtParm2->Text.IsEmpty())
    Value2 = 0.0;
  else
    Value2 = StrToFloat(edtParm2->Text);
  GenerateGraph(cboDist->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GenerateGraph(int aDistInx)
{
  #define RandomCount 1000000

  int Buckets[401];
  int i;
  double R;
  int Inx;
  int MaxHt;
  double MaxLineFactor;
  double GraphWidth;
  int OldPercent;
  int NewPercent;
  double MaxY;

  // zero out the buckets
  ZeroMemory(Buckets, sizeof(Buckets));

  // calculate random numbers according to distribution, convert to a
  // bucket index, and increment that bucket count
  OldPercent = -1;
  GraphWidth = imgGraph->Width;
  for (i = 0; i < RandomCount; i++) {
    NewPercent = (i * 100) / RandomCount;
    if (NewPercent != OldPercent) {
      prgGenProgress->Position = NewPercent;
      OldPercent = NewPercent;
    }
    R = GetRandom();
    if (((double) GraphLeft <= R) && (R <= (double) GraphRight)) {
      Inx = INT((R - GraphLeft) * GraphWidth / (GraphRight - GraphLeft));
      if ((0 <= Inx) && (Inx <= 400))
        Buckets[Inx]++;
    }
  }

  // calculate the largest bucket
  MaxHt = 1;
  for (i = 0; i <= 400; i++) {
    if (MaxHt < Buckets[i])
      MaxHt = Buckets[i];
  }

  // draw the graph
  imgGraph->Canvas->Lock();
  try {
    imgGraph->Canvas->FillRect(Rect(0, 0, imgGraph->Width, imgGraph->Height));
    MaxLineFactor = (double) imgGraph->Height / MaxHt;
    imgGraph->Canvas->Pen->Color = clRed;
    for (i = 0; i <= 400; i++) {
      imgGraph->Canvas->PenPos = Point(i, imgGraph->Height);
      imgGraph->Canvas->LineTo(i, imgGraph->Height - INT(Buckets[i] * MaxLineFactor));
    }
  }
  __finally {
    imgGraph->Canvas->Unlock();
  }

  MaxY = (double) MaxHt / RandomCount;
  lblMaxY->Caption = Format("Max: %8.6f", ARRAYOFCONST((MaxY)));
}
//---------------------------------------------------------------------------

