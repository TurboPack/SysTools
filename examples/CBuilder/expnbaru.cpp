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
#include <vcl\printers.hpp>
#include <math.h>
#include <ststrs.hpp>
#pragma hdrstop

#include "expnbaru.h"
//---------------------------------------------------------------------------
#pragma link "stbarpn"
#pragma link "StBarPN"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  meZIP->Text = BarCode1->PostalCode;
  meLeft->Text = " 5.0";
  meTop->Text = " 3.9";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnPrintClick(TObject *Sender)
{

  TPoint P;
  long   L, T;
  long   PrinterPixPerInch = 600;

  L = floor(StrToFloat(meLeft->Text) * PrinterPixPerInch);
  T = floor(StrToFloat(meTop->Text) * PrinterPixPerInch);
  P = Point(L, T);
  
  Printer()->BeginDoc();
  BarCode1->PaintToPrinterCanvas(Printer()->Canvas, P);
  Printer()->EndDoc();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::meZIPChange(TObject *Sender)
{

  AnsiString S = Trim(meZIP->Text);
  if (S.Length() == 5)
    BarCode1->PostalCode = meZIP->Text;
}
//---------------------------------------------------------------------------