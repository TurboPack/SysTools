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

#include "ExBarCU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StDbBarC"
#pragma link "StBarC"
#pragma link "Grids"
#pragma link "Printers"
#pragma link "StBarC"
#pragma link "Controls"

#pragma link "St2DBarC"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnPrintClick(TObject *Sender)
{
  if (!PrintDialog1->Execute())
    return;
  Application->ProcessMessages();

  Screen->Cursor = crHourGlass;
  Printer()->BeginDoc();
  Printer()->Title = "StBarCode";

  BarCode1->BarCodeType = bcUPC_A;
  BarCode1->Code = "52100014015";
  BarCode1->SupplementalCode = "";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 0.5, 0.5, 0.5);
  BarCode1->SupplementalCode = "12";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 3.0, 0.5, 0.5);
  BarCode1->SupplementalCode = "12345";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 6.0, 0.5, 0.5);

  BarCode1->BarCodeType =bcUPC_E;
  BarCode1->Code = "173559";
  BarCode1->SupplementalCode = "";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 0.5, 1.5, 0.5);
  BarCode1->SupplementalCode = "12";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 3.0, 1.5, 0.5);
  BarCode1->SupplementalCode = "12345";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 6.0, 1.5, 0.5);

  BarCode1->BarCodeType = bcEAN_13;
  BarCode1->Code = "737622135746";
  BarCode1->SupplementalCode = "";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 0.5, 2.5, 0.5);
  BarCode1->SupplementalCode = "12";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 3.0, 2.5, 0.5);
  BarCode1->SupplementalCode = "12345";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 6.0, 2.5, 0.5);

  BarCode1->BarCodeType = bcEAN_8;
  BarCode1->Code = "1234567";
  BarCode1->SupplementalCode = "";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 0.5, 3.5, 0.5);
  BarCode1->SupplementalCode = "12";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 3.0, 3.5, 0.5);
  BarCode1->SupplementalCode = "12345";
  BarCode1->PaintToCanvasSize(Printer()->Canvas, 6.0, 3.5, 0.5);

  BarCode2->BarCodeType = bcInterleaved2of5;
  BarCode2->Code = "0123456789";
  BarCode2->PaintToCanvasSize(Printer()->Canvas, 0.5, 4.5, 0.5);

  BarCode3->BarCodeType = bcCodabar;
  BarCode3->Code = "c1234567890d";
  BarCode3->PaintToCanvasSize(Printer()->Canvas, 3.0, 4.5, 0.5);

  BarCode4->BarCodeType = bcCode11;
  BarCode4->Code = "0123456-12";
  BarCode4->PaintToCanvasSize(Printer()->Canvas, 5.0, 4.5, 0.5);

  BarCode5->BarCodeType = bcCode39;
  BarCode5->Code = "1234567890ABCDEFG";
  BarCode5->PaintToCanvasSize(Printer()->Canvas, 0.5, 5.5, 0.5);
  BarCode5->Code = "4-976 SUGARLOAF HWY";
  BarCode5->PaintToCanvasSize(Printer()->Canvas, 4.5, 5.5, 0.5);

  BarCode6->BarCodeType = bcCode93;
  BarCode6->Code = "CODE 93";
  BarCode6->PaintToCanvasSize(Printer()->Canvas, 0.5, 6.5, 0.5);

  BarCode7->BarCodeType = bcCode128;
  BarCode7->Code128Subset = csCodeA;
  BarCode7->Code = "CODE 128";
  BarCode7->Validate(true);
  BarCode7->PaintToCanvasSize(Printer()->Canvas, 3.0, 6.5, 0.5);

  StMaxiCodeBarcode1->Mode = cmMode5;
  StMaxiCodeBarcode1->Code = "MaxiCode";
  StMaxiCodeBarcode1->Caption = "MaxiCode";
  StMaxiCodeBarcode1->PaintToPrinterCanvasSize (Printer()->Canvas, 0.5, 7.5, 1);

  StPDF417Barcode1->Code = "PDF417";
  StPDF417Barcode1->Caption = "PDF417";
  StPDF417Barcode1->PaintToPrinterCanvasSize (Printer()->Canvas, 3.0, 7.5, 1);
      
  if (!Printer()->Aborted)
    Printer()->EndDoc();
  Screen->Cursor = crDefault;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnValidateClick(TObject *Sender)
{
  switch (NB->PageIndex) {
    case 0 : {BarCode1->Validate(true); break;};
    case 1 : {BarCode2->Validate(true); break;};
    case 2 : {BarCode3->Validate(true); break;};
    case 3 : {BarCode4->Validate(true); break;};
    case 4 : {BarCode5->Validate(true); break;};
    case 5 : {BarCode6->Validate(true); break;};
    case 6 : {BarCode7->Validate(true); break;};
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnSaveClick(TObject *Sender)
{
  switch (NB->PageIndex) {
    case 0 : {BarCode1->SaveToFile("UPCEAN.bmp"); break;};
    case 1 : {BarCode2->SaveToFile("I2of5.bmp"); break;};
    case 2 : {BarCode3->SaveToFile("Codabar.bmp"); break;};
    case 3 : {BarCode4->SaveToFile("Code11.bmp"); break;};
    case 4 : {BarCode5->SaveToFile("Code39.bmp"); break;};
    case 5 : {BarCode6->SaveToFile("Code93.bmp"); break;};
    case 6 : {BarCode7->SaveToFile("Code128.bmp"); break;};
    case 7 : {StPDF417Barcode1->SaveToFile ("PDF417.bmp"); break;}
    case 8 : {StMaxiCodeBarcode1->SaveToFile ("MaxiCode.bmp"); break;}
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnCopyClick(TObject *Sender)
{
  switch (NB->PageIndex) {
    case 0 : {BarCode1->CopyToClipboard(); break;};
    case 1 : {BarCode2->CopyToClipboard(); break;};
    case 2 : {BarCode3->CopyToClipboard(); break;};
    case 3 : {BarCode4->CopyToClipboard(); break;};
    case 4 : {BarCode5->CopyToClipboard(); break;};
    case 5 : {BarCode6->CopyToClipboard(); break;};
    case 6 : {BarCode7->CopyToClipboard(); break;};
    case 7 : {StPDF417Barcode1->CopyToClipboard(); break;};
    case 8 : {StMaxiCodeBarcode1->CopyToClipboard(); break;};
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnCloseClick(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate1Click(TObject *Sender)
{
  switch (rgType->ItemIndex) {
    case 0 : {
          BarCode1->BarCodeType = bcUPC_A;
          edCode1->MaxLength = 12;
          if (edCode1->Text.Length() > 12)
            edCode1->Text.SetLength(12);
          Label1->Caption = "Code: 11 or 12 Numeric ";
          break;
        };
    case 1 : {
          BarCode1->BarCodeType = bcUPC_E;
          edCode1->MaxLength = 6;
          Label1->Caption = "Code: 6 Numeric ";
          if (edCode1->Text.Length() > 6)
            edCode1->Text.SetLength(6);
          break;
        };
    case 2 : {
          BarCode1->BarCodeType = bcEAN_13;
          edCode1->MaxLength = 13;
          Label1->Caption = "Code: 12 or 13 Numeric ";
          if (edCode1->Text.Length() > 13)
            edCode1->Text.SetLength(13);
          break;
        };
    case 3 : {
          BarCode1->BarCodeType = bcEAN_8;
          edCode1->MaxLength = 8;
          Label1->Caption = "Code: 7 or 8 Numeric ";
          if (edCode1->Text.Length() > 8)
            edCode1->Text.SetLength(8);
          break;
        };
  };

  BarCode1->ShowCode = cbShowCode->Checked;
  BarCode1->TallGuardBars = cbTallGuardBars->Checked;
  BarCode1->Code = edCode1->Text;
  if (cbSupp->Checked) {
    BarCode1->SupplementalCode = edSupp->Text;
    edSupp->Enabled = true;
  } else {
    BarCode1->SupplementalCode = "";
    edSupp->Enabled = false;
  };
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate2Click(TObject *Sender)
{
  BarCode2->Code = edCode2->Text;
  BarCode2->ShowCode = cbShowCode2->Checked;
  BarCode2->BearerBars = cbBearerBars->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate3Click(TObject *Sender)
{
  BarCode3->Code = edCode3->Text;
  BarCode3->ShowCode = cbShowCode3->Checked;
  BarCode3->ShowGuardChars = cbShowGuardChars3->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate4Click(TObject *Sender)
{
  BarCode4->Code = edCode4->Text;
  BarCode4->ShowCode = cbShowCode4->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate5Click(TObject *Sender)
{
  BarCode5->Code = edCode5->Text;
  BarCode5->ShowCode = cbShowCode5->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate6Click(TObject *Sender)
{
  BarCode6->Code = edCode6->Text;
  BarCode6->ShowCode = cbShowCode6->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdate7Click(TObject *Sender)
{
  BarCode7->Code = edCode7->Text;
  BarCode7->ShowCode = cbShowCode7->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::edCodePDF417Exit(TObject *Sender)
{
  StPDF417Barcode1->Truncated = cbPDF417Truncated->Checked;
  StPDF417Barcode1->Code = edCodePDF417->Text;
  if (cbShowCodePDF417->Checked)
    StPDF417Barcode1->Caption = edCodePDF417->Text;
  else
    StPDF417Barcode1->Caption = "";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::btnUpdateMaxiCodeClick(TObject *Sender)
{
  StMaxiCodeBarcode1->Code = edCodeMaxiCode->Text;
  StMaxiCodeBarcode1->CarrierCountryCode = StrToInt (edMCCountryCode->Text);
  StMaxiCodeBarcode1->CarrierPostalCode = edMCPostalCode->Text;
  StMaxiCodeBarcode1->CarrierServiceClass = StrToInt (edMCServiceClass->Text);
  if (cbShowCodeMaxiCode->Checked)
    StMaxiCodeBarcode1->Caption = edCodeMaxiCode->Text;
  else
    StMaxiCodeBarcode1->Caption = "";
}
//---------------------------------------------------------------------------
void __fastcall TForm1::radMCMode23Click(TObject *Sender)
{
  edMCServiceClass->Enabled = true;
  edMCPostalCode->Enabled = true;
  edMCCountryCode->Enabled = true;
  StMaxiCodeBarcode1->Mode = cmMode2;
  btnUpdateMaxiCodeClick (Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::radMCMode4Click(TObject *Sender)
{
  edMCServiceClass->Enabled = false;
  edMCPostalCode->Enabled = false;
  edMCCountryCode->Enabled = false;
  StMaxiCodeBarcode1->Mode = cmMode4;
  btnUpdateMaxiCodeClick (Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::radMCMode5Click(TObject *Sender)
{
  edMCServiceClass->Enabled = false;
  edMCPostalCode->Enabled = false;
  edMCCountryCode->Enabled = false;
  StMaxiCodeBarcode1->Mode = cmMode5;
  btnUpdateMaxiCodeClick (Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::NBChange(TObject *Sender, int NewTab,
      bool &AllowChange)
{
  if (NewTab == 7 || NewTab == 8)
    btnValidate->Enabled = false;
  else
    btnValidate->Enabled = true;
}
//---------------------------------------------------------------------------
