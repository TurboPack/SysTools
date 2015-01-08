(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit ExBarCU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Printers, ExtCtrls, Tabnotbk, ComCtrls, Grids,
  Db, DBTables, DBCtrls, DBGrids,

  StBase, StBarC, StDbBarC, St2DBarC;

type
  TBarCodeForm = class(TForm)
    btnPrint: TButton;
    PrintDialog1: TPrintDialog;
    NB: TTabbedNotebook;
    edCode1: TEdit;
    edSupp: TEdit;
    Label1: TLabel;
    BarCode1: TStBarCode;
    btnCopy: TButton;
    BarCode2: TStBarCode;
    BarCode5: TStBarCode;
    rgType: TRadioGroup;
    cbSupp: TCheckBox;
    btnClose: TButton;
    cbTallGuardBars: TCheckBox;
    cbShowCode: TCheckBox;
    btnUpdate1: TButton;
    cbBearerBars: TCheckBox;
    cbShowCode2: TCheckBox;
    Label2: TLabel;
    edCode2: TEdit;
    btnUpdate2: TButton;
    BarCode7: TStBarCode;
    DataSource1: TDataSource;
    Table1: TTable;
    DBGrid1: TDBGrid;
    btnSave: TButton;
    BarCode6: TStBarCode;
    StDbBarCode1: TStDbBarCode;
    Label3: TLabel;
    edCode3: TEdit;
    Button2: TButton;
    cbShowCode3: TCheckBox;
    BarCode3: TStBarCode;
    Label4: TLabel;
    cbShowGuardChars3: TCheckBox;
    cbShowCode4: TCheckBox;
    BarCode4: TStBarCode;
    Button1: TButton;
    edCode4: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    edCode5: TEdit;
    btnUpdate5: TButton;
    cbShowCode5: TCheckBox;
    Label7: TLabel;
    edCode6: TEdit;
    btnUpdate6: TButton;
    cbShowCode6: TCheckBox;
    Label8: TLabel;
    edCode7: TEdit;
    btnUpdate7: TButton;
    cbShowCode7: TCheckBox;
    Label9: TLabel;
    edCodePDF417: TEdit;
    btnUpdatePDF417: TButton;
    cbShowCodePDF417: TCheckBox;
    Label10: TLabel;
    edCodeMaxiCode: TEdit;
    btnUpdateMaxiCode: TButton;
    cbShowCodeMaxiCode: TCheckBox;
    radMCMode23: TRadioButton;
    radMCMode4: TRadioButton;
    radMCMode5: TRadioButton;
    Label11: TLabel;
    edMCCountryCode: TEdit;
    edMCPostalCode: TEdit;
    edMCServiceClass: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    cbPDF417Truncated: TCheckBox;
    StPDF417Barcode1: TStPDF417Barcode;
    StMaxiCodeBarcode1: TStMaxiCodeBarcode;
    btnValidate: TButton;
    procedure btnPrintClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure btnUpdate1Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnUpdate2Click(Sender: TObject);
    procedure btnUpdate3Click(Sender: TObject);
    procedure btnUpdate4Click(Sender: TObject);
    procedure btnUpdate5Click(Sender: TObject);
    procedure btnUpdate6Click(Sender: TObject);
    procedure btnUpdate7Click(Sender: TObject);
    procedure btnUpdatePDF417Click(Sender: TObject);
    procedure btnUpdateMaxiCodeClick(Sender: TObject);
    procedure radMCMode4Click(Sender: TObject);
    procedure radMCMode23Click(Sender: TObject);
    procedure radMCMode5Click(Sender: TObject);
    procedure NBChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BarCodeForm: TBarCodeForm;

implementation

{$R *.DFM}

procedure TBarCodeForm.btnPrintClick(Sender: TObject);
begin
  if not PrintDialog1.Execute then
    Exit;
  Application.ProcessMessages;

  Screen.Cursor := crHourGlass;
  try
    Printer.BeginDoc;
    try
      Printer.Title := 'StBarCode';

      BarCode1.BarcodeType := bcUPC_A;
      BarCode1.Code := '52100014015';
      BarCode1.SupplementalCode := '';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 0.5, 0.5, 0.5);
      BarCode1.SupplementalCode := '12';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 3.0, 0.5, 0.5);
      BarCode1.SupplementalCode := '12345';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 6.0, 0.5, 0.5);

      BarCode1.BarcodeType :=bcUPC_E;
      BarCode1.Code := '173559';
      BarCode1.SupplementalCode := '';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 0.5, 1.5, 0.5);
      BarCode1.SupplementalCode := '12';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 3.0, 1.5, 0.5);
      BarCode1.SupplementalCode := '12345';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 6.0, 1.5, 0.5);

      BarCode1.BarcodeType := bcEAN_13;
      BarCode1.Code := '737622135746';
      BarCode1.SupplementalCode := '';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 0.5, 2.5, 0.5);
      BarCode1.SupplementalCode := '12';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 3.0, 2.5, 0.5);
      BarCode1.SupplementalCode := '12345';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 6.0, 2.5, 0.5);

      BarCode1.BarcodeType := bcEAN_8;
      BarCode1.Code := '1234567';
      BarCode1.SupplementalCode := '';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 0.5, 3.5, 0.5);
      BarCode1.SupplementalCode := '12';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 3.0, 3.5, 0.5);
      BarCode1.SupplementalCode := '12345';
      BarCode1.PaintToCanvasSize(Printer.Canvas, 6.0, 3.5, 0.5);

      BarCode2.BarcodeType := bcInterleaved2of5;
      BarCode2.Code := '0123456789';
      BarCode2.PaintToCanvasSize(Printer.Canvas, 0.5, 4.5, 0.5);

      BarCode3.BarCodeType := bcCodabar;
      BarCode3.Code := 'c1234567890d';
      BarCode3.PaintToCanvasSize(Printer.Canvas, 3.0, 4.5, 0.5);

      BarCode4.BarCodeType := bcCode11;
      BarCode4.Code := '0123456-12';
      BarCode4.PaintToCanvasSize(Printer.Canvas, 5.0, 4.5, 0.5);

      BarCode5.BarCodeType := bcCode39;
      BarCode5.Code := '1234567890ABCDEFG';
      BarCode5.PaintToCanvasSize(Printer.Canvas, 0.5, 5.5, 0.5);
      BarCode5.Code := '4-976 SUGARLOAF HWY';
      BarCode5.PaintToCanvasSize(Printer.Canvas, 4.5, 5.5, 0.5);

      BarCode6.BarCodeType := bcCode93;
      BarCode6.Code := 'CODE 93';
      BarCode6.PaintToCanvasSize(Printer.Canvas, 0.5, 6.5, 0.5);

      BarCode7.BarCodeType := bcCode128;
      BarCode7.Code128Subset := csCodeA;
      BarCode7.Code := 'CODE 128';
      BarCode7.Validate(True);
      BarCode7.PaintToCanvasSize(Printer.Canvas, 3.0, 6.5, 0.5);

      StMaxiCodeBarcode1.Mode := cmMode5;
      StMaxiCodeBarcode1.Code := 'MaxiCode';
      StMaxiCodeBarcode1.Caption := 'MaxiCode';
      StMaxiCodeBarcode1.PaintToPrinterCanvasSize (Printer.Canvas, 0.5, 7.5, 1);

      StPDF417Barcode1.Code := 'PDF417';
      StPDF417Barcode1.Caption := 'PDF417';
      StPDF417Barcode1.PaintToPrinterCanvasSize (Printer.Canvas, 3.0, 7.5, 1);
    finally
      if not Printer.Aborted then
        Printer.EndDoc;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TBarCodeForm.btnCopyClick(Sender: TObject);
begin
  case NB.PageIndex of
    0 : BarCode1.CopyToClipboard;
    1 : BarCode2.CopyToClipboard;
    2 : BarCode3.CopyToClipboard;
    3 : BarCode4.CopyToClipboard;
    4 : BarCode5.CopyToClipboard;
    5 : BarCode6.CopyToClipboard;
    6 : BarCode7.CopyToClipboard;
    7 : StPDF417Barcode1.CopyToClipboard;
    8 : StMaxiCodeBarcode1.CopyToClipboard;
  end;
end;

procedure TBarCodeForm.btnValidateClick(Sender: TObject);
begin
  case NB.PageIndex of
    0 : BarCode1.Validate(True);
    1 : BarCode2.Validate(True);
    2 : BarCode3.Validate(True);
    3 : BarCode4.Validate(True);
    4 : BarCode5.Validate(True);
    5 : BarCode6.Validate(True);
    6 : BarCode7.Validate(True);
  end;
end;

procedure TBarCodeForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TBarCodeForm.btnSaveClick(Sender: TObject);
begin
  case NB.PageIndex of
    0 : BarCode1.SaveToFile('UPCEAN.bmp');
    1 : BarCode2.SaveToFile('I2of5.bmp');
    2 : BarCode3.SaveToFile('Codabar.bmp');
    3 : BarCode4.SaveToFile('Code11.bmp');
    4 : BarCode5.SaveToFile('Code39.bmp');
    5 : BarCode6.SaveToFile('Code93.bmp');
    6 : BarCode7.SaveToFile('Code128.bmp');
    7 : StPDF417Barcode1.SaveToFile ('PDF417.bmp');
    8 : StMaxiCodeBarcode1.SaveToFile ('MaxiCode.bmp');
  end;
end;

procedure TBarCodeForm.btnUpdate1Click(Sender: TObject);
begin
  case rgType.ItemIndex of
    0 : begin
          BarCode1.BarcodeType := bcUPC_A;
          edCode1.MaxLength := 12;
          edCode1.Text := Copy(edCode1.Text, 1, 12);
          Label1.Caption := 'Code: 11 or 12 Numeric ';
        end;
    1 : begin
          BarCode1.BarcodeType := bcUPC_E;
          edCode1.MaxLength := 6;
          Label1.Caption := 'Code: 6 Numeric ';
          edCode1.Text := Copy(edCode1.Text, 1, 6);
        end;
    2 : begin
          BarCode1.BarcodeType := bcEAN_13;
          edCode1.MaxLength := 13;
          Label1.Caption := 'Code: 12 or 13 Numeric ';
          edCode1.Text := Copy(edCode1.Text, 1, 13);
        end;
    3 : begin
          BarCode1.BarcodeType := bcEAN_8;
          edCode1.MaxLength := 8;
          Label1.Caption := 'Code: 7 or 8 Numeric ';
          edCode1.Text := Copy(edCode1.Text, 1, 8);
        end;
  end;
  BarCode1.ShowCode := cbShowCode.Checked;
  BarCode1.TallGuardBars := cbTallGuardBars.Checked;
  BarCode1.Code := edCode1.Text;
  if cbSupp.Checked then begin
    BarCode1.SupplementalCode := edSupp.Text;
    edSupp.Enabled := True;
  end else begin
    BarCode1.SupplementalCode := '';
    edSupp.Enabled := False;
  end;
end;

procedure TBarCodeForm.btnUpdate2Click(Sender: TObject);
begin
  BarCode2.Code := edCode2.Text;
  BarCode2.ShowCode := cbShowCode2.Checked;
  BarCode2.BearerBars := cbBearerBars.Checked;
end;

procedure TBarCodeForm.btnUpdate3Click(Sender: TObject);
begin
  BarCode3.Code := edCode3.Text;
  BarCode3.ShowCode := cbShowCode3.Checked;
  BarCode3.ShowGuardChars := cbShowGuardChars3.Checked;
end;

procedure TBarCodeForm.btnUpdate4Click(Sender: TObject);
begin
  BarCode4.Code := edCode4.Text;
  BarCode4.ShowCode := cbShowCode4.Checked;
end;

procedure TBarCodeForm.btnUpdate5Click(Sender: TObject);
begin
  BarCode5.Code := edCode5.Text;
  BarCode5.ShowCode := cbShowCode5.Checked;
end;

procedure TBarCodeForm.btnUpdate6Click(Sender: TObject);
begin
  BarCode6.Code := edCode6.Text;
  BarCode6.ShowCode := cbShowCode6.Checked;
end;

procedure TBarCodeForm.btnUpdate7Click(Sender: TObject);
begin
  BarCode7.Code := edCode7.Text;
  BarCode7.ShowCode := cbShowCode7.Checked;
end;

procedure TBarCodeForm.btnUpdatePDF417Click(Sender: TObject);
begin
  StPDF417Barcode1.Truncated := cbPDF417Truncated.Checked;
  StPDF417BarCode1.Code := edCodePDF417.Text;
  if cbShowCodePDF417.Checked then
    StPDF417BarCode1.Caption := edCodePDF417.Text
  else
    StPDF417BarCode1.Caption := '';
end;

procedure TBarCodeForm.btnUpdateMaxiCodeClick(Sender: TObject);
begin
  StMaxiCodeBarCode1.Code := edCodeMaxiCode.Text;
  StMaxiCodeBarCode1.CarrierCountryCode := StrToInt (edMCCountryCode.Text);
  StMaxiCodeBarCode1.CarrierPostalCode := edMCPostalCode.Text;
  StMaxiCodeBarCode1.CarrierServiceClass := StrToInt (edMCServiceClass.Text);
  if cbShowCodeMaxiCode.Checked then
    StMaxiCodeBarCode1.Caption := edCodeMaxiCode.Text
  else
    StMaxiCodeBarCode1.Caption := '';
end;

procedure TBarCodeForm.radMCMode4Click(Sender: TObject);
begin
  edMCServiceClass.Enabled := False;
  edMCPostalCode.Enabled := False;
  edMCCountryCode.Enabled := False;
  StMaxiCodeBarCode1.Mode := cmMode4;
  btnUpdateMaxiCodeClick (Sender);
end;

procedure TBarCodeForm.radMCMode23Click(Sender: TObject);
begin
  edMCServiceClass.Enabled := True;
  edMCPostalCode.Enabled := True;
  edMCCountryCode.Enabled := True;
  StMaxiCodeBarCode1.Mode := cmMode2;
  btnUpdateMaxiCodeClick (Sender);
end;

procedure TBarCodeForm.radMCMode5Click(Sender: TObject);
begin
  edMCServiceClass.Enabled := False;
  edMCPostalCode.Enabled := False;
  edMCCountryCode.Enabled := False;
  StMaxiCodeBarCode1.Mode := cmMode5;
  btnUpdateMaxiCodeClick (Sender);
end;

procedure TBarCodeForm.NBChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  if (NewTab = 7) or (NewTab = 8) then
    btnValidate.Enabled := False
  else
    btnValidate.Enabled := True;
end;

end.



