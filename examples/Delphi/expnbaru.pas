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

(******************************************************************************

Enter a five-digit zip code and the inches to print from the left and top of
the page or envelope. Don't forget that laser printers can't print all the
way to the edge and so the value you enter must be adjusted accordingly. For
example, if you enter 5.0 for the "From Left" value and the printer has a
1/4" non-printable border, the bar code will be located 5.25" from the
actual edge of the paper.

NOTE: Be sure to change the constant PrinterPixPerInch to match the
      resolution of your printer.

*******************************************************************************)

unit expnbaru;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask,

  StBarPN;

const
  PrinterPixPerInch = 600;

type
  TfrmPostNet = class(TForm)
    btnPrint: TButton;
    meLeft: TMaskEdit;
    Label1: TLabel;
    Label2: TLabel;
    meTop: TMaskEdit;
    BarCode1: TStPNBarCode;
    meZIP: TMaskEdit;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure meZIPChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPostNet: TfrmPostNet;

implementation

{$R *.DFM}

uses
  Printers,
  StStrS;

procedure TfrmPostNet.FormCreate(Sender: TObject);
begin
  meZIP.Text := BarCode1.PostalCode;
  meLeft.Text := ' 5' + DecimalSeparator + '0';
  meTop.Text := ' 3' + DecimalSeparator + '9';
end;

procedure TfrmPostNet.btnPrintClick(Sender: TObject);
var
  P  : TPoint;
  L,
  T  : longint;
begin
  L := Round(StrToFloat(meLeft.Text) * PrinterPixPerInch);
  T := Round(StrToFloat(meTop.Text) * PrinterPixPerInch);
  P := Point(L, T);
  Printer.BeginDoc;
  BarCode1.PaintToPrinterCanvas(Printer.Canvas, P);
  Printer.EndDoc;
end;

procedure TfrmPostNet.meZIPChange(Sender: TObject);
var
  S : ShortString;
begin
  S := TrimS(meZIP.Text);
  if (Length(S) = 5) then
    BarCode1.PostalCode := meZIP.Text;
end;

end.
