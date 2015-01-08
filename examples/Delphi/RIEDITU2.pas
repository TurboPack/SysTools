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

unit RIEditU2;

interface

uses
  Windows, Messages, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, Mask;

type
  EditingStateType = (etAll, etName, etValue);

  TDataDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DataTypeRG: TRadioGroup;
    Label1: TLabel;
    ValueName: TEdit;
    Label2: TLabel;
    IData: TMaskEdit;
    procedure DataTypeRGClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    EditingState   : EditingStateType;
    RGIdx : Integer;
  end;

var
  DataDlg: TDataDlg;

implementation

{$R *.DFM}


procedure TDataDlg.DataTypeRGClick(Sender: TObject);
begin
  case DataTypeRG.ItemIndex of
    0,
    1,
    5 : begin
          IData.MaxLength := 256;
          IData.EditMask := '';
        end;
    2,
    3 : begin
          IData.MaxLength := 10;
          IData.EditMask := '';
        end;
    4 : begin
          IData.MaxLength := 15;
          IData.EditMask := '';
        end;
    6 : begin
          IData.MaxLength := 12;
          IData.EditMask := '';
        end;
  end;
  IData.SetFocus;
end;

procedure TDataDlg.FormActivate(Sender: TObject);
begin
  DataTypeRG.ItemIndex := RGIdx;
  case EditingState of
    etAll   : begin
                DataTypeRG.Enabled := True;
                IData.Enabled := True;
                ValueName.Enabled := True;
                DataTypeRG.ItemIndex := 5;
                ValueName.SetFocus;
              end;

    etName  : begin
                DataTypeRG.Enabled := False;
                ValueName.Enabled := True;
                IData.Enabled := False;
                DataTypeRG.ItemIndex := RGIdx;
                ValueName.SetFocus;
              end;

    etValue : begin
                DataTypeRG.Enabled := False;
                ValueName.Enabled := False;
                IData.Enabled := True;
                DataTypeRG.ItemIndex := RGIdx;
                IData.SetFocus;
              end;
  end;
end;

end.
