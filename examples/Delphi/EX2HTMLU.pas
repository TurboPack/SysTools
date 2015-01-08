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

unit ex2htmlu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons,

  StBase, StToHTML;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StFileToHTML1: TStFileToHTML;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure GetFileClick(Sender: TObject);
    procedure StFileToHTML1Progress(Sender: TObject; Percent: Word);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if (not FileExists(Edit1.Text)) then begin
    ShowMessage('Input file does not exist');
    Exit;
  end;

  StFileToHTML1.InFileName := Edit1.Text;
  StFileToHTML1.OutFileName := Edit2.Text;

  StFileToHTML1.Execute;
  Label1.Caption := 'Waiting';
  ShowMessage('Done');
end;


procedure TForm1.GetFileClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then begin
    if (Sender = SpeedButton1) then
      Edit1.Text := OpenDialog1.FileName
    else if (Sender = SpeedButton2) then
      Edit2.Text := OpenDialog1.FileName;
  end;
end;

procedure TForm1.StFileToHTML1Progress(Sender: TObject; Percent: Word);
begin
  Label1.Caption := 'Completed: ' + IntToStr(Percent) + '%';
end;


end.
