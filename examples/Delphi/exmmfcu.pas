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

unit exmmfcu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  StStrms;

type
  TfrmMain = class(TForm)
    Edit1: TEdit;
    btnOpenMMF: TButton;
    btnCloseMMF: TButton;
    Timer1: TTimer;
    Label1: TLabel;
    procedure btnOpenMMFClick(Sender: TObject);
    procedure btnCloseMMFClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }

    AMMFile  : TStMemoryMappedFile;
    Buf      : array[0..100] of char;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}


procedure TfrmMain.btnOpenMMFClick(Sender: TObject);
begin
  AMMFile := TStMemoryMappedFile.Create('', 100, False, True);
  if (Assigned(AMMFile)) then begin
    btnOpenMMF.Enabled := False;
    btnCloseMMF.Enabled := True;
    Timer1.Enabled := True;
    Label1.Caption := 'Waiting: ';
  end else
    ShowMessage('Unable to open file');
end;

procedure TfrmMain.btnCloseMMFClick(Sender: TObject);
begin
  AMMFile.Free;
  AMMFile := nil;
  btnOpenMMF.Enabled := True;
  btnCloseMMF.Enabled := False;
  Timer1.Enabled := False;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := 'Seeking: ' + IntToStr(AMMFile.DataSize);
  Timer1.Enabled := False;
  AMMFile.Seek(0, soFromBeginning);
  AMMFile.Read(Buf, AMMFile.DataSize);
  Edit1.Text := StrPas(Buf);
  Timer1.Enabled := True;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AMMFile.Free;
  AMMFile := nil;
end;

end.
