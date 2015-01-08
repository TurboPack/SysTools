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

unit ExTextU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TSTDlg = class(TForm)
    Memo1: TMemo;
    OD1: TOpenDialog;
    LoadBtn: TButton;
    SeekBtn: TButton;
    FlushBtn: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    CloseFBtn: TButton;
    procedure LoadBtnClick(Sender: TObject);
    procedure SeekBtnClick(Sender: TObject);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Memo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FlushBtnClick(Sender: TObject);
    procedure CloseFBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdatePos;
    procedure UpdateButtons(FOK : Boolean);
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

uses
  StConst,
  StBase,
  StText;


var
  F   : TextFile;
  AFO : Boolean;


procedure TSTDlg.UpdateButtons(FOK : Boolean);
begin
  CloseFBtn.Enabled := FOK;
  SeekBtn.Enabled   := FOK;
  FlushBtn.Enabled  := FOK;
end;

procedure TSTDlg.FormCreate(Sender: TObject);
begin
  UpdateButtons(False);
  AFO := False;
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
var
  S  : string;
  hC : HCursor;
begin
  if (OD1.Execute) then
  begin
    if (AFO) then
      CloseFile(F);
    AFO := False;

    AssignFile(F,OD1.FileName);
    ReSet(F);

    Memo1.Enabled := True;
    Memo1.Perform(WM_SETREDRAW,0,0);
    hC := SetCursor(LoadCursor(0,IDC_WAIT));

    while NOT EOF(F) do
    begin
      Readln(F,S);
      Memo1.Lines.Add(S);
    end;

    Memo1.Perform(WM_SETREDRAW,1,0);
    Memo1.Update;
    Memo1.SelStart := 0;
    Memo1.SelLength := 0;

    Reset(F);

    Edit1.Text := OD1.FileName;
    Edit2.Text := IntToStr(TextFileSize(F));
    Edit3.Text := IntToStr(TextPos(F));

    SetCursor(hC);
    Memo1.SetFocus;
    AFO := True;
  end;
  UpdateButtons(AFO);
end;

procedure TSTDlg.CloseFBtnClick(Sender: TObject);
begin
  CloseFile(F);
  Memo1.Clear;
  AFO := False;
  UpdateButtons(False);
end;


procedure TSTDlg.SeekBtnClick(Sender: TObject);
var
  NP : LongInt;
begin
  NP := StrToInt(Edit4.Text);
  Memo1.SetFocus;
  if (NP < 0) OR (NP >= TextFileSize(F)) then
  begin
    ShowMessage('Value out of range');
    Exit;
  end;

  if TextSeek(F,NP) then
  begin
    NP := TextPos(F);
    Memo1.SelStart := NP;
    Memo1.SelLength := 0;
    Edit3.Text := IntToStr(NP);
  end else
  begin
    ShowMessage('Unable to seek to position');
    Memo1.SetFocus;
  end;
end;

procedure TSTDlg.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdatePos;
end;

procedure TSTDlg.Memo1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdatePos;
end;


procedure TSTDlg.UpdatePos;
var
  CP : LongInt;
begin
  CP := Memo1.SelStart;

  if NOT TextSeek(F,CP) then
  begin
    ShowMessage('Unable to update file pointer');
    Exit;
  end;

  Edit3.Text := IntToStr(TextPos(F));
end;

procedure TSTDlg.FlushBtnClick(Sender: TObject);
begin
  if NOT (TextFlush(F)) then
  begin
    ShowMessage('Unable to flush file');
  end;
  Memo1.SetFocus;
end;



procedure TSTDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if AFO then CloseFile(F);
end;

procedure TSTDlg.FormShow(Sender: TObject);
begin
  LoadBtn.SetFocus;
end;

end.
