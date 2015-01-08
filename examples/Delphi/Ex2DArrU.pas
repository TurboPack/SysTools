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

unit Ex2DArrU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  StConst, StUtils, StBase, StLArr;

type
  TSTDlg = class(TForm)
    ArrayLB: TListBox;
    CreateBtn: TButton;
    Label5: TLabel;
    LMValue: TEdit;
    Label6: TLabel;
    LMRow: TEdit;
    LMCol: TEdit;
    ClearBtn: TButton;
    FillBtn: TButton;
    PutBtn: TButton;
    PutRowBtn: TButton;
    GetBtn: TButton;
    GetRowBtn: TButton;
    SortBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure FillBtnClick(Sender: TObject);
    procedure PutBtnClick(Sender: TObject);
    procedure GetBtnClick(Sender: TObject);
    procedure PutRowBtnClick(Sender: TObject);
    procedure GetRowBtnClick(Sender: TObject);
    procedure SortBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ArrayLBDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetBusy(B : Boolean);
    procedure FillListBox;
    procedure UpdateButtons(AOK : Boolean);
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

type
  S10 = string[10];

const
  MaxRows = 1000;
  MaxCols = 10;

var
  MyLMatrix : TStLMatrix;
  LIArray : array[1..MaxCols] of LongInt;

function MyArraySort(const E1, E2) : Integer; far;
begin
  Result := LongInt(E1) - LongInt(E2);
end;

procedure TSTDlg.UpdateButtons(AOK : Boolean);
begin
  ClearBtn.Enabled  := AOK;
  FillBtn.Enabled   := AOK;
  PutBtn.Enabled    := AOK;
  PutRowBtn.Enabled := AOK;
  GetBtn.Enabled    := AOK;
  GetRowBtn.Enabled := AOK;
  SortBtn.Enabled   := AOK;
  SaveBtn.Enabled   := AOK;
end;

procedure TSTDlg.SetBusy(B : Boolean);
begin
  if B then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;

procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClass(TStLMatrix);
  UpdateButtons(False);
end;

procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyLMatrix.Free;
end;

procedure TSTDlg.FillListBox;
var
  row,
  col,
  Value : LongInt;
begin
  SetBusy(True);
  ArrayLB.Clear;
  ArrayLB.Perform(WM_SETREDRAW,0,0);

  for row := 0 to MyLMatrix.Rows-1 do
  begin
    for col := 0 to MyLMatrix.Cols-1 do
    begin
      MyLMatrix.Get(row,col,Value);
      ArrayLB.Items.Add(IntToStr(row) + ',' +
                        IntToStr(col) + ' = ' + IntToStr(Value));
    end;
  end;
  ArrayLB.Perform(WM_SETREDRAW,1,0);
  ArrayLB.Update;
  SetBusy(False);
end;


procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  row,
  col,
  Value  : LongInt;
begin
  ArrayLB.Clear;

  if Assigned(MyLMatrix) then
    MyLMatrix.Free;

  UpdateButtons(False);
  MyLMatrix := TStLMatrix.Create(MaxRows,MaxCols,sizeof(LongInt));
  MyLMatrix.ElementsStorable := True;

  SetBusy(True);
  for row := 0 to MaxRows-1 do
  begin
    for col := 0 to MaxCols-1 do
    begin
      Value := Trunc(Random(10000));
      MyLMatrix.Put(row,col,Value);
    end;
  end;
  SetBusy(False);

  FillListBox;
  UpdateButtons(True);

  LMRow.Text := '0';
  LMCol.Text := '0';
  MyLMatrix.Get(0,0,Value);
  LMValue.Text := IntToStr(Value);
end;

procedure TSTDlg.ClearBtnClick(Sender: TObject);
var
  Value : LongInt;
begin
  MyLMatrix.Clear;
  ArrayLB.Clear;

  LMRow.Text := '0';
  LMCol.Text := '0';
  MyLMatrix.Get(0,0,Value);
  LMValue.Text := IntToStr(Value);
end;

procedure TSTDlg.FillBtnClick(Sender: TObject);
var
  row,
  col,
  Value : LongInt;
begin
  if (LMValue.Text = '') then
  begin
    ShowMessage('No value entered');
    Exit;
  end;

  Value := StrToInt(LMValue.Text);
  MyLMatrix.Fill(Value);

  FillListBox;

  row := 0;
  col := 0;
  LMRow.Text := IntToStr(row);
  LMCol.Text := IntToStr(col);

  MyLMatrix.Get(row, col, Value);
  LMValue.Text := IntToStr(Value);

  SetBusy(False);
end;

procedure TSTDlg.PutBtnClick(Sender: TObject);
var
  LBE,
  row,
  col,
  Value : LongInt;
begin
  if (LMValue.Text = '') then
  begin
    ShowMessage('No value entered');
    Exit;
  end;

  if (LMRow.Text = '') then
    LMRow.Text := '0';
  if (LMCol.Text = '') then
    LMCol.Text := '0';

  Value := StrToInt(LMValue.Text);
  row := StrToInt(LMRow.Text);
  col := StrToInt(LMCol.Text);
  MyLMatrix.Put(row,col,Value);

  LBE := (row * MaxRows) + col;
  ArrayLB.Items[LBE] := IntToStr(row) + ',' +
                        IntToStr(col) + ' = ' + IntToStr(Value);

  row := StrToInt(LMRow.Text);
  col := StrToInt(LMCol.Text);
  MyLMatrix.Get(row, col, Value);
  LMValue.Text := IntToStr(Value);
end;

procedure TSTDlg.GetBtnClick(Sender: TObject);
var
  LBE,
  row,
  col,
  Value : LongInt;
begin
  if (LMValue.Text = '') then begin
    ShowMessage('No value entered');
    Exit;
  end;

  if (LMRow.Text = '') then
    LMRow.Text := '0';
  if (LMCol.Text = '') then
    LMCol.Text := '0';

  Value := StrToInt(LMValue.Text);
  row := StrToInt(LMRow.Text);
  col := StrToInt(LMCol.Text);
  MyLMatrix.Get(row,col,Value);

  LMRow.Text := IntToStr(row);
  LMCol.Text := IntToStr(col);
  LMValue.Text := IntToStr(Value);

  LBE := (row * MaxCols) + col;
  ArrayLB.ItemIndex := LBE;
end;

procedure TSTDlg.PutRowBtnClick(Sender: TObject);
var
  row,
  col,
  Value : LongInt;

begin
  if (LMValue.Text = '') then
  begin
    ShowMessage('No value entered');
    Exit;
  end;

  if (LMRow.Text = '') then
    LMRow.Text := '0';

  Value := StrToInt(LMValue.Text);
  row := StrToInt(LMRow.Text);

  FillStruct(LIArray,MaxCols,Value,SizeOf(Value));

  MyLMatrix.PutRow(row,LIArray);
  FillListBox;

  row := StrToInt(LMRow.Text);
  col := 0;
  MyLMatrix.Get(row, col, Value);

  LMValue.Text := IntToStr(Value);
  LMCol.Text := '0';
end;

procedure TSTDlg.GetRowBtnClick(Sender: TObject);
var
  step,
  LIV   : LongInt;

begin
  if (LMRow.Text = '') then
    LMRow.Text := '0';

  LIV := 0;
  FillStruct(LIArray,MaxCols,LIV,SizeOf(LIV));
  MyLMatrix.GetRow(0,LIArray);

  ArrayLB.Clear;
  ArrayLB.Perform(WM_SETREDRAW,0,0);

  for step := 1 to MaxCols do
    ArrayLB.Items.Add('Col' + IntToStr(step-1) + ': ' + IntToStr(LIArray[step]));

  ArrayLB.Perform(WM_SETREDRAW,1,0);
  ArrayLB.Update;
end;

procedure TSTDlg.SortBtnClick(Sender: TObject);
begin
  MyLMatrix.SortRows(0,MyArraySort);
  FillListBox;
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if NOT (Assigned(MyLMatrix)) then
    begin
      UpdateButtons(False);
      MyLMatrix := TStLMatrix.Create(MaxRows,MaxCols,sizeof(LongInt));
      MyLMatrix.ElementsStorable := True;
    end;
    MyLMatrix.LoadFromFile(OD1.FileName);
    FillListBox;
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
    MyLMatrix.StoreToFile(SD1.FileName);
end;

procedure TSTDlg.ArrayLBDblClick(Sender: TObject);
var
  row,
  col,
  I,
  Value  : LongInt;

begin
  I := ArrayLB.ItemIndex;
  row := I div MaxCols;
  col := I mod MaxCols;

  MyLMatrix.Get(row, col, Value);
  LMRow.Text := IntToStr(row);
  LMCol.Text := IntToStr(col);
  LMValue.Text := IntToStr(Value);
end;



end.
