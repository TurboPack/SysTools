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

unit Ex1DArrU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,

  StConst, StUtils, StBase, StLArr;

type
  ARecord = record
    X, Y   : LongInt;
    Mag    : Double;
    Name1  : string[10];
    Name2  : string[10];
  end;

type
  TSTDlg = class(TForm)
    CreateBtn: TButton;
    ElemNum: TEdit;
    ClearBtn: TButton;
    FillBtn: TButton;
    PutBtn: TButton;
    GetBtn: TButton;
    SortBtn: TButton;
    LB1: TListBox;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure FillBtnClick(Sender: TObject);
    procedure PutBtnClick(Sender: TObject);
    procedure GetBtnClick(Sender: TObject);
    procedure SortBtnClick(Sender: TObject);
    procedure LB1DblClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyLArray : TStLArray;
    ARec     : ARecord;

    procedure SetBusy(B : Boolean);
    procedure FillControls;
    procedure FillListBox;
    function CheckControls(var AR : ARecord) : Boolean;
    procedure UpdateButtons(AOK : Boolean);
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}


procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClass(TStLArray);
  UpdateButtons(False);
end;

procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyLArray.Free;
end;

procedure TSTDlg.SetBusy(B : Boolean);
begin
  if B then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;


function MyArraySort(const El1, El2) : Integer; far;
var
  R1, R2  : ARecord;
begin
  R1 := ARecord(El1);
  R2 := ARecord(El2);

  Result := (R1.X-R2.X);
  if Result = 0 then
    Result := (R1.Y-R2.Y);
  if Result = 0 then
    Result := Trunc(R1.Mag-R2.Mag);
  if Result = 0 then
    Result := CompareText(R1.Name1,R2.Name1);
  if Result = 0 then
    Result := CompareText(R1.Name2,R2.Name2);
end;

procedure TSTDlg.FillControls;
begin
  with ARec do
  begin
    Edit1.Text := Name1;
    Edit2.Text := Name2;
    Edit3.Text := IntToStr(X);
    Edit4.Text := IntToStr(Y);
    Edit5.Text := FloatToStr(Mag);
  end;
end;

procedure TSTDlg.FillListBox;
var
  step : integer;
begin
  SetBusy(True);
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);
  for step := 0 to MyLArray.Count-1 do
  begin
    MyLArray.Get(step,ARec);
    LB1.Items.Add(IntToStr(ARec.X) + ', ' + IntToStr(ARec.Y));
  end;
  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;
  MyLArray.Get(0,ARec);
  SetBusy(False);
end;

function TSTDlg.CheckControls(var AR : ARecord) : Boolean;
var
  C,
  IV : Integer;
  IR : Single;

begin
  Result := False;

  if (Edit1.Text = '') OR
     (Edit2.Text = '') OR
     (Edit3.Text = '') OR
     (Edit4.Text = '') OR
     (Edit5.Text = '') then
    Exit;

  AR.Name1 := Edit1.Text;
  AR.Name2 := Edit2.Text;

  Val(Edit3.Text,IV,C);
  if (C<>0) then
    Exit
  else
    AR.X := IV;

  Val(Edit4.Text,IV,C);
  if (C<>0) then
    Exit
  else
    AR.Y := IV;

  Val(Edit5.Text,IR,C);
  if (C<>0) then
    Exit
  else
    AR.Mag := IR;
  Result := True;
end;

procedure TSTDlg.UpdateButtons(AOK : Boolean);
begin
  ClearBtn.Enabled := AOK;
  FillBtn.Enabled  := AOK;
  GetBtn.Enabled   := AOK;
  PutBtn.Enabled   := AOK;
  SortBtn.Enabled  := AOK;
  SaveBtn.Enabled  := AOK;
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  step,
  I, J,
  Value : LongInt;
begin
  LB1.Clear;
  SetBusy(True);

  if Assigned(MyLArray) then
    MyLArray.Free;

  UpdateButtons(False);
  MyLArray := TStLArray.Create(5000, sizeof(ARec));

  MyLArray.ElementsStorable := True;
  MyLArray.Clear;

  Randomize;
  LB1.Perform(WM_SETREDRAW,0,0);
  Value := MyLArray.Count;

  for step := 0 to Value-1 do
  begin
    with ARec do begin
      Name1 := '';
      Name2 := '';
      for I := 1 to 10 do begin
        J := Random(26) + Ord('A');
        Name1 := Name1+ Chr(J);
        J := Random(26) + Ord('A');
        Name2 := Name2 + Chr(J);
      end;

      X := Trunc(Random(1000));
      Y := Trunc(Random(1000));
      Mag := Sqrt(Random(25000));

      MyLArray.Put(step,ARec);

      LB1.Items.Add(IntToStr(ARec.X) + ', ' + IntToStr(ARec.Y));
    end;
  end;
  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;

  ElemNum.Text := '0';
  MyLArray.Get(0,ARec);
  FillControls;
  UpdateButtons(True);
  SetBusy(False);
end;


procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  MyLArray.Clear;
  LB1.Clear;

  ElemNum.Text := '0';
  MyLArray.Get(0,ARec);
  FillControls;
end;

procedure TSTDlg.FillBtnClick(Sender: TObject);
begin
  if NOT CheckControls(ARec) then
  begin
    ShowMessage('One or more invalid entries');
    Exit;
  end;

  MyLArray.Fill(ARec);

  FillListBox;
  ElemNum.Text := '0';
  MyLArray.Get(0,ARec);
  FillControls;
end;

procedure TSTDlg.PutBtnClick(Sender: TObject);
var
  E  : LongInt;
begin
  if (ElemNum.Text = '') then
    ElemNum.Text := '0';

  if NOT CheckControls(ARec) then
  begin
    ShowMessage('One or more invalid entries');
    Exit;
  end;

  E := StrToInt(ElemNum.Text);
  MyLArray.Put(E,ARec);

  LB1.Items[E] := IntToStr(ARec.X) + ', ' + IntToStr(ARec.Y);

  MyLArray.Get(E,ARec);
  FillControls;
end;


procedure TSTDlg.GetBtnClick(Sender: TObject);
var
  E : LongInt;
begin
  if (ElemNum.Text = '') then
    ElemNum.Text := '0';

  E := StrToInt(ElemNum.Text);
  MyLArray.Get(E,ARec);

  FillControls;
end;

procedure TSTDlg.SortBtnClick(Sender: TObject);
begin
  SetBusy(True);
  MyLArray.Sort(MyArraySort);
  SetBusy(False);

  FillListBox;
  FillControls;
  SetBusy(False);
end;

procedure TSTDlg.LB1DblClick(Sender: TObject);
begin
  MyLArray.Get(LB1.ItemIndex,ARec);
  ElemNum.Text := IntToStr(LB1.ItemIndex);
  FillControls;
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if (NOT Assigned(MyLArray)) then
    begin
      UpdateButtons(False);
      MyLArray := TStLArray.Create(5000, sizeof(ARec));
      MyLArray.ElementsStorable := True;
    end;

    MyLArray.Clear;
    MyLArray.LoadFromFile(OD1.FileName);

    FillListBox;
    FillControls;
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
    MyLArray.StoreToFile(SD1.FileName);
end;


end.
