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

unit ExPQU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  StBase, StPQueue;

const
  InitSize =  50;
  Delta    = 100;
  DefJobs  =  15;

type
  TPQRec = record
    Priority : LongInt;
    Name : string[10];
  end;
  PPQRec = ^TPQRec;

  TStDlg = class(TForm)
    CreateBtn: TButton;
    ClearBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    InsertBtn: TButton;
    DeleteMinBtn: TButton;
    DeleteMaxBtn: TButton;
    LB1: TListBox;
    OD1: TOpenDialog;
    SD1: TSaveDialog;
    ActionEdit: TEdit;
    ActionLabel: TLabel;
    QueueLabel: TLabel;
    JobEdit: TEdit;
    JobLabel: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure DeleteMinBtnClick(Sender: TObject);
    procedure DeleteMaxBtnClick(Sender: TObject);
    procedure JobSpinDownClick(Sender: TObject);
    procedure JobSpinUpClick(Sender: TObject);
  private
    MyPQ : TStPQueue;
    procedure FillListBox;
    function InsertItem : PPQRec;
  end;

var
  StDlg: TStDlg;

implementation

{$R *.DFM}

function MyCompare(Data1, Data2 : Pointer) : Integer; far;
begin
  Result := PPQRec(Data1)^.Priority-PPQRec(Data2)^.Priority;
end;

procedure MyDelNodeData(Data : pointer); far;
begin
  Dispose(PPQRec(Data));
end;

function MyLoadData(Reader : TReader) : Pointer; far;
var
  pn : PPQRec;
begin
  New(pn);
  pn^.Priority := Reader.ReadInteger;
  pn^.Name := Reader.ReadString;
  Result := pn;
end;

procedure MyStoreData(Writer : TWriter; Data : Pointer); far;
begin
  Writer.WriteInteger(PPQRec(Data)^.Priority);
  Writer.WriteString(PPQRec(Data)^.Name);
end;

function JobString(pn : PPQRec) : string;
begin
  with pn^ do
    Result := IntToStr(Priority)+'      '+Name;
end;

function MyListBoxAdd(Container : TStContainer;
  Data, OtherData : Pointer) : Boolean; far;
begin
  TListBox(OtherData).Items.Add(JobString(PPQRec(Data)));
  Result := true;
end;

{--------------------------------------------------------------}

procedure TStDlg.FormCreate(Sender: TObject);
begin
  RegisterClasses([TStPQueue]);
  ClearBtn.Enabled := false;
  SaveBtn.Enabled := false;
  LoadBtn.Enabled := false;
  InsertBtn.Enabled := false;
  DeleteMinBtn.Enabled := false;
  DeleteMaxBtn.Enabled := false;
  JobEdit.Text := IntToStr(DefJobs);
end;

procedure TStDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(MyPQ) then
    MyPQ.Free;
end;

procedure TStDlg.FillListBox;
var
  benabled : boolean;
begin
  Screen.Cursor := crHourGlass;
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW, 0, 0);
  if Assigned(MyPQ) then
    MyPQ.Iterate(MyListBoxAdd, LB1);
  LB1.Perform(WM_SETREDRAW, 1, 0);
  LB1.Update;
  benabled := Assigned(MyPQ) and (MyPQ.Count > 0);
  DeleteMinBtn.Enabled := benabled;
  DeleteMaxBtn.Enabled := benabled;
  Screen.Cursor := crDefault;
end;

function TStDlg.InsertItem : PPQRec;
var
  i : integer;
  pn : PPQRec;
begin
  {create a new item}
  new(pn);
  with pn^ do begin
    {give it a random priority and a random name}
    priority := 100+random(100);
    name := 'job ';
    for i := 1 to 8 do
      name := name+Char(random(26)+Byte('A'));
  end;
  {insert item into priority queue}
  MyPQ.Insert(pn);
  Result := pn;
end;

procedure TStDlg.CreateBtnClick(Sender: TObject);
var
  i, jobs : integer;
begin
  if Assigned(MyPQ) then
    MyPQ.Free;

  MyPQ := TStPQueue.Create(InitSize, Delta);
  MyPQ.Compare := MyCompare;
  MyPQ.DisposeData := MyDelNodeData;
  MyPQ.LoadData := MyLoadData;
  MyPQ.StoreData := MyStoreData;

  {determine how many jobs to add}
  try
    jobs := StrToInt(JobEdit.Text);
    if (jobs < 1) then
      jobs := 1
    else if (jobs > 1000) then
      jobs := 1000;
  except
    jobs := DefJobs;
  end;
  JobEdit.Text := IntToStr(jobs);

  {add random jobs}
  Randomize;
  for i := 1 to jobs do
    InsertItem;

  {update form display}
  FillListBox;
  ActionEdit.Text := 'created';
  ClearBtn.Enabled := true;
  SaveBtn.Enabled := true;
  InsertBtn.Enabled := true;
end;

procedure TStDlg.ClearBtnClick(Sender: TObject);
begin
  MyPQ.Clear;
  FillListBox;
  ActionEdit.Text := 'cleared';
end;

procedure TStDlg.InsertBtnClick(Sender: TObject);
var
  pn : PPQRec;
begin
  pn := InsertItem;
  ActionEdit.Text := JobString(pn)+' inserted';
  FillListBox;
end;

procedure TStDlg.DeleteMinBtnClick(Sender: TObject);
var
  pn : PPQRec;
begin
  pn := PPQRec(MyPQ.DeleteMin);
  ActionEdit.Text := JobString(pn)+' deleted';
  MyPQ.DisposeData(pn);
  FillListBox;
end;

procedure TStDlg.DeleteMaxBtnClick(Sender: TObject);
var
  pn : PPQRec;
begin
  pn := PPQRec(MyPQ.DeleteMax);
  ActionEdit.Text := JobString(pn)+' deleted';
  MyPQ.DisposeData(pn);
  FillListBox;
end;

procedure TStDlg.JobSpinDownClick(Sender: TObject);
var
  jobs : integer;
begin
  try
    jobs := StrToInt(JobEdit.Text);
  except
    jobs := DefJobs;
  end;
  if (jobs > 1) then
    dec(jobs);
  JobEdit.Text := IntToStr(jobs);
end;

procedure TStDlg.JobSpinUpClick(Sender: TObject);
var
  jobs : integer;
begin
  try
    jobs := StrToInt(JobEdit.Text);
  except
    jobs := DefJobs;
  end;
  if (jobs < 1000) then
    inc(jobs);
  JobEdit.Text := IntToStr(jobs);
end;

procedure TStDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then begin
    MyPQ.LoadFromFile(OD1.FileName);
    FillListBox;
    ActionEdit.Text := 'loaded';
  end;
end;

procedure TStDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then begin
    MyPQ.StoreToFile(SD1.FileName);
    LoadBtn.Enabled := true;
    ActionEdit.Text := 'saved';
  end;
end;

end.
