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

unit ExDQueU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  StConst, StBase, StUtils, StList, StDQue;

type
  S10 = string[10];
  TSTDlg = class(TForm)
    CreateBtn: TButton;
    Edit1: TEdit;
    PushHeadBtn: TButton;
    PopHeadBtn: TButton;
    HeadBtn: TButton;
    TailBtn: TButton;
    LB1: TListBox;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;
    ClearBtn: TButton;
    PushTailBtn: TButton;
    PopTailBtn: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CreateBtnClick(Sender: TObject);
    procedure PushHeadBtnClick(Sender: TObject);
    procedure PopHeadBtnClick(Sender: TObject);
    procedure HeadBtnClick(Sender: TObject);
    procedure TailBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure PushTailBtnClick(Sender: TObject);
    procedure PopTailBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyDQue  : TStDQue;

    procedure FillListBox;
    procedure UpdateButtons(QueOK : Boolean);
  end;

const
  MaxElem = 100;
var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

procedure MyDelNodeData(Data : pointer); far;
 {-procedure to delete data pointer in each node
   during call to TSTList.Destroy}
begin
  FreeMem(Data, SizeOf(S10));
end;

function MyLoadData(Reader : TReader) : Pointer; far;
begin
  GetMem(Result, SizeOf(S10));
  S10(Result^) := Reader.ReadString;
end;

procedure MyStoreData(Writer : TWriter; Data : Pointer); far;
begin
  Writer.WriteString(S10(Data^));
end;

procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClasses([TStDQue,TStListNode]);
  UpdateButtons(False);
end;

procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyDQue.Free;
end;

procedure TSTDlg.FillListBox;
var
  PN : TStListNode;
begin
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW, 0, 0);
  PN := MyDQue.Head;
  while (PN <> nil) do
  begin
    LB1.Items.Add(S10(PN.Data^));
    PN := MyDQue.Next(PN);
  end;
  LB1.Perform(WM_SETREDRAW, 1, 0);
  LB1.Update;
end;

procedure TSTDlg.UpdateButtons(QueOK : Boolean);
begin
  ClearBtn.Enabled := QueOK;
  PushHeadBtn.Enabled  := QueOK;
  PopHeadBtn.Enabled  := QueOK;
  PushTailBtn.Enabled  := QueOK;
  PopTailBtn.Enabled  := QueOK;
  HeadBtn.Enabled  := QueOK;
  TailBtn.Enabled  := QueOK;
  SaveBtn.Enabled  := QueOK;
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  I : Integer;
  S : ^S10;
begin
  if Assigned(MyDQue) then
    MyDQue.Free;

  UpdateButtons(False);
  MyDQue := TStDQue.Create(TStListNode);

  MyDQue.DisposeData := MyDelNodeData;
  MyDQue.LoadData := MyLoadData;
  MyDQue.StoreData := MyStoreData;

  for I := 1 to MaxElem do
  begin
    GetMem(S, SizeOf(S10));
    S^ := 'Item' + IntToStr(I);
    MyDQue.Append(S);
  end;
  FillListBox;
  UpdateButtons(True);
end;

procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  MyDQue.Clear;
  Edit1.Text := '';
  FillListBox;
  UpdateButtons(False);
end;

procedure TSTDlg.PushHeadBtnClick(Sender: TObject);
var
  NewString : ^S10;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('No value entered');
    Exit;
  end;
  GetMem(NewString,SizeOf(S10));
  NewString^ := Edit1.Text;
  MyDQue.PushHead(NewString);
  FillListBox;
end;

procedure TSTDlg.PopHeadBtnClick(Sender: TObject);
begin
  MyDQue.PopHead;
  FillListBox;
end;

procedure TSTDlg.PushTailBtnClick(Sender: TObject);
var
  NewString : ^S10;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('No value entered');
    Exit;
  end;
  GetMem(NewString,SizeOf(S10));
  NewString^ := Edit1.Text;
  MyDQue.PushTail(NewString);
  FillListBox;
end;

procedure TSTDlg.PopTailBtnClick(Sender: TObject);
begin
  MyDQue.PopTail;
  FillListBox;
end;

procedure TSTDlg.HeadBtnClick(Sender: TObject);
var
  Data : Pointer;
begin
  MyDQue.PeekHead(Data);
  Edit1.Text := S10(Data^);
end;

procedure TSTDlg.TailBtnClick(Sender: TObject);
var
  Data : Pointer;
begin
  MyDQue.PeekTail(Data);
  Edit1.Text := S10(Data^);
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if (NOT Assigned(MyDQue)) then
    begin
      UpdateButtons(False);
      MyDQue := TStDQue.Create(TStListNode);

      MyDQue.DisposeData := MyDelNodeData;
      MyDQue.LoadData := MyLoadData;
      MyDQue.StoreData := MyStoreData;
    end;
    MyDQue.LoadFromFile(OD1.FileName);
    FillListBox;
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
    MyDQue.StoreToFile(SD1.FileName);
end;

end.
