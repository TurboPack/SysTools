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

unit ExListU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  StConst, StBase, StUtils, StList;

type
  TSTDlg = class(TForm)
    CreateBtn: TButton;
    ClearBtn: TButton;
    DeleteBtn: TButton;
    HeadBtn: TButton;
    LB1: TListBox;
    Edit1: TEdit;
    AppendBtn: TButton;
    InsertBtn: TButton;
    InsSortedBtn: TButton;
    PlaceBtn: TButton;
    PlaceBeforeBtn: TButton;
    SortBtn: TButton;
    Bevel1: TBevel;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure HeadBtnClick(Sender: TObject);
    procedure AppendBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure InsSortedBtnClick(Sender: TObject);
    procedure PlaceBtnClick(Sender: TObject);
    procedure PlaceBeforeBtnClick(Sender: TObject);
    procedure SortBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetBusy(B : Boolean);
    procedure FillListBox;
    procedure UpdateButtons(LOK : Boolean);
    procedure CreateList;
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

const
  MaxElems = 5000;

type
  S10 = string[10];

var
  MyList : TStList;


function MyCompare(Data1, Data2 : Pointer) : Integer; far;
  {-global function used to sort string items in TStList based classes}
begin
  Result := CompareText(S10(Data1^),S10(Data2^));
end;

function MatchStrings(Container : TStContainer;
                      Node : TStNode;
                      OtherData : Pointer) : Boolean; far;
 {-user defined function to search for strings in a
   TStList based class. Used by the TStList.Iterate method}
begin
  Result := S10(Node.Data^) <> S10(OtherData^);
end;

procedure MyDelNodeData(Data : pointer); far;
 {-procedure to delete data pointer in each node
   during call to TStList.Destroy}
begin
  FreeMem(Data,SizeOf(S10));
end;


function MyLoadData(Reader : TReader) : Pointer; far;
begin
  GetMem(Result,SizeOf(S10));
  S10(Result^) := Reader.ReadString;
end;


procedure MyStoreData(Writer : TWriter; Data : Pointer); far;
begin
  Writer.WriteString(S10(Data^));
end;

procedure TSTDlg.FormCreate(Sender: TObject);
begin
 RegisterClasses([TStList,TStListNode]);
 UpdateButtons(False);
end;


procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyList.Free;
end;

procedure TSTDlg.SetBusy(B : Boolean);
begin
  if B then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;

procedure TSTDlg.UpdateButtons(LOK : Boolean);
begin
  ClearBtn.Enabled       := LOK;
  DeleteBtn.Enabled      := LOK;
  HeadBtn.Enabled        := LOK;
  AppendBtn.Enabled      := LOK;
  InsertBtn.Enabled      := LOK;
  InsSortedBtn.Enabled   := LOK;
  PlaceBtn.Enabled       := LOK;
  PlaceBeforeBtn.Enabled := LOK;
  SortBtn.Enabled        := LOK;
  SaveBtn.Enabled        := LOK;
end;

procedure TSTDlg.CreateList;
begin
  UpdateButtons(False);
  MyList := TStList.Create(TStListNode);

  MyList.Compare := MyCompare;
  MyList.DisposeData := MyDelNodeData;
  MyList.LoadData := MyLoadData;
  MyList.StoreData := MyStoreData;
end;


procedure TSTDlg.FillListBox;
var
  PN : TStListNode;

begin
  PN := MyList.Head;

  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);
  SetBusy(True);

  while (PN <> nil) do
  begin
    LB1.Items.Add(S10(PN.Data^));
    PN := MyList.Next(PN);
  end;

  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;
  SetBusy(False);
end;


procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  J,
  step : integer;
  S    : ^S10;
begin
  if Assigned(MyList) then
    MyList.Free;

  CreateList;
  Randomize;
  for step := 1 to MaxElems do
  begin
    GetMem(S,SizeOf(S10));
    S^[0] := Chr(10);
    for J := 1 to 10 do
      S^[J] := Chr(random(26) + Ord('A'));
    MyList.Append(S);
  end;
  FillListBox;
  UpdateButtons(True);
end;


procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  MyList.Clear;
  {confirm list was cleared}
  FillListBox;
end;

procedure TSTDlg.DeleteBtnClick(Sender: TObject);
var
  WhichOne : integer;
  PN : TStListNode;
  S : S10;
begin
  WhichOne := LB1.ItemIndex;
  if (WhichOne < 0) then
  begin
    ShowMessage('No item selected');
    Exit;
  end;

  S := LB1.Items[WhichOne];
  PN := MyList.Iterate(MatchStrings,True,@S);

  if (PN <> nil) then
  begin
    MyList.Delete(PN);
    FillListBox;
  end;
end;

procedure TSTDlg.HeadBtnClick(Sender: TObject);
var
  WhichOne : integer;
  S        : S10;
  PN       : TStListNode;
begin
  WhichOne := LB1.ItemIndex;
  if (WhichOne < 0) then
  begin
    ShowMessage('No item selected');
    Exit;
  end;

  S := LB1.Items[WhichOne];
  PN := MyList.Iterate(MatchStrings,True,@S);
  if (PN <> nil) then
  begin
    MyList.MoveToHead(PN);
    FillListBox;
  end;
end;

procedure TSTDlg.AppendBtnClick(Sender: TObject);
var
  S   : ^S10;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('Empty string not allowed');
    Exit;
  end;
  GetMem(S,SizeOf(S10));
  S^ := Edit1.Text;
  MyList.Append(S);
  FillListBox;
end;

procedure TSTDlg.InsertBtnClick(Sender: TObject);
var
  S : ^S10;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('Empty string not allowed');
    Exit;
  end;
  GetMem(S,SizeOf(S10));
  S^ := Edit1.Text;
  MyList.Insert(S);
  FillListBox;
end;

procedure TSTDlg.InsSortedBtnClick(Sender: TObject);
var
  S : ^S10;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('Empty string not allowed');
    Exit;
  end;
  GetMem(S,SizeOf(S10));
  S^ := Edit1.Text;
  MyList.InsertSorted(S);
  FillListBox;
end;

procedure TSTDlg.PlaceBtnClick(Sender: TObject);
var
  WhichOne : integer;
  S        : ^S10;
  PS       : S10;
  PN       : TStListNode;
begin
  WhichOne := LB1.ItemIndex;
  if (WhichOne < 0) then
  begin
    ShowMessage('No item selected');
    Exit;
  end;
  if (Edit1.Text = '') then
  begin
    ShowMessage('Empty string not allowed');
    Exit;
  end;
  GetMem(S,SizeOf(S10));
  S^ := Edit1.Text;
  PS := LB1.Items[WhichOne];
  PN := MyList.Iterate(MatchStrings,True,@PS);
  if (PN <> nil) then
  begin
    MyList.Place(S,PN);
    FillListBox;
  end;
end;

procedure TSTDlg.PlaceBeforeBtnClick(Sender: TObject);
var
  WhichOne : integer;
  S        : ^S10;
  PS       : S10;
  PN       : TStListNode;

begin
  WhichOne := LB1.ItemIndex;
  if (WhichOne < 0) then
  begin
    ShowMessage('No item selected');
    Exit;
  end;
  if (Edit1.Text = '') then
  begin
    ShowMessage('Empty string not allowed');
    Exit;
  end;
  GetMem(S,SizeOf(S10));
  S^ := Edit1.Text;
  PS := LB1.Items[WhichOne];
  PN := MyList.Iterate(MatchStrings,True,@PS);
  if (PN <> nil) then
  begin
    MyList.PlaceBefore(S,PN);
    FillListBox;
  end;
end;

procedure TSTDlg.SortBtnClick(Sender: TObject);
begin
  MyList.Sort;
  FillListBox;
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if (NOT Assigned(MyList)) then
      CreateList;
    MyList.Clear;
    MyList.LoadFromFile(OD1.FileName);

    FillListBox;
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
    MyList.StoreToFile(SD1.FileName);
end;


end.
