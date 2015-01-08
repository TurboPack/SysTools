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

unit ExTreeU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,

  StConst, StBase, StTree;

type
  S10 = String[10];
  S15 = String[15];

  PersonRecord = record
    First : S10;
    Last  : S15;
    Age   : Integer;
  end;
  PPersonRecord = ^PersonRecord;

  TSTDlg = class(TForm)
    CreateBtn: TButton;
    ClearBtn: TButton;
    LB1: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    InsertBtn: TButton;
    DeleteBtn: TButton;
    FindBtn: TButton;
    SearchBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure LB1DblClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetBusy(B : Boolean);
    procedure FillListBox;
    procedure FillControls(PR : PersonRecord);
    function GetControls(var PR : PersonRecord) : Boolean;
    procedure UpdateButtons(TOK : Boolean);
  end;

const
  MaxElem = 3000;

var
  STDlg: TSTDlg;
  FirstA : array[0..7] of S10;
  LastA  : array[0..7] of S15;
  MyTree : TStTree;


implementation

{$R *.DFM}

function MyLoadData(Reader : TReader) : Pointer; far;
begin
  GetMem(Result,SizeOf(PersonRecord));
  with PersonRecord(Result^), Reader do
  begin
    First := ReadString;
    Last  := ReadString;
    Age   := ReadInteger;
  end;
end;

procedure MyStoreData(Writer : TWriter; Data : Pointer); far;
var
  PR : PersonRecord;
begin
  PR := PersonRecord(Data^);
  with Writer do
  begin
    WriteString(PR.First);
    WriteString(PR.Last);
    WriteInteger(PR.Age);
  end;
end;


procedure MyDisposeData(Data : Pointer); far;
begin
  FreeMem(Data, SizeOf(PersonRecord));
end;

function MySortTree(Data1, Data2 : Pointer) : Integer; far;
var
  R1 : PPersonRecord absolute Data1;
  R2 : PPersonRecord absolute Data2;
begin
  Result := CompareText(R1^.Last, R2^.Last);
  if Result = 0 then
    CompareText(R1^.First, R2^.First);
  if Result = 0 then
    Result := (R1^.Age - R2^.Age);
end;

function MyTreeWalker(Contariner : TStContainer;
                      Node : TStNode;
                      OtherData : Pointer) : Boolean; far;
var
  R : PersonRecord;
  S : String;
begin
  R := PersonRecord(Node.Data^);
  S := R.Last + ', ' + R.First + ', ' + IntToStr(R.Age);
  STDlg.LB1.Items.Add(S);
  Result := True;
end;

function MyTreeSearcher(Contariner : TStContainer;
                        Node : TStNode;
                        OtherData : Pointer) : Boolean; far;

var
  S   : string;
  R1  : PersonRecord;
  R2  : PPersonRecord absolute OtherData;
begin
  R1 := PersonRecord(Node.Data^);
  if (CompareText(R1.Last, R2^.Last) = 0) then
  begin
    S := 'Match: ' + R1.First + ' ' + R1.Last + ', ' + IntToStr(R1.Age);
    if MessageDlg(S,mtInformation,[mbOK,mbCancel],0) = mrCancel then
      Result := False
    else
      Result := True;
  end else
    Result := True;
end;

procedure TSTDlg.SetBusy(B : Boolean);
begin
  if B then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;

procedure TSTDlg.FillListBox;
begin
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);
  SetBusy(True);

  MyTree.Iterate(MyTreeWalker,True,nil);

  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;
  SetBusy(False);
end;

procedure TSTDlg.FillControls(PR : PersonRecord);
begin
  Edit1.Text := PR.First;
  Edit2.Text := PR.Last;
  Edit3.Text := IntToStr(PR.Age);
end;

function TSTDlg.GetControls(var PR : PersonRecord) : Boolean;
var
  I,
  Code : Integer;
begin
  Result := False;
  if (Edit1.Text = '') OR
     (Edit2.Text = '') OR
     (Edit3.Text = '') then
    Exit;

  PR.First := Edit1.Text;
  PR.Last  := Edit2.Text;

  Val(Edit3.Text,I,Code);
  if (Code <> 0) then
    Exit
  else
    PR.Age := I;
  Result := True;
end;


procedure TSTDlg.UpdateButtons(TOK : Boolean);
begin
  ClearBtn.Enabled  := TOK;
  InsertBtn.Enabled := TOK;
  DeleteBtn.Enabled := TOK;
  FindBtn.Enabled   := TOK;
  SearchBtn.Enabled := TOK;
  SaveBtn.Enabled   := TOK;
end;


procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClasses([TStTree,TStTreeNode]);
  UpdateButtons(False);
end;


procedure TSTDlg.FormActivate(Sender: TObject);
begin
  FirstA[0] := 'Fred';
  FirstA[1] := 'Mike';
  FirstA[2] := 'Barney';
  FirstA[3] := 'Horatio';
  FirstA[4] := 'Mickey';
  FirstA[5] := 'Arthur';
  FirstA[6] := 'Santa';
  FirstA[7] := 'John Q. ';

  LastA[0] := 'Flintstone';
  LastA[1] := 'Hammer';
  LastA[2] := 'Rubble';
  LastA[3] := 'Hornblower';
  LastA[4] := 'Spilane';
  LastA[5] := 'Miller';
  LastA[6] := 'Claus';
  LastA[7] := 'Public';
end;

procedure TSTDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MyTree.Free;
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  I  : Integer;
  PR : PPersonRecord;
  TN : TStTreeNode;
begin
  if Assigned(MyTree) then
    MyTree.Free;

  UpdateButtons(False);
  MyTree:= TStTree.Create(TStTreeNode);

  MyTree.Compare := MySortTree;
  MyTree.DisposeData := MyDisposeData;
  MyTree.LoadData := MyLoadData;
  MyTree.StoreData := MyStoreData;

  SetBusy(True);
  for I := 0 to MaxElem-1 do
  begin
    if (I mod 250 = 0) then Randomize;
    GetMem(PR, SizeOf(PersonRecord));
    with PR^ do
    repeat
      First := FirstA[Random(8)];
      Last := LastA[Random(8)];
      Age := Random(10000);

      {search for duplicate entry, if found - don't try to add}
      TN := MyTree.Find(PR);
      if TN = nil then
        MyTree.Insert(PR);
    until TN = nil;
  end;
  FillListBox;
  SetBusy(False);
  UpdateButtons(True);
end;

procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  MyTree.Clear;
  LB1.Clear;
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
end;

procedure TSTDlg.InsertBtnClick(Sender: TObject);
var
  PR : PPersonRecord;
begin
  GetMem(PR, SizeOf(PersonRecord));
  if NOT (GetControls(PR^)) then
  begin
    FreeMem(PR, SizeOf(PersonRecord));
    ShowMessage('One or more fields invalid');
    Exit;
  end else
  begin
    MyTree.Insert(PR);
    FillListBox;
  end;
end;

procedure TSTDlg.DeleteBtnClick(Sender: TObject);
var
  PR : PersonRecord;
  TN : TStTreeNode;
begin
  if NOT (GetControls(PR)) then
  begin
    ShowMessage('One or more invalid entry fields');
    Exit;
  end;
  TN := MyTree.Find(@PR);
  if (TN <> nil) then
  begin
    MyTree.Delete(@PR);
    FillListBox;
  end else
    ShowMessage('Record not found');
end;

procedure TSTDlg.FindBtnClick(Sender: TObject);
var
  PR : PersonRecord;
  TN : TStTreeNode;
begin
  if NOT (GetControls(PR)) then
  begin
    ShowMessage('One or more invalid entry fields');
    Exit;
  end;

  TN := MyTree.Find(@PR);
  if (TN <> nil) then
    ShowMessage('Record was found');
end;

procedure TSTDlg.SearchBtnClick(Sender: TObject);
var
  PR : PersonRecord;
begin
  PR.Last := Edit2.Text;
  MyTree.Iterate(MyTreeSearcher, True, @PR);
end;

procedure TSTDlg.LB1DblClick(Sender: TObject);
var
  I,
  L  : Integer;
  PR : PersonRecord;
  S  : string;
  TN : TStTreeNode;

begin
  S := LB1.Items[LB1.ItemIndex];
  L := Length(S);
  I := pos(',', S);

  PR.Last := S;
  Delete(PR.Last, I, L-I+1);
  Delete(S, 1, I+1);

  PR.First := S;
  L := Length(PR.First);
  I := pos(',', PR.First);

  Delete(PR.First, I, L-I+1);
  Delete(S, 1, I+1);
  PR.Age := StrToInt(S);

  TN := MyTree.Find(@PR);
  if TN <> nil then
  begin
    MyTree.Delete(@PR);
    FillListBox;
  end;
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if OD1.Execute then
  begin
    if (NOT Assigned(MyTree)) then
    begin
      UpdateButtons(False);
      MyTree:= TStTree.Create(TStTreeNode);
      MyTree.Compare := MySortTree;
      MyTree.DisposeData := MyDisposeData;
      MyTree.LoadData := MyLoadData;
      MyTree.StoreData := MyStoreData;
    end;

    MyTree.Clear;
    MyTree.LoadFromFile(OD1.FileName);
    FillListBox;
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if SD1.Execute then
    MyTree.StoreToFile(SD1.FileName);
end;

end.
