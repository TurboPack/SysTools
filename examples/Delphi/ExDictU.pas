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

unit ExDictU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  StConst, StBase, StUtils, StList, StDict;

type
  TSTDlg = class(TForm)
    CreateBtn: TButton;
    ClearBtn: TButton;
    LB1: TListBox;
    Label9: TLabel;
    Edit1: TEdit;
    Label10: TLabel;
    Edit2: TEdit;
    AddBtn: TButton;
    DelBtn: TButton;
    ExistsBtn: TButton;
    UpDateBtn: TButton;
    SaveBtn: TButton;
    LoadBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;
    Label1: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ClearBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure ExistsBtnClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure UpDateBtnClick(Sender: TObject);
    procedure LB1Click(Sender: TObject);
    procedure LB1DblClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    MyDD  : TStDictionary;

    function RandomData : ShortString;
    procedure UpdateButtons(DOK : Boolean);
    procedure FillListBox;
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

const
  MaxElem = 100;
  MaxLen  = 15;
  GHash   = 127;


function DDWalker(Container : TStContainer;
                  Data : TStNode;
                  OtherData : Pointer) : Boolean; far;
var
  S : ShortString;
begin
  S := ShortString(Data.Data^);
  STDlg.LB1.Items.Add(TStDictNode(Data).Name + ' = ' + S);
  Result := True;
end;

function MyLoadData(Reader : TReader) : Pointer; far;
begin
  GetMem(Result,SizeOf(ShortString));
  ShortString(Result^) := Reader.ReadString;
end;

procedure MyStoreData(Writer : TWriter; Data : Pointer); far;
begin
  Writer.WriteString(ShortString(Data^));
end;

procedure TSTDlg.FillListBox;
begin
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);

  MyDD.Iterate(DDWalker,nil);

  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;
end;

procedure TSTDlg.UpdateButtons(DOK : Boolean);
begin
  ClearBtn.Enabled  := DOK;
  AddBtn.Enabled    := DOK;
  UpdateBtn.Enabled := DOK;
  ExistsBtn.Enabled := DOK;
  DelBtn.Enabled    := DOK;
  SaveBtn.Enabled   := DOK;
  LB1.Enabled       := DOK;
end;

procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClasses([TStDictionary,TStDictNode]);
  UpdateButtons(False);
end;

procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyDD.Free;
end;

function TSTDlg.RandomData : ShortString;
var
  Len,
  I    : Integer;
begin
  Len := Random(MaxLen)+1;
  Result[0] := Chr(Len);
  for I := 1 to Len do
    Result[I] := Chr(Random(26) + Ord('A'));
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  I  : Integer;
  S  : ^ShortString;
begin
  Randomize;
  if Assigned(MyDD) then
    MyDD.Free;

  UpdateButtons(False);
  MyDD := TStDictionary.Create(GHash);
  MyDD.LoadData := MyLoadData;
  MyDD.StoreData := MyStoreData;
  MyDD.Hash := AnsiElfHashText;

  for I := 1 to MaxElem do
  begin
    GetMem(S,SizeOf(ShortString));
    S^ := RandomData;
    MyDD.Add('Item' + IntToStr(I),S);
  end;
  FillListBox;
  UpdateButtons(True);
end;

procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  LB1.Clear;
  MyDD.Clear;
  Edit1.Clear;
  Edit2.Clear;
end;

procedure TSTDlg.AddBtnClick(Sender: TObject);
var
  Name : ShortString;
  PS   : ^ShortString;
begin
  if (Edit1.Text = '') OR (Edit2.Text = '') then
  begin
    ShowMessage('Name and/or data missing');
    Exit;
  end;

  GetMem(PS,SizeOf(ShortString));
  PS^ := Edit2.Text;
  Name := Edit1.Text;

  MyDD.Add(Name,PS);

  FillListBox;
end;

procedure TSTDlg.UpDateBtnClick(Sender: TObject);
var
  P    : Pointer;
begin
  if (Edit1.Text = '') OR (Edit2.Text = '') then
  begin
    ShowMessage('Name and/or data missing');
    Exit;
  end;

   if (MyDD.Exists(Edit1.Text,P)) then
  begin
    ShortString(P^) := Edit2.Text;
    MyDD.Update(Edit1.Text,P);
  end else
  begin
    ShowMessage(Edit1.Text + ' not found');
    Exit;
  end;
  FillListBox;
end;

procedure TSTDlg.DelBtnClick(Sender: TObject);
var
  P : Pointer;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('No name entered');
    Exit;
  end;

  if (MyDD.Exists(Edit1.Text,P)) then
  begin
    MyDD.Delete(Edit1.Text);
    FillListBox;
  end else
    ShowMessage('Entry not found');
end;

procedure TSTDlg.ExistsBtnClick(Sender: TObject);
var
  S : Pointer;
begin
  if (Edit1.Text = '') then
  begin
    ShowMessage('No name entry');
    Exit;
  end;

  if MyDD.Exists(Edit1.Text,S) then
  begin
    Edit2.Clear;
    Edit2.Text := ShortString(S^);
    Edit2.Update;
  end else
    ShowMessage('No matching entry found');
end;

procedure TSTDlg.LB1Click(Sender: TObject);
var
  S1,
  S2  : string;
  P,
  Len : integer;
begin
  S1 := LB1.Items[LB1.ItemIndex];
  S2 := S1;
  Len := Length(S1);
  P := pos('=',S1);

  Delete(S1,p-1,Len-p+2);
  Edit1.Text := S1;

  Delete(S2,1,p+1);
  Edit2.Text := S2;
end;

procedure TSTDlg.LB1DblClick(Sender: TObject);
var
  P : Pointer;
begin
  if (MyDD.Exists(Edit1.Text,P)) then
  begin
    MyDD.Delete(Edit1.Text);
    FillListBox;
  end;
  LB1.ItemIndex := 0;
  LB1Click(LB1);
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if (NOT Assigned(MyDD)) then
    begin
      UpdateButtons(False);
      MyDD := TStDictionary.Create(GHash);
      MyDD.LoadData := MyLoadData;
      MyDD.StoreData := MyStoreData;
    end;

    MyDD.Clear;
    MyDD.LoadFromFile(OD1.FileName);

    FillListBox;
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
    MyDD.StoreToFile(SD1.FileName);
end;


end.
