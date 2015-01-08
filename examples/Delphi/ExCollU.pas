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

unit ExCollU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  StConst, StBase, StColl;

type
  S10 = string[10];
  S15 = string[15];

  ARecord = record
    First : S10;
    Last  : S15;
    Age   : Integer;
  end;

  TSTDlg = class(TForm)
    CreateBtn: TButton;
    LB1: TListBox;
    ClearBtn: TButton;
    PackBtn: TButton;
    EffBtn: TButton;
    Edit1: TEdit;
    Edit3: TEdit;
    Label8: TLabel;
    Edit2: TEdit;
    AtBtn: TButton;
    AtInsBtn: TButton;
    AtPutBtn: TButton;
    DelBtn: TButton;
    AtDelBtn: TButton;
    InsBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure PackBtnClick(Sender: TObject);
    procedure EffBtnClick(Sender: TObject);
    procedure AtBtnClick(Sender: TObject);
    procedure AtInsBtnClick(Sender: TObject);
    procedure AtPutBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure AtDelBtnClick(Sender: TObject);
    procedure InsBtnClick(Sender: TObject);
    procedure LB1DblClick(Sender: TObject);
    procedure LB1Click(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetBusy(B : Boolean);
    procedure FillControls(AR : ARecord);
    function CheckControls(var AR : ARecord) : Boolean;
    procedure FillListBox;
    procedure UpdateButtons(COK : Boolean);
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}


const
  MaxElem = 20000;

var
  FirstA : array[0..7] of S10;
  LastA  : array[0..7] of S15;
  MyCollection : TStCollection;


procedure MyDelNodeData(Data : pointer); far;
 {-procedure to delete data pointer in each node}
begin
  FreeMem(Data,SizeOf(ARecord));
end;

function MatchCollString(Container : TStContainer;
                         Data : Pointer;
                         OtherData : Pointer) : Boolean; far;
begin
  Result := (ARecord(Data^).First <> ARecord(OtherData^).First) OR
            (ARecord(Data^).Last <> ARecord(OtherData^).Last);
end;

function CollWalker(Container : TStContainer;
                    Data : Pointer;
                    OtherData : Pointer) : Boolean; far;
{this function makes no comparison and always returns True}
{so it will visit all nodes in the collection}
begin
  with ARecord(Data^) do
    STDlg.LB1.Items.Add(First + ' ' + Last + ', ' + IntToStr(Age));
  Result := True;
end;

procedure MyStoreData(Writer : TWriter; Data : Pointer); far;
begin
  with ARecord(Data^), Writer do
  begin
    WriteString(First);
    WriteString(Last);
    WriteInteger(Age);
  end;
end;

function MyLoadData(Reader : TReader) : Pointer; far;
begin
  GetMem(Result,SizeOf(ARecord));
  with ARecord(Result^), Reader do
  begin
    First := ReadString;
    Last  := ReadString;
    Age   := ReadInteger;
  end;
end;


procedure TSTDlg.UpdateButtons(COK : Boolean);
begin
  ClearBtn.Enabled := COK;
  PackBtn.Enabled  := COK;
  AtBtn.Enabled    := COK;
  AtInsBtn.Enabled := COK;
  AtPutBtn.Enabled := COK;
  DelBtn.Enabled   := COK;
  AtDelBtn.Enabled := COK;
  InsBtn.Enabled   := COK;
  EffBtn.Enabled   := COK;
  SaveBtn.Enabled  := COK;
end;

procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClass(TStCollection);
  UpdateButtons(False);

  FirstA[0] := 'Fred';
  FirstA[1] := 'Robert';
  FirstA[2] := 'Barney';
  FirstA[3] := 'Horatio';
  FirstA[4] := 'Kent';
  FirstA[5] := 'Arthur';
  FirstA[6] := 'Lee';
  FirstA[7] := 'John Q. ';

  LastA[0] := 'Flintstone';
  LastA[1] := 'Java';
  LastA[2] := 'Rubble';
  LastA[3] := 'Hornblower';
  LastA[4] := 'C++Builder';
  LastA[5] := 'Miller';
  LastA[6] := 'Delphi';
  LastA[7] := 'Public';
end;

procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyCollection.Free;
end;

procedure TSTDlg.SetBusy(B : Boolean);
begin
  if B then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;

function TSTDlg.CheckControls(var AR : ARecord) : Boolean;
var
  C,
  IV : Integer;
begin
  Result := False;

  if (Edit3.Text = '') OR
     (Edit4.Text = '') OR
     (Edit5.Text = '') then
    Exit;

  AR.First := Edit3.Text;
  AR.Last := Edit4.Text;

  Val(Edit5.Text,IV,C);
  if (C<>0) then
    Exit
  else
    AR.Age := IV;
  Result := True;
end;

procedure TSTDlg.FillControls(AR : ARecord);
begin
  with AR do
  begin
    Edit3.Text := First;
    Edit4.Text := Last;
    Edit5.Text := IntToStr(Age);
  end;
end;

procedure TSTDlg.FillListBox;
begin
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);

  SetBusy(True);

  MyCollection.Iterate(CollWalker,True,nil);

  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;
  LB1.ItemIndex := 0;
  Edit2.Text := '0';

  SetBusy(False);
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  I   : Integer;
  AR  : ^ARecord;
begin
  if Assigned(MyCollection) then
    MyCollection.Free;

  UpdateButtons(False);
  MyCollection := TStCollection.Create(100);

  MyCollection.DisposeData := MyDelNodeData;
  MyCollection.LoadData := MyLoadData;
  MyCollection.StoreData := MyStoreData;

  Randomize;
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);
  SetBusy(True);

  for I := 0 to MaxElem-1 do
  begin
    GetMem(AR,SizeOf(ARecord));
    with AR^ do
    begin
      First := FirstA[Random(8)];
      Last := LastA[Random(8)];
      Age := Random(100);

      MyCollection.Insert(AR);
      LB1.Items.Add(First + ' ' + Last + ', ' + IntToStr(Age));
    end;
  end;
  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;

  MyCollection.Pack;
  Edit1.Text := IntToStr(MyCollection.Efficiency);
  UpdateButtons(True);
  SetBusy(False);
end;

procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  MyCollection.Clear;
  LB1.Clear;
  Edit1.Text := IntToStr(MyCollection.Efficiency);
end;

procedure TSTDlg.PackBtnClick(Sender: TObject);
begin
  if (MessageDlg('Current Efficiency: ' + IntToStr(MyCollection.Efficiency) +
                 #13 + 'Pack Collection?',
                 mtConfirmation,[mbYes,mbNo],0) = mrNo) then Exit;

  MyCollection.Pack;
  Edit1.Text := IntToStr(MyCollection.Efficiency);
end;

procedure TSTDlg.EffBtnClick(Sender: TObject);
begin
  Edit1.Text := IntToStr(MyCollection.Efficiency);
end;

procedure TSTDlg.AtBtnClick(Sender: TObject);
var
  Data : Pointer;
  E : LongInt;
begin
  if (Edit2.Text = '') then
    Edit2.Text := '0';
  E := StrToInt(Edit2.Text);
  if (E > MyCollection.Count-1) OR (E < 0) then
  begin
    ShowMessage('Element value out of range (0..' + IntToStr(MyCollection.Count) + ')');
    Edit2.Text := '0';
    Exit;
  end;

  Data := MyCollection.At(E);
  FillControls(ARecord(Data^));;
end;

procedure TSTDlg.AtInsBtnClick(Sender: TObject);
var
  E    : LongInt;
  PAR  : ^ARecord;
begin
  GetMem(PAR,SizeOf(ARecord));
  if (NOT CheckControls(PAR^)) then
  begin
    ShowMessage('One or more data controls invalid');
    FreeMem(PAR,SizeOf(ARecord));
    Exit;
  end;

  if (Edit2.Text = '') then
    Edit2.Text := '0';
  E := StrToInt(Edit2.Text);
  if (E > MyCollection.Count-1) OR (E < 0) then
  begin
    ShowMessage('Element value out of range (0..' + IntToStr(MyCollection.Count) + ')');
    Edit2.Text := '0';
    Exit;
  end;

  MyCollection.AtInsert(E,PAR);
  FillListBox;
end;

procedure TSTDlg.AtPutBtnClick(Sender: TObject);
var
  E    : LongInt;
  Data : Pointer;
  AR   : ARecord;
begin
  if (NOT CheckControls(AR)) then
  begin
    ShowMessage('One or more data controls invalid');
    Exit;
  end;

  if (Edit2.Text = '') then
    Edit2.Text := '0';
  E := StrToInt(Edit2.Text);
  if (E > MyCollection.Count-1) OR (E < 0) then
  begin
    ShowMessage('Element value out of range (0..' + IntToStr(MyCollection.Count) + ')');
    Edit2.Text := '0';
    Exit;
  end;

  Data := MyCollection.At(E);
  if Data <> nil then
  begin
    ARecord(Data^) := AR;
    MyCollection.AtPut(E, Data);
    FillListBox;
  end;
end;

procedure TSTDlg.DelBtnClick(Sender: TObject);
var
  AR   : ARecord;
  PN   : Pointer;
begin
  if (NOT CheckControls(AR)) then
  begin
    ShowMessage('One or more data entry fields invalid');
    Exit;
  end;
  PN := MyCollection.Iterate(MatchCollString,True,@AR);
  if (PN <> nil) then
  begin
    MyCollection.Delete(PN);
    FillListBox;
  end else
    ShowMessage('Data not found');
end;

procedure TSTDlg.AtDelBtnClick(Sender: TObject);
var
  E : LongInt;
begin
  if (Edit2.Text = '') then
    E := 0
  else
    E := StrToInt(Edit2.Text);
  if (E > MyCollection.Count-1) OR (E < 0) then
  begin
    ShowMessage('Element value out of range (0..' + IntToStr(MyCollection.Count) + ')');
    Edit2.Text := '0';
    Exit;
  end;
  MyCollection.AtDelete(E);
  FillListBox;
end;

procedure TSTDlg.InsBtnClick(Sender: TObject);
var
  E  : Integer;
  AR : ^ARecord;
begin
  if (Edit2.Text = '') then
    E := 0
  else
    E := StrToInt(Edit2.Text);
  if (E > MyCollection.Count-1) OR (E < 0) then
  begin
    ShowMessage('Element value out of range (0..' + IntToStr(MyCollection.Count) + ')');
    Edit2.Text := '0';
    Exit;
  end;

  GetMem(AR,SizeOf(ARecord));
  if (NOT CheckControls(AR^)) then
  begin
    ShowMessage('One or more data entry fields invalid');
    FreeMem(AR,SizeOf(ARecord));
    Exit;
  end;

  MyCollection.Insert(AR);
  FillListBox;
end;

procedure TSTDlg.LB1DblClick(Sender: TObject);
begin
  MyCollection.AtDelete(LB1.ItemIndex);
  FillListBox;
  Edit2.Text := '0';
end;

procedure TSTDlg.LB1Click(Sender: TObject);
begin
  Edit2.Text := IntToStr(LB1.ItemIndex);
end;

procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if (NOT Assigned(MyCollection)) then
    begin
      UpdateButtons(False);
      MyCollection := TStCollection.Create(100);
      MyCollection.DisposeData := MyDelNodeData;
      MyCollection.LoadData := MyLoadData;
      MyCollection.StoreData := MyStoreData;
    end;

    LB1.Clear;
    MyCollection.Clear;

    SetBusy(True);
    MyCollection.LoadFromFile(OD1.FileName);
    MyCollection.Pack;
    SetBusy(False);

    FillListBox;
    UpdateButtons(True);
  end;
end;


procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
  begin
    SetBusy(True);
    MyCollection.StoreToFile(SD1.FileName);
    SetBusy(False);
  end;
end;



end.
