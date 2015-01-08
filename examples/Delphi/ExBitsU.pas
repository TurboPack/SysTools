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

unit ExBitsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TSTDlg = class(TForm)
    CreateBtn: TButton;
    NumElemsValue: TEdit;
    Label2: TLabel;
    ClearAllBtn: TButton;
    SetAllBtn: TButton;
    InvertAllBtn: TButton;
    Label1: TLabel;
    SetBitBtn: TButton;
    SetBitValue: TEdit;
    ClearBitBtn: TButton;
    ClearBitValue: TEdit;
    IsBitSetBtn: TButton;
    IsBitSetValue: TEdit;
    ControlBitBtn: TButton;
    ControlBitValue: TEdit;
    BitOnCB: TCheckBox;
    ToggleBitBtn: TButton;
    ToggleBitValue: TEdit;
    Msg1: TMemo;
    LoadBtn: TButton;
    SaveBtn: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;
    procedure CreateBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClearAllBtnClick(Sender: TObject);
    procedure SetAllBtnClick(Sender: TObject);
    procedure InvertAllBtnClick(Sender: TObject);
    procedure SetBitBtnClick(Sender: TObject);
    procedure ControlBitBtnClick(Sender: TObject);

    procedure ClearBitBtnClick(Sender: TObject);
    procedure IsBitSetBtnClick(Sender: TObject);
    procedure ToggleBitBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateButtons(BitsOK : Boolean);
    function CheckValue(S : string; var N : longint) : Boolean;
    function GetTFString(N : LongInt) : string;
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

uses
  StConst,
  StBase,
  StBits;

var
  MyBits : TStBits;


procedure TSTDlg.FormCreate(Sender: TObject);
begin
  RegisterClass(TStBits);
  UpdateButtons(False);
end;


procedure TSTDlg.UpdateButtons(BitsOK : Boolean);
begin
  IsBitSetBtn.Enabled   := BitsOK;
  ControlBitBtn.Enabled := BitsOK;
  SetAllBtn.Enabled     := BitsOK;
  InvertAllBtn.Enabled  := BitsOK;
  ClearAllBtn.Enabled   := BitsOK;
  ToggleBitBtn.Enabled  := BitsOK;
  SetBitBtn.Enabled     := BitsOK;
  ClearBitBtn.Enabled   := BitsOK;
  SaveBtn.Enabled       := BitsOK;
end;


procedure TSTDlg.FormActivate(Sender: TObject);
begin
  IsBitSetValue.Text := '-1';
  ToggleBitValue.Text := '-1';
  SetBitValue.Text := '-1';
  ControlBitValue.Text := '-1';
  ClearBitValue.Text := '-1';

  Msg1.Lines.Clear;
  Msg1.Lines.Add('BitSet not created');
end;


procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyBits.Free;
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  MaxBits : longint;
begin
  Msg1.Lines.Clear;

  if (NumElemsValue.Text = '') then
    NumElemsValue.Text := '50';

  MaxBits := StrToInt(NumElemsValue.Text);
  if (MaxBits < 1) OR (MaxBits > 9999) then
  begin
    ShowMessage('Value out of range (1 - 9999)');
    Exit;
  end;

  Msg1.Lines.Clear;

  if Assigned(MyBits) then
    MyBits.Free;

  UpdateButtons(False);
  MyBits := TStBits.Create(MaxBits);

  Label1.Caption := 'In entry fields below, enter a value from 0 to '
                  + IntToStr(MaxBits);
  Label2.Caption := 'Elements in BitSet: ' + IntToStr(MyBits.Max+1);

  IsBitSetValue.Text := '0';
  ToggleBitValue.Text := '0';
  SetBitValue.Text := '0';
  ControlBitValue.Text := '0';
  ClearBitValue.Text := '0';

  Msg1.Lines.Add('BitSet created');
  Msg1.Lines.Add(IntToStr(MyBits.Count));
  UpdateButtons(True);
end;

procedure TSTDlg.ClearAllBtnClick(Sender: TObject);
begin
  Msg1.Lines.Clear;
  MyBits.Clear;
  Msg1.Lines.Add('Bits Cleared');
end;

procedure TSTDlg.SetAllBtnClick(Sender: TObject);
begin
  Msg1.Lines.Clear;
  MyBits.SetBits;
  Msg1.Lines.Add('Bits Set');
end;

procedure TSTDlg.InvertAllBtnClick(Sender: TObject);
begin
  Msg1.Lines.Clear;
  MyBits.InvertBits;
  Msg1.Lines.Add('Bits Inverted');
end;

function TSTDlg.CheckValue(S : String; var N : longint) : Boolean;
begin
  Result := FALSE;
  if (S = '') then
  begin
    ShowMessage('No value entered');
    Exit;
  end;

  N := StrToInt(S);
  if (N < 0) or (N > MyBits.Max) then
  begin
    ShowMessage('Number out of range');
    Exit;
  end;
  Result := TRUE;
end;

function TSTDlg.GetTFString(N : LongInt) : string;
begin
  if MyBits.BitIsSet(N) then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

procedure TSTDlg.SetBitBtnClick(Sender: TObject);
var
  BitNum : longint;
  WasStr,
  NowStr  : string[5];
begin
  if NOT CheckValue(SetBitValue.Text,BitNum) then
    Exit;

  WasStr := GetTFString(BitNum);
  MyBits.SetBit(BitNum);
  NowStr := GetTFString(BitNum);

  Msg1.Lines.Clear;
  Msg1.Lines.Add('Bit was: ' + WasStr);
  Msg1.Lines.Add('Bit is now: ' + NowStr);
end;

procedure TSTDlg.ControlBitBtnClick(Sender: TObject);
var
  BitNum  : longint;
  WasStr,
  NowStr  : string[5];
begin
  if NOT CheckValue(ControlBitValue.Text,BitNum) then
    Exit;

  WasStr := GetTFString(BitNum);
  MyBits.ControlBit(BitNum,BitOnCB.Checked);
  NowStr := GetTFString(BitNum);

  Msg1.Lines.Clear;
  Msg1.Lines.Add('Bit was: ' + WasStr);
  Msg1.Lines.Add('Bit is now: ' + NowStr);
end;

procedure TSTDlg.ClearBitBtnClick(Sender: TObject);
var
  BitNum : longint;
  WasStr,
  NowStr : string;
begin
  if NOT CheckValue(ClearBitValue.Text,BitNum) then
    Exit;

  WasStr := GetTFString(BitNum);
  MyBits.ClearBit(BitNum);
  NowStr := GetTFString(BitNum);

  Msg1.Lines.Clear;
  Msg1.Lines.Add('Bit was: ' + WasStr);
  Msg1.Lines.Add('Bit is now: ' + NowStr);
end;

procedure TSTDlg.IsBitSetBtnClick(Sender: TObject);
var
  BitNum  : longint;
begin
  if NOT CheckValue(IsBitSetValue.Text,BitNum) then
    Exit;

  Msg1.Lines.Clear;
  if (MyBits.BitIsSet(BitNum)) then
    Msg1.Lines.Add('Bit is set')
  else
    Msg1.Lines.Add( 'Bit not set');
end;

procedure TSTDlg.ToggleBitBtnClick(Sender: TObject);
var
  BitNum : longint;
  WasStr,
  NowStr : string;
begin
  if NOT CheckValue(ToggleBitValue.Text,BitNum) then
    Exit;

  WasStr := GetTFString(BitNum);
  MyBits.ToggleBit(BitNum);
  NowStr := GetTFString(BitNum);

  Msg1.Lines.Clear;
  Msg1.Lines.Add('Bit was: ' + WasStr);
  Msg1.Lines.Add('Bit is now: ' + NowStr);
end;


procedure TSTDlg.LoadBtnClick(Sender: TObject);
begin
  if (OD1.Execute) then
  begin
    if (NOT Assigned(MyBits)) then
    begin
      {create a minimum sized bitset - load will resize it}
      MyBits := TStBits.Create(1);

      if NOT (Assigned(MyBits)) then
      begin
        Msg1.Lines.Add('BitSet Create Failed');
        UpdateButtons(False);
        Exit;
      end;
    end;

    MyBits.Clear;
    MyBits.LoadFromFile(OD1.FileName);

    Label1.Caption := 'In entry fields below, enter a value from 0 to '
                    + IntToStr(MyBits.Max);
    Label2.Caption := 'Elements in BitSet: ' + IntToStr(MyBits.Max+1);

    IsBitSetValue.Text := '0';
    ToggleBitValue.Text := '0';
    SetBitValue.Text := '0';
    ControlBitValue.Text := '0';
    ClearBitValue.Text := '0';

    Msg1.Clear;
    Msg1.Lines.Add('BitSet loaded');
    UpdateButtons(True);
  end;
end;

procedure TSTDlg.SaveBtnClick(Sender: TObject);
begin
  if (SD1.Execute) then
  begin
    MyBits.StoreToFile(SD1.FileName);
    Msg1.Clear;
    Msg1.Lines.Add('BitSet saved');
  end;
end;

end.
