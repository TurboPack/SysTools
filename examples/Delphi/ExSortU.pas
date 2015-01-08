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

unit ExSortU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  StConst, StBase, StSort;

type
  SortException = class(Exception);

  TSTDlg = class(TForm)
    LB1: TListBox;
    LB2: TListBox;
    NewBtn: TButton;
    SorterBtn: TButton;
    Btn4: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure Btn4Click(Sender: TObject);
    procedure SorterBtnClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DidGet : Boolean;
    MaxElems : Integer;
    ISort  : TStSorter;
    procedure DoRandomStrings;
  end;

var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

type
  S15 = string[15];


function MyCompare(const E1, E2) : Integer; far;
begin
  Result := CompareText(S15(E1),S15(E2));
end;

procedure TSTDlg.FormActivate(Sender: TObject);
var
  OHTU : LongInt;
begin
  MaxElems := 1000;
  Edit1.Text := IntToStr(MaxElems);
  DoRandomStrings;
  OHTU := OptimumHeapToUse(SizeOf(S15),MaxElems);
  ISort := TStSorter.Create(OHTU,SizeOf(S15));
  ISort.Compare := MyCompare;
  DidGet := False;
end;

procedure TSTDlg.Btn4Click(Sender: TObject);
begin
  ISort.Free;
  Close;
end;

procedure TSTDlg.DoRandomStrings;
var
  step, I : Integer;
  AStr : S15;
begin
  LB1.Clear;
  LB1.Perform(WM_SETREDRAW,0,0);
  Randomize;
  for step := 1 to MaxElems do
  begin
    AStr[0] := chr(15);
    for I := 1 to 15 do
      AStr[I] := Chr(Random(26) + Ord('A'));
    LB1.Items.Add(AStr);
  end;
  LB1.Perform(WM_SETREDRAW,1,0);
  LB1.Update;
end;

procedure TSTDlg.SorterBtnClick(Sender: TObject);
var
  I   : integer;
  S   : S15;
begin
  if DidGet then
    ISort.Reset;
  Screen.Cursor := crHourGlass;
  if LB1.Items.Count > 0 then
  begin
    for I := 0 to LB1.Items.Count-1 do
    begin
      S := LB1.Items[I];
      ISort.Put(S);
    end;
  end;
  LB2.Clear;
  LB2.Perform(WM_SETREDRAW,0,0);
  while (ISort.Get(S)) do
    LB2.Items.Add(S);
  LB2.Perform(WM_SETREDRAW,1,0);
  LB2.Update;
  DidGet := True;
  Screen.Cursor := crDefault;
end;

procedure TSTDlg.NewBtnClick(Sender: TObject);
var
  Code : Integer;
begin
  Val(Edit1.Text,MaxElems,Code);
  if (Code <> 0) OR (MaxElems = 0) OR (MaxElems > 5000) then
  begin
    ShowMessage('Invalid entry or value out of range (1..5000)');
    Exit;
  end;
  LB2.Clear;
  DoRandomStrings;
end;


end.
