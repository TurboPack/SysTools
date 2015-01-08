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

unit ExVarrU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,

  StConst, StBase, StUtils, StVArr;

type
  ARecord = record
    X, Y   : LongInt;
  end;

  TMyVMatrix = class(TStVMatrix)
  protected
    Header : array[0..1023] of char;
  public
    constructor Create(Rows, Cols, ElementSize : Cardinal;
                       CacheRows : Integer;
                       const DataFile : string; OpenMode : Word); override;
    function HeaderSize : LongInt; override;
    procedure ReadHeader; override;
    procedure WriteHeader; override;
  end;

  TSTDlg = class(TForm)
    ArrayLB: TListBox;
    CreateBtn: TButton;
    Label6: TLabel;
    VMRow: TEdit;
    VMCol: TEdit;
    ClearBtn: TButton;
    FillBtn: TButton;
    PutBtn: TButton;
    PutRowBtn: TButton;
    GetBtn: TButton;
    GetRowBtn: TButton;
    SortBtn: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CreateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure FillBtnClick(Sender: TObject);
    procedure PutBtnClick(Sender: TObject);
    procedure GetBtnClick(Sender: TObject);
    procedure PutRowBtnClick(Sender: TObject);
    procedure GetRowBtnClick(Sender: TObject);
    procedure SortBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetBusy(B : Boolean);
    procedure FillListBox;
    procedure FillControls;
    function GetControls(var AR : ARecord) : Boolean;
    function ValidateRowCol(var R, C : LongInt) : Boolean;
    procedure UpdateButtons(AOK : Boolean);
  end;

var
  STDlg: TSTDlg;
  ARec : ARecord;

implementation

{$R *.DFM}

{ File and Share modes

  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;

  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;
}

type
  S10 = string[10];

const
  MaxRows = 1000;
  MaxCols = 10;
  RowsCached = 10;
  FN = 'MyCache.DAT';

var
  MyVMatrix : TMyVMatrix;
  RowArray : array[1..MaxCols] of ARecord;


function MyArraySort(const E1, E2) : Integer; far;
var
  R1 : ARecord absolute E1;
  R2 : ARecord absolute E2;
begin
  Result := R1.X-R2.X;
  if Result = 0 then
    Result := R1.Y-R2.Y;
end;


{ ==========   Descendant TMyVMatrix methods =================}

constructor TMyVMatrix.Create(Rows, Cols, ElementSize : Cardinal;
                   CacheRows : Integer;
                   const DataFile : string; OpenMode : Word);
begin
  strcopy(Header,'DataFile1.  Contains data stored in a 2D virtual array');
  inherited Create(Rows, Cols, ElementSize, CacheRows, DataFile, OpenMode);
end;

procedure TMyVMatrix.WriteHeader;
begin
  FileWrite(vmDataF,Header,SizeOf(Header));
end;

function TMyVMatrix.HeaderSize : LongInt;
begin
  Result := SizeOf(Header);
end;

procedure TMyVMatrix.ReadHeader;
begin
  FillChar(Header,SizeOf(Header),#0);
  FileRead(vmDataF,Header,SizeOf(Header));
end;


{ =================   Form methods  ==========================}


procedure TSTDlg.FormCreate(Sender: TObject);
begin
  UpdateButtons(False);
end;

procedure TSTDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  MyVMatrix.Free;
end;

procedure TSTDlg.SetBusy(B : Boolean);
begin
  if B then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;

procedure TSTDlg.UpdateButtons(AOK : Boolean);
begin
  ClearBtn.Enabled  := AOK;
  FillBtn.Enabled   := AOK;
  SortBtn.Enabled   := AOK;
  PutBtn.Enabled    := AOK;
  PutRowBtn.Enabled := AOK;
  GetBtn.Enabled    := AOK;
  GetRowBtn.Enabled := AOK;
end;


procedure TSTDlg.FillListBox;
var
  row, col : LongInt;

begin
  ArrayLB.Clear;

  ArrayLB.Perform(WM_SETREDRAW,0,0);
  SetBusy(True);
  for row := 0 to MaxRows-1 do
  begin
    for col := 0 to MaxCols-1 do
    begin
      MyVMatrix.Get(Row,Col,ARec);
      ArrayLB.Items.Add(IntToStr(row) + ',' +
                        IntToStr(col) + ':   X = ' +
                        IntToStr(ARec.X) + '  Y = ' +
                        IntToStr(ARec.Y));
    end;
  end;
  ArrayLB.Perform(WM_SETREDRAW,1,0);
  ArrayLB.Update;
end;


procedure TSTDlg.FillControls;
begin
  with ARec do
  begin
    Edit1.Text := IntToStr(X);
    Edit2.Text := IntToStr(Y);
  end;
end;


function TSTDlg.GetControls(var AR : ARecord) : Boolean;
var
  Code : Integer;
  IV   : LongInt;
begin
  Result := False;
  if (Edit1.Text = '') OR (Edit2.Text = '') then
  begin
    ShowMessage('One or more blank fields');
    Exit;
  end;

  FillChar(AR,SizeOf(AR),#0);
  Val(Edit1.Text,IV,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Illegal entry for X');
    Exit;
  end else
    AR.X := IV;

  Val(Edit2.Text,IV,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Illegal entry for Y');
    Exit;
  end else
    AR.Y := IV;
  Result := True;
end;


function TSTDlg.ValidateRowCol(var R,C : LongInt) : Boolean;
var
  Code  : Integer;
  Value : LongInt;

begin
  Result := False;

  if (VMRow.Text = '') then
    VMRow.Text := '0';
  if (VMCol.Text = '') then
    VMCol.Text := '0';

  Val(VMRow.Text,Value,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Invalid row entry');
    Exit;
  end else
  begin
    if (Value < 0) or (Value > MaxRows-1) then
    begin
      ShowMessage('Row value out of range');
      Exit;
    end else
      R := Value;
  end;

  Val(VMCol.Text,Value,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Invalid Col entry');
    Exit;
  end else
  begin
    if (Value < 0) or (Value > MaxCols-1) then
    begin
      ShowMessage('Col value out of range');
      Exit;
    end else
      C := Value;
  end;

  Result := True;
end;

procedure TSTDlg.CreateBtnClick(Sender: TObject);
var
  row,
  col    : LongInt;
begin
  ArrayLB.Clear;

  if (MyVMatrix <> nil) then
    MyVMatrix.Free;

  MyVMatrix := TMyVMatrix.Create(MaxRows,MaxCols,sizeof(ARecord),RowsCached,
                                 FN,fmOpenReadWrite);
  if (NOT Assigned(MyVMatrix)) then
  begin
    ShowMessage('Failed to create Matrix');
    UpdateButtons(False);
    Exit;
  end;

  SetBusy(True);
  Randomize;
  for row := 0 to MaxRows-1 do
  begin
    for col := 0 to MaxCols-1 do
    begin
      with ARec do
      begin
        X := Random(1000);
        Y := Random(1000);
        MyVMatrix.Put(Row,Col,ARec);
      end;
    end;
  end;
  FillListBox;

  VMRow.Text := '0';
  VMCol.Text := '0';
  MyVMatrix.Get(0,0,ARec);

  FillControls;
  UpdateButtons(True);

  SetBusy(False);
end;

procedure TSTDlg.ClearBtnClick(Sender: TObject);
begin
  MyVMatrix.Clear;
  ArrayLB.Clear;

  VMRow.Text := '0';
  VMCol.Text := '0';
  MyVMatrix.Get(0,0,ARec);

  FillControls;
end;

procedure TSTDlg.FillBtnClick(Sender: TObject);
begin
  if NOT GetControls(ARec) then
    Exit;
  MyVMatrix.Fill(ARec);

  FillListBox;

  VMRow.Text := '0';
  VMCol.Text := '0';

  MyVMatrix.Get(0, 0, ARec);
  FillControls;
  SetBusy(False);
end;

procedure TSTDlg.PutBtnClick(Sender: TObject);
var
  Code,
  Row,
  Col   : LongInt;

begin
  if NOT GetControls(ARec) then
    Exit;
  if NOT ValidateRowCol(Row,Col) then
    Exit;

  MyVMatrix.Put(Row,Col,ARec);

  Code := (Row * MaxRows) + Col;
  ArrayLB.Items[Code] := IntToStr(row) + ',' +
                         IntToStr(col) + ':   X = ' +
                         IntToStr(ARec.X) + '  Y = ' +
                         IntToStr(ARec.Y);

  MyVMatrix.Get(Row, Col, ARec);
  FillControls;
end;

procedure TSTDlg.GetBtnClick(Sender: TObject);
var
  row,
  col   : LongInt;
begin
  if NOT ValidateRowCol(Row,Col) then
    Exit;
  MyVMatrix.Get(Row,Col,ARec);
  FillControls;
end;

procedure TSTDlg.PutRowBtnClick(Sender: TObject);
var
  Code   : Integer;
  row,
  step,
  Value  : LongInt;

begin
  if NOT GetControls(ARec) then
    Exit;
  if (VMRow.Text = '') then
    VMRow.Text := '0';

  Val(VMRow.Text,Value,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Invalid Row Entry');
    Exit;
  end else
  begin
    if (Value < 0) OR (Value >= MaxRows) then
    begin
      ShowMessage('Row out of range');
      Exit;
    end else
      Row := Value;
  end;

  FillStruct(RowArray,MaxCols,ARec,SizeOf(ARec));
  MyVMatrix.PutRow(Row,RowArray);

  ArrayLB.Clear;
  ArrayLB.Perform(WM_SETREDRAW,0,0);

  for step := 1 to MaxCols do
    ArrayLB.Items.Add(IntToStr(row) + ',' +
                      IntToStr(step) + ':   X = ' +
                      IntToStr(ARec.X) + '  Y = ' +
                      IntToStr(ARec.Y));

  ArrayLB.Perform(WM_SETREDRAW,1,0);
  ArrayLB.Update;

  MyVMatrix.Get(Row, 0, ARec);
  FillControls;

  SetBusy(False);
end;

procedure TSTDlg.GetRowBtnClick(Sender: TObject);
var
  Code    : Integer;
  Row,
  step,
  Value   : LongInt;

begin
  if (VMRow.Text = '') then
    VMRow.Text := '0';

  Val(VMRow.Text,Value,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Invalid Row Entry');
    Exit;
  end else
  begin
    if (Value < 0) OR (Value >= MaxRows) then
    begin
      ShowMessage('Row out of range');
      Exit;
    end else
      Row := Value;
  end;
  FillChar(ARec,SizeOf(ARec),#0);
  FillStruct(RowArray,MaxCols,ARec,SizeOf(ARec));
  MyVMatrix.GetRow(Row,RowArray);

  ArrayLB.Clear;
  ArrayLB.Perform(WM_SETREDRAW,0,0);

  for step := 1 to MaxCols do
    ArrayLB.Items.Add(IntToStr(row) + ',' +
                      IntToStr(step) + ':   X = ' +
                      IntToStr(ARec.X) + '  Y = ' +
                      IntToStr(ARec.Y));

  MyVMatrix.Get(Row, 0, ARec);
  FillControls;

  ArrayLB.Perform(WM_SETREDRAW,1,0);
  ArrayLB.Update;
end;

procedure TSTDlg.SortBtnClick(Sender: TObject);
var
  row,
  col    : LongInt;
begin
  SetBusy(True);
  MyVMatrix.SortRows(0,MyArraySort);

  ArrayLB.Clear;
  col := 0;
  ArrayLB.Perform(WM_SETREDRAW,0,0);
  for row := 0 to MaxRows-1 do
  begin
    MyVMatrix.Get(row,col,ARec);
    ArrayLB.Items.Add(IntToStr(row) + ',' +
                      IntToStr(col) + ':  X = ' +
                      IntToStr(ARec.X) + '  Y = ' +
                      IntToStr(ARec.Y));
  end;
  ArrayLB.Perform(WM_SETREDRAW,1,0);
  ArrayLB.Update;

  SetBusy(False);
end;


end.
