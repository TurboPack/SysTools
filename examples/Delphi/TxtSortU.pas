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

unit TxtSortU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons,

  StConst, StBase, StColl, StSort;

const
  MaxStrLen = 1024;

type
  SortException = class(Exception);
  LineBuf = array[0..MaxStrLen-1] of char;

  TSTDlg = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    InFile: TEdit;
    OutFile: TEdit;
    GroupBox2: TGroupBox;
    RevOrder: TCheckBox;
    IgnoreCase: TCheckBox;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    StartPos: TEdit;
    KeyLen: TEdit;
    OkBtn: TBitBtn;
    CloseBtn: TBitBtn;
    GroupBox4: TGroupBox;
    Status: TLabel;
    AbortBtn: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    InputBtn: TSpeedButton;
    OutputBtn: TSpeedButton;
    procedure OkBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);
    procedure InputBtnClick(Sender: TObject);
    procedure OutputBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DoAbort,
    InSort,
    DoRev,
    Ignore  : Boolean;

    SPos,
    KeyL    : Integer;

    LC      : LongInt;

    InF,
    OutF    : TextFile;

    MySort  : TStSorter;

    function ValidateEntryFields : Boolean;
    procedure CleanUp;
  end;


var
  STDlg: TSTDlg;

implementation

{$R *.DFM}

procedure DelNodeData(Data : pointer); far;
 {-procedure to delete data pointer in each node}
begin
  Dispose(Data);
end;


function TFSorter(const S1, S2) : Integer; far;
var
  PX, PY : LineBuf;
begin
  if STDlg.DoRev then begin
    StrCopy(PX, LineBuf(S2));
    StrCopy(PY, LineBuf(S1));
  end else begin
    StrCopy(PX, LineBuf(S1));
    StrCopy(PY, LineBuf(S2));
  end;



  if STDlg.Ignore then begin
    if (StrLIComp(@PX[STDlg.SPos-1], @PY[STDlg.SPos-1], STDlg.KeyL) < 0) then
      Result := -1
    else
      Result := 0;
  end else begin
    if (StrLComp(@PX[STDlg.SPos-1], @PY[STDlg.SPos-1], STDlg.KeyL) < 0) then
      Result := -1
    else
      Result := 0;
  end;
end;

procedure TSTDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if MySort <> nil then
    MySort.Free;
end;

procedure TSTDlg.CloseBtnClick(Sender: TObject);
begin
  if InSort then Exit;
  Close;
end;

function TSTDlg.ValidateEntryFields : Boolean;
var
  Code  : Integer;

begin
  Result := False;

  if NOT FileExists(InFile.Text) then
  begin
    ShowMessage('Input file does not exist');
    Exit;
  end;

  if FileExists(OutFile.Text) then
  begin
    if MessageDlg('Output file exists' + #13 + 'Continue?',
                  mtConfirmation,[mbYes,mbNo],0) = mrNo then
      Exit;
  end;

  if (CompareText(InFile.Text,OutFile.Text) = 0) then
  begin
    ShowMessage('Input and Output file can not be the same');
    Exit;
  end;

  val(StartPos.Text,SPos,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Invalid Start entry');
    Exit;
  end;
  if (SPos < 1) OR (SPos >= MaxStrLen) then
  begin
    ShowMessage('Start out of range');
    Exit;
  end;

  val(KeyLen.Text,KeyL,Code);
  if (Code <> 0) then
  begin
    ShowMessage('Invalid Length entry');
    Exit;
  end;
  if (KeyL < 1) OR (KeyL > MaxStrLen-SPos) then
  begin
    ShowMessage('Key Length out of range');
    Exit;
  end;

  DoRev  := RevOrder.Checked;
  Ignore := IgnoreCase.Checked;

  Result := True;
end;


procedure TSTDlg.CleanUp;
begin
  CloseFile(InF);
  CloseFile(OutF);
  InSort := False;
  DoAbort := True;

  MySort.Free;
  MySort := nil;
end;

procedure TSTDlg.OkBtnClick(Sender: TObject);
var
  PS : LineBuf;
begin
  if NOT ValidateEntryFields then
    Exit;

  AssignFile(InF,InFile.Text);
  Reset(InF);
  AssignFile(OutF,OutFile.Text);
  ReWrite(OutF);

  if MySort <> nil then begin
    MySort.Free;
    MySort := nil;
  end;

  MySort := TStSorter.Create(500000, SizeOf(LineBuf));
  MySort.Compare := TFSorter;

  DoAbort := False;
  InSort := True;
  LC := 0;

  while NOT EOF(InF) do begin
    FillChar(PS, SizeOf(PS), #0);
    Readln(InF, PS);
    Inc(LC);
    Status.Caption := 'Reading/Sorting line: ' + IntToStr(LC);
    MySort.Put(PS);

    if (LC mod 100) = 0 then begin
      Application.ProcessMessages;
      if DoAbort then begin
        CleanUp;
        Status.Caption := 'Sort Aborted';
        Exit;
      end;
    end;
  end;

  Status.Caption := 'Processing';
  Status.Update;
  Application.ProcessMessages;

  if NOT DoAbort then begin
    LC := 0;
    while MySort.Get(PS) do begin
      Inc(LC);
      Status.Caption := 'Writing line: ' + IntToStr(LC);
      Writeln(OutF, PS);

      if (LC mod 100) = 0 then begin
        Application.ProcessMessages;
        if DoAbort then begin
          CleanUp;
          Status.Caption := 'Sort Aborted';
          Exit;
        end;
      end;
    end;
  end;

  if NOT DoAbort then begin
    CleanUp;
    Status.Caption := 'Done';
  end;
end;


procedure TSTDlg.FormActivate(Sender: TObject);
begin
  IgnoreCase.Checked := True;
  RevOrder.Checked := False;
  InFile.Text := '';
  OutFile.Text := '';
  StartPos.Text := '1';
  KeyLen.Text := '20';
  Status.Caption := 'Idle';
end;

procedure TSTDlg.AbortBtnClick(Sender: TObject);
begin
  DoAbort := True;
end;

procedure TSTDlg.InputBtnClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    InFile.Text := OpenDialog1.FileName;
end;

procedure TSTDlg.OutputBtnClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    OutFile.Text := SaveDialog1.FileName;
end;

end.
