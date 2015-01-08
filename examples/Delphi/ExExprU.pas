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

unit ExExprU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Buttons, StdCtrls,

  StBase, StConst, StExpr;

type
  TStDlg = class(TForm)
    Label1: TLabel;
    ResultEdit: TEdit;
    Label2: TLabel;
    EvaluateBtn: TBitBtn;
    ClearBtn: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    ExprEdit: TStExpressionEdit;
    DivBtn: TBitBtn;
    MulBtn: TBitBtn;
    MinusBtn: TBitBtn;
    PlusBtn: TBitBtn;
    EBtn: TBitBtn;
    Nr3Btn: TBitBtn;
    Nr6Btn: TBitBtn;
    Nr9Btn: TBitBtn;
    DotBtn: TBitBtn;
    Nr2Btn: TBitBtn;
    Nr5Btn: TBitBtn;
    Nr8Btn: TBitBtn;
    Nr0Btn: TBitBtn;
    Nr1Btn: TBitBtn;
    Nr4Btn: TBitBtn;
    Nr7Btn: TBitBtn;
    SqrtBtn: TBitBtn;
    PiBtn: TBitBtn;
    ExpBtn: TBitBtn;
    PowerBtn: TBitBtn;
    CommaBtn: TBitBtn;
    CosBtn: TBitBtn;
    LnBtn: TBitBtn;
    SqrBtn: TBitBtn;
    RParBtn: TBitBtn;
    ArctanBtn: TBitBtn;
    IntBtn: TBitBtn;
    SinBtn: TBitBtn;
    LParBtn: TBitBtn;
    AbsBtn: TBitBtn;
    FracBtn: TBitBtn;
    RoundBtn: TBitBtn;
    BSBtn: TBitBtn;
    procedure LParBtnClick(Sender: TObject);
    procedure RParBtnClick(Sender: TObject);
    procedure CommaBtnClick(Sender: TObject);
    procedure PowerBtnClick(Sender: TObject);
    procedure AbsBtnClick(Sender: TObject);
    procedure ArctanBtnClick(Sender: TObject);
    procedure CosBtnClick(Sender: TObject);
    procedure ExpBtnClick(Sender: TObject);
    procedure FracBtnClick(Sender: TObject);
    procedure IntBtnClick(Sender: TObject);
    procedure LnBtnClick(Sender: TObject);
    procedure PiBtnClick(Sender: TObject);
    procedure RoundBtnClick(Sender: TObject);
    procedure SinBtnClick(Sender: TObject);
    procedure SqrBtnClick(Sender: TObject);
    procedure SqrtBtnClick(Sender: TObject);
    procedure Nr0BtnClick(Sender: TObject);
    procedure Nr1BtnClick(Sender: TObject);
    procedure Nr2BtnClick(Sender: TObject);
    procedure Nr3BtnClick(Sender: TObject);
    procedure Nr4BtnClick(Sender: TObject);
    procedure Nr5BtnClick(Sender: TObject);
    procedure Nr6BtnClick(Sender: TObject);
    procedure Nr7BtnClick(Sender: TObject);
    procedure Nr8BtnClick(Sender: TObject);
    procedure Nr9BtnClick(Sender: TObject);
    procedure DotBtnClick(Sender: TObject);
    procedure EBtnClick(Sender: TObject);
    procedure PlusBtnClick(Sender: TObject);
    procedure MinusBtnClick(Sender: TObject);
    procedure MulBtnClick(Sender: TObject);
    procedure DivBtnClick(Sender: TObject);
    procedure EvaluateBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ExprEditError(Sender: TObject; ErrorNumber: Longint;
      const ErrorStr: String);
    procedure BSBtnClick(Sender: TObject);
  private
    procedure AddExpr(Add : string; Parens : Boolean);
  end;

var
  StDlg: TStDlg;

implementation

{$R *.DFM}

procedure TStDlg.AddExpr(add : string; parens : boolean);
var
  position, sellen : integer;
  temp : string;
begin
  position := ExprEdit.SelStart+1;
  sellen := ExprEdit.SelLength;
  temp := ExprEdit.Text;

  if (parens) then begin
    add := add+'(';
    if (sellen > 0) then
      {surround the selection with the parentheses}
      insert(')', temp, position+sellen)
    else
      add := add+')';
  end;
  insert(add, temp, position);

  ExprEdit.Text := temp;
  ExprEdit.SetFocus;

  if (parens) then begin
    if (sellen > 0) then
      {position after the add}
      ExprEdit.SelStart := position+sellen+length(add)
    else
      {position before last parenthesis}
      ExprEdit.SelStart := position+length(add)-2;
  end else
    {position after the add}
    ExprEdit.SelStart := position+length(add)-1;
  ExprEdit.SelLength := 0;
end;

procedure TStDlg.LParBtnClick(Sender: TObject);
begin
  AddExpr('(', False);
end;

procedure TStDlg.RParBtnClick(Sender: TObject);
begin
  AddExpr(')', False);
end;

procedure TStDlg.CommaBtnClick(Sender: TObject);
begin
  AddExpr(ListSeparator, False);
end;

procedure TStDlg.PowerBtnClick(Sender: TObject);
begin
  AddExpr('^', False);
end;

procedure TStDlg.AbsBtnClick(Sender: TObject);
begin
  AddExpr('abs', True);
end;

procedure TStDlg.ArctanBtnClick(Sender: TObject);
begin
  AddExpr('arctan', True);
end;

procedure TStDlg.CosBtnClick(Sender: TObject);
begin
  AddExpr('cos', True);
end;

procedure TStDlg.ExpBtnClick(Sender: TObject);
begin
  AddExpr('exp', True);
end;

procedure TStDlg.FracBtnClick(Sender: TObject);
begin
  AddExpr('frac', True);
end;

procedure TStDlg.IntBtnClick(Sender: TObject);
begin
  AddExpr('int', True);
end;

procedure TStDlg.LnBtnClick(Sender: TObject);
begin
  AddExpr('ln', True);
end;

procedure TStDlg.PiBtnClick(Sender: TObject);
begin
  AddExpr('pi', False);
end;

procedure TStDlg.RoundBtnClick(Sender: TObject);
begin
  AddExpr('round', True);
end;

procedure TStDlg.SinBtnClick(Sender: TObject);
begin
  AddExpr('sin', True);
end;

procedure TStDlg.SqrBtnClick(Sender: TObject);
begin
  AddExpr('sqr', True);
end;

procedure TStDlg.SqrtBtnClick(Sender: TObject);
begin
  AddExpr('sqrt', True);
end;

procedure TStDlg.Nr0BtnClick(Sender: TObject);
begin
  AddExpr('0', False);
end;

procedure TStDlg.Nr1BtnClick(Sender: TObject);
begin
  AddExpr('1', False);
end;

procedure TStDlg.Nr2BtnClick(Sender: TObject);
begin
  AddExpr('2', False);
end;

procedure TStDlg.Nr3BtnClick(Sender: TObject);
begin
  AddExpr('3', False);
end;

procedure TStDlg.Nr4BtnClick(Sender: TObject);
begin
  AddExpr('4', False);
end;

procedure TStDlg.Nr5BtnClick(Sender: TObject);
begin
  AddExpr('5', False);
end;

procedure TStDlg.Nr6BtnClick(Sender: TObject);
begin
  AddExpr('6', False);
end;

procedure TStDlg.Nr7BtnClick(Sender: TObject);
begin
  AddExpr('7', False);
end;

procedure TStDlg.Nr8BtnClick(Sender: TObject);
begin
  AddExpr('8', False);
end;

procedure TStDlg.Nr9BtnClick(Sender: TObject);
begin
  AddExpr('9', False);
end;

procedure TStDlg.DotBtnClick(Sender: TObject);
begin
  AddExpr(DecimalSeparator, False);
end;

procedure TStDlg.EBtnClick(Sender: TObject);
begin
  AddExpr('E', False);
end;

procedure TStDlg.PlusBtnClick(Sender: TObject);
begin
  AddExpr('+', False);
end;

procedure TStDlg.MinusBtnClick(Sender: TObject);
begin
  AddExpr('-', False);
end;

procedure TStDlg.MulBtnClick(Sender: TObject);
begin
  AddExpr('*', False);
end;

procedure TStDlg.DivBtnClick(Sender: TObject);
begin
  AddExpr('/', False);
end;

procedure TStDlg.EvaluateBtnClick(Sender: TObject);
var
  res : double;
begin
  res := ExprEdit.Evaluate;
  if ExprEdit.Expr.LastError = 0 then
    ResultEdit.Text := FloatToStr(res);
  ExprEdit.SetFocus;
end;

procedure TStDlg.ClearBtnClick(Sender: TObject);
begin
  ExprEdit.Text := '';
  ResultEdit.Text := '';
  ExprEdit.SetFocus;
end;

procedure TStDlg.ExprEditError(Sender: TObject; ErrorNumber: Longint;
  const ErrorStr: String);
begin
  ResultEdit.Text := 'Error ' + IntToStr(ErrorNumber) + ': ' + ErrorStr;
end;

procedure TStDlg.BSBtnClick(Sender: TObject);
begin
  ExprEdit.Perform(WM_CHAR, VK_BACK, 0);
end;

end.
