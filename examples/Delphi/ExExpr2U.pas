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

unit ExExpr2U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  StExpr, StBase, StConst;

type
  TForm1 = class(TForm)
    Button1: TButton;
    edVar1: TEdit;
    edVar2: TEdit;
    edFunc1Param: TEdit;
    edMeth1Param: TEdit;
    edFunc0Param: TEdit;
    edMeth0Param: TEdit;
    edFunc2Param: TEdit;
    edFunc3Param: TEdit;
    edMeth2Param: TEdit;
    edMeth3Param: TEdit;
    lblVar1: TLabel;
    lblVar2: TLabel;
    lblFunc1: TLabel;
    lblMeth1: TLabel;
    lblFunc0: TLabel;
    lblMeth0: TLabel;
    lblMeth2: TLabel;
    lblFunc2: TLabel;
    lblFunc3: TLabel;
    lblMeth3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    StExpressionEdit1: TStExpressionEdit;
    ListBox1: TListBox;
    Exp: TStExpression;
    Label9: TLabel;
    Label10: TLabel;
    lblError: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure StExpressionEdit1AddIdentifier(Sender: TObject);
    procedure StExpressionEdit1Error(Sender: TObject; ErrorNumber: Longint;
      const ErrorStr: string);
  private
    { Private declarations }
  public
    { Public declarations }
    Ref : TStFloat;
    function MyMethod0Param : TStFloat;
    function MyMethod1Param(Value : TStFloat) : TStFloat;
    function MyMethod2Param(Value1, Value2 : TStFloat) : TStFloat;
    function MyMethod3Param(Value1, Value2, Value3 : TStFloat) : TStFloat;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function MyFunc0Param : TStFloat; far;
begin
  Result := 10;
end;

function MyFunc1Param(Value : TStFloat) : TStFloat; far;
begin
  Result := Value * 10;
end;

function MyFunc2Param(Value1, Value2 : TStFloat) : TStFloat; far;
begin
  Result := Value1 + Value2;
end;

function MyFunc3Param(Value1, Value2, Value3 : TStFloat) : TStFloat; far;
begin
  Result := Value1 + Value2 + Value3;
end;


function TForm1.MyMethod0Param : TStFloat;
begin
  Result := 10;
end;

function TForm1.MyMethod1Param(Value : TStFloat) : TStFloat;
begin
  Result := Value * 10;
end;

function TForm1.MyMethod2Param(Value1, Value2 : TStFloat) : TStFloat;
begin
  Result := Value1 + Value2;
end;

function TForm1.MyMethod3Param(Value1, Value2, Value3 : TStFloat) : TStFloat;
begin
  Result := Value1 + Value2 + Value3;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Exp.ClearIdentifiers;
  Exp.AddInternalFunctions;

  {add variables and functions}
  Exp.AddConstant('X', 5.5);
  Exp.AddVariable('Ref', @Ref);
  Exp.AddFunction0Param('Func0', MyFunc0Param);
  Exp.AddFunction1Param('Func1', MyFunc1Param);
  Exp.AddFunction2Param('Func2', MyFunc2Param);
  Exp.AddFunction3Param('Func3', MyFunc3Param);
  Exp.AddMethod0Param('Meth0', MyMethod0Param);
  Exp.AddMethod1Param('Meth1', MyMethod1Param);
  Exp.AddMethod2Param('Meth2', MyMethod2Param);
  Exp.AddMethod3Param('Meth3', MyMethod3Param);

  Ref := 6.6;
  Exp.Expression := 'X * 2';
  lblVar1.Caption := Exp.Expression;
  edVar1.Text := Exp.AsString;

  Ref := 7.6;
  Exp.Expression := '(X * 2) + Ref';
  lblVar2.Caption := Exp.Expression;
  edVar2.Text := Exp.AsString;

  Exp.AddFunction0Param('Func0', MyFunc0Param);
  Exp.Expression := 'Func0';
  lblFunc0.Caption := Exp.Expression;
  edFunc0Param.Text := Exp.AsString;

  Exp.AddFunction1Param('Func1', MyFunc1Param);
  Exp.Expression := 'Func1(50)';
  lblFunc1.Caption := Exp.Expression;
  edFunc1Param.Text := Exp.AsString;

  Exp.AddFunction2Param('Func2', MyFunc2Param);
  Exp.Expression := 'Func2(50' + ListSeparator + ' 2)';
  lblFunc2.Caption := Exp.Expression;
  edFunc2Param.Text := Exp.AsString;

  Exp.AddFunction3Param('Func3', MyFunc3Param);
  Exp.Expression := 'Func3(50' + ListSeparator + ' 2' + ListSeparator + ' 30)';
  lblFunc3.Caption := Exp.Expression;
  edFunc3Param.Text := Exp.AsString;

  Exp.AddMethod0Param('Meth0', MyMethod0Param);
  Exp.Expression := 'Meth0';
  lblMeth0.Caption := Exp.Expression;
  edMeth0Param.Text := Exp.AsString;

  Exp.AddMethod1Param('Meth1', MyMethod1Param);
  Exp.Expression := 'Meth1(50)';
  lblMeth1.Caption := Exp.Expression;
  edMeth1Param.Text := Exp.AsString;

  Exp.AddMethod2Param('Meth2', MyMethod2Param);
  Exp.Expression := 'Meth2(50' + ListSeparator + ' 2)';
  lblMeth2.Caption := Exp.Expression;
  edMeth2Param.Text := Exp.AsString;

  Exp.AddMethod3Param('Meth3', MyMethod3Param);
  Exp.Expression := 'Meth3(50' + ListSeparator + ' 2' + ListSeparator + ' 30)';
  lblMeth3.Caption := Exp.Expression;
  edMeth3Param.Text := Exp.AsString;
end;

procedure TForm1.StExpressionEdit1AddIdentifier(Sender: TObject);
begin
  StExpressionEdit1.Expr.GetIdentList(ListBox1.Items);
end;

procedure TForm1.StExpressionEdit1Error(Sender: TObject;
  ErrorNumber: Longint; const ErrorStr: string);
begin
  if ErrorNumber > 0 then
    lblError.Caption := ErrorStr
  else
    lblError.Caption := '';
end;

end.
