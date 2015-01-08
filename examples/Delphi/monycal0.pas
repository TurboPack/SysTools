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

unit monycal0;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, ClipBrd,

  StStrL, StDecMth, StMoney;

const
  DefaultCurrency = 'USD';
  BaseFormCaption = 'Money Calculator';

type
  MoneyCharSet = set of Char;
  MoneyOperSet = set of Char;

type
  TMoneyCalcDlg = class(TForm)
    GroupBox1: TGroupBox;
    ZeroBtn: TBitBtn;
    DecKey: TBitBtn;
    ThreeKey: TBitBtn;
    OneKey: TBitBtn;
    TwoKey: TBitBtn;
    SixKey: TBitBtn;
    FourKey: TBitBtn;
    FiveKey: TBitBtn;
    NineKey: TBitBtn;
    SevenKey: TBitBtn;
    EightKey: TBitBtn;
    AddBtn: TBitBtn;
    SubBtn: TBitBtn;
    MulBtn: TBitBtn;
    DivBtn: TBitBtn;
    PlusMinusBtn: TBitBtn;
    ClearBtn: TBitBtn;
    EqualBtn: TBitBtn;
    ClearEntryBtn: TBitBtn;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ConvertBtn: TBitBtn;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Memo1: TMemo;
    BSBtn: TBitBtn;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    procedure NumBtnClick(Sender: TObject);
    procedure DecKeyClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ClearEntryBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure SubBtnClick(Sender: TObject);
    procedure MulBtnClick(Sender: TObject);
    procedure DivBtnClick(Sender: TObject);
    procedure PlusMinusBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure EqualBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BSBtnClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
  private
    procedure UpdateConversionCombo;
    procedure UpdateCurrencyCombo;
    procedure UpdateFormCaption;
    procedure ShowExchangeData(const src, trg: string);
    procedure ShowCurrencyData(const Name : string);
    { Private declarations }
  public
    MoneyChar   : MoneyCharSet;
    MoneyOper   : MoneyOperSet;
    PendOp    : Char;
    DFHold    : Integer;
    XBuffer   : string[20];
    ClearOnNext, Converting : Boolean;
    BaseCurrency : string;

    Currencies  : TStCurrencyList;
    Conversions : TStExchangeRateList;

    procedure SendKeyPress(Sender : TObject; C : Char);
    procedure DoRateUpdate(Sender: TObject; NewRate: TStDecimal;
      var NewDate: TDateTime);
    { Public declarations }
  end;

var
  MoneyCalcDlg: TMoneyCalcDlg;

implementation

{$R *.DFM}

procedure TMoneyCalcDlg.UpdateFormCaption;
begin
  if BaseCurrency <> '' then
    Caption := BaseFormCaption + '-' + BaseCurrency
  else
    Caption := BaseFormCaption;
end;

procedure TMoneyCalcDlg.FormCreate(Sender: TObject);
begin
  MoneyChar := ['0'..'9', SysUtils.DecimalSeparator, '~'];
  MoneyOper := ['+', '-', '/', '*'];
  DecKey.Caption := SysUtils.DecimalSeparator;
  Memo1.Lines[0] := '0';

  PendOp := #0;
  DFHold := 0;
  XBuffer := '0';
  ClearOnNext := False;

  Currencies := TStCurrencyList.Create;
  Currencies.LoadFromFile('..\..\STCCY.DAT');
  UpdateCurrencyCombo;
  ComboBox1.Text := DefaultCurrency;
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(ComboBox1.Text);
  BaseCurrency := ComboBox1.Text;
  ShowCurrencyData(ComboBox1.Text);

  Conversions := TStExchangeRateList.Create;
  Conversions.LoadFromFile('..\..\STCCYCNV.DAT');
  UpdateConversionCombo;

  UpdateFormCaption;
end;

procedure TMoneyCalcDlg.FormDestroy(Sender: TObject);
begin
  Currencies.Free;
  Conversions.Free;
end;

procedure TMoneyCalcDlg.SendKeyPress(Sender : TObject; C : Char);
var
  KP : Char;
begin
  KP := C;
  FormKeyPress(Sender,KP);
end;

procedure TMoneyCalcDlg.NumBtnClick(Sender: TObject);
var
  C : Char;
begin
  C := IntToStr((Sender as TBitBtn).Tag)[1];
  SendKeyPress(Sender, C);
end;

procedure TMoneyCalcDlg.DecKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender, SysUtils.DecimalSeparator);
end;

procedure TMoneyCalcDlg.ClearBtnClick(Sender: TObject);
begin
  XBuffer := '0.';
  Memo1.Lines[0] := '0.';
  PendOp := #0;
  ClearOnNext := True;
end;

procedure TMoneyCalcDlg.ClearEntryBtnClick(Sender: TObject);
begin
  Memo1.Lines[0] := '0.';
  ClearOnNext := True;
end;

procedure TMoneyCalcDlg.AddBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'+');
end;

procedure TMoneyCalcDlg.SubBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'-');
end;

procedure TMoneyCalcDlg.MulBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'*');

end;

procedure TMoneyCalcDlg.DivBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'/');
end;

procedure TMoneyCalcDlg.PlusMinusBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'~');
end;

procedure TMoneyCalcDlg.FormKeyPress(Sender: TObject; var Key: Char);
var
  HldOp : Char;
  L     : Integer;
  Money1  : TStMoney;
  S     : string[21];
begin
  Money1 := TStMoney.Create;

  if Memo1.Lines[0] = '0' then
    Memo1.Lines[0] := '';

  try

    if Key = #13 then begin
      if XBuffer = '0' then begin
        XBuffer :=   Memo1.Lines[0];
      end
      else begin
        EqualBtnClick(Sender);
        XBuffer := '0';
      end;
      Key := #0;
      ClearOnNext := True;
    end;

    if Key in MoneyChar then begin
      if (Length(Memo1.Lines[0]) = 0) and (Key = SysUtils.DecimalSeparator) then
        Memo1.Lines[0] := '0';
      if (Key = '~') then begin
        S := Memo1.Lines[0];

        if (S[1] <> '-') then
          Insert('-',S,1)
        else
          Delete(S,1,1);
        Memo1.Lines[0] := S;
        Money1.Amount.AsString := S;
        Key := #0;
      end else begin
        if ClearOnNext then begin
          Memo1.Lines[0] := '';
          ClearOnNext := False;
        end;
      end;
    end;

    if Key in MoneyOper then begin
      if not (Key in ['s', 'e', 'l']) then begin
        if Memo1.Lines[0] = '' then
          Memo1.Lines[0] := '0';
        if (XBuffer <> '0') then
          EqualBtnClick(Sender);
        XBuffer := Memo1.Lines[0];
        Money1.Amount.AsString := XBuffer;
        PendOp := Key;
        Key := #0;
        ClearOnNext := True;
      end else begin
        HldOp := PendOp;
        PendOp := Key;
        EqualBtnClick(Sender);
        PendOp := HldOp;
        Key := #0;
      end;
    end;

    if (Key in MoneyChar) then begin
      S := Memo1.Lines[0];
      L := Length(S);
      if (L < Memo1.MaxLength) then begin
        Memo1.Lines[0] := S + Key;
      end;

      Key := #0
    end;

    Memo1.SetFocus;
    Memo1.SelStart := Length(Memo1.Lines[0]);
    Memo1.SelLength := 0;

  finally
    Money1.Free;
  end;
end;

procedure TMoneyCalcDlg.EqualBtnClick(Sender: TObject);
var
  S : AnsiString;
  RV, Money   : TStMoney;
begin
  RV    := TStMoney.Create;
  Money := TStMoney.Create;

  try
    if PendOp <> #0 then begin
      S := Memo1.Lines[0];
      if S = '' then begin
        MessageBeep(0);
        Exit;
      end;

      RV.Amount.AsString := XBuffer;
      Money.Amount.AsString := S;

      case PendOp of
        '+' : begin
                RV.Add(Money, RV);
                Memo1.Lines[0] := RV.AsString;
              end;

        '-' : begin
                RV.Subtract(Money, RV);
                Memo1.Lines[0] := RV.AsString;
              end;

        '*' : begin
                RV.Multiply(StrToFloat(S), RV);
                Memo1.Lines[0] := RV.AsString;
              end;

        '/' : begin
                if Money.IsZero then begin
                 Memo1.Lines[0] := 'Divide by zero error';
                  PendOp := #0;
                  ClearOnNext := False;
                end else begin
                  RV.Divide(StrToFloat(S), RV);
                  Memo1.Lines[0] := RV.AsString;
                end;
              end;

      end; { case }

    end;

    PendOp := #0;
    ClearOnNext := True;

    Memo1.SetFocus;
    Memo1.SelStart := 0;
    Memo1.SelLength := 0;
  finally
    Money.Free;
    RV.Free;
  end;
end;

procedure TMoneyCalcDlg.UpdateCurrencyCombo;
var
  i : Integer;
begin
  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  for i := 0 to Pred(Currencies.Count) do
    ComboBox1.Items.Add(Currencies.Items[i].ISOName);
  ComboBox1.Text := '';
  ComboBox1.Items.EndUpdate;
end;

procedure TMoneyCalcDlg.UpdateConversionCombo;
var
  i : Integer;
begin
  ComboBox2.Items.BeginUpdate;
  ComboBox2.Items.Clear;
  for i := 0 to Pred(Conversions.Count) do
    if Conversions.Items[i].Source = BaseCurrency then
      ComboBox2.Items.Add(Conversions.Items[i].Target);
  ComboBox2.Text := '';
  ComboBox2.Items.EndUpdate;
  ListBox2.Clear;
end;

procedure TMoneyCalcDlg.ShowCurrencyData(const Name : string);
var
  Cur : TStCurrency;
begin
  Cur := Currencies.Currencies[Name];
  ListBox1.Items.Clear;
  ListBox1.Items.Add('Name:' + #9 + Cur.Name);
  ListBox1.Items.Add('ISOName:' + #9 + Cur.ISOName);
  ListBox1.Items.Add('ISOCode:' + #9 + Cur.ISOCode);
  ListBox1.Items.Add('Major:' + #9 + Cur.UnitMajor);
  ListBox1.Items.Add('Minor:' + #9 + Cur.UnitMinor);

  if ComboBox2.Text <> '' then
    ShowExchangeData(Name, ComboBox2.Text);
end;

procedure TMoneyCalcDlg.ComboBox1Change(Sender: TObject);
begin
  BaseCurrency := ComboBox1.Text;
  UpdateConversionCombo;
  UpdateFormCaption;
  ShowCurrencyData(BaseCurrency);
end;

procedure TMoneyCalcDlg.ConvertBtnClick(Sender: TObject);
var
  CV : TStMoney;
begin
  CV := TStMoney.Create;

  try
    CV.ExchangeRates := Conversions;
    CV.Amount.AsString := Memo1.Lines[0];
    CV.Currency := ComboBox1.Text;

    CV.Convert(ComboBox2.Text, CV);
    Memo1.Lines[0] := CV.AsString;
  finally
    CV.Free;
  end;
end;

procedure TMoneyCalcDlg.ShowExchangeData(const src, trg : string);
var
  Cur : TStCurrency;
  Rate : TStExchangeRate;
begin
  Cur := Currencies.Currencies[trg];
  Rate := Conversions.Rates[src, trg];

  ListBox2.Items.Clear;
  case Rate.ConversionType of
    ctTriangular: begin
      ListBox2.Items.Add('Name:' + #9 + Cur.Name);
      ListBox2.Items.Add(src + '->' + trg + ' inter.: ' + Rate.Intermediate);
    end;

    ctMultiply: begin
      ListBox2.Items.Add('Name: ' + #9 + Cur.Name);
      ListBox2.Items.Add(src + '->' + trg + ' multiply by: ' + Rate.Rate.AsString);
    end;

    ctDivide: begin
      ListBox2.Items.Add('Name: ' + #9 + Cur.Name);
      ListBox2.Items.Add(src + '->' + trg + ' divide by: ' + Rate.Rate.AsString);
    end;
  end;

end;

procedure TMoneyCalcDlg.ComboBox2Change(Sender: TObject);
begin
  ShowExchangeData(ComboBox1.Text, ComboBox2.Text);
end;

procedure GetRateAndDate(var Rate, Date: string);
begin

end;

procedure TMoneyCalcDlg.DoRateUpdate(Sender: TObject;
  NewRate : TStDecimal; var NewDate : TDateTime);
var
  ARate, ADate : string;
begin
  GetRateAndDate(ARate, ADate);
  NewRate.AsString := ARate;
  NewDate := StrToDateTime(ADate);
end;



procedure TMoneyCalcDlg.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DOWN then
    Key := 0;
end;

procedure TMoneyCalcDlg.BSBtnClick(Sender: TObject);
begin
  Memo1.Lines[0] := Copy(Memo1.Lines[0], 1, Length(Memo1.Lines[0]) - 1);
  if Length(Memo1.Lines[0]) = 0 then
    ClearBtnClick(ClearBtn);
end;

procedure TMoneyCalcDlg.Copy1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
  Memo1.SelStart := 0;
end;

procedure TMoneyCalcDlg.Paste1Click(Sender: TObject);
var
  S : string;
  IsNeg : Boolean;
begin
  IsNeg := False;
  S := Clipboard.AsText;
  if (S[1] = '-') then begin
    IsNeg := True;
    S := Copy(S, 2, Length(S) - 1);
  end;

  if IsStrNumericL(S, '0123456789' + SysUtils.DecimalSeparator) then begin
    if IsNeg then S := '-' + S;
    Memo1.Lines[0] := S;
  end;
end;

end.

