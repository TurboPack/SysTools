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

unit BcdCalU;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, Menus, Clipbrd;

type
  BCDCharSet = set of Char;
  BCDOperSet = set of Char;

type
  TBCDCalcDlg = class(TForm)
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
    SqrtBtn: TBitBtn;
    LnBtn: TBitBtn;
    ExpBtn: TBitBtn;
    XtoYBtn: TBitBtn;
    AddBtn: TBitBtn;
    SubBtn: TBitBtn;
    MulBtn: TBitBtn;
    DivBtn: TBitBtn;
    PlusMinusBtn: TBitBtn;
    ClearBtn: TBitBtn;
    EqualBtn: TBitBtn;
    ClearEntryBtn: TBitBtn;
    Bevel1: TBevel;
    gb1: TGroupBox;
    BCDString: TEdit;
    BSBtn: TBitBtn;
    Memo1: TMemo;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CloseBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ZeroBtnClick(Sender: TObject);
    procedure DecKeyClick(Sender: TObject);
    procedure OneKeyClick(Sender: TObject);
    procedure TwoKeyClick(Sender: TObject);
    procedure ThreeKeyClick(Sender: TObject);
    procedure FourKeyClick(Sender: TObject);
    procedure FiveKeyClick(Sender: TObject);
    procedure SixKeyClick(Sender: TObject);
    procedure SevenKeyClick(Sender: TObject);
    procedure EightKeyClick(Sender: TObject);
    procedure NineKeyClick(Sender: TObject);
    procedure PlusMinusBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure SubBtnClick(Sender: TObject);
    procedure MulBtnClick(Sender: TObject);
    procedure DivBtnClick(Sender: TObject);
    procedure SqrtBtnClick(Sender: TObject);
    procedure ExpBtnClick(Sender: TObject);
    procedure LnBtnClick(Sender: TObject);
    procedure XtoYBtnClick(Sender: TObject);
    procedure EqualBtnClick(Sender: TObject);
    procedure ClearEntryBtnClick(Sender: TObject);
    procedure BSBtnClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BCDChar   : BCDCharSet;
    BCDOper   : BCDOperSet;
    PendOp    : Char;
    DFHold    : Integer;
    XBuffer   : string[20];
    ClearOnNext : Boolean;

    procedure SendKeyPress(Sender : TObject; C : Char);
  end;


var
  BCDCalcDlg: TBCDCalcDlg;

implementation

{$R *.DFM}

uses
  StConst,
  StBase,
  StStrL,
  StBCD;

procedure TBCDCalcDlg.FormCreate(Sender: TObject);
begin
  BCDChar := ['0'..'9', SysUtils.DecimalSeparator, 'p'];
  BCDOper := ['+', '-', '/', '*', '^', 'e', 'l', 's', '='];
  DecKey.Caption := SysUtils.DecimalSeparator;
  Memo1.Lines[0] := '0';
  PendOp := #0;
  DFHold := 0;
  XBuffer := '0';
  ClearOnNext := False;

end;


function BytesToString(Value : PByte; Size : Cardinal) : string;
  {-convert byte array to string, no spaces or hex enunciators, e.g., '$'}
var
  I,
  Index  : Cardinal;
  S      : String[3];
begin
  {$IFOPT H+}
  SetLength(Result,2*Size);
  {$ELSE}
  Result[0] := AnsiChar(Size*2);
  {$ENDIF}

  for I := 1 to Size do
  begin
    Index := I*2;
  {$IFOPT H+}
    S := HexBL(Byte(PAnsiChar(Value)[I-1]));
  {$ELSE}
    S := HexBS(Byte(PAnsiChar(Value)[I-1]);
  {$ENDIF}
    Result[(Index)-1] := S[1];
    Result[Index] := S[2];
  end;
end;

function StringToBytes(IString : string; var Value; Size : LongInt) : Boolean;
  {-convert string (by groups of 2 char) to byte values}
var
  Code,
  Index,
  I     : Integer;
  Q     : TBcd;
  S     : array[1..3] of AnsiChar;
begin
  if ((Length(IString) div 2) <> Size) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  for I := 1 to Size do
  begin
    Index := (2*(I-1))+1;
    S[1] := '$';
    S[2] := IString[Index];
    S[3] := IString[Index+1];
    Val(S,Q[I-1],Code);
    if (Code <> 0) then
    begin
      Result := False;
      Exit;
    end;
 end;
  Move(Q,Value,Size);
end;

procedure TBCDCalcDlg.FormKeyPress(Sender: TObject; var Key: Char);
var
  HldOp : Char;
  L     : Integer;
  BCD1  : TBcd;
  S     : string[21];
begin
  if Memo1.Lines[0] = '0' then
    Memo1.Lines[0] := '';

  if Key = #13 then begin
    if XBuffer = '0' then
        XBuffer := Memo1.Lines[0]
    else begin
      EqualBtnClick(Sender);
      XBuffer := '0';
    end;
    Key := #0;
    ClearOnNext := True;
  end;

  if Key in BCDChar then begin
    if (Length(Memo1.Lines[0]) = 0) and (Key = SysUtils.DecimalSeparator) then begin
      Memo1.Lines[0] := '0';
    end;
    if (Key = 'p') then begin
      S := Memo1.Lines[0];
      if (S[1] <> '-') then
        Insert('-',S,1)
      else
        Delete(S,1,1);
      Memo1.Lines[0] := S;
      BCD1 := ValBcd(S);
      BCDString.Text := BytesToString(@BCD1,SizeOf(BCD1));
      Key := #0;
    end else begin
      if ClearOnNext then begin
        Memo1.Lines[0] := '';
        ClearOnNext := False;
      end;
    end;
  end;

  if Key in BCDOper then begin
    if not (Key in ['s', 'e', 'l']) then begin
      if Memo1.Lines[0] = '' then
        Memo1.Lines[0] := '0';
      if (XBuffer <> '0') then
        EqualBtnClick(Sender);
      XBuffer := Memo1.Lines[0];
      BCD1 := ValBcd(XBuffer);
      BCDString.Text := BytesToString(@BCD1,SizeOf(BCD1));
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

  if (Key in BCDChar) then begin
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
end;


procedure TBCDCalcDlg.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TBCDCalcDlg.ClearBtnClick(Sender: TObject);
begin
  XBuffer := '0';
  Memo1.Lines[0] := '0';
  BCDString.Text := '';
  PendOp := #0;
  ClearOnNext := True;
end;

procedure TBCDCalcDlg.ClearEntryBtnClick(Sender: TObject);
begin
  Memo1.Lines[0] := '0';
  ClearOnNext := True;
end;

procedure TBCDCalcDlg.SendKeyPress(Sender : TObject; C : Char);
var
  KP : Char;
begin
  KP := C;
  FormKeyPress(Sender,KP);
end;

procedure TBCDCalcDlg.ZeroBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'0');
end;

procedure TBCDCalcDlg.DecKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender, SysUtils.DecimalSeparator);
end;

procedure TBCDCalcDlg.OneKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'1');
end;

procedure TBCDCalcDlg.TwoKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'2');
end;

procedure TBCDCalcDlg.ThreeKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'3');
end;

procedure TBCDCalcDlg.FourKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'4');
end;

procedure TBCDCalcDlg.FiveKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'5');
end;

procedure TBCDCalcDlg.SixKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'6');
end;

procedure TBCDCalcDlg.SevenKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'7');
end;

procedure TBCDCalcDlg.EightKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'8');
end;

procedure TBCDCalcDlg.NineKeyClick(Sender: TObject);
begin
  SendKeyPress(Sender,'9');
end;

procedure TBCDCalcDlg.PlusMinusBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'p');
end;

procedure TBCDCalcDlg.AddBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'+');
end;

procedure TBCDCalcDlg.SubBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'-');
end;

procedure TBCDCalcDlg.MulBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'*');
end;

procedure TBCDCalcDlg.DivBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'/');
end;

procedure TBCDCalcDlg.SqrtBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'s');
end;

procedure TBCDCalcDlg.ExpBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'e');
end;

procedure TBCDCalcDlg.LnBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'l');
end;

procedure TBCDCalcDlg.XtoYBtnClick(Sender: TObject);
begin
  SendKeyPress(Sender,'^');
end;

procedure TBCDCalcDlg.EqualBtnClick(Sender: TObject);
var
//  RV    : Extended;
  S     : string[21];
  BCD   : TBcd;
begin
  if PendOp <> #0 then begin
    S := Memo1.Lines[0];
    if S = '' then begin
      MessageBeep(0);
      Exit;
    end;
    case PendOp of
      '+' : begin
//              RV := StrToFloat(XBuffer) + StrToFloat(S);
              BCD := AddBCD(ValBCD(XBuffer), ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '-' : begin
//              RV := StrToFloat(XBuffer) - StrToFloat(S);
              BCD := SubBCD(ValBCD(XBuffer), ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '*' : begin
//              RV := StrToFloat(XBuffer) * StrToFloat(S);
              BCD := MulBCD(ValBCD(XBuffer), ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '/' : begin
//              RV := StrToFloat(S);
              BCD := ValBCD(S);
//              if RV = 0 then begin
              if CmpBcd(BCD, ZeroBcd) = 0 then begin
                Memo1.Lines[0] := 'Divide by zero error';
                PendOp := #0;
                ClearOnNext := False;
              end else begin
//                RV := StrToFloat(XBuffer) / StrToFloat(S);
                BCD := DivBCD(ValBCD(XBuffer), BCD);
//                Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//                BCD := ValBcd(Memo1.Lines[0]);
                BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
              end;
            end;
      's' : begin
//              RV := Sqrt(StrToFloat(S));
              BCD := SqrtBcd(ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      'e' : begin
//              RV := Exp(StrToFloat(S));
              BCD := ExpBCD(ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      'l' : begin
//              RV := ln(StrToFloat(S));
              BCD := lnBCD(ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;
      '^' : begin
//              RV := exp(ln(StrToFloat(XBuffer)) * StrToFloat(S));
              BCD := PowBCD(ValBCD(XBuffer), ValBCD(S));
//              Memo1.Lines[0] := FloatToStr(RV);
              Memo1.Lines[0] := RightTrimCharsL(Trim(StrBCD(BCD, 35, 20)), '0');
//              BCD := ValBcd(Memo1.Lines[0]);
              BCDString.Text := BytesToString(@BCD,SizeOf(BCD));
            end;

    end;
  end;
  PendOp := #0;
  ClearOnNext := True;
  Memo1.SetFocus;
  Memo1.SelStart := 0;
  Memo1.SelLength := 0;
end;


procedure TBCDCalcDlg.BSBtnClick(Sender: TObject);
begin
  Memo1.Lines[0] := Copy(Memo1.Lines[0], 1, Length(Memo1.Lines[0]) - 1);
  if Length(Memo1.Lines[0]) = 0 then
    ClearBtnClick(ClearBtn);
end;

procedure TBCDCalcDlg.Copy1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
  Memo1.SelStart := 0;
end;

procedure TBCDCalcDlg.Paste1Click(Sender: TObject);
var
  S : string;
  IsNeg : Boolean;
begin
  S := Clipboard.AsText;
  IsNeg := False;
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
