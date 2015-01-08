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

unit Chain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StPtrns, ExtCtrls;

type
  TInputData = class
    public
    { Public declarations }
     InData : integer;
  end;

type
  TOutputData = class
    public
    { Public declarations }
     OutData : integer;
  end;

type
  TChainForm = class(TForm)
    Panel2: TPanel;
    Ch1Lbl: TLabel;
    Ch2Lbl: TLabel;
    Ch3Lbl: TLabel;
    Ch4Lbl: TLabel;
    Ch5Lbl: TLabel;
    Ch1Value: TEdit;
    Ch2Value: TEdit;
    Ch3Value: TEdit;
    Ch4Value: TEdit;
    Ch5Value: TEdit;
    Ch1Msg: TEdit;
    Ch2Msg: TEdit;
    Ch3Msg: TEdit;
    Ch4Msg: TEdit;
    Ch5Msg: TEdit;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    InputValue: TEdit;
    Start: TButton;
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
// Code for the chain
    TheChain : TStChain;
    procedure Chain1Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
    procedure Chain2Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
    procedure Chain3Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
    procedure Chain4Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
    procedure Chain5Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
    procedure ClearScreen;

  public
    { Public declarations }
  end;

var
  ChainForm: TChainForm;

implementation
var
  ChainPotato : Boolean;
  TheChain: TStChain;

{$R *.DFM}

procedure TChainForm.RadioButton1Click(Sender: TObject);
begin
  if (RadioButton1.Checked) then begin
    Ch1Lbl.Caption := 'Handle < 10';
    Ch2Lbl.Caption := 'Handle 10';
    Ch3Lbl.Caption := 'Handle Odd';
    Ch4Lbl.Caption := 'Handle > 100';
    Ch5Lbl.Caption := 'Default Handler';
    ChainPotato := true;
    ClearScreen;
  end;
end;

procedure TChainForm.RadioButton2Click(Sender: TObject);
begin
  if (RadioButton2.Checked) then begin
    Ch1Lbl.Caption := 'Add 10';
    Ch2Lbl.Caption := 'Multiply by 10';
    Ch3Lbl.Caption := 'Add 3';
    Ch4Lbl.Caption := 'Subtract 4';
    Ch5Lbl.Caption := 'Zero out';
    ChainPotato := false;
    ClearScreen;
  end;


end;
procedure TChainForm.Chain1Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
var
  myInputData : TInputData;
  myOutputData : TOutputData;
begin
  if (ChainPotato) then begin
    myInputData := TInputData(aInputData);
    if (myInputData.InData < 10) then begin
      Ch1Value.text := Inttostr(myInputData.InData);
      Ch1Msg.Text := 'I handled it';
      aStopNow := true;
    end else begin
      Ch1Value.text := ' ';
      Ch1Msg.Text := 'Not here';
      aStopNow := false;
    end
  end else begin
    myOutputData := TOutputData(aResultData);
    myOutputData.OutData := myOutputData.OutData + 10;
    Ch1Value.text := Inttostr(myOutputData.OutData);
    Ch1Msg.Text := 'Added 10';
  end
end;

procedure TChainForm.Chain2Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
var
  myInputData : TInputData;
  myOutputData : TOutputData;
begin
  if (ChainPotato) then begin
    myInputData := TInputData(aInputData);
    if (myInputData.InData = 10) then begin
      Ch2Value.text := Inttostr(myInputData.InData);
      Ch2Msg.Text := 'I handled it';
      aStopNow := true;
    end else begin
      Ch2Value.text := ' ';
      Ch2Msg.Text := 'Not here';
      aStopNow := false;
    end
  end else begin
    myOutputData := TOutputData(aResultData);
    myOutputData.OutData := myOutputData.OutData * 10;
    Ch2Value.text := Inttostr(myOutputData.OutData);
    Ch2Msg.Text := 'Mulitplied by 10';
  end
end;

procedure TChainForm.Chain3Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
var
  myInputData : TInputData;
  myOutputData : TOutputData;
begin
  if (ChainPotato) then begin
    myInputData := TInputData(aInputData);
    if odd(myInputData.InData) then begin
      Ch3Value.text := Inttostr(myInputData.InData);
      Ch3Msg.Text := 'I handled it';
      aStopNow := true;
    end else begin
      Ch3Value.text := ' ';
      Ch3Msg.Text := 'Not here';
      aStopNow := false;
    end
  end else begin
    myOutputData := TOutputData(aResultData);
    myOutputData.OutData := myOutputData.OutData + 3;
    Ch3Value.text := Inttostr(myOutputData.OutData);
    Ch3Msg.Text := 'Added 3';
  end
end;

procedure TChainForm.Chain4Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
var
  myInputData : TInputData;
  myOutputData : TOutputData;
begin
  if (ChainPotato) then begin
    myInputData := TInputData(aInputData);
    if (myInputData.InData > 100) then begin
      Ch4Value.text := Inttostr(myInputData.InData);
      Ch4Msg.Text := 'I handled it';
      aStopNow := true;
    end else begin
      Ch4Value.text := ' ';
      Ch4Msg.Text := 'Not here';
      aStopNow := false;
    end
  end else begin
    myOutputData := TOutputData(aResultData);
    myOutputData.OutData := myOutputData.OutData -4;
    Ch4Value.text := Inttostr(myOutputData.OutData);
    Ch4Msg.Text := 'Subtracted 4';
  end
end;

procedure TChainForm.Chain5Proc(aInputData, aResultData : TObject; var aStopNow : boolean);
var
  myInputData : TInputData;
  myOutputData : TOutputData;
begin
  if (ChainPotato) then begin
    myInputData := TInputData(aInputData);
    Ch5Value.text := Inttostr(myInputData.InData);
    Ch5Msg.Text := 'I handled it';
    aStopNow := true;
  end else begin
    myOutputData := TOutputData(aResultData);
    myOutputData.OutData := 0;
    Ch5Value.text := Inttostr(myOutputData.OutData);
    Ch5Msg.Text := 'Zeroed out';
  end
end;

procedure TChainForm.FormCreate(Sender: TObject);
begin
  ChainPotato := true;
  TheChain:= TStChain.create;
  TheChain.Add(Chain1Proc);
  TheChain.Add(Chain2Proc);
  TheChain.Add(Chain3Proc);
  TheChain.Add(Chain4Proc);
  TheChain.Add(Chain5Proc);
end;

procedure TChainForm.StartClick(Sender: TObject);
var
  myInputData : TInputData;
  myOutputData : TOutputData;
begin
  myInputData := nil;
  myOutputData := nil;
  try
    myInputData := TInputData.Create;
    myOutputData := TOutputData.Create;
    myInputData.InData := Strtoint(InputValue.Text);
    myOutputData.OutData := Strtoint(InputValue.Text);
    ClearScreen;
    TheChain.Handle(myInputData, myOutputData);
  finally;
    myInputData.free;
    myOutputData.free;
  end;
end;

procedure TChainForm.ClearScreen;
begin
  Ch1Value.text := ' ';
  Ch1Msg.Text := ' ';
  Ch2Value.text := ' ';
  Ch2Msg.Text := ' ';
  Ch3Value.text := ' ';
  Ch3Msg.Text := ' ';
  Ch4Value.text := ' ';
  Ch4Msg.Text := ' ';
  Ch5Value.text := ' ';
  Ch5Msg.Text := ' ';
end;

procedure TChainForm.FormDestroy(Sender: TObject);
begin
  TheChain.free;
end;

end.
