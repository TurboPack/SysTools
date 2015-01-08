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

unit Medtr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StPtrns, ExtCtrls;

type
  TMediatorForm = class(TForm)
    Panel1: TPanel;
    ASelectBox: TCheckBox;
    BSelectBox: TCheckBox;
    CSelectBox: TCheckBox;
    DSelectBox: TCheckBox;
    ESelectBox: TCheckBox;
    Start: TButton;
    Panel2: TPanel;
    Ch1Lbl: TLabel;
    Ch2Lbl: TLabel;
    Ch3Lbl: TLabel;
    Ch4Lbl: TLabel;
    Panel3: TPanel;
    AEvents: TRadioGroup;
    H1Result: TLabel;
    H2Result: TLabel;
    H3Result: TLabel;
    H4Result: TLabel;
    BEvents: TRadioGroup;
    CEvents: TRadioGroup;
    DEvents: TRadioGroup;
    EEvents: TRadioGroup;
    procedure StartClick(Sender: TObject);
  private
    TheMediator: TStMediator;

    procedure MediatedFunction1(aInputData, aResultData : TObject);
    procedure MediatedFunction2(aInputData, aResultData : TObject);
    procedure MediatedFunction3(aInputData, aResultData : TObject);
    procedure MediatedFunction4(aInputData, aResultData : TObject);
    procedure SetupMediator(Letter: String; Which: TRadioGroup);
    { Private declarations }

  public
    { Public declarations }
  end;

var
  MediatorForm: TMediatorForm;

implementation

{$R *.DFM}

procedure TMediatorForm.StartClick(Sender: TObject);
begin
  TheMediator := TStMediator.create;
  H1Result.Caption := '';
  H2Result.Caption := '';
  H3Result.Caption := '';
  H4Result.Caption := '';
  SetupMediator('A', AEvents);
  SetupMediator('B', BEvents);
  SetupMediator('C', CEvents);
  SetupMediator('D', DEvents);
  SetupMediator('E', EEvents);

  if (ASelectBox.Checked) then
    TheMediator.Handle('A', nil, nil);
  if (BSelectBox.Checked) then
    TheMediator.Handle('B', nil, nil);
  if (CSelectBox.Checked) then
    TheMediator.Handle('C', nil, nil);
  if (DSelectBox.Checked) then
    TheMediator.Handle('D', nil, nil);
  if (ESelectBox.Checked) then
    TheMediator.Handle('E', nil, nil);

  TheMediator.free;

end;

procedure TMediatorForm.SetupMediator(Letter: String; Which: TRadioGroup);
begin
  If (Which.ItemIndex = 0) then
      TheMediator.Add(Letter, MediatedFunction1)
  else If (Which.ItemIndex = 1) then
      TheMediator.Add(Letter, MediatedFunction2)
  else If (Which.ItemIndex = 2) then
      TheMediator.Add(Letter, MediatedFunction3)
  else If (Which.ItemIndex = 3) then
      TheMediator.Add(Letter, MediatedFunction4);
end;

procedure TMediatorForm.MediatedFunction1(aInputData, aResultData : TObject);
begin
  H1Result.Caption := 'I worked';
end;

procedure TMediatorForm.MediatedFunction2(aInputData, aResultData : TObject);
begin
  H2Result.Caption := 'I worked';
end;

procedure TMediatorForm.MediatedFunction3(aInputData, aResultData : TObject);
begin
  H3Result.Caption := 'I worked';
end;

procedure TMediatorForm.MediatedFunction4(aInputData, aResultData : TObject);
begin
  H4Result.Caption := 'I worked';
end;

procedure InitUnit;
begin
end;

procedure DoneUnit;
begin
end;


initialization
  InitUnit;

finalization
  DoneUnit;

end.
