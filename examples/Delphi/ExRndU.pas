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

unit ExRndU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls,
  
  StRandom;

type
  TGetRandom = function : double of object;

type
  TForm1 = class(TForm)
    imgGraph: TImage;
    cboDist: TComboBox;
    lblPrompt: TLabel;
    btnGenerate: TButton;
    prgGenProgress: TProgressBar;
    lblGraphTitle: TLabel;
    lblParms: TLabel;
    lblParm1: TLabel;
    lblParm2: TLabel;
    edtParm1: TEdit;
    edtParm2: TEdit;
    lblLeft: TLabel;
    lblRight: TLabel;
    updRight: TUpDown;
    updLeft: TUpDown;
    lblMaxY: TLabel;
    procedure btnGenerateClick(Sender: TObject);
    procedure cboDistChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure updRightClick(Sender: TObject; Button: TUDBtnType);
    procedure updLeftClick(Sender: TObject; Button: TUDBtnType);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    GraphLeft : double;
    GraphRight : double;
    Value1     : double;
    Value2     : double;
    PRNG : TStRandomBase;
    GetRandom : TGetRandom;

    procedure GenerateGraph(aDistInx : integer);

    procedure PrepForBeta;
    procedure PrepForCauchy;
    procedure PrepForChiSquared;
    procedure PrepForErlang;
    procedure PrepForExponential;
    procedure PrepForF;
    procedure PrepForGamma;
    procedure PrepForLogNormal;
    procedure PrepForNormal;
    procedure PrepForT;
    procedure PrepForUniform;
    procedure PrepForWeibull;

    function GetBeta : double;
    function GetCauchy : double;
    function GetChiSquared : double;
    function GetErlang : double;
    function GetExponential : double;
    function GetF : double;
    function GetGamma : double;
    function GetLogNormal : double;
    function GetNormal : double;
    function GetT : double;
    function GetUniform : double;
    function GetWeibull : double;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  DistNames : array [0..11] of string = (
    'Beta', 'Cauchy', 'ChiSquared', 'Erlang', 'Exponential',
    'F', 'Gamma', 'LogNormal', 'Normal', 'Student''s t',
    'Uniform', 'Weibull');

const
  RandomCount = 1000000;

procedure TForm1.GenerateGraph(aDistInx : integer);
var
  Buckets : array[0..400] of integer;
  i       : integer;
  R       : double;
  Inx     : integer;
  MaxHt   : integer;
  MaxLineFactor : double;
  GraphWidth : double;
  OldPercent : integer;
  NewPercent : integer;
begin
  {zero out the buckets}
  FillChar(Buckets, sizeof(Buckets), 0);

  {calculate random numbers according to distribution, convert to a
   bucket index, and increment that bucket count}
  OldPercent := -1;
  GraphWidth := imgGraph.Width;
  for i := 1 to RandomCount do begin
    NewPercent := (i * 100) div RandomCount;
    if (NewPercent <> OldPercent) then begin
      prgGenProgress.Position := NewPercent;
      OldPercent := NewPercent;
    end;
    R := GetRandom;
    if (GraphLeft <= R) and (R <= GraphRight) then begin
      Inx := trunc((R - GraphLeft) * GraphWidth / (GraphRight - GraphLeft));
      if (0 <= Inx) and (Inx <= 400) then
        inc(Buckets[Inx]);
    end;
  end;

  {calculate the largest bucket}
  MaxHt := 1;
  for i := 0 to 400 do
    if (MaxHt < Buckets[i]) then
      MaxHt := Buckets[i];

  {draw the graph}
  imgGraph.Canvas.Lock;
  try
    imgGraph.Canvas.FillRect(Rect(0, 0, imgGraph.Width, imgGraph.Height));
    MaxLineFactor := imgGraph.Height / MaxHt;
    imgGraph.Canvas.Pen.Color := clRed;
    for i := 0 to 400 do begin
      imgGraph.Canvas.PenPos := Point(i, imgGraph.Height);
      imgGraph.Canvas.LineTo(i, imgGraph.Height - trunc(Buckets[i] * MaxLineFactor));
    end;
  finally
    imgGraph.Canvas.Unlock;
  end;

  lblMaxY.Caption := Format('Max: %8.6f', [MaxHt / RandomCount]);
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
begin
  if (edtParm1.Text = '') then
    Value1 := 0.0
  else
    Value1 := StrToFloat(edtParm1.Text);
  if (edtParm2.Text = '') then
    Value2 := 0.0
  else
    Value2 := StrToFloat(edtParm2.Text);
  GenerateGraph(cboDist.ItemIndex);
end;

procedure TForm1.cboDistChange(Sender: TObject);
begin
  case cboDist.ItemIndex of
    0 : PrepForBeta;
    1 : PrepForCauchy;
    2 : PrepForChiSquared;
    3 : PrepForErlang;
    4 : PrepForExponential;
    5 : PrepForF;
    6 : PrepForGamma;
    7 : PrepForLogNormal;
    8 : PrepForNormal;
    9 : PrepForT;
    10: PrepForUniform;
    11: PrepForWeibull
  end;
  updRightClick(Self, btNext);
  updLeftClick(Self, btNext);
  edtParm1.Text := FloatToStr(Value1);
  edtParm2.Text := FloatToStr(Value2);
end;

procedure TForm1.PrepForBeta;
begin
  lblParm1.Caption := 'Shape 1:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Shape 2:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := 0;
  updRight.Position := 1;
  Value1 := 2.0;
  Value2 := 4.0;
  GetRandom := GetBeta;
end;

procedure TForm1.PrepForCauchy;
begin
  lblParm1.Caption := '(none)';
  lblParm1.Visible := true;
  lblParm2.Visible := false;
  edtParm1.Visible := false;
  edtParm1.Enabled := false;
  edtParm2.Visible := false;
  edtParm2.Enabled := false;
  updLeft.Position := -5;
  updRight.Position := 5;
  Value1 := 0.0;
  Value2 := 0.0;
  GetRandom := GetCauchy;
end;

procedure TForm1.PrepForChiSquared;
begin
  lblParm1.Caption := 'Degrees of freedom:';
  lblParm1.Visible := true;
  lblParm2.Visible := false;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := false;
  edtParm2.Enabled := false;
  updLeft.Position := 0;
  updRight.Position := 20;
  Value1 := 5.0;
  Value2 := 0.0;
  GetRandom := GetChiSquared;
end;

procedure TForm1.PrepForErlang;
begin
  lblParm1.Caption := 'Mean:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Order:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := 0;
  updRight.Position := 5;
  Value1 := 1.0;
  Value2 := 4.0;
  GetRandom := GetErlang;
end;

procedure TForm1.PrepForExponential;
begin
  lblParm1.Caption := 'Mean:';
  lblParm1.Visible := true;
  lblParm2.Visible := false;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := false;
  edtParm2.Enabled := false;
  updLeft.Position := 0;
  updRight.Position := 10;
  Value1 := 1.0;
  Value2 := 0.0;
  GetRandom := GetExponential;
end;

procedure TForm1.PrepForF;
begin
  lblParm1.Caption := 'Degrees of freedom 1:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Degrees of freedom 2:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := 0;
  updRight.Position := 20;
  Value1 := 10.0;
  Value2 := 5.0;
  GetRandom := GetF;
end;

procedure TForm1.PrepForGamma;
begin
  lblParm1.Caption := 'Shape:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Scale:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := 0;
  updRight.Position := 10;
  Value1 := 2.0;
  Value2 := 1.0;
  GetRandom := GetGamma;
end;

procedure TForm1.PrepForLogNormal;
begin
  lblParm1.Caption := 'Mean:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Standard deviation:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := 0;
  updRight.Position := 10;
  Value1 := 0.0;
  Value2 := 1.0;
  GetRandom := GetLogNormal;
end;

procedure TForm1.PrepForNormal;
begin
  lblParm1.Caption := 'Mean:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Standard deviation:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := -5;
  updRight.Position := 5;
  Value1 := 0.0;
  Value2 := 1.0;
  GetRandom := GetNormal;
end;

procedure TForm1.PrepForT;
begin
  lblParm1.Caption := 'Degrees of freedom:';
  lblParm1.Visible := true;
  lblParm2.Visible := false;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := false;
  edtParm2.Enabled := false;
  updLeft.Position := -10;
  updRight.Position := 10;
  Value1 := 10.0;
  Value2 := 0.0;
  GetRandom := GetT;
end;

procedure TForm1.PrepForUniform;
begin
  lblParm1.Caption := '(none)';
  lblParm1.Visible := true;
  lblParm2.Visible := false;
  edtParm1.Visible := false;
  edtParm1.Enabled := false;
  edtParm2.Visible := false;
  edtParm2.Enabled := false;
  updLeft.Position := 0;
  updRight.Position := 1;
  Value1 := 0.0;
  Value2 := 0.0;
  GetRandom := GetUniform;
end;

procedure TForm1.PrepForWeibull;
begin
  lblParm1.Caption := 'Shape:';
  lblParm1.Visible := true;
  lblParm2.Caption := 'Scale:';
  lblParm2.Visible := true;
  edtParm1.Visible := true;
  edtParm1.Enabled := true;
  edtParm2.Visible := true;
  edtParm2.Enabled := true;
  updLeft.Position := 0;
  updRight.Position := 10;
  Value1 := 2.0;
  Value2 := 3.0;
  GetRandom := GetWeibull;
end;

function TForm1.GetBeta : double;
begin
  Result := PRNG.AsBeta(Value1, Value2)
end;

function TForm1.GetCauchy : double;
begin
  Result := PRNG.AsCauchy
end;

function TForm1.GetChiSquared : double;
begin
  if (Value1 > 65535.0) then
    raise Exception.Create(
      'TForm1.GetChiSquared: the degrees of freedom value 1 is too large for this example program');
  Result := PRNG.AsChiSquared(trunc(Value1))
end;

function TForm1.GetErlang : double;
begin
  Result := PRNG.AsErlang(Value1, trunc(Value2))
end;

function TForm1.GetExponential : double;
begin
  Result := PRNG.AsExponential(Value1)
end;

function TForm1.GetF : double;
begin
  if (Value1 > 65535.0) then
    raise Exception.Create(
      'TForm1.GetF: the degrees of freedom value 1 is too large for this example program');
  if (Value2 > 65535.0) then
    raise Exception.Create(
      'TForm1.GetF: the degrees of freedom value 2 is too large for this example program');
  Result := PRNG.AsF(trunc(Value1), trunc(Value2))
end;

function TForm1.GetGamma : double;
begin
  Result := PRNG.AsGamma(Value1, Value2)
end;

function TForm1.GetLogNormal : double;
begin
  Result := PRNG.AsLogNormal(Value1, Value2)
end;

function TForm1.GetNormal : double;
begin
  Result := PRNG.AsNormal(Value1, Value2)
end;

function TForm1.GetT : double;
begin
  if (Value1 > 65535.0) then
    raise Exception.Create(
      'TForm1.GetT: the degrees of freedom value is too large for this example program');
  Result := PRNG.AsT(trunc(Value1))
end;

function TForm1.GetUniform : double;
begin
  Result := PRNG.AsFloat
end;

function TForm1.GetWeibull : double;
begin
  Result := PRNG.AsWeibull(Value1, Value2)
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : integer;
  UniformInx : integer;
begin
  cboDist.Items.Clear;
  UniformInx := -1;
  for i := 0 to high(DistNames) do begin
    cboDist.Items.Add(DistNames[i]);
    if (Copy(DistNames[i], 1, 7) = 'Uniform') then
      UniformInx := i;
  end;
  cboDist.ItemIndex := UniformInx;
  cboDistChange(Self);
  PRNG := TStRandomSystem.Create(0);
end;

procedure TForm1.updRightClick(Sender: TObject; Button: TUDBtnType);
begin
  lblRight.Caption := IntToStr(updRight.Position);
  GraphRight := updRight.Position;
end;

procedure TForm1.updLeftClick(Sender: TObject; Button: TUDBtnType);
begin
  lblLeft.Caption := IntToStr(updLeft.Position);
  GraphLeft := updLeft.Position;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  PRNG.Free;
end;

end.
