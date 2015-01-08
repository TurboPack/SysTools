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

unit AstCalU;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls,

  StConst,
  StDate,
  StDateSt,
  StAstro,
  StAstroP;


type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    MonthEF: TEdit;
    DateEF: TEdit;
    YearEF: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    GB1: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    LocalTimeEF: TEdit;
    SiderealTimeEF: TEdit;
    GB2: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    SunRiseEF: TEdit;
    MoonRiseEF: TEdit;
    SunSetEF: TEdit;
    MoonSetEF: TEdit;
    TwiStartEF: TEdit;
    TwiEndEF: TEdit;
    GB3: TGroupBox;
    PositionsLB: TListBox;
    Header1: THeader;
    GB4: TGroupBox;
    NMFirstDate: TEdit;
    FQFirstDate: TEdit;
    NMFirstTime: TEdit;
    FQFirstTime: TEdit;
    FMFirstDate: TEdit;
    LQFirstDate: TEdit;
    LQFirstTime: TEdit;
    FMFirstTime: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    NMSecondDate: TEdit;
    NMSecondTime: TEdit;
    FQSecondDate: TEdit;
    FQSecondTime: TEdit;
    FMSecondDate: TEdit;
    FMSecondTime: TEdit;
    LQSecondTime: TEdit;
    LQSecondDate: TEdit;
    NMPrevDate: TEdit;
    FQPrevDate: TEdit;
    FMPrevDate: TEdit;
    LQPrevDate: TEdit;
    LQPrevTime: TEdit;
    FMPrevTime: TEdit;
    FQPrevTime: TEdit;
    NMPrevTime: TEdit;
    NMNextDate: TEdit;
    FQNextDate: TEdit;
    FMNextDate: TEdit;
    LQNextDate: TEdit;
    LQNextTime: TEdit;
    FMNextTime: TEdit;
    FQNextTime: TEdit;
    NMNextTime: TEdit;
    Header2: THeader;
    GB5: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    SpringTime: TEdit;
    SummerTime: TEdit;
    SummerDate: TEdit;
    SpringDate: TEdit;
    FallTime: TEdit;
    WinterTime: TEdit;
    WinterDate: TEdit;
    FallDate: TEdit;
    EasterEF: TEdit;
    PhaseLabel: TLabel;
    Label23: TLabel;
    SunlightEF: TEdit;
    LongEF: TEdit;
    LatEF: TEdit;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    TheDT   : TStDateTimeRec;
    RS      : TStRiseSetRec;

    D, M, Y : Integer;

    ObsLat,
    ObsLong : Double;

    procedure DoCalcTimes;
    procedure DoFixedCalcs;
    procedure DoCalcs(ObsLong, ObsLat : Double);

  end;

var
  Form1: TForm1;


implementation

{$R *.DFM}


procedure TForm1.DoCalcTimes;
var
  TT   : TStTime;
begin
  LocalTimeEF.Text := CurrentTimeString('hh:mm:ss', False);
  TheDT.T := CurrentTime;
  TT := Round(SiderealTime(TheDT) * 240);
  SiderealTimeEF.Text := StTimeToTimeString('hh:mm:ss', TT, False);
end;


procedure TForm1.DoFixedCalcs;
var
  Y,
  M ,
  D    : integer;
  DTR  : TStDateTimeRec;
  MPR  : TStMoonPosRec;
  SPR  : TStPosRec;
  LR   : TStLunarRecord;
  PA   : TStPlanetsArray;

begin

{Calculate Positions}

  SPR := SunPos(TheDT);
  PositionsLB.Items.Add('Sun        ' + HoursMin(SPR.RA) + '    ' + DegsMin(SPR.DC));

  MPR := MoonPos(TheDT);
  PositionsLB.Items.Add('Moon       ' + HoursMin(MPR.RA) + '    ' + DegsMin(MPR.DC));

  PlanetsPos(AstJulianDate(TheDT.D) + TheDT.T/86400, PA);
  PositionsLB.Items.Add('Mercury    ' + HoursMin(PA[1].RA) + '    ' + DegsMin(PA[1].DC));
  PositionsLB.Items.Add('Venus      ' + HoursMin(PA[2].RA) + '    ' + DegsMin(PA[2].DC));
  PositionsLB.Items.Add('Mars       ' + HoursMin(PA[3].RA) + '    ' + DegsMin(PA[3].DC));
  PositionsLB.Items.Add('Jupiter    ' + HoursMin(PA[4].RA) + '    ' + DegsMin(PA[4].DC));
  PositionsLB.Items.Add('Saturn     ' + HoursMin(PA[5].RA) + '    ' + DegsMin(PA[5].DC));
  PositionsLB.Items.Add('Uranus     ' + HoursMin(PA[6].RA) + '    ' + DegsMin(PA[6].DC));
  PositionsLB.Items.Add('Neptune    ' + HoursMin(PA[7].RA) + '    ' + DegsMin(PA[7].DC));
  PositionsLB.Items.Add('Pluto      ' + HoursMin(PA[8].RA) + '    ' + DegsMin(PA[8].DC));


{Calculate lunar phases}

  if LunarPhase(TheDT) >= 0 then
    PhaseLabel.Caption := 'Waxing'
  else
    PhaseLabel.Caption := 'Waning';


  LR := NewMoon(TheDT.D);
  NMFirstDate.Text := StDateToDateString('mm/dd', LR.T[0].D, False);
  NMFirstTime.Text := StTimeToTimeString('hh:mm', LR.T[0].T, False);
  if LR.T[1].D <> BadDate then
  begin
    NMSecondDate.Text := StDateToDateString('mm/dd', LR.T[1].D, False);
    NMSecondTime.Text := StTimeToTimeString('hh:mm', LR.T[1].T, False);
  end else
  begin
    NMSecondDate.Text := '';
    NMSecondTime.Text := '';
  end;

  LR := FirstQuarter(TheDT.D);
  FQFirstDate.Text := StDateToDateString('mm/dd', LR.T[0].D, False);
  FQFirstTime.Text := StTimeToTimeString('hh:mm', LR.T[0].T, False);
  if LR.T[1].D <> BadDate then
  begin
    FQSecondDate.Text := StDateToDateString('mm/dd', LR.T[1].D, False);
    FQSecondTime.Text := StTimeToTimeString('hh:mm', LR.T[1].T, False);
  end else
  begin
    FQSecondDate.Text := '';
    FQSecondTime.Text := '';
  end;

  LR := FullMoon(TheDT.D);
  FMFirstDate.Text := StDateToDateString('mm/dd', LR.T[0].D, False);
  FMFirstTime.Text := StTimeToTimeString('hh:mm', LR.T[0].T, False);
  if LR.T[1].D <> BadDate then
  begin
    FMSecondDate.Text := StDateToDateString('mm/dd', LR.T[1].D, False);
    FMSecondTime.Text := StTimeToTimeString('hh:mm', LR.T[1].T, False);
  end else
  begin
    FMSecondDate.Text := '';
    FMSecondTime.Text := '';
  end;

  LR := LastQuarter(TheDT.D);
  LQFirstDate.Text := StDateToDateString('mm/dd', LR.T[0].D, False);
  LQFirstTime.Text := StTimeToTimeString('hh:mm', LR.T[0].T, False);
  if LR.T[1].D <> BadDate then
  begin
    LQSecondDate.Text := StDateToDateString('mm/dd', LR.T[1].D, False);
    LQSecondTime.Text := StTimeToTimeString('hh:mm', LR.T[1].T, False);
  end else
  begin
    LQSecondDate.Text := '';
    LQSecondTime.Text := '';
  end;


{Calculate Next/Previous}

  DTR := PrevNewMoon(TheDT.D);
  if DTR.D <> BadDate then
  begin
    NMPrevDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    NMPrevTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    NMPrevDate.Text := '';
    NMPrevTime.Text := '';
  end;

  DTR := NextNewMoon(TheDT.D);
  if DTR.D <> BadDate then
  begin
    NMNextDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    NMNextTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    NMNextDate.Text := '';
    NMNextTime.Text := '';
  end;


  DTR := PrevFirstQuarter(TheDT.D);
  if DTR.D <> BadDate then
  begin
    FQPrevDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    FQPrevTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    FQPrevDate.Text := '';
    FQPrevTime.Text := '';
  end;

  DTR := NextFirstQuarter(TheDT.D);
  if DTR.D <> BadDate then
  begin
    FQNextDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    FQNextTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    FQNextDate.Text := '';
    FQNextTime.Text := '';
  end;


  DTR := PrevFullMoon(TheDT.D);
  if DTR.D <> BadDate then
  begin
    FMPrevDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    FMPrevTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    FMPrevDate.Text := '';
    FMPrevTime.Text := '';
  end;

  DTR := NextFullMoon(TheDT.D);
  if DTR.D <> BadDate then
  begin
    FMNextDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    FMNextTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    FMNextDate.Text := '';
    FMNextTime.Text := '';
  end;


  DTR := PrevLastQuarter(TheDT.D);
  if DTR.D <> BadDate then
  begin
    LQPrevDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    LQPrevTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    LQPrevDate.Text := '';
    LQPrevTime.Text := '';
  end;

  DTR := NextLastQuarter(TheDT.D);
  if DTR.D <> BadDate then
  begin
    LQNextDate.Text := StDateToDateString('mm/dd', DTR.D, False);
    LQNextTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
  end else
  begin
    LQNextDate.Text := '';
    LQNextTime.Text := '';
  end;


{Calculate Other Events}

  StDateToDMY(TheDT.D, D, M, Y);
  EasterEF.Text := StDateToDateString('mm/dd', Easter(Y, 0), False);


  DTR := Equinox(Y, 0, True);
  SpringDate.Text := StDateToDateString('mm/dd', DTR.D, False);
  SpringTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);


  DTR := Equinox(Y, 0, False);
  FallDate.Text := StDateToDateString('mm/dd', DTR.D, False);
  FallTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);


  DTR := Solstice(Y, 0, True);
  SummerDate.Text := StDateToDateString('mm/dd', DTR.D, False);
  SummerTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);


  DTR := Solstice(Y, 0, False);
  WinterDate.Text := StDateToDateString('mm/dd', DTR.D, False);
  WinterTime.Text := StTimeToTimeString('hh:mm', DTR.T, False);
end;



procedure TForm1.DoCalcs(ObsLong, ObsLat : Double);
begin
  SunlightEF.Text := StTimeToTimeString('hh:mm',
                       AmountOfSunlight(TheDT.D, ObsLong, ObsLat), False);


  RS := SunRiseSet(TheDT.D, ObsLong, ObsLat);
  SunRiseEF.Text := StTimeToTimeString('hh:mm', RS.ORise, False);
  SunSetEF.Text  := StTimeToTimeString('hh:mm', RS.OSet, False);

  RS := MoonRiseSet(TheDT.D, ObsLong, ObsLat);
  MoonRiseEF.Text := StTimeToTimeString('hh:mm', RS.ORise, False);
  MoonSetEF.Text  := StTimeToTimeString('hh:mm', RS.OSet, False);

  RS := Twilight(TheDT.D, ObsLong, ObsLat, ttAstronomical);
  TwiStartEF.Text := StTimeToTimeString('hh:mm', RS.ORise, False);
  TwiEndEF.Text := StTimeToTimeString('hh:mm', RS.OSet, False);
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    M := StrToInt(MonthEF.Text);
    if not (M in [1..12]) then
    begin
      ShowMessage('Month value out of range (1..12)');
      Exit;
    end;

    D := StrToInt(DateEF.Text);
    if not (D in [1..31]) then
    begin
      ShowMessage('Date value out of range (1..31)');
      Exit;
    end;

    Y := StrToInt(YearEF.Text);
    if (Y < 1800) or (Y > 2200) then
    begin
      ShowMessage('Year value out of range (1800..2200)');
      Exit;
    end;

    TheDT.D := DMYToStDate(D, M, Y, 0);
    if TheDT.D = BadDate then
    begin
      ShowMessage('Invalid date');
      Exit;
    end;
    TheDT.T := CurrentTime;

    ObsLong := StrToFloat(LongEF.Text);
    if (ObsLong < -180) or (ObsLong > 180) then
    begin
      ShowMessage('Longitude out of range (-180..180)');
      Exit;
    end;

    ObsLat := StrToFloat(LatEF.Text);
    if (ObsLat < -90) or (ObsLat > 90) then
    begin
      ShowMessage('Latitude out of range (-90..90)');
      Exit;
    end;

    PositionsLB.Clear;
    DoFixedCalcs;
    DoCalcs(ObsLong, ObsLat);

  except
    ShowMessage('One or more entry fields has illegal data');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TheDT.D := CurrentDate;
  TheDT.T := CurrentTime;

  StDateToDMY(TheDT.D, D, M, Y);
  MonthEF.Text := IntToStr(M);
  DateEF.Text  := IntToStr(D);
  YearEF.Text  := IntToStr(Y);

  LongEF.Text := FloatToStr(-105.27);
  LatEF.Text  := FloatToStr(38.87);

  DoCalcTimes;
  Button1Click(Button1);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  DoCalcTimes;
end;

end.
