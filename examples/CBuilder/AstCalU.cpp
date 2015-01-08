// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower SysTools
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1996-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****
//---------------------------------------------------------------------------
#include <vcl\vcl.h>
#pragma hdrstop
#include "AstCalU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StDate"
#pragma link "StAstro"
#pragma link "StAstroP"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  TheDT.D = CurrentDate();
  TheDT.T = CurrentTime();

  StDateToDMY(TheDT.D, D, M, Y);
  MonthEF->Text = IntToStr(M);
  DateEF->Text  = IntToStr(D);
  YearEF->Text  = IntToStr(Y);

  LongEF->Text = FloatToStr(-105.27);
  LatEF->Text  = FloatToStr(38.87);

  DoCalcTimes();
  Button1Click(this);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  DoCalcTimes();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DoCalcTimes()
{
  TStTime TT;

  LocalTimeEF->Text = CurrentTimeString("hh:mm:ss", false);
  TheDT.T = CurrentTime();
  TT = floor(SiderealTime(TheDT) * 240 + 0.5);
  SiderealTimeEF->Text = StTimeToTimeString("hh:mm:ss", TT, false);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::DoFixedCalcs() {
  int Y, M, D;
  TStDateTimeRec DTR;
  TStMoonPosRec MPR;
  TStPosRec SPR;
  String TS;
  TStLunarRecord LR;
  TStPlanetsArray PA;

  //Calculate Positions

  SPR = SunPos(TheDT);
  PositionsLB->Items->Add("Sun        " + HoursMin(SPR.RA) + "    " + DegsMin(SPR.DC));

  MPR = MoonPos(TheDT);
  PositionsLB->Items->Add("Moon       " + HoursMin(MPR.RA) + "    " + DegsMin(MPR.DC));
  PlanetsPos(AstJulianDate(TheDT.D) + TheDT.T/86400, PA);
  PositionsLB->Items->Add("Mercury    " + HoursMin(PA[1].RA) + "    " + DegsMin(PA[1].DC));
  PositionsLB->Items->Add("Venus      " + HoursMin(PA[2].RA) + "    " + DegsMin(PA[2].DC));
  PositionsLB->Items->Add("Mars       " + HoursMin(PA[3].RA) + "    " + DegsMin(PA[3].DC));
  PositionsLB->Items->Add("Jupiter    " + HoursMin(PA[4].RA) + "    " + DegsMin(PA[4].DC));
  PositionsLB->Items->Add("Saturn     " + HoursMin(PA[5].RA) + "    " + DegsMin(PA[5].DC));
  PositionsLB->Items->Add("Uranus     " + HoursMin(PA[6].RA) + "    " + DegsMin(PA[6].DC));
  PositionsLB->Items->Add("Neptune    " + HoursMin(PA[7].RA) + "    " + DegsMin(PA[7].DC));
  PositionsLB->Items->Add("Pluto      " + HoursMin(PA[8].RA) + "    " + DegsMin(PA[8].DC));

  //Calculate lunar phases

  if (LunarPhase(TheDT) >= 0)
    PhaseLabel->Caption = "Waxing";
  else
    PhaseLabel->Caption = "Waning";

  LR = NewMoon(TheDT.D);
  NMFirstDate->Text = StDateToDateString("mm/dd", LR.T[0].D, false);
  NMFirstTime->Text = StTimeToTimeString("hh:mm", LR.T[0].T, false);
  if (LR.T[1].D != BadDate) {
    NMSecondDate->Text = StDateToDateString("mm/dd", LR.T[1].D, false);
    NMSecondTime->Text = StTimeToTimeString("hh:mm", LR.T[1].T, false);
  }
  else {
    NMSecondDate->Text = "";
    NMSecondTime->Text = "";
  };

  LR = FirstQuarter(TheDT.D);
  FQFirstDate->Text = StDateToDateString("mm/dd", LR.T[0].D, false);
  FQFirstTime->Text = StTimeToTimeString("hh:mm", LR.T[0].T, false);
  if (LR.T[1].D != BadDate) {
    FQSecondDate->Text = StDateToDateString("mm/dd", LR.T[1].D, false);
    FQSecondTime->Text = StTimeToTimeString("hh:mm", LR.T[1].T, false);
  }
  else {
    FQSecondDate->Text = "";
    FQSecondTime->Text = "";
  };

  LR = FullMoon(TheDT.D);
  FMFirstDate->Text = StDateToDateString("mm/dd", LR.T[0].D, false);
  FMFirstTime->Text = StTimeToTimeString("hh:mm", LR.T[0].T, false);
  if (LR.T[1].D != BadDate) {
    FMSecondDate->Text = StDateToDateString("mm/dd", LR.T[1].D, false);
    FMSecondTime->Text = StTimeToTimeString("hh:mm", LR.T[1].T, false);
  }
  else {
    FMSecondDate->Text = "";
    FMSecondTime->Text = "";
  };

  LR = LastQuarter(TheDT.D);
  LQFirstDate->Text = StDateToDateString("mm/dd", LR.T[0].D, false);
  LQFirstTime->Text = StTimeToTimeString("hh:mm", LR.T[0].T, false);
  if (LR.T[1].D != BadDate) {
    LQSecondDate->Text = StDateToDateString("mm/dd", LR.T[1].D, false);
    LQSecondTime->Text = StTimeToTimeString("hh:mm", LR.T[1].T, false);
  }
  else {
    LQSecondDate->Text = "";
    LQSecondTime->Text = "";
  };


  //Calculate Next/Previous

  DTR = PrevNewMoon(TheDT.D);
  if (DTR.D != BadDate) {
    NMPrevDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    NMPrevTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    NMPrevDate->Text = "";
    NMPrevTime->Text = "";
  };

  DTR = NextNewMoon(TheDT.D);
  if (DTR.D != BadDate) {
    NMNextDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    NMNextTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    NMNextDate->Text = "";
    NMNextTime->Text = "";
  };

  DTR = PrevFirstQuarter(TheDT.D);
  if (DTR.D != BadDate) {
    FQPrevDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    FQPrevTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    FQPrevDate->Text = "";
    FQPrevTime->Text = "";
  };

  DTR = NextFirstQuarter(TheDT.D);
  if (DTR.D != BadDate) {
    FQNextDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    FQNextTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    FQNextDate->Text = "";
    FQNextTime->Text = "";
  };


  DTR = PrevFullMoon(TheDT.D);
  if (DTR.D != BadDate) {
    FMPrevDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    FMPrevTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    FMPrevDate->Text = "";
    FMPrevTime->Text = "";
  };

  DTR = NextFullMoon(TheDT.D);
  if (DTR.D != BadDate) {
    FMNextDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    FMNextTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    FMNextDate->Text = "";
    FMNextTime->Text = "";
  };


  DTR = PrevLastQuarter(TheDT.D);
  if (DTR.D != BadDate) {
    LQPrevDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    LQPrevTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    LQPrevDate->Text = "";
    LQPrevTime->Text = "";
  };

  DTR = NextLastQuarter(TheDT.D);
  if (DTR.D != BadDate) {
    LQNextDate->Text = StDateToDateString("mm/dd", DTR.D, false);
    LQNextTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
  }
  else {
    LQNextDate->Text = "";
    LQNextTime->Text = "";
  };

  //Calculate Other Events

  StDateToDMY(TheDT.D, D, M, Y);
  EasterEF->Text = StDateToDateString("mm/dd", Easter(Y, 0), false);


  DTR = Equinox(Y, 0, True);
  SpringDate->Text = StDateToDateString("mm/dd", DTR.D, false);
  SpringTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);


  DTR = Equinox(Y, 0, false);
  FallDate->Text = StDateToDateString("mm/dd", DTR.D, false);
  FallTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);


  DTR = Solstice(Y, 0, True);
  SummerDate->Text = StDateToDateString("mm/dd", DTR.D, false);
  SummerTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);


  DTR = Solstice(Y, 0, false);
  WinterDate->Text = StDateToDateString("mm/dd", DTR.D, false);
  WinterTime->Text = StTimeToTimeString("hh:mm", DTR.T, false);
};
//---------------------------------------------------------------------------
void __fastcall TForm1::DoCalcs(double ObsLong, double ObsLat) {
  SunlightEF->Text = StTimeToTimeString("hh:mm",
                     AmountOfSunlight(TheDT.D, ObsLong, ObsLat), false);

  RS = SunRiseSet(TheDT.D, ObsLong, ObsLat);
  SunRiseEF->Text = StTimeToTimeString("hh:mm", RS.ORise, false);
  SunSetEF->Text  = StTimeToTimeString("hh:mm", RS.OSet, false);

  RS = MoonRiseSet(TheDT.D, ObsLong, ObsLat);
  MoonRiseEF->Text = StTimeToTimeString("hh:mm", RS.ORise, false);
  MoonSetEF->Text  = StTimeToTimeString("hh:mm", RS.OSet, false);

  RS = Twilight(TheDT.D, ObsLong, ObsLat, ttAstronomical);
  TwiStartEF->Text = StTimeToTimeString("hh:mm", RS.ORise, false);
  TwiEndEF->Text = StTimeToTimeString("hh:mm", RS.OSet, false);
};
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  try {
    M = StrToInt(MonthEF->Text);
    if ((M < 1) || (M > 12)) {
      ShowMessage("Month value out of range (1..12)");
      return;
    };

    D = StrToInt(DateEF->Text);
    if ((D < 1) || (D > 31)) {
      ShowMessage("Date value out of range (1..31)");
      return;
    };

    Y = StrToInt(YearEF->Text);
    if ((Y < 1800) || (Y > 2200)) {
      ShowMessage("Year value out of range (1800..2200)");
      return;
    };

    TheDT.D = DMYtoStDate(D, M, Y, 0);
    if (TheDT.D == BadDate) {
      ShowMessage("Invalid date");
      return;
    };
    TheDT.T = CurrentTime();

    ObsLong = StrToFloat(LongEF->Text);
    if ((ObsLong < -180) || (ObsLong > 180)) {
      ShowMessage("Longitude out of range (-180..180)");
      return;
    };

    ObsLat = StrToFloat(LatEF->Text);
    if ((ObsLat < -90) || (ObsLat > 90)) {
      ShowMessage("Latitude out of range (-90..90)");
      return;
    };

    PositionsLB->Clear();
    DoFixedCalcs();
    DoCalcs(ObsLong, ObsLat);
  }
  catch(...) {
    ShowMessage("One or more entry fields has illegal data");
  };
};
//---------------------------------------------------------------------------