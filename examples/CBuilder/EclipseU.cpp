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

#include "EclipseU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "StEclpse"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void TForm1::WriteLunarData(TStEclipseRecord Eclipse)
{
  String S;

  switch (Eclipse.EType) {
    case etLunarPenumbral : {Memo1->Lines->Add("Lunar - Penumbra"); break;};
    case etLunarPartial   : {Memo1->Lines->Add("Lunar - Partial"); break;};
    case etLunarTotal     : {Memo1->Lines->Add("Lunar - Total"); break;};
  };
  S = Format("%0:3f", OPENARRAY(TVarRec, (Eclipse.Magnitude)));

  Memo1->Lines->Add("Mag: " + S);
  Memo1->Lines->Add("Penumbral Starts: " + DateTimeToStr(Eclipse.LContacts.UT1));
  Memo1->Lines->Add("First  Contact:   " + DateTimeToStr(Eclipse.LContacts.FirstContact));
  Memo1->Lines->Add("Second Contact:   " + DateTimeToStr(Eclipse.LContacts.SecondContact));
  Memo1->Lines->Add("Mid Eclipse       " + DateTimeToStr(Eclipse.LContacts.MidEclipse));
  Memo1->Lines->Add("Third  Contact:   " + DateTimeToStr(Eclipse.LContacts.ThirdContact));
  Memo1->Lines->Add("Fourth Contact:   " + DateTimeToStr(Eclipse.LContacts.FourthContact));
  Memo1->Lines->Add("Penumbral Ends:   " + DateTimeToStr(Eclipse.LContacts.UT2));

  Memo1->Lines->Add("");
  Memo1->Lines->Add("");
  Memo1->Lines->Add("");
}
//---------------------------------------------------------------------------
void TForm1::WriteSolarData(TStEclipseRecord Eclipse)
{
  int I;
  String S, P;
  TStLongLat LL;

  switch (Eclipse.EType) {
    case etSolarPartial :
      {
        Memo1->Lines->Add("Solar - Partial");
        S = Format("%0:3f", OPENARRAY(TVarRec, (Eclipse.Magnitude)));
        Memo1->Lines->Add("Mag: " + S);
        if (Eclipse.Hemisphere == htNorthern) {
          Memo1->Lines->Add("Hemisphere: Northern");
        }
        else {
          Memo1->Lines->Add("Hemisphere: Southern");
        }
        Memo1->Lines->Add("Mid Eclipse: " +
               DateTimeToStr(Eclipse.LContacts.MidEclipse));
        break;
      };
    case etSolarTotal :
      {
        Memo1->Lines->Add("Solar - Total");
        Memo1->Lines->Add("Mag: N/A");
        if (Eclipse.Hemisphere == htNorthern) {
          Memo1->Lines->Add("Hemisphere: Northern");
        }
        else {
          Memo1->Lines->Add("Hemisphere: Southern");
        }
        Memo1->Lines->Add("Mid Eclipse: " +
               DateTimeToStr(Eclipse.LContacts.MidEclipse));
        break;
      };
    case etSolarAnnularTotal :
      {
        S = Format("%0:3f", OPENARRAY(TVarRec, (Eclipse.Magnitude)));
        Memo1->Lines->Add("Mag: N/A");
        if (Eclipse.Hemisphere == htNorthern) {
          Memo1->Lines->Add("Hemisphere: Northern");
        }
        else {
          Memo1->Lines->Add("Hemisphere: Southern");
        }
        Memo1->Lines->Add("Mid Eclipse: " +
               DateTimeToStr(Eclipse.LContacts.MidEclipse));
        break;
      }
    case etSolarAnnular :
      {
        Memo1->Lines->Add("Solar - Annular");
        Memo1->Lines->Add("Mag: N/A");
        if (Eclipse.Hemisphere == htNorthern) {
          Memo1->Lines->Add("Hemisphere: Northern");
        }
        else {
          Memo1->Lines->Add("Hemisphere: Southern");
        }
        Memo1->Lines->Add("Mid Eclipse: " +
               DateTimeToStr(Eclipse.LContacts.MidEclipse));
        break;
      }
  };

  if (Eclipse.Path) {
    for (I = 0; I < Eclipse.Path->Count; I++) {
      LL = *(TStLongLat*)Eclipse.Path->Items[I]->Data;
      P = "  " + DateTimeToStr(LL.JD) + "   ";

      S = Format("%0:2f", OPENARRAY(TVarRec, (LL.Longitude)));
      P = P + S + "   ";

      S = Format("%0:2f", OPENARRAY(TVarRec, (LL.Latitude)));
      P = P + S + "   ";

      S = Format("%0:2f", OPENARRAY(TVarRec, (LL.Duration)));
      P = P + S;
      Memo1->Lines->Add(P);
    };
  };

  Memo1->Lines->Add("");
  Memo1->Lines->Add("");
  Memo1->Lines->Add("");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  int I;
  TStEclipseRecord Eclipse;

  Memo1->Clear();
  TStEclipses* Data = new TStEclipses(__classid(TStListNode));
  try {
    Data->FindEclipses(StrToInt(YearEF->Text));
    for (I = 0; I < Data->Count; I++) {
      Eclipse = (TStEclipseRecord)*Data->Eclipses[I];
      if ((Eclipse.EType == etLunarPenumbral) || (Eclipse.EType == etLunarPartial) ||
          (Eclipse.EType == etLunarTotal)) {
        WriteLunarData(Eclipse);
      }
      else {
        WriteSolarData(Eclipse);
      }  
    }
    delete Data;
  }
  catch(...) {
    delete Data;
    throw;
  };
}
//---------------------------------------------------------------------------

