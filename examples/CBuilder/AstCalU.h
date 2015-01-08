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
#ifndef AstCalUH
#define AstCalUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <math.h>
#include <StDate.hpp>
#include <StAstro.hpp>
#include <StAstroP.hpp>

//---------------------------------------------------------------------------
class PACKAGE TForm1 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label6;
	TButton *Button1;
	TEdit *MonthEF;
	TEdit *DateEF;
	TEdit *YearEF;
	TEdit *LongEF;
	TEdit *LatEF;
	TGroupBox *GB1;
	TLabel *Label7;
	TLabel *Label8;
	TEdit *LocalTimeEF;
	TEdit *SiderealTimeEF;
	TGroupBox *GB2;
	TLabel *Label9;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *Label12;
	TLabel *Label13;
	TEdit *SunRiseEF;
	TEdit *MoonRiseEF;
	TEdit *SunSetEF;
	TEdit *MoonSetEF;
	TEdit *TwiStartEF;
	TEdit *TwiEndEF;
	TGroupBox *GB3;
	TListBox *PositionsLB;
	THeader *Header1;
	TGroupBox *GB4;
	TLabel *Label14;
	TLabel *Label15;
	TLabel *Label16;
	TLabel *Label17;
	TLabel *PhaseLabel;
	TEdit *NMFirstDate;
	TEdit *FQFirstDate;
	TEdit *NMFirstTime;
	TEdit *FQFirstTime;
	TEdit *FMFirstDate;
	TEdit *LQFirstDate;
	TEdit *LQFirstTime;
	TEdit *FMFirstTime;
	TEdit *NMSecondDate;
	TEdit *NMSecondTime;
	TEdit *FQSecondDate;
	TEdit *FQSecondTime;
	TEdit *FMSecondDate;
	TEdit *FMSecondTime;
	TEdit *LQSecondTime;
	TEdit *LQSecondDate;
	TEdit *NMPrevDate;
	TEdit *FQPrevDate;
	TEdit *FMPrevDate;
	TEdit *LQPrevDate;
	TEdit *LQPrevTime;
	TEdit *FMPrevTime;
	TEdit *FQPrevTime;
	TEdit *NMPrevTime;
	TEdit *NMNextDate;
	TEdit *FQNextDate;
	TEdit *FMNextDate;
	TEdit *LQNextDate;
	TEdit *LQNextTime;
	TEdit *FMNextTime;
	TEdit *FQNextTime;
	TEdit *NMNextTime;
	THeader *Header2;
	TGroupBox *GB5;
	TLabel *Label18;
	TLabel *Label19;
	TLabel *Label20;
	TLabel *Label21;
	TLabel *Label22;
	TLabel *Label23;
	TEdit *SpringTime;
	TEdit *SummerTime;
	TEdit *SummerDate;
	TEdit *SpringDate;
	TEdit *FallTime;
	TEdit *WinterTime;
	TEdit *WinterDate;
	TEdit *FallDate;
	TEdit *EasterEF;
	TEdit *SunlightEF;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
        void __fastcall DoCalcTimes();
        void __fastcall DoFixedCalcs();
        void __fastcall DoCalcs(double ObsLong, double ObsLat);
public:		// User declarations
        TStDateTimeRec TheDT;
    	TStRiseSetRec RS;
        int D, M, Y;
        double ObsLat, ObsLong;

	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
