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
#ifndef ExRndUH
#define ExRndUH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <StRandom.hpp>
//---------------------------------------------------------------------------
typedef double __fastcall (__closure *TGetRandom)();

class TForm1 : public TForm
{
__published:	// IDE-managed Components
    TComboBox *cboDist;
    TLabel *lblPrompt;
    TLabel *lblParameters;
    TLabel *lblParm1;
    TLabel *lblParm2;
    TEdit *edtParm1;
    TEdit *edtParm2;
    TButton *btnGenerate;
    TImage *imgGraph;
    TProgressBar *prgGenProgress;
    TUpDown *updRight;
    TUpDown *updLeft;
    TLabel *lblLeft;
    TLabel *lblRight;
    TLabel *lblMaxY;
    void __fastcall FormCreate(TObject *Sender);
    
    void __fastcall cboDistChange(TObject *Sender);
    void __fastcall updRightClick(TObject *Sender, TUDBtnType Button);
    void __fastcall updLeftClick(TObject *Sender, TUDBtnType Button);
    void __fastcall btnGenerateClick(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
public:		// User declarations
    double GraphLeft;
    double GraphRight;
    double Value1;
    double Value2;
    TStRandomBase *PRNG;
    TGetRandom GetRandom;

    __fastcall TForm1(TComponent* Owner);

    void __fastcall GenerateGraph(int aDistInx);

    void __fastcall PrepForBeta();
    void __fastcall PrepForCauchy();
    void __fastcall PrepForChiSquared();
    void __fastcall PrepForErlang();
    void __fastcall PrepForExponential();
    void __fastcall PrepForF();
    void __fastcall PrepForGamma();
    void __fastcall PrepForLogNormal();
    void __fastcall PrepForNormal();
    void __fastcall PrepForT();
    void __fastcall PrepForUniform();
    void __fastcall PrepForWeibull();

    double __fastcall GetBeta();
    double __fastcall GetCauchy();
    double __fastcall GetChiSquared();
    double __fastcall GetErlang();
    double __fastcall GetExponential();
    double __fastcall GetF();
    double __fastcall GetGamma();
    double __fastcall GetLogNormal();
    double __fastcall GetNormal();
    double __fastcall GetT();
    double __fastcall GetUniform();
    double __fastcall GetWeibull();

};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
