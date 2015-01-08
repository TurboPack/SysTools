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
#ifndef exregeu1H
#define exregeu1H
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\Dialogs.hpp>
#include "StRegEx.hpp"
#include "cgauges.h"
#include "StBase.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TGroupBox *gbOptions;
	TCheckBox *cbSelect;
	TCheckBox *cbIgnoreCase;
	TCheckBox *cbLineNumbers;
	TCheckBox *cbxModOnly;
	TCheckBox *cbxCountOnly;
	TLabel *Label1;
	TEdit *edtSourceFile;
	TSpeedButton *sbSource;
	TLabel *Label2;
	TEdit *edtDestFile;
	TSpeedButton *sbDest;
	TButton *btnReplace;
	TButton *btnMatch;
	TButton *bntSelAvoid;
	TMemo *Memo1;
	TLabel *lblSelAvoid;
	TLabel *lblLPS;
	TLabel *lblMatch;
	TLabel *lblReplace;
	TButton *Button1;
	TOpenDialog *OpenDialog1;
	TStRegEx *StRegEx1;


   TCGauge *Gauge1;
	void __fastcall bntSelAvoidClick(TObject *Sender);
	void __fastcall btnMatchClick(TObject *Sender);
	void __fastcall btnReplaceClick(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall SelectFile(TObject *Sender);
	void __fastcall StRegEx1Match(TObject *Sender, TMatchPosition &REPosition);
   void __fastcall StRegEx1Progress(TObject *Sender, WORD Percent);
private:	// User declarations
public:		// User declarations
  long ACount;

	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
