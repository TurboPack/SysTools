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
#ifndef ExDictUH
#define ExDictUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Dialogs.hpp>

#include "StDict.hpp"

//---------------------------------------------------------------------------
class PACKAGE TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label9;
	TLabel *Label10;
	TLabel *Label1;
	TButton *CreateBtn;
	TButton *ClearBtn;
	TListBox *LB1;
	TEdit *Edit1;
	TEdit *Edit2;
	TButton *AddBtn;
	TButton *DelBtn;
	TButton *ExistsBtn;
	TButton *UpdateBtn;
	TButton *SaveBtn;
	TButton *LoadBtn;
	TOpenDialog *OD1;
	TSaveDialog *SD1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall LB1Click(TObject *Sender);
	void __fastcall LB1DblClick(TObject *Sender);
	void __fastcall CreateBtnClick(TObject *Sender);
	void __fastcall ClearBtnClick(TObject *Sender);
	void __fastcall AddBtnClick(TObject *Sender);
	void __fastcall UpdateBtnClick(TObject *Sender);
	void __fastcall ExistsBtnClick(TObject *Sender);
	void __fastcall DelBtnClick(TObject *Sender);
	void __fastcall LoadBtnClick(TObject *Sender);
	void __fastcall SaveBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        TStDictionary* MyDD;

	__fastcall TForm1(TComponent* Owner);
        String RandomData(void);
        void UpdateButtons(bool DOK);
        void FillListBox(void);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
