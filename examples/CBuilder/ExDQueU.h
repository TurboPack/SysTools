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
#ifndef ExDQueUH
#define ExDQueUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>

#include "StDQue.hpp"
#include <vcl\Dialogs.hpp>

//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TButton *CreateBtn;
	TEdit *Edit1;
	TButton *PushHeadBtn;
	TButton *PopHeadBtn;
	TButton *HeadBtn;
	TButton *TailBtn;
	TListBox *LB1;
	TButton *LoadBtn;
	TButton *SaveBtn;
	TButton *ClearBtn;
	TButton *PushTailBtn;
	TButton *PopTailBtn;
	TOpenDialog *OD1;
	TSaveDialog *SD1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall CreateBtnClick(TObject *Sender);
	void __fastcall ClearBtnClick(TObject *Sender);
	void __fastcall PushHeadBtnClick(TObject *Sender);
	void __fastcall PopHeadBtnClick(TObject *Sender);
	void __fastcall PushTailBtnClick(TObject *Sender);
	void __fastcall PopTailBtnClick(TObject *Sender);
	void __fastcall HeadBtnClick(TObject *Sender);
	void __fastcall TailBtnClick(TObject *Sender);
	void __fastcall LoadBtnClick(TObject *Sender);
	void __fastcall SaveBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        TStDQue *MyDQue;
        
	__fastcall TForm1(TComponent* Owner);
    	void FillListBox(void);
        void UpdateButtons(bool QueOK);
};
//---------------------------------------------------------------------------
extern TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
