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
#ifndef ExBitsUH
#define ExBitsUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Dialogs.hpp>

#include "StBits.hpp"
//---------------------------------------------------------------------------
class PACKAGE TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label2;
	TLabel *Label1;
	TButton *CreateBtn;
	TEdit *NumElemsValue;
	TButton *ClearAllBtn;
	TButton *SetAllBtn;
	TButton *InvertAllBtn;
	TButton *SetBitBtn;
	TEdit *SetBitValue;
	TButton *ClearBitBtn;
	TEdit *ClearBitValue;
	TButton *IsBitSetBtn;
	TEdit *IsBitSetValue;
	TButton *ControlBitBtn;
	TEdit *ControlBitValue;
	TCheckBox *BitOnCB;
	TButton *ToggleBitBtn;
	TEdit *ToggleBitValue;
	TMemo *Msg1;
	TButton *LoadBtn;
	TButton *SaveBtn;
	TOpenDialog *OD1;
	TSaveDialog *SD1;
	void __fastcall CreateBtnClick(TObject *Sender);
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall IsBitSetBtnClick(TObject *Sender);
	void __fastcall ControlBitBtnClick(TObject *Sender);
	void __fastcall SetAllBtnClick(TObject *Sender);
	void __fastcall InvertAllBtnClick(TObject *Sender);
	void __fastcall ClearAllBtnClick(TObject *Sender);
	void __fastcall ToggleBitBtnClick(TObject *Sender);
	void __fastcall SetBitBtnClick(TObject *Sender);
	void __fastcall ClearBitBtnClick(TObject *Sender);
	void __fastcall LoadBtnClick(TObject *Sender);
	void __fastcall SaveBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
       TStBits* MyBits;
	__fastcall TForm1(TComponent* Owner);
        void UpdateButtons(bool BitsOK);
        bool CheckValue(String S, long &N);
        String GetTFString(long N);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
   