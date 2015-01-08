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
#ifndef BcdCalUH
#define BcdCalUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\clipbrd.hpp>
#include <vcl\SysUtils.hpp>

#include "math.h"
#include "StBcd.hpp"
#include "StStrL.hpp"
#include <Buttons.hpp>
#include <Menus.hpp>

//---------------------------------------------------------------------------
typedef Set <char, 0, 255> BCDCharSet;
typedef Set <char, 0, 255> BCDOperSet;

//---------------------------------------------------------------------------
class PACKAGE TBCDCalcDlg : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
   TBitBtn *ZeroBtn;
   TBitBtn *DecKey;
   TBitBtn *ThreeKey;
   TBitBtn *OneKey;
   TBitBtn *TwoKey;
   TBitBtn *SixKey;
   TBitBtn *FourKey;
   TBitBtn *FiveKey;
   TBitBtn *NineKey;
   TBitBtn *SevenKey;
   TBitBtn *EightKey;
   TBitBtn *SqrtBtn;
   TBitBtn *LnBtn;
   TBitBtn *ExpBtn;
   TBitBtn *XtoYBtn;
   TBitBtn *AddBtn;
   TBitBtn *SubBtn;
   TBitBtn *MulBtn;
   TBitBtn *DivBtn;
   TBitBtn *PlusMinusBtn;
   TBitBtn *ClearBtn;
   TBitBtn *EqualBtn;
   TBitBtn *ClearEntryBtn;
   TGroupBox *gb1;
   TEdit *BCDString;
   TBitBtn *BSBtn;
   TMemo *Memo1;
   TPopupMenu *PopupMenu1;
   TMenuItem *Copy1;
   TMenuItem *Paste1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormKeyPress(TObject *Sender, char &Key);
	void __fastcall ClearBtnClick(TObject *Sender);
	void __fastcall ClearEntryBtnClick(TObject *Sender);
	void __fastcall SevenKeyClick(TObject *Sender);
	void __fastcall EightKeyClick(TObject *Sender);
	void __fastcall NineKeyClick(TObject *Sender);
	void __fastcall FourKeyClick(TObject *Sender);
	void __fastcall FiveKeyClick(TObject *Sender);
	void __fastcall SixKeyClick(TObject *Sender);
	void __fastcall OneKeyClick(TObject *Sender);
	void __fastcall TwoKeyClick(TObject *Sender);
	void __fastcall ThreeKeyClick(TObject *Sender);
	void __fastcall ZeroBtnClick(TObject *Sender);
	void __fastcall DecKeyClick(TObject *Sender);
	void __fastcall PlusMinusBtnClick(TObject *Sender);
	void __fastcall AddBtnClick(TObject *Sender);
	void __fastcall SubBtnClick(TObject *Sender);
	void __fastcall MulBtnClick(TObject *Sender);
	void __fastcall DivBtnClick(TObject *Sender);
	void __fastcall SqrtBtnClick(TObject *Sender);
	void __fastcall ExpBtnClick(TObject *Sender);
	void __fastcall LnBtnClick(TObject *Sender);
	void __fastcall XtoYBtnClick(TObject *Sender);
	void __fastcall EqualBtnClick(TObject *Sender);
   void __fastcall Copy1Click(TObject *Sender);
   void __fastcall Paste1Click(TObject *Sender);
   void __fastcall BSBtnClick(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TBCDCalcDlg(TComponent* Owner);

        BCDCharSet BCDChar;
        BCDOperSet BCDOper;
        char PendOp;
        int DFHold;
        String XBuffer;
        bool ClearOnNext;
};
//---------------------------------------------------------------------------
extern PACKAGE TBCDCalcDlg *BCDCalcDlg;
//---------------------------------------------------------------------------
#endif
