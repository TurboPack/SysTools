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
#ifndef ExCollUH
#define ExCollUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Dialogs.hpp>

#include "STColl.hpp"
//---------------------------------------------------------------------------
#define MaxElem 20000

#pragma pack(push, 1)
struct ARecord
{
    String First;
    String Last;
    int Age;
} ;
#pragma pack(pop)

class PACKAGE TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label8;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TButton *CreateBtn;
	TListBox *LB1;
	TButton *ClearBtn;
	TButton *PackBtn;
	TButton *EffBtn;
	TEdit *Edit1;
	TEdit *Edit3;
	TEdit *Edit2;
	TButton *AtBtn;
	TButton *AtInsBtn;
	TButton *AtPutBtn;
	TButton *DelBtn;
	TButton *AtDelBtn;
	TButton *InsBtn;
	TEdit *Edit4;
	TEdit *Edit5;
	TButton *LoadBtn;
	TButton *SaveBtn;
	TOpenDialog *OD1;
	TSaveDialog *SD1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall CreateBtnClick(TObject *Sender);
	void __fastcall ClearBtnClick(TObject *Sender);
	void __fastcall PackBtnClick(TObject *Sender);
	void __fastcall AtBtnClick(TObject *Sender);
	void __fastcall AtInsBtnClick(TObject *Sender);
	void __fastcall AtPutBtnClick(TObject *Sender);
	void __fastcall DelBtnClick(TObject *Sender);
	void __fastcall AtDelBtnClick(TObject *Sender);
	void __fastcall InsBtnClick(TObject *Sender);
	void __fastcall EffBtnClick(TObject *Sender);
	void __fastcall LoadBtnClick(TObject *Sender);
	void __fastcall SaveBtnClick(TObject *Sender);
	void __fastcall LB1Click(TObject *Sender);
	void __fastcall LB1DblClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        String FirstA[8];
        String LastA[8];
        TStCollection* MyCollection;

	__fastcall TForm1(TComponent* Owner);
        void SetBusy(bool B);
        void FillControls(ARecord AR);
        Boolean CheckControls(ARecord &AR);
        void FillListBox(void);
        void UpdateButtons(bool COK);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
