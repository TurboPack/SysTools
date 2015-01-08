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
#ifndef Ex1DArrUH
#define Ex1DArrUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Dialogs.hpp>

#include "math.h"
#include "StLArr.hpp"
//---------------------------------------------------------------------------
#pragma pack(push, 1)
struct ARecord
{
    long X, Y;
    Double Mag;
    String Name1;
    String Name2;
} ;
#pragma pack(pop)
//---------------------------------------------------------------------------
class PACKAGE TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label4;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label5;
	TLabel *Label6;
	TButton *CreateBtn;
	TEdit *ElemNum;
	TButton *ClearBtn;
	TButton *FillBtn;
	TButton *PutBtn;
	TButton *GetBtn;
	TButton *SortBtn;
	TListBox *LB1;
	TEdit *Edit1;
	TEdit *Edit2;
	TEdit *Edit3;
	TEdit *Edit4;
	TEdit *Edit5;
	TButton *LoadBtn;
	TButton *SaveBtn;
	TOpenDialog *OD1;
	TSaveDialog *SD1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall LB1DblClick(TObject *Sender);
	void __fastcall CreateBtnClick(TObject *Sender);
	void __fastcall ClearBtnClick(TObject *Sender);
	void __fastcall FillBtnClick(TObject *Sender);
	void __fastcall GetBtnClick(TObject *Sender);
	void __fastcall PutBtnClick(TObject *Sender);
	void __fastcall SortBtnClick(TObject *Sender);
	void __fastcall LoadBtnClick(TObject *Sender);
	void __fastcall SaveBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    TStLArray *MyLArray;
    ARecord ARec;

	__fastcall TForm1(TComponent* Owner);
    void SetBusy(Boolean B);
    void FillControls();
    void FillListBox();
    bool CheckControls(ARecord& AR);
    void UpdateButtons(bool AOK);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
