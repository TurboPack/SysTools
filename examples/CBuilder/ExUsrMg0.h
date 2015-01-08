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
#ifndef ExUsrMg0H
#define ExUsrMg0H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "StNet.hpp"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
    TSplitter *Splitter1;
    TPanel *Panel1;
    TListView *ListView1;
    TPanel *Panel2;
    TListView *ListView2;
    TMainMenu *MainMenu1;
    TMenuItem *User1;
    TMenuItem *NewUser1;
    TMenuItem *NewGlobalGroup1;
    TMenuItem *NewLocalGroup1;
    TMenuItem *N1;
    TMenuItem *Delete1;
    TMenuItem *Properties1;
    TMenuItem *N2;
    TMenuItem *SelectServer1;
    TMenuItem *N3;
    TMenuItem *Exit1;
    TMenuItem *About1;
    void __fastcall SelectServer1Click(TObject *Sender);
    void __fastcall About1Click(TObject *Sender);
    void __fastcall Properties1Click(TObject *Sender);
    void __fastcall Delete1Click(TObject *Sender);
    void __fastcall ListViewKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
    void __fastcall NewUser1Click(TObject *Sender);
    void __fastcall NewGlobalGroup1Click(TObject *Sender);
    void __fastcall NewLocalGroup1Click(TObject *Sender);
    void __fastcall Exit1Click(TObject *Sender);
private:	// User declarations
    TStNetServerItem* CS;
    TStringList* UL;
    TStringList* GL;
public:		// User declarations
    __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
