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
#ifndef ExUsrMg2H
#define ExUsrMg2H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include "StNet.hpp"
//---------------------------------------------------------------------------
class TGroupPropertiesForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TLabel *Label3;
    TEdit *GroupNameEdit;
    TEdit *CommentEdit;
    TGroupBox *GroupBox1;
    TListView *ListView1;
    TButton *OKBtn;
    TButton *CancelBtn;
    void __fastcall FormShow(TObject *Sender);
private:	// User declarations
public:		// User declarations
    TStNetGroupItem* G;
    TStringList* ML;
    __fastcall TGroupPropertiesForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGroupPropertiesForm *GroupPropertiesForm;
//---------------------------------------------------------------------------
#endif
