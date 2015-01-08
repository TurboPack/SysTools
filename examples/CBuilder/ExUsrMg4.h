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
#ifndef ExUsrMg4H
#define ExUsrMg4H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class TNewGroupForm : public TForm
{
__published:	// IDE-managed Components
    TLabel *Label1;
    TLabel *Label3;
    TButton *OKBtn;
    TButton *CancelBtn;
    TEdit *GroupNameEdit;
    TEdit *CommentEdit;
    void __fastcall OKBtnClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
    __fastcall TNewGroupForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TNewGroupForm *NewGroupForm;
//---------------------------------------------------------------------------
#endif
