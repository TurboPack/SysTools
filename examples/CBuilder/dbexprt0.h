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
#ifndef dbexprt0H
#define dbexprt0H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>

#include "StExport.hpp"
#include <Db.hpp>
#include <DBTables.hpp>
#include <Dialogs.hpp>

#define   DefaultCaption "Table Export Example"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
   TLabel *Label1;
   TMemo *Memo1;
   TButton *Button2;
   TButton *Button3;
   TButton *Button4;
   TCheckBox *CheckBox1;
   TButton *Button1;
   TEdit *Edit1;
   TOpenDialog *OpenDialog1;
   TTable *Table1;
   TSaveDialog *SaveDialog1;
   void __fastcall Button1Click(TObject *Sender);
   void __fastcall Button2Click(TObject *Sender);
   void __fastcall Button3Click(TObject *Sender);
   void __fastcall Button4Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TForm1(TComponent* Owner);
   void __fastcall TForm1::SetCaption(String FN);

  String CurrentDb;
  TStDBtoCSVExport* Exporter;
  TStDbSchemaGenerator* SchemaMaker;


};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
