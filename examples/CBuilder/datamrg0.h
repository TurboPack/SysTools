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
#ifndef datamrg0H
#define datamrg0H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>

#include "StTxtDat.hpp"
#include "StMerge.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
   TSplitter *Splitter1;
   TSplitter *Splitter2;
   TPanel *Panel1;
   TButton *Button1;
   TButton *Button2;
   TButton *Button3;
   TPanel *Panel5;
   TSpeedButton *SpeedButton1;
   TSpeedButton *SpeedButton2;
   TSpeedButton *SpeedButton3;
   TSpeedButton *SpeedButton4;
   TButton *Button4;
   TButton *Button5;
   TButton *Button6;
   TPanel *Panel2;
   TMemo *Memo1;
   TPanel *Panel3;
   TMemo *Memo2;
   TPanel *Panel4;
   TMemo *Memo3;
   TOpenDialog *OpenDialog1;
   TOpenDialog *OpenDialog2;
   TOpenDialog *OpenDialog3;
   TSaveDialog *SaveDialog1;
   TSaveDialog *SaveDialog2;
   void __fastcall FormCreate(TObject *Sender);
   void __fastcall FormDestroy(TObject *Sender);
   void __fastcall ClearMemo(TObject *Sender);
   void __fastcall NavClick(TObject *Sender);
   void __fastcall Button1Click(TObject *Sender);
   void __fastcall Button2Click(TObject *Sender);
   void __fastcall Button3Click(TObject *Sender);
   void __fastcall Button4Click(TObject *Sender);
   void __fastcall Button5Click(TObject *Sender);
   void __fastcall Button6Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TForm1(TComponent* Owner);
   void __fastcall DoUnknownTag(TObject *Sender, AnsiString Tag, AnsiString &Value, bool &Discard);
   void __fastcall DisableButtons(void);
   void __fastcall UpdateButtons(void);
   void __fastcall UpdateTagDisplay(void);
   String __fastcall NextFile(void);

    String TemplateName;
    int MergeNo;
    TStTextDataSchema* Schema;
    TStTextDataRecordSet* DataSet;
    TStTextMerge* Merger;
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
