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
#ifndef ExBarCUH
#define ExBarCUH
//---------------------------------------------------------------------------
#include <vcl\Classes.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include "StDbBarC.hpp"
#include "StBarC.hpp"
#include <vcl\Tabnotbk.hpp>
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\DBGrids.hpp>
#include "Grids.hpp"
#include <vcl\Dialogs.hpp>
#include <vcl\DB.hpp>
#include <vcl\DBTables.hpp>
#include <vcl\Printers.hpp>

#include "StBarC.hpp"
#include <Db.hpp>
#include "St2DBarC.hpp"

//---------------------------------------------------------------------------
class PACKAGE TForm1 : public TForm
{
__published:	// IDE-managed Components
	TStDbBarCode *StDbBarCode1;
	TLabel *Label4;
	TTabbedNotebook *NB;
	TStBarCode *BarCode1;
	TLabel *Label1;
	TEdit *edCode1;
	TEdit *edSupp;
	TRadioGroup *rgType;
	TCheckBox *cbSupp;
	TCheckBox *cbTallGuardBars;
	TCheckBox *cbShowCode;
	TButton *btnUpdate1;
	TStBarCode *BarCode2;
	TLabel *Label2;
	TCheckBox *cbBearerBars;
	TCheckBox *cbShowCode2;
	TEdit *edCode2;
	TButton *btnUpdate2;
	TLabel *Label3;
	TStBarCode *BarCode3;
	TEdit *edCode3;
	TButton *btnUpdate3;
	TCheckBox *cbShowCode3;
	TCheckBox *cbShowGuardChars3;
	TStBarCode *BarCode4;
	TLabel *Label5;
	TCheckBox *cbShowCode4;
	TButton *btnUpdate4;
	TEdit *edCode4;
	TStBarCode *BarCode5;
	TLabel *Label6;
	TEdit *edCode5;
	TButton *btnUpdate5;
	TCheckBox *cbShowCode5;
	TStBarCode *BarCode6;
	TLabel *Label7;
	TEdit *edCode6;
	TButton *btnUpdate6;
	TCheckBox *cbShowCode6;
	TStBarCode *BarCode7;
	TLabel *Label8;
	TEdit *edCode7;
	TButton *btnUpdate7;
	TCheckBox *cbShowCode7;
	TButton *btnCopy;
	TButton *btnPrint;
	TButton *btnClose;
	TDBGrid *DBGrid1;
	TButton *btnSave;
	TPrintDialog *PrintDialog1;
	TDataSource *DataSource1;
        TTable *Table1;
    TLabel *Label9;
    TEdit *edCodePDF417;
    TButton *btnUpdatePDF417;
    TStPDF417Barcode *StPDF417Barcode1;
    TCheckBox *cbShowCodePDF417;
    TCheckBox *cbPDF417Truncated;
    TLabel *Label10;
    TEdit *edCodeMaxiCode;
    TStMaxiCodeBarcode *StMaxiCodeBarcode1;
    TCheckBox *cbShowCodeMaxiCode;
    TButton *btnUpdateMaxiCode;
    TLabel *Label11;
    TRadioButton *radMCMode23;
    TRadioButton *radMCMode4;
    TRadioButton *radMCMode5;
    TLabel *Label13;
    TLabel *Label12;
    TLabel *Label14;
    TEdit *edMCServiceClass;
    TEdit *edMCPostalCode;
    TEdit *edMCCountryCode;
    TButton *btnValidate;
	void __fastcall btnPrintClick(TObject *Sender);
	void __fastcall btnValidateClick(TObject *Sender);
	void __fastcall btnSaveClick(TObject *Sender);
	void __fastcall btnCopyClick(TObject *Sender);
	void __fastcall btnCloseClick(TObject *Sender);
	void __fastcall btnUpdate1Click(TObject *Sender);
	void __fastcall btnUpdate2Click(TObject *Sender);
	void __fastcall btnUpdate3Click(TObject *Sender);
	void __fastcall btnUpdate4Click(TObject *Sender);
	void __fastcall btnUpdate5Click(TObject *Sender);
	void __fastcall btnUpdate6Click(TObject *Sender);
	void __fastcall btnUpdate7Click(TObject *Sender);
    void __fastcall edCodePDF417Exit(TObject *Sender);
    
    void __fastcall btnUpdateMaxiCodeClick(TObject *Sender);
    void __fastcall radMCMode23Click(TObject *Sender);
    void __fastcall radMCMode4Click(TObject *Sender);
    void __fastcall radMCMode5Click(TObject *Sender);
    void __fastcall NBChange(TObject *Sender, int NewTab,
          bool &AllowChange);
private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
