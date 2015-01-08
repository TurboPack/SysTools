(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit dbexprt0;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Db, DBTables,

  StTxtDat, StExport;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    Table1: TTable;
    SaveDialog1: TSaveDialog;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure SetCaption(const FN: string);
    { Private declarations }
  public
    CurrentDb : string;
    Exporter : TStDBtoCSVExport;
    SchemaMaker : TStDbSchemaGenerator;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
  DefaultCaption = 'Table Export Example';

procedure TForm1.SetCaption(const FN : string);
begin
  if FN = '' then
    Caption := DefaultCaption
  else
    Caption := DefaultCaption + ' - [' + FN + ']';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    Table1.Close;
    CurrentDb := OpenDialog1.FileName;
    Table1.TableName := CurrentDb;
    Table1.Open;
    SetCaption(CurrentDb);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CurrentDb := '';
  Table1.Close;
  Table1.TableName := CurrentDb;
  Memo1.Lines.Clear;
  SetCaption(CurrentDb);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Exporter := TStDBtoCSVExport.Create;

  try
    Exporter.DataSet := Table1;
    Exporter.IncludeHeader := CheckBox1.Checked;
    Exporter.FieldDelimiter := StDeEscape(Edit1.Text);

    SaveDialog1.FileName := ChangeFileExt(ExtractFileName(CurrentDb), '.csv');
    SaveDialog1.DefaultExt := 'csv';
    SaveDialog1.Filter := 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*';
    SaveDialog1.Title := 'Save Table Data';
    if SaveDialog1.Execute then begin
      Exporter.ExportToFile(SaveDialog1.FileName);
      Memo1.Lines.LoadFromFile(SaveDialog1.FileName);
    end;

  finally
    Exporter.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SchemaMaker := TStDbSchemaGenerator.Create;

  try
    SchemaMaker.FieldDelimiter := StDeEscape(Edit1.Text);

    SchemaMaker.DataSet := Table1;

    SaveDialog1.FileName := ChangeFileExt(ExtractFileName(CurrentDb), '.sch');
    SaveDialog1.DefaultExt := 'sch';
    SaveDialog1.Filter := 'Schema Files (*.sch)|*.sch|All Files (*.*)|*.*';
    SaveDialog1.Title := 'Save Schema';
    if SaveDialog1.Execute then begin
      SchemaMaker.SchemaName := ChangeFileExt(ExtractFileName(SaveDialog1.FileName), '');
      SchemaMaker.ExportToFile(SaveDialog1.FileName);
      Memo1.Lines.LoadFromFile(SaveDialog1.FileName);
    end;

  finally
    SchemaMaker.Free;
  end;
end;

end.
