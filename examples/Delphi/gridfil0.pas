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

unit gridfil0;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls,

  StTxtDat;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    StringGrid1: TStringGrid;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ClearGrid(ClearCaptions: Boolean);
    procedure FillCaptions;
    procedure FillCells;
    { Private declarations }
  public
    Schema : TStTextDataSchema;
    DataSet : TStTextDataRecordSet;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ClearGrid(ClearCaptions : Boolean);
var
  i : Integer;
begin
  if ClearCaptions then
    StringGrid1.Rows[0].Clear;
  for i := 1 to Pred(StringGrid1.RowCount) do
    StringGrid1.Rows[i].Clear;
end;

procedure TForm1.FillCaptions;
begin
  StringGrid1.ColCount := Schema.Captions.Count;
  StringGrid1.Rows[0].Assign(Schema.Captions);
end;

procedure TForm1.FillCells;
var
  i : Integer;
begin
  StringGrid1.RowCount := DataSet.Count + 1;
  i := 1;
  DataSet.First;

  while not DataSet.EOF do begin
    StringGrid1.Rows[i].Assign(DataSet.CurrentRecord.Values);
    DataSet.Next;
    Inc(i);
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    ClearGrid(True);
    Schema.Free;
    Schema := TStTextDataSchema.Create;
    Schema.LoadFromFile(OpenDialog1.FileName);
    FillCaptions;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog2.Execute then begin
    ClearGrid(False);
    DataSet.Free;
    DataSet := TStTextDataRecordSet.Create;
    DataSet.Schema := Schema;
    DataSet.LoadFromFile(OpenDialog2.FileName);
    FillCells;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Schema.Free;
  DataSet.Free;
end;

end.
