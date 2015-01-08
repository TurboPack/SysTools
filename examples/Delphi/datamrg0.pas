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

unit datamrg0;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, 

  StTxtDat, StMerge;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenDialog3: TOpenDialog;
    Button2: TButton;
    Button3: TButton;
    Panel5: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Button4: TButton;
    SaveDialog1: TSaveDialog;
    Button5: TButton;
    SaveDialog2: TSaveDialog;
    Button6: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure NavClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ClearMemo(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    procedure UpdateButtons;
    procedure UpdateTagDisplay;
    function NextFile: string;
    procedure DoUnknownTag(Sender: TObject; Tag: AnsiString;
      var Value: AnsiString; var Discard: Boolean);
    procedure DisableButtons;
    { Private declarations }
  public
    TemplateName : string;
    MergeNo : Integer;
    Schema  : TStTextDataSchema;
    DataSet : TStTextDataRecordSet;
    Merger  : TStTextMerge;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog2.Execute and OpenDialog3.Execute then begin
    Schema.Free;
    Schema := TStTextDataSchema.Create;
    Schema.LoadFromFile(OpenDialog2.FileName);

    DataSet.Free;
    DataSet := TStTextDataRecordSet.Create;
    DataSet.Schema := Schema;
    DataSet.LoadFromFile(OpenDialog3.FileName);
    DataSet.First;

    UpdateButtons;
    UpdateTagDisplay;

  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Schema.Free;
  DataSet.Free;
  Merger.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    TemplateName := OpenDialog1.FileName;
    MergeNo := 1;
    Merger.LoadTemplateFromFile(TemplateName);
    Memo3.Lines.Assign(Merger.Template);
  end;
end;

procedure TForm1.UpdateTagDisplay;
begin
  Memo2.Lines.Assign(DataSet.CurrentRecord.FieldList);
end;

procedure TForm1.DisableButtons;
begin
  SpeedButton1.Enabled := False;
  SpeedButton2.Enabled := False;
  SpeedButton3.Enabled := False;
  SpeedButton4.Enabled := False;
end;

procedure TForm1.UpdateButtons;
begin
  if DataSet.Active then begin

    SpeedButton1.Enabled := True;
    SpeedButton2.Enabled := True;
    SpeedButton3.Enabled := True;
    SpeedButton4.Enabled := True;

    if DataSet.BOF then begin
      SpeedButton1.Enabled := False;
      SpeedButton2.Enabled := False;
    end;

    if DataSet.EOF then begin
      SpeedButton3.Enabled := False;
      SpeedButton4.Enabled := False;
    end;

  end else
    DisableButtons;
end;

procedure TForm1.NavClick(Sender: TObject);
begin
  if Sender = SpeedButton1 then DataSet.First;
  if Sender = SpeedButton2 then DataSet.Prior;
  if Sender = SpeedButton3 then DataSet.Next;
  if Sender = SpeedButton4 then DataSet.Last;

  UpdateButtons;
  UpdateTagDisplay;
end;

function TForm1.NextFile : string;
begin
  Result := ChangeFileExt(ExtractFileName(TemplateName),
     Format('.M%.2d', [MergeNo]));
  Inc(MergeNo);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  SaveDialog1.FileName := NextFile;
  if SaveDialog1.Execute then begin
    Memo3.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.DoUnknownTag(Sender : TObject; Tag : AnsiString;
    var Value : AnsiString; var Discard : Boolean);
begin
  if Tag = 'TIME' then
    Value := FormatDateTime('hh:mm:ss', Now)
  else
    Value := InputBox('Unknown Tag', 'Value for ' + Tag, '');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Merger := TStTextMerge.Create;
  Merger.DefaultTags.Add('FIRST_NAME=Sir or Madam');
  Merger.DefaultTags.Add('CITY=ANYTOWN');
  Merger.DefaultTags.Add('COLOR=BLUE');
  Merger.OnGotUnknownTag := DoUnknownTag;

  DisableButtons;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Merger.MergeTags.Assign(Memo2.Lines);
  Merger.Merge;
  Memo1.Lines.Assign(Merger.MergedText);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  SaveDialog2.FileName := TemplateName;
  if SaveDialog2.Execute then begin
    TemplateName := SaveDialog2.FileName;
    Memo3.Lines.SaveToFile(TemplateName);
    Merger.Template.Assign(Memo3.Lines);
  end;
end;

procedure TForm1.ClearMemo(Sender: TObject);
begin
  (Sender as TMemo).Lines.Clear;
end;


procedure TForm1.Button6Click(Sender: TObject);
begin
  if Assigned(DataSet) and DataSet.Active then begin
    DataSet.Active := False;
    DataSet.Free;
    DataSet := nil;
    Memo2.Lines.Clear;
    DisableButtons;
  end;
end;

end.
