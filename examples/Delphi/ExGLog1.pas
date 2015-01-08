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

unit ExGLog1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, StBase, StGenLog;

const
  AppleEvent   = 1;
  OrangeEvent  = 2;
  LemonEvent   = 3;
  GrapeEvent   = 4;
  UnknownEvent = 5;

type
  TForm1 = class(TForm)
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Button3: TButton;
    CheckBox1: TCheckBox;
    StGeneralLog1: TStGeneralLog;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure StGeneralLog1GetLogString(Sender: TObject; const D1, D2, D3,
      D4: Integer; var LogString: String);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    StGeneralLog1.WriteMode := wmAppend
  else
    StGeneralLog1.WriteMode := wmOverwrite;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  StGeneralLog1.DumpLog;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  StGeneralLog1.WriteLogString(Edit1.Text);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0 : StGeneralLog1.AddLogEntry(AppleEvent, 0, 0, 0);
    1 : StGeneralLog1.AddLogEntry(OrangeEvent, 0, 0, 0);
    2 : StGeneralLog1.AddLogEntry(LemonEvent, 0, 0, 0);
    3 : StGeneralLog1.AddLogEntry(GrapeEvent, 0, 0, 0);
  else
    StGeneralLog1.AddLogEntry(UnknownEvent, 0, 0, 0);
  end;
end;

procedure TForm1.StGeneralLog1GetLogString(Sender: TObject; const D1, D2,
  D3, D4: Integer; var LogString: String);
begin
  case D1 of
    AppleEvent  : LogString := 'AppleEvent';
    OrangeEvent : LogString := 'OrangeEvent';
    LemonEvent  : LogString := 'LemonEvent';
    GrapeEvent  : LogString := 'GrapeEvent';
  else
    LogString := 'UnknownEvent';
  end
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  StGeneralLog1.FileName := ExtractFilePath(Application.ExeName) + 'exgenlog.log';
end;

end.
