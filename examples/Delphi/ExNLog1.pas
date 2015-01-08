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

unit ExNLog1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StBase, StNTLog;

type
  TForm1 = class(TForm)
    Button1: TButton;
    EL: TStNTEventLog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure MyOnRead(Sender : TObject; const EventRec : TStNTEventLogRec; var Abort : Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ReadCount : DWORD = 0;

implementation

{$R *.DFM}

procedure TForm1.MyOnRead(Sender : TObject; const EventRec : TStNTEventLogRec; var Abort : Boolean);
begin
  Inc(ReadCount);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I : Integer;
begin
  EL.OnReadRecord := MyOnRead;
  Label1.Caption := IntToStr(EL.LogCount);
  for I := 0 to EL.LogCount-1 do
    Listbox1.Items.Add(EL.Logs[I]);
  Label2.Caption := IntToStr(EL.RecordCount);
  EL.ReadLog(True);
  Label3.Caption := IntToStr(ReadCount);
end;

end.
