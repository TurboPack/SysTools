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

unit exspawnu;

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ShellAPI, ExtCtrls,

  StBase, StSpawn;

type
  TForm1 = class(TForm)
    StSpawnApplication1: TStSpawnApplication;
    btnSpawn: TButton;
    RG1: TRadioGroup;
    OpenDialog1: TOpenDialog;
    cbNotify: TCheckBox;
    cbTimeout: TCheckBox;
    RG2: TRadioGroup;
    procedure btnSpawnClick(Sender: TObject);
    procedure StSpawnApplication1Completed(Sender: TObject);
    procedure StSpawnApplication1SpawnError(Sender: TObject; Error: Word);
    procedure StSpawnApplication1TimeOut(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EnableControls(B : Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.EnableControls(B : Boolean);
begin
  rg1.Enabled := B;
  rg2.Enabled := B;
  cbNotify.Enabled := B;
  cbTimeOut.Enabled := B;
  btnSpawn.Enabled := B;
end;

procedure TForm1.btnSpawnClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    StSpawnApplication1.FileName := OpenDialog1.FileName;
    StSpawnApplication1.SpawnCommand := TStSpawnCommand(rg1.ItemIndex);
    StSpawnApplication1.NotifyWhenDone := cbNotify.Checked;
    if (rg2.ItemIndex = 0) then
      StSpawnApplication1.ShowState := ssMinimized
    else
      StSpawnApplication1.ShowState := ssNormal;
    StSpawnApplication1.TimeOut := Ord(cbTimeout.Checked) * 15;
    EnableControls(StSpawnApplication1.TimeOut = 0);
    StSpawnApplication1.Execute;
  end;
end;

procedure TForm1.StSpawnApplication1Completed(Sender: TObject);
begin
  EnableControls(True);
  ShowMessage('Done');
end;

procedure TForm1.StSpawnApplication1SpawnError(Sender: TObject; Error: Word);
begin
  EnableControls(True);
  ShowMessage(IntToStr(Error));
end;

procedure TForm1.StSpawnApplication1TimeOut(Sender: TObject);
begin
  EnableControls(True);
  ShowMessage('TimeOut');
end;

end.
