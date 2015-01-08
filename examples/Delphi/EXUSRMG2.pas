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

unit EXUSRMG2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,

  StNet;

type
  TGroupPropertiesForm = class(TForm)
    Label1: TLabel;
    GroupNameEdit: TEdit;
    Label3: TLabel;
    CommentEdit: TEdit;
    GroupBox1: TGroupBox;
    ListView1: TListView;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    G : TStNetGroupItem;
    ML : TStringList;
  end;

var
  GroupPropertiesForm: TGroupPropertiesForm;

implementation

{$R *.DFM}

procedure TGroupPropertiesForm.FormShow(Sender: TObject);
var
  I : Integer;
  MI : TStNetItem;
  LI : TListItem;
begin
  Caption := Format(Caption, [G.Name]);
  GroupNameEdit.Text := G.Name;
  CommentEdit.Text := G.Comment;

  ML := G.Items;
  for I := 0 to ML.Count-1 do begin
    MI := TStNetItem(ML.Objects[I]);
    LI := ListView1.Items.Add;
    LI.Caption := MI.Name;
    LI.SubItems.Add(MI.Comment);
  end;
end;

end.
