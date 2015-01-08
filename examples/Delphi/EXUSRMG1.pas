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

unit EXUSRMG1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,

  StNet;

type
  TUserPropertiesForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    UserNameEdit: TEdit;
    FullNameEdit: TEdit;
    CommentEdit: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label4: TLabel;
    Password1Edit: TEdit;
    Password2Edit: TEdit;
    Label5: TLabel;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    U : TStNetUserItem;
    GL : TStringList;
  end;

var
  UserPropertiesForm: TUserPropertiesForm;

implementation

{$R *.DFM}

procedure TUserPropertiesForm.FormShow(Sender: TObject);
var
  I : Integer;
  G : TStNetGroupItem;
  LI : TListItem;
begin
  Caption := Format(Caption, [U.Name]);
  UserNameEdit.Text := U.Name;
  FullNameEdit.Text := U.FullName;
  CommentEdit.Text := U.Comment;

  GL := U.Groups;
  for I := 0 to GL.Count-1 do begin
    G := TStNetGroupItem(GL.Objects[I]);
    LI := ListView1.Items.Add;
    LI.Caption := G.Name;
    LI.SubItems.Add(G.Comment);
  end;
end;

procedure TUserPropertiesForm.OKBtnClick(Sender: TObject);
begin
  if Password1Edit.Text <> Password2Edit.Text then begin
    ShowMessage('Password and Confirm Password do not match!');
    ModalResult := mrNone;
  end;
end;

end.
