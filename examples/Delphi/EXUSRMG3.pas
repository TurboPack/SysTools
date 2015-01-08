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

unit EXUSRMG3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TNewUserForm = class(TForm)
    Label1: TLabel;
    UserNameEdit: TEdit;
    Label2: TLabel;
    FullNameEdit: TEdit;
    Label3: TLabel;
    CommentEdit: TEdit;
    Panel1: TPanel;
    Label4: TLabel;
    Password1Edit: TEdit;
    Label5: TLabel;
    Password2Edit: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewUserForm: TNewUserForm;

implementation

{$R *.DFM}

procedure TNewUserForm.OKBtnClick(Sender: TObject);
begin
  if Trim(UserNameEdit.Text) = '' then
    ModalResult := mrNone;
  if Password1Edit.Text <> Password2Edit.Text then begin
    ShowMessage('Password and Confirm Password do not match!');
    ModalResult := mrNone;
  end;
end;

end.
