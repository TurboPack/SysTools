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

unit EXUSRMG0;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ExtCtrls,

  StNet;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    User1: TMenuItem;
    About1: TMenuItem;
    NewUser1: TMenuItem;
    NewGlobalGroup1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    Properties1: TMenuItem;
    N2: TMenuItem;
    SelectServer1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    NewLocalGroup1: TMenuItem;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    ListView1: TListView;
    ListView2: TListView;
    procedure SelectServer1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NewUser1Click(Sender: TObject);
    procedure NewGlobalGroup1Click(Sender: TObject);
    procedure NewLocalGroup1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CS : TStNetServerItem;
    UL : TStringList;
    GL : TStringList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses EXUSRMG1, EXUSRMG2, EXUSRMG3, EXUSRMG4;

procedure TMainForm.SelectServer1Click(Sender: TObject);
var
  SelectedServer : string;
  I : Integer;
  U : TStNetUserItem;
  G : TStNetGroupItem;
  LI : TListItem;
begin
  if InputQuery('Server name', 'Select the server name to use', SelectedServer) then begin
    CS := StNetwork.Server[SelectedServer];

    if CS <> nil then begin

      { load the users into the listview }
      ListView1.Items.Clear;
      UL := CS.Users;
      for I := 0 to UL.Count-1 do begin
        U := TStNetUserItem(UL.Objects[I]);
        if (U.ItemType = nitLocalUser) or (U.ItemType = nitGlobalUser) then begin
          LI := ListView1.Items.Add;
          LI.Caption := U.Name;
          LI.SubItems.Add(U.FullName);
          LI.SubItems.Add(U.Comment);
        end;
      end;

      { load the groups into the listview }
      ListView2.Items.Clear;
      GL := CS.Groups;
      for I := 0 to GL.Count-1 do begin
        G := TStNetGroupItem(GL.Objects[I]);
        if (G.ItemType = nitLocalGroup) or (G.ItemType = nitGlobalGroup) then begin
          LI := ListView2.Items.Add;
          LI.Caption := G.Name;
          LI.SubItems.Add(G.Comment);
        end;
      end;

      { check for GloableGroup creation on domain controllers }
      if (nsvtDomainCtrl in CS.ServerType) or (nsvtDomainBackupCtrl in CS.ServerType) then
        NewGlobalGroup1.Enabled := True
      else
        NewGlobalGroup1.Enabled := False;
    end;
  end;
end;

procedure TMainForm.About1Click(Sender: TObject);
var
  AdditionalText, Caption : string;
begin
  AdditionalText := 'An network example program from the TurboPower SysTools Library';
  Caption := MainForm.Caption;

  Application.MessageBox(PChar(AdditionalText), PChar(Caption),
    MB_OK or MB_ICONINFORMATION);
end;

procedure TMainForm.Properties1Click(Sender: TObject);
var
  LI : TListItem;
  UPF : TUserPropertiesForm;
  GPF : TGroupPropertiesForm;
begin
  { Are we dealing with a user or group? }
  if (ListView1.Focused) and (ListView1.SelCount > 0) then begin
    LI := ListView1.Selected;

    UPF := TUserPropertiesForm.Create(Self);
    UPF.U := TStNetUserItem(UL.Objects[UL.IndexOf(LI.Caption)]);
    case UPF.ShowModal of
      mrOK :
        begin
          { update the data }
          UPF.U.Name := UPF.UserNameEdit.Text;
          UPF.U.FullName := UPF.FullNameEdit.Text;
          UPF.U.Comment := UPF.CommentEdit.Text;

          if (UPF.Password1Edit.Text <> 'NO PASSWORD CHANGE') then
            UPF.U.Password := UPF.Password1Edit.Text;

          LI.Caption := UPF.U.Name;
          LI.SubItems.Strings[0] := UPF.U.FullName;
          LI.SubItems.Strings[1] := UPF.U.Comment;
        end;

      mrCancel : ;
    end;
    UPF.Free;
  end else if (ListView2.Focused) and (ListView2.SelCount > 0) then begin
    LI := ListView2.Selected;
    GPF := TGroupPropertiesForm.Create(Self);
    GPF.G := TStNetGroupItem(GL.Objects[GL.IndexOf(LI.Caption)]);
    case GPF.ShowModal of
      mrOK :
        begin
          { update the data }
          GPF.G.Name := GPF.GroupNameEdit.Text;
          GPF.G.Comment := GPF.CommentEdit.Text;

          LI.Caption := GPF.G.Name;
          LI.SubItems.Strings[0] := GPF.G.Comment;
        end;

      mrCancel : ;
    end;
    GPF.Free;
  end;
end;

procedure TMainForm.Delete1Click(Sender: TObject);
var
  LI : TListItem;
  U : TStNetUserItem;
  G : TStNetGroupItem;
begin
  { Are we dealing with a user or group? }
  if (ListView1.Focused) and (ListView1.SelCount > 0) then begin
    LI := ListView1.Selected;
    U := TStNetUserItem(UL.Objects[UL.IndexOf(LI.Caption)]);

    if MessageDlg('Are you sure you want to delete the user - [' + U.Name + ']?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      U.Delete;
      LI.Delete;
    end;
  end else if (ListView2.Focused) and (ListView2.SelCount > 0) then begin
    LI := ListView2.Selected;
    G := TStNetGroupItem(GL.Objects[GL.IndexOf(LI.Caption)]);
    if MessageDlg('Are you sure you want to delete the group - [' + G.Name + ']?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      G.Delete;
      LI.Delete;
    end;
  end;
end;

procedure TMainForm.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    Properties1Click(Sender);
  end;
end;

procedure TMainForm.NewUser1Click(Sender: TObject);
var
  NU : TNewUserForm;
  U : TStNetUserItem;
  LI : TListItem;
begin
  if CS = nil then
    Exit;
  { Create a new user }
  NU := TNewUserForm.Create(Self);
  NU.Caption := Format(NU.Caption, [CS.Name]);
  case NU.ShowModal of
    mrOK :
      begin
        U := CS.AddUser(NU.UserNameEdit.Text, NU.Password1Edit.Text, True);
        U.FullName := NU.FullNameEdit.Text;
        U.Comment  := NU.CommentEdit.Text;

        LI := ListView1.Items.Add;
        LI.Caption := U.Name;
        LI.SubItems.Add(U.FullName);
        LI.SubItems.Add(U.Comment);

        U.Free;                                                        {!!.01}
        UL := CS.Users;
      end;
    mrCancel : ;
  end;
  NU.Free;
end;

procedure TMainForm.NewGlobalGroup1Click(Sender: TObject);
var
  NG : TNewGroupForm;
  G  : TStNetGroupItem;
  LI : TLIstItem;
begin
  if CS = nil then
    Exit;
  { Create a new global group }
  NG := TNewGroupForm.Create(Self);
  NG.Caption := Format(NG.Caption, [CS.Name, 'global']);
  case NG.ShowModal of
    mrOK :
      begin
        G := CS.AddGroup(NG.GroupNameEdit.Text, NG.CommentEdit.Text, True);

        LI := ListView2.Items.Add;
        LI.Caption := G.Name;
        LI.SubItems.Add(G.Comment);

        G.Free;                                                        {!!.01}
        GL := CS.Groups;
      end;
    mrCancel : ;
  end;
  NG.Free;
end;

procedure TMainForm.NewLocalGroup1Click(Sender: TObject);
var
  NG : TNewGroupForm;
  G  : TStNetGroupItem;
  LI : TLIstItem;
begin
  if CS = nil then
    Exit;
  { create a new local group }
  NG := TNewGroupForm.Create(Self);
  NG.Caption := Format(NG.Caption, [CS.Name, 'local']);
  case NG.ShowModal of
    mrOK :
      begin
        G := CS.AddGroup(NG.GroupNameEdit.Text, NG.CommentEdit.Text, False);

        LI := ListView2.Items.Add;
        LI.Caption := G.Name;
        LI.SubItems.Add(G.Comment);

        G.Free;                                                        {!!.01}
        GL := CS.Groups;
      end;
    mrCancel : ;
  end;
  NG.Free;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
