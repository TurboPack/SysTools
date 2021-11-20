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

{*********************************************************}
{* SysTools: StAbout0.pas 4.04                           *}
{*********************************************************}
{* SysTools: Version property "About Box" form           *}
{*********************************************************}

{$I StDefine.inc}

unit StAbout0;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows,
{$IFnDEF FPC}
  DesignIntf,
  DesignEditors,
{$ELSE}
  PropEdits,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, StConst;

type
  TStAboutForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    lblVersion: TLabel;
    btnOK: TButton;
    Bevel1: TBevel;
    Label2: TLabel;
    WebLbl: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    NewsLbl: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TStVersionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes;  override;
    procedure Edit;override;
  end;

implementation

//{$IFnDEF FPC}
  {$R *.dfm}
//{$ELSE}
//  {$R *.lfm}
//{$ENDIF}


{*** TEsVersionProperty ***}

function TStVersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TStVersionProperty.Edit;
begin
  with TStAboutForm.Create(Application) do begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;


{*** TStAboutForm ***}

procedure TStAboutForm.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TStAboutForm.FormCreate(Sender: TObject);
begin
  Top := (Screen.Height - Height) div 3;
  Left := (Screen.Width - Width) div 2;
  lblVersion.Caption := 'Version ' + StVersionStr;
end;

end.
