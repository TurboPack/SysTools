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

unit Observer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StPtrns, ExtCtrls;


type
  TObserverForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
// Code for the observer

    { Private declarations }
  public
    TheObserver: TStObserver;

    procedure ReceiveNotification1(WhatChanged: TObject);
    procedure ReceiveNotification2(WhatChanged: TObject);
    procedure ReceiveNotification3(WhatChanged: TObject);
    procedure ReceiveNotification4(WhatChanged: TObject);
    { Public declarations }
  end;


var
  ObserverForm: TObserverForm;

implementation

{$R *.DFM}

procedure TObserverForm.Edit1Change(Sender: TObject);
begin
  TheObserver.Notify(TObject(Edit1.Text));
end;

procedure TObserverForm.ReceiveNotification1(WhatChanged: TObject);
begin
  Button1.Caption := String(WhatChanged);
end;

procedure TObserverForm.ReceiveNotification2(WhatChanged: TObject);
begin
  Button2.Caption := String(WhatChanged);
end;

procedure TObserverForm.ReceiveNotification3(WhatChanged: TObject);
begin
  Button3.Caption := String(WhatChanged);
end;

procedure TObserverForm.ReceiveNotification4(WhatChanged: TObject);
begin
  Button4.Caption := String(WhatChanged);
end;

procedure TObserverForm.Button1Click(Sender: TObject);
begin
  Edit1.Text:= 'Reset 1';
end;

procedure TObserverForm.Button2Click(Sender: TObject);
begin
  Edit1.Text:= 'Reset 2';
end;

procedure TObserverForm.Button3Click(Sender: TObject);
begin
  Edit1.Text:= 'Reset 3';
end;

procedure TObserverForm.Button4Click(Sender: TObject);
begin
  Edit1.Text:= 'Reset 4';
end;

procedure TObserverForm.FormCreate(Sender: TObject);
begin
  TheObserver := TStObserver.Create;
  TheObserver.Add(ReceiveNotification1);
  TheObserver.Add(ReceiveNotification2);
  TheObserver.Add(ReceiveNotification3);
  TheObserver.Add(ReceiveNotification4);
end;

procedure TObserverForm.FormDestroy(Sender: TObject);
begin
  TheObserver.Free;
end;

end.
