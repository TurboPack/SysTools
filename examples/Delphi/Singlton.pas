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

unit Singlton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, StPtrns, ExtCtrls;

type
  TSingleData = class(TStSingleton)
    public
    { Public declarations }
     TheData : integer;
end;

type
  TSingletonForm = class(TForm)
    Panel1: TPanel;
    Display1: TEdit;
    CnR1: TButton;
    Set1: TButton;
    CnR2: TButton;
    Display2: TEdit;
    Set2: TButton;
    CnR3: TButton;
    Display3: TEdit;
    Set3: TButton;
    Create1: TButton;
    Create2: TButton;
    Create3: TButton;
    Free1: TButton;
    Free2: TButton;
    Free3: TButton;
    Counter: TEdit;
    Label1: TLabel;
    Ref1: TEdit;
    Ref2: TEdit;
    Ref3: TEdit;
    procedure CnR1Click(Sender: TObject);
    procedure CnR2Click(Sender: TObject);
    procedure CnR3Click(Sender: TObject);
    procedure Set1Click(Sender: TObject);
    procedure Set2Click(Sender: TObject);
    procedure Set3Click(Sender: TObject);
    procedure Create1Click(Sender: TObject);
    procedure Create2Click(Sender: TObject);
    procedure Create3Click(Sender: TObject);
    procedure Free1Click(Sender: TObject);
    procedure Free2Click(Sender: TObject);
    procedure Free3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateDisplays;
  public
    { Public declarations }
  end;

var
  SingletonForm: TSingletonForm;

implementation
var
  SingleCounter : integer;
  MySingleton1:  TSingleData;
  MySingleton2:  TSingleData;
  MySingleton3:  TSingleData;

{$R *.DFM}



procedure TSingletonForm.CnR1Click(Sender: TObject);
begin
  if (MySingleton1 <> nil) then
    Display1.Text := IntToStr(MySingleton1.TheData);
end;

procedure TSingletonForm.CnR2Click(Sender: TObject);
begin
  if (MySingleton2 <> nil) then
    Display2.Text := IntToStr(MySingleton2.TheData);
end;

procedure TSingletonForm.CnR3Click(Sender: TObject);
begin
  if (MySingleton3 <> nil) then
    Display3.Text := IntToStr(MySingleton3.TheData);
end;

procedure TSingletonForm.Set1Click(Sender: TObject);
begin
  if (MySingleton1 = nil) then
    exit;
  MySingleton1.TheData := Strtoint(Display1.text);
  UpdateDisplays;
end;

procedure TSingletonForm.Set2Click(Sender: TObject);
begin
  if (MySingleton2 = nil) then
    exit;
  MySingleton2.TheData := Strtoint(Display2.text);
  UpdateDisplays;
end;

procedure TSingletonForm.Set3Click(Sender: TObject);
begin
  if (MySingleton3 = nil) then
    exit;
  MySingleton3.TheData := Strtoint(Display3.text);
  UpdateDisplays;
end;

procedure TSingletonForm.Create1Click(Sender: TObject);
begin
  if (MySingleton1 = nil) then begin
    MySingleton1 :=   TSingleData.create;
    SingleCounter := SingleCounter + 1;
    Counter.Text := Inttostr(SingleCounter);
    Display1.Text := IntToStr(MySingleton1.TheData);
    Ref1.Text := 'ref exists';
  end
end;

procedure TSingletonForm.Create2Click(Sender: TObject);
begin
  if (MySingleton2 = nil) then begin
    MySingleton2 :=   TSingleData.create;
    SingleCounter := SingleCounter + 1;
    Counter.Text := Inttostr(SingleCounter);
    Display2.Text := IntToStr(MySingleton2.TheData);
    Ref2.Text := 'ref exists';
  end
end;

procedure TSingletonForm.Create3Click(Sender: TObject);
begin
  if (MySingleton3 = nil) then begin
    MySingleton3 :=   TSingleData.create;
    SingleCounter := SingleCounter + 1;
    Counter.Text := Inttostr(SingleCounter);
    Display3.Text := IntToStr(MySingleton3.TheData);
    Ref3.Text := 'ref exists';
  end
end;

procedure TSingletonForm.Free1Click(Sender: TObject);
begin
  if (MySingleton1 = nil) then
    exit;
  MySingleton1.free;
  MySingleton1 := nil;
  if (SingleCounter > 0) then
    SingleCounter := SingleCounter - 1;
  Counter.Text := Inttostr(SingleCounter);
  Display1.Text := '(empty)';
  Ref1.Text := 'ref is nil';
end;

procedure TSingletonForm.Free2Click(Sender: TObject);
begin
  if (MySingleton2 = nil) then
    exit;
  MySingleton2.free;
  MySingleton2 := nil;
  if (SingleCounter > 0) then
    SingleCounter := SingleCounter - 1;
  Counter.Text := Inttostr(SingleCounter);
  Display2.Text := '(empty)';
  Ref2.Text := 'ref is nil';
end;

procedure TSingletonForm.Free3Click(Sender: TObject);
begin
  if (MySingleton3 = nil) then
    exit;
  MySingleton3.free;
  MySingleton3 := nil;
  if (SingleCounter > 0) then
    SingleCounter := SingleCounter - 1;
  Counter.Text := Inttostr(SingleCounter);
  Display3.Text := '(empty)';
  Ref3.Text := 'ref is nil';
end;

procedure TSingletonForm.FormCreate(Sender: TObject);
begin
  SingleCounter := 0;
end;

procedure TSingletonForm.UpdateDisplays;
begin
  if (MySingleton1 <> nil) then
    Display1.Text := IntToStr(MySingleton1.TheData);
  if (MySingleton2 <> nil) then
    Display2.Text := IntToStr(MySingleton2.TheData);
  if (MySingleton3 <> nil) then
    Display3.Text := IntToStr(MySingleton3.TheData);
end;

end.
