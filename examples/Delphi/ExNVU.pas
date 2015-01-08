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

unit ExNVU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  StBase, StNVCont, StNVBits, StNVDict, StNVList, StNVDQ, StNVLAry,
  StNVLMat, StNVColl, StNVSCol, StNVTree;

type
  TNVForm = class(TForm)
    StNVBits1: TStNVBits;
    StNVDictionary1: TStNVDictionary;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NVForm: TNVForm;

implementation

{$R *.DFM}

procedure TNVForm.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.Add('Initializing bit set to hold 500 bits');
  StNVBits1.MaxBits := 500;
  Memo1.Lines.Add('Set bit 5');
  StNVBits1.Container.SetBit(5);
  if StNVBits1.Container.BitIsSet(5) then
    Memo1.Lines.Add('bit 5 is set')
  else
    Memo1.Lines.Add('bit 5 is not set');
  Memo1.Lines.Add('Toggle bit 5');
  StNVBits1.Container.ToggleBit(5);
  if StNVBits1.Container.BitIsSet(5) then
    Memo1.Lines.Add('bit 5 is set')
  else
    Memo1.Lines.Add('bit 5 is not set');
end;

procedure TNVForm.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TNVForm.Button2Click(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.Add('Clearing dictionary');
  StNVDictionary1.Container.Clear;
  Memo1.Lines.Add('Adding items to dictionary');
  StNVDictionary1.Container.Add('First', nil);
  StNVDictionary1.Container.Add('Second', nil);
  StNVDictionary1.Container.Add('Third', nil);
  StNVDictionary1.Container.Add('Fourth', nil);
  StNVDictionary1.Container.Add('Fifth', nil);
end;

end.
