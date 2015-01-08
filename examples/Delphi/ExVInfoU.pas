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

unit ExVInfoU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  StVInfo, StBase;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    VerInfo: TStVersionInfo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ShowVersionInfo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    VerInfo.FileName := OpenDialog1.FileName;
    ShowVersionInfo;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowVersionInfo;
end;

procedure TForm1.ShowVersionInfo;
begin
  with Memo1.Lines do begin
    Memo1.Clear;
    Add('Comments: ' + VerInfo.Comments);
    Add('Company Name: ' + VerInfo.CompanyName);
    Add('File Description: ' + VerInfo.FileDescription);
    Add('File Version: ' + VerInfo.FileVersion);
    Add('Internal Name: ' + VerInfo.InternalName);
    Add('Legal Copyright: ' + VerInfo.LegalCopyright);
    Add('Legal Trademark: ' + VerInfo.LegalTrademark);
    Add('Original Filename: ' + VerInfo.OriginalFilename);
    Add('Product Name: ' + VerInfo.ProductName);
    Add('Product Version: ' + VerInfo.ProductVersion);
    if UpperCase(ExtractFileName(VerInfo.FileName))
        = UpperCase('exvrinfo.exe') then begin
      Add('Extra Info 1: ' + VerInfo.GetKeyValue('ExtraInfo1'));
      Add('Extra Info 2: ' + VerInfo.GetKeyValue('ExtraInfo2'));
    end;
    Add('Language: ' + VerInfo.LanguageName);
    if VerInfo.FileDate <> 0 then
      Add('File Date: ' + DateToStr(VerInfo.FileDate));
  end;
end;

end.
