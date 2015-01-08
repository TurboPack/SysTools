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

unit ExJupStU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    edDate: TEdit;
    edTime: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  StDate, StDateSt, StAstro, StJupSat;

procedure TForm1.Button1Click(Sender: TObject);
var
  D   : TDateTime;
  XS,
  YS  : string[20];
  JS  : TStJupSats;
begin
  Memo1.Clear;

  D := StrToDate(edDate.Text);
  if edTime.Text > '' then
     D := D + StrToTime(edTime.Text);

  JS := GetJupSats(D, False, False);

  Memo1.Lines.Add('Low Precision');
  Memo1.Lines.Add('--------------------------------');
  Str(JS.Io.X:6:2, XS);
  Str(JS.Io.Y:6:2, YS);
  Memo1.Lines.Add('Io         ' + XS + '    '  + YS);

  Str(JS.Europa.X:6:2, XS);
  Str(JS.Europa.Y:6:2, YS);
  Memo1.Lines.Add('Europa     ' + XS + '    '  + YS);

  Str(JS.Ganymede.X:6:2, XS);
  Str(JS.Ganymede.Y:6:2, YS);
  Memo1.Lines.Add('Ganymede   ' + XS + '    '  + YS);

  Str(JS.Callisto.X:6:2, XS);
  Str(JS.Callisto.Y:6:2, YS);
  Memo1.Lines.Add('Callisto   ' + XS + '    '  + YS);


  Memo1.Lines.Add(' ');

  JS := GetJupSats(D, True, False);
  Memo1.Lines.Add('High Precision - Non Shadow');
  Memo1.Lines.Add('--------------------------------');

  Str(JS.Io.X:8:4, XS);
  Str(JS.Io.Y:8:4, YS);
  Memo1.Lines.Add('Io         ' + XS + '    '  + YS);

  Str(JS.Europa.X:8:4, XS);
  Str(JS.Europa.Y:8:4, YS);
  Memo1.Lines.Add('Europa     ' + XS + '    '  + YS);

  Str(JS.Ganymede.X:8:4, XS);
  Str(JS.Ganymede.Y:8:4, YS);
  Memo1.Lines.Add('Ganymede   ' + XS + '    '  + YS);

  Str(JS.Callisto.X:8:4, XS);
  Str(JS.Callisto.Y:8:4, YS);
  Memo1.Lines.Add('Callisto   ' + XS + '    '  + YS);


  Memo1.Lines.Add(' ');

  JS := GetJupSats(D, True, True);
  Memo1.Lines.Add('High Precision - Shadow');
  Memo1.Lines.Add('--------------------------------');

  Str(JS.Io.X:8:4, XS);
  Str(JS.Io.Y:8:4, YS);
  Memo1.Lines.Add('Io         ' + XS + '    '  + YS);

  Str(JS.Europa.X:8:4, XS);
  Str(JS.Europa.Y:8:4, YS);
  Memo1.Lines.Add('Europa     ' + XS + '    '  + YS);

  Str(JS.Ganymede.X:8:4, XS);
  Str(JS.Ganymede.Y:8:4, YS);
  Memo1.Lines.Add('Ganymede   ' + XS + '    '  + YS);

  Str(JS.Callisto.X:8:4, XS);
  Str(JS.Callisto.Y:8:4, YS);
  Memo1.Lines.Add('Callisto   ' + XS + '    '  + YS);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edDate.Text := DateToStr(Date);
  edTime.Text := '';
end;

end.
