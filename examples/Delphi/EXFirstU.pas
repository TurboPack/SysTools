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

(*****************************************************************************

This unit shows a basic implementation of the STFIRST Unit methods working
with the StWmDataCopy component. Compile the project and then start it, giving
a text file as part of the command line parameter. For example:

  EXFIRST c:\myfiles\afile.txt

You can also start the program without a parameter and use the Load button
to load a text file into the TMemo component

Keep that program running and try to start a second instance, this time
specifying a different file. For example:

  EXFIRST c:\myfiles\File2.txt

Note that a second instance of the program does not run but that File2
is loaded into the TMemo component.

*******
WARNING
*******
If running this under Windows 9x, TMemo cannot accept a file larger
than about 32K. If you try to load a file via the command line that is larger,
Delphi raises an execption and the application is terminated. You could avoid
this by modifying the DoFileOpen method to check the size of the file before
it's loaded into the TMemo

******************************************************************************)


unit EXFirstU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  StBase, StwmDCpy;

type
  TfrmEXWDC = class(TForm)
    StWMDataCopy1: TStWMDataCopy;
    Memo1: TMemo;
    btnLoadFile: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure StWMDataCopy1DataReceived(Sender: TObject;
      CopyData: TCopyDataStruct);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure DoFileOpen(FN : ShortString);
  end;

var
  frmEXWDC: TfrmEXWDC;

implementation

{$R *.DFM}

procedure TfrmEXWDC.FormCreate(Sender: TObject);
begin
  if (ParamCount > 0) then
    DoFileOpen(ParamStr(1));
end;

procedure TfrmEXWDC.DoFileOpen(FN : ShortString);
begin
  Memo1.Clear;
  if (FileExists(FN)) then
    Memo1.Lines.LoadFromFile(FN);
end;

procedure TfrmEXWDC.btnLoadFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmEXWDC.StWMDataCopy1DataReceived(Sender: TObject;
  CopyData: TCopyDataStruct);
var
  S : string;
  I : Longint;
begin
  S := String(PChar(CopyData.lpData));

  ShowMessage(S);
  I := pos(' ', S);
  if (I = 0) then
    I := pos(#9, S);
  if (I > 0) then begin
    S := Copy(S, I+1, Length(S));
    DoFileOpen(S);
  end;
end;

end.


