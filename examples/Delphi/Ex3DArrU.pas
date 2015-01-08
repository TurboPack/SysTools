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

unit Ex3DArrU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,

  StConst, StBase, StLArr;

type
  TMy3D = class(TStLArray)
  protected
    XMax,
    YMax,
    ZMax    : LongInt;
  public
    constructor Create(X, Y, Z : Cardinal);
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Edit4: TEdit;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    My3D : TMy3D;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
const
  MaxX = 50;
  MaxY = 50;
  MaxZ = 50;

constructor TMy3D.Create(X, Y, Z : Cardinal);
var
  row,
  col,
  up,
  Value : LongInt;
  A     : TStLMatrix;
begin
  XMax := X;
  YMax := Y;
  ZMax := Z;

  inherited Create(ZMax, SizeOf(TStLMatrix));
  for up := 0 to ZMax-1 do
  begin
    A := TStLMatrix.Create(XMax, YMax, SizeOf(LongInt));
    for row := 0 to YMax-1 do
      for col := 0 to XMax-1 do begin
        Value := up+100*col+10000*row;
        A.Put(row, col, Value);
      end;
    Put(up, A);
  end;
end;

destructor TMy3D.Destroy;
var
  Up : LongInt;
  A  : TStLMatrix;
begin
  for Up := 0 to ZMax-1 do
  begin
    Get(Up, A);
    if Assigned(A) then
      A.Free;
  end;
  inherited Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  My3D := TMy3D.Create(MaxX, MaxY, MaxZ);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  My3D.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  XV,
  YV,
  ZV,
  Value  : LongInt;
  Z      : TStLMatrix;
begin
  XV := StrToInt(Edit1.Text);
  YV := StrToInt(Edit2.Text);
  ZV := StrToInt(Edit3.Text);
  if (XV < 0) or (XV >= MaxX) then begin
    Edit1.Text := '0';
    XV := StrToInt(Edit1.Text);
  end;
  if (YV < 0) or (YV >= MaxY) then begin
    Edit2.Text := '0';
    YV := StrToInt(Edit2.Text);
  end;
  if (ZV < 0) or (ZV >= MaxZ) then begin
    Edit3.Text := '0';
    ZV := StrToInt(Edit3.Text);
  end;

  My3D.Get(ZV, Z);
  Z.Get(XV, YV, Value);
  Edit4.Text := IntToStr(Value);
end;

end.
