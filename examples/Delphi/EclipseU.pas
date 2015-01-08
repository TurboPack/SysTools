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

unit EclipseU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  StBase, StDate, StList, StEclpse;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    YearEF: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure WriteLunarData(Eclipse : TStEclipseRecord; SL : TStrings);
    procedure WriteSolarData(Eclipse : TStEclipseRecord; SL : TStrings);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  YearEF.Text := '1998';
end;

{-----------------------------------------------------------------------------}

procedure TForm1.Button1Click(Sender: TObject);
var
  I       : integer;
  Data    : TStEclipses;
  Eclipse : TStEclipseRecord;
begin
  Memo1.Clear;
  Data := TStEclipses.Create(TStListNode);
  try
    Data.FindEclipses(StrToInt(YearEF.Text));
    for I := 0 to pred(Data.Count) do begin
      Eclipse := TStEclipseRecord(Data.Eclipses[I]^);
      if (Eclipse.Etype in [etLunarPenumbral, etLunarPartial, etLunarTotal]) then
        WriteLunarData(Eclipse, Memo1.Lines)
      else
        WriteSolarData(Eclipse, Memo1.Lines);
    end;
  finally
    Data.Free;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TForm1.WriteLunarData(Eclipse : TStEclipseRecord; SL : TStrings);
var
  S : string[255];
begin
  case Eclipse.EType of
    etLunarPenumbral : SL.Add('Lunar - Penumbra');
    etLunarPartial   : SL.Add('Lunar - Partial');
    etLunarTotal     : SL.Add('Lunar - Total');
  end;
  Str(Eclipse.Magnitude : 5 : 3, S);
  SL.Add('Mag: ' + S);

  SL.Add('Penumbral Starts: ' + DateTimeToStr(Eclipse.LContacts.UT1));
  SL.Add('First  Contact:   ' + DateTimeToStr(Eclipse.LContacts.FirstContact));
  SL.Add('Second Contact:   ' + DateTimeToStr(Eclipse.LContacts.SecondContact));
  SL.Add('Mid Eclipse       ' + DateTimeToStr(Eclipse.LContacts.MidEclipse));
  SL.Add('Third  Contact:   ' + DateTimeToStr(Eclipse.LContacts.ThirdContact));
  SL.Add('Fourth Contact:   ' + DateTimeToStr(Eclipse.LContacts.FourthContact));
  SL.Add('Penumbral Ends:   ' + DateTimeToStr(Eclipse.LContacts.UT2));

  SL.Add('');
  SL.Add('');
  SL.Add('');
end;

{-----------------------------------------------------------------------------}

procedure TForm1.WriteSolarData(Eclipse : TStEclipseRecord; SL : TStrings);
var
  I   : integer;
  S,
  P   : string[255];
  LL  : TStLongLat;
begin
  case Eclipse.EType of
    etSolarPartial      : begin
                            SL.Add('Solar - Partial');
                            Str(Eclipse.Magnitude : 5 : 3, S);
                            SL.Add('Mag: ' + S);
                            if Eclipse.Hemisphere = htNorthern then
                              SL.Add('Hemisphere: Northern')
                            else
                              SL.Add('Hemisphere: Southern');
                            SL.Add('Mid Eclipse: ' +
                                   DateTimeToStr(Eclipse.LContacts.MidEclipse));
                          end;
    etSolarTotal        : begin
                            SL.Add('Solar - Total');
                            SL.Add('Mag: N/A');
                            if Eclipse.Hemisphere = htNorthern then
                              SL.Add('Hemisphere: Northern')
                            else
                              SL.Add('Hemisphere: Southern');
                            SL.Add('Mid Eclipse: ' +
                                   DateTimeToStr(Eclipse.LContacts.MidEclipse));
                          end;

    etSolarAnnularTotal : begin
                            Str(Eclipse.Magnitude : 5 : 3, S);
                            SL.Add('Mag: N/A');
                            if Eclipse.Hemisphere = htNorthern then
                              SL.Add('Hemisphere: Northern')
                            else
                              SL.Add('Hemisphere: Southern');
                            SL.Add('Mid Eclipse: ' +
                                   DateTimeToStr(Eclipse.LContacts.MidEclipse));
                          end;

    etSolarAnnular      : begin
                            SL.Add('Solar - Annular');
                            SL.Add('Mag: N/A');
                            if Eclipse.Hemisphere = htNorthern then
                              SL.Add('Hemisphere: Northern')
                            else
                              SL.Add('Hemisphere: Southern');
                            SL.Add('Mid Eclipse: ' +
                                   DateTimeToStr(Eclipse.LContacts.MidEclipse));
                          end;
  end;
  if Assigned(Eclipse.Path) then begin
    for I := 0 to pred(Eclipse.Path.Count) do begin
      LL := TStLongLat(Eclipse.Path.Items[I].Data^);
      P := '  ' + DateTimeToStr(LL.JD) + '   ';

      Str(LL.Longitude : 7 : 2, S);
      P := P + S + '   ';

      Str(LL.Latitude : 6 : 2, S);
      P := P + S + '   ';

      Str(LL.Duration : 4 : 2, S);
      P := P + S;
      SL.Add(P);
    end;
  end;
  SL.Add('');
  SL.Add('');
  SL.Add('');
end;



end.
