// Upgraded to Delphi 2009: Sebastian Zierer

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
{* SysTools: StBarPN.pas 4.04                            *}
{*********************************************************}
{* SysTools: PostNet Bar Code component                  *}
{*********************************************************}

{$I StDefine.inc}

unit StBarPN;

interface

uses
  Windows, Classes, ClipBrd, Controls, Graphics, Messages, SysUtils,
  StBase, StConst, StStrL;

type
  TStPNBarCodeDims = packed record
    PixPerBar        : Longint;
    PixPerSpace      : Longint;
    ShortBarHeight   : Longint;
    TallBarHeight    : Longint;
    Width            : Longint;
    Height           : Longint;
  end;

  TStPNBarCodeRes = packed record
    XRes  : Longint;
    YRes  : Longint;
  end;

  TStPNBarCode = class(TGraphicControl)
  protected {private}
    {property variables}
    FPostalCode        : string;
    FCheckNumber       : Integer;


    {internal variables}
    pnbcDisplayDims    : TStPNBarCodeDims;
    pnbcDefRes         : TStPNBarCodeRes;

    {property methods}
    function  GetVersion : string;
    procedure SetPostalCode(Value : String);
    procedure SetVersion (const v : string);

    {internal methods}
    function DrawTallBar(C         : TCanvas;
                         Dims      : TStPNBarCodeDims;
                         XPos      : Integer;
                         AddSpace  : Boolean) : Longint;
    function DrawShortBar(C        : TCanvas;
                          Dims     : TStPNBarCodeDims;
                          XPos     : Integer;
                          AddSpace : Boolean) : Longint;
    function DrawNumber(C          : TCanvas;
                        Dims       : TStPNBarCodeDims;
                        Value      : Integer;
                        XPos       : Longint;
                        FrontGuard : Boolean;
                        EndGuard   : Boolean) : Longint;
    procedure DrawBarCode(C : TCanvas; Dims : TStPNBarCodeDims);
    procedure SetCheckNumber;

(*
    procedure CMTextChanged(var Msg : TMessage);
      message CM_TEXTCHANGED;
*)

  protected
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;

    procedure ComputeSizes(C        : TCanvas;
                           Res      : TStPNBarCodeRes;
                           var Dims : TStPNBarCodeDims);
    procedure CopyToClipboard;
    procedure PaintToCanvas(ACanvas : TCanvas; Position : TPoint);
    procedure PaintToDC(DC : hDC; Position : TPoint);
    procedure PaintToPrinterCanvas(ACanvas : TCanvas; Position : TPoint);
    procedure PaintToPrinterDC(DC : hDC; Position : TPoint);
    procedure SaveToFile(ACanvas : TCanvas; const FileName : string);
    procedure SaveToFileRes(Res : TStPNBarCodeRes; const FileName : string);

  published
    {properties}
    property Cursor;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property PostalCode : string read FPostalCode write SetPostalCode;

    property Version : string read GetVersion write SetVersion stored False;

    {events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation

{*** TStPNBarCode ***}

function TStPNBarCode.GetVersion : string;
begin
  Result := StVersionStr;
end;


procedure TStPNBarCode.SetVersion(const v : string);
begin
end;

constructor TStPNBarCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {defaults}
  pnbcDefRes.XRes := 0;
  pnbcDefRes.YRes := 0;
{set arbitrary values for height/width so that component automatically resizes}
  Height := 10;
  Width  := 10;
  PostalCode := '12345';
  SetCheckNumber;
end;


procedure TStPNBarCode.Loaded;
begin
  inherited Loaded;
  Invalidate;
end;


procedure TStPNBarCode.Paint;
begin
  ComputeSizes(Canvas, pnbcDefRes, pnbcDisplayDims);
  Height := pnbcDisplayDims.Height;
  Width  := pnbcDisplayDims.Width;
  DrawBarCode(Canvas, pnbcDisplayDims);
end;


procedure TStPNBarCode.SetCheckNumber;
var
  I : Longint;
begin
  if (Length(TrimL(FPostalCode)) < 5) then Exit;
  FCheckNumber := 0;
  for I := 1 to Length(FPostalCode) do
    FCheckNumber := FCheckNumber + StrToInt(FPostalCode[I]);
  I := FCheckNumber mod 10;
  if (I > 0) then
    FCheckNumber := 10 - I
  else
    FCheckNumber := 0;
end;

procedure TStPNBarCode.SetPostalCode(Value : string);
var
  I : Integer;
  Local : string;                                                        
begin
  if (csLoading in ComponentState) then Exit;

  Local := TrimL(Value);                                                 

  {strip non-numerics}
  I := 1;
  repeat
    if not CharInSet(Local[I], ['0'..'9']) then
      System.Delete(Local, I, 1)
    else
      Inc(I);
  until (I > Length(Local));

  { looks like a valid Postal Code?}
  if (Local <> FPostalCode) then begin                                   
     if (Length(Local) in [5, 9, 11]) then begin                         
      FPostalCode := Local;
      SetCheckNumber;
      Invalidate;
    end else
      RaiseStError(EStPNBarCodeError, stscInvalidLength);
  end; { else it's the same code, don't bother updating }
end;


function TStPNBarCode.DrawTallBar(C        : TCanvas;
                                  Dims     : TStPNBarCodeDims;
                                  XPos     : Integer;
                                  AddSpace : Boolean) : Longint;
var
  YPos : Longint;
begin
  Result := XPos;
  YPos := Dims.Height - 5 - Dims.TallBarHeight;
  C.Rectangle(XPos, YPos, XPos+Dims.PixPerBar, YPos+Dims.TallBarHeight);
  Result := Result + Dims.PixPerBar;

  if (AddSpace) then
    Inc(Result, Dims.PixPerSpace);
end;


function TStPNBarCode.DrawShortBar(C        : TCanvas;
                                   Dims     : TStPNBarCodeDims;
                                   XPos     : Integer;
                                   AddSpace : Boolean) : Longint;
var
  YPos : Longint;
begin
  Result := XPos;
  YPos := Dims.Height - 5 - Dims.ShortBarHeight;
  C.Rectangle(XPos, YPos, XPos+Dims.PixPerBar, YPos+Dims.ShortBarHeight);
  Result := Result + Dims.PixPerBar;

  if (AddSpace) then
    Inc(Result, Dims.PixPerSpace);
end;


function TStPNBarCode.DrawNumber(C          : TCanvas;
                                 Dims       : TStPNBarCodeDims;
                                 Value      : Integer;
                                 XPos       : Longint;
                                 FrontGuard : Boolean;
                                 EndGuard   : Boolean) : Longint;
begin
  Result := XPos;
  if (FrontGuard) then
    Result := DrawTallBar(C, Dims, Result, True);

  case Value of
    0 : begin
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
        end;

    1 : begin
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
        end;

    2 : begin
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
        end;

    3 : begin
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
        end;

    4 : begin
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
        end;

    5 : begin
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
        end;

    6 : begin
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
        end;

    7 : begin
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
        end;

    8 : begin
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
        end;

    9 : begin
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawTallBar(C,  Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
          Result := DrawShortBar(C, Dims, Result, True);
        end;
  end;

  if (EndGuard) then
    Result := DrawTallBar(C, Dims, Result, False);
end;


procedure TStPNBarCode.ComputeSizes(C        : TCanvas;
                                    Res      : TStPNBarCodeRes;
                                    var Dims : TStPNBarCodeDims);
var
  PPIX,
  PPIY   : Longint;
begin
  if csLoading in ComponentState then
    Exit;
  {get resolution}
  if ((Res.XRes > 0) and (Res.YRes > 0)) then begin
    PPIX := Res.XRes;
    PPIY := Res.YRes;
  end else begin
    PPIX := GetDeviceCaps(C.Handle, LOGPIXELSX);
    PPIY := GetDeviceCaps(C.Handle, LOGPIXELSY);
  end;

  {PN bar is 0.015" to 0.025" - use mid value}
  {add 1 since Canvas.Rectangle draws 1 pixel less than Width}
  Dims.PixPerBar := Round(PPIX * 0.017) + 1;

  {CenterLine distance is 0.0416" to 0.0500". Space is that minus width of bar}
  {In all cases the Pitch must be 22 +/-2 bars/Inch where a bar is the bar and}
  {the trailing space}

  {add 1 since Canvas.Rectangle draws 1 pixel less than Width}
  Dims.PixPerSpace := Round(0.0475 * PPIX) - Dims.PixPerBar + 1;

  {max height of short bar is 0.050" +/-0.010". To allow for 75dpi, go a}
  {little less}
  {add 1 since Canvas.Rectangle draws 1 pixel less than Height}
  Dims.ShortBarHeight := Round(0.047 * PPIY) + 1;

  {max height of tall bar is 0.125" +/-0.010". To allow for 75dpi, go a}
  {little less}
  {add 1 since Canvas.Rectangle draws 1 pixel less than Height}
  Dims.TallBarHeight := Round(0.122 * PPIY) + 1;


  {Total Width of Canvas =
     FrontGuardBar + Space +
     (NumberChars + CheckChar) * (5 * (PixelsPerBar + PixelsPerSpace)) +
     (EndBar w/o Space) +
     5 pixels left/right margin
  }
  Dims.Width :=
    (Dims.PixPerBar + Dims.PixPerSpace) +
    (Length(PostalCode) + 1) * (5 * (Dims.PixPerBar + Dims.PixPerSpace)) +
    Dims.PixPerBar + 10;

  {Height = Height of tall bar + 3 pixel top/bottom margin}
  Dims.Height := Dims.TallBarHeight + 3;
end;


procedure TStPNBarCode.DrawBarCode(C : TCanvas; Dims : TStPNBarCodeDims);
var
  I,
  XPos : Longint;
begin
  if csLoading in ComponentState then
    Exit;

  C.Brush.Color := clBlack;
  C.Brush.Style := bsSolid;

  {Draw the Code}
  XPos := 5;
  XPos := DrawNumber(C, Dims, StrToInt(PostalCode[1]), XPos, True, False);
  for I := 2 to Length(PostalCode) do
    XPos := DrawNumber(C, Dims, StrToInt(PostalCode[I]), XPos, False, False);
  DrawNumber(C, Dims, FCheckNumber, XPos, False, True);
end;


(*
procedure TStPNBarCode.CMTextChanged(var Msg : TMessage);
begin
  SetCheckNumber;
  Invalidate;
end;
*)

procedure TStPNBarCode.CopyToClipboard;
var
  MetaFile       : TMetaFile;
  MetaFileCanvas : TMetaFileCanvas;
  Bitmap         : TBitmap;
  Dims           : TStPNBarCodeDims;
begin
  Clipboard.Clear;
  Clipboard.Open;
  try
    {bitmap}
    Bitmap := TBitmap.Create;
    try
      ComputeSizes(Bitmap.Canvas, pnbcDefRes, Dims);
      Bitmap.Width  := Dims.Width;
      Bitmap.Height := Dims.Height;
      DrawBarCode(Bitmap.Canvas, Dims);
      Clipboard.Assign(Bitmap);

      {metafile}
      MetaFile := TMetaFile.Create;
      try
        MetaFileCanvas := TMetaFileCanvas.Create(MetaFile, 0);
        try
          MetaFile.Enhanced := True;
          MetaFile.Width := ClientWidth;
          MetaFile.Height := ClientHeight;
          MetaFileCanvas.Draw(0, 0, Bitmap);
        finally
          MetaFileCanvas.Free;
        end;
        Clipboard.Assign(MetaFile);
      finally
        MetaFile.Free;
      end;

    finally
      Bitmap.Free;
    end
  finally
    Clipboard.Close;
  end;
end;



procedure TStPNBarCode.PaintToDC(DC : hDC; Position : TPoint);
var
  Bmp     : TBitmap;
  ACanvas : TCanvas;
  Dims    : TStPNBarCodeDims;
  R1,
  R2      : TRect;
begin
  ACanvas := TCanvas.Create;
  ACanvas.Handle := DC;
  Bmp := TBitmap.Create;
  try
    ComputeSizes(ACanvas, pnbcDefRes, Dims);
    Bmp.Height := Dims.Height;
    Bmp.Width  := Dims.Width;
    R1 := Rect(0, 0, Dims.Width, Dims.Height);
    R2 := Rect(Position.X, Position.Y,
               Dims.Width + Position.X,
               Dims.Height + Position.Y);

    DrawBarCode(Bmp.Canvas, Dims);
    ACanvas.CopyRect(R2, Bmp.Canvas, R1);
  finally
    Bmp.Free;
    ACanvas.Free;
  end;
end;



procedure TStPNBarCode.PaintToCanvas(ACanvas : TCanvas; Position : TPoint);
begin
  PaintToDC(ACanvas.Handle, Position);
end;



procedure TStPNBarCode.PaintToPrinterCanvas(ACanvas : TCanvas;
                                            Position : TPoint);
begin
  PaintToPrinterDC(ACanvas.Handle, Position);
end;



procedure TStPNBarCode.PaintToPrinterDC(DC : hDC; Position : TPoint);
var
  Bmp       : TBitmap;
  ACanvas   : TCanvas;
  Dims      : TStPNBarCodeDims;
  R1,
  R2        : TRect;

  Info      : PBitMapInfo;
  InfoSize  : DWORD;
  ImageSize : DWORD;
  Image     : Pointer;
begin
  ACanvas := TCanvas.Create;
  Bmp     := TBitmap.Create;
  ACanvas.Handle := DC;
  try
    ComputeSizes(ACanvas, pnbcDefRes, Dims);
    Bmp.Height := Dims.Height;
    Bmp.Width  := Dims.Width;
    R1 := Rect(0, 0, Dims.Width, Dims.Height);
    R2 := Rect(Position.X, Position.Y,
               Dims.Width + Position.X,
               Dims.Height + Position.Y);

    DrawBarCode(Bmp.Canvas, Dims);

    {Delphi does not allow a simple Canvas.CopyRect to the printer Canvas}
    with Bmp do begin
        GetDIBSizes(Handle, InfoSize, ImageSize);
        GetMem(Info, InfoSize);
        try
          GetMem(Image, ImageSize);
          try
            GetDIB(Handle, Palette, Info^, Image^);
            with Info^.bmiHeader do begin
              StretchDIBits(ACanvas.Handle,
                            R2.Left, R2.Top, Dims.Width, Dims.Height,
                            0, 0, biWidth, biHeight,
                            Image, Info^, DIB_RGB_COLORS, SRCCOPY);
            end;
          finally
            FreeMem(Image, ImageSize)
          end;
        finally
          FreeMem(Info, InfoSize);
        end;
      end;
  finally
    Bmp.Free;
    ACanvas.Free;
  end;
end;



procedure TStPNBarCode.SaveToFile(ACanvas : TCanvas;
                                  const FileName : string);
var
  Bmp          : TBitmap;
  Dims         : TStPNBarCodeDims;
begin
  Bmp := TBitmap.Create;
  try
    ComputeSizes(ACanvas, pnbcDefRes, Dims);
    Bmp.Height := Dims.Height;
    Bmp.Width  := Dims.Width;
    DrawBarCode(Bmp.Canvas, Dims);
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;



procedure TStPNBarCode.SaveToFileRes(Res : TStPNBarCodeRes;
                                     const FileName : string);
var
  Bmp          : TBitmap;
  Dims         : TStPNBarCodeDims;
begin
  Bmp := TBitmap.Create;
  try
    ComputeSizes(Bmp.Canvas, Res, Dims);
    Bmp.Height := Dims.Height;
    Bmp.Width  := Dims.Width;
    DrawBarCode(Bmp.Canvas, Dims);
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;

end.
