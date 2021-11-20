unit graphics_delphi;

{$mode delphi}

interface

uses
  Classes, SysUtils
  , LCLType
  , Graphics
  ;

procedure GetDIBSizes(Bitmap: HBITMAP;
                        var InfoHeaderSize: DWORD; var ImageSize: DWORD);

function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
                        var BitmapInfo; var Bits): Boolean;

implementation
uses Windows ;

resourcestring
    SInvalidBitmap = 'invalid bitmap handle used';

procedure InvalidGraphic(Str: PResStringRec);
begin
  raise EInvalidGraphic.CreateRes(Str);
end;

procedure InvalidBitmap;
begin
  InvalidGraphic(@SInvalidBitmap);
end;

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, AlignmentBits: Longint): Longint;
var
  AlignmentMask: Longint;
begin
  AlignmentMask := AlignmentBits-1;
  Result := ((PixelsPerScanline * BitsPerPixel) + AlignmentMask) and not AlignmentMask;
  Result := Result div 8;
end;

function  _newDC() : HDC;
begin
{$IF 1}
  Result := Windows.GetDC(0);
{$ELSE}
  // delphi like this:
  Result := CreateCompatibleDC(0);
{$ENDIF}
end;

procedure _deleteDC( dc : HDC);
begin
  Windows.ReleaseDC(0, dc);
end;

procedure GetBitmapInfoHeader(Bitmap: HBITMAP;
                                      var BI: TBitmapInfoHeader;
                                        Colors: Integer);
var
  bmih  : PBitmapInfoHeader;
  DS    : TDIBSection;
  Bytes : Integer;
  DC    : HDC;
begin
  bmih := @DS.dsBmih;

{$IF 1 }
  FillChar(bmih^, sizeof(TBitmapInfoHeader), 0);
  bmih^.biSize := sizeof(TBitmapInfoHeader);

  DC := _newDC();
  Bytes := Windows.GetDIBits(DC, Bitmap, 0, 1, nil, PBitmapInfo(bmih)^ , DIB_RGB_COLORS);
  _deleteDC(DC);

  if Bytes = 0 then begin
    // DbgLog('Getinfo');
    // failed ???
    InvalidBitmap;
    Exit;
  end
  else if ( bmih^.biSize >= DWORD(sizeof(bmih^)) ) then
    BI := bmih^
  else begin
      FillChar(BI, sizeof(BI), 0);
      //InvalidBitmap;
      Exit;
  end

{$ELSE}
  bmih^.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);

  if Bytes = 0 then InvalidBitmap
  else if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih)))
        and ( bmih^.biSize >= DWORD(sizeof(bmih^)) )
  then
    BI := DS.dsbmih
  else begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do  begin
      biSize    := SizeOf(BI);
      biWidth   := bmWidth;
      biHeight  := bmHeight;
    end;
  end;

  case Colors of
    2: BI.biBitCount := 1;

    3..16: begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;

    17..256: begin
        BI.biBitCount := 8;
        BI.biClrUsed := Colors;
      end;

    else
        BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;

  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;

  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);

{$ENDIF}

end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP;
                              var InfoHeaderSize: DWORD; var ImageSize: DWORD;
                              Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  GetBitmapInfoHeader(Bitmap, BI, Colors);

  if BI.biBitCount > 8 then begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end

  else if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader)
                        + SizeOf(TRGBQuad) * (1 shl BI.biBitCount)

  else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader)
                        + SizeOf(TRGBQuad) * BI.biClrUsed;

  ImageSize := BI.biSizeImage;
end;

procedure GetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD; var ImageSize: DWORD);
begin
  InternalGetDIBSizes(Bitmap, InfoHeaderSize, ImageSize, 0);
end;


function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
                         var BitmapInfo; var Bits;
                         Colors: Integer): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  GetBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
  OldPal := 0;
  DC := _newDC();
  try
    if Palette <> 0 then begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
                        TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    if OldPal <> 0 then
        SelectPalette(DC, OldPal, False);
    _deleteDC(DC);
  end;
end;

function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean;
begin
  Result := InternalGetDIB(Bitmap, Palette, BitmapInfo, Bits, 0);
end;


end.

