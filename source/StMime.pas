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
{* SysTools: StMime.pas 4.04                             *}
{*********************************************************}
{* SysTools: Internet Conversion unit for SysTools       *}
{*********************************************************}

{$I StDefine.inc}

{
Note: Some Mime routines rely on overflows for their results,
so these need to be off:
}
{$R-}
{$Q-}

unit StMime;

interface

uses
  Windows,
  SysUtils,
  Classes,
  StConst,
  StBase,
  StStrZ,
  StStrL,
  StOStr;

const
  AttachmentFileMode = (fmOpenRead or fmShareDenyWrite);
  CRLFStr : array[0..1] of AnsiChar = #13#10;
  DefStContentDisposition = 'attachment';
  DefStContentType = 'application/octet-stream';
  DefStMimeEncoding = 'base64';
  ExtractFileMode = (fmOpenReadWrite or fmShareExclusive);
  MaxMimeLine = 78;

type

  TStConvertState = (csStarted, csProgressing, csFinished);

  TStProgressEvent =
    procedure(Sender : TObject; Status : TStConvertState; PercentDone : Byte) of object;

  TStSaveAsEvent = procedure(Sender : TObject; var FileName : string) of object;

{.Z+}
  TStMimeConverter = class;

  { Base conversion stream }
  TStConvertStream = class
  protected {private}
    FCurrentFile : string;
    FOwner : TStMimeConverter;
    FOnProgress : TStProgressEvent;
  public
    constructor Create(Owner : TStMimeConverter); virtual;
    procedure DecodeToStream(InStream, OutStream : TStream); virtual; abstract;
    procedure EncodeToStream(InStream, OutStream : TStream); virtual; abstract;
    procedure Progress(Status : TStConvertState; PercentDone : Byte); virtual;
    property CurrentFile : string
      read FCurrentFile write FCurrentFile;
    property OnProgress : TStProgressEvent
      read FOnProgress write FOnProgress;
  end;

  { Conversion stream for raw copying }
  TStRawStream = class(TStConvertStream)
  public
    constructor Create(Owner : TStMimeConverter); override;
    procedure DecodeToStream(InStream, OutStream : TStream); override;
    procedure EncodeToStream(InStream, OutStream : TStream); override;
  end;

  { Conversion stream for Quoted-Printable }
  TStQuotedStream = class(TStConvertStream)
  public
    constructor Create(Owner : TStMimeConverter); override;
    procedure DecodeToStream(InStream, OutStream : TStream); override;
    procedure EncodeToStream(InStream, OutStream : TStream); override;
  end;

  { Conversion stream for UUEncoding }
  TStUUStream = class(TStConvertStream)
  public
    constructor Create(Owner : TStMimeConverter); override;
    procedure DecodeToStream(InStream, OutStream : TStream); override;
    procedure EncodeToStream(InStream, OutStream : TStream); override;
  end;

  { Conversion stream for Base64 }
  TStBase64Stream = class(TStConvertStream)
  public
    constructor Create(Owner : TStMimeConverter); override;
    procedure DecodeToStream(InStream, OutStream : TStream); override;
    procedure EncodeToStream(InStream, OutStream : TStream); override;
  end;

{.Z-}

  TStConverterClass = class of TStConvertStream;

  TStAttachment = class
  protected {private}
    FContentDescription : string;
    FContentDisposition : string;
    FContentType : string;
    FEncoding : string;
    FFileName : string;
    FOldStyle : Boolean;
    FSize : Integer;
    FStreamOffset : Integer;
  public
    { Description of this attachment }
    property atContentDescription : string
      read FContentDescription write FContentDescription;

    { Disposition of this attachment }
    property atContentDisposition : string
      read FContentDisposition write FContentDisposition;

    { Content type of this attachment }
    property atContentType : string
      read FContentType write FContentType;

    { Encoding used for this attachment }
    property atEncoding : string
      read FEncoding write FEncoding;

    { Filename for this attachment }
    property atFilename : string
      read FFileName write FFileName;

    { Old style (non-mime) attachment }
    property atOldStyle : Boolean
      read FOldStyle write FOldStyle;

    { Size of attachment (in the unencoded state) }
    property atSize : Integer
      read FSize write FSize;

    { Offset of attachment in message }
    property atStreamOffset : Integer
      read FStreamOffset write FStreamOffset;
  end;

  TStMimeConverter = class
  protected {private}
    {.Z+}
    FAttachments : TStringList;
    FBoundary : string;
    FBoundaryUsed : Boolean;
    FContentDescription : string;
    FContentDisposition : string;
    FContentType : string;
    FConverter : TStConvertStream;
    FDirectory : string;
    FEncoding : string;
    FEndBoundaryOffset : Integer;
    FMimeHeaders : Boolean;
    FStream : TStream;
    FInternalStream : TMemoryStream;
    FOnProgress : TStProgressEvent;
    FOnSaveAs : TStSaveAsEvent;
    procedure AddMimeFooters;
    procedure AddMimeHeaders(const AFileName : string);
    procedure DeleteAttachments;
    procedure ForceType(ConverterType : TStConverterClass);
    function GetBoundary : string;
    function GetStream : TStream;
    procedure InitConverter;
    procedure SetBoundary(Value : string);
    procedure SetConverter(Value : TStConvertStream);
    procedure SetEncoding(Value : string);
    procedure SetStream(Value : TStream);
    {.Z-}
  protected
    procedure FindOldAttachment;
    function GenerateBoundary : string; dynamic;
    procedure PositionForExtract(Att : TStAttachment); dynamic;
    procedure Progress(Sender : TObject; Status : TStConvertState;
      PercentDone : Byte); dynamic;
    procedure SaveAs(var FileName : string);
    procedure ScanAttachments;
  public
    constructor Create;
    constructor CreateInit(AStream : TStream); virtual;
    destructor Destroy; override;
    procedure AddFileAttachment(const AFileName : string);
    procedure AddStreamAttachment(AStream : TStream; const AFileName : string); dynamic;
    procedure ExtractAttachment(const Attachment : string); dynamic;
    procedure ExtractAttachmentIndex(Index : Integer); dynamic;
    procedure ExtractToStream(Index : Integer; AStream : TStream); dynamic;
    procedure ExtractAttachments;
    procedure FillConverterList(List : TStrings);
    function GetTag(const Description : string): string;
    class procedure RegisterConverter(const ATag, ADesc : string;
      AClass : TStConverterClass);
    class procedure UnRegisterConverterClass(AClass : TStConverterClass);

    { List of attachments in current stream }
    property Attachments : TStringList
      read FAttachments;

    { Boundary being used for attachments }
    property Boundary : string
      read GetBoundary write SetBoundary;

    { Default encoding to use for attachments }
    property Encoding : string
      read FEncoding write SetEncoding;

    { Default Content Description to use for attachments }
    property ContentDescription : string
      read FContentDescription write FContentDescription;

    { Default Content Disposition to use for attachments }
    property ContentDisposition : string
      read FContentDisposition write FContentDisposition;

    { Default Content Type to use for attachments }
    property ContentType : string
      read FContentType write FContentType;

    { Instance of converter to be used with current encoding method }
    property Converter : TStConvertStream
      read FConverter write SetConverter;

    { Default directory used for ExtractAttachments }
    property Directory : string
      read FDirectory write FDirectory;

    { Determines whether Mime boundaries/headers are added to attachments }
    property MimeHeaders : Boolean
      read FMimeHeaders write FMimeHeaders default True;

    { Access to internal stream }
    property Stream : TStream
      read GetStream write SetStream;

    { Progress event -- optional for converters to support this }
    property OnProgress : TStProgressEvent
      read FOnProgress write FOnProgress;

    { SaveAs event -- fired when extracting an attachment  }
    property OnSaveAs : TStSaveAsEvent
      read FOnSaveAs write FOnSaveAs;
  end;

implementation

uses
  AnsiStrings;

const
  StUUTable : array[0..63] of AnsiChar = (#96, #33, #34, #35, #36, #37,
        #38, #39, #40, #41, #42, #43, #44, #45, #46, #47, #48, #49,
        #50, #51, #52, #53, #54, #55, #56, #57, #58, #59, #60, #61,
        #62, #63, #64, #65, #66, #67, #68, #69, #70, #71, #72, #73,
        #74, #75, #76, #77, #78, #79, #80, #81, #82, #83, #84, #85,
        #86, #87, #88, #89, #90, #91, #92, #93, #94, #95);

const
  St64Table : array[0..63] of AnsiChar = ( #65,  #66,  #67,  #68,  #69,
         #70,  #71,  #72,  #73,  #74,  #75,  #76,  #77,  #78,  #79,
         #80,  #81,  #82,  #83,  #84,  #85,  #86,  #87,  #88,  #89,
         #90,  #97,  #98,  #99, #100, #101, #102, #103, #104, #105,
        #106, #107, #108, #109, #110, #111, #112, #113, #114, #115,
        #116, #117, #118, #119, #120, #121, #122,  #48,  #49,  #50,
         #51,  #52,  #53,  #54,  #55,  #56,  #57,  #43,  #47);

const
  StD64Table : array[43..122] of Byte = ($3E, $7F, $7F, $7F, $3F, $34,
      $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $7F, $7F, $7F, $7F,
      $7F, $7F, $7F, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09,
      $0A, $0B, $0C, $0D, $0E, $0F, $10, $11, $12, $13, $14, $15, $16,
      $17, $18, $19, $7F, $7F, $7F, $7F, $7F, $7F, $1A, $1B, $1C, $1D,
      $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A,
      $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33);


var
  CvtLock : TRTLCriticalSection;

type
  TStContentTag = (ctType, ctEncoding, ctDescription, ctDisposition);

  PStTernaryNode = ^TStTernaryNode;
  TStTernaryNode = record
    SplitChar : AnsiChar;
    LoKid, EqKid, HiKid : PStTernaryNode;
  end;

  TStTernaryTree = class
  private
    Root : PStTernaryNode;
    pData : Pointer;
    function Insert(P : PStTernaryNode; C : PAnsiChar) : PStTernaryNode;
    class procedure DeleteSubTree(Root : PStTernaryNode);
    function NewNode : PStTernaryNode;
  public
    destructor Destroy; override;
    procedure InsertStr(C : PAnsiChar; Data : Pointer);
    function SearchUC(C : PAnsiChar; var Data : Pointer) : Boolean;
  end;

{ TStTernaryTree }
class procedure TStTernaryTree.DeleteSubTree(Root : PStTernaryNode);
begin
  if Root <> nil then begin
    DeleteSubTree(Root^.LoKid);
    if Root^.SplitChar <> #0 then
      DeleteSubTree(Root^.EqKid);
    DeleteSubTree(Root^.HiKid);
    Dispose(Root);
  end;
end;

destructor TStTernaryTree.Destroy;
begin
  DeleteSubTree(Root);
  inherited Destroy;
end;

function TStTernaryTree.NewNode : PStTernaryNode;
begin
  Result := AllocMem(SizeOf(TStTernaryNode));
end;

function TStTernaryTree.Insert(P : PStTernaryNode; C : PAnsiChar) : PStTernaryNode;
begin
  if P = nil then begin
    P := NewNode;
    P^.SplitChar := C^;
    if C^ <> #0 then begin
      Inc(C);
      P^.EqKid := Insert(P^.EqKid,C);
    end else
      P^.EqKid := pData;
    Result := P;
    Exit;
  end;
  if C^ < P^.SplitChar then
    P^.LoKid := Insert(P^.LoKid,C)
  else if C^ = P^.SplitChar then
    if C^ <> #0 then begin
      Inc(C);
      P^.EqKid := Insert(P^.EqKid,C);
    end else
      RaiseStError(EStMimeError, stscDupeString)
  else
    P^.HiKid := Insert(P^.HiKid,C);
  Result := P;
end;

procedure TStTernaryTree.InsertStr(C : PAnsiChar; Data : Pointer);
begin
  pData := Data;
  Root := Insert(Root, C);
end;

function TStTernaryTree.SearchUC(C : PAnsiChar; var Data : Pointer) : Boolean;
var
  P : PStTernaryNode;
  CU : AnsiChar;
begin
  P := Root;
  while P <> nil do begin
    CU := System.UpCase(C^);
    if CU < P^.SplitChar then
      P := P^.LoKid
    else if CU = P^.SplitChar then begin
      Inc(C);
      if C^ = #0 then begin
        Data := P^.EqKid^.EqKid;
        Result := True;
        Exit;
      end;
      P := P^.EqKid;
    end else
      P := P^.HiKid;
  end;
  Result := False;
end;

{ TStConvertStream }
constructor TStConvertStream.Create(Owner : TStMimeConverter);
begin
  FOwner := Owner;
  inherited Create;
end;

procedure TStConvertStream.Progress(Status : TStConvertState; PercentDone : Byte);
begin
  if Assigned(FOnProgress) then
    OnProgress(Self, Status, PercentDone);
end;

{ TStRawStream }
constructor TStRawStream.Create(Owner : TStMimeConverter);
begin
  inherited Create(Owner);
end;

procedure TStRawStream.DecodeToStream(InStream, OutStream : TStream);
begin
  Progress(csStarted, 0);
  try
    OutStream.CopyFrom(InStream, InStream.Size-InStream.Position);
  except
    Progress(csFinished, 0);
    raise;
  end;
  Progress(csFinished, 100);
end;

procedure TStRawStream.EncodeToStream(InStream, OutStream : TStream);
begin
  Progress(csStarted, 0);
  try
    OutStream.CopyFrom(InStream, InStream.Size);
  except
    Progress(csFinished, 0);
    raise;
  end;
  Progress(csFinished, 100);
end;

{ TStQuotedStream }
constructor TStQuotedStream.Create(Owner : TStMimeConverter);
begin
  inherited Create(Owner);
end;

procedure TStQuotedStream.DecodeToStream(InStream, OutStream : TStream);
var
  I, O, Count, WS : Byte;
  InBuf  : array[0..85] of Byte;
  OutBuf : array[0..85] of Byte;
  Decoding : Boolean;
  Keeper : Boolean;
begin
  FillChar(InBuf, SizeOf(InBuf), #0);
  WS := $FF;
  Decoding := True;
  Keeper := False;

  { Skip any CR/LF's to get to the encoded stuff }
  while True do begin
    Count := InStream.Read(InBuf, 1);
    { End of stream -- exit assuming we're done }
    if Count <> 1 then Exit;
    if ((InBuf[0] <> $0D) and (InBuf[0] <> $0A)) then begin
      Keeper := True;
      Break;
    end;
  end;

  while Decoding do begin
    { Initialize }
    if Keeper then begin
      I := 1;
      Keeper := False;
    end else begin
      I := 0;
    end;
    O := 0;

    { Read in one line at a time - skipping over bad characters }
    while True do begin
      if InStream.Read(InBuf[I], 1) = 0 then Break;
      case InBuf[I] of
        $0A : Continue;
        $0D : begin
                Inc(I);
                Break;
              end;
       { Test for potential end of data }
       { '--' is probably the next Mime boundary }
        $2D : if (I = 1) and (InBuf[0] = $2D) then Exit;
      end;
      Inc(I);
    end;

    if I = 0 then Exit;                                                
    Count := I;
    I := 0;

    { Decode data to output stream }
    while I < Count do begin
      case InBuf[I] of
        9       : begin
                    if WS = $FF then
                      WS := O;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        13      : if WS = $FF then begin
                    OutBuf[O] := 13;                                   
                    OutBuf[O+1] := 10;                                 
                    Inc(O, 2);
                    Inc(I);
                  end else begin
                    OutBuf[WS] := 13;
                    OutBuf[WS+1] := 10;
                    O := WS+2;
                    Inc(I);
                  end;
        32      : begin
                    if WS = $FF then
                      WS := O;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        33..60  : begin
                    WS := $FF;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        61      : begin
                    WS := $FF;
                    if I+2 >= Count then Break;
                    case InBuf[I+1] of
                      48 : OutBuf[O] := 0;    {0}
                      49 : OutBuf[O] := 16;   {1}
                      50 : OutBuf[O] := 32;   {2}
                      51 : OutBuf[O] := 48;   {3}
                      52 : OutBuf[O] := 64;   {4}
                      53 : OutBuf[O] := 80;   {5}
                      54 : OutBuf[O] := 96;   {6}
                      55 : OutBuf[O] := 112;  {7}
                      56 : OutBuf[O] := 128;  {8}
                      57 : OutBuf[O] := 144;  {9}
                      65 : OutBuf[O] := 160;  {A}
                      66 : OutBuf[O] := 176;  {B}
                      67 : OutBuf[O] := 192;  {C}
                      68 : OutBuf[O] := 208;  {D}
                      69 : OutBuf[O] := 224;  {E}
                      70 : OutBuf[O] := 240;  {F}
                      97 : OutBuf[O] := 160;  {a}
                      98 : OutBuf[O] := 176;  {b}
                      99 : OutBuf[O] := 192;  {c}
                     100 : OutBuf[O] := 208;  {d}
                     101 : OutBuf[O] := 224;  {e}
                     102 : OutBuf[O] := 240;  {f}
                    end;
                    case InBuf[I+2] of
                      48 : ;                             {0}
                      49 : OutBuf[O] := OutBuf[O] + 1;   {1}
                      50 : OutBuf[O] := OutBuf[O] + 2;   {2}
                      51 : OutBuf[O] := OutBuf[O] + 3;   {3}
                      52 : OutBuf[O] := OutBuf[O] + 4;   {4}
                      53 : OutBuf[O] := OutBuf[O] + 5;   {5}
                      54 : OutBuf[O] := OutBuf[O] + 6;   {6}
                      55 : OutBuf[O] := OutBuf[O] + 7;   {7}
                      56 : OutBuf[O] := OutBuf[O] + 8;   {8}
                      57 : OutBuf[O] := OutBuf[O] + 9;   {9}
                      65 : OutBuf[O] := OutBuf[O] + 10;  {A}
                      66 : OutBuf[O] := OutBuf[O] + 11;  {B}
                      67 : OutBuf[O] := OutBuf[O] + 12;  {C}
                      68 : OutBuf[O] := OutBuf[O] + 13;  {D}
                      69 : OutBuf[O] := OutBuf[O] + 14;  {E}
                      70 : OutBuf[O] := OutBuf[O] + 15;  {F}
                      97 : OutBuf[O] := OutBuf[O] + 10;  {a}
                      98 : OutBuf[O] := OutBuf[O] + 11;  {b}
                      99 : OutBuf[O] := OutBuf[O] + 12;  {c}
                     100 : OutBuf[O] := OutBuf[O] + 13;  {d}
                     101 : OutBuf[O] := OutBuf[O] + 14;  {e}
                     102 : OutBuf[O] := OutBuf[O] + 15;  {f}
                    end;
                    Inc(I, 3);
                    Inc(O);
                  end;
        62..126 : begin
                    WS := $FF;
                    OutBuf[O] := InBuf[I];
                    Inc(O);
                    Inc(I);
                  end;
        else
          Inc(I);
      end;
    end;

    if O>0 then                                                        
      OutStream.Write(OutBuf, O)
    else                                                               
      Break;   { OutBuf is empty }                                     
  end;
end;

procedure TStQuotedStream.EncodeToStream(InStream, OutStream : TStream);
var
  O, W : Integer;
  WordBuf, OutBuf : array[0..80] of AnsiChar;
  CurChar : AnsiChar;

  procedure SendLine;
  begin
    if (OutBuf[O-1] = #9) or (OutBuf[O-1] = #32) then begin
      OutBuf[O] := '=';
      Inc(O);
    end;
    OutBuf[O] := #13;
    OutBuf[O+1] := #10;
    Inc(O, 2);
    OutStream.Write(OutBuf, O);
    O := 0;
  end;

  procedure AddWordToOutBuf;
  var
    J : Integer;
  begin
    if (O + W) > 74 then SendLine;
    for J := 0 to (W - 1) do begin
      OutBuf[O] := WordBuf[J];
      Inc(O);
    end;
    W := 0;
  end;

  procedure AddHexToWord(B : Byte);
  begin
    if W > 73 then AddWordToOutBuf;
    WordBuf[W] := '=';
    WordBuf[W+1] := StHexDigits[B shr 4];
    WordBuf[W+2] := StHexDigits[B and $F];
    Inc(W, 3)
  end;

begin
  O := 0;
  W := 0;
  while InStream.Read(CurChar, 1) <> 0 do begin
    if (Ord(CurChar) in [33..60, 62..126]) then begin
      WordBuf[W] := CurChar;
      Inc(W);
      if W > 74 then AddWordToOutBuf;
    end else if (CurChar = ' ') or (CurChar = #9) then begin
      WordBuf[W] := CurChar;
      Inc(W);
      AddWordToOutBuf;
    end else if (CurChar = #13) then begin
      AddWordToOutBuf;
      SendLine;
    end else if (CurChar = #10) then begin
      { Do nothing }
    end else begin
      AddHexToWord(Byte(CurChar));
    end;
  end;
end;

{ TStUUStream }
constructor TStUUStream.Create(Owner : TStMimeConverter);
begin
  inherited Create(Owner);
end;

procedure TStUUStream.DecodeToStream(InStream, OutStream : TStream);
var
  I, O, Len, Count : Byte;
  InBuf  : array[0..85] of Byte;
  OutBuf : array[0..65] of Byte;
  FirstLine : Boolean;
begin
  FirstLine := True;

  while True do begin
    { Initialize }
    I := 0;
    O := 0;

    { Skip any CR/LF's to get to the encoded stuff }
    while True do begin
      Count := InStream.Read(InBuf, 1);
      { End of stream -- bail assuming we're done }
      if Count <> 1 then Exit;
      if FirstLine then begin
        if ((InBuf[0] <> $0D) and (InBuf[0] <> $0A)) then begin
          FirstLine := False;
          Break;
        end;
      end else begin
        if ((InBuf[0] = $0D) or (InBuf[0] = $0A)) then FirstLine := True;
      end;
    end;

    { We're done }
    if AnsiChar(InBuf[0]) = '`' then Exit;

    { Get count for this line }
    Len := (((InBuf[0] - $20) and $3F) * 4) div 3;
    if (((InBuf[0] - $20) and $3F) * 4) mod 3 <> 0 then
      Inc(Len);

    Count := InStream.Read(InBuf, Len);

    { Unexpected situation }
    if (Count <> Len) or (Count > 63) then
      RaiseStError(EStMimeError, stscInStream);
    { Decode buffer }
    while (I < Count) do begin
      if ((Count - I) >= 4) then begin
{!!.01 -- Changed}
{Note: indexing inverted to avoid compiler problem with D6upd2 and BCB6}
        OutBuf[O+2] := (((InBuf[I+2] - $20) and $3F) shl 6) or
          (((InBuf[I+3] - $20) and $3F));
        OutBuf[O+1] := (((InBuf[I+1] - $20) and $3F) shl 4) or
          (((InBuf[I+2] - $20) and $3F) shr 2);
        OutBuf[O] := (((InBuf[I] - $20) and $3F) shl 2) or
          (((InBuf[I+1] - $20) and $3F) shr 4);
{!!.01 -- End Changed}
        Inc(O, 3);
      end else begin
        if (Count >= 2) then begin
          OutBuf[O] := (((InBuf[I] - $20) and $3F) shl 2) or
            (((InBuf[I+1] - $20) and $3F) shr 4);
          Inc(O);
        end;
        if (Count >= 3) then begin
          OutBuf[O+1] := (((InBuf[I+1] - $20) and $3F) shl 4) or
            (((InBuf[I+2] - $20) and $3F) shr 2);
          Inc(O);
        end;
      end;
      Inc(I, 4);
    end;
    OutStream.Write(OutBuf, O);

  end;
end;

procedure TStUUStream.EncodeToStream(InStream, OutStream : TStream);
var
  I, O, Count, Temp : Byte;
  InBuf  : array[1..45] of Byte;
  OutBuf : array[0..63] of AnsiChar;
  S : AnsiString;
begin
  S := AnsiString(Format('begin 600 %s'#13#10, [FCurrentFile]));
  OutStream.Write(S[1], Length(S));

  { Encode and stream the attachment }
  repeat
    Count := InStream.Read(InBuf, SizeOf(InBuf));
    if Count <= 0 then Break;
    I := 1;
    O := 0;
    OutBuf[O] := AnsiChar(StUUTable[Count and $3F]);
    Inc(O);
    while I+2 <= Count do begin
      { Encode 1st byte }
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := AnsiChar(StUUTable[Temp and $3F]);

      { Encode 1st/2nd byte }
      Temp := (InBuf[I] shl 4) or (InBuf[I+1] shr 4);
      OutBuf[O+1] := AnsiChar(StUUTable[Temp and $3F]);

      { Encode 2nd/3rd byte }
      Temp := (InBuf[I+1] shl 2) or (InBuf[I+2] shr 6);
      OutBuf[O+2] := AnsiChar(StUUTable[Temp and $3F]);

      { Encode 3rd byte }
      Temp := (InBuf[I+2] and $3F);
      OutBuf[O+3] := AnsiChar(StUUTable[Temp]);

      Inc(I, 3);
      Inc(O, 4);
    end;

    { Are there odd bytes to add? }
    if (I <= Count) then begin
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := AnsiChar(StUUTable[Temp and $3F]);

      { One odd byte }
      if (I = Count) then begin
        Temp := (InBuf[I] shl 4) and $30;
        OutBuf[O+1] := AnsiChar(StUUTable[Temp and $3F]);
        Inc(O, 2);
      { Two odd bytes }
      end else begin
        Temp := ((InBuf[I] shl 4) and $30) or ((InBuf[I+1] shr 4) and $0F);
        OutBuf[O+1] := AnsiChar(StUUTable[Temp and $3F]);
        Temp := (InBuf[I+1] shl 2) and $3C;
        OutBuf[O+2] := AnsiChar(StUUTable[Temp and $3F]);
        Inc(O, 3);
      end;
    end;

    { Add CR/LF }
    OutBuf[O] := #13;
    OutBuf[O+1] := #10;

    { Write line to stream }
    OutStream.Write(OutBuf, (O + 2));
  until Count < SizeOf(InBuf);

  { Add terminating end }
  AnsiStrings.StrCopy(AnsiStrings.StrECopy(OutBuf, '`'#13#10), 'end'#13#10);
  OutStream.Write(OutBuf, AnsiStrings.StrLen(OutBuf));
end;

{ TStBase64Stream }
constructor TStBase64Stream.Create(Owner : TStMimeConverter);
begin
  inherited Create(Owner);
end;

procedure TStBase64Stream.DecodeToStream(InStream, OutStream : TStream);
var
  I, O, Count, c1, c2, c3 : Byte;
  InBuf  : array[0..85] of Byte;
  OutBuf : array[0..65] of Byte;
  Decoding : Boolean;
begin
  Decoding := True;
  while Decoding do begin
    { Initialize }
    O := 0;
    I := 0;

    { Skip any CR/LF's to get to the encoded stuff }
    while True do begin
      Count := InStream.Read(InBuf, 1);
      { End of stream -- exit assuming we're done }
      if Count <> 1 then Exit;
      if ((InBuf[0] <> $0D) and (InBuf[0] <> $0A)) then begin
        Inc(I);
        Break;
      end;
    end;

    { Read in a line at a time - skipping over bad characters }
    while True do begin
      if InStream.Read(InBuf[I], 1) = 0 then Break;
      case InBuf[I] of
        $00..$09 : Continue;
        $0A      : Break;
        $0B..$0C : Continue;
        $0D      : Break;
        $0E..$2A : Continue;
       { Test for potential end of data }
       { '--' is probably the next Mime boundary }
        $2D      : begin
                     if I = 1 then
                       if (InBuf[0] = $2D) then
                         Exit;
                     Continue;
                   end;
        $3D      : begin
                     Inc(I);
                     Break;
                   end;
        $7B..$FF : Continue;
      end;

      if (StD64Table[InBuf[I]] = $7F) then Continue;
      Inc(I);
    end;

    { 'end' can mean end of data for base64 }
    if (InBuf[0] = $65) and (InBuf[1] = $6E) and
       (InBuf[2] = $64) and (InBuf[3] = $0D) then Exit;

    Count := I;
    I := 0;

    { Decode data to output stream }
    while I < Count do begin
      c1 := StD64Table[InBuf[I]];
      c2 := StD64Table[InBuf[I+1]];
      c3 := StD64Table[InBuf[I+2]];
      OutBuf[O] := ((c1 shl 2) or (c2 shr 4));
      Inc(O);
      if AnsiChar(InBuf[I+2]) <> '=' then begin
        OutBuf[O] := ((c2 shl 4) or (c3 shr 2));
        Inc(O);
        if AnsiChar(InBuf[I+3]) <> '=' then begin
          OutBuf[O] := ((c3 shl 6) or StD64Table[InBuf[I+3]]);
          Inc(O);
        end else
          Decoding := False;
      end else
        Decoding := False;
      Inc(I, 4);
    end;
    OutStream.Write(OutBuf, O);
  end;
end;

procedure TStBase64Stream.EncodeToStream(InStream, OutStream : TStream);
var
  I, O, Count : Integer;
  InBuf  : array[1..45] of Byte;
  OutBuf : array[0..62] of AnsiChar;
  Temp : Byte;
  S : AnsiString;
begin
  FillChar(OutBuf, Sizeof(OutBuf), #0);

  if not FOwner.MimeHeaders then begin
    S := AnsiString(Format('begin-base64 600 %s'#13#10, [FCurrentFile]));
    OutStream.Write(S[1], Length(S));
  end;

  { Encode and stream the attachment }
  repeat
    Count := InStream.Read(InBuf, SizeOf(InBuf));
    if Count = 0 then Break;
    I := 1;
    O := 0;
    while I <= (Count-2) do begin
      { Encode 1st byte }
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := AnsiChar(St64Table[Temp and $3F]);

      { Encode 1st/2nd byte }
      Temp := (InBuf[I] shl 4) or (InBuf[I+1] shr 4);
      OutBuf[O+1] := AnsiChar(St64Table[Temp and $3F]);

      { Encode 2nd/3rd byte }
      Temp := (InBuf[I+1] shl 2) or (InBuf[I+2] shr 6);
      OutBuf[O+2] := AnsiChar(St64Table[Temp and $3F]);

      { Encode 3rd byte }
      Temp := (InBuf[I+2] and $3F);
      OutBuf[O+3] := AnsiChar(St64Table[Temp]);

      Inc(I, 3);
      Inc(O, 4);
    end;

    { Are there odd bytes to add? }
    if (I <= Count) then begin
      Temp := (InBuf[I] shr 2);
      OutBuf[O] := AnsiChar(St64Table[Temp and $3F]);

      { One odd byte }
      if I = Count then begin
        Temp := (InBuf[I] shl 4) and $30;
        OutBuf[O+1] := AnsiChar(St64Table[Temp and $3F]);
        OutBuf[O+2] := '=';
      { Two odd bytes }
      end else begin
        Temp := ((InBuf[I] shl 4) and $30) or ((InBuf[I+1] shr 4) and $0F);
        OutBuf[O+1] := AnsiChar(St64Table[Temp and $3F]);
        Temp := (InBuf[I+1] shl 2) and $3C;
        OutBuf[O+2] := AnsiChar(St64Table[Temp and $3F]);
      end;
      { Add padding }
      OutBuf[O+3] := '=';
      Inc(O, 4);
    end;

    { Add CR/LF }
    OutBuf[O] := #13;
    OutBuf[O+1] := #10;

    { Write line to stream }
    OutStream.Write(OutBuf, (O + 2));
  until Count < SizeOf(InBuf);

  { Add terminating end if necessary }
  if not FOwner.MimeHeaders then begin
    AnsiStrings.StrCopy(OutBuf, 'end'#13#10);
    OutStream.Write(OutBuf, AnsiStrings.StrLen(OutBuf));
  end;
end;


{ TConverterList }

type
  TCvtFormat = class
    ConverterClass : TStConverterClass;
    Description : string;
  end;

  TConverterList = class(TStringList)
  protected
    procedure LockList;
    procedure UnlockList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddConverter(const ATag, ADesc : string; AClass : TStConverterClass);
    procedure Remove(AClass: TStConverterClass);
  end;

constructor TConverterList.Create;
begin
  inherited Create;
  Sorted := True;
  Duplicates := dupError;
  AddConverter('raw', 'Raw Copy', TStRawStream);
  AddConverter('base64', 'Base64', TStBase64Stream);
  AddConverter('quoted-printable', 'Quoted-Printable', TStQuotedStream);
  AddConverter('uuencoded', 'UUEncoded', TStUUStream);
end;

destructor TConverterList.Destroy;
var
  I: Integer;
begin
  LockList;
  try
    for I := 0 to Count-1 do
      TCvtFormat(Objects[I]).Free;
    inherited Destroy;
  finally
    UnlockList;
  end;
end;

procedure TConverterList.LockList;
begin
  EnterCriticalSection(CvtLock);
end;

procedure TConverterList.UnlockList;
begin
  LeaveCriticalSection(CvtLock);
end;

procedure TConverterList.AddConverter(const ATag, ADesc : string; AClass : TStConverterClass);
var
  Temp : TCvtFormat;
begin
  LockList;
  try
    Temp := TCvtFormat.Create;
    with Temp do begin
      ConverterClass := AClass;
      Description := ADesc;
    end;
    AddObject(ATag, Temp);
  finally
    UnlockList;
  end;
end;

procedure TConverterList.Remove(AClass: TStConverterClass);
var
  I : Integer;
  Cvt : TCvtFormat;
begin
  LockList;
  try
    for I := Count-1 downto 0 do begin
      Cvt := TCvtFormat(Objects[I]);
      if Cvt.ConverterClass.InheritsFrom(AClass) then begin
        Cvt.Free;
        Delete(I);
      end;
    end;
  finally
    UnlockList;
  end;
end;

const
  Converters : TConverterList = nil;

  function GetConverters : TConverterList;
  begin
    EnterCriticalSection(CvtLock);
    try
      if Converters = nil then
        Converters := TConverterList.Create;
      Result := Converters;
    finally
      LeaveCriticalSection(CvtLock);
    end;
  end;

{ TStMimeConverter }
constructor TStMimeConverter.Create;
begin
  inherited Create;
  InitConverter;
end;

constructor TStMimeConverter.CreateInit(AStream : TStream);
begin
  inherited Create;
  InitConverter;
  if Assigned(AStream) then
    Stream := AStream;
end;

destructor TStMimeConverter.Destroy;
begin
  DeleteAttachments;
  FAttachments.Free;
  FInternalStream.Free;
  FConverter.Free;
  inherited Destroy;
end;

procedure TStMimeConverter.AddFileAttachment(const AFileName : string);
var
  F : TFileStream;
begin
  F := TFileStream.Create(AFileName, AttachmentFileMode);
  try
    AddStreamAttachment(F, AFileName);
  finally
    F.Free;
  end;
end;

procedure TStMimeConverter.AddMimeFooters;
var
  SavePos : Integer;
  Temp : AnsiString;
begin
  SavePos := Stream.Position;
  Stream.Write(CRLFStr, SizeOf(CRLFStr));
  Temp := AnsiString('--' + Boundary + '--');
  Stream.Write(Temp[1], Length(Temp));
  Stream.Write(CRLFStr, SizeOf(CRLFStr));
  Stream.Position := SavePos;
end;

procedure TStMimeConverter.AddMimeHeaders(const AFileName : string);
var
  Temp: AnsiString;
  Descr : string;
begin
  Stream.Write(CRLFStr, SizeOf(CRLFStr));
  Temp := AnsiString('--' + Boundary);
  Stream.Write(Temp[1], Length(Temp));
  Stream.Write(CRLFStr, SizeOf(CRLFStr));

  Temp := AnsiString(Format('Content-Type: %s; name="%s"'#13#10,
    [ContentType, ExtractFileName(AFileName)]));
  Stream.Write(Temp[1], Length(Temp));

  Temp := AnsiString(Format('Content-Transfer-Encoding: %s'#13#10, [Encoding]));
  Stream.Write(Temp[1], Length(Temp));

  if ContentDescription = '' then
    Descr := ExtractFileName(AFileName)
  else
    Descr := ContentDescription;
  Temp := AnsiString(Format('Content-Description: %s'#13#10, [Descr]));
  Stream.Write(Temp[1], Length(Temp));

  Temp := AnsiString(Format('Content-Disposition: %s; filename="%s"'#13#10#13#10,
    [ContentDisposition, ExtractFileName(AFileName)]));
  Stream.Write(Temp[1], Length(Temp));
end;

procedure TStMimeConverter.AddStreamAttachment(AStream : TStream; const AFileName : string);
var
  I : Integer;
  AttObj : TStAttachment;
  SavePos : Integer;
begin
  if Converters.Find(FEncoding, I) then
    ForceType(TCvtFormat(Converters.Objects[I]).ConverterClass)
  else
    RaiseStError(EStMimeError, stscBadEncodeFmt);

  SavePos := Stream.Position;

  if FMimeHeaders then
    AddMimeHeaders(AFileName);

  FConverter.CurrentFile := ExtractFilename(AFileName);
  FConverter.EncodeToStream(AStream, Stream);

  if MimeHeaders then
    AddMimeFooters;

  AttObj := TStAttachment.Create;
  with AttObj do begin
    atContentDescription := ContentDescription;
    atContentDisposition := ContentDisposition;
    atContentType := ContentType;
    atEncoding := Encoding;
    atSize := AStream.Size;
    atStreamOffset := SavePos;
  end;

  FAttachments.AddObject(ExtractFilename(AFileName), AttObj);
end;

procedure TStMimeConverter.DeleteAttachments;
var
  I : Integer;
begin
  if not Assigned(FAttachments) then Exit;
  for I := 0 to (FAttachments.Count - 1) do
    TStAttachment(FAttachments.Objects[I]).Free;
  FAttachments.Clear;
end;

procedure TStMimeConverter.ExtractAttachment(const Attachment : string);
var
  I : Integer;
begin
  if FAttachments.Find(Attachment, I) then
    ExtractAttachmentIndex(I)
  else
    RaiseStError(EStMimeError, stscBadAttachment);
end;

procedure TStMimeConverter.ExtractAttachmentIndex(Index : Integer);
var
  F : TFileStream;
  S : string;
begin
  if (Index < 0) or (Index > (FAttachments.Count - 1)) then
    RaiseStError(EStMimeError, stscBadAttachment);

  if FDirectory <> '' then begin
    S := JustPathNameL(FDirectory);
    S := AddBackSlashL(S);
    S := S + TStAttachment(FAttachments.Objects[Index]).atFileName;
  end else begin
    S := TStAttachment(FAttachments.Objects[Index]).atFileName;
  end;

  SaveAs(S);
  F := TFileStream.Create(S, fmCreate);
  try
    ExtractToStream(Index, F);
  finally
    F.Free;
  end;
end;

procedure TStMimeConverter.ExtractToStream(Index : Integer; AStream : TStream);
var
  I : Integer;
  SaveEncoding : string;
begin
  SaveEncoding := FEncoding;
  try
    { Position stream to beginning of data }
    if (Index < 0) or (Index > (FAttachments.Count - 1)) then
      RaiseStError(EStMimeError, stscBadAttachment);
    PositionForExtract(TStAttachment(FAttachments.Objects[Index]));

    { Find matching converter type and use it }
    if Converters.Find(TStAttachment(FAttachments.Objects[Index]).atEncoding, I) then
      ForceType(TCvtFormat(Converters.Objects[I]).ConverterClass)
    else
      { If we don't have a matching converter, save as a raw stream }
      ForceType(TStRawStream);

    FConverter.DecodeToStream(Stream, AStream);
  finally
    FEncoding := SaveEncoding;
  end;
end;

procedure TStMimeConverter.ExtractAttachments;
var
  I : Integer;
begin
  for I := 0 to (FAttachments.Count - 1) do
    ExtractAttachmentIndex(I);
end;

procedure TStMimeConverter.FillConverterList(List : TStrings);
var
  I : Integer;
begin
  List.Clear;
  for I := 0 to (Converters.Count - 1) do
    List.Add(TCvtFormat(Converters.Objects[I]).Description);
end;

procedure TStMimeConverter.FindOldAttachment;
const
  StmSize = 32*1024;
type
  MemArray = array[0..(StmSize-1)] of AnsiChar;
var
  I, Pos, ScanSize, StmOffset : Integer;
  NewAtt : TStAttachment;
  ScanStream : TMemoryStream;
  FoundPos : Cardinal;
  SearchString : AnsiString;//array[0..80] of Char;
  TempBuf : AnsiString; //array[0..80] of Char;
  TokenBuf : AnsiString; //array[0..80] of Char;
  TempWord : Word;
  BMT : BTable;

  function Min(A, B : Integer) : Integer;
  begin
    Result := A;
    if A > B then
      Result := B;
  end;

begin
  NewAtt := nil;

  { Position stream to beginning }
  Stream.Seek(0, soFromBeginning);

  { Create memory stream for search }
  ScanStream := TMemoryStream.Create;
  try
    ScanStream.SetSize(StmSize);
    StmOffset := Stream.Position;
    ScanSize := ScanStream.CopyFrom(Stream, Min(StmSize,
      (Stream.Size - Stream.Position)));

    SearchString := #13#10'begin'; //  StrPCopy(SearchString, #13#10'begin');
    BMMakeTableZ(PAnsiChar(SearchString), BMT);
    ScanStream.Position := 0;

    while True do begin
      { Look for an old style attachment -- process appropriately }
      if BMSearchZ(ScanStream.Memory^, ScanSize, BMT, PAnsiChar(SearchString), FoundPos) then begin

        FillChar(TempBuf, SizeOf(TempBuf), #0);
        Pos := FoundPos + 2;

        { Collect line containing potential begin marker }
        for I := 0 to 79 do begin
          if MemArray(ScanStream.Memory^)[Pos+I] = #13 then Break;
          TempBuf[I] := MemArray(ScanStream.Memory^)[Pos+I];
        end;

        { Grab second word -- should be a number if this is an attachment }
        TokenBuf := AnsiString(ExtractWordL(2, string(TempBuf), ' '));
        if Str2WordL(string(TokenBuf), TempWord) then begin
          { We've got an attachment }
          NewAtt := TStAttachment.Create;
          NewAtt.atStreamOffset := Pos;
          TokenBuf := AnsiString(ExtractWordL(1, string(TempBuf), ' '));
          if CompStringL(string(TokenBuf), 'begin') = 0 then
            NewAtt.atEncoding := 'uuencoded'
          else
            NewAtt.atEncoding := 'base64';
          TokenBuf := AnsiString(ExtractWordL(3, string(TempBuf), ' '));
          NewAtt.atFilename := string(TokenBuf);
          NewAtt.atOldStyle := True;
          Attachments.AddObject(NewAtt.atFileName, NewAtt);
          NewAtt := nil;
          Break;
        end else begin
          Stream.Position := (StmOffset + Integer(FoundPos) + Integer(Length(SearchString)));
          StmOffset := Stream.Position;
          ScanStream.Position := 0;
          ScanSize := ScanStream.CopyFrom(Stream,
            Min(StmSize, (Stream.Size - Stream.Position)));
        end;

      end else begin
        if (ScanSize < StmSize) then Exit;
        Stream.Seek(-Length(SearchString), soFromCurrent);
        StmOffset := Stream.Position;
        ScanStream.Position := 0;
        ScanSize := ScanStream.CopyFrom(Stream,
          Min(StmSize, (Stream.Size - Stream.Position)));
      end;
    end;
  finally
    ScanStream.Free;
    NewAtt.Free;
  end;
end;

procedure TStMimeConverter.ForceType(ConverterType : TStConverterClass);
begin
  if not (Converter is ConverterType) then begin
    FConverter.Free;
    FConverter := nil;
    FConverter := ConverterType.Create(Self);
    FConverter.OnProgress := Progress;
  end;
end;

function TStMimeConverter.GenerateBoundary : string;
var
  Temp : TDateTime;
begin
  Temp := Now;
  Randomize;
  Result := 'StMime-' + IntToHex(Trunc(Temp), 8) + '-' +
    IntToHex(Trunc(Frac(Temp) * 10000), 8) + '-' +
    IntToHex(GetTickCount, 8) + '-' + IntToHex(Random($FFFF), 4);
end;

function TStMimeConverter.GetBoundary : string;
begin
  if FBoundary = '' then
    FBoundary := GenerateBoundary;
  Result := FBoundary;
end;

function TStMimeConverter.GetStream : TStream;
begin
  if not Assigned(FStream) then begin
    if not Assigned(FInternalStream) then
      FInternalStream := TMemoryStream.Create;
    FStream := FInternalStream;
  end;
  Result := FStream;
end;

function TStMimeConverter.GetTag(const Description : string): string;
var
  I : Integer;
begin
  for I := 0 to (Converters.Count - 1) do begin
    if CompareStr(Description,
      TCvtFormat(Converters.Objects[I]).Description) = 0 then begin
      Result := Converters[I];
      Exit;
    end;
  end;
end;

procedure TStMimeConverter.InitConverter;
begin
  FAttachments := TStringList.Create;
  FContentType := DefStContentType;
  FContentDisposition := DefStContentDisposition;
  FEncoding := DefStMimeEncoding;
  FMimeHeaders := True;
  GetConverters;
end;

procedure TStMimeConverter.PositionForExtract(Att : TStAttachment);
const
  BufSize = 1024;
var
  I : Integer;
  Ptr : PAnsiChar;
  TempBuf : array[0..BufSize] of AnsiChar;
begin
  FillChar(TempBuf, SizeOf(TempBuf), #0);
  Stream.Position := Att.atStreamOffset;
  Stream.Read(TempBuf, BufSize);
  if Att.atOldStyle then begin
    for I := 0 to BufSize do begin
      if TempBuf[I] = #13 then begin
        Stream.Position := (Att.atStreamOffset + I);
        Exit;
      end;
    end;
  end else begin
    Ptr := AnsiStrings.StrPos(TempBuf, #13#10#13#10'');
    Stream.Position := (Att.atStreamOffset + (Ptr - TempBuf));
  end;
end;

procedure TStMimeConverter.Progress(Sender : TObject; Status : TStConvertState;
  PercentDone : Byte);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Status, PercentDone);
end;

class procedure TStMimeConverter.RegisterConverter(const ATag, ADesc : string;
  AClass : TStConverterClass);
begin
  GetConverters.AddConverter(ATag, ADesc, AClass);
end;

procedure TStMimeConverter.SaveAs(var FileName : string);
begin
  if Assigned(FOnSaveAs) then FOnSaveAs(Self, FileName);
end;

procedure TStMimeConverter.ScanAttachments;
const
  StmSize = 32*1024;
type
  MemArray = array[0..(StmSize-1)] of AnsiChar;
var
  I, Pos, ScanSize, StmOffset : Integer;
  TTree : TStTernaryTree;
  TTag : Pointer;
  NewAtt : TStAttachment;
  ScanStream : TMemoryStream;
  OStr : TStString;
  FoundPos, BoundPos : Cardinal;
  SearchString : array[0..80] of AnsiChar;
  TempBuf : array[0..1024] of AnsiChar;
  AttToken : array[0..MaxMimeLine] of AnsiChar;
  BMT : BTable;

  function Min(A, B : Integer) : Integer;
  begin
    Result := A;
    if A > B then
      Result := B;
  end;

  procedure InitTree;
  begin
    TTree := TStTernaryTree.Create;
    with TTree do begin
      InsertStr('CONTENT-TYPE', Pointer(ctType));
      InsertStr('CONTENT-TRANSFER-ENCODING', Pointer(ctEncoding));
      InsertStr('CONTENT-DESCRIPTION', Pointer(ctDescription));
      InsertStr('CONTENT-DISPOSITION', Pointer(ctDisposition));
    end;
  end;

begin
  NewAtt := nil;
  TTree := nil;

  DeleteAttachments;

  { Position stream to beginning }
  Stream.Seek(0, soFromBeginning);

  { Create memory stream for search }
  ScanStream := TMemoryStream.Create;
  try
    ScanStream.SetSize(StmSize);
    StmOffset := Stream.Position;
    ScanSize := ScanStream.CopyFrom(Stream, Min(StmSize,
      (Stream.Size - Stream.Position)));

    { If we have a boundary, use it -- if not, look for one }
    if FBoundary = '' then
      AnsiStrings.StrCopy(SearchString, #13#10'--')
    else begin
      FBoundaryUsed := True;
      AnsiStrings.StrPCopy(SearchString, AnsiString('--' + Boundary));
    end;
    BMMakeTableZ(SearchString, BMT);
    ScanStream.Position := 0;

    while True do begin
      { Look for a Mime boundary -- process appropriately }
      if BMSearchZ(ScanStream.Memory^, ScanSize, BMT, SearchString, FoundPos) then begin

        Pos := FoundPos + AnsiStrings.StrLen(SearchString);

        { Add add'l checks here -- look for the Boundary header entry first }
        { if that method fails, beef up this method against false positives a bit }
        { maybe checking for 'Content-' shortly following this potential boundary }

        { Do we have a boundary? If not, assume this might be it and collect }
        if FBoundary = '' then begin
          FillChar(TempBuf, SizeOf(TempBuf), #0);
          for I := 0 to (MaxMimeLine - 1) do begin
            if MemArray(ScanStream.Memory^)[Pos+I] = #13 then begin
              Pos := Pos+I;
              Break;
            end;
            TempBuf[I] := MemArray(ScanStream.Memory^)[Pos+I];
          end;
          Boundary := string(AnsiStrings.StrPas(TempBuf));
          AnsiStrings.StrCopy(AnsiStrings.StrECopy(SearchString, '--'), TempBuf);

          { Adjust to account for CR/LF searched on this go around }
          Inc(FoundPos, 2);

          { Get this out of the way for subsequent searches }
          BMMakeTableZ(SearchString, BMT);
        end;

        if not Assigned(TTree) then InitTree;

        { Check to see if this was an 'ending' boundary }
        if (MemArray(ScanStream.Memory^)[Pos] = '-') and
           (MemArray(ScanStream.Memory^)[Pos+1] = '-') then begin
          { Position the stream to the beginning of the end marker }
          FEndBoundaryOffset := (StmOffset + Integer(FoundPos) - 2);
          Stream.Position := FEndBoundaryOffset;
          Exit;
        end else begin
          if not Assigned(NewAtt) then NewAtt := TStAttachment.Create;
          { Go ahead and reposition here -- won't lose us much, and it }
          { guarantees all tags for this attachment will be within the buffer }
          NewAtt.atStreamOffset := (StmOffset + Integer(FoundPos));
          Stream.Position := (StmOffset + Integer(FoundPos) + Length(FBoundary) + 2);
          StmOffset := Stream.Position;
          ScanStream.Position := 0;
          if Stream.Position >= Stream.Size then Exit;
          ScanSize := ScanStream.CopyFrom(Stream,
            Min(StmSize, (Stream.Size - Stream.Position)));
        end;

        { Init for token search }
        OStr := TStString.CreateZ(AnsiStrings.StrLCopy(TempBuf, ScanStream.Memory, SizeOf(TempBuf)-1));
        try
          with OStr do begin
            { Check for another boundary in buffer }
            if not BMSearchUC(AnsiString(FBoundary), BoundPos) then
              BoundPos := SizeOf(TempBuf);
            Delimiters := ' :;='#13#10;
            Quote := '"';
            EnableCursor := True;
            RepeatValue := 10;
            BMSearchUC('Content-', FoundPos);
            RepeatValue := 1;
          end;

          for I := 0 to OStr.Items.Count-1 do begin

            OStr.CursorPos := Cardinal(OStr.Items.Objects[I]);

            { These tokens belong to the next section }
            if OStr.CursorPos > BoundPos then Break;

            OStr.GetWordAtCursorZ(AttToken);

            { Process tag appropriately }
            if TTree.SearchUC(AttToken, TTag) then begin
              case TStContentTag(TTag) of
                ctType :
                  begin
                    OStr.CursorNextWord;
                    NewAtt.atContentType := string(OStr.GetAsciiAtCursor);
                    OStr.CursorNextWord;
                    if CompareText(string(OStr.GetAsciiAtCursor), 'name') = 0 then begin
                      OStr.Delimiters := ' :;="'#13#10;
                      OStr.CursorNextWord;
                      NewAtt.atFileName := string(OStr.GetWordAtCursor);
                      OStr.Delimiters := ' :;='#13#10;
                    end;
                  end;
                ctEncoding :
                  begin
                    OStr.CursorNextWord;
                    NewAtt.atEncoding := string(OStr.GetAsciiAtCursor);
                  end;
                ctDescription :
                  begin
                    OStr.CursorNextWord;
                    NewAtt.atContentDescription := string(OStr.GetAsciiAtCursor);
                  end;
                ctDisposition :
                  begin
                    OStr.CursorNextWord;
                    NewAtt.atContentDisposition := string(OStr.GetAsciiAtCursor);
                    OStr.CursorNextWord;
                    if CompareText(string(OStr.GetAsciiAtCursor), 'filename') = 0 then begin
                      OStr.Delimiters := ' :;="'#13#10;
                      OStr.CursorNextWord;
                      NewAtt.atFileName := string(OStr.GetWordAtCursor);
                      OStr.Delimiters := ' :;='#13#10;
                    end;
                  end;
              end;
            end;
          end;
          { If it's an 'attachment' -- add it to the list }
          if CompareText(NewAtt.atContentDisposition, 'attachment') = 0 then begin
            if NewAtt.atFilename = '' then
              NewAtt.atFileName := 'attach' + IntToStr(FAttachments.Count) + '.att';
            Attachments.AddObject(NewAtt.atFileName, NewAtt);
            NewAtt := nil;
          end else if CompareText(NewAtt.atContentDisposition, 'inline') = 0 then begin
            if NewAtt.atFilename = '' then
              NewAtt.atFileName := 'attach' + IntToStr(FAttachments.Count) + '.att';
            Attachments.AddObject(NewAtt.atFileName, NewAtt);
            NewAtt := nil;
          end;
        finally
          OStr.Free;
        end;

      end else begin
        if (ScanSize < StmSize) then Exit;
        Stream.Seek(-AnsiStrings.StrLen(SearchString), soCurrent);
        StmOffset := Stream.Position;
        ScanStream.Position := 0;
        ScanSize := ScanStream.CopyFrom(Stream,
          Min(StmSize, (Stream.Size - Stream.Position)));
      end;
    end;
  finally
    ScanStream.Free;
    NewAtt.Free;
    TTree.Free;
    if FAttachments.Count = 0 then FindOldAttachment;
  end;
end;

procedure TStMimeConverter.SetBoundary(Value : string);
begin
  if CompareStr(FBoundary, Value) <> 0 then begin
    FBoundary := Value;
    FBoundaryUsed := False;
    if Length(Value) > 74 then
      SetLength(FBoundary, 74);
  end;
end;

procedure TStMimeConverter.SetConverter(Value : TStConvertStream);
var
  NewConverter : TStConvertStream;
begin
  NewConverter := nil;
  if Value <> nil then begin
    NewConverter := TStConverterClass(Value.ClassType).Create(Self);
    NewConverter.OnProgress := Progress;
  end;
  try
    FConverter.Free;
    FConverter := NewConverter;
  except
    NewConverter.Free;
    raise;
  end;
end;

procedure TStMimeConverter.SetEncoding(Value : string);
var
  I : Integer;
begin
  if FEncoding <> Value then begin
    if Converters.Find(Value, I) then begin
      FEncoding := Value;
      ForceType(TCvtFormat(Converters.Objects[I]).ConverterClass);
    end else
      RaiseStError(EStMimeError, stscBadEncodeFmt);
  end;
end;

procedure TStMimeConverter.SetStream(Value : TStream);
begin
  {if FStream <> Value then begin}
    FStream := Value;
    if FBoundaryUsed then
      Boundary := '';
    ScanAttachments;
  {end;}
end;

class procedure TStMimeConverter.UnRegisterConverterClass(AClass : TStConverterClass);
begin
  if Assigned(Converters) then Converters.Remove(AClass);
end;

initialization
  InitializeCriticalSection(CvtLock);

finalization
  Converters.Free;
  DeleteCriticalSection(CvtLock);
end.
