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
{* SysTools: StBits.pas 4.04                             *}
{*********************************************************}
{* SysTools: Bit set class                               *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  CopyBits, OrBits, AndBits, and SubBits require that the parameter B have
  the same Max value as the current object, or an exception is generated.

  Use the inherited Count property to get the number of bits currently set.

  TStBits takes advantage of the suballocator whenever the bit set is
  small enough to allow it. Changing the Max property of the class
  allocates a new data area, copies the old data into it, and then
  deallocates the old data area.

  Supports up to 2**34 bits, if they will fit into memory.

  When Windows 3.1 is used, it requires enhanced mode operation.
}

unit StBits;

interface

uses
  Windows, Classes, SysUtils,
  
  StBase, StConst;

type
  TStBits = class;

  TBitIterateFunc =
    function(Container : TStBits; N : Integer; OtherData : Pointer) : Boolean;

  TStBits = class(TStContainer)
  {.Z+}
  protected
    {property instance variables}
    FMax : Integer;          {highest element number}

    {private instance variables}
    btBlockSize : Integer;   {bytes allocated to data area}
    btBits : PByte;          {pointer to data area}

    {undocumented protected methods}
    procedure btSetMax(Max : Integer);
    procedure btRecount;
    function btByte(I : Integer) : PByte;
    procedure SetByte(I: Integer; B: Byte);
    function  GetByte(I: Integer): Byte;
    function  GetAsString: AnsiString;
    procedure SetAsString(S: AnsiString);

  {.Z-}
  public
    constructor Create(Max : Integer); virtual;
      {-Initialize an empty bitset with highest element number Max}
    destructor Destroy; override;
      {-Free a bitset}

    procedure LoadFromStream(S : TStream); override;
      {-Read a bitset from a stream}
    procedure StoreToStream(S : TStream); override;
      {-Write a bitset to a stream}

    procedure Clear; override;
      {-Clear all bits in set but leave instance intact}

    procedure CopyBits(B : TStBits);
      {-Copy all bits in B to this bitset}
    procedure SetBits;
      {-Set all bits}
    procedure InvertBits;
      {-Invert all bits}
    procedure OrBits(B : TStBits);
      {-Or the specified bitset into this one (create the union)}
    procedure AndBits(B : TStBits);
      {-And the specified bitset with this one (create the intersection)}
    procedure SubBits(B : TStBits);
      {-Subtract the specified bitset from this one (create the difference)}

    procedure SetBit(N : Integer);
      {-Set bit N}
    procedure ClearBit(N : Integer);
      {-Clear bit N}
    procedure ToggleBit(N : Integer);
      {-Toggle bit N}
    procedure ControlBit(N : Integer; State : Boolean);
      {-Set or clear bit N according to State}
    procedure MoveBit(SrcBitset : TStBits; SrcN, DestN: Integer);
      {used to remap bit positions}
    procedure MapBits(SrcBitSet: TStBits; DestMap : Array of Integer);
      {copy bit set, and move bits into different positions in the new bitset} 
    function BitIsSet(N : Integer) : Boolean;
      {-Return True if bit N is set}

    function FirstSet : Integer;
      {-Return the index of the first set bit, -1 if none}
    function LastSet : Integer;
      {-Return the index of the last set bit, -1 if none}
    function FirstClear : Integer;
      {-Return the index of the first clear bit, -1 if none}
    function LastClear : Integer;
      {-Return the index of the last clear bit, -1 if none}
    function NextSet(N : Integer) : Integer;
      {-Return the index of the next set bit after N, -1 if none}
    function PrevSet(N : Integer) : Integer;
      {-Return the index of the previous set bit after N, -1 if none}
    function NextClear(N : Integer) : Integer;
      {-Return the index of the next set bit after N, -1 if none}
    function PrevClear(N : Integer) : Integer;
      {-Return the index of the previous set bit after N, -1 if none}

    function Iterate(Action : TBitIterateFunc;
                     UseSetBits, Up : Boolean;
                     OtherData : Pointer) : Integer;
      {-Call Action for all the matching bits, returning the last bit visited}
    function IterateFrom(Action : TBitIterateFunc;
                         UseSetBits, Up : Boolean;
                         OtherData : Pointer;
                         From : Integer) : Integer;
      {-Call Action for all the matching bits starting with bit From}

    property Max : Integer
      {-Read or write the maximum element count in the bitset}
      read FMax
      write btSetMax;

    property Items[N : Integer] : Boolean
      {-Read or write Nth bit in set}
      read BitIsSet
      write ControlBit;
      default;
    property Bytes[N : Integer] : byte {gives direct access to bytes}
      {-Read or write Nth byte in set}
      read GetByte
      write SetByte;
    property AsString : AnsiString {gives access as longstring}
      read GetAsString
      write SetAsString;
  end;


{======================================================================}


implementation

{$IFDEF ThreadSafe}
var
  ClassCritSect : TRTLCriticalSection;
{$ENDIF}

procedure EnterClassCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(ClassCritSect);
{$ENDIF}
end;

procedure LeaveClassCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(ClassCritSect);
{$ENDIF}
end;

function MinLong(A, B : Integer) : Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MaxLong(A, B : Integer) : Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

{----------------------------------------------------------------------}

procedure TStBits.AndBits(B : TStBits);
var
  I : Integer;
  P : PByte;
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  B.EnterCS;
  try
{$ENDIF}
    if (not Assigned(B)) or (B.Max <> FMax) then
      RaiseContainerError(stscBadType);
    for I := 0 to btBlockSize-1 do begin
      P := btByte(I);
      P^ := P^ and B.btByte(I)^;
    end;
    btRecount;
{$IFDEF ThreadSafe}
  finally
    B.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;
procedure TStBits.MoveBit(SrcBitset : TStBits; SrcN, DestN: Integer);
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  SrcBitSet.EnterCS;
  try
{$ENDIF}
    if (not Assigned(SrcBitSet)) or (SrcN > FMax) or (SrcBitSet.Max < SrcN) then
      RaiseContainerError(stscBadType);
    if SrcBitset.BitIsSet(SrcN)
      then SetBit(DestN)
      else ClearBit(DestN);
    //btRecount;
{$IFDEF ThreadSafe}
  finally
    SrcBitSet.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

function TStBits.BitIsSet(N : Integer) : Boolean;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (N < 0) or (N > FMax) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    Result := (btByte(N shr 3)^ and (1 shl (Byte(N) and 7)) <> 0);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStBits.btByte(I : Integer) : PByte;
begin
  Result := PByte(PAnsiChar(btBits)+I);
end;
function TStBits.GetByte(I : Integer) : Byte;
  var P : PByte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (I < 0) or (I >= btBlockSize) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    P := btByte(I);
    Result := P^;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStBits.GetAsString : ansistring;
{very inefficient!}
  var P : PByte; i:Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result:='';
    for i:=0 to btBlockSize-1 do begin
      P := btByte(I);
      Result := Result+ ansichar(P^);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.SetAsString(S:AnsiString);
{very inefficient!}
  var P : PByte; I: Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (length(S) <> btBlockSize) then
      RaiseContainerError(stscBadType);
{$ENDIF}
    for i:=1 to length(S) do begin
      P:=btByte(i-1);
      P^:=byte(S[i]);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.SetByte(I : Integer; B:byte);
  var P : PByte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (I < 0) or (I >= btBlockSize) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    P := btByte(I);
    P^ := B;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.MapBits(SrcBitset: TStBits; DestMap : Array of Integer);
  var N: Integer;
begin
  if (not Assigned(SrcBitset)) or (SrcBitset.Max <> FMax) then
    RaiseContainerError(stscBadType);

  for N:=0 to High(DestMap) do begin
    MoveBit(SrcBitset, N, DestMap[N]);
  end;
end;

procedure TStBits.btRecount;
const
  {number of bits set in every possible byte}
  BitCount : array[Byte] of Byte = (
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);
var
  N : Integer;
  P : PByte;
  B : Byte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Clear unused bits in last byte}
    B := Byte(FMax) and 7;
    if B < 7 then begin
      P := btByte(btBlockSize-1);
      P^ := P^ and ((1 shl (B+1))-1);
    end;

    {Add up the bits in each byte}
    FCount := 0;
    for N := 0 to btBlockSize-1 do
      inc(FCount, BitCount[btByte(N)^]);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.btSetMax(Max : Integer);
var
  BlockSize, OldBlockSize, OldMax : Integer;
  OldBits : PByte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Validate new size}
    if Max < 0 then
      RaiseContainerError(stscBadSize);
    BlockSize := (Max+8) div 8;

    {Save old size settings}
    OldBlockSize := btBlockSize;
    OldMax := FMax;

    {Assign new size settings}
    FMax := Max;
    btBlockSize := BlockSize;

    if BlockSize <> OldBlockSize then begin
      {Get new data area and transfer data}
      OldBits := btBits;
      try
        GetMem(Pointer(btBits), btBlockSize);
      except
        btBlockSize := OldBlockSize;
        btBits := OldBits;
        FMax := OldMax;
        raise;
      end;

      if OldBlockSize < btBlockSize then begin
        FillChar(btByte(OldBlockSize)^, btBlockSize-OldBlockSize, 0);
        BlockSize := OldBlockSize;
      end else
        BlockSize := btBlockSize;
      Move(OldBits^, btBits^, BlockSize);

      {Free old data area}
      FreeMem(Pointer(OldBits), OldBlockSize);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    FillChar(btBits^, btBlockSize, 0);
    FCount := 0;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.ClearBit(N : Integer);
var
  P : PByte;
  M : Byte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (N < 0) or (N > FMax) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    P := btByte(N shr 3);
    M := 1 shl (Byte(N) and 7);
    if (P^ and M) <> 0 then begin
      P^ := P^ and not M;
      dec(FCount);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.ControlBit(N : Integer; State : Boolean);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if State then
      SetBit(N)
    else
      ClearBit(N);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.CopyBits(B : TStBits);
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  B.EnterCS;
  try
{$ENDIF}
    if (not Assigned(B)) or (B.Max <> FMax) then
      RaiseContainerError(stscBadType);

    Move(B.btBits^, btBits^, btBlockSize);
    FCount := B.FCount;
{$IFDEF ThreadSafe}
  finally
    B.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

constructor TStBits.Create(Max : Integer);
begin
  {Validate size}
  if Max < 0 then
    RaiseContainerError(stscBadSize);

  CreateContainer(TStNode, 0);

  FMax := Max;
  btBlockSize := (Max+8) div 8;
  GetMem(Pointer(btBits), btBlockSize);
  Clear;
end;

destructor TStBits.Destroy;
begin
  if Assigned(btBits) then
    FreeMem(Pointer(btBits), btBlockSize);

  {Prevent calling Clear}
  IncNodeProtection;
  inherited Destroy;
end;

function StopImmediately(Container : TStBits; N : Integer;
                         OtherData : Pointer) : Boolean; far;
  {-Iterator function used to stop after first found bit}
begin
  Result := False;
end;

function TStBits.FirstClear : Integer;
begin
  Result := IterateFrom(StopImmediately, False, True, nil, 0);
end;

function TStBits.FirstSet : Integer;
begin
  Result := IterateFrom(StopImmediately, True, True, nil, 0);
end;

procedure TStBits.InvertBits;
var
  I : Integer;
  P : PByte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    for I := 0 to btBlockSize-1 do begin
      P := btByte(I);
      P^ := not P^;
    end;
    FCount := FMax-FCount+1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStBits.Iterate(Action : TBitIterateFunc;
                         UseSetBits, Up : Boolean;
                         OtherData : Pointer) : Integer;
begin
  if Up then
    Result := IterateFrom(Action, UseSetBits, True, OtherData, 0)
  else
    Result := IterateFrom(Action, UseSetBits, False, OtherData, FMax);
end;

function TStBits.IterateFrom(Action : TBitIterateFunc;
                             UseSetBits, Up : Boolean;
                             OtherData : Pointer;
                             From : Integer) : Integer;
var
  I, N, F : Integer;
  O : ShortInt;
  B, TB : Byte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if UseSetBits then
      TB := 0
    else
      TB := $FF;

    if Up then begin
      {do the first possibly-partial byte}
      N := MaxLong(From, 0);
      F := MinLong(btBlockSize-1, N shr 3);
      O := ShortInt(N) and 7;
      B := btByte(F)^;

      while (N <= FMax) and (O <= ShortInt(7)) do begin
        if not (UseSetBits xor ((B and (1 shl O)) <> 0)) then
          if not Action(Self, N, OtherData) then begin
            Result := N;
            Exit;
          end;
        inc(O);
        inc(N);
      end;

      {do the rest of the bytes}
      for I := F+1 to btBlockSize-1 do begin
        B := btByte(I)^;
        if B <> TB then begin
          {byte has bits of interest}
          O := 0;
          while (N <= FMax) and (O < ShortInt(8)) do begin
            if not (UseSetBits xor ((B and (1 shl O)) <> 0)) then
              if not Action(Self, N, OtherData) then begin
                Result := N;
                Exit;
              end;
            inc(O);
            inc(N);
          end;
        end else
          inc(N, 8);
      end;

    end else begin
      {do the last possibly-partial byte}
      N := MinLong(From, FMax);
      F := MaxLong(N, 0) shr 3;
      O := ShortInt(N) and 7;
      B := btByte(F)^;

      while (N >= 0) and (O >= ShortInt(0)) do begin
        if not (UseSetBits xor ((B and (1 shl O)) <> 0)) then
          if not Action(Self, N, OtherData) then begin
            Result := N;
            Exit;
          end;
        dec(O);
        dec(N);
      end;

      {do the rest of the bytes}
      for I := F-1 downto 0 do begin
        B := btByte(I)^;
        if B <> TB then begin
          {byte has bits of interest}
          O := 7;
          while (N >= 0) and (O >= ShortInt(0)) do begin
            if not (UseSetBits xor ((B and (1 shl O)) <> 0)) then
              if not Action(Self, N, OtherData) then begin
                Result := N;
                Exit;
              end;
            dec(O);
            dec(N);
          end;
        end else
          dec(N, 8);
      end;
    end;

    {Iterated all bits}
    Result := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStBits.LastClear : Integer;
begin
  Result := IterateFrom(StopImmediately, False, False, nil, FMax);
end;

function TStBits.LastSet : Integer;
begin
  Result := IterateFrom(StopImmediately, True, False, nil, FMax);
end;

function TStBits.NextClear(N : Integer) : Integer;
begin
  Result := IterateFrom(StopImmediately, False, True, nil, N+1);
end;

function TStBits.NextSet(N : Integer) : Integer;
begin
  Result := IterateFrom(StopImmediately, True, True, nil, N+1);
end;

procedure TStBits.OrBits(B : TStBits);
var
  I : Integer;
  P : PByte;
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  B.EnterCS;
  try
{$ENDIF}
    if (not Assigned(B)) or (B.Max <> FMax) then
      RaiseContainerError(stscBadType);
    for I := 0 to btBlockSize-1 do begin
      P := btByte(I);
      P^ := P^ or B.btByte(I)^;
    end;
    btRecount;
{$IFDEF ThreadSafe}
  finally
    B.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

function TStBits.PrevClear(N : Integer) : Integer;
begin
  Result := IterateFrom(StopImmediately, False, False, nil, N-1);
end;

function TStBits.PrevSet(N : Integer) : Integer;
begin
  Result := IterateFrom(StopImmediately, True, False, nil, N-1);
end;

procedure TStBits.SetBit(N : Integer);
var
  P : PByte;
  M : Byte;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (N < 0) or (N > FMax) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    P := btByte(N shr 3);
    M := 1 shl (Byte(N) and 7);
    if (P^ and M) = 0 then begin
      P^ := P^ or M;
      inc(FCount);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.SetBits;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    FillChar(btBits^, btBlockSize, $FF);
    FCount := FMax+1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.SubBits(B : TStBits);
var
  I : Integer;
  P : PByte;
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  B.EnterCS;
  try
{$ENDIF}
    if (not Assigned(B)) or (B.Max <> FMax) then
      RaiseContainerError(stscBadType);
    for I := 0 to btBlockSize-1 do begin
      P := btByte(I);
      P^ := P^ and not B.btByte(I)^;
    end;
    btRecount;
{$IFDEF ThreadSafe}
  finally
    B.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

procedure TStBits.ToggleBit(N : Integer);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if BitIsSet(N) then
      ClearBit(N)
    else
      SetBit(N);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.LoadFromStream(S : TStream);
var
  Reader : TReader;
  StreamedClass : TPersistentClass;
  StreamedClassName : String;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Clear;
    Reader := TReader.Create(S, 1024);
    try
      with Reader do
        begin
          StreamedClassName := ReadString;
          StreamedClass := GetClass(StreamedClassName);
          if (StreamedClass = nil) then
            RaiseContainerErrorFmt(stscUnknownClass, [StreamedClassName]);
          if (not IsOrInheritsFrom(StreamedClass, Self.ClassType)) or
              (not IsOrInheritsFrom(TStBits, StreamedClass)) then
            RaiseContainerError(stscWrongClass);
          Max := ReadInteger;
          FCount := ReadInteger;
          Read(btBits^, btBlockSize);
        end;
    finally
      Reader.Free;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStBits.StoreToStream(S : TStream);
var
  Writer : TWriter;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Writer := TWriter.Create(S, 1024);
    try
      with Writer do
        begin
          WriteString(Self.ClassName);
          WriteInteger(Max);
          WriteInteger(Count);
          Write(btBits^, btBlockSize);
        end;
    finally
      Writer.Free;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{$IFDEF ThreadSafe}
initialization
  Windows.InitializeCriticalSection(ClassCritSect);
finalization
  Windows.DeleteCriticalSection(ClassCritSect);
{$ENDIF}
end.
