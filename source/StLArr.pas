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
{* SysTools: StLArr.pas 4.04                             *}
{*********************************************************}
{* SysTools: Large array classes                         *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  - requires a 386 or better processor, even for 16-bit Delphi apps

  - uses the value in the SYSTEM variable HeapAllocFlags when allocating
    memory for the array.

  - changing the size of an array allocates a new array, transfers the
    old data, and then frees the original array.

  - arrays are always indexed from 0 to Count-1.

  - after creating a descendant that knows the type of each element, an
    indexed default property can be used to access array elements in a
    convenient fashion, e.g., A[100] := 6.0;

  - the Get and Put methods don't perform range checking.

  - for 32-bit matrix, Rows*Cols cannot exceed 2**32.
}

unit StLArr;

interface

uses
  Windows,
  Classes, StConst, StBase;

type
  TStLArray = class(TStContainer)
  {.Z+}
  protected
    {property instance variables}
    FElSize     : Integer;     {Size of each array element}            
    FElStorable : boolean;     {True if elements can be stored directly}

    {private instance variables}
    laData     : Pointer;    {Pointer to data block}

    {undocumented protected methods}
    procedure ForEachUntypedVar(Action : TIterateUntypedFunc;
                                OtherData : pointer);
      override;
    procedure GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
      override;
    procedure SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
      override;
    function StoresUntypedVars : boolean;
      override;
    procedure laSetCount(Elements : Integer);

  {.Z-}
  public
    constructor Create(Elements : Integer; ElementSize : Cardinal);
      {-Initialize a large 1D array}
    destructor Destroy; override;
      {-Free a large 1D array}

    procedure LoadFromStream(S : TStream); override;
      {-Load a collection's data from a stream}
    procedure StoreToStream(S : TStream); override;
      {-Write a collection and its data to a stream}

    procedure Assign(Source: TPersistent); override;
      {-Assign another container's contents to this one}
    procedure Clear; override;
      {-Fill the array with zeros}

    procedure Fill(const Value);
      {-Fill array with specified value}

    procedure Put(El : Integer; const Value);
      {-Set an element}
    procedure Get(El : Integer; var Value);
      {-Return an element}

    procedure Exchange(El1, El2 : Integer);
      {-Exchange the specified elements}
    procedure Sort(Compare : TUntypedCompareFunc);
      {-Sort the array using the given comparison function}

    property Count : Integer
      {-Read or write the number of elements in the array}
      read FCount
      write laSetCount;

    property ElementSize : Integer                                     
      read FElSize;

    property ElementsStorable : boolean
      {-True if elements can be written directly to (or read from) disk}
      read FElStorable write FElStorable;
  end;

type
  TStLMatrix = class(TStContainer)
  {.Z+}
  protected
    {property instance variables}
    FElSize   : Integer;    {Size of each array element}               
    FCols     : Cardinal;   {Number of columns}
    FRows     : Cardinal;   {Number of rows}
    FElStorable : boolean;     {True if elements can be stored directly}

    {private instance variables}
    lmData     : Pointer;    {Pointer to data block}
    lmRowSize  : Integer;    {Number of bytes in a row}

    {undocumented protected methods}
    procedure ForEachUntypedVar(Action : TIterateUntypedFunc; OtherData : pointer);
      override;
    procedure GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
      override;
    procedure SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
      override;
    function StoresUntypedVars : boolean;
      override;
    procedure lmSetRows(Rows : Cardinal);
    procedure lmSetCols(Cols : Cardinal);

  {.Z-}
  public
    constructor Create(Rows, Cols, ElementSize : Cardinal);
      {-Initialize a large 2D matrix}
    destructor Destroy; override;
      {-Free a large 2D matrix}

    procedure LoadFromStream(S : TStream); override;
      {-Load a collection's data from a stream}
    procedure StoreToStream(S : TStream); override;
      {-Write a collection and its data to a stream}

    procedure Assign(Source: TPersistent); override;
      {-Assign another container's contents to this one}
    procedure Clear; override;
      {-Fill the matrix with zeros}

    procedure Fill(const Value);
      {-Fill matrix with specified value}

    procedure Put(Row, Col : Cardinal; const Value);
      {-Set an element}
    procedure Get(Row, Col : Cardinal; var Value);
      {-Return an element}

    procedure PutRow(Row : Cardinal; const RowValue);
      {-Set an entire row}
    procedure GetRow(Row : Cardinal; var RowValue);
      {-Return an entire row}

    procedure ExchangeRows(Row1, Row2 : Cardinal);
      {-Exchange the specified rows}
    procedure SortRows(KeyCol : Cardinal; Compare : TUntypedCompareFunc);
      {-Sort the array rows using the given comparison function and
        the elements in the given column}

    property Rows : Cardinal
      {-Read or write the number of rows in the array}
      read FRows
      write lmSetRows;
    property Cols : Cardinal
      {-Read or write the number of columns in the array}
      read FCols
      write lmSetCols;
    property ElementSize : Integer                                     
      read FElSize;
    property ElementsStorable : boolean
      {-True if elements can be written directly to (or read from) disk}
      read FElStorable write FElStorable;
  end;

{======================================================================}

implementation

function AssignArrayData(Container : TStContainer;
                     var Data;
                         OtherData : Pointer) : Boolean; far;
  var
    OurArray : TStLArray absolute OtherData;
    RD : TAssignRowData absolute Data;
  begin
    OurArray.Put(RD.RowNum, RD.Data);
    Result := true;
  end;

function AssignMatrixData(Container : TStContainer;
                      var Data;
                          OtherData : Pointer) : Boolean; far;
  var
    OurMatrix : TStLMatrix absolute OtherData;
    RD : TAssignRowData absolute Data;
  begin
    OurMatrix.PutRow(RD.RowNum, RD.Data);
    Result := true;
  end;

procedure TStLArray.Assign(Source: TPersistent);
  begin
    {$IFDEF ThreadSafe}
    EnterCS;
    try
    {$ENDIF}
      {The only containers that we allow to be assigned to a large array
       are:
         - another SysTools large array (TStLArray)
         - a SysTools large matrix (TStLMatrix) with one column
         - a SysTools virtual matrix (TStVMatrix) with one column}
      if not AssignUntypedVars(Source, AssignArrayData) then
        inherited Assign(Source);
    {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;{try..finally}
    {$ENDIF}
  end;

procedure TStLArray.Clear;
var
  C : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    C := FCount;
    FillChar(laData^, C*FElSize, 0);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLArray.ForEachUntypedVar(Action : TIterateUntypedFunc;
                                      OtherData : pointer);
  var
    FullRow : ^TAssignRowData;
    i       : Cardinal;
  begin
  {$IFDEF ThreadSafe}
    EnterCS;
    try
  {$ENDIF}
      GetMem(FullRow, sizeof(Cardinal) + ElementSize);
      try
        for i := 0 to pred(Count) do
          begin
            FullRow^.RowNum := i;
            Get(i, FullRow^.Data);
            Action(Self, FullRow^, OtherData);
          end;
      finally
        FreeMem(FullRow, sizeof(Cardinal) + ElementSize);
      end;
  {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;
  {$ENDIF}
  end;

procedure TStLArray.GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
begin
  RowCount := Count;
  ColCount := 1;
  ElSize := ElementSize;
end;

procedure TStLArray.SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
begin
  if (ColCount <> 1) then
    RaiseContainerError(stscTooManyCols);
  if (Integer(RowCount) <> Count) or
     (Integer(ElSize) <> ElementSize) then begin
    HugeFreeMem(laData, FCount*FElSize);
    FCount := RowCount;
    FElSize := ElSize;
    HugeGetMem(laData, RowCount*ElSize);
    Clear;
  end;
end;

function TStLArray.StoresUntypedVars : boolean;
begin
  Result := True;
end;

constructor TStLArray.Create(Elements : Integer; ElementSize : Cardinal);
begin
  if (Elements <= 0) or (ElementSize = 0) or
  ProductOverflow(Elements, ElementSize) then
    RaiseContainerError(stscBadSize);

  CreateContainer(TStNode, 0);

  FCount := Elements;
  FElSize := ElementSize;

  HugeGetMem(laData, Elements*Integer(ElementSize));
  Clear;
end;

destructor TStLArray.Destroy;
begin
  HugeFreeMem(laData, FCount*FElSize);
  IncNodeProtection;
  inherited Destroy;
end;

procedure TStLArray.Exchange(El1, El2 : Integer);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (El1 < 0) or (El1 >= Count) or (El2 < 0) or (El2 >= Count) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    asm
      mov eax,Self
      push ebx
      push esi
      push edi

      mov esi,El1
      mov edi,El2
      mov ecx,TStLArray([eax]).FElSize
      mov edx,TStLArray([eax]).laData
      db $0F,$AF,$F1            {imul esi,ecx, compiler bug workaround}
      add esi,edx
      db $0F,$AF,$F9            {imul edi,ecx, compiler bug workaround}
      add edi,edx
      mov edx,ecx
      shr ecx,2
      jz @2

  @1: mov eax,[esi]             {avoid xchg instruction, which is slow}
      mov ebx,[edi]
      mov [esi],ebx
      mov [edi],eax
      add esi,4
      add edi,4
      dec ecx
      jnz @1

  @2: mov ecx,edx
      and ecx,3
      jz @4

  @3: mov al,[esi]              {avoid xchg instruction, which is slow}
      mov bl,[edi]
      mov [esi],bl
      mov [edi],al
      inc esi
      inc edi
      dec ecx
      jnz @3

  @4: pop edi
      pop esi
      pop ebx
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


procedure TStLArray.Fill(const Value);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    HugeFillStruc(laData^, FCount, Value, FElSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLArray.Get(El : Integer; var Value);
(* model for code below
begin
  move((PChar(laData)+El*FElSize)^, Value, FElSize);
end;
*)
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (El < 0) or (El >= Count) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    asm
      mov eax,Self
      push esi
      push edi
      mov edi,Value
      mov ecx,TStLArray([eax]).FElSize
      mov esi,El
      db $0F,$AF,$F1     {imul esi,ecx, compiler bug workaround}
      add esi,TStLArray([eax]).laData
      mov eax,ecx
      shr ecx,2
      rep movsd
      mov ecx,eax
      and ecx,3
      rep movsb
      pop edi
      pop esi
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


procedure TStLArray.laSetCount(Elements : Integer);
var
  CurSize, NewSize : Integer;
  CurFData : Pointer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {validate new size}
    if (Elements <= 0) or ProductOverflow(Elements, FElSize) then
      RaiseContainerError(stscBadSize);

    NewSize := Elements*FElSize;                            
    CurSize := FCount*FElSize;
    CurFData := laData;

    {allocate data block of new size}
    HugeGetMem(laData, NewSize);

    FCount := Elements;

    {fill extra area with zeros and copy old data}
    if NewSize > CurSize then begin
      Clear;
      NewSize := CurSize;
    end;
    HugeMove(CurFData^, laData^, NewSize);

    {free original data area}
    HugeFreeMem(CurFData, CurSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLArray.Put(El : Integer; const Value);
(* model for assembly language below
begin
  move(Value, (PChar(laData)+Row*FElSize)^, FElSize);
end;
*)
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (El < 0) or (El >= Count) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    asm
      mov eax,Self
      push esi
      push edi
      mov esi,Value
      mov ecx,TStLArray([eax]).FElSize
      mov edi,El
      db $0F,$AF,$F9            {imul edi,ecx, compiler bug workaround}
      add edi,TStLArray([eax]).laData
      mov eax,ecx
      shr ecx,2
      rep movsd
      mov ecx,eax
      and ecx,3
      rep movsb
      pop edi
      pop esi
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLArray.Sort(Compare : TUntypedCompareFunc);
const
  StackSize = 32;
type
  Stack = array[0..StackSize-1] of Integer;
var
  L : Integer;
  R : Integer;
  PL : Integer;
  PR : Integer;
  CurEl : Pointer;
  PivEl : Pointer;
  StackP : Integer;
  LStack : Stack;
  RStack : Stack;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Need at least 2 elements to sort}
    if FCount <= 1 then
      Exit;

    GetMem(CurEl, FElSize);
    try
      GetMem(PivEl, FElSize);
      try
        {Initialize the stacks}
        StackP := 0;
        LStack[0] := 0;
        RStack[0] := FCount-1;

        {Repeatedly take top partition from stack}
        repeat

          {Pop the stack}
          L := LStack[StackP];
          R := RStack[StackP];
          Dec(StackP);

          {Sort current partition}
          repeat

            {Load the pivot element}
            Get((L+R) div 2, PivEl^);
            PL := L;
            PR := R;

            {Swap items in sort order around the pivot index}
            repeat
              Get(PL, CurEl^);
              while Compare(CurEl^, PivEl^) < 0 do begin
                Inc(PL);
                Get(PL, CurEl^);
              end;
              Get(PR, CurEl^);
              while Compare(PivEl^, CurEl^) < 0 do begin
                Dec(PR);
                Get(PR, CurEl^);
              end;
              if PL <= PR then begin
                if PL <> PR then
                  {Swap the two elements}
                  Exchange(PL, PR);
                Inc(PL); {assume we'll never sort 2 billion elements}
                Dec(PR);
              end;
            until PL > PR;

            {Decide which partition to sort next}
            if (PR-L) < (R-PL) then begin
              {Right partition is bigger}
              if PL < R then begin
                {Stack the request for sorting right partition}
                Inc(StackP);
                LStack[StackP] := PL;
                RStack[StackP] := R;
              end;
              {Continue sorting left partition}
              R := PR;
            end else begin
              {Left partition is bigger}
              if L < PR then begin
                {Stack the request for sorting left partition}
                Inc(StackP);
                LStack[StackP] := L;
                RStack[StackP] := PR;
              end;
              {Continue sorting right partition}
              L := PL;
            end;

          until L >= R;
        until StackP < 0;
      finally
        FreeMem(PivEl, FElSize);
      end;
    finally
      FreeMem(CurEl, FElSize);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLArray.LoadFromStream(S : TStream);
var
  Data         : pointer;
  Reader       : TReader;
  NumElements  : Integer;
  ElementSize  : Integer;
  i            : Integer;
  TotSize      : Integer;
  StreamedClass : TPersistentClass;
  StreamedClassName : string;
  Value        : TValueType;
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
              (not IsOrInheritsFrom(TStLArray, StreamedClass)) then
            RaiseContainerError(stscWrongClass);
          NumElements := ReadInteger;
          ElementSize := ReadInteger;
          if (NumElements <> FCount) or (ElementSize <> FElSize) then
            begin
              HugeFreeMem(laData, FCount*FElSize);
              FCount := NumElements;
              FElSize := ElementSize;
              HugeGetMem(laData, NumElements*ElementSize);
              Clear;
            end;
          ElementsStorable := ReadBoolean;
          if ElementsStorable then
            begin
              Read(Value, sizeof(Value)); {s/b vaBinary}
              Read(TotSize, sizeof(Integer));
              GetMem(Data, FElSize);
              try
                for i := 0 to pred(FCount) do
                  begin
                    Read(Data^, FElSize);
                    Put(i, Data^);
                  end;
              finally
                FreeMem(Data, FElSize);
              end;
            end
          else
            begin
              ReadListBegin;
              for i := 0 to pred(FCount) do begin
                Data := DoLoadData(Reader);
                Put(i, Data^);
              end;
              ReadListEnd;
            end;
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

procedure TStLArray.StoreToStream(S : TStream);
var
  Writer : TWriter;
  i      : integer;
  Data   : pointer;
  TotSize: Integer;
  Value  : TValueType;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Writer := TWriter.Create(S, 1024);
    try
      GetMem(Data, FElSize);
      try
        with Writer do begin
          WriteString(Self.ClassName);
          WriteInteger(FCount);
          WriteInteger(FElSize);
          WriteBoolean(FElStorable);
          if ElementsStorable then begin
            Value := vaBinary;
            Write(Value, sizeof(Value));
            TotSize := FCount * FElSize;
            Write(TotSize, sizeof(Integer));
            for i := 0 to pred(FCount) do begin
              Get(i, Data^);
              Write(Data^, FElSize);
            end;
          end else begin
            WriteListBegin;
            for i := 0 to pred(FCount) do begin
              Get(i, Data^);
              DoStoreData(Writer, Data);
            end;
            WriteListEnd;
          end;
        end;
      finally
        FreeMem(Data, FElSize);
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

{----------------------------------------------------------------------}

procedure TStLMatrix.Assign(Source: TPersistent);
  begin
    {$IFDEF ThreadSafe}
    EnterCS;
    try
    {$ENDIF}
      {The only containers that we allow to be assigned to a large matrix
       are:
         - a SysTools large array (TStLArray)
         - another SysTools large matrix (TStLMatrix)
         - a SysTools virtual matrix (TStVMatrix)}
      if not AssignUntypedVars(Source, AssignMatrixData) then
        inherited Assign(Source);
    {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;{try..finally}
    {$ENDIF}
  end;

procedure TStLMatrix.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    FillChar(lmData^, FCount*FElSize, 0);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.ForEachUntypedVar(Action : TIterateUntypedFunc;
                                          OtherData : pointer);
  var
    FullRow : ^TAssignRowData;
    i       : Cardinal;
  begin
  {$IFDEF ThreadSafe}
    EnterCS;
    try
  {$ENDIF}
      GetMem(FullRow, sizeof(Cardinal) + lmRowSize);
      try
        for i := 0 to pred(Rows) do
          begin
            FullRow^.RowNum := i;
            GetRow(i, FullRow^.Data);
            Action(Self, FullRow^, OtherData);
          end;
      finally
        FreeMem(FullRow, sizeof(Cardinal) + lmRowSize);
      end;
  {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;
  {$ENDIF}
  end;

procedure TStLMatrix.GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
begin
  RowCount := Rows;
  ColCount := Cols;
  ElSize := ElementSize;
end;

procedure TStLMatrix.SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
begin
  if (RowCount <> Rows) or (ColCount <> Cols) or
     (Integer(ElSize) <> ElementSize) then
    begin
      HugeFreeMem(lmData, FCount*FElSize);
      FElSize := ElSize;
      FRows := RowCount;
      FCols := ColCount;
      FCount := RowCount*ColCount;
      lmRowSize := ColCount*ElSize;
      HugeGetMem(lmData, FCount*Integer(ElSize));
      Clear;
    end;
end;

function TStLMatrix.StoresUntypedVars : boolean;
begin
  Result := true;
end;

constructor TStLMatrix.Create(Rows, Cols, ElementSize : Cardinal);
begin
  CreateContainer(TStNode, 0);

  FElSize := ElementSize;
  FRows := Rows;
  FCols := Cols;
  FCount := Integer(Rows)*Integer(Cols);
  lmRowSize := Integer(Cols)*Integer(ElementSize);

  if (Rows = 0) or (Cols = 0) or (ElementSize = 0) or
  ProductOverflow(FCount, ElementSize) then
    RaiseContainerError(stscBadSize);

  HugeGetMem(lmData, FCount*Integer(ElementSize));
  Clear;
end;

destructor TStLMatrix.Destroy;
begin
  HugeFreeMem(lmData, FCount*FElSize);
  IncNodeProtection;
  inherited Destroy;
end;

procedure TStLMatrix.ExchangeRows(Row1, Row2 : Cardinal);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (Row1 >= Rows) or (Row2 >= Rows) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    asm
      mov eax,Self
      push ebx
      push esi
      push edi

      mov esi,Row1
      mov edi,Row2
      mov ecx,TStLMatrix([eax]).lmRowSize
      mov edx,TStLMatrix([eax]).lmData
      db $0F,$AF,$F1            {imul esi,ecx, compiler bug workaround}
      add esi,edx
      db $0F,$AF,$F9            {imul edi,ecx, compiler bug workaround}
      add edi,edx
      mov edx,ecx
      shr ecx,2
      jz @2

  @1: mov eax,[esi]             {avoid xchg instruction, which is slow}
      mov ebx,[edi]
      mov [esi],ebx
      mov [edi],eax
      add esi,4
      add edi,4
      dec ecx
      jnz @1

  @2: mov ecx,edx
      and ecx,3
      jz @4

  @3: mov al,[esi]              {avoid xchg instruction, which is slow}
      mov bl,[edi]
      mov [esi],bl
      mov [edi],al
      inc esi
      inc edi
      dec ecx
      jnz @3

  @4: pop edi
      pop esi
      pop ebx
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.Fill(const Value);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    HugeFillStruc(lmData^, FCount, Value, FElSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.Get(Row, Col : Cardinal; var Value);
(* model for assembly language below
begin
  move((PChar(lmData)+(Row*FCols+Col)*FElSize)^, Value, FElSize);
end;
*)
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (Row >= Rows) or (Col >= Cols) then
      RaiseContainerError(stscBadIndex);
    asm
      mov eax,Self
      push esi
      push edi
      mov edi,Value
      mov esi,Row
      imul esi,TStLMatrix([eax]).FCols
      add esi,Col
      mov ecx,TStLMatrix([eax]).FElSize
      db $0F,$AF,$F1     {imul esi,ecx, compiler bug workaround}
      add esi,TStLMatrix([eax]).lmData
      mov eax,ecx
      shr ecx,2
      rep movsd
      mov ecx,eax
      and ecx,3
      rep movsb
      pop edi
      pop esi
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.GetRow(Row : Cardinal; var RowValue);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if Row >= Rows then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    move((PAnsiChar(lmData)+(Integer(Row)*lmRowSize))^, RowValue, lmRowSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.lmSetCols(Cols : Cardinal);
var
  CurSize, NewSize, CurRowSize, NewRowSize, BufSize : Integer;
  R, CurCols : Cardinal;
  CurFData, NewFData, RowData : Pointer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Cols = FCols then
      Exit;

    {validate new size}
    if (Cols = 0) or
    ProductOverflow(Cols, FRows) or
    ProductOverflow(Integer(Cols)*Integer(FRows), FElSize) then
      RaiseContainerError(stscBadSize);

    {compute and save various sizes}
    CurSize := FCount*FElSize;
    NewSize := Integer(Cols)*Integer(FRows)*FElSize;
    CurRowSize := lmRowSize;
    NewRowSize := Integer(Cols)*FElSize;
    CurCols := FCols;
    CurFData := lmData;

    {allocate data block of new size}
    HugeGetMem(NewFData, NewSize);

    {allocate a buffer to transfer row data}
    if NewRowSize > CurRowSize then
      BufSize := NewRowSize
    else
      BufSize := CurRowSize;
    try
      HugeGetMem(RowData, BufSize);
    except
      HugeFreeMem(NewFData, NewSize);
    end;

    {transfer rows from old array to new}
    if Cols > CurCols then
      FillChar(RowData^, BufSize, 0);
    for R := 0 to FRows-1 do begin
      FCols := CurCols;
      lmRowSize := CurRowSize;
      lmData := CurFData;
      GetRow(R, RowData^);
      FCols := Cols;
      lmRowSize := NewRowSize;
      lmData := NewFData;
      PutRow(R, RowData^);
    end;
    HugeFreeMem(RowData, BufSize);

    FCount := Integer(Cols)*Integer(FRows);

    {free original data area}
    HugeFreeMem(CurFData, CurSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.lmSetRows(Rows : Cardinal);
var
  CurSize, NewSize : Integer;
  CurFData : Pointer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Rows = FRows then
      Exit;

    {validate new size}
    if (Rows = 0) or
    ProductOverflow(Rows, FCols) or
    ProductOverflow(Integer(Rows)*Integer(FCols), FElSize) then
      RaiseContainerError(stscBadSize);

    CurSize := FCount*FElSize;
    NewSize := Integer(Rows)*Integer(FCols)*FElSize;
    CurFData := lmData;

    {allocate data block of new size}
    HugeGetMem(lmData, NewSize);

    FCount := Integer(Rows)*Integer(FCols);
    FRows := Rows;

    {fill extra area with zeros and copy old data}
    if NewSize > CurSize then begin
      Clear;
      NewSize := CurSize;
    end;
    HugeMove(CurFData^, lmData^, NewSize);

    {free original data area}
    HugeFreeMem(CurFData, CurSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.Put(Row, Col : Cardinal; const Value);
(* model for assembly language below
begin
  move(Value, (PChar(lmData)+(Row*FCols+Col)*FElSize)^, FElSize);
end;
*)
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if (Row >= Rows) or (Col >= Cols) then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    asm
      mov eax,Self
      push esi
      push edi
      mov esi,Value
      mov edi,Row
      imul edi, TStLMatrix([eax]).FCols
      add edi,Col
      mov ecx,TStLMatrix([eax]).FElSize
      db $0F,$AF,$F9     {imul edi,ecx, compiler bug workaround}
      add edi,TStLMatrix([eax]).lmData
      mov eax,ecx
      shr ecx,2
      rep movsd
      mov ecx,eax
      and ecx,3
      rep movsb
      pop edi
      pop esi
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.PutRow(Row : Cardinal; const RowValue);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
{$IFOPT R+}
    if Row >= Rows then
      RaiseContainerError(stscBadIndex);
{$ENDIF}
    move(RowValue, (PAnsiChar(lmData)+(Integer(Row)*lmRowSize))^, lmRowSize);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.SortRows(KeyCol : Cardinal; Compare : TUntypedCompareFunc);
const
  StackSize = 32;
type
  Stack = array[0..StackSize-1] of Integer;
var
  L : Integer;
  R : Integer;
  PL : Integer;
  PR : Integer;
  CurEl : Pointer;
  PivEl : Pointer;
  StackP : Integer;
  LStack : Stack;
  RStack : Stack;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if KeyCol >= FCols then
      RaiseContainerError(stscBadIndex);

    {Need at least 2 rows to sort}
    if FRows <= 1 then
      Exit;

    GetMem(CurEl, FElSize);
    try
      GetMem(PivEl, FElSize);

      {Initialize the stacks}
      StackP := 0;
      LStack[0] := 0;
      RStack[0] := FRows-1;

      {Repeatedly take top partition from stack}
      repeat

        {Pop the stack}
        L := LStack[StackP];
        R := RStack[StackP];
        Dec(StackP);

        {Sort current partition}
        repeat

          {Load the pivot element}
          Get((L+R) div 2, KeyCol, PivEl^);
          PL := L;
          PR := R;

          {Swap items in sort order around the pivot index}
          repeat
            Get(PL, KeyCol, CurEl^);
            while Compare(CurEl^, PivEl^) < 0 do begin
              Inc(PL);
              Get(PL, KeyCol, CurEl^);
            end;
            Get(PR, KeyCol, CurEl^);
            while Compare(PivEl^, CurEl^) < 0 do begin
              Dec(PR);
              Get(PR, KeyCol, CurEl^);
            end;
            if PL <= PR then begin
              if PL <> PR then
                {Swap the two elements}
                ExchangeRows(PL, PR);
              Inc(PL); {assume we'll never sort 2 billion elements}
              Dec(PR);
            end;
          until PL > PR;

          {Decide which partition to sort next}
          if (PR-L) < (R-PL) then begin
            {Right partition is bigger}
            if PL < R then begin
              {Stack the request for sorting right partition}
              Inc(StackP);
              LStack[StackP] := PL;
              RStack[StackP] := R;
            end;
            {Continue sorting left partition}
            R := PR;
          end else begin
            {Left partition is bigger}
            if L < PR then begin
              {Stack the request for sorting left partition}
              Inc(StackP);
              LStack[StackP] := L;
              RStack[StackP] := PR;
            end;
            {Continue sorting right partition}
            L := PL;
          end;

        until L >= R;
      until StackP < 0;

      FreeMem(PivEl, FElSize);
    finally
      FreeMem(CurEl, FElSize);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStLMatrix.LoadFromStream(S : TStream);
var
  Data         : pointer;
  Reader       : TReader;
  NumRows      : Integer;
  NumCols      : Integer;
  ElementSize  : cardinal;
  R, C         : Integer;
  TotSize      : Integer;
  StreamedClass : TPersistentClass;
  StreamedClassName : string;
  Value        : TValueType;
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
             (not IsOrInheritsFrom(TStLMatrix, StreamedClass)) then
            RaiseContainerError(stscWrongClass);
          NumRows := ReadInteger;
          NumCols := ReadInteger;
          ElementSize := ReadInteger;
          if (NumRows <> Integer(Rows)) or (NumCols <> Integer(Cols)) or
             (Integer(ElementSize) <> FElSize) then
            begin
              HugeFreeMem(lmData, FCount*FElSize);
              FElSize := ElementSize;
              FRows := NumRows;
              FCols := NumCols;
              FCount := Integer(NumRows)*NumCols;
              lmRowSize := Integer(NumCols)*Integer(ElementSize);
              HugeGetMem(lmData, FCount*Integer(ElementSize));
              Clear;
            end;
          ElementsStorable := ReadBoolean;
          if ElementsStorable then
            begin
              Read(Value, sizeof(Value)); {s/b vaBinary}
              Read(TotSize, sizeof(Integer));
              GetMem(Data, FElSize);
              try
                for R := 0 to pred(FRows) do
                  for C := 0 to pred(FCols) do
                     begin
                       Read(Data^, FElSize);
                       Put(R, C, Data^);
                     end;
              finally
                FreeMem(Data, FElSize);
              end;
            end
          else
            begin
              ReadListBegin;
              for R := 0 to pred(FRows) do
                for C := 0 to pred(FCols) do begin
                  Data := DoLoadData(Reader);
                  Put(R, C, Data^);
                end;
              ReadListEnd;
            end;
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

procedure TStLMatrix.StoreToStream(S : TStream);
var
  Writer : TWriter;
  R, C   : integer;
  Data   : pointer;
  TotSize: Integer;
  Value  : TValueType;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Writer := TWriter.Create(S, 1024);
    try
      GetMem(Data, FElSize);
      try
        with Writer do
          begin
            WriteString(Self.ClassName);
            WriteInteger(FRows);
            WriteInteger(FCols);
            WriteInteger(FElSize);
            WriteBoolean(FElStorable);
            if ElementsStorable then
              begin
                Value := vaBinary;
                Write(Value, sizeof(Value));
                TotSize := FCount * FElSize;
                Write(TotSize, sizeof(Integer));
                for R := 0 to pred(FRows) do
                  for C := 0 to pred(FCols) do
                    begin
                      Get(R, C, Data^);
                      Write(Data^, FElSize);
                    end;
              end
            else
              begin
                WriteListBegin;
                for R := 0 to pred(FRows) do
                  for C := 0 to pred(FCols) do
                    begin
                      Get(R, C, Data^);
                      DoStoreData(Writer, Data);
                    end;
                WriteListEnd;
              end;
          end;
      finally
        FreeMem(Data, FElSize);
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


end.
