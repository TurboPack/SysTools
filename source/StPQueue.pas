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
{* SysTools: StPQueue.pas 4.04                           *}
{*********************************************************}
{* SysTools: Priority Queue Classes                      *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
   Based on the double-ended heap (deap) described in Horowitz and Sahni,
   Data Structures and Algorithms in C.

   The deap was first reported in:
     Svante Carlsson, "The Deap - a double-ended heap to implement double-
     ended priority queues", Information Processing Letters, 26,
     pp. 33-36, 1987.

   A deap is a complete binary tree. The root node holds no data. Its
   left subtree is a min heap. Its right subtree is a max heap. If the right
   subtree is not empty, let i be any node in the left subtree. Let j be
   the node at the corresponding position in the right subtree. If such a
   j does not exist, let j be the node in the right subtree at the position
   corresponding to i's parent. The deap has the property that the data in
   node i is less than or equal to the data in node j.

   Insertion is an O(log2(n)) operation. Deletion of the min or max node
   is also an O(log2(n)) operation.

   Data elements in the deap are pointers, which can point to any record
   structure or class, or can contain any data type of 4 bytes or less.
   The deap needs an ordering relationship, so it is essential to assign
   to the Compare property inherited from the TStContainer class.

   STPQUEUE uses the DisposeData procedure of TStContainer to determine
   how to free elements in the collection. By default, it does nothing.

   In 16-bit programs the deap is limited to 16380 elements. In 32-bit
   programs the limit is set by memory usage or performance.
}

unit StPQueue;

interface

uses
  Windows, SysUtils, Classes,
  StConst, StBase;

type
  {first actual element is at index 2}
  {.Z+}
  TStPQData = array[2..(StMaxBlockSize div SizeOf(Pointer))+1] of Pointer;
  PStPQData = ^TStPQData;
  {.Z-}

  TStPQueue = class(TStContainer)
    {.Z+}
    protected {private}
      pqData     : PStPQData;         {data - the complete binary tree}
      pqCapacity : Integer;           {max elements currently possible}
      pqDelta    : Integer;           {delta elements to grow when needed}

      procedure ForEachPointer(Action : TIteratePointerFunc; OtherData : Pointer);
        override;
      function StoresPointers : Boolean;
        override;

      procedure Expand(Need : Integer);
      procedure InsertMin(I : Integer; Data : Pointer);
      procedure InsertMax(I : Integer; Data : Pointer);
      procedure ModifiedInsert(I : Integer; Data : Pointer);

    {.Z-}
    public
      constructor Create(InitCapacity, Delta : Integer);
        virtual;
        {-Initialize an empty PQueue of given capacity. If it overflows
          grow the PQueue by Delta elements}
      destructor Destroy;
        override;
        {-Free a PQueue}

      procedure LoadFromStream(S : TStream);
        override;
        {-Create a PQueue and its data from a stream}
      procedure StoreToStream(S : TStream);
        override;
        {-Write a PQueue and its data to a stream}

      procedure Clear;
        override;
        {-Remove all data from container but leave it instantiated and
          with its current capacity}

      procedure Insert(Data : Pointer);
        {-Add a new node}
      function DeleteMin : Pointer;
        {-Remove the minimum node and return its Pointer}
      function DeleteMax : Pointer;
        {-Remove the maximum node and return its Pointer}

      procedure Assign(Source : TPersistent);
        override;
        {-Assign another container's contents to this one. Only SysTools
          containers that store pointers are allowed.}
      procedure Join(Q : TStPQueue);
        {-Add PQueue Q into this one and dispose Q}

      function Iterate(Action : TIteratePointerFunc;
        OtherData : Pointer) : Pointer;
        {-Call Action for all the nodes or until Action returns false. Note
          that the nodes are visited in no particular order.}

      function Test : Boolean;
        {-Determine whether deap properties are currently valid (for debugging)}
  end;

  {.Z+}
  TStPQueueClass = class of TStPQueue;
  {.Z-}


implementation

{$IFDEF ThreadSafe}
var
  ClassCritSect : TRTLCriticalSection;
{$ENDIF}

type
  TStoreInfo = record
    Wtr : TWriter;
    SDP : TStoreDataProc;
  end;

function AssignData(Container : TStContainer;
                    Data, OtherData : Pointer) : Boolean; far;
begin
  TStPQueue(OtherData).Insert(Data);
  AssignData := True;
end;

function DestroyNode(Container : TStContainer;
  Data, OtherData : Pointer) : Boolean; far;
begin
  if Assigned(Data) then
    Container.DoDisposeData(Data);
  DestroyNode := True;
end;

procedure EnterClassCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(ClassCritSect);
{$ENDIF}
end;

function JoinData(Container : TStContainer;
  Data, OtherData : Pointer) : Boolean; far;
begin
  TStPQueue(OtherData).Insert(Data);
  JoinData := True;
end;

procedure LeaveClassCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(ClassCritSect);
{$ENDIF}
end;

function log2(I : Integer) : Integer;
  {-Return the Integer below log2(I)}
begin
  Result := 0;
  while (I > 1) do begin
    Inc(Result);
    I := I shr 1;
  end;
end;

function StoreNode(Container : TStContainer;
  Data, OtherData : Pointer) : Boolean; far;
begin
  StoreNode := True;
  with TStoreInfo(OtherData^) do
    SDP(Wtr, Data);
end;

procedure TStPQueue.Assign(Source : TPersistent);
begin
  {$IFDEF ThreadSafe}
  EnterCS;
  try
  {$ENDIF}
    if not AssignPointers(Source, AssignData) then
      inherited Assign(Source);
  {$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
  {$ENDIF}
end;

procedure TStPQueue.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if conNodeProt = 0 then
      ForEachPointer(StPQueue.DestroyNode, nil);
    FCount := 0;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

constructor TStPQueue.Create(InitCapacity, Delta : Integer);
begin
  if (InitCapacity < 2) or (Delta < 1) then
    RaiseContainerError(stscBadSize);

  FCount := 0;
  {ensure that Expand creates initial capacity InitCapacity}
  pqCapacity := -Delta;
  pqDelta := Delta;
  pqData := nil;

  CreateContainer(TStNode, 0);

  Expand(InitCapacity);
end;

function TStPQueue.DeleteMin : Pointer;
var
  I, j, n : Integer;
  Temp    : Pointer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (FCount < 1) then begin
      {deap is empty}
      DeleteMin := nil;
      exit;
    end;

    {return min element}
    DeleteMin := pqData^[2];

    {save last element and reset (helps debugging)}
    Temp := pqData^[FCount+1];
    pqData^[FCount+1] := nil;
    {decrement count, n is index of new last element}
    n := FCount;
    dec(FCount);

    if (FCount > 0) then begin
      {move empty min-root down to an appropriate leaf}
      I := 2;
      while (I shl 1 <= n) do begin
        {find child with smaller key}
        j := I shl 1;
        if (j+1 <= n) then
          if (DoCompare(pqData^[j], pqData^[j+1]) > 0) then
            Inc(j);
        pqData^[I] := pqData^[j];
        I := j;
      end;

      {insert the old last element at the given leaf position}
      ModifiedInsert(I, Temp);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStPQueue.DeleteMax : Pointer;
var
  I, j, n : Integer;
  Temp    : Pointer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (FCount < 1) then begin
      {deap is empty}
      DeleteMax := nil;
      exit;
    end;

    {return max element}
    if (FCount = 1) then
      DeleteMax := pqData^[2]
    else
      DeleteMax := pqData^[3];

    {save last element and reset (helps debugging)}
    Temp := pqData^[FCount+1];
    pqData^[FCount+1] := nil;
    {decrement count, n is index of new last element}
    n := FCount;
    dec(FCount);

    if (FCount > 0) then begin
      {move empty max-root down to an appropriate leaf}
      I := 3;
      while (I shl 1 <= n) do begin
        {find child with larger key}
        j := I shl 1;
        if (j+1 <= n) then
          if (DoCompare(pqData^[j], pqData^[j+1]) < 0) then
            Inc(j);
        pqData^[I] := pqData^[j];
        I := j;
      end;

      {insert the old last element at the given leaf position}
      ModifiedInsert(I, Temp);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

destructor TStPQueue.Destroy;
begin
  if (pqData <> nil) then begin
    Clear;
    FreeMem(pqData, pqCapacity*SizeOf(Pointer));
  end;

  IncNodeProtection;
  inherited Destroy;
end;

procedure TStPQueue.Expand(Need : Integer);
var
  NewCapacity : Integer;
  Size        : Integer;
  NewData     : PStPQData;
begin
  if Need > pqCapacity then begin
    {determine new capacity}
    NewCapacity := pqCapacity+pqDelta;
    if (NewCapacity < Need) then
      NewCapacity := Need;

    {make sure it's feasible to allocate it}
    Size := Integer(NewCapacity)*SizeOf(Pointer);
    {if Size > MaxBlockSize then}
      {RaiseContainerError(stscBadSize);}                              

    {allocate new data}
    GetMem(NewData, Size);

    {copy old data to it and free old data}
    if (pqData <> nil) then begin
      move(pqData^, NewData^, pqCapacity*SizeOf(Pointer));
      FreeMem(pqData, pqCapacity*SizeOf(Pointer));
    end;

    {update instance variables}
    pqData := NewData;
    pqCapacity := NewCapacity;
  end;
end;

procedure TStPQueue.ForEachPointer(Action : TIteratePointerFunc; OtherData : Pointer);
var
  I : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {first element is 2, last is FCount+1}
    for I := 2 to FCount+1 do
      if not Action(Self, pqData^[I], OtherData) then
        Exit;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStPQueue.Insert(Data : Pointer);
var
  I, n, p : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {adding an element, make sure there's space}
    Inc(FCount);
    Expand(FCount);

    if (FCount = 1) then
      {insert into empty deap}
      pqData^[2] := Data
    else begin
      {n is the actual array index}
      n := FCount+1;
      {determine whether n is in the min or max subtree}
      p := n;
      while (p > 3) do
        p := p shr 1;
      if (p = 2) then begin
        {n is a position on the min side}
        {I is its partner on the max side}
        I := (n+(1 shl (log2(n)-1))) shr 1;
        if (DoCompare(Data, pqData^[I]) > 0) then begin
          pqData^[n] := pqData^[I];
          InsertMax(I, Data);
        end else
          InsertMin(n, Data);
      end else begin
        {n is a position on the max side}
        {I is its partner on the min side}
        I := n-(1 shl (log2(n)-1));
        if (DoCompare(Data, pqData^[I]) < 0) then begin
          pqData^[n] := pqData^[I];
          InsertMin(I, Data);
        end else
          InsertMax(n, Data);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStPQueue.InsertMin(I : Integer; Data : Pointer);
  {-Insert into min-heap rooted at node 2}
var
  j : Integer;
begin
  while (I > 2) and (DoCompare(Data, pqData^[I shr 1]) < 0) do begin
    j := I shr 1;
    pqData^[I] := pqData^[j];
    I := j;
  end;
  pqData^[I] := Data;
end;

procedure TStPQueue.InsertMax(I : Integer; Data : Pointer);
  {-Insert into max-heap rooted at node 3}
var
  j : Integer;
begin
  while (I > 3) and (DoCompare(Data, pqData^[I shr 1]) > 0) do begin
    j := I shr 1;
    pqData^[I] := pqData^[j];
    I := j;
  end;
  pqData^[I] := Data;
end;

function TStPQueue.Iterate(Action : TIteratePointerFunc;
  OtherData : Pointer) : Pointer;
var
  I : Integer;
begin
  Iterate := nil;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {first element is 2, last is FCount+1}
    for I := 2 to FCount+1 do
      if not Action(Self, pqData^[I], OtherData) then begin
        Iterate := pqData^[I];
        Exit;
      end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStPQueue.Join(Q : TStPQueue);
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  Q.EnterCS;
  try
{$ENDIF}
    if (not Assigned(Q)) then
      RaiseContainerError(stscBadType);
    Q.ForEachPointer(JoinData, Self);
    Q.IncNodeProtection;
    Q.Free;
{$IFDEF ThreadSafe}
  finally
    Q.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

procedure TStPQueue.LoadFromStream(S : TStream);
var
  Data : Pointer;
  Reader : TReader;
  StreamedClass : TPersistentClass;
  StreamedClassName : string;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Clear;
    Reader := TReader.Create(S, 1024);
    try
      with Reader do begin
        StreamedClassName := ReadString;
        StreamedClass := GetClass(StreamedClassName);
        if (StreamedClass = nil) then
          RaiseContainerErrorFmt(stscUnknownClass, [StreamedClassName]);
        if (not IsOrInheritsFrom(StreamedClass, Self.ClassType)) or
            (not IsOrInheritsFrom(TStPQueue, StreamedClass)) then
          RaiseContainerError(stscWrongClass);
        ReadListBegin;
        while not EndOfList do begin
          Data := DoLoadData(Reader);
          Insert(Data);
        end;
        ReadListEnd;
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

procedure TStPQueue.ModifiedInsert(I : Integer; Data : Pointer);
  {-Special insert after a delete. I is the actual array index where
    insertion of Data occurs. Tree does not grow.}
var
  p, j : Integer;
begin
  if (I > 1) then begin
    {determine whether I is in the min or max subtree}
    p := I;
    while (p > 3) do
      p := p shr 1;
    if (p = 2) then begin
      {I is a position on the min side}
      {j is its partner on the max side}
      j := I+(1 shl (log2(I)-1));
      if (j > FCount+1) then
        j := j shr 1;
      if (j < 3) then
        {empty max heap}
        pqData^[I] := Data
      else if (DoCompare(Data, pqData^[j]) > 0) then begin
        pqData^[I] := pqData^[j];
        InsertMax(j, Data);
      end else
        InsertMin(I, Data);
    end else begin
      {I is a position on the max side}
      {j is its partner on the min side}
      j := I-(1 shl (log2(I)-1));
      {check its children too to preserve deap property}
      if (j shl 1 <= FCount+1) then begin
        j := j shl 1;
        if (j+1 <= FCount+1) then
          if (DoCompare(pqData^[j], pqData^[j+1]) < 0) then
            Inc(j);
      end;
      if (DoCompare(Data, pqData^[j]) < 0) then begin
        pqData^[I] := pqData^[j];
        InsertMin(j, Data);
      end else
        InsertMax(I, Data);
    end;
  end;
end;

function TStPQueue.StoresPointers : Boolean;
begin
  StoresPointers := True;
end;

procedure TStPQueue.StoreToStream(S : TStream);
var
  Writer : TWriter;
  StoreInfo : TStoreInfo;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Writer := TWriter.Create(S, 1024);
    try
      with Writer do begin
        WriteString(Self.ClassName);
        WriteListBegin;
        StoreInfo.Wtr := Writer;
        StoreInfo.SDP := StoreData;
        Iterate(StoreNode, @StoreInfo);
        WriteListEnd;
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

function TStPQueue.Test : Boolean;
var
  I, i2, j, n, p : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Test := True;
    if (FCount = 0) then
      exit;
    n := FCount+1;
    {start with each leaf node}
    for I := (1 shl log2(n)) to n do begin
      p := I;
      while (p > 3) do
        p := p shr 1;
      if (p = 2) then begin
        {I is a position on the min side}
        {test min-heap condition}
        i2 := I;
        while (i2 shr 1 >= 2) do begin
          j := i2 shr 1;
          if (DoCompare(pqData^[j], pqData^[i2]) > 0) then begin
            Test := false;
            {writeln('min: j=', j, ' i2=', i2,
              ' d[j]=', Integer(pqData^[j]), ' d[i2]=', Integer(pqData^[i2]));}
            exit;
          end;
          i2 := j;
        end;
        {test deap condition}
        if n >= 3 then begin
          j := I+(1 shl (log2(I)-1));
          if (j > n) then
            j := j shr 1;
          if (DoCompare(pqData^[I], pqData^[j]) > 0) then begin
            Test := false;
            {writeln('deap: j=', j, ' I=', I,
              ' d[j]=', Integer(pqData^[j]), ' d[I]=', Integer(pqData^[I]));}
            exit;
          end;
        end;
      end else begin
        {I is a position on the max side}
        {test max-heap condition}
        i2 := I;
        while (i2 shr 1 >= 3) do begin
          j := i2 shr 1;
          if (DoCompare(pqData^[j], pqData^[i2]) < 0) then begin
            Test := false;
            {writeln('max: j=', j, ' i2=', i2,
              ' d[j]=', Integer(pqData^[j]), ' d[i2]=', Integer(pqData^[i2]));}
            exit;
          end;
          i2 := j;
        end;
      end;
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