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
{* SysTools: StHASH.PAS 4.04                             *}
{*********************************************************}
{* SysTools: Hash table class                            *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  - Generally the same as STDICT.PAS, but the hash table is
    keyed on elements of arbitrary type rather than just strings.

  - Also manages an LRU counter and updates each node's LRU when
    it is added or accessed. If the maximum allowed number of nodes
    in the table is exceeded, the least recently used node is
    automatically removed from the table. By default, MaxLongInt
    nodes can be in the table so the automatic removal logic does
    not come into play. When a node is automatically removed, the
    NodeRemoved virtual method is called to notify the program
    that the node is being removed.
}

unit StHASH;

interface

uses
  SysUtils,
  Classes,
 {$IFDEF ThreadSafe}
  Windows,
 {$ENDIF}
  StConst,
  StBase;

type
  TStHashNode = class(TStNode)
{.Z+}
    protected
      hnNext : TStHashNode;     {Next node in hash list}
      hnValue: Pointer;         {Pointer to value of element}
      hnValSize : Cardinal;     {Size of hnValue memory block}
      FLRU : LongInt;           {LRU counter of this node}

      function GetValue : Pointer;

{.Z-}
    public
      constructor CreateNode(const AValue; AValSize : Cardinal; AData : Pointer);
        virtual;
        {-Initialize node}
      destructor Destroy; override;
        {-Free name string and destroy node}

      property Value : Pointer
         read GetValue;
      property LRU : LongInt
         read FLRU
         write FLRU;
  end;

{.Z+}
  THashArray = array[0..(MaxInt div SizeOf(TStHashNode))-1] of TStHashNode;
  PHashArray = ^THashArray;
{.Z-}

  THashFunc = function (const V; Size : Integer) : Integer;

  TStHashTable = class(TStContainer)
{.Z+}
    protected
      {property instance variables}
      FValSize : Cardinal;          {Size of each element in table}
      FHashSize : Integer;          {Bins in hash array}
      FEqual : TUntypedCompareFunc; {Element compare function}
      FHash : THashFunc;            {Hash function}
      FMaxNodes : LongInt;          {Max nodes allowed in table}

      {private instance variables}
      htHeads : PHashArray;         {Pointer to head of node lists}
      htTails : PHashArray;         {Pointer to tail of node lists}
      htLRU : LongInt;              {LRU counter}
      htIgnoreDups : Boolean;       {Ignore duplicates during Join?}

      {protected undocumented methods}
      procedure htInsertNode(H : Integer; This : TStHashNode);
      procedure htIterate(Action : TIterateFunc; OtherData : Pointer;
                          var H : Integer; var Prev, This : TStHashNode);
      procedure htSetEqual(E : TUntypedCompareFunc);
      procedure htSetHash(H : THashFunc);
      procedure htSetHashSize(Size : Integer);
      procedure htSetMaxNodes(Nodes : LongInt);
      procedure htMoveToFront(H : Integer; Prev, This : TStHashNode);
      procedure htFindNode(const V; var H : Integer;
                           var Prev, This : TStHashNode);
      procedure htUpdateLRU(This : TStHashNode);
      procedure htDeleteOldestNode;

{.Z-}
    public
      constructor Create(AValSize : Cardinal; AHashSize : Integer); virtual;
        {-Initialize an empty hash table}
      destructor Destroy; override;
        {-Destroy a hash table}

      procedure LoadFromStream(S : TStream); override;
        {-Read a hash table and its data from a stream}
      procedure StoreToStream(S : TStream); override;
        {-Write a hash table and its data to a stream}

      procedure Clear; override;
        {-Remove all nodes from container but leave it instantiated}

      function Exists(const V; var Data : Pointer) : Boolean;
        {-Return True and the Data pointer if V is in the hash table}
      procedure Add(const V; Data : Pointer);
        {-Add new value and Data to the hash table}
      procedure Delete(const V);
        {-Delete a value from the hash table}
      procedure Update(const V; Data : Pointer);
        {-Update the data for an existing element}
      function Find(Data : Pointer; var V) : Boolean;
        {-Return True and the element value that matches Data}

      procedure Assign(Source: TPersistent); override;
        {-Assign another hash table's contents to this one}
      procedure Join(H : TStHashTable; IgnoreDups : Boolean);
        {-Add hash table H into this one and dispose H}

      function Iterate(Action : TIterateFunc;
                       OtherData : Pointer) : TStHashNode;
        {-Call Action for all the nodes, returning the last node visited}

      procedure NodeRemoved(const V; Data : Pointer); virtual;
        {-Called when a not recently used node is removed from the table}

      function BinCount(H : Integer) : LongInt;
        {-Return number of names in a hash bin (for testing)}

      property Equal : TUntypedCompareFunc
        {-Change the string compare function; only for an empty table}
        read FEqual
        write htSetEqual;

      property Hash : THashFunc
        {-Change the hash function; only for an empty table}
        read FHash
        write htSetHash;

      property HashSize : Integer
        {-Change the hash table size; preserves existing elements}
        read FHashSize
        write htSetHashSize;

      property ValSize : Cardinal
        {-Read the size of each element in the table}
        read FValSize;

      property MaxNodes : LongInt
        {-Change the maximum nodes in the table}
        read FMaxNodes
        write htSetMaxNodes;
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



{----------------------------------------------------------------------}

constructor TStHashNode.CreateNode(const AValue; AValSize : Cardinal;
                                   AData : Pointer);
begin
  Create(AData);
  hnValSize := AValSize;
  GetMem(hnValue, AValSize);
  Move(AValue, hnValue^, AValSize);
end;

destructor TStHashNode.Destroy;
begin
  if Assigned(hnValue) then
    FreeMem(hnValue, hnValSize);
  inherited Destroy;
end;

function TStHashNode.GetValue : Pointer;
begin
  Result := hnValue;
end;

{----------------------------------------------------------------------}

procedure TStHashTable.Add(const V; Data : Pointer);
var
  H : Integer;
  P, T : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    htFindNode(V, H, P, T);
    if Assigned(T) then
      RaiseContainerError(stscDupNode);
    htInsertNode(H, TStHashNode.CreateNode(V, FValSize, Data));
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function AssignNode(Container : TStContainer;
                    Node : TStNode;
                    OtherData : Pointer) : Boolean; far;
var
  HashNode : TStHashNode absolute Node;
  OurHashTbl : TStHashTable absolute OtherData;
begin
  OurHashTbl.Add(HashNode.Value^, HashNode.Data);
  Result := true;
end;

procedure TStHashTable.Assign(Source: TPersistent);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {The only container that we allow to be assigned to a hash table
     is... another hash table}
    if (Source is TStHashTable) then begin
      Clear;
      FValSize := TStHashTable(Source).ValSize;
      TStHashTable(Source).Iterate(AssignNode, Self);
    end
    else
      inherited Assign(Source);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


function TStHashTable.BinCount(H : Integer) : LongInt;
var
  C : LongInt;
  T : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    C := 0;
    T := htHeads^[H];
    while Assigned(T) do begin
      inc(C);
      T := T.hnNext;
    end;
    Result := C;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStHashTable.Clear;
var
  TableSize : Cardinal;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if FCount <> 0 then begin
      Iterate(DestroyNode, nil);
      FCount := 0;
      htLRU := 0;
      TableSize := FHashSize*SizeOf(TStHashNode);
      FillChar(htHeads^, TableSize, 0);
      FillChar(htTails^, TableSize, 0);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

constructor TStHashTable.Create(AValSize : Cardinal; AHashSize : Integer);
begin
  if AValSize = 0 then
    RaiseContainerError(stscBadSize);

  CreateContainer(TStHashNode, 0);

  FValSize := AValSize;
  FMaxNodes := MaxLongInt;

  {allocate hash table by assigning to the HashSize property}
  HashSize := AHashSize;
end;

procedure TStHashTable.Delete(const V);
var
  H : Integer;
  P, T : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    htFindNode(V, H, P, T);
    if Assigned(T) then begin
      if Assigned(P) then
        P.hnNext := T.hnNext
      else
        htHeads^[H] := T.hnNext;
      if T = htTails^[H] then
        htTails^[H] := P;
      DestroyNode(Self, T, nil);
      Dec(FCount);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

destructor TStHashTable.Destroy;
var
  TableSize : Cardinal;
begin
  if conNodeProt = 0 then
    Clear;
  TableSize := FHashSize*SizeOf(TStHashNode);
  if Assigned(htHeads) then
    FreeMem(htHeads, TableSize);
  if Assigned(htTails) then
    FreeMem(htTails, TableSize);
  IncNodeProtection;
  inherited Destroy;
end;

function TStHashTable.Exists(const V; var Data : Pointer) : Boolean;
var
  H : Integer;
  P, T : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    htFindNode(V, H, P, T);
    if Assigned(T) then begin
      htMoveToFront(H, P, T);
      htUpdateLRU(T);
      Result := True;
      Data := T.Data;
    end else
      Result := False;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function FindNodeData(Container : TStContainer; Node : TStNode;
                      OtherData : Pointer) : Boolean; far;
begin
  Result := (OtherData <> Node.Data);
end;

function TStHashTable.Find(Data : Pointer; var V) : Boolean;
var
  H : Integer;
  P, T : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    htIterate(FindNodeData, Data, H, P, T);
    if Assigned(T) then begin
      htMoveToFront(H, P, T);
      htUpdateLRU(T);
      Result := True;
      Move(T.Value^, V, FValSize);
    end else
      Result := False;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStHashTable.htDeleteOldestNode;
  {-Find and delete the hash node with the smallest LRU counter}
var
  H, MinH :  Integer;
  MinLRU : LongInt;
  T, P : TStHashNode;
begin
  if FCount <> 0 then begin
    MinLRU := MaxLongInt;
    MinH := 0;
    for H := 0 to FHashSize-1 do
      if Assigned(htTails^[H]) and (htTails^[H].LRU <= MinLRU) then begin
        MinH := H;
        MinLRU := htTails^[H].LRU;
      end;

    {notify the application}
    with htTails^[MinH] do
      NodeRemoved(hnValue^, Data);

    {destroy the node}
    DestroyNode(Self, htTails^[MinH], nil);
    dec(FCount);

    {remove the node}
    if htTails^[MinH] = htHeads^[MinH] then begin
      {only node in this bin}
      htTails^[MinH] := nil;
      htHeads^[MinH] := nil;
    end else begin
      {at least two nodes in this bin}
      T := htHeads^[MinH];
      P := nil;
      while T <> htTails^[MinH] do begin
        P := T;
        T := T.hnNext;
      end;
      P.hnNext := nil;
      htTails^[MinH] := P;
    end;
  end;
end;

procedure TStHashTable.htFindNode(const V; var H : Integer;
                                  var Prev, This : TStHashNode);
var
  P, T : TStHashNode;
begin
  if not(Assigned(FEqual) and Assigned(FHash)) then
    RaiseContainerError(stscNoCompare);

  Prev := nil;
  This := nil;
  H := FHash(V, HashSize);
  T := htHeads^[H];
  P := nil;
  while Assigned(T) do begin
    if FEqual(V, T.Value^) = 0 then begin
      Prev := P;
      This := T;
      Exit;
    end;
    P := T;
    T := T.hnNext;
  end;

  {not found}
  This := nil;
end;

procedure TStHashTable.htInsertNode(H : Integer; This : TStHashNode);
  {-Insert node This at front of hash bin H}
var
  P : TStHashNode;
begin
  P := htHeads^[H];
  htHeads^[H] := This;
  if not Assigned(htTails^[H]) then
    htTails^[H] := This;
  This.hnNext := P;
  htUpdateLRU(This);
  Inc(FCount);
  if FCount > FMaxNodes then
    htDeleteOldestNode;
end;

procedure TStHashTable.htIterate(Action : TIterateFunc; OtherData : Pointer;
                                 var H : Integer; var Prev, This : TStHashNode);
  {-Internal version of Iterate that returns more details}
var
  AHash :  Integer;
  P, T, N : TStHashNode;
begin
  if FCount <> 0 then begin
    for AHash := 0 to FHashSize-1 do begin
      T := htHeads^[AHash];
      P := nil;
      while Assigned(T) do begin
        N := T.hnNext;
        if Action(Self, T, OtherData) then begin
          P := T;
          T := N;
        end else begin
          H := AHash;
          Prev := P;
          This := T;
          Exit;
        end;
      end;
    end;
  end;
  This := nil;
end;

procedure TStHashTable.htMoveToFront(H : Integer; Prev, This : TStHashNode);
  {-Move This to front of list}
begin
  if Assigned(Prev) then begin
    Prev.hnNext := This.hnNext;
    This.hnNext := htHeads^[H];
    htHeads^[H] := This;
    if This = htTails^[H] then
      htTails^[H] := Prev;
  end;
end;

procedure TStHashTable.htSetEqual(E : TUntypedCompareFunc);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      FEqual := E;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStHashTable.htSetHash(H : THashFunc);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      FHash := H;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStHashTable.htSetHashSize(Size : Integer);
var
  HInx     : integer;
  TableSize: LongInt;
  Temp     : TStHashNode;
  Node     : TStHashNode;
  OldHeads : PHashArray;
  OldTails : PHashArray;
  OldSize  : Integer;
  OldCount : Integer;
  OldDisposeData : TDisposeDataProc;
  OldOnDisposeData : TStDisposeDataEvent;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {calculate the new table size}
    TableSize := LongInt(Size) * sizeof(TStHashNode);
    if (Size <= 0) {or (TableSize > MaxBlockSize)} then
      RaiseContainerError(stscBadSize);

    {only do something if there's something to do}
    if (Size <> FHashSize) then begin

      {Notes: lots of things are going to be happening here: new
              allocations, nodes copied from the old table to the new,
              etc. Ideally if an exception is raised we would like to
              restore the hash table to the state it was in
              originally, before letting the exception escape}

      {save enough data about the current state of the table to
       allow restoring in case of an exception}
      OldHeads := htHeads;
      OldTails := htTails;
      OldSize := FHashSize;
      OldCount := FCount;
      OldDisposeData := DisposeData;
      OldOnDisposeData := OnDisposeData;

      {reset Self's data}
      htHeads := nil;
      htTails := nil;
      FHashSize := Size;
      FCount := 0;
      DisposeData := nil;
      OnDisposeData := nil;

      {from this point, exceptions can occur with impunity...}
      try
        {allocate the new head and tail tables}
        htHeads := AllocMem(TableSize);
        htTails := AllocMem(TableSize);

        {if there is data to transfer, do so}
        if (OldHeads <> nil) and (OldCount <> 0) then begin
          for HInx := 0 to pred(OldSize) do begin
            Node := OldHeads^[HInx];
            while Assigned(Node) do begin
              Add(Node.hnValue^, Node.Data);
              Node := Node.hnNext;
            end;
          end;
        end;

        {now all the data has been transferred, we can
         destroy the old table}
        if (OldHeads <> nil) then begin
          for HInx := 0 to pred(OldSize) do begin
            Node := OldHeads^[HInx];
            while Assigned(Node) do begin
              Temp := Node;
              Node := Node.hnNext;
              Temp.Free;
            end;
          end;
          FreeMem(OldHeads, OldSize * sizeof(TStHashNode));
        end;
        if (OldTails <> nil) then
          FreeMem(OldTails, OldSize * sizeof(TStHashNode));

        {restore the disposedata routines}
        DisposeData := OldDisposeData;
        OnDisposeData := OldOnDisposeData;

      except
        {destroy the new data}
        if (htHeads <> nil) then begin
          for HInx := 0 to pred(FHashSize) do begin
            Node := htHeads^[HInx];
            while Assigned(Node) do begin
              Temp := Node;
              Node := Node.hnNext;
              Temp.Free;
            end;
          end;
          FreeMem(htHeads, TableSize);
        end;
        if (htTails <> nil) then
          FreeMem(htTails, TableSize);
        {restore the old data}
        htHeads := OldHeads;
        htTails := OldTails;
        FHashSize := OldSize;
        FCount := OldCount;
        DisposeData := OldDisposeData;
        OnDisposeData := OldOnDisposeData;
        {reraise the exception}
        raise;
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStHashTable.htSetMaxNodes(Nodes : LongInt);
begin
  if Nodes < 1 then
    RaiseContainerError(stscBadSize);
  FMaxNodes := Nodes;
  while FCount > FMaxNodes do
    htDeleteOldestNode;
end;

type
  TMinNode = record
    MLRU : LongInt;
    MNode : TStHashNode;
  end;
  PMinNode = ^TMinNode;

function FindMinPositiveNode(Container : TStContainer;
                             Node : TStNode;
                             OtherData : Pointer) : Boolean; far;
  {-Used to find the smallest non-negative LRU in the table}
begin
  with PMinNode(OtherData)^, TStHashNode(Node) do
    if (LRU >= 0) and (LRU <= MLRU) then begin
      MLRU := LRU;
      MNode := TStHashNode(Node);
    end;
  Result := True;
end;

function NegateNodeLRU(Container : TStContainer;
                       Node : TStNode;
                       OtherData : Pointer) : Boolean; far;
  {-Used to negate the LRU values of all nodes in the table}
begin
  with TStHashNode(Node) do
    LRU := -LRU;
  Result := True;
end;

procedure TStHashTable.htUpdateLRU(This : TStHashNode);
  {-Reassign all LRU values sequentially in their existing order}
var
  MinNode : TMinNode;
begin
  inc(htLRU);
  This.LRU := htLRU;
  if htLRU = MaxLongInt then begin
    {scan table and pack LRU values}
    htLRU := 0;
    repeat
      inc(htLRU);
      MinNode.MLRU := MaxLongInt;
      MinNode.MNode := nil;
      Iterate(FindMinPositiveNode, @MinNode);
      if not Assigned(MinNode.MNode) then
        break;
      {nodes already visited are set to a negative value}
      {depends on never having an LRU of zero}
      MinNode.MNode.LRU := -htLRU;
    until False;
    {negative values are made positive}
    Iterate(NegateNodeLRU, nil);
  end;
end;

function TStHashTable.Iterate(Action : TIterateFunc;
                              OtherData : Pointer) : TStHashNode;
var
  H :  Integer;
  P : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    htIterate(Action, OtherData, H, P, Result);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function JoinNode(Container : TStContainer;
                  Node : TStNode;
                  OtherData : Pointer) : Boolean; far;
  {-Used to add nodes from another table into this one}
var
  H : Integer;
  P, T : TStHashNode;
begin
  Result := True;
  with TStHashTable(OtherData) do begin
    htFindNode(TStHashNode(Node).Value^, H, P, T);
    if Assigned(T) then
      if htIgnoreDups then begin
        Node.Free;
        Exit;
      end else
        RaiseContainerError(stscDupNode);
    htInsertNode(H, TStHashNode(Node));
  end;
end;

procedure TStHashTable.Join(H : TStHashTable; IgnoreDups : Boolean);
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  H.EnterCS;
  try
{$ENDIF}
    htIgnoreDups := IgnoreDups;
    H.Iterate(JoinNode, Self);
    {dispose of D, but not its nodes}
    H.IncNodeProtection;
    H.Free;
{$IFDEF ThreadSafe}
  finally
    H.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

procedure TStHashTable.LoadFromStream(S : TStream);
var
  Data, Value : Pointer;
  AValSize : Cardinal;
  Reader : TReader;
  StreamedClass : TPersistentClass;
  StreamedNodeClass : TPersistentClass;
  StreamedClassName : string;
  StreamedNodeClassName : string;
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
        if not Assigned(StreamedClass) then
          RaiseContainerErrorFmt(stscUnknownClass, [StreamedClassName]);
        if (StreamedClass <> Self.ClassType) then
          RaiseContainerError(stscWrongClass);
        StreamedNodeClassName := ReadString;
        StreamedNodeClass := GetClass(StreamedNodeClassName);
        if not Assigned(StreamedNodeClass) then
          RaiseContainerErrorFmt(stscUnknownNodeClass, [StreamedNodeClassName]);
        if (StreamedNodeClass <> conNodeClass) then
          RaiseContainerError(stscWrongNodeClass);

        AValSize := ReadInteger;
        if AValSize <> FValSize then
          RaiseContainerError(stscBadSize);
        HashSize := ReadInteger;
        FMaxNodes := ReadInteger;
        GetMem(Value, FValSize);
        try
          ReadListBegin;
          while not EndOfList do begin
            ReadBoolean;
            Read(Value^, FValSize);
            Data := DoLoadData(Reader);
            Add(Value^, Data);
          end;
          ReadListEnd;
        finally
          FreeMem(Value, FValSize);
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

procedure TStHashTable.NodeRemoved(const V; Data : Pointer);
begin
  {does nothing by default}
end;

procedure TStHashTable.StoreToStream(S : TStream);
var
  H : Integer;
  Walker : TStHashNode;
  Writer : TWriter;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Writer := TWriter.Create(S, 1024);
    try
      with Writer do begin
        WriteString(Self.ClassName);
        WriteString(conNodeClass.ClassName);
        WriteInteger(FValSize);
        WriteInteger(FHashSize);
        WriteInteger(FMaxNodes);
        WriteListBegin;
        if Count <> 0 then
          for H := 0 to FHashSize-1 do begin
            Walker := htHeads^[H];
            while Assigned(Walker) do begin
              {writing the True boolean prevents false termination of the
               list if Value's first byte is zero when the stream is
               loaded into another hash table}
              WriteBoolean(True);
              Write(Walker.Value^, FValSize);
              DoStoreData(Writer, Walker.Data);
              Walker := Walker.hnNext;
            end;
          end;
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

procedure TStHashTable.Update(const V; Data : Pointer);
var
  H : Integer;
  P, T : TStHashNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    htFindNode(V, H, P, T);
    if Assigned(T) then begin
      htMoveToFront(H, P, T);
      htUpdateLRU(T);
      T.Data := Data;
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
