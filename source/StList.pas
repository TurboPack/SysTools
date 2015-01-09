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
{* SysTools: StList.pas 4.04                             *}
{*********************************************************}
{* SysTools: Linked list class                           *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  Nodes stored in the list can be of type TStListNode or of a derived type.
  Pass the node class to the list constructor.

  TStList is a doubly-linked list that can be scanned backward just as
  efficiently as forward.

  The list retains the index and node of the last node found by Nth (or by
  the indexed array property). This makes For loops that scan a list much
  faster and speeds up random calls to Nth by about a factor of two.
}

unit StList;

interface

uses
  Windows, SysUtils, Classes,
  StConst, StBase;

type
  TStListNode = class(TStNode)
  {.Z+}
    protected
      FNext : TStListNode;     {Next node}
      FPrev : TStListNode;     {Previous node}

  {.Z-}
    public
      constructor Create(AData : Pointer); override;
        {-Initialize node}
  end;

  TStList = class(TStContainer)
  {.Z+}
  protected
    {property instance variables}
    FHead : TStListNode;     {Start of list}
    FTail : TStListNode;     {End of list}

    {private instance variables}
    lsLastI : Integer;        {Last index requested from Nth}
    lsLastP : TStListNode;    {Last node returned by Nth}

    {protected undocumented methods}
    procedure ForEachPointer(Action : TIteratePointerFunc;
                             OtherData : pointer);
      override;
  function StoresPointers : boolean;
    override;
  {.Z-}
  public
    constructor Create(NodeClass : TStNodeClass); virtual;
      {-Initialize an empty list}

    procedure LoadFromStream(S : TStream); override;
      {-Create a list and its data from a stream}
    procedure StoreToStream(S : TStream); override;
      {-Write a list and its data to a stream}

    procedure Clear; override;
      {-Remove all nodes from container but leave it instantiated}

    function Append(Data : Pointer) : TStListNode;
      {-Add a new node to the end of a list}
    function Insert(Data : Pointer) : TStListNode;
      {-Insert a new node at the start of a list}
    function Place(Data : Pointer; P : TStListNode) : TStListNode;
      {-Place a new node into a list after an existing node P}
    function PlaceBefore(Data : Pointer; P : TStListNode) : TStListNode;
      {-Place a new node into a list before an existing node P}
    function InsertSorted(Data : Pointer) : TStListNode;
      {-Insert a new node in sorted order}
    procedure MoveToHead(P : TStListNode);
      {-Move P to the head of the list}

    procedure Assign(Source: TPersistent); override;
      {-Assign another container's contents to this one}
    procedure Join(P : TStListNode; L : TStList);
      {-Join list L after P in the current list. L is freed}
    function Split(P : TStListNode) : TStList;
      {-Split list, creating a new list that starts with P}

    procedure Sort;
      {-Put the list into sorted order}

    procedure Delete(P : TStListNode);
      {-Remove an element and dispose of its contents}

    function Next(P : TStListNode) : TStListNode;
      {-Return the node after P, nil if none}
    function Prev(P : TStListNode) : TStListNode;
      {-Return the node before P, nil if none}
    function Nth(Index : Integer) : TStListNode;
      {-Return the Index'th node in the list, Index >= 0 (cached)}
    function NthFrom(P : TStListNode; Index : Integer) : TStListNode;
      {-Return the Index'th node from P, either direction}
    function Posn(P : TStListNode) : Integer;
      {-Return the ordinal position of an element in the list}
    function Distance(P1, P2 : TStListNode) : Integer;
      {-Return the number of nodes separating P1 and P2 (signed)}
    function Find(Data : Pointer) : TStListNode;
      {-Return the first node whose data equals Data}
    function Iterate(Action : TIterateFunc; Up : Boolean;
                     OtherData : Pointer) : TStListNode;
      {-Call Action for all the nodes, returning the last node visited}

    property Head : TStListNode
      {-Return the head node}
      read FHead;
    property Tail : TStListNode
      {-Return the tail node}
      read FTail;
    property Items[Index : Integer] : TStListNode
      {-Return the Index'th node, 0-based}
      read Nth;
      default;
  end;

  {.Z+}
  TStListClass = class of TStList;
  {.Z-}

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

constructor TStListNode.Create(AData : Pointer);
begin
  inherited Create(AData);
end;

{----------------------------------------------------------------------}

function FindNode(Container : TStContainer;
                  Node : TStNode;
                  OtherData : Pointer) : Boolean; far;
begin
  Result := (Node.Data <> OtherData);
end;

function AssignData(Container : TStContainer;
                    Data, OtherData : Pointer) : Boolean; far;
  var
    OurList : TStList absolute OtherData;
  begin
    OurList.Append(Data);
    Result := true;
  end;

{----------------------------------------------------------------------}

function TStList.Append(Data : Pointer) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TStListNode(conNodeClass.Create(Data));
    N.FPrev := FTail;
    if not Assigned(FHead) then begin
      {Special case for first node}
      FHead := N;
      FTail := N;
    end else begin
      {Add at end of existing list}
      FTail.FNext := N;
      FTail := N;
    end;
    Inc(FCount);
    Result := N;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.Assign(Source: TPersistent);
  begin
    {$IFDEF ThreadSafe}
    EnterCS;
    try
    {$ENDIF}
      {The only containers that we allow to be assigned to a linked list are
         - another SysTools linked list (TStList)
         - a SysTools binary search tree (TStTree)
         - a SysTools collection (TStCollection, TStSortedCollection)}
      if not AssignPointers(Source, AssignData) then
        inherited Assign(Source);
    {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;{try..finally}
    {$ENDIF}
  end;

procedure TStList.Clear;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count > 0 then begin
      Iterate(DestroyNode, True, nil);
      FCount := 0;
    end;
    FHead := nil;
    FTail := nil;
    lsLastI := -1;
    lsLastP := nil;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

constructor TStList.Create(NodeClass : TStNodeClass);
begin
  CreateContainer(NodeClass, 0);
  Clear;
end;

procedure TStList.Delete(P : TStListNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (not Assigned(P)) or (Count <= 0) then
      Exit;
    if not (P is conNodeClass) then
      RaiseContainerError(stscBadType);

    with P do begin
      {Fix pointers of surrounding nodes}
      if Assigned(FNext) then
        FNext.FPrev := FPrev;
      if Assigned(FPrev) then
        FPrev.FNext := FNext;
    end;

    {Fix head and tail of list}
    if FTail = P then
      FTail := FTail.FPrev;
    if FHead = P then
      FHead := FHead.FNext;

    {Dispose of the node}
    DisposeNodeData(P);
    P.Free;
    Dec(FCount);
    lsLastI := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Distance(P1, P2 : TStListNode) : Integer;
var
  I : Integer;
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Count forward}
    I := 0;
    N := P1;
    while Assigned(N) and (N <> P2) do begin
      Inc(I);
      N := N.FNext;
    end;
    if N = P2 then begin
      Result := I;
      Exit;
    end;

    {Count backward}
    I := 0;
    N := P1;
    while Assigned(N) and (N <> P2) do begin
      Dec(I);
      N := N.FPrev;
    end;
    if N = P2 then begin
      Result := I;
      Exit;
    end;

    {Not on same list}
    Result := MaxLongInt;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Find(Data : Pointer) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result := Iterate(FindNode, True, Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.ForEachPointer(Action : TIteratePointerFunc;
                                 OtherData : pointer);
var
  N : TStListNode;
  P : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := FHead;
    while Assigned(N) do begin
      P := N.FNext;
      if Action(Self, N.Data, OtherData) then
        N := P
      else
        Exit;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Insert(Data : Pointer) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TStListNode(conNodeClass.Create(Data));
    {N.FPrev := nil;}
    N.FNext := FHead;
    if not Assigned(FHead) then
      {Special case for first node}
      FTail := N
    else
      {Add at start of existing list}
      FHead.FPrev := N;
    FHead := N;
    Inc(FCount);
    lsLastI := -1;
    Result := N;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.InsertSorted(Data : Pointer) : TStListNode;
var
  N : TStListNode;
  P : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TStListNode(conNodeClass.Create(Data));
    Result := N;
    Inc(FCount);
    lsLastI := -1;

    if not Assigned(FHead) then begin
      {First element added to list}
      FHead := N;
      FTail := N;
    end else begin
      P := FHead;
      while Assigned(P) do begin
        if DoCompare(N.Data, P.Data) < 0 then begin
          if not Assigned(P.FPrev) then begin
            {New head}
            FHead := N;
          end else begin
            P.FPrev.FNext := N;
            N.FPrev := P.FPrev;
          end;
          P.FPrev := N;
          N.FNext := P;
          Exit;
        end;
        P := P.FNext;
      end;
      {New tail}
      FTail.FNext := N;
      N.FPrev := FTail;
      FTail := N;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Iterate(Action : TIterateFunc; Up : Boolean;
                         OtherData : Pointer) : TStListNode;
var
  N : TStListNode;
  P : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Up then begin
      N := FHead;
      while Assigned(N) do begin
        P := N.FNext;
        if Action(Self, N, OtherData) then
          N := P
        else begin
          Result := N;
          Exit;
        end;
      end;
    end else begin
      N := FTail;
      while Assigned(N) do begin
        P := N.FPrev;
        if Action(Self, N, OtherData) then
          N := P
        else begin
          Result := N;
          Exit;
        end;
      end;
    end;
    Result := nil;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.Join(P : TStListNode; L : TStList);
var
  N : TStListNode;
  Q : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterClassCS;
  EnterCS;
  L.EnterCS;
  try
{$ENDIF}
    if Assigned(L) then begin
      if Assigned(P) and (L.Count > 0) then begin
        {Patch the list into the current one}
        N := L.Head;
        Q := P.FNext;

        P.FNext := N;
        N.FPrev := P;

        if Assigned(Q) then begin
          N := L.Tail;
          N.FNext := Q;
          Q.FPrev := N;
        end;

        Inc(FCount, L.Count);
        lsLastI := -1;
      end;

      {Free L (but not its nodes)}
      L.IncNodeProtection;
      L.Free;
    end;
{$IFDEF ThreadSafe}
  finally
    L.LeaveCS;
    LeaveCS;
    LeaveClassCS;
  end;
{$ENDIF}
end;

procedure TStList.LoadFromStream(S : TStream);
var
  Data : pointer;
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
      with Reader do
        begin
          StreamedClassName := ReadString;
          StreamedClass := GetClass(StreamedClassName);
          if (StreamedClass = nil) then
            RaiseContainerErrorFmt(stscUnknownClass, [StreamedClassName]);
          if (not IsOrInheritsFrom(StreamedClass, Self.ClassType)) or
              (not IsOrInheritsFrom(TStList, StreamedClass)) then
            RaiseContainerError(stscWrongClass);
          StreamedNodeClassName := ReadString;
          StreamedNodeClass := GetClass(StreamedNodeClassName);
          if (StreamedNodeClass = nil) then
            RaiseContainerErrorFmt(stscUnknownNodeClass, [StreamedNodeClassName]);
          if (not IsOrInheritsFrom(StreamedNodeClass, conNodeClass)) or
              (not IsOrInheritsFrom(TStListNode, StreamedNodeClass)) then
            RaiseContainerError(stscWrongNodeClass);
          ReadListBegin;
          while not EndOfList do
            begin
              Data := DoLoadData(Reader);
              Append(Data);
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

procedure TStList.MoveToHead(P : TStListNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(P) then
      if P <> Head then begin
        with P do begin
          {Fix pointers of surrounding nodes}
          if FTail = P then
            FTail := FTail.FPrev
          else
            FNext.FPrev := FPrev;
          FPrev.FNext := FNext;

          FNext := FHead;
          FPrev := nil;
        end;
        FHead.FPrev := P;
        FHead := P;
     end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Next(P : TStListNode) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result := P.FNext;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Nth(Index : Integer) : TStListNode;
var
  MinI : Integer;
  MinP : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (Index < 0) or (Index >= FCount) then
      Result := nil
    else begin
      MinI := Index;
      MinP := FHead;
      if lsLastI >= 0 then
        {scan the fewest possible nodes}
        if Index <= lsLastI then begin
          if lsLastI-Index < Index then begin
            MinI := Index-lsLastI;
            MinP := lsLastP;
          end;
        end else if Index-lsLastI < FCount-1-Index then begin
          MinI := Index-lsLastI;
          MinP := lsLastP;
        end else begin
          MinI := Index-(FCount-1);
          MinP := FTail;
        end;

      Result := NthFrom(MinP, MinI);
      lsLastI := Index;
      lsLastP := Result;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.NthFrom(P : TStListNode; Index : Integer) : TStListNode;
var
  I : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(P) then begin
      if not (P is conNodeClass) then
        RaiseContainerError(stscBadType);
      if Index > 0 then begin
        for I := 1 to Index do begin
          P := P.FNext;
          if not Assigned(P) then
            break;
        end;
      end else begin
        for I := 1 to -Index do begin
          P := P.FPrev;
          if not Assigned(P) then
            break;
        end;
      end;
    end;
    Result := P;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Place(Data : Pointer; P : TStListNode) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if not Assigned(P) then
      Result := Insert(Data)
    else if P = FTail then
      Result := Append(Data)
    else begin
      N := TStListNode(conNodeClass.Create(Data));
      N.FPrev := P;
      N.FNext := P.FNext;
      P.FNext.FPrev := N;
      P.FNext := N;
      Inc(FCount);
      lsLastI := -1;
      Result := N;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.PlaceBefore(Data : Pointer; P : TStListNode) : TStListNode;
var
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (not Assigned(P)) or (P = Head) then
      {Place the new element at the start of the list}
      Result := Insert(Data)
    else begin
      {Patch in the new element}
      N := TStListNode(conNodeClass.Create(Data));
      N.FNext := P;
      N.FPrev := P.FPrev;
      P.FPrev.FNext := N;
      P.FPrev := N;
      lsLastI := -1;
      Inc(FCount);
      Result := N;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Posn(P : TStListNode) : Integer;
var
  I : Integer;
  N : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if not Assigned(P) then
      Result := -1
    else begin
      if not (P is conNodeClass) then
        RaiseContainerError(stscBadType);
      I := 0;
      N := FHead;
      while Assigned(N) do begin
        if P = N then begin
          Result := I;
          exit;
        end;
        Inc(I);
        N := N.FNext;
      end;
      Result := -1;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Prev(P : TStListNode) : TStListNode;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Result := P.FPrev;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStList.Sort;
const
  StackSize = 32;
type
  Stack = array[0..StackSize-1] of TStListNode;
var
  L : TStListNode;
  R : TStListNode;
  PL : TStListNode;
  PR : TStListNode;
  PivotData : Pointer;
  TmpData : Pointer;
  Dist : Integer;
  DistL : Integer;
  DistR : Integer;
  StackP : Integer;
  LStack : Stack;
  RStack : Stack;
  DStack : array[0..StackSize-1] of Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    {Need at least 2 elements to sort}
    if Count <= 1 then
      Exit;
    lsLastI := -1;

    {Initialize the stacks}
    StackP := 0;
    LStack[0] := FHead;
    RStack[0] := FTail;
    DStack[0] := Count-1;

    {Repeatedly take top partition from stack}
    repeat

      {Pop the stack}
      L := LStack[StackP];
      R := RStack[StackP];
      Dist := DStack[StackP];
      Dec(StackP);

      if L <> R then
        {Sort current partition}
        repeat

          {Load the pivot element}
          PivotData := NthFrom(L, Dist div 2).Data;
          PL := L;
          PR := R;
          DistL := Dist;
          DistR := Dist;

          {Swap items in sort order around the pivot index}
          repeat
            while DoCompare(PL.Data, PivotData) < 0 do begin
              PL := PL.FNext;
              Dec(Dist);
              Dec(DistR);
            end;
            while DoCompare(PivotData, PR.Data) < 0 do begin
              PR := PR.FPrev;
              Dec(Dist);
              Dec(DistL);
            end;
            if Dist >= 0 then begin
              if PL <> PR then begin
                {Swap the two elements}
                TmpData := PL.Data;
                PL.Data := PR.Data;
                PR.Data := TmpData;
              end;
              if Assigned(PL.FNext) then begin
                PL := PL.FNext;
                Dec(Dist);
                Dec(DistR);
              end;
              if Assigned(PR.FPrev) then begin
                PR := PR.FPrev;
                Dec(Dist);
                Dec(DistL);
              end;
            end;
          until Dist < 0;

          {Decide which partition to sort next}
          if DistL < DistR then begin
            {Right partition is bigger}
            if DistR > 0 then begin
              {Stack the request for sorting right partition}
              Inc(StackP);
              LStack[StackP] := PL;
              RStack[StackP] := R;
              DStack[StackP] := DistR;
            end;
            {Continue sorting left partition}
            R := PR;
            Dist := DistL;
          end else begin
            {Left partition is bigger}
            if DistL > 0 then begin
              {Stack the request for sorting left partition}
              Inc(StackP);
              LStack[StackP] := L;
              RStack[StackP] := PR;
              DStack[StackP] := DistL;
            end;
            {Continue sorting right partition}
            L := PL;
            Dist := DistR;
          end;

        until Dist <= 0;
    until StackP < 0;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.Split(P : TStListNode) : TStList;
var
  I : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    I := Posn(P);
    if I < 0 then begin
      Result := nil;
      Exit;
    end;

    {Create and initialize the new list}
    Result := TStListClass(ClassType).Create(conNodeClass);
    Result.Compare := Compare;
    Result.OnCompare := OnCompare;
    Result.DisposeData := DisposeData;
    Result.OnDisposeData := OnDisposeData;
    Result.LoadData := LoadData;
    Result.OnLoadData := OnLoadData;
    Result.StoreData := StoreData;
    Result.OnStoreData := OnStoreData;
    Result.FHead := P;
    Result.FTail := FTail;
    Result.FCount := Count-I;
    Result.lsLastI := -1;

    {Truncate the old list}
    if Assigned(P.FPrev) then begin
      P.FPrev.FNext := nil;
      FTail := P.FPrev;
      P.FPrev := nil;
    end;
    if P = FHead then
      FHead := nil;
    FCount := I;
    lsLastI := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStList.StoresPointers : Boolean;
begin
  Result := true;
end;

procedure TStList.StoreToStream(S : TStream);
var
  Writer : TWriter;
  Walker : TStListNode;
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
          WriteString(conNodeClass.ClassName);
          WriteListBegin;
          Walker := Head;
          while Walker <> nil do
            begin
              DoStoreData(Writer, Walker.Data);
              Walker := Next(Walker);
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

{$IFDEF ThreadSafe}
initialization
  Windows.InitializeCriticalSection(ClassCritSect);
finalization
  Windows.DeleteCriticalSection(ClassCritSect);
{$ENDIF}
end.
