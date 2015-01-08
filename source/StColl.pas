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
{* SysTools: StColl.pas 4.04                             *}
{*********************************************************}
{* SysTools: Huge, sparse collection class               *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
  - STCOLL generally follows the standards set by Borland's TP6
    TCollection. All elements in the collection are pointers. Elements can
    be inserted, deleted, and accessed by index number. The size of the
    collection grows dynamically as needed. However, STCOLL is implemented
    in a different fashion that gives it more capacity and higher
    efficiency in some ways.

  - STCOLL theoretically allows up to 2 billion elements. The collection
    is "sparse" in the sense that most of the memory is allocated only
    when a value is assigned to an element in the collection.

  - STCOLL is implemented as a linked list of pointers to pages. Each
    page can hold a fixed number of collection elements, the size
    being specified when the TStCollection is created. Only when an
    element with a given index is written to is a page descriptor and a
    page allocated for it.  However, the first page is allocated when the
    collection is created.

  - The larger the page size, the faster it is to access a given index
    and the less memory overhead is used for management of the collection.
    If the page size is at least as large as the number of elements added
    to the collection, TStCollection works just like Borland's old
    TCollection.  Inserting elements in the middle of very large pages can
    be slow, however, because lots of data must be shifted to make room
    for each new element. Conversely, if the page size is 1, TStCollection
    acts much like a traditional linked list.

  - The page size is limited to 16380 elements in 16-bit mode, or
    536 million elements in 32-bit mode.

  - STCOLL uses the DisposeData procedure of TStContainer to determine
    how to free elements in the collection. By default, it does nothing.

  - AtFree and Free do not exist in TStCollection. Instead the AtDelete
    and Delete methods will also dispose of the element if the DisposeData
    property of the class has been set.

  - The Count property returns the index (plus one) of the highest
    element inserted or put.

  - AtInsert can insert an item at any index, even larger than Count+1.
    AtPut also can put an item at any index.

  - If the At function is called for any non-negative index whose value
    has not been explicitly assigned using Insert or AtInsert, it returns
    nil.

  - For the non-sorted collection, IndexOf compares the data pointers
    directly, for exact equality, without using any Comparison function.

  - TStSortedCollection allows duplicate nodes only if its Duplicates
    property is set.

  - The Efficiency property returns a measure of how fully the collection
    is using the memory pages it has allocated. It returns a number in the
    range of 0 to 100 (percent). Calling TStSortedCollection.Insert,
    AtInsert, Delete, or AtDelete can result in a low efficiency. After a
    series of calls to these methods it is often worthwhile to call the
    Pack method to increase the efficiency as much as possible.
}

unit StColl;
{-}

interface

uses
  Windows, Classes,
  
  StConst, StBase, StList;

type
  {.Z+}
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..(StMaxBlockSize div SizeOf(Pointer))-1] of Pointer;

  TPageDescriptor = class(TStListNode)
  protected
    {PageElements count is stored in inherited Data field}
    pdPage  : PPointerArray; {Pointer to page data}
    pdStart : LongInt;       {Index of first element in page}
    pdCount : Integer;       {Number of elements used in page}

  public
    constructor Create(AData : Pointer); override;
    destructor Destroy; override;
  end;
  {.Z-}

  TCollIterateFunc = function (Container : TStContainer;
                               Data : Pointer;
                               OtherData : Pointer) : Boolean;

  TStCollection = class(TStContainer)
  {.Z+}
  protected
    colPageList : TStList;      {List of page descriptors}
    colPageElements : Integer;  {Number of elements in a page}
    colCachePage : TPageDescriptor; {Page last found by At}

    procedure colAdjustPagesAfter(N : TPageDescriptor; Delta : LongInt);
    procedure colAtInsertInPage(N : TPageDescriptor; PageIndex : Integer;
                                AData : Pointer);
    procedure colAtDeleteInPage(N : TPageDescriptor; PageIndex : Integer);
    function colGetCount : LongInt;
    function colGetEfficiency : Integer;

    procedure ForEachPointer(Action : TIteratePointerFunc; OtherData : pointer);
      override;
    function StoresPointers : boolean;
      override;
  {.Z-}
  public
    constructor Create(PageElements : Integer); virtual;
      {-Initialize a collection with given page size and allocate first page}
    destructor Destroy; override;
      {-Free a collection}

    procedure LoadFromStream(S : TStream); override;
      {-Load a collection's data from a stream}
    procedure StoreToStream(S : TStream); override;
      {-Write a collection and its data to a stream}

    procedure Clear; override;
      {-Deallocate all pages and free all items}
    procedure Assign(Source: TPersistent); override;
      {-Assign another container's contents to this one}
    procedure Pack;
      {-Squeeze collection elements into the least memory possible}

    function At(Index : LongInt) : Pointer;
      {-Return the element at a given index}
    function IndexOf(Data : Pointer) : LongInt; virtual;
      {-Return the index of the first item with given data}

    procedure AtInsert(Index : LongInt; Data : Pointer);
      {-Insert a new element at a given index and move following items down}
    procedure AtPut(Index : LongInt; Data : Pointer);
      {-Replace element at given index with new data}
    procedure Insert(Data : Pointer); virtual;
      {-Insert item at the end of the collection}

    procedure AtDelete(Index : LongInt);
      {-Remove element at a given index, move following items up, free element}
    procedure Delete(Data : Pointer);
      {-Delete the first item with the given data}

    function Iterate(Action : TCollIterateFunc; Up : Boolean;
                     OtherData : Pointer) : Pointer;
      {-Call Action for all the non-nil elements, returning the last data}

    property Count : LongInt
      {-Return the index of the highest assigned item, plus one}
      read colGetCount;

    property Efficiency : Integer
      {-Return the overall percent Efficiency of the pages}
      read colGetEfficiency;

    property Items[Index : LongInt] : Pointer
      {-Return the Index'th node, 0-based}
      read At
      write AtPut;
      default;
  end;

  {.Z+}
  TSCSearch = (SCSPageEmpty,
               SCSLessThanThisPage,
               SCSInThisPageRange,
               SCSFound,
               SCSGreaterThanThisPage);
  {.Z-}

  TStSortedCollection = class(TStCollection)
  {.Z+}
  protected
    FDuplicates : Boolean;

    function scSearchPage(AData : Pointer; N : TPageDescriptor;
                          var PageIndex : Integer) : TSCSearch;

    procedure scSetDuplicates(D : Boolean);
  {.Z-}
  public
    procedure LoadFromStream(S : TStream); override;
      {-Load a sorted collection's data from a stream}
    procedure StoreToStream(S : TStream); override;
     {-Write a collection and its data to a stream}

    function IndexOf(Data : Pointer) : LongInt; override;
      {-Return the index of the first item with given data}
    procedure Insert(Data : Pointer); override;
      {-Insert item in sorted position}
    property Duplicates : Boolean
      {-Determine whether sorted collection allows duplicate data}
    read FDuplicates
    write scSetDuplicates;
  end;

{======================================================================}

implementation

function AssignData(Container : TStContainer;
                    Data, OtherData : Pointer) : Boolean; far;
  var
    OurColl : TStCollection absolute OtherData;
  begin
    OurColl.Insert(Data);
    Result := true;
  end;

constructor TPageDescriptor.Create(AData : Pointer);
begin
  inherited Create(AData);
  GetMem(pdPage, LongInt(Data)*SizeOf(Pointer));
  FillChar(pdPage^, LongInt(Data)*SizeOf(Pointer), 0);
end;

destructor TPageDescriptor.Destroy;
begin
  if Assigned(pdPage) then
    FreeMem(pdPage, LongInt(Data)*SizeOf(Pointer));
  inherited Destroy;
end;

{----------------------------------------------------------------------}

procedure TStCollection.Assign(Source: TPersistent);
  begin
    {$IFDEF ThreadSafe}
    EnterCS;
    try
    {$ENDIF}
      {The only containers that we allow to be assigned to a collection are
         - a SysTools linked list (TStList)
         - a SysTools binary search tree (TStTree)
         - another SysTools collection (TStCollection, TStSortedCollection)}
      if not AssignPointers(Source, AssignData) then
        inherited Assign(Source);
    {$IFDEF ThreadSafe}
    finally
      LeaveCS;
    end;{try..finally}
    {$ENDIF}
  end;

function TStCollection.At(Index : LongInt) : Pointer;
var
  Start : LongInt;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Index < 0 then
      RaiseContainerError(stscBadIndex);

    N := colCachePage;
    if Index >= N.pdStart then
      {search up}
      repeat
        with N do begin
          Start := pdStart;
          if Index < Start then begin
            {element has not been set}
            colCachePage := N;
            break;
          end else if Index < Start+pdCount then begin
            {element is in this page}
            colCachePage := N;
            Result := pdPage^[Index-Start];
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N)

    else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        with N do begin
          Start := pdStart;
          if (Index >= Start+pdCount) then begin
            {element has not been set}
            colCachePage := N;
            break;
          end else if Index >= Start then begin
            {element is in this page}
            colCachePage := N;
            Result := pdPage^[Index-Start];
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    {not found, leave cache page unchanged}
    Result := nil;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.AtDelete(Index : LongInt);
var
  Start : LongInt;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Index < 0 then
      RaiseContainerError(stscBadIndex);

    N := colCachePage;
    if Index >= N.pdStart then
      repeat
        with N do begin
          Start := pdStart;
          if Index < Start then begin
            {element has not been set, nothing to free}
            Dec(pdStart);
            colAdjustPagesAfter(N, -1);
            colCachePage := N;
            Exit;
          end else if Index < Start+pdCount then begin
            {element is in this page}
            colCachePage := N;
            colAtDeleteInPage(N, Index-Start);
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N)

    else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        with N do begin
          Start := pdStart;
          if Index >= Start+pdCount then begin
            {element has not been set, nothing to free}
            Dec(pdStart);
            colAdjustPagesAfter(N, -1);
            colCachePage := N;
            Exit;
          end else if Index >= Start then begin
            {element is in this page}
            colCachePage := N;
            colAtDeleteInPage(N, Index-Start);
            Exit;
          end;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    {index not found, nothing to delete}
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.AtInsert(Index : LongInt; Data : Pointer);
var
  Start : LongInt;
  NC : Integer;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
  if Index < 0 then
      RaiseContainerError(stscBadIndex);

    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      Start := N.pdStart;
      if Index < Start then begin
        {current page has indexes greater than the specified one}
        if Start-Index <= colPageElements-N.pdCount then begin
          {room to squeeze element into this page}
          NC := Start-Index;
          Move(N.pdPage^[0], N.pdPage^[NC], N.pdCount*SizeOf(Pointer));
          FillChar(N.pdPage^[1], (NC-1)*SizeOf(Pointer), 0);
          Inc(N.pdCount, NC);
        end else begin
          {insert on a new page before this one}
          N := TPageDescriptor(colPageList.PlaceBefore(Pointer(colPageElements), N));
          N.pdCount := 1;
        end;
        N.pdStart := Index;
        N.pdPage^[0] := Data;
        colAdjustPagesAfter(N, +1);
        Exit;
      end else if Index < Start+colPageElements then
        if (not Assigned(N.FNext)) or (Index < TPageDescriptor(N.FNext).pdStart) then begin
          {should be inserted on this page}
          colAtInsertInPage(N, Index-Start, Data);
          Exit;
        end;
      N := TPageDescriptor(N.FNext);
    end;

    {should be inserted after all existing pages}
    N := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
    N.pdStart := Index;
    N.pdCount := 1;
    N.pdPage^[0] := Data;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.AtPut(Index : LongInt; Data : Pointer);
var
  Start : LongInt;
  N, T : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Index < 0 then
      RaiseContainerError(stscBadIndex);

    {special case for putting to end of collection}
    T := TPageDescriptor(colPageList.Tail);
    if Index = T.pdStart+T.pdCount then begin
      if T.pdCount >= colPageElements then begin
        {last page is full, add another}
        Start := T.pdStart+colPageElements;
        T := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
        T.pdStart := Start;
        {T.pdCount := 0;}
      end;
      T.pdPage^[T.pdCount] := Data;
      inc(T.pdCount);
      Exit;
    end;

    N := colCachePage;
    if Index >= N.pdStart then
      {search up}
      repeat
        Start := N.pdStart;
        if Index < Start then begin
          {element has not been set before}
          N := TPageDescriptor(colPageList.PlaceBefore(Pointer(colPageElements), N));
          N.pdStart := Index;
          N.pdCount := 1;
          N.pdPage^[0] := Data;
          colCachePage := N;
          Exit;
        end else if Index < Start+N.pdCount then begin
          {element fits in this page}
          colCachePage := N;
          N.pdPage^[Index-Start] := Data;
          Exit;
        end else if (N = T) and (Index < Start+colPageElements) then begin
          {element fits in last page}
          colCachePage := N;
          N.pdPage^[Index-Start] := Data;
          N.pdCount := Index-Start+1;
          Exit;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N)

    else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        Start := N.pdStart;
        if (Index >= Start+N.pdCount) then begin
          {element has not been set before}
          N := TPageDescriptor(colPageList.PlaceBefore(Pointer(colPageElements), N));
          N.pdStart := Index;
          N.pdCount := 1;
          N.pdPage^[0] := Data;
          colCachePage := N;
          Exit;
        end else if Index >= Start then begin
          {element is in this page}
          colCachePage := N;
          N.pdPage^[Index-Start] := Data;
          Exit;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    {an element after all existing ones}
    N := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
    colCachePage := N;
    N.pdStart := Index;
    N.pdCount := 1;
    N.pdPage^[0] := Data;
    Exit;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.Clear;
var
  I : Integer;
  N, P : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TPageDescriptor(colPageList.Head);
    colCachePage := N;
    while Assigned(N) do begin
      for I := 0 to N.pdCount-1 do
        DoDisposeData(N.pdPage^[I]);
      P := TPageDescriptor(N.FNext);
      if N = colCachePage then begin
        {keep the first page, which is now empty}
        N.pdCount := 0;
        N.pdStart := 0;
      end else
        {delete all other pages}
        colPageList.Delete(N);
      N := P;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.colAdjustPagesAfter(N : TPageDescriptor; Delta : LongInt);
begin
  N := TPageDescriptor(N.FNext);
  while Assigned(N) do begin
    inc(N.pdStart, Delta);
    N := TPageDescriptor(N.FNext);
  end;
end;

procedure TStCollection.colAtDeleteInPage(N : TPageDescriptor; PageIndex : Integer);
begin
  with N do begin
    {free the element}
    DoDisposeData(pdPage^[PageIndex]);
    Move(pdPage^[PageIndex+1], pdPage^[PageIndex],
         (colPageElements-PageIndex-1)*SizeOf(Pointer));
    Dec(pdCount);
    colAdjustPagesAfter(N, -1);
    if (pdCount = 0) and (colPageList.Count > 1) then begin
      {delete page if at least one page will remain}
      if N = colCachePage then begin                           
        colCachePage := TPageDescriptor(colPageList.Head);
        if N = colCachePage then                               
          colCachePage := TPageDescriptor(N.FNext);            
      end;
      colPageList.Delete(N);
    end;
  end;
end;

procedure TStCollection.colAtInsertInPage(N : TPageDescriptor; PageIndex : Integer;
                                       AData : Pointer);
var
  P : TPageDescriptor;
  PC : Integer;
begin
  with N do
    if pdCount >= colPageElements then begin
      {page is full, add another}
      P := TPageDescriptor(colPageList.Place(Pointer(colPageElements), N));
      {new page starts with element after the new one}
      P.pdStart := pdStart+PageIndex+1;
      PC := colPageElements-PageIndex;
      Move(pdPage^[PageIndex], P.pdPage^[0], PC*SizeOf(Pointer));
      pdPage^[PageIndex] := AData;
      pdCount := PageIndex+1;
      P.pdCount := PC;
      colAdjustPagesAfter(P, +1);
    end else begin
      {room to add on this page}
      if pdCount > PageIndex then begin
        Move(pdPage^[PageIndex], pdPage^[PageIndex+1], (pdCount-PageIndex)*SizeOf(Pointer));
        colAdjustPagesAfter(N, +1);
        inc(pdCount);
      end else begin
        FillChar(pdPage^[pdCount], (PageIndex-pdCount)*SizeOf(Pointer), 0);
        colAdjustPagesAfter(N, PageIndex+1-pdCount);
        pdCount := PageIndex+1;
      end;
      pdPage^[PageIndex] := AData;
    end;
end;

function TStCollection.colGetCount : LongInt;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    with TPageDescriptor(colPageList.Tail) do
      Result := pdStart+pdCount;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStCollection.colGetEfficiency : Integer;
var
  Pages, ECount : LongInt;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    ECount := 0;
    Pages := 0;
    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      with N do begin
        inc(Pages);
        inc(ECount, N.pdCount);
      end;
      N := TPageDescriptor(N.FNext);
    end;
    Result := (100*ECount) div (Pages*colPageElements);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.ForEachPointer(Action : TIteratePointerFunc;
                                       OtherData : pointer);
var
  I : Integer;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      with N do
        for I := 0 to pdCount-1 do
          if (pdPage^[I] <> nil) then
            if not Action(Self, pdPage^[I], OtherData) then begin
              Exit;
            end;
      N := TPageDescriptor(N.FNext);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStCollection.StoresPointers : boolean;
begin
  Result := true;
end;

constructor TStCollection.Create(PageElements : Integer);
begin
  CreateContainer(TStNode, 0);

  if (PageElements = 0) then
    RaiseContainerError(stscBadSize);

  colPageList := TStList.Create(TPageDescriptor);
  colPageElements := PageElements;

  {start with one empty page}
  colPageList.Append(Pointer(colPageElements));
  colCachePage := TPageDescriptor(colPageList.Head);
end;

procedure TStCollection.Delete(Data : Pointer);
var
  Index : LongInt;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Index := IndexOf(Data);
    if Index >= 0 then
      AtDelete(Index);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

destructor TStCollection.Destroy;
begin
  Clear;
  colPageList.Free;
  IncNodeProtection;
  inherited Destroy;
end;

function TStCollection.IndexOf(Data : Pointer) : LongInt;
var
  I : LongInt;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      for I := 0 to N.pdCount-1 do
        if N.pdPage^[I] = Data then begin
          colCachePage := N;
          Result := N.pdStart+I;
          Exit;
        end;
      N := TPageDescriptor(N.FNext);
    end;
    IndexOf := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.Insert(Data : Pointer);
var
  Start : LongInt;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TPageDescriptor(colPageList.Tail);
    if N.pdCount >= colPageElements then begin
      {last page is full, add another}
      Start := N.pdStart+colPageElements;
      N := TPageDescriptor(colPageList.Append(Pointer(colPageElements)));
      N.pdStart := Start;
      {N.pdCount := 0;}
    end;
    N.pdPage^[N.pdCount] := Data;
    inc(N.pdCount);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStCollection.Iterate(Action : TCollIterateFunc; Up : Boolean;
                               OtherData : Pointer) : Pointer;
var
  I : Integer;
  N : TPageDescriptor;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Up then begin
      N := TPageDescriptor(colPageList.Head);
      while Assigned(N) do begin
        with N do
          for I := 0 to pdCount-1 do
            if (pdPage^[I] <> nil) then
              if not Action(Self, pdPage^[I], OtherData) then begin
                Result := pdPage^[I];
                Exit;
              end;
        N := TPageDescriptor(N.FNext);
      end;
    end else begin
      N := TPageDescriptor(colPageList.Tail);
      while Assigned(N) do begin
        with N do
          for I := pdCount-1 downto 0 do
            if (pdPage^[I] <> nil) then
              if not Action(Self, pdPage^[I], OtherData) then begin
                Result := pdPage^[I];
                Exit;
              end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    Result := nil;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.Pack;
var
  N, P : TPageDescriptor;
  NC : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    colCachePage := TPageDescriptor(colPageList.Head);
    N := colCachePage;
    while Assigned(N) do begin
      while Assigned(N.FNext) and (N.pdCount < colPageElements) do begin
        {there is a page beyond this page and room to add to this page}
        P := TPageDescriptor(N.FNext);
        if N.pdStart+N.pdCount = P.pdStart then begin
          {next page has contiguous elements}
          NC := colPageElements-N.pdCount;
          if NC > P.pdCount then
            NC := P.pdCount;
          move(P.pdPage^[0], N.pdPage^[N.pdCount], NC*SizeOf(Pointer));
          move(P.pdPage^[NC], P.pdPage^[0], (P.pdCount-NC)*SizeOf(Pointer));
          inc(N.pdCount, NC);
          dec(P.pdCount, NC);
          if P.pdCount = 0 then
            colPageList.Delete(P)
          else
            inc(P.pdStart, NC);
        end else
          {pages aren't contiguous, can't merge}
          break;
      end;
      N := TPageDescriptor(N.FNext);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStCollection.LoadFromStream(S : TStream);
var
  Data         : pointer;
  Reader       : TReader;
  PageElements : integer;
  Index        : longint;
  StreamedClass : TPersistentClass;
  StreamedClassName : string;
begin
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
           (not IsOrInheritsFrom(TStCollection, StreamedClass)) then
          RaiseContainerError(stscWrongClass);
        PageElements := ReadInteger;
        if (PageElements <> colPageElements) then
          begin
            colPageList.Clear;
            colPageElements := PageElements;
            colPageList.Append(Pointer(colPageElements));
            colCachePage := TPageDescriptor(colPageList.Head);
          end;
        ReadListBegin;
        while not EndOfList do
          begin
            Index := ReadInteger;
            Data := DoLoadData(Reader);
            AtPut(Index, Data);
          end;
        ReadListEnd;
      end;
  finally
    Reader.Free;
  end;
end;

procedure TStCollection.StoreToStream(S : TStream);
var
  Writer : TWriter;
  N      : TPageDescriptor;
  i      : integer;
begin
  Writer := TWriter.Create(S, 1024);
  try
    with Writer do
      begin
        WriteString(Self.ClassName);
        WriteInteger(colPageElements);
        WriteListBegin;
        N := TPageDescriptor(colPageList.Head);
        while Assigned(N) do
          begin
            with N do
              for i := 0 to pdCount-1 do
                if (pdPage^[i] <> nil) then
                  begin
                    WriteInteger(pdStart + i);
                    DoStoreData(Writer, pdPage^[i]);
                  end;
            N := TPageDescriptor(N.FNext);
          end;
        WriteListEnd;
      end;
  finally
    Writer.Free;
  end;
end;

{----------------------------------------------------------------------}

function TStSortedCollection.IndexOf(Data : Pointer) : LongInt;
var
  N : TPageDescriptor;
  PageIndex : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (Count = 0) then begin
      Result := -1;
      Exit;
    end;
    N := colCachePage;
    if DoCompare(Data, N.pdPage^[0]) >= 0 then begin
      {search up}
      repeat
        case scSearchPage(Data, N, PageIndex) of
          SCSFound :
            begin
              colCachePage := N;
              Result := N.pdStart+PageIndex;
              Exit;
            end;
          SCSGreaterThanThisPage :
            {keep on searching} ;
        else
          {can't be anywhere else in the collection}
          break;
        end;
        N := TPageDescriptor(N.FNext);
      until not Assigned(N);

    end else begin
      {search down}
      N := TPageDescriptor(N.FPrev);
      while Assigned(N) do begin
        case scSearchPage(Data, N, PageIndex) of
          SCSFound :
            begin
              colCachePage := N;
              Result := N.pdStart+PageIndex;
              Exit;
            end;
          SCSLessThanThisPage :
            {keep on searching} ;
        else
          {can't be anywhere else in the collection}
          break;
        end;
        N := TPageDescriptor(N.FPrev);
      end;
    end;

    Result := -1;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStSortedCollection.Insert(Data : Pointer);
var
  N : TPageDescriptor;
  PageIndex : Integer;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    N := TPageDescriptor(colPageList.Head);
    while Assigned(N) do begin
      case scSearchPage(Data, N, PageIndex) of
        SCSPageEmpty, SCSInThisPageRange, SCSLessThanThisPage :
          begin
            colAtInsertInPage(N, PageIndex, Data);
            Exit;
          end;
        SCSFound :
          if FDuplicates then begin
            colAtInsertInPage(N, PageIndex, Data);
            Exit;
          end else
          RaiseContainerError(stscDupNode);
      end;
      N := TPageDescriptor(N.FNext);
    end;

    {greater than all other items}
    inherited Insert(Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStSortedCollection.scSearchPage(AData : Pointer; N : TPageDescriptor;
                                          var PageIndex : Integer) : TSCSearch;
var
  L, R, M, Comp : Integer;
begin
  with N do
    if pdCount = 0 then begin
      Result := SCSPageEmpty;
      PageIndex := 0;
    end else if DoCompare(AData, pdPage^[0]) < 0 then begin
      Result := SCSLessThanThisPage;
      PageIndex := 0;
    end else if DoCompare(AData, pdPage^[pdCount-1]) > 0 then
      Result := SCSGreaterThanThisPage
    else begin
      {data might be in this page, check using binary search}
      Result := SCSInThisPageRange;
      L := 0;
      R := pdCount-1;
      repeat
        M := (L+R) div 2;
        Comp := DoCompare(AData, pdPage^[M]);
        if Comp > 0 then
          L := M+1
        else begin
          R := M-1;
        if Comp = 0 then begin
          PageIndex := M;
          Result := SCSFound;
            if not FDuplicates then
              {force exit from repeat loop}
              L := M;
            {else loop to find first of a group of duplicate nodes}
          end;
        end;
      until L > R;

      if Result = SCSInThisPageRange then begin
      {not found in page, return where it would be inserted}
      PageIndex := M;
      if Comp > 0 then
        inc(PageIndex);
    end;
end;
end;

procedure TStSortedCollection.scSetDuplicates(D : Boolean);
begin
  if FDuplicates <> D then
    if D then
      FDuplicates := True
    else if FCount <> 0 then
      RaiseContainerError(stscBadDups)
    else
      FDuplicates := False;
end;

procedure TStSortedCollection.LoadFromStream(S : TStream);
var
  Data         : pointer;
  Reader       : TReader;
  PageElements : integer;
  StreamedClass : TPersistentClass;
  StreamedClassName : string;
begin
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
           (not IsOrInheritsFrom(TStCollection, StreamedClass)) then
          RaiseContainerError(stscWrongClass);
        PageElements := ReadInteger;
        if (PageElements <> colPageElements) then
          begin
            colPageList.Clear;
            colPageElements := PageElements;
            colPageList.Append(Pointer(colPageElements));
            colCachePage := TPageDescriptor(colPageList.Head);
          end;
      FDuplicates := ReadBoolean;
        ReadListBegin;
        while not EndOfList do
          begin
            ReadInteger; {read & discard index number}
            Data := DoLoadData(Reader);
            Insert(Data);
          end;
        ReadListEnd;
      end;
  finally
    Reader.Free;
  end;
end;

procedure TStSortedCollection.StoreToStream(S : TStream);
var
  Writer : TWriter;
  N      : TPageDescriptor;
  i      : integer;
begin
  Writer := TWriter.Create(S, 1024);
  try
    with Writer do
      begin
        WriteString(Self.ClassName);
        WriteInteger(colPageElements);
        WriteBoolean(FDuplicates);
        WriteListBegin;
        N := TPageDescriptor(colPageList.Head);
        while Assigned(N) do
          begin
            with N do
              for i := 0 to pdCount-1 do
                if (pdPage^[i] <> nil) then
                  begin
                    WriteInteger(pdStart + i);
                    DoStoreData(Writer, pdPage^[i]);
                  end;
            N := TPageDescriptor(N.FNext);
          end;
        WriteListEnd;
      end;
  finally
    Writer.Free;
  end;
end;


end.
