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
{* SysTools: StDQue.pas 4.04                             *}
{*********************************************************}
{* SysTools: DEQue class                                 *}
{*********************************************************}

{$I StDefine.inc}

{Notes:
   This class is derived from TStList and allows all of
   the inherited list methods to be used.

   The "head" of the queue is element 0 in the list. The "tail" of the
   queue is the last element in the list.

   The dequeue can be used as a LIFO stack by calling PushTail and
   PopTail, or as a FIFO queue by calling PushTail and PopHead.
}

unit StDQue;

interface

uses
  Windows,
  STConst, StBase, StList;

type
  TStDQue = class(TStList)
    public
      procedure PushTail(Data : Pointer);
        {-Add element at tail of queue}
      procedure PopTail;
        {-Delete element at tail of queue, destroys its data}
      procedure PeekTail(var Data : Pointer);
        {-Return data at tail of queue}

      procedure PushHead(Data : Pointer);
        {-Add element at head of queue}
      procedure PopHead;
        {-Delete element at head of queue, destroys its data}
      procedure PeekHead(var Data : Pointer);
        {-Return data at head of queue}
  end;

{======================================================================}

implementation



procedure TStDQue.PeekHead(var Data : Pointer);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      Data := nil
    else
      Data := Head.Data;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue.PeekTail(var Data : Pointer);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count = 0 then
      Data := nil
    else
      Data := Tail.Data;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue.PopHead;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count > 0 then
      Delete(Head);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue.PopTail;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Count > 0 then
      Delete(Tail);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue.PushHead(Data : Pointer);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Insert(Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

procedure TStDQue.PushTail(Data : Pointer);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Append(Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;


end.
