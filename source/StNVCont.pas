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
{* SysTools: StNVCont.pas 4.04                           *}
{*********************************************************}
{* SysTools: non visual components for container classes *}
{*********************************************************}

{$I StDefine.inc}

unit StNVCont;

interface

uses
  Windows,
  Classes,
  StBase, StBits;

type
  TStContainerClass = class of TStContainer;

  TStDisposeDataEvent = procedure(Sender : TObject; Data : Pointer)
    of object;
  TStLoadDataEvent = procedure(Sender : TObject; Reader : TReader; var Data : Pointer)
    of object;
  TStStoreDataEvent = procedure(Sender : TObject; Writer : TWriter; Data : Pointer)
    of object;

  {.Z+}
  TStNVContainerBase = class(TStComponent)
  protected
    {virtual property methods}
    function GetOnCompare : TStCompareEvent;
      virtual;
    function GetOnDisposeData : TStDisposeDataEvent;
      virtual;
    function GetOnLoadData : TStLoadDataEvent;
      virtual;
    function GetOnStoreData : TStStoreDataEvent;
      virtual;
    procedure SetOnCompare(Value : TStCompareEvent);
      virtual;
    procedure SetOnDisposeData(Value : TStDisposeDataEvent);
      virtual;
    procedure SetOnLoadData(Value : TStLoadDataEvent);
      virtual;
    procedure SetOnStoreData(Value : TStStoreDataEvent);
      virtual;

    {events}
    property OnCompare : TStCompareEvent
      read GetOnCompare
      write SetOnCompare;

    property OnDisposeData : TStDisposeDataEvent
      read GetOnDisposeData
      write SetOnDisposeData;

    property OnLoadData : TStLoadDataEvent
      read GetOnLoadData
      write SetOnLoadData;

    property OnStoreData : TStStoreDataEvent
      read GetOnStoreData
      write SetOnStoreData;
  end;
  {.Z-}


implementation



{*** TStNVContainerBase ***}

function TStNVContainerBase.GetOnCompare : TStCompareEvent;
begin
end;

function TStNVContainerBase.GetOnDisposeData : TStDisposeDataEvent;
begin
end;

function TStNVContainerBase.GetOnLoadData : TStLoadDataEvent;
begin
end;

function TStNVContainerBase.GetOnStoreData : TStStoreDataEvent;
begin
end;

procedure TStNVContainerBase.SetOnCompare(Value : TStCompareEvent);
begin
end;

procedure TStNVContainerBase.SetOnDisposeData(Value : TStDisposeDataEvent);
begin
end;

procedure TStNVContainerBase.SetOnLoadData(Value : TStLoadDataEvent);
begin
end;

procedure TStNVContainerBase.SetOnStoreData(Value : TStStoreDataEvent);
begin
end;


end.
