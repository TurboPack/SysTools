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
{* SysTools: StNVLAry.pas 4.04                           *}
{*********************************************************}
{* SysTools: non visual component for TStLArray          *}
{*********************************************************}

{$I StDefine.inc}

unit StNVLAry;

interface

uses
  Windows,
  Classes,
  StBase, StLArr, StNVCont;

type
  TStNVLArray = class(TStNVContainerBase)
  {.Z+}
  protected {private}
    {property variables}
    FContainer        : TStLArray; {instance of the container}
    FElementCount     : LongInt;
    FElementSize      : Cardinal;

    {property methods}
    function GetStoreable : Boolean;
    procedure SetElementCount(Value : LongInt);
    procedure SetElementSize(Value : Cardinal);
    procedure SetStoreable(Value : Boolean);

    {internal methods}
    procedure RecreateContainer;

  protected
    function GetOnCompare : TStCompareEvent;
      override;
    function GetOnDisposeData : TStDisposeDataEvent;
      override;
    function GetOnLoadData : TStLoadDataEvent;
      override;
    function GetOnStoreData : TStStoreDataEvent;
      override;
    procedure SetOnCompare(Value : TStCompareEvent);
      override;
    procedure SetOnDisposeData(Value : TStDisposeDataEvent);
      override;
    procedure SetOnLoadData(Value : TStLoadDataEvent);
      override;
    procedure SetOnStoreData(Value : TStStoreDataEvent);
      override;

  public
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
  {.Z-}

    property Container : TStLArray
      read FContainer;

  published
    property ElementCount : LongInt
      read FElementCount
      write SetElementCount default 10;

    property ElementSize : Cardinal
      read FElementSize
      write SetElementSize default SizeOf(LongInt);

    property ElementsStorable : Boolean
      read GetStoreable
      write SetStoreable default False;

    property OnCompare;
    property OnDisposeData;
    property OnLoadData;
    property OnStoreData;
  end;


implementation

{*** TStNVLArray ***}

constructor TStNVLArray.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {defaults}
  FElementCount := 10;
  FElementSize := SizeOf(LongInt);

  if Classes.GetClass(TStLArray.ClassName) = nil then
    RegisterClass(TStLArray);

  FContainer := TStLArray.Create(FElementCount, FElementSize);
end;

destructor TStNVLArray.Destroy;
begin
  FContainer.Free;
  FContainer := nil;

  inherited Destroy;
end;

function TStNVLArray.GetOnCompare : TStCompareEvent;
begin
  Result := FContainer.OnCompare;
end;

function TStNVLArray.GetOnDisposeData : TStDisposeDataEvent;
begin
  Result := FContainer.OnDisposeData;
end;

function TStNVLArray.GetOnLoadData : TStLoadDataEvent;
begin
  Result := FContainer.OnLoadData;
end;

function TStNVLArray.GetOnStoreData : TStStoreDataEvent;
begin
  Result := FContainer.OnStoreData;
end;

function TStNVLArray.GetStoreable : Boolean;
begin
  Result := FContainer.ElementsStorable;
end;

procedure TStNVLArray.RecreateContainer;
var
  HoldOnCompare     : TStCompareEvent;
  HoldOnDisposeData : TStDisposeDataEvent;
  HoldOnLoadData    : TStLoadDataEvent;
  HoldOnStoreData   : TStStoreDataEvent;
begin
  HoldOnCompare := FContainer.OnCompare;
  HoldOnDisposeData := FContainer.OnDisposeData;
  HoldOnLoadData := FContainer.OnLoadData;
  HoldOnStoreData := FContainer.OnStoreData;
  FContainer.Free;
  FContainer := TStLArray.Create(FElementCount, FElementSize);
  FContainer.OnCompare := HoldOnCompare;
  FContainer.OnDisposeData := HoldOnDisposeData;
  FContainer.OnLoadData := HoldOnLoadData;
  FContainer.OnStoreData := HoldOnStoreData;
end;

procedure TStNVLArray.SetElementCount(Value : LongInt);
begin
  FElementCount := Value;
  RecreateContainer;
end;

procedure TStNVLArray.SetElementSize(Value : Cardinal);
begin
  FElementSize := Value;
  RecreateContainer;
end;

procedure TStNVLArray.SetOnCompare(Value : TStCompareEvent);
begin
  FContainer.OnCompare := Value;
end;

procedure TStNVLArray.SetOnDisposeData(Value : TStDisposeDataEvent);
begin
  FContainer.OnDisposeData := Value;
end;

procedure TStNVLArray.SetOnLoadData(Value : TStLoadDataEvent);
begin
  FContainer.OnLoadData := Value;
end;

procedure TStNVLArray.SetOnStoreData(Value : TStStoreDataEvent);
begin
  FContainer.OnStoreData := Value;
end;

procedure TStNVLArray.SetStoreable(Value : Boolean);
begin
  FContainer.ElementsStorable := Value;
end;


end.
