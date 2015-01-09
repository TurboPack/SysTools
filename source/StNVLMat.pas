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
{* SysTools: StNVLMat.pas 4.04                           *}
{*********************************************************}
{* SysTools:non visual component for TStLMatrix          *}
{*********************************************************}

{$I StDefine.inc}

unit StNVLMat;

interface

uses
  Windows,
  Classes,
  StBase, StLArr, StNVCont;

type
  TStNVLMatrix = class(TStNVContainerBase)
  {.Z+}
  protected {private}
    {property variables}
    FContainer        : TStLMatrix; {instance of the container}
    FCols             : Cardinal;
    FRows             : Cardinal;
    FElementSize      : Cardinal;

    {property methods}
    function GetStoreable : Boolean;
    procedure SetCols(Value : Cardinal);
    procedure SetRows(Value : Cardinal);
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

    property Container : TStLMatrix
      read FContainer;

  published
    property Cols : Cardinal
      read FCols
      write SetCols default 2;

    property Rows : Cardinal
      read FRows
      write SetRows default 10;

    property ElementSize : Cardinal
      read FElementSize
      write SetElementSize default SizeOf(Integer);

    property ElementsStorable : Boolean
      read GetStoreable
      write SetStoreable default False;

    property OnCompare;
    property OnDisposeData;
    property OnLoadData;
    property OnStoreData;
  end;


implementation

{*** TStNVLMatrix ***}

constructor TStNVLMatrix.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {defaults}
  FCols := 2;
  FRows := 10;
  FElementSize := SizeOf(Integer);

  if Classes.GetClass(TStLMatrix.ClassName) = nil then
    RegisterClass(TStLMatrix);

  FContainer := TStLMatrix.Create(FRows, FCols, FElementSize);
end;

destructor TStNVLMatrix.Destroy;
begin
  FContainer.Free;
  FContainer := nil;

  inherited Destroy;
end;

function TStNVLMatrix.GetOnCompare : TStCompareEvent;
begin
  Result := FContainer.OnCompare;
end;

function TStNVLMatrix.GetOnDisposeData : TStDisposeDataEvent;
begin
  Result := FContainer.OnDisposeData;
end;

function TStNVLMatrix.GetOnLoadData : TStLoadDataEvent;
begin
  Result := FContainer.OnLoadData;
end;

function TStNVLMatrix.GetOnStoreData : TStStoreDataEvent;
begin
  Result := FContainer.OnStoreData;
end;

function TStNVLMatrix.GetStoreable : Boolean;
begin
  Result := FContainer.ElementsStorable;
end;

procedure TStNVLMatrix.RecreateContainer;
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
  FContainer := TStLMatrix.Create(FRows, FCols, FElementSize);
  FContainer.OnCompare := HoldOnCompare;
  FContainer.OnDisposeData := HoldOnDisposeData;
  FContainer.OnLoadData := HoldOnLoadData;
  FContainer.OnStoreData := HoldOnStoreData;
end;

procedure TStNVLMatrix.SetCols(Value : Cardinal);
begin
  FCols := Value;
  RecreateContainer;
end;

procedure TStNVLMatrix.SetElementSize(Value : Cardinal);
begin
  FElementSize := Value;
  RecreateContainer;
end;

procedure TStNVLMatrix.SetOnCompare(Value : TStCompareEvent);
begin
  FContainer.OnCompare := Value;
end;

procedure TStNVLMatrix.SetOnDisposeData(Value : TStDisposeDataEvent);
begin
  FContainer.OnDisposeData := Value;
end;

procedure TStNVLMatrix.SetOnLoadData(Value : TStLoadDataEvent);
begin
  FContainer.OnLoadData := Value;
end;

procedure TStNVLMatrix.SetOnStoreData(Value : TStStoreDataEvent);
begin
  FContainer.OnStoreData := Value;
end;

procedure TStNVLMatrix.SetRows(Value : Cardinal);
begin
  FRows := Value;
  RecreateContainer;
end;

procedure TStNVLMatrix.SetStoreable(Value : Boolean);
begin
  FContainer.ElementsStorable := Value;
end;


end.
