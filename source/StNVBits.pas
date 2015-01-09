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
{* SysTools: StNVBits.pas 4.04                           *}
{*********************************************************}
{* SysTools: non visual component for TStBits            *}
{*********************************************************}

{$I StDefine.inc}

unit StNVBits;

interface

uses
  Windows,
  Classes,
  StBase, StBits, StNVCont;

type
  TStNVBits = class(TStNVContainerBase)
  {.Z+}
  protected {private}
    {property variables}
    FContainer : TStBits; {instance of the container}
    FMaxBits   : Integer;

    {property methods}
    procedure SetMaxBits(Value : Integer);

  protected
    {virtual property methods}
    function GetOnLoadData : TStLoadDataEvent;
      override;
    function GetOnStoreData : TStStoreDataEvent;
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

    property Container : TStBits
      read FContainer;

  published
    property MaxBits : Integer
      read FMaxBits
      write SetMaxBits default 100;

    property OnLoadData;
    property OnStoreData;
  end;


implementation

{*** TStNVBits ***}

constructor TStNVBits.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {defaults}
  FMaxBits := 100;

  if Classes.GetClass(TStBits.ClassName) = nil then
    RegisterClass(TStBits);

  FContainer := TStBits.Create(FMaxBits-1);
end;

destructor TStNVBits.Destroy;
begin
  FContainer.Free;
  FContainer := nil;

  inherited Destroy;
end;

function TStNVBits.GetOnLoadData : TStLoadDataEvent;
begin
  Result := FContainer.OnLoadData;
end;

function TStNVBits.GetOnStoreData : TStStoreDataEvent;
begin
  Result := FContainer.OnStoreData;
end;

procedure TStNVBits.SetMaxBits(Value : Integer);
var
  HoldOnLoadData  : TStLoadDataEvent;
  HoldOnStoreData : TStStoreDataEvent;
begin
  {setting MaxBits will destroy exisiting data}
  if Value < 0 then
    Value := 0;
  FMaxBits := Value;

  HoldOnLoadData := FContainer.OnLoadData;
  HoldOnStoreData := FContainer.OnStoreData;
  FContainer.Free;
  FContainer := TStBits.Create(FMaxBits-1);
  FContainer.OnLoadData := HoldOnLoadData;
  FContainer.OnStoreData := HoldOnStoreData;
end;

procedure TStNVBits.SetOnLoadData(Value : TStLoadDataEvent);
begin
  FContainer.OnLoadData := Value;
end;

procedure TStNVBits.SetOnStoreData(Value : TStStoreDataEvent);
begin
  FContainer.OnStoreData := Value;
end;


end.
