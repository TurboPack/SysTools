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
{* SysTools: StNVDict.pas 4.04                           *}
{*********************************************************}
{* SysTools: non visual component for TStDictionary      *}
{*********************************************************}

{$I StDefine.inc}

unit StNVDict;

interface

uses
  Windows,  Classes,
  StBase, StDict, StNVCont;

type
  TStNVDictionary = class(TStNVContainerBase)
  {.Z+}
  protected {private}
    {property variables}
    FContainer : TStDictionary; {instance of the container}
    FHashSize  : Integer;

    {property methods}
    function GetHashSize : Integer;
    function GetOnEqual : TStStringCompareEvent;
    procedure SetHashSize(Value : Integer);
    procedure SetOnEqual(Value : TStStringCompareEvent);

  protected
    function GetOnDisposeData : TStDisposeDataEvent;
      override;
    function GetOnLoadData : TStLoadDataEvent;
      override;
    function GetOnStoreData : TStStoreDataEvent;
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

    property Container : TStDictionary
      read FContainer;

  published
    property HashSize : Integer
      read GetHashSize
      write SetHashSize default 509;

    property OnEqual : TStStringCompareEvent
      read GetOnEqual
      write SetOnEqual;

    property OnDisposeData;
    property OnLoadData;
    property OnStoreData;
  end;


implementation

{*** TStNVDictionary ***}

constructor TStNVDictionary.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {defaults}
  FHashSize := 509;

  if Classes.GetClass(TStDictionary.ClassName) = nil then
    RegisterClass(TStDictionary);
  if Classes.GetClass(TStDictNode.ClassName) = nil then
    RegisterClass(TStDictNode);

  FContainer := TStDictionary.Create(FHashSize);
end;

destructor TStNVDictionary.Destroy;
begin
  FContainer.Free;
  FContainer := nil;

  inherited Destroy;
end;

function TStNVDictionary.GetHashSize : Integer;
begin
  Result := FContainer.HashSize;
end;

function TStNVDictionary.GetOnDisposeData : TStDisposeDataEvent;
begin
  Result := FContainer.OnDisposeData;
end;

function TStNVDictionary.GetOnEqual : TStStringCompareEvent;
begin
  Result := FContainer.OnEqual;
end;

function TStNVDictionary.GetOnLoadData : TStLoadDataEvent;
begin
  Result := FContainer.OnLoadData;
end;

function TStNVDictionary.GetOnStoreData : TStStoreDataEvent;
begin
  Result := FContainer.OnStoreData;
end;

procedure TStNVDictionary.SetHashSize(Value : Integer);
begin
  FContainer.HashSize := Value;
end;

procedure TStNVDictionary.SetOnDisposeData(Value : TStDisposeDataEvent);
begin
  FContainer.OnDisposeData := Value;
end;

procedure TStNVDictionary.SetOnEqual(Value : TStStringCompareEvent);
begin
  FContainer.OnEqual := Value;
end;

procedure TStNVDictionary.SetOnLoadData(Value : TStLoadDataEvent);
begin
  FContainer.OnLoadData := Value;
end;

procedure TStNVDictionary.SetOnStoreData(Value : TStStoreDataEvent);
begin
  FContainer.OnStoreData := Value;
end;



end.
