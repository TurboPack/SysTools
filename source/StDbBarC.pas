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
{* SysTools: StDbBarC.pas 4.04                           *}
{*********************************************************}
{* SysTools: data aware bar code components              *}
{*********************************************************}

{$I StDefine.inc}

unit StDbBarC;

interface

uses
  Windows, Classes, ClipBrd, Controls, Graphics, Messages, SysUtils,
  Db, DbCtrls, StConst, StBarc;

type
  TStDbBarCode = class(TStBarCode)
  protected {private}
    {.Z+}
    FDataLink : TFieldDataLink;

    procedure DataChange(Sender : TObject);
    function GetDataField : string;
    function GetDataSource : TDataSource;
    procedure SetDataField(const Value : string);
    procedure SetDataSource(Value : TDataSource);

  public
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    {.Z+}
  published
    property Code
      stored False;

    property DataField : string
      read GetDataField
      write SetDataField;

    property DataSource : TDataSource
      read GetDataSource
      write SetDataSource;
  end;


implementation


{*** TStDbBarCode ***}

constructor TStDbBarCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
end;

procedure TStDbBarCode.DataChange(Sender : TObject);
begin
  if FDataLink.Field = nil then
    Code := '12345678922'
  else
    Code := FDataLink.Field.DisplayText;
end;

destructor TStDbBarCode.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

function TStDbBarCode.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end;

function TStDbBarCode.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource
end;

procedure TStDbBarCode.SetDataField(const Value : string);
begin
  FDataLink.FieldName := Value;
end;

procedure TStDbBarCode.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
end;


end.
